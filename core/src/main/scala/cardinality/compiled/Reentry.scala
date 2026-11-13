package cardinality.compiled

import scala.collection.mutable
import scala.util.Try

import tastyquery.Contexts.*
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.Symbols.*
import tastyquery.Traversers.TreeTraverser
import tastyquery.Trees.*

/** Whether a concrete member a class of the program overrides may call the member being measured
  * (the old reading's view): such a member is a behavior of the measured member's own, not a
  * capability beside it, and `Receivers` leaves the row. (An abstract member is a capability
  * whatever its implementations call, as the scalameta front end has it.)
  *
  * Every behavior is searched: the member's own body and each overrider's, and from them every
  * member they call that the module declares, with what overrides it in turn. A call may reach the
  * measured member
  *
  *   - by naming a member of its name (a path or an override the search does not follow);
  *   - by calling, on the instance, code the module does not declare, whose body is not read and
  *     may call back (what `Any` declares aside);
  *   - by giving the instance away (`this` anywhere but as the receiver of a selection);
  *   - by building an instance of the measured member's class, which runs its constructor.
  *
  * A body that is not read (an overrider no symbol stands for), a name that does not resolve, or a
  * search that reaches `Reach` members, may. Only the instance is followed: another instance of the
  * class handed to code outside the module (`List(a, b).sorted`) is not.
  *
  * A value of a capability record is another instance than the site's (`may` with `instance`
  * false): its behaviors re-enter the measured member only by reaching it, which `reached` reads.
  */
final private[compiled] class Reentry(world: World)(using Context) {

  def may(c: ClassSymbol, member: TermSymbol, measured: TermSymbol, instance: Boolean): Boolean = {
    val seen = mutable.Set.empty[Symbol]
    val name = measured.name.toString
    // The class the measured member runs in (a local method's enclosing one).
    val home = World.owners(measured.owner).collectFirst { case k: ClassSymbol => k }

    // Members a finished search on another instance found reach nothing of this measured member.
    val cleared =
      if (instance) mutable.Set.empty[Symbol]
      else unreaching.getOrElseUpdate((name, home), mutable.Set.empty)

    // Every body that may run where `m` is called on an instance of `owner`: its own and each
    // overrider's.
    def behaviors(owner: ClassSymbol, m: TermSymbol): Boolean = {
      val others = world.overriders(owner, m)
      others.exists(_.isEmpty) || (m :: others.flatten).exists(reaches)
    }

    def dispatched(m: TermSymbol): Boolean =
      m.name.toString == name || (m.owner match {
        case owner: ClassSymbol => behaviors(owner, m)
        case _                  => reaches(m)
      })

    def reaches(m: TermSymbol): Boolean =
      seen.size >= Reentry.Reach || (!cleared(m) && seen.add(m) && m.tree.exists(calls))

    // A member called on the instance: the module's is searched, any other may call back.
    def fromInstance(m: TermSymbol): Boolean =
      if (Receivers.accessor(m)) {
        val target = m.name.toString.split("\\$\\$").last
        target == name || m.owner.isClass && m.owner.asClass.linearization.drop(1).exists {
          _.declarations.exists {
            case parent: TermSymbol if parent.name.toString == target => fromInstance(parent)
            case _                                                    => false
          }
        }
      } else if (world.ofModule(m)) dispatched(m)
      else m.name.toString == name || !World.universal(m.owner)

    // Whether a reference names a member of the measured one's name.
    def measures(reference: Tree): Boolean = called(reference).exists(_.name.toString == name)

    // On another instance than the site's (a value of a capability record), a behavior re-enters
    // the measured member only by reaching it: naming a member of its name, calling a member of
    // the module that does, a name that does not resolve, applying a function a field holds (built
    // where the search does not look), or building an instance of its class. Code outside the
    // module reaches the module back only through the members of the module's classes that
    // override its own: those of a class whose instance the behavior gives away (`this`) or builds
    // are searched.
    def reached(body: Tree): Boolean = Reentry.search(body) {
      case reference: TermReferenceTree if measures(reference) =>
        Some(true)
      case reference: TermReferenceTree if Try(reference.symbol).isFailure => Some(true)
      case Apply(fun @ Select(held, _), _)
          if called(fun).exists(_.name.toString == "apply") && called(held).exists { m =>
            (m.isParamAccessor || !m.isMethod) && World.functional(m.declaredType)
          } =>
        Some(true)
      // A selection on the instance names a member, and gives nothing away.
      case tree @ Select(_: This | _: Super, _) => Some(module(tree))
      case self: This                           =>
        Some(Try(self.tpe match {
          case own: tastyquery.Types.ThisType => escapes(own.cls)
          case _                              => true
        }).getOrElse(true))
      case reference: TermReferenceTree => Option.when(module(reference))(true)
      case New(tpt)                     =>
        Try(World.classNamed(tpt.toType)).toOption.flatten
          .map { k =>
            home.exists(k.isSubClass) || escapes(k)
          }
          .filter(identity)
      case _ => None
    }

    // Whether a reference calls a member of the module that reaches the measured one; overriders
    // that are not read may be anything.
    def module(reference: Tree): Boolean =
      called(reference).exists(m => world.ofModule(m) && Try(dispatched(m)).getOrElse(true))

    // Whether a member a class of the module declares, overriding one of code outside it, reaches
    // the measured member: code outside the module calls it on an instance it is given.
    def escapes(k: ClassSymbol): Boolean =
      k.linearization.filter(world.ofModule).exists {
        _.declarations.exists {
          case m: TermSymbol => m.allOverriddenSymbols.exists(o => !world.ofModule(o)) && reaches(m)
          case _             => false
        }
      }

    def calls(body: Tree): Boolean = if (!instance) reached(body)
    else
      Reentry.search(body) {
        case reference: TermReferenceTree if measures(reference) =>
          Some(true)
        case tree @ Select(_: This | _: Super, _) => Some(called(tree).forall(fromInstance))
        case _: This                              => Some(true)
        // A name that does not resolve may be anything.
        case reference: Ident if called(reference).isEmpty                 => Some(true)
        case reference: Ident if called(reference).exists(_.owner.isClass) =>
          Some(called(reference).forall(fromInstance))
        case reference: Select =>
          Option.when(called(reference).forall { m =>
            m.name.toString == name || (world.ofModule(m) && dispatched(m))
          })(true)
        case New(tpt) if World.classNamed(tpt.toType).exists(k => home.exists(k.isSubClass)) =>
          Some(true)
        case _ => None
      }

    val found = behaviors(c, member)
    // Every member such a search visited, finished below the cap, reaches nothing.
    if (!instance && !found && seen.size < Reentry.Reach) cleared ++= seen
    found
  }

  // For a record value, the members a finished search found to reach nothing, by the measured
  // member's name and class: what `reached` reads depends on those alone (and on the program), so
  // a member one search cleared reaches nothing for any other with the same key.
  private val unreaching = mutable.Map.empty[(String, Option[ClassSymbol]), mutable.Set[Symbol]]

  /** Whether the member's own body evaluates the measured one whenever it is evaluated (the
    * scalameta front end's definite view): the measured member, or a member of the instance whose
    * own body does, fully applied on the instance as the body's result, a statement of a block or
    * the value of one of its `val`s. Such a member re-enters the measured one in a receiver that
    * overrides nothing, so no implementation uses it: it supplies nothing.
    *
    * Only what no conversion, overload or deferred argument can redirect is definite: not a
    * measured member that is implicit, a given or an extension (the compiler inserts or selects
    * it), nor one an overload of as many value clauses shares a name with, nor a body that imports
    * anything (a conversion it brings may take the call by name). The old reading finds more
    * positions definite; a member this reading misses is left by `may`.
    */
  def definite(member: TermSymbol, measured: TermSymbol): Boolean = {
    val seen = mutable.Set.empty[Symbol]
    def evaluates(m: TermSymbol): Boolean =
      seen.add(m) && !m.isAbstractMember && m.tree.exists {
        case method: DefDef => method.rhs.exists(body => !imports(body) && spine(body))
        case value: ValDef  => value.rhs.exists(body => !imports(body) && spine(body))
        case _              => false
      }
    def spine(tree: Tree): Boolean = tree match {
      case Block(statements, result) =>
        statements.exists {
          // A lazy value runs only where something reads it.
          case value: ValDef if value.symbol.kind != TermSymbolKind.LazyVal =>
            value.rhs.exists(spine)
          case term: TermTree => spine(term)
          case _              => false
        } || spine(result)
      case Typed(expression, _) => spine(expression)
      case _                    =>
        onThis(tree) && applied(tree).exists { m =>
          m == measured || (m.owner.isClass && evaluates(m))
        }
    }
    settled(measured) && evaluates(member)
  }

  // Whether a call of `measured` is its own: no conversion, extension or given, and no overload of
  // as many value clauses.
  private def settled(measured: TermSymbol): Boolean = {
    val clauses = measured.paramSymss.count(_.isLeft)
    !measured.isImplicit && !measured.isGivenOrUsing && !measured.isExtensionMethod &&
    (measured.owner match {
      case owner: ClassSymbol =>
        Try(owner.linearization).getOrElse(List(owner)).flatMap(_.declarations).forall {
          case other: TermSymbol if other != measured && other.name == measured.name =>
            other.paramSymss.count(_.isLeft) != clauses
          case _ => true
        }
      case _ => true
    })
  }

  private def imports(body: Tree): Boolean = Reentry.search(body) {
    case _: Import => Some(true)
    case _         => None
  }

  // The member a tree applies to every value clause it takes (a value: named alone), if any.
  private def applied(tree: Tree): Option[TermSymbol] = {
    def head(t: Tree, clauses: Int): Option[(TermSymbol, Int)] = t match {
      case Apply(function, _)           => head(function, clauses + 1)
      case TypeApply(function, _)       => head(function, clauses)
      case reference: TermReferenceTree => called(reference).map(_ -> clauses)
      case _                            => None
    }
    head(tree, 0).collect {
      case (m, clauses) if m.paramSymss.count(_.isLeft) == clauses => m
    }
  }

  // Whether a call is made on the instance (or is a local method's): its receiver is `this`, or it
  // names the member alone.
  private def onThis(tree: Tree): Boolean = tree match {
    case Apply(function, _)     => onThis(function)
    case TypeApply(function, _) => onThis(function)
    case Select(_: This, _)     => true
    case _: Ident               => true
    case _                      => false
  }

  // The member a reference names; none where it does not resolve, which may be anything.
  private def called(tree: Tree): Option[TermSymbol] = World.named(tree)

}

private[compiled] object Reentry {

  /** How many members the search reads before it gives up. */
  val Reach = 200

  /** Whether a node of `body` is found: `decide` says so for a node it knows (`false` leaves the
    * node's children unsearched), and the children of any other node are searched.
    */
  def search(body: Tree)(decide: Tree => Option[Boolean]): Boolean = {
    var found = false
    new TreeTraverser {
      override def traverse(tree: Tree): Unit = if (!found) decide(tree) match {
        case Some(answer) => found = answer
        case None         => super.traverse(tree)
      }
    }.traverse(body)
    found
  }

}
