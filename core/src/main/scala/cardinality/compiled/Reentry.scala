package cardinality.compiled

import scala.collection.mutable
import scala.util.Try

import tastyquery.Contexts.*
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.Names.{SignedName, TermName}
import tastyquery.Symbols.*
import tastyquery.Traversers.TreeTraverser
import tastyquery.Trees.*
import tastyquery.Types.{ByNameType, MethodType, PolyType, TermType, Type}

/** Whether a concrete member a class of the program overrides may call the member being measured
  * (the old reading's view): such a member is a behavior of the measured member's own, not a
  * capability beside it, and `Receivers` leaves the row. (An abstract member is a capability
  * whatever its implementations call, as the scalameta front end has it.)
  *
  * Every behavior is searched: the member's own body and each overrider's, and from them every
  * member they call that the module declares, with what overrides it in turn. A call may reach the
  * measured member
  *
  *   - by a reference that may run it (`runs`: the member, or one of its name it may dispatch to);
  *   - by calling, on the instance, code the module does not declare, whose body is not read and
  *     may call back (what `Any` declares aside);
  *   - by giving the instance away (`this` anywhere but as the receiver of a selection);
  *   - by building an instance of the measured member's class, which runs its constructor;
  *   - where code outside the module may reach the measured member (`outward`), by handing that
  *     code a value of the module (`List(a).sorted(o)`), or by building a class whose body reaches
  *     it;
  *   - by first accessing an object of the module whose body reaches it.
  *
  * A body that is not read (an overrider no symbol stands for), a name that does not resolve, or a
  * search that reaches `Reach` members, may.
  *
  * A value of a capability record is another instance than the site's (`may` with `instance`
  * false): its behaviors re-enter the measured member only by reaching it, which `reached` reads,
  * and only if a chain of references leads from them to it (`callersOf`).
  */
final private[compiled] class Reentry(world: World)(using Context) {

  def may(c: ClassSymbol, member: TermSymbol, measured: TermSymbol, instance: Boolean): Boolean = {
    val seen = mutable.Set.empty[Symbol]
    // The class the measured member runs in (a local method's enclosing one).
    val home = World.owners(measured.owner).collectFirst { case k: ClassSymbol => k }

    // Members a finished search on another instance found reach nothing of this measured member.
    val cleared =
      if (instance) mutable.Set.empty[Symbol]
      else unreaching.getOrElseUpdate(measured, mutable.Set.empty)

    // Code outside the module may call the measured member, which no reference names.
    val external = calledOutside(measured)
    // Whether code outside the module may run code that reaches the measured member: it may call
    // the member, or references do not settle what calls the member (`callersOf`).
    lazy val outward = external || callersOf(measured).isEmpty

    // Every body that may run where `m` is called on an instance of `owner`: its own and each
    // overrider's; and, where code outside the module may reach the measured member (else no such
    // body names it), a function literal's, not read, that implements a subclass of `owner` (whose
    // abstract method may redeclare `m`) or a class that does not resolve.
    def behaviors(owner: ClassSymbol, m: TermSymbol, reach: TermSymbol => Boolean): Boolean = {
      val others = world.overriders(owner, m)
      others.exists(_.isEmpty) || (m :: others.flatten).exists(reach) ||
      outward && implemented.exists(_.forall(_.isSubClass(owner)))
    }

    def dispatched(m: TermSymbol): Boolean =
      runs(m, measured) || (m.owner match {
        case owner: ClassSymbol => behaviors(owner, m, reaches)
        case _                  => reaches(m)
      })

    def reaches(m: TermSymbol): Boolean =
      seen.size >= Reentry.Reach || (!cleared(m) && seen.add(m) && m.tree.exists(calls))

    // A member called on the instance: the module's is searched, any other may call back.
    def fromInstance(m: TermSymbol): Boolean =
      if (Receivers.accessor(m)) {
        val target = m.name.toString.split("\\$\\$").last
        target == measured.name.toString || m.owner.isClass && m.owner.asClass.linearization
          .drop(1)
          .exists {
            _.declarations.exists {
              case parent: TermSymbol if parent.name.toString == target => fromInstance(parent)
              case _                                                    => false
            }
          }
      } else if (world.ofModule(m)) dispatched(m)
      else runs(m, measured) || !World.universal(m.owner)

    // Whether a reference may run the measured member.
    def measures(reference: Tree): Boolean = called(reference).exists(runs(_, measured))

    // On another instance than the site's (a value of a capability record), a behavior re-enters
    // the measured member only by reaching it: a reference that may run it, a call of a member of
    // the module that does, a name that does not resolve, applying a function a field or parameter
    // holds or naming a by-name parameter (built where the search does not look), building an
    // instance of its class or of one that does not resolve, or first accessing an object whose
    // body reaches it. Where code outside the module may reach it (`outward`), so may that code:
    // when the behavior hands it a value of the module (`handsOver`, a member declared outside the
    // module selected on the instance included), or builds a class whose body reaches it.
    def reached(body: Tree): Boolean = Reentry.search(body) {
      case reference: TermReferenceTree if measures(reference) =>
        Some(true)
      case reference: TermReferenceTree if Try(reference.symbol).isFailure => Some(true)
      case tree if outward && handsOver(tree)                              => Some(true)
      case Apply(fun @ Select(held, _), _)
          if called(fun).exists(_.name.toString == "apply") && called(held).exists { m =>
            (m.isParamAccessor || !m.isMethod) && World.functional(m.declaredType)
          } =>
        Some(true)
      case reference: TermReferenceTree
          if called(reference).exists(_.declaredType.isInstanceOf[ByNameType]) =>
        Some(true)
      // A selection on the instance names a member, and gives nothing away.
      case tree @ Select(_: This | _: Super, _)                   => Some(module(tree))
      case reference: TermReferenceTree if initializes(reference) => Some(true)
      case reference: TermReferenceTree => Option.when(module(reference))(true)
      case New(tpt)                     => Option.when(builds(tpt, reachesBuilt))(true)
      case _                            => None
    }

    // Whether building an instance of `k` may reach the measured member: it is of the measured
    // member's class, or its construction reaches it (an object's whatever code outside the module
    // may do, since references name its body; a class's where that code may reach the member).
    def reachesBuilt(k: ClassSymbol): Boolean =
      home.exists(k.isSubClass) || (outward || k.isModuleClass) && constructs(k)

    // Whether a reference to an object of the module, or to a member of one, runs the object's
    // body (its first access does) and that reaches the measured member.
    def initializes(reference: Tree): Boolean = called(reference).exists { s =>
      (s.moduleClass.toList ++ List(s.owner).collect {
        case k: ClassSymbol if k.isModuleClass => k
      })
        .exists(k => world.ofModule(k) && constructs(k))
    }

    // A member, or a module's value, the module declares.
    def own(m: TermSymbol): Boolean = world.ofModule(m) || m.moduleClass.exists(world.ofModule)

    // Whether a call or selection runs code outside the module (what `Any` declares aside).
    def outside(tree: Tree): Boolean =
      called(Reentry.unapplied(tree)._1).exists(m => !own(m) && !World.universal(m.owner))

    // Whether a call or selection hands a value of the module to code outside it: an argument, on
    // which that code may call anything; or the receiver, on which the member selected runs its
    // body (any, where it is not read) or what overrides it.
    def handsOver(tree: Tree): Boolean = tree match {
      case Apply(function, arguments)            => outside(function) && arguments.exists(handed)
      case Select(qualifier, _) if outside(tree) =>
        handed(qualifier) && called(tree).exists { m =>
          Try(dispatched(m) || m.tree.isEmpty && !m.isAbstractMember).getOrElse(true)
        }
      case _ => false
    }

    // Whether a term may hold a value of the module: the instance, or a reference to what the module
    // declares (or one that does not resolve) whose value is no primitive, `String` or `Unit`. A
    // function literal's closure names only its own method, whose body beside it is searched.
    def handed(term: Tree): Boolean = Reentry.search(term) {
      case _: This                                 => Some(true)
      case _: Lambda                               => Some(false)
      case plain: TermTree if Reentry.plain(plain) => Some(false)
      case reference: TermReferenceTree => Option.when(called(reference).forall(own))(true)
      case _                            => None
    }

    // Whether building an instance of `k` runs code that reaches the measured member: the
    // constructors of the module's classes it is, their parents' arguments and the statements of
    // their bodies (a method, a lazy value or a class declared there runs only where it is used).
    def constructs(k: ClassSymbol): Boolean = k.linearization.filter(world.ofModule).exists { c =>
      seen.size >= Reentry.Reach || seen.add(c) && c.tree.exists { definition =>
        val template = definition.rhs
        (template.parents ++ template.body).exists {
          case _: DefDef | _: ClassDef | _: TypeMember | _: TypeTree        => false
          case value: ValDef if value.symbol.kind == TermSymbolKind.LazyVal => false
          case statement                                                    => calls(statement)
        }
      }
    }

    // Whether a reference calls a member of the module that reaches the measured one; overriders
    // that are not read may be anything.
    def module(reference: Tree): Boolean =
      called(reference).exists(m => world.ofModule(m) && Try(dispatched(m)).getOrElse(true))

    def calls(body: Tree): Boolean = if (!instance) reached(body)
    else
      Reentry.search(body) {
        case reference: TermReferenceTree if measures(reference) =>
          Some(true)
        case tree if outward && handsOver(tree)   => Some(true)
        case tree @ Select(_: This | _: Super, _) => Some(called(tree).forall(fromInstance))
        case _: This                              => Some(true)
        // A name that does not resolve may be anything.
        case reference: Ident if called(reference).isEmpty                 => Some(true)
        case reference: Ident if called(reference).exists(_.owner.isClass) =>
          Some(called(reference).forall(fromInstance))
        case reference: Select =>
          Option.when(called(reference).forall { m =>
            runs(m, measured) || (world.ofModule(m) && dispatched(m))
          })(true)
        case reference: TermReferenceTree if initializes(reference) => Some(true)
        case New(tpt) if builds(tpt, reachesBuilt)                  => Some(true)
        case _                                                      => None
      }

    // On another instance, a behavior no chain of references leads from to the measured
    // member (`callersOf`) does not reach it, whatever the search says. A measured strict field
    // runs when its class is built, which no reference names: not read so. A behavior that is a
    // field runs in its class's body, so it is a caller where that body is.
    val field = !measured.isMethod && measured.kind != TermSymbolKind.LazyVal
    val callers = Option.when(!instance && !field && !external)(callersOf(measured)).flatten
    val found = callers.forall { set =>
      behaviors(c, member, m => set(m) || (!m.isMethod && set(m.owner)))
    } && behaviors(c, member, reaches)
    // Every member such a search visited, finished below the cap, reaches nothing.
    if (!instance && !found && seen.size < Reentry.Reach) cleared ++= seen
    found
  }

  // For a record value, the members a finished search found to reach nothing, by the measured
  // member: what `reached` reads depends on it alone (and on the program).
  private val unreaching = mutable.Map.empty[TermSymbol, mutable.Set[Symbol]]

  // Whether a reference to `s` may run `t`: the same member, one of its name in a class an instance
  // may share with `t`'s (or one `mixes` cannot compare), through which it dispatches, or the field
  // of a constructor's parameter, which its class's body names.
  private def runs(s: TermSymbol, t: TermSymbol): Boolean =
    s == t || s.name == t.name && ((s.owner, t.owner) match {
      case (a: ClassSymbol, b: ClassSymbol) => Try(world.mixes(a, b)).getOrElse(true)
      case (a: ClassSymbol, b: TermSymbol)  => World.isConstructor(b) && b.owner == a
      case _                                => false
    })

  // Whether code outside the module may call `m`: it overrides a member of that code, or a class
  // of the program that inherits it has a parent outside the module declaring a member of its name,
  // which `m` implements there (`trait Half { def compare… }; object C extends Half with Ordering`).
  private def calledOutside(m: TermSymbol): Boolean = Try(
    m.allOverriddenSymbols.exists(o => !world.ofModule(o)) || (m.owner match {
      case owner: ClassSymbol =>
        world.below(owner).forall { below =>
          (owner :: below).exists(_.linearization.exists { parent =>
            !world.ofModule(parent) && parent.declarations.exists(_.name == m.name)
          })
        }
      case _ => false
    })
  ).getOrElse(true)

  // The classes a function literal of the module implements other than a function type (a single
  // abstract method conversion): its body runs where that method is called. `None` for one whose
  // class does not resolve, or for a class of the module without a tree, which may hold any.
  private lazy val implemented: Set[Option[ClassSymbol]] = {
    val found = mutable.Set.empty[Option[ClassSymbol]]
    val traverser = new TreeTraverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          case Lambda(_, Some(tpt)) => found += Try(World.classNamed(tpt.toType)).toOption.flatten
          case _                    => ()
        }
        super.traverse(tree)
      }
    }
    world.module.flatMap(_.tree).foreach(traverser.traverse)
    if (world.module.exists(_.tree.isEmpty)) found += None
    found.toSet
  }

  /** The definitions of the module that may call `measured`: those whose code holds a reference
    * that may run it, or run a caller (`runs`: dispatch; a reference that does not resolve, by its
    * name alone). Code a module class's body holds is run by any access to it, which names its
    * value or a member. None where code may run that no reference names: the body of a function
    * literal or a by-name argument (run by whatever holds it), of a member code outside the module
    * may call (`calledOutside`), of a class that is no object (which `new` runs), or a class of the
    * module without a tree.
    */
  private def callersOf(measured: TermSymbol): Option[Set[Symbol]] = callerSets.getOrElseUpdate(
    measured, {
      val found = mutable.Set.empty[Symbol]
      val targets = mutable.Queue(measured)
      val queued = mutable.Set[Symbol](measured)
      def call(symbol: Symbol): Unit = symbol match {
        case t: TermSymbol if queued.add(t) => targets += t
        case _                              => ()
      }
      var open = world.module.exists(_.tree.isEmpty)
      while (!open && targets.nonEmpty) {
        val target = targets.dequeue()
        referrers.getOrElse(target.name.toString, Nil).foreach {
          case (_, Some(symbol)) if !runs(symbol, target) => ()
          case (Some(holder), _)                          =>
            if (found.add(holder)) holder match {
              case k: ClassSymbol if k.isModuleClass =>
                Try {
                  k.moduleValue.foreach(call)
                  k.linearization.filterNot(World.universal).flatMap(_.declarations).foreach(call)
                }.getOrElse { open = true }
              case t: TermSymbol if !calledOutside(t) => call(t)
              case _                                  => open = true
            }
          case (None, _) => open = true
        }
      }
      Option.when(!open)(found.toSet)
    }
  )

  private val callerSets = mutable.Map.empty[TermSymbol, Option[Set[Symbol]]]

  // Each name the module references, with the code that holds such a reference: the nearest
  // method or lazy value around it, the class whose body does, or the parameter a by-name argument
  // of a method of the module nothing overrides is given (its code runs where the parameter is
  // named); none for a function literal's body or another by-name argument (run by whatever holds
  // it). Each reference comes with the member it resolves to, if it does. A pattern's extractor is
  // applied, and its result asked what the match reads.
  private lazy val referrers: Map[String, List[(Option[Symbol], Option[TermSymbol])]] = {
    val found = mutable.Map.empty[String, mutable.Set[(Option[Symbol], Option[TermSymbol])]]
    class Holder(holder: Option[Symbol]) extends TreeTraverser {
      def note(name: String, symbol: Option[TermSymbol] = None): Unit =
        found.getOrElseUpdate(name, mutable.Set.empty) += holder -> symbol
      def inside(tree: Tree): Unit = super.traverse(tree)
      override def traverse(tree: Tree): Unit = tree match {
        case d: DefDef =>
          Holder(Option.unless(d.symbol.name.toString.startsWith("$anonfun"))(d.symbol)).inside(d)
        case v: ValDef if v.symbol.kind == TermSymbolKind.LazyVal =>
          Holder(Some(v.symbol)).inside(v)
        case c: ClassDef => Holder(Some(c.symbol)).inside(c)
        case _: Unapply  =>
          Reentry.Matched.foreach(note(_))
          super.traverse(tree)
        case call: Apply =>
          val (head, clauses) = Reentry.unapplied(call)
          traverse(head)
          val callee = World.named(head)
          val parameters = Try(callee.toList.flatMap(_.paramSymss.collect { case Left(p) => p }))
            .getOrElse(Nil)
          // Which parameters are by name, as the call's function is typed where it is selected (a
          // function value's `apply` of a by-name type included); one not read is taken for one.
          def flags(tpe: TermType, left: Int): List[List[Boolean]] = tpe match {
            case _ if left == 0     => Nil
            case poly: PolyType     => flags(poly.resultType, left)
            case method: MethodType =>
              method.paramTypes.map(_.isInstanceOf[ByNameType]) :: flags(
                method.resultType,
                left - 1
              )
            case _ => Nil
          }
          val byNames = head match {
            case function: TermTree =>
              Try(flags(function.tpe.widenTermRef, clauses.size)).getOrElse(Nil)
            case _ => Nil
          }
          // A call whose value holds no function runs what it is given, if at all, before it
          // returns (no side effects: nothing else may keep it).
          val plain = Reentry.plain(call)
          // A by-name argument is forced where its parameter is named, if the module's method
          // declaring it is the only one the call may run.
          val sole = callee.exists { m =>
            world.ofModule(m) && !m.isAbstractMember &&
            (m.owner match {
              case k: ClassSymbol => world.overriders(k, m).isEmpty
              case _              => true
            })
          }
          for {
            (arguments, i) <- clauses.zipWithIndex
            (argument, j) <- arguments.zipWithIndex
          } {
            val parameter = parameters.lift(i).flatMap(_.lift(j))
            val byName = byNames.lift(i).flatMap(_.lift(j)).getOrElse(true)
            argument match {
              case Block(List(literal: DefDef), _: Lambda) if plain => inside(literal)
              case _ if plain || !byName                            => traverse(argument)
              case _ => Holder(parameter.filter(_ => sole)).traverse(argument)
            }
          }
        case reference: TermReferenceTree =>
          note(
            Reentry.unsigned(reference match {
              case Ident(name)     => name
              case Select(_, name) => name
            }),
            World.named(reference)
          )
          super.traverse(reference)
        case _ => super.traverse(tree)
      }
    }
    world.module.flatMap(_.tree).foreach(c => Holder(Some(c.symbol)).inside(c))
    found.view.mapValues(_.toList).toMap
  }

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
    val (head, clauses) = Reentry.unapplied(tree)
    called(head).filter(_.paramSymss.count(_.isLeft) == clauses.size)
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

  // Whether a `new` builds a class `p` holds of, or one whose type does not resolve (its arguments
  // included), which may be any. (The `<init>` it selects mostly fails to resolve first.)
  private def builds(tpt: TypeTree, p: ClassSymbol => Boolean): Boolean =
    Try(World.classNamed(tpt.toType).forall(p)).getOrElse(true)

  // The member a reference names; none where it does not resolve, which may be anything.
  private def called(tree: Tree): Option[TermSymbol] = World.named(tree)

}

private[compiled] object Reentry {

  /** A name without the signature a selection may carry. */
  private def unsigned(name: TermName): String = name match {
    case signed: SignedName => signed.underlying.toString
    case other              => other.toString
  }

  /** A call's function and the value clauses it is applied to, in order. */
  private def unapplied(tree: Tree): (Tree, List[List[TermTree]]) = tree match {
    case Apply(function, arguments) =>
      val (head, clauses) = unapplied(function)
      (head, clauses :+ arguments)
    case TypeApply(function, _) => unapplied(function)
    case head                   => (head, Nil)
  }

  /** What a pattern match may call on an extractor's result. */
  private val Matched =
    List("isEmpty", "get", "lengthCompare", "length", "apply", "drop", "toSeq") ++
      (1 to 22).map(n => s"_$n")

  /** Whether a term's value holds no function: a primitive value, a `String` or `Unit`. */
  private def plain(term: TermTree)(using Context): Boolean = Try(term.tpe match {
    case tpe: Type =>
      List(
        defn.IntType,
        defn.LongType,
        defn.FloatType,
        defn.DoubleType,
        defn.BooleanType,
        defn.ByteType,
        defn.ShortType,
        defn.CharType,
        defn.UnitType,
        defn.StringType
      ).exists(tpe.isSubType)
    case _ => false
  }).getOrElse(false)

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
