package cardinality.compiled

import scala.util.Try

import tastyquery.Contexts.*
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.{ByNameType, MethodType, NoPrefix, TermRef, TermType, ThisType, TypeRef}

/** The definite view of a member on the measured one (`Receivers` drops such a member; `Reentry`
  * reads whether one may re-enter otherwise).
  */
private[compiled] object Definite {

  /** Whether the member's own body evaluates the measured one whenever it is evaluated (the
    * definite view, closed world: no override elsewhere): the measured member, or a member of the
    * instance whose own body does, fully applied on the instance, in a position the body always
    * evaluates. Such a member re-enters the measured one in a receiver that overrides nothing, so
    * no implementation uses it: it supplies nothing.
    *
    * The positions (`strict`): the body's result, and a function literal it returns as a function
    * (every use of a function-typed member applies it); a block's statements and result, a strict
    * `val`'s value; a call's function and its arguments (named or not) a parameter takes by value
    * (not by name, and not an inline method's, whose expansion may drop it); a selection's receiver
    * (an extension or a conversion is a call in the tree, which takes its argument as its parameter
    * says); an `if`'s condition and a match's scrutinee. Not the operand of `&&` or `||`, which the
    * compiler evaluates only on one value of the left one, and not the function literal a member
    * further along the chain holds, which reading it does not apply. A call is on the instance
    * (`onThis`) when its receiver is `this`, it names a member of the instance or a local
    * definition alone (not through an import of another value's members), it builds the measured
    * member's own constructor or a `c` (which runs exactly that constructor, in a `c`), or it
    * selects a member of an object the instance names alone (one object, its class final, whose
    * members are its own). The tree has resolved every name, so a reference to the measured member
    * is a call of it: no conversion or overload can redirect it. A member this reading misses is
    * left by `may`.
    *
    * On a record value (`value`) the measured member runs on no instance of the record, and a call
    * of it re-enters only with the same arguments: only `this` and the members it names alone chain
    * (the arguments are fixed by the call). A member of the instance runs the body an instance of
    * `c` that overrides nothing has: the nearest in `c`'s linearization.
    */
  def apply(
      c: ClassSymbol,
      member: TermSymbol,
      measured: TermSymbol,
      value: Boolean
  )(using Context): Boolean = Evaluation(c, measured, value).evaluates(member, top = true)

  final private class Evaluation(c: ClassSymbol, measured: TermSymbol, value: Boolean)(using
      Context
  ) {
    private val seen = scala.collection.mutable.Set.empty[Symbol]

    private def effective(m: TermSymbol): TermSymbol =
      Try(c.linearization.iterator.flatMap(m.overridingSymbol).nextOption()).toOption.flatten
        .getOrElse(m)

    // `top` for the member itself, every use of which applies a function it returns.
    def evaluates(m: TermSymbol, top: Boolean): Boolean =
      seen.add(m) && !m.isAbstractMember && m.tree.exists {
        case method: DefDef => method.rhs.exists(result(_, top))
        case field: ValDef  => field.rhs.exists(result(_, top))
        case _              => false
      }

    private def result(tree: Tree, top: Boolean): Boolean = tree match {
      case Block(List(literal: DefDef), Lambda(_, None)) if top =>
        literal.rhs.exists(result(_, top))
      case Block(statements, last) => statements.exists(statement) || result(last, top)
      case Typed(expression, _)    => result(expression, top)
      case _                       => strict(tree)
    }

    private def statement(tree: Tree): Boolean = tree match {
      // A lazy value runs only where something reads it.
      case local: ValDef if local.symbol.kind != TermSymbolKind.LazyVal => local.rhs.exists(strict)
      case term: TermTree                                               => strict(term)
      case _                                                            => false
    }

    private def strict(tree: Tree): Boolean = tree match {
      case Block(statements, last) => statements.exists(statement) || strict(last)
      case Typed(expression, _)    => strict(expression)
      case If(condition, _, _)     => strict(condition)
      case Match(scrutinee, _)     => strict(scrutinee)
      case NamedArg(_, argument)   => strict(argument)
      case _                       => reenters(tree) || strictParts(tree)
    }

    // Whether the tree itself is a call on the instance of a member that evaluates the measured one.
    private def reenters(tree: Tree): Boolean =
      onThis(tree) && applied(tree).exists { m =>
        m == measured && again(tree) ||
        m != measured && m.owner.isClass && evaluates(effective(m), top = false)
      }

    private def strictParts(tree: Tree): Boolean = tree match {
      case Apply(function, arguments) =>
        strict(function) || arguments.zipWithIndex.exists { (argument, i) =>
          byValue(function, i) && strict(argument)
        }
      case TypeApply(function, _) => strict(function)
      case Select(qualifier, _)   => strict(qualifier)
      case _                      => false
    }

    // Whether a call of the measured member passes it its own parameters, in order: with any
    // other arguments it may return without calling itself again. A local one also captures the
    // parameters of the methods it is written in, which the call that reached it may have given
    // other values: only where those methods take none.
    private def again(tree: Tree): Boolean =
      Reentry.unapplied(tree)._2.flatten.map(World.named) ==
        measured.paramSymss.collect { case Left(clause) => clause }.flatten.map(Some(_)) &&
        World.owners(measured.owner).takeWhile(!_.isClass).forall {
          case method: TermSymbol => method.paramSymss.forall(_.left.forall(_.isEmpty))
          case _                  => true
        }

    // Whether a call is made on the instance the measured member runs on.
    private def onThis(tree: Tree): Boolean = tree match {
      case Apply(function, _)     => onThis(function)
      case TypeApply(function, _) => onThis(function)
      case Select(_: This, _)     => true
      case Select(New(tpt), _)    => builtOnThis(tpt)
      case Select(path, _)        => !value && named(path) && ofNamedObject(path, tree)
      case reference: Ident       => namedCall(reference)
      case _                      => false
    }

    // A new instance of the class runs its own members, which nothing overrides. Building one
    // calls no member of the measured constructor's row, whose positions are inputs.
    // Only one built on the instance's own outer instance: `new w.C` runs `w`'s members.
    private def builtOnThis(tpt: TypeTree): Boolean =
      !value && Try(World.classNamed(tpt.toType)).toOption.flatten.contains(c) &&
        (World.inObjects(c) || local(tpt.toType))

    // A member named alone: a member of the instance or a local definition, or on a record value
    // a member of its own object.
    private def namedCall(reference: Ident): Boolean =
      named(reference) &&
        called(reference).exists(m =>
          !Reentry.objectMember(m) || !value && m.owner == measured.owner
        )

    // A member of an object the instance names: `this` there is the object, which is the
    // measured instance's own only where it does not inherit the measured member itself.
    private def ofNamedObject(path: Tree, tree: Tree): Boolean =
      called(path).flatMap(_.moduleClass).exists { k =>
        (measured.owner match {
          case owner: ClassSymbol => owner == k || !k.isSubClass(owner)
          case _                  => false
        }) && called(tree).exists(_.owner == k)
      }

    // A name alone that the instance or a scope around it binds: not a member of another value an
    // import brings.
    private def named(path: Tree): Boolean = path match {
      case Select(_: This, _) => true
      case reference: Ident   => local(reference.tpe)
      case _                  => false
    }

    // Whether a reference's prefix is none or a `this`: the instance or a scope around it.
    private def local(tpe: => TermType): Boolean = Try(tpe match {
      case ref: (TermRef | TypeRef) => ref.prefix == NoPrefix || ref.prefix.isInstanceOf[ThisType]
      case _                        => false
    }).getOrElse(false)

  }

  // Whether a call's function takes its argument at `i` by value: its type says so, and it is no
  // inline method and no `&&` or `||`, whose operand is evaluated on one value of the left one.
  private def byValue(function: Tree, i: Int)(using Context): Boolean =
    !called(Reentry.unapplied(function)._1).exists { m =>
      m.isInline || m.owner.displayFullName == "scala.Boolean" && Set("&&", "||")(m.name.toString)
    } && Try(function match {
      case term: TermTree =>
        term.tpe.widenTermRef match {
          case method: MethodType => !method.paramTypes(i).isInstanceOf[ByNameType]
          case _                  => false
        }
      case _ => false
    }).getOrElse(false)

  // The member a tree applies to every value clause it takes (a value: named alone), if any.
  private def applied(tree: Tree)(using Context): Option[TermSymbol] = {
    val (head, clauses) = Reentry.unapplied(tree)
    called(head).filter(_.paramSymss.count(_.isLeft) == clauses.size)
  }

  // The member a reference names; none where it does not resolve, which may be anything.
  private def called(tree: Tree)(using Context): Option[TermSymbol] = World.named(tree)

}
