package cardinality.compiled

import scala.util.Try

import tastyquery.Contexts.*
import tastyquery.Symbols.{ClassSymbol, ClassTypeParamSymbol, LocalTypeParamSymbol, TypeSymbol}
import tastyquery.Trees.*
import tastyquery.Types.{AppliedType, Type, TypeOrWildcard, TypeParamRef, TypeRef, WildcardTypeArg}

/** Whether code outside the module given a term can run no code of the module through it (no side
  * effects: that code holds nothing from before, and calls only what its own types declare). Such a
  * term is a plain value; one of a class of the module whose parents outside it declare no member
  * (`silent`); one of an object outside the module (built without the module's code); one of a type
  * parameter bounded by such a type; or one of a strict container outside the module (`Strict`)
  * whose type arguments are such. Any other value outside the module may wrap a function of the
  * module (`Ordering.by(f)`, a view, a lazy list); `Product` and `Equals` declare members a class
  * of the module may implement; a value of a top type (an unbounded parameter's included) may be
  * anything, which that code may test (`ScalaRunTime.stringOf`); and an opaque type's
  * representation may be anything.
  */
final private[compiled] class Inert(world: World)(using Context) {

  def apply(term: Tree): Boolean = term match {
    case value: TermTree =>
      Try(value.tpe.widenTermRef match {
        case tpe: Type => inertType(tpe)
        case _         => false
      }).getOrElse(false)
    case _ => false
  }

  private def inertType(tpe: Type): Boolean = Reentry.plainType(tpe) || (tpe.dealias match {
    case applied: AppliedType =>
      Reentry.classOf(applied.tycon).exists { k =>
        ownInert(k)(Inert.Strict(k.displayFullName) && applied.args.forall(inertArgument))
      }
    case ref: TypeRef      => ref.optSymbol.exists(inertSymbol)
    case ref: TypeParamRef => inertType(ref.bounds.high)
    case _                 => false
  })

  private def inertArgument(arg: TypeOrWildcard): Boolean = arg match {
    case arg: Type                 => inertType(arg)
    case wildcard: WildcardTypeArg => inertType(wildcard.bounds.high)
  }

  private def inertSymbol(symbol: TypeSymbol): Boolean = symbol match {
    case k: ClassSymbol                                   => ownInert(k)(k.isModuleClass)
    case p: (ClassTypeParamSymbol | LocalTypeParamSymbol) => inertType(p.declaredBounds.high)
    case _                                                => false
  }

  // A class of the module is inert as `inertClass` says; any other, as `outside` says.
  private def ownInert(k: ClassSymbol)(outside: => Boolean): Boolean =
    if (world.ofModule(k)) inertClass(k) else outside

  // Whether no parent outside the module of a class of the module, or of a class below it (the value
  // may be one, which that code may test for), declares a member.
  private def inertClass(k: ClassSymbol): Boolean = world
    .below(k)
    .exists(below =>
      (k :: below).forall(_.linearization.forall(a => world.ofModule(a) || Reentry.silent(a)))
    )

}

private[compiled] object Inert {

  /** The containers outside the module that hold only the values of their type arguments, built
    * eagerly: no function of the module hides in one.
    */
  private val Strict: Set[String] = Set(
    "scala.Option",
    "scala.Some",
    "scala.collection.immutable.List",
    "scala.collection.immutable.Vector"
  )

}
