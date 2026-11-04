package cardinality.types

import scala.meta.*

/** How a declared type parameter varies. */
private[cardinality] enum Variance {
  case Covariant, Contravariant, Invariant
}

private[cardinality] object Variance {

  def of(param: Type.Param): Variance =
    if (param.mods.exists(_.is[Mod.Covariant])) Covariant
    else if (param.mods.exists(_.is[Mod.Contravariant])) Contravariant
    else Invariant

  /** Whether `tpe` holds a value of the parameter `name` only where a value of any supertype could
    * stand: the parameter itself, tuples, unions and intersections of such, by-name and repeated
    * ones, a function's result, and the covariant slots of a constructor — of a declaration the
    * name denotes where the type is written (`declared` gives its parameters' variances), or of a
    * standard data constructor, every slot. Held, it is the standard constructors the answer rests
    * on, which the caller checks no source binds; a function's domain, a match type, a qualified or
    * any other constructor is not held.
    */
  def holdsBelow(
      tpe: Type,
      name: String,
      declared: String => Option[List[Variance]]
  ): Option[List[String]] = {
    def mentions(t: Type): Boolean = TypeName.words(t).contains(name)
    def all(parts: List[Type]): Option[List[String]] =
      Traversals.defined(parts.map(go)).map(_.flatten)
    def go(t: Type): Option[List[String]] = t match {
      case _ if !mentions(t)    => Some(Nil)
      case Type.Name(_)         => Some(Nil)
      case Trees.Held(parts)    => all(parts)
      case f: Type.FunctionType =>
        if (f.paramClause.values.exists(mentions)) None else go(f.res)
      case Type.Apply.After_4_6_0(Type.Name(callee), Type.ArgClause(args)) =>
        val known = declared(callee)
        val slots = known.fold(StandardConstructors.slots(callee, args.size).getOrElse(Nil))(
          _.map(_ == Covariant)
        )
        val standard = if (known.isEmpty) List(callee) else Nil
        val held = slots.padTo(args.size, false)
        if (args.zip(held).exists((arg, co) => mentions(arg) && !co)) None
        else all(args).map(standard ++ _)
      case _ => None
    }
    go(tpe)
  }

}
