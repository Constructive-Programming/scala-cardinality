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

}
