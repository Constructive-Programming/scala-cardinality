package tastyquery

import tastyquery.Modifiers.Variance
import tastyquery.Symbols.{Symbol, TypeParamSymbol}
import tastyquery.Trees.PolyTypeDefinitionTree
import tastyquery.Types.TypeLambda

/** What tasty-query keeps private: whether a higher-kinded type parameter declares a variance on a
  * parameter of its own (`F[+_]`, `F[-_]`), as its bound's type lambda or its tree records it (a
  * method's own parameter keeps it in the tree alone). Declared in tasty-query's package to read
  * the flags, and the flags read through the method the bytecode keeps public.
  */
object Variances {

  private val flagsOf = classOf[Symbol].getMethod("flags")

  private def declaresVariance(symbol: Symbol): Boolean = {
    val flags = flagsOf.invoke(symbol).asInstanceOf[java.lang.Long].longValue
    flags.asInstanceOf[Flags.FlagSet].isAnyOf(Flags.Covariant | Flags.Contravariant)
  }

  def variant(parameter: TypeParamSymbol): Boolean = {
    val inLambda = parameter.declaredBounds.high match {
      case lambda: TypeLambda =>
        lambda.typeLambdaParams.exists(_.declaredVariance != Variance.Invariant)
      case _ => false
    }
    val inTree = parameter.tree.exists {
      case defined: Trees.TypeParam =>
        defined.bounds match {
          case PolyTypeDefinitionTree(params, _) => params.exists(p => declaresVariance(p.symbol))
          case _                                 => false
        }
      case _ => false
    }
    inLambda || inTree
  }

}
