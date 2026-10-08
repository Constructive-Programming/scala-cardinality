package cardinality.types

import scala.meta.*

// Parameter substitution shared by match-type reduction and its callers.
private[cardinality] object TypeSubstitution {

  // A type with the definition's parameters replaced by the argument types, syntax kept as it is.
  def replace(tpe: Type, arguments: Map[TypeName, Type]): Type =
    if (arguments.isEmpty) tpe
    else {
      new Replacer(arguments).transform(tpe) match {
        case rewritten: Type => rewritten
        case other           => other.asInstanceOf[Type]
      }
    }

  private class Replacer(replacements: Map[TypeName, Type]) extends Transformer {

    override protected def replaceSubtree(tree: Tree): Tree = tree match {
      // A selected member name is nominal, not an occurrence of a type parameter.
      case selected: Type.Select   => selected
      case projected: Type.Project =>
        projected.copy(qual = replace(projected.qual, replacements))
      case lambda: Type.Lambda =>
        lambda.body match {
          case body: Type =>
            val bound = lambda.tparamClause.values.map(p => TypeName.of(p.name.value)).toSet
            lambda.copy(tpe = replace(body, replacements -- bound))
          case _ => lambda
        }
      case poly: Type.PolyFunction =>
        poly.body match {
          case body: Type =>
            val bound = poly.tparamClause.values.map(p => TypeName.of(p.name.value)).toSet
            poly.copy(tpe = replace(body, replacements -- bound))
          case _ => poly
        }
      case refinement: Type.Refine =>
        val members = refinement.body.stats.collect {
          case d: Defn.Type => TypeName.of(d.name.value)
          case d: Decl.Type => TypeName.of(d.name.value)
        }.toSet
        val scoped = new Replacer(replacements -- members)
        refinement.copy(
          tpe = refinement.tpe.map(replace(_, replacements)),
          body = refinement.body.copy(
            stats = refinement.body.stats.map(s => scoped.transform(s).asInstanceOf[Stat])
          )
        )
      case definition: Defn.Type =>
        val bound = definition.tparamClause.values.map(p => TypeName.of(p.name.value)).toSet
        definition.copy(body = replace(definition.body, replacements -- bound))
      case _: Type.Param                            => tree
      case Type.Match.After_4_9_9(scrutinee, block) =>
        Type.Match.After_4_9_9(
          replace(scrutinee, replacements),
          Type.CasesBlock(block.cases.map { c =>
            val bound = c.pat.collect {
              case Type.Name(name) if name.headOption.exists(_.isLower) => TypeName.of(name)
            }.toSet
            val scoped = replacements -- bound
            TypeCase(replace(c.pat, scoped), replace(c.body, scoped))
          })
        )
      case Type.Name(name) if replacements.contains(TypeName.of(name)) =>
        replacements(TypeName.of(name))
      case _ => null
    }

  }

}
