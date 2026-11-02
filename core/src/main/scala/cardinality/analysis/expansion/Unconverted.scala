package cardinality.analysis.expansion

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.analysis.resolution.{Found, OpaqueTypes}
import cardinality.types.{TypeApplications, TypeName, TypeSubstitution}

import MethodAnalysis.Frame

/** A body the compiler wraps in no implicit conversion: one of the method's own parameters whose
  * declared type is the declared result. Scala 3 inserts a conversion only around an expression
  * whose type does not conform to the expected type, a selection of a member its type lacks, or an
  * application no member of its type accepts (reference, *Implicit Conversions*); a parameter of
  * the expected type is none of those. Aliases visible where the method is written are unfolded —
  * an opaque one only inside its scope — when their bodies name nothing but their own parameters,
  * so the unfolded type is spelled in the method's own scope.
  */
private[expansion] object Unconverted {

  /** Whether the body is one of the method's own parameters, declared of the result's type. */
  def returnsOwnParameter(definition: Definition, terms: BodyTerms): Boolean =
    definition.tree match {
      case d: Defn.Def => returned(d.body, d.decltpe, definition, terms)
      case _           => false
    }

  private def returned(
      body: Term,
      result: Option[Type],
      definition: Definition,
      terms: BodyTerms
  ): Boolean = (body, result) match {
    case (Term.Name(name), Some(declaredResult)) =>
      (definition.frame :: definition.frame.parent.filter(extension).toList)
        .flatMap(_.params)
        .find(_.name.value == name)
        .flatMap(_.decltpe)
        .exists(declared => same(declared, declaredResult, definition.frame, terms))
    case _ => false
  }

  // An extension's receiver clause is the method's own parameter list's head.
  private def extension(frame: Frame): Boolean = frame.tree.exists(_.is[Defn.ExtensionGroup])

  private def same(left: Type, right: Type, frame: Frame, terms: BodyTerms): Boolean =
    unfold(left, frame, terms, Set.empty).syntax == unfold(right, frame, terms, Set.empty).syntax

  // Each alias is unfolded once on the way: aliases cannot be cyclic in valid Scala, and stopping at
  // a repeated one leaves two spellings that may differ, which only keeps a conversion possible.
  @annotation.tailrec
  private def unfold(tpe: Type, frame: Frame, terms: BodyTerms, seen: Set[String]): Type = {
    val (head, args) = TypeApplications.headAndArguments(tpe)
    val expanded = head match {
      case named @ (_: Type.Name | _: Type.Select) => aliasExpansion(named, args, frame, terms)
      case _                                       => None
    }
    expanded match {
      case Some((key, body)) if !seen(key) => unfold(body, frame, terms, seen + key)
      case _                               => tpe
    }
  }

  // The body of the alias a name denotes, its parameters bound to `args`, when it reads the same in
  // any scope, with the alias's key.
  private def aliasExpansion(
      named: Type,
      args: List[Type],
      frame: Frame,
      terms: BodyTerms
  ): Option[(String, Type)] =
    terms.typeDeclaration(named, frame)._1 match {
      case Found.Entry(entry) =>
        entry.tree match {
          case alias: Defn.Type
              if !OpaqueTypes.hidden(entry, frame) && closed(alias) &&
                alias.tparamClause.values.size == args.size =>
            val bound = alias.tparamClause.values.map(p => TypeName.of(p.name.value)).zip(args)
            Some(entry.key -> TypeSubstitution.replace(alias.body, bound.toMap))
          case _ => None
        }
      case _ => None
    }

  // An alias whose body names only its own parameters reads the same in any scope.
  private def closed(alias: Defn.Type): Boolean = {
    val parameters = alias.tparamClause.values.map(_.name.value).toSet
    TypeSubstitution.typeNames(alias.body).forall(parameters)
  }

}
