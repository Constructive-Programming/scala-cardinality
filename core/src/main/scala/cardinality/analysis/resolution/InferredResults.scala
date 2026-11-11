package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.model.ResolutionContext
import cardinality.analysis.{BuiltinTypes, MethodAnalysis, SourceIndex}

import MethodAnalysis.Target

/** A member's result type: the one written in its source, which for a module the build compiled is
  * also every result the compiler inferred (`CompiledTypes`, rule T). A member written without a
  * type in a source that was not compiled has no result this reading knows: the compiler's
  * inference is not imitated.
  */
private[cardinality] object InferredResults {

  /** A result type, and the context it is read in. */
  final case class Inferred(tpe: Type, context: ResolutionContext)

  val Unresolved = "inferred result type not resolved"

  /** A target's result type and the context to read it in, where its source writes one. A
    * `transparent inline` method's call sites see its right-hand side's own type, which no reading
    * here gives.
    */
  def declared(target: Target, context: ResolutionContext): Either[String, Inferred] =
    target.result match {
      case Some(tpe)                                      => Right(Inferred(tpe, context))
      case None if TransparentResults.transparent(target) =>
        Left(s"$Unresolved: transparent inline method")
      case None => Left(s"$Unresolved: written without a type, in a source not compiled")
    }

  /** A literal's class, by a root-anchored path no declaration in scope can rebind: `true` and the
    * literal type `true` are a `Boolean`, never the name `Boolean` in scope.
    */
  private def literalClass(literal: Lit): Option[Type] =
    (literal match {
      case _: Lit.Boolean => Some("Boolean")
      case _: Lit.Int     => Some("Int")
      case _: Lit.Long    => Some("Long")
      case _: Lit.Double  => Some("Double")
      case _: Lit.Float   => Some("Float")
      case _: Lit.Char    => Some("Char")
      case _: Lit.String  => Some("String")
      case _: Lit.Unit    => Some("Unit")
      case _              => None
    }).map(rootedClass)

  /** The canonical keys of the types a literal has without a conversion: its class and the top
    * types above it.
    */
  def literalTypes(literal: Lit): Set[String] =
    literalClass(literal).flatMap(c => BuiltinTypes.canonical(c.syntax)).fold(Set.empty[String]) {
      own =>
        Set(
          own,
          "scala.Any",
          "scala.Matchable",
          if (own == "scala.String") "scala.AnyRef" else "scala.AnyVal"
        )
    }

  // A literal's class by its simple name: `String` is `java.lang`'s, the others `scala`'s.
  private def rootedClass(name: String): Type =
    rooted(if (name == "String") List("java", "lang", name) else List("scala", name))

  /** A stdlib class by its root-anchored path, which no declaration in scope can rebind. */
  def rooted(path: List[String]): Type = {
    val prefix = path.init.foldLeft(Term.Name(SourceIndex.Root): Term.Ref)((qual, segment) =>
      Term.Select(qual, Term.Name(segment))
    )
    Type.Select(prefix, Type.Name(path.last))
  }

}
