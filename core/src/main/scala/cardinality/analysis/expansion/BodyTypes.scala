package cardinality.analysis.expansion

import scala.meta.*

import cardinality.analysis.resolution.Found
import cardinality.analysis.{MethodAnalysis, Resolver}
import cardinality.types.Spelling
import cardinality.types.TypeSubstitution.{Meet, Union}

import MethodAnalysis.Frame

/** The types an inline body writes, read for what an expansion decides at its call site (register
  * D27): `scala.compiletime`'s operations and quoted types, context function types (whose values
  * implicit search applies where they are read), and a runtime class an expansion instantiates.
  * `paths` reads a type's qualifier as the walk reads a term path.
  */
final private[expansion] class BodyTypes(
    terms: BodyTerms,
    resolver: Resolver,
    paths: (Term, Scope) => Option[Meaning]
) {

  /** The first reason a type the body writes is read at the call site, if any. A type no value
    * depends on refuses nothing else.
    */
  def types(tpe: Type, scope: Scope): Option[String] =
    Option
      .when(contextual(tpe))(
        s"writes the context function type ${Spelling.of(tpe)}, whose values implicit search " +
          "applies where they are read"
      )
      .orElse(compileTimeType(tpe, scope))

  private def compileTimeType(tpe: Type, scope: Scope): Option[String] = tpe match {
    case _: Type.Macro                             => Some("uses a type splice")
    case Type.Name(name) if !scope.variables(name) =>
      compileTime(terms.named(name, scope.frame, tpe.pos.start))
    case s: Type.Select => paths(s.qual, scope).flatMap(compileTime)
    case other          =>
      other.children.iterator
        .flatMap {
          case t: Type => compileTimeType(t, scope)
          case _       => None
        }
        .nextOption()
  }

  private def compileTime(meaning: Meaning): Option[String] = meaning match {
    case found: Meaning.CompileTime => found.refusal
    case _                          => None
  }

  /** Whether a type spells a context function type, or names an alias of one (by name). */
  def contextual(tpe: Type): Boolean =
    tpe.collect {
      case _: Type.ContextFunction                               => true
      case named: (Type.Name | Type.Select) if aliasNamed(named) => true
    }.nonEmpty

  /** Whether a type is a context function type at its top, through annotations and aliases. */
  def topContext(tpe: Type): Boolean = tpe match {
    case _: Type.ContextFunction => true
    case Type.Annotate(inner, _) => topContext(inner)
    case other                   => aliasNamed(other)
  }

  private def aliasNamed(tpe: Type): Boolean = tpe match {
    case Type.Name(name)       => resolver.insertions.contextAliases(name)
    case selected: Type.Select => resolver.insertions.contextAliases(selected.name.value)
    case a: Type.Apply         => aliasNamed(a.tpe)
    case _                     => false
  }

  /** A type whose runtime class the body reads — a type test, or a class `classOf` reifies (`what`
    * says which). Scala substitutes the expansion's type arguments into an inline body (fixture a6
    * b1: `x.isInstanceOf[A]` is false for `isA[String](1)`, true in a method; review-A6 r2 c2:
    * `classOf[Array[A]]` is `Array[Int]`'s class in `intArr[Int]`), so a class that depends on a
    * type variable tells instantiations apart.
    */
  def reified(tpe: Type, scope: Scope, what: String): Option[String] =
    types(tpe, scope).orElse(
      Option.unless(fixed(tpe, scope, Set.empty))(
        s"$what ${Spelling.of(tpe)} at run time, which each expansion instantiates"
      )
    )

  // An array's runtime class is its element type's (fixture review-A6 h5: `Array[A]` reads `A`),
  // every other class's its own.
  private def fixed(tpe: Type, scope: Scope, seen: Set[String]): Boolean = tpe match {
    case Type.Name(name) if scope.variables(name) => false
    case _: (Type.Name | Type.Select)             =>
      denoted(tpe, scope.frame).exists(_.fold(fixedEntry(_, seen), _ => true))
    case a: Type.Apply =>
      fixed(a.tpe, scope, seen) &&
      (!array(a.tpe, scope.frame) || a.argClause.values.forall(fixed(_, scope, seen)))
    case _: (Type.Function | Type.Tuple | Type.Singleton) => true
    case Type.Annotate(inner, _)                          => fixed(inner, scope, seen)
    case Union(left, right) => fixed(left, scope, seen) && fixed(right, scope, seen)
    case Meet(left, right)  => fixed(left, scope, seen) && fixed(right, scope, seen)
    case _                  => false
  }

  private def array(head: Type, frame: Frame): Boolean =
    denoted(head, frame).exists(_.exists(_.stripPrefix("scala.") == "Array"))

  /** What a type name written in `frame` denotes: a supplied declaration, or a builtin's name. */
  def denoted(tpe: Type, frame: Frame): Option[Either[MethodAnalysis.TypeEntry, String]] =
    tpe match {
      case named: (Type.Name | Type.Select) =>
        terms.typeDeclaration(named, frame) match {
          case (Found.Entry(entry), _) => Some(Left(entry))
          case (Found.Absent, context) =>
            resolver.builtin(named.syntax, context).toOption.map(Right(_))
          case _ => None
        }
      case _ => None
    }

  // A class, trait, enum or object is one runtime class; an alias is what it stands for, read
  // where it is declared, with that scope's type parameters as variables.
  private def fixedEntry(entry: MethodAnalysis.TypeEntry, seen: Set[String]): Boolean =
    entry.tree match {
      case _: (Defn.Class | Defn.Trait | Defn.Enum | Defn.Object)                    => true
      case alias: Defn.Type if alias.tparamClause.values.isEmpty && !seen(entry.key) =>
        val around = entry.owner.chain.flatMap(_.typeParams).map(_.name.value).toSet
        fixed(alias.body, Scope(entry.owner, Map.empty, around), seen + entry.key)
      case _ => false
    }

}
