package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.model.{ScopeTerms, UniversalMembers}
import cardinality.analysis.{MethodAnalysis, Resolver, StdlibModel}
import cardinality.types.{Overrides, Spelling}

import MethodAnalysis.Frame

/** The term members a template inherits, as far as the sources tell them.
  *
  * Every class, trait and object, and the synthetic object that holds a file's top-level
  * definitions, has `Any` and `java.lang.Object` among its base classes and inherits their members
  * (`equals`, `hashCode`, `toString`, `clone`, …). A case class or case object also extends
  * `Product` and `Equals`, and an enum `scala.reflect.Enum` with both (Scala 3.9.0 fixtures:
  * `def productPrefix = 1` in a case class, `def canEqual(x: Any) = 2` in an enum and `def clone() =
  * new C` are typed against the inherited `String`, `Boolean` and `Object`). A supplied parent
  * passes on its definitions but its `private` ones; a parent outside the sources may declare any
  * name, except a marker the stdlib model knows to declare none and a parent whose term members it
  * lists.
  */
private[cardinality] object InheritedTerms {

  /** A parent that declares a term of the name (`certain`), or may: one this reading does not
    * enumerate, named as a reason spells it.
    */
  final case class Declaring(parent: String, certain: Boolean)

  /** The members of `Any` and `java.lang.Object`. */
  private val universal: Set[String] =
    UniversalMembers.any ++ UniversalMembers.anyRef ++ UniversalMembers.objectMethods

  private val products: Map[String, String] =
    (UniversalMembers.productValues ++ UniversalMembers.productReaders)
      .map(_ -> "Product")
      .toMap + ("canEqual" -> "Equals")

  /** How a template whose supplied ancestors are `levels` inherits a term `name`: from a parent
    * outside the sources (an implicit one included) or a supplied ancestor; None when no parent
    * declares it.
    */
  def inherits(
      template: Frame,
      levels: List[Frame],
      name: String,
      resolver: Resolver
  ): Option[Declaring] =
    foreign(template :: levels, name, resolver).orElse(supplied(levels, name))

  /** A parent outside the sources that declares or may declare a term `name` of a template whose
    * frame and supplied ancestors are `frames`: an implicit parent, or a written one.
    */
  def foreign(frames: List[Frame], name: String, resolver: Resolver): Option[Declaring] =
    implicitParent(frames, name).orElse(
      frames.iterator
        .flatMap(frame => frame.parents.flatMap(written(_, frame, name, resolver)))
        .nextOption()
    )

  /** A supplied ancestor that passes on a term `name`: a definition or a field (certain), or an
    * export clause, whose selectors may name a type only (uncertain).
    */
  private def supplied(levels: List[Frame], name: String): Option[Declaring] =
    levels.iterator
      .flatMap { level =>
        val label = level.path.lastOption.getOrElse("")
        if (ScopeTerms.inherited(level.stats)(name) || level.params.exists(field(_, name, level)))
          Some(Declaring(label, certain = true))
        else
          Option.when(ScopeTerms.exported(level.stats).forall(_(name)))(
            Declaring(label, certain = false)
          )
      }
      .nextOption()

  private def implicitParent(frames: List[Frame], name: String): Option[Declaring] =
    (if (universal(name)) Some("Any")
     else
       frames.flatMap(_.tree).collectFirst {
         case tree if caseLike(tree) && products.contains(name) => products(name)
         case _: Defn.Enum if name == "ordinal"                 => "scala.reflect.Enum"
       }).map(Declaring(_, certain = true))

  private def caseLike(tree: Tree): Boolean = tree match {
    case _: Defn.Enum        => true
    case stat: Stat.WithMods => stat.mods.exists(_.is[Mod.Case])
    case _                   => false
  }

  // A written parent outside the sources: a marker the model knows to declare no term, or one
  // whose term members it lists without the name, declares nothing here; a model summary that
  // lists the name declares it; any other may.
  private def written(
      init: Init,
      from: Frame,
      name: String,
      resolver: Resolver
  ): Option[Declaring] =
    resolver.outsideParent(init, from) match {
      case Some(parent) =>
        StdlibModel
          .parent(parent.path)
          .filter(summary => summary.listed && parent.shadow.isEmpty) match {
          case Some(summary) =>
            Option.when((summary.abstracts ++ summary.overridable)(name))(
              Declaring(parent.path, certain = true)
            )
          case None => Some(Declaring(parent.path, certain = false))
        }
      case None =>
        Option.when(resolver.unsuppliedParent(init, from))(
          Declaring(Spelling.of(init.tpe), certain = false)
        )
    }

  // A class parameter its subclasses inherit: a field that is not `private`.
  private def field(param: Term.Param, name: String, level: Frame): Boolean =
    FinalImplementations.field(param, name, level) && !Overrides.unqualifiedPrivate(param.mods)

  /** The values a supplied ancestor passes on that a wildcard (or, `givens`, a `given` selector)
    * over a subclass's object imports: its definitions and its fields, but private ones.
    */
  def imported(level: Frame, givens: Boolean): Set[String] =
    ScopeTerms.importable(level.stats, givens) ++ level.params.collect {
      case param
          if field(param, param.name.value, level) &&
            (!givens || param.mods.exists(mod => mod.is[Mod.Implicit] || mod.is[Mod.Using])) =>
        param.name.value
    }

  /** The values a supplied ancestor declares, which a path's first segment may name: its
    * definitions but objects, classes and enums, and its parameters.
    */
  def values(level: Frame): Set[String] =
    ScopeTerms.values(level.stats) ++ level.params.map(_.name.value)

}
