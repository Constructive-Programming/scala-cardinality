package cardinality.capacity

import scala.meta.*

import cardinality.analysis.model.ScopeTerms
import cardinality.types.TypeName

/** The cases an enum's companion holds (Scala 3 reference, enums: a class case is "a final case
  * class in `E`'s companion object"): a class case is a type and a term, a singleton case a term.
  * `import E.*`, `export E.*` and a bare name inside an explicit `object E` bind them over every
  * outer or other-file name (Scala 3.9.0 fixtures, `StoredScopesSpec`'s `enumCompanion`). A case
  * class is not read as a type of its own, so a type name that binds one is refused, as on the
  * method side.
  */
final private[capacity] case class EnumCompanions(
    types: Set[TypeName],
    terms: Set[TypeName],
    enumName: String
) {

  /** Why a type name that binds a class case has no number. */
  def refusal(name: TypeName): Definition.Unbound =
    Definition.Unbound.Refusal(s"enum case type not read: $enumName.${name.value}")

}

private[capacity] object EnumCompanions {

  private val none = EnumCompanions(Set.empty, Set.empty, "")

  /** The cases `o` holds: those of the enum of its name beside it, if it accompanies one. */
  def of(o: Defn.Object): EnumCompanions =
    o.parent.toList
      .flatMap(_.children)
      .collectFirst { case e: Defn.Enum if e.name.value == o.name.value => e }
      .fold(none)(cases)

  private def cases(e: Defn.Enum): EnumCompanions =
    EnumCompanions(
      ScopeTerms.classCases(e).map(TypeName.of).toSet,
      ScopeTerms.enumCases(e).map(TypeName.of).toSet,
      Qualified.of(e)
    )

  /** An enum's explicit companion, if it has one. */
  def companion(e: Defn.Enum): Option[Defn.Object] =
    e.parent.toList.flatMap(_.children).collectFirst {
      case o: Defn.Object if o.name.value == e.name.value => o
    }

  /** Whether an enum has a singleton case: a value of its companion, built with it. */
  def singletons(e: Defn.Enum): Boolean =
    (ScopeTerms.enumCases(e).toSet -- ScopeTerms.classCases(e)).nonEmpty

  /** The scope an enum's case parameters read in, from the scope around the enum: the companion's —
    * the explicit one's view, else the cases alone — with the case's copy of the enum's type
    * parameters innermost (Scala 3.9.0 builds `E.C(E.A(true))` for `case C(a: A)` beside another
    * file's `type A = Unit`: `StoredScopesSpec`'s `enumCases`).
    */
  def caseScope(scope: Scope, e: Defn.Enum): Scope = {
    val inside = companion(e).flatMap(Walk.moduleView(scope, _)).getOrElse(refusing(scope, e))
    Scope.bindersLens.modify(Counter.binders(e) ++ _)(inside)
  }

  /** The scope an enum's own body reads in: Scala 3 imports the cases into it by name (it rejects
    * `e.In(())` for a body's `In(a: A)`: `StoredScopesSpec`'s `enumCases`).
    */
  def bodyScope(scope: Scope, e: Defn.Enum): Scope = refusing(scope, e)

  // A scope where each class case's name binds the case, which is not read.
  private def refusing(scope: Scope, e: Defn.Enum): Scope = {
    val found = cases(e)
    if (found.types.isEmpty) scope
    else
      Scope.refusedLens.modify(_ ++ found.types.map(n => n -> found.refusal(n)))(
        scope.within(Layer(types = found.types))
      )
  }

}
