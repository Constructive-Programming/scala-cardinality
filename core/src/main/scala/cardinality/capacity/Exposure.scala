package cardinality.capacity

import scala.meta.*

import cardinality.types.*

/** What a clause written outside a namespace's body — an object's, or the top level of a package's
  * files — imports of its members. Scala imports through a wildcard every accessible member but a
  * given, through a `given` selector the given ones (Scala 3 reference, given imports); a private
  * or protected member only where it is accessible, which the estimate does not follow (Scala 3.9.0
  * binds `compose` under `import Lib.*` to another file's package when `Lib`'s is private:
  * probes/review-A7/round2/st6). Nor does it read the members the namespace's parents pass on or
  * its export clauses define (`inherits`, `exports`): a name the body does not define may be one of
  * them.
  */
final private[cardinality] case class Exposure(
    stats: List[Stat],
    inherits: Boolean,
    exports: Boolean
) {

  private lazy val restricted: Set[TypeName] = stats
    .filter(Exposure.limited)
    .flatMap(stat => Walk.typeNames(List(stat)) ++ Walk.termsOf(List(stat)).keySet)
    .toSet

  private lazy val givens: Set[TypeName] = stats.collect {
    case g: Defn.Given if g.name.value.nonEmpty      => TypeName.of(g.name.value)
    case g: Defn.GivenAlias if g.name.value.nonEmpty => TypeName.of(g.name.value)
    case g: Decl.Given if g.name.value.nonEmpty      => TypeName.of(g.name.value)
  }.toSet

  /** Why a wildcard does not read a name the body defines; None where it imports it. */
  def withheld(name: TypeName, wildcard: Wildcard): Option[String] =
    Option.when(restricted(name))(s"${Namespaces.mayBind(wildcard)}, a private member not followed")

  /** Whether a wildcard over the namespace imports a term the body defines that is not withheld. */
  def imports(name: TypeName, wildcard: Wildcard): Boolean = wildcard.givens || !givens(name)

  /** Why a wildcard over the namespace may bind a name its body does not define; None where it
    * binds nothing else.
    */
  def unread(wildcard: Wildcard): Option[String] =
    Option
      .when(inherits || exports)(
        if (!exports) "an inherited member"
        else if (!inherits) "an exported member"
        else "an inherited or exported member"
      )
      .map(member => s"${Namespaces.mayBind(wildcard)}, $member not followed")

}

private[cardinality] object Exposure {

  val none: Exposure = Exposure(Nil, inherits = false, exports = false)

  // A private or protected member: accessible only in some scopes.
  private def limited(stat: Stat): Boolean = stat match {
    case member: Stat.WithMods =>
      member.mods.exists(mod => mod.is[Mod.Private] || mod.is[Mod.Protected])
    case _ => false
  }

  /** An object's or package object's: its parents and its body. */
  def of(stats: List[Stat], parents: List[Init]): Exposure =
    Exposure(stats, parents.nonEmpty, stats.exists(_.is[Export]))

}
