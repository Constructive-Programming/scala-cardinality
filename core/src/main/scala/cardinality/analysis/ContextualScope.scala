package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import cardinality.analysis.model.ContextualEvidence
import cardinality.analysis.resolution.InheritedScopes
import cardinality.types.Spelling

import ContextualEvidence.{Candidate, Nominal}
import Inhabitation.Shape
import MethodAnalysis.{Frame, Resolved}

/** The contextual values a search inside the target can find, by nesting level along its chain
  * (outermost 0), from the outermost owner holding an anonymous parameter inward: a shallower value
  * never shadows one. Parameters, given and implicit definitions of those scopes and of their
  * supplied parents, imports and exports that can bring givens, and parents outside the source set
  * (whose givens are not read) all compete.
  */
final private[cardinality] class ContextualScope(
    owners: List[Frame],
    steps: List[InheritedScopes.Step],
    reader: ContextualScope.Reader
) {

  def candidates: List[Candidate] =
    placed
      .collectFirst {
        case place if place.owner.params.exists(ContextualEvidence.anonymous) => place.level
      }
      .toList
      .flatMap(first => placed.filter(_.level >= first).flatMap(at))

  // An extension's clauses are clauses of every method it declares (Scala desugars `extension
  // (x: A)(using B) def m(using C)` into `def m(x: A)(using B)(using C)`), so the group shares its
  // method's level and declaring site.
  private val placed: List[ContextualScope.Place] = {
    val levels = owners.scanLeft(0)((level, owner) => if (extension(owner)) level else level + 1)
    owners.zip(levels).zipWithIndex.map {
      case ((owner, level), index) =>
        val site = if (extension(owner)) owners.lift(index + 1).getOrElse(owner) else owner
        ContextualScope.Place(owner, level, site.id)
    }
  }

  private def extension(owner: Frame): Boolean = owner.tree.exists(_.is[Defn.ExtensionGroup])

  private def at(place: ContextualScope.Place): List[Candidate] = {
    val owner = place.owner
    val inherited = steps.filter(_.root.id == owner.id)
    parameters(place) ++
      reader
        .frames(owner)
        .flatMap(scope => definitions(scope.stats.filter(seen(scope, _)), scope, Map.empty, place))
      ++
      inherited.flatMap(step =>
        definitions(
          step.declared.stats,
          step.declared,
          step.env,
          place.copy(site = s"${place.site}<${step.declared.id}")
        )
      ) ++
      ((owner -> owner.parents) :: inherited.map(s => s.declared -> s.declared.parents))
        .flatMap((from, parents) => parents.filter(reader.unsupplied(_, from)))
        .map(parent =>
          Candidate(
            place.level,
            place.site,
            s"unsupplied parent ${Spelling.of(parent.tpe)}",
            None,
            false
          )
        )
  }

  // A block's givens are all in its scope, wherever they sit: a given written after a local
  // definition is the one its implicit search selects (a 3.9.0 fixture). Only values and imports
  // are positional.
  private def seen(scope: Frame, stat: Stat): Boolean =
    reader.visible(scope, stat) || (scope.local && ContextualEvidence.offered(stat).nonEmpty)

  // A parameter whose binding failed has already said why; its unread type is not an overlap. A
  // template's parameters are typed where its constructor reads them, outside its body and parents.
  private def parameters(place: ContextualScope.Place): List[Candidate] =
    place.owner.params.filter(ContextualEvidence.contextual).map { p =>
      val tpe = p.decltpe.fold("<untyped>")(_.syntax)
      val scope = ParameterScope.of(place.owner)
      Candidate(
        place.level,
        place.site,
        if (ContextualEvidence.anonymous(p)) tpe else s"${p.name.value}: $tpe",
        p.decltpe.flatMap(reader.read(_, scope, Map.empty)),
        ContextualEvidence.anonymous(p),
        reported = reader.bound(p, place.owner),
        nominal = p.decltpe.fold(Nominal(None, None))(reader.identify(_, scope, Map.empty))
      )
    }

  private def definitions(
      stats: List[Stat],
      scope: Frame,
      env: Map[String, Resolved],
      place: ContextualScope.Place
  ): List[Candidate] =
    stats.flatMap { stat =>
      def label = stat.syntax.linesIterator.nextOption().getOrElse("").trim
      ContextualEvidence
        .offered(stat)
        .map(tpe =>
          Candidate(
            place.level,
            place.site,
            label,
            tpe.flatMap(reader.read(_, scope, env)),
            false,
            nominal = tpe.fold(Nominal(None, None))(reader.identify(_, scope, env))
          )
        )
        .orElse(
          Option.when(ContextualEvidence.importsCandidates(stat))(
            Candidate(place.level, place.site, label, None, false)
          )
        )
        .toList
    }

}

private[cardinality] object ContextualScope {

  /** What the scope reads from the measurement: the frames sharing an owner's path, whether a
    * statement of a scope is visible from the target, a type read in a scope under an environment
    * (as a shape, and as the declarations it names), whether a parameter's binding already reported
    * its type, and whether a parent is outside the source set.
    */
  final case class Reader(
      frames: Frame => List[Frame],
      visible: (Frame, Stat) => Boolean,
      read: (Type, Frame, Map[String, Resolved]) => Option[Shape],
      identify: (Type, Frame, Map[String, Resolved]) => Nominal,
      bound: (Term.Param, Frame) => Boolean,
      unsupplied: (Init, Frame) => Boolean
  )

  /** An owner of the chain, its nesting level and the site that declares its parameters. */
  final case class Place(owner: Frame, level: Int, site: String)

}
