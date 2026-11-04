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
  * (outermost 0), from the outermost owner holding an anonymous parameter or binder inward: a
  * shallower value never shadows one. Parameters, given and implicit definitions of those scopes
  * and of their supplied parents, imports and exports that can bring givens, and parents outside
  * the source set (whose givens are not read) all compete, and so do the binders a definition's
  * context-function result puts around its body (`ContextBinders`): between its parameters and
  * everything nested in it, each clause of the chain one level deeper than the last. An enclosing
  * definition's binders are anonymous evidence, reached only through implicit search; the target's
  * own are the arguments of its result, which an explicit literal names.
  */
final private[cardinality] class ContextualScope(
    owners: List[Frame],
    steps: List[InheritedScopes.Step],
    reader: ContextualScope.Reader
) {

  def candidates: List[Candidate] =
    placed
      .collectFirst { case place if anonymous(place) => place.level }
      .toList
      .flatMap(first => placed.filter(_.level >= first).flatMap(at))

  private val target = owners.lastOption.map(_.id)

  private def anonymous(place: ContextualScope.Place): Boolean =
    place.owner.params.exists(ContextualEvidence.anonymous) || enclosing(place.owner)

  private def enclosing(owner: Frame): Boolean =
    !target.contains(owner.id) && reader.binders(owner).nonEmpty

  // An extension's clauses are clauses of every method it declares (Scala desugars `extension
  // (x: A)(using B) def m(using C)` into `def m(x: A)(using B)(using C)`), so the group shares its
  // method's level and declaring site. Each owner's binder clauses take the levels after its own,
  // and what is nested in it the next.
  private val placed: List[ContextualScope.Place] =
    owners.zipWithIndex
      .foldLeft((List.empty[ContextualScope.Place], 0)) {
        case ((places, level), (owner, index)) =>
          val site = if (extension(owner)) owners.lift(index + 1).getOrElse(owner) else owner
          val next = if (extension(owner)) level else level + clauses(owner) + 1
          (ContextualScope.Place(owner, level, site.id) :: places, next)
      }
      ._1
      .reverse

  private def clauses(owner: Frame): Int =
    reader.binders(owner).map(_.clause + 1).maxOption.getOrElse(0)

  private def extension(owner: Frame): Boolean = owner.tree.exists(_.is[Defn.ExtensionGroup])

  private def at(place: ContextualScope.Place): List[Candidate] = {
    val owner = place.owner
    val inherited = InheritedScopes.of(steps, owner)
    parameters(place) ++ binders(place) ++
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

  // Binders of one clause share a level and a site, so two of one type are ambiguous; an unread
  // type is reported where the binder is bound (or, for the target's own, with its goal).
  private def binders(place: ContextualScope.Place): List[Candidate] = {
    val synthetic = enclosing(place.owner)
    reader.binders(place.owner).map { binder =>
      Candidate(
        place.level + 1 + binder.clause,
        s"${place.site}?=>${binder.clause}",
        binder.label,
        binder.shape.toOption,
        synthetic,
        reported = binder.shape.isLeft,
        nominal = binder.nominal,
        results = binder.results
      )
    }
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
      val (shape, results) = typed(p.decltpe, scope, Map.empty)
      Candidate(
        place.level,
        place.site,
        if (ContextualEvidence.anonymous(p)) tpe else s"${p.name.value}: $tpe",
        shape,
        ContextualEvidence.anonymous(p),
        reported = reader.bound(p, place.owner),
        nominal = p.decltpe.fold(Nominal(None, None))(reader.identify(_, scope, Map.empty)),
        results = results
      )
    }

  // A candidate's type read in a scope, and the results of the context functions it starts with.
  private def typed(
      tpe: Option[Type],
      scope: Frame,
      env: Map[String, Resolved]
  ): (Option[Shape], List[Shape]) = {
    val shape = tpe.flatMap(reader.read(_, scope, env))
    (shape, tpe.zip(shape).toList.flatMap((t, s) => reader.results(t, scope, env, s)))
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
        .map { tpe =>
          val (shape, results) = typed(tpe, scope, env)
          Candidate(
            place.level,
            place.site,
            label,
            shape,
            false,
            nominal = tpe.fold(Nominal(None, None))(reader.identify(_, scope, env)),
            results = results
          )
        }
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
    * (as a shape, as the results of the context functions it starts with, and as the declarations
    * it names), whether a parameter's binding already reported its type, whether a parent is
    * outside the source set, and the context-function binders an owner's result puts around its
    * body.
    */
  final case class Reader(
      frames: Frame => List[Frame],
      visible: (Frame, Stat) => Boolean,
      read: (Type, Frame, Map[String, Resolved]) => Option[Shape],
      results: (Type, Frame, Map[String, Resolved], Shape) => List[Shape],
      identify: (Type, Frame, Map[String, Resolved]) => Nominal,
      bound: (Term.Param, Frame) => Boolean,
      unsupplied: (Init, Frame) => Boolean,
      binders: Frame => List[ContextBinders.Read]
  )

  /** An owner of the chain, its nesting level and the site that declares its parameters. */
  final case class Place(owner: Frame, level: Int, site: String)

}
