package cardinality.analysis.inhabitation

import scala.meta.*

import cardinality.types.{Kind, Kinds, Traversals}
import dev.constructive.eo.optics.Getter

import GoalSolver.Producer
import Inhabitation.{Binder, Head, Shape}
import Shape.*

/** The type parameters of a supplied capability's member (capabilities C8, PB0–PB4).
  *
  * Supplied members are parametric (register D3), so a binder is not a free choice of type: free
  * theorems identify terms that instantiate it differently, and naive instantiation can count a
  * known identity as a generator of infinitely many terms. Two readings are exact and done where
  * the member is read, for an input and a goal alike, since both are type isomorphisms:
  *
  *   - PB0: a binder the member's type never mentions is irrelevant; any instantiation gives the
  *     same monomorphic type, once its bounds admit one (`close`).
  *   - PB1 (Yoneda): a binder met only as a generator `P → M` (a bare `M` is `1 → M`) and at
  *     positive positions of the rest `G(M) = Rest → Result` is the sum of its generators' domains:
  *     `∀M. (P₁ → M) → … → (Pₖ → M) → G(M) ≅ G(P₁ + … + Pₖ)` for covariant `G`. So `∀M. (A → M) → S
  *     → M` is `S → A`, `∀C. C → C` is `1`, `∀M. M → M → M` is the two-valued tag and `∀C. C` is
  *     the empty type: a member of that type makes the environment vacuous.
  *
  * Anything else stays polymorphic (`Shape.Poly`). The solver instantiates such an input only where
  * matching its result against a goal fixes every binder (PB2): where no result is a bare binder,
  * every normal form applies it at those instantiations, so the naive count bounds the true one
  * from above (quotienting only merges) and only 0 or 1 is reported; a bare-binder result may be
  * instantiated at a compound type and eliminated, so beside one no finite count is (`uncovered`).
  * A member whose result meets no goal is irrelevant there (PB3); one whose binders the goal does
  * not fix keeps a reason (PB4).
  */
private[cardinality] object PolymorphicMembers {

  /** How a member's binder is read in its type: a first-order one is its own atom; a higher-kinded
    * one is the rigid constructor `rigid` binds (`TypeOperators.rigid`) — the member's type applies
    * it uninterpreted, exactly as a method's own higher-kinded binder reads.
    */
  def binder(
      member: String,
      parameter: Type.Param,
      rigid: (String, Kind) => Shape
  ): Either[String, Shape] = {
    val holes = parameter.tparamClause.values
    if (holes.isEmpty) Right(Atom(Binder(member, parameter.name.value).id))
    else Right(rigid(Binder(member, parameter.name.value).id, Kinds.of(parameter)))
  }

  /** A member's type with its binders read (PB0, PB1), or kept for the solver. A bound restricts
    * the instantiations a reading would need — a context or view bound is an evidence parameter the
    * member's clauses do not spell — so a bounded binder is read only when its type never meets it.
    * Even then the bound must be met by some type, or no call of the member typechecks: a one-sided
    * bound always is (`Nothing` below an upper bound, `Any` above a lower one, whatever other
    * binders it mentions), a two-sided one need not be (`M >: Int <: String`, or `M >: X <: String`
    * for a rigid `X`), so it is refused rather than dropped.
    */
  def close(member: String, parameters: List[Type.Param], shape: Shape): Either[String, Shape] = {
    val binders = parameters.map(p => p -> Binder(member, p.name.value))
    val occurring = binders.filter((_, binder) => occurs(shape, binder))
    val applied = binders.find((_, binder) => appliesResult(shape, binder))
    val evidence = parameters.find(p => (p.bounds.context ++ p.bounds.view).nonEmpty)
    val twoSided = parameters.find(p => p.bounds.lo.nonEmpty && p.bounds.hi.nonEmpty)
    val bounded = occurring.collectFirst {
      case (p, _) if p.bounds.lo.nonEmpty || p.bounds.hi.nonEmpty => p
    }
    evidence.orElse(twoSided).orElse(bounded) match {
      case Some(p) => Left(s"polymorphic capability binder: ${p.name.value} (bounded)")
      case None    =>
        applied match {
          case Some((p, _)) =>
            Left(
              s"polymorphic capability binder: ${p.name.value} (the result instantiates the binder)"
            )
          case None =>
            val kept = occurring.map(_._2)
            Right(if (kept.isEmpty) shape else yoneda(kept, shape).getOrElse(Poly(kept, shape)))
        }
    }
  }

  /** Whether the callable's result applies the binder's rigid constructor: a caller instantiates
    * the binder there, and the solver's terms from such a member never search the instantiation
    * (`polymorphic capability binder: G (the result instantiates the binder)`). A binder the result
    * never applies leaves the member's terms exact: its occurrences stand in parameters a counted
    * implementation can only fill from its own environment, which holds no value of a rigid
    * binder's application.
    */
  private def appliesResult(shape: Shape, binder: Binder): Boolean = {
    val (_, result) = Shape.uncurried.get(shape)
    applies(result, binder.id)
  }

  private def applies(shape: Shape, id: String): Boolean = shape match {
    case Applied(Head.Rigid(`id`), _) => true
    case other                        => Shape.children(other).exists(applies(_, id))
  }

  def occurs(shape: Shape, binder: Binder): Boolean = mentions(shape, binder.id)

  private def mentions(shape: Shape, id: String): Boolean = Shape.exists(shape)(_ == Atom(id))

  // PB1: the generators of each binder, removed, and the binder replaced by the sum of their
  // domains — when every binder is covariant in what remains.
  private def yoneda(binders: List[Binder], shape: Shape): Option[Shape] = {
    val ids = binders.map(_.id)
    val bound = ids.toSet
    val (parameters, result) = Shape.uncurried.get(shape)
    val (generators, rest) = parameters.flatMap(spread).partitionMap { parameter =>
      generator(parameter, bound).toLeft(parameter)
    }
    val reduced = if (rest.isEmpty) result else Function(rest, result)
    Option.when(
      ids.forall(b => polarities(reduced, b, positive = true).exists(_.forall(identity)))
    ) {
      val sums = ids.map(b => b -> alternatives(generators.collect { case (`b`, d) => d })).toMap
      normalized.get(ExistentialInputs.mapAtoms(reduced)(atom => sums.getOrElse(atom.id, atom)))
    }
  }

  // A tuple parameter is its components, uncurried: `(A, B) → C ≅ A → B → C`.
  private def spread(parameter: Shape): List[Shape] = parameter match {
    case Product(fields) => fields.flatMap(spread)
    case other           => List(other)
  }

  // `P → M` with `P` free of every binder, or a bare `M` (`1 → M`): its binder and domain.
  private def generator(parameter: Shape, bound: Set[String]): Option[(String, Shape)] =
    parameter match {
      case Atom(id) if bound(id) =>
        Some(id -> Product(Nil))
      case Function(domain, Atom(id)) if bound(id) && domain.forall(free(_, bound)) =>
        Some(id -> (domain match {
          case List(single) => single
          case several      => Product(several)
        }))
      case _ => None
    }

  private def free(shape: Shape, bound: Set[String]): Boolean =
    !Shape.exists(shape) {
      case Atom(id) => bound(id)
      case _        => false
    }

  private def alternatives(domains: List[Shape]): Shape = domains match {
    case List(single) => single
    case several      => Sum(several)
  }

  // The polarities a binder occurs at — a function parameter flips — or none where it sits under
  // a constructor whose variance this reading does not know (a rigid application, a sequence, a
  // proof, a package, a singleton or another member's binders). One pass: a part without the binder
  // joins as no polarity, so only a constructor the walk does not enter is searched for it.
  private def polarities(shape: Shape, binder: String, positive: Boolean): Option[Set[Boolean]] =
    shape match {
      case Atom(`binder`)               => Some(Set(positive))
      case Function(parameters, result) =>
        joined(
          parameters.map(polarities(_, binder, !positive)) :+ polarities(result, binder, positive)
        )
      case Product(parts)                   => joined(parts.map(polarities(_, binder, positive)))
      case Sum(parts)                       => joined(parts.map(polarities(_, binder, positive)))
      case other if mentions(other, binder) => None
      case _                                => Some(Set.empty)
    }

  private def joined(parts: List[Option[Set[Boolean]]]): Option[Set[Boolean]] =
    Traversals.defined(parts).map(_.flatten.toSet)

  // A Unit parameter adds nothing (`1 → A ≅ A`), and a callable of none is its result.
  private val normalized: Getter[Shape, Shape] = Shape.cata[Shape] { (node, kids) =>
    Shape.rebuild(node, kids)(identity) match {
      case Function(parameters, result) =>
        val kept = parameters.filterNot(_ == Product(Nil))
        if (kept.isEmpty) result else Function(kept, result)
      case other => other
    }
  }

  /** What a polymorphic input's producer path offers a goal (PB2–PB4). */
  enum Offer {

    /** Its result meets the goal under no instantiation: it heads no rule here (PB3). */
    case Irrelevant

    /** The path mentions none of the member's binders: an ordinary producer (PB0). */
    case Monomorphic

    /** The goal fixes every binder: an application at those types, whose count is exact only at 0
      * or 1 (PB2).
      */
    case Instantiated(instance: Producer, quotient: String)

    /** The member is relevant but the goal fixes no instantiation that covers its uses (PB4). */
    case Refused(reason: String)
  }

  /** Why goal-fixed instantiations do not cover a member: a path whose result is one of its binders
    * may be instantiated at a compound type and eliminated afterwards, which matching at goals
    * never searches (PB2's boundary).
    */
  def uncovered(poly: Poly): Option[String] =
    GoalSolver.producerPaths
      .get(poly.body)(Nil)
      .flatMap(producer => poly.binders.find(b => producer.output == Atom(b.id)))
      .headOption
      .map(reason(_, "instantiations at compound types not searched for"))

  /** Whether a producer path of a member mentions none of its binders. */
  def monomorphic(binders: List[Binder], producer: Producer): Boolean =
    mentioned(binders, producer).isEmpty

  /** What one producer path of a polymorphic member — its parameters and output, binders free —
    * offers `target`. A path whose output is eliminated rather than matched (a sum, a ground value,
    * a sequence, an existential) is relevant to every goal and fixed by none.
    */
  def offer(binders: List[Binder], producer: Producer, target: Shape): Offer =
    mentioned(binders, producer) match {
      case Nil                                       => Offer.Monomorphic
      case named :: _ if eliminated(producer.output) => Offer.Refused(undetermined(named))
      case bound                                     => instantiated(bound, producer, target)
    }

  // A path mentioning `bound`, its output matched against the goal (PB2–PB4).
  private def instantiated(bound: List[Binder], producer: Producer, target: Shape): Offer =
    matching(producer.output, target, bound.map(_.id).toSet).fold(Offer.Irrelevant) { fixed =>
      val instance = Producer(producer.parameters.map(at(fixed)), at(fixed)(producer.output))
      bound.find(b => !fixed.contains(b.id)) match {
        case Some(free)                          => Offer.Refused(undetermined(free))
        case None if eliminated(instance.output) => Offer.Refused(quotient(bound.head))
        case None => Offer.Instantiated(instance, quotient(bound.head))
      }
    }

  private def mentioned(binders: List[Binder], producer: Producer): List[Binder] =
    binders.filter(b => (producer.output :: producer.parameters).exists(occurs(_, b)))

  private def at(fixed: Map[String, Shape])(shape: Shape): Shape =
    ExistentialInputs.mapAtoms(shape)(a => fixed.getOrElse(a.id, a))

  // A result the solver eliminates by cases rather than matches against a goal.
  private def eliminated(output: Shape): Boolean = output match {
    case Sum(alternatives)                                  => alternatives.nonEmpty
    case _: Ground | _: Repeated | _: Existential | _: Poly => true
    case _                                                  => false
  }

  // First-order matching: the binders in `pattern` are variables, every other node must agree.
  private def matching(
      pattern: Shape,
      target: Shape,
      bound: Set[String]
  ): Option[Map[String, Shape]] =
    pattern match {
      case Atom(id) if bound(id)           => Some(Map(id -> target))
      case _ if free(pattern, bound)       => Option.when(pattern == target)(Map.empty)
      case _: (Existential | Poly)         => None
      case _ if !sameNode(pattern, target) => None
      case _                               =>
        val pairs = Shape.children(pattern).zip(Shape.children(target))
        pairs.foldLeft(Option(Map.empty[String, Shape])) {
          case (found, (p, t)) =>
            for {
              sofar <- found
              next <- matching(p, t, bound)
              if next.forall((b, s) => sofar.get(b).forall(_ == s))
            } yield sofar ++ next
        }
    }

  // The same node with the same non-shape fields, whatever its children.
  private def sameNode(pattern: Shape, target: Shape): Boolean = {
    val kids = Shape.children(target)
    Shape.children(pattern).size == kids.size &&
    Shape.rebuild(pattern, kids)(identity) == target
  }

  private def undetermined(binder: Binder): String =
    reason(binder, "not determined by the result of")

  private def quotient(binder: Binder): String =
    reason(binder, "free-theorem quotient not modelled for")

  private def reason(binder: Binder, why: String): String =
    s"polymorphic capability binder: ${binder.name} ($why ${binder.member})"

}
