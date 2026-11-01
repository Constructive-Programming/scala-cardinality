package cardinality.analysis.inhabitation

import scala.annotation.tailrec

import cardinality.analysis.*

import Inhabitation.Shape
import Inhabitation.Shape.*

/** Proofs are constraints and identity-preserving transports, not opaque functions or fresh data.
  *
  * Endpoints are free atoms or rigid applications: structural shapes erase nominal identity, so
  * equality of their representations is not enough to prove Scala type equality. Available equality
  * proofs identify endpoints locally (a cycle of subtype proofs is one); subtype proofs instead add
  * directed, provenance-preserving views. Proofs appearing under a callable or a sum do not become
  * assumptions.
  *
  * A binder's bound to a closed structural type is the one proof with a structural side: an upper
  * bound views each of the binder's values as the bound, for elimination only, and a lower bound
  * lets the binder's goal be built as the bound.
  */
private[cardinality] object EvidenceAnalysis {

  def resolve(name: String, left: Shape, right: Shape): Either[String, Shape] =
    if (endpoint(left) && endpoint(right)) Right(Evidence(left, right, name.endsWith("=:=")))
    else
      Left(
        s"unsupported evidence relationship: $name requires free-atom or rigid-application endpoints"
      )

  /** A shape a proof may relate by identity: a free atom or a rigid application. */
  def endpoint(shape: Shape): Boolean = shape match {
    case _: Atom | _: Applied => true
    case _                    => false
  }

  /** The proofs a goal's environment makes available: how they rewrite a shape, the types a value
    * may be read as, the structural types a goal may be built as, and whether a goal's values — its
    * own values and the views read beside them — interact with the proofs in a way the solver
    * counts.
    */
  final case class Context(
      rewrite: Shape => Either[String, Shape],
      views: Shape => List[Shape],
      lowers: Shape => List[Shape],
      validate: (List[Shape], List[Shape], Shape) => Either[String, Unit]
  )

  def context(shapes: List[Shape]): Either[String, Context] =
    for {
      proofs <- AvailableProofs.extract(shapes)
      equality = new EqualityClosure(proofs)
      subtypes = new SubtypeClosure(proofs, equality)
      _ <- SupportedInteractions.consistent(subtypes)
    } yield Context(
      shape => equality.settle(shape).flatMap(discharged.get(_)(equality, subtypes)),
      subtypes.views,
      subtypes.lowers,
      SupportedInteractions.validate(_, _, _, subtypes)
    )

  private object AvailableProofs {

    private val endpointError =
      "unsupported evidence relationship: proofs require free-atom or rigid-application endpoints"

    // Only already available proofs participate: do not descend into sums or callables.
    def extract(shapes: List[Shape]): Either[String, List[Evidence]] = {
      val proofs = shapes.flatMap(Shape.evidencePrism.getOption)
      Shape.sequenceEithers(proofs.map(validate)).map(_ => proofs)
    }

    def validate(proof: Evidence): Either[String, Unit] =
      if (atomic(proof) || bound(proof)) Right(()) else Left(endpointError)

    // A structural side is a binder's bound: a subtype proof whose other side is the binder's atom.
    private def bound(proof: Evidence): Boolean = proof match {
      case Evidence(_: Atom, _, false) | Evidence(_, _: Atom, false) => true
      case _                                                         => false
    }

  }

  private def atomic(proof: Evidence): Boolean = endpoint(proof.left) && endpoint(proof.right)

  private def atomicSubtypes(proofs: List[Evidence]): List[Evidence] =
    proofs.filter(p => !p.equality && atomic(p))

  /** Reflexive transitive closure; equality supplies both directions, subtyping only one. */
  final private class Closure(edges: List[(Shape, Shape)]) {
    def reachable(start: Shape): List[Shape] = expand(Set(start)).toList.sortBy(_.toString)

    private def expand(found: Set[Shape]): Set[Shape] = {
      val next = found ++ edges.collect { case (from, to) if found(from) => to }
      if (next == found) found else expand(next)
    }

  }

  // Two endpoints each below the other are one type: `A >: B <: B` makes A and B equal.
  final private class EqualityClosure(proofs: List[Evidence]) {

    private val below = new Closure(atomicSubtypes(proofs).map(p => p.left -> p.right))

    private val cycles = atomicSubtypes(proofs).collect {
      case p if below.reachable(p.right).contains(p.left) => p.left -> p.right
    }

    private val closure = new Closure(
      (proofs.filter(_.equality).map(p => p.left -> p.right) ++ cycles)
        .flatMap((l, r) => List(l -> r, r -> l))
    )

    private val representatives = proofs
      .filter(atomic)
      .flatMap(p => List(p.left, p.right))
      .distinct
      .map(endpoint => endpoint -> closure.reachable(endpoint).head)
      .toMap

    def canonical(shape: Shape): Shape = representatives.getOrElse(shape, shape)

    def proves(proof: Evidence): Boolean = canonical(proof.left) == canonical(proof.right)

    // A representative may be an application whose own arguments another proof rewrites: rewrite
    // until nothing changes. Each step replaces an occurrence by its class's representative, which
    // depends on the class alone, so a rewriting that ends unfolds every class at most once along a
    // chain; one still changing after a step per class has met a class again and never ends
    // (`A =:= F[A]`, an equation no finite type solves for every F) — refused.
    def settle(shape: Shape): Either[String, Shape] =
      settled(shape, representatives.values.toSet.size + 1)

    @tailrec private def settled(shape: Shape, fuel: Int): Either[String, Shape] = {
      val next = canonicalized.get(shape)(this)
      if (next == shape) Right(shape)
      else if (fuel == 0)
        Left("unsupported evidence relationship: an equality proof keeps unfolding")
      else settled(next, fuel - 1)
    }

  }

  final private class SubtypeClosure(proofs: List[Evidence], equality: EqualityClosure) {

    private val edges = atomicSubtypes(proofs).map { proof =>
      equality.canonical(proof.left) -> equality.canonical(proof.right)
    }

    private val closure = new Closure(edges)

    private val upper = bounds(proofs.collect {
      case Evidence(atom: Atom, bound, false) if !endpoint(bound) => atom -> bound
    })

    private val lower = bounds(proofs.collect {
      case Evidence(bound, atom: Atom, false) if !endpoint(bound) => atom -> bound
    })

    private def bounds(pairs: List[(Shape, Shape)]): Map[Shape, List[Shape]] =
      pairs.groupMap((atom, _) => equality.canonical(atom))(_._2)

    val directed: Boolean = edges.exists { case (from, to) => from != to }

    val structural: Boolean = upper.nonEmpty

    /** Some binder bounded below by a structural type and some bounded above by one have a common
      * supertype (either may be that supertype).
      */
    val meeting: Boolean =
      lower.keys.exists(low =>
        upper.keys.exists(high => reachable(low).exists(reachable(high).contains))
      )

    /** Every atom whose values an upper bound views as a structural type. */
    val viewed: Set[Shape] =
      endpoints.filter(atom => reachable(atom).exists(upper.contains)).toSet

    def reachable(start: Shape): List[Shape] =
      closure.reachable(equality.canonical(start))

    /** The atoms whose values may be read as `top`: itself and every atom the proofs put below it.
      */
    def below(top: Shape): Set[Shape] = {
      val canonical = equality.canonical(top)
      (canonical :: endpoints).filter(reachable(_).contains(canonical)).toSet
    }

    private def endpoints: List[Shape] =
      upper.keys.toList ++ edges.flatMap((from, to) => List(from, to))

    /** Some atom's structural views hold a value of a type read through those views again (`A <:
      * P[A]`, or `A <:< B` beside `B <: P[A]`): reading the views would never end, and whether such
      * a type has any value at all is a fixpoint question the shapes do not answer. An
      * application's arguments are keys, not values, so `A <: P[F[A]]` holds no A.
      */
    val recursive: Boolean = {
      def holds(atom: Shape): Set[Shape] =
        views(atom)
          .filterNot(endpoint)
          .flatMap(valueAtoms)
          .map(equality.canonical)
          .toSet
          .intersect(viewed)
      def cyclic(atom: Shape, path: Set[Shape]): Boolean =
        path(atom) || holds(atom).exists(cyclic(_, path + atom))
      viewed.exists(cyclic(_, Set.empty))
    }

    /** A value's types: its own, each supertype the atomic proofs reach, and each structural upper
      * bound of those.
      */
    def views(start: Shape): List[Shape] = {
      val above = reachable(start)
      above ++ above.flatMap(upper.getOrElse(_, Nil))
    }

    /** The structural types a goal may be built as: the lower bounds of every atom below it. */
    def lowers(target: Shape): List[Shape] =
      lower.toList.flatMap { (atom, bounds) =>
        if (reachable(atom).contains(equality.canonical(target))) bounds else Nil
      }

    def proves(proof: Evidence): Boolean =
      views(proof.left).contains(equality.canonical(proof.right)) ||
        lowers(proof.right).contains(proof.left)

  }

  private object SupportedInteractions {

    // The empty sum is the absurdity goal every goal carries: it has no value to view.
    private val complexCheck =
      Shape.cata[Boolean] { (node, kids) =>
        node match {
          case _: Atom | _: Evidence | _: Applied | Sum(Nil) => false
          case Product(_)                                    => kids.exists(identity)
          case _                                             => true
        }
      }

    // Where a structural lower bound meets a structural upper one, a value viewed through the upper
    // bound and a construction through the lower one may be the same value (`x: A` in its `On` case
    // and the built `On`, for `A >: Flag <: Flag`), which the solver would count twice; and the two
    // bounds may admit no instantiation at all (`A >: Flag <: Boolean`: every call is rejected, a
    // vacuous context). Neither is decidable from shapes, which erase nominal identity. A view that
    // holds its own type again is never read to its end.
    def consistent(subtypes: SubtypeClosure): Either[String, Unit] =
      if (subtypes.meeting)
        Left(
          "unsupported subtype evidence interaction: a structural lower bound meets an upper one"
        )
      else if (subtypes.recursive)
        Left(
          "unsupported subtype evidence interaction: a structural view holds a value of its own type"
        )
      else Right(())

    /** Whether the solver counts a goal's values under the proofs: `values` are the goal's own,
      * `views` the supertypes they are read as beside them. A view of a sum is only ever split into
      * its case, which is a value of its own, so it is not a complex value here.
      */
    def validate(
        values: List[Shape],
        views: List[Shape],
        target: Shape,
        subtypes: SubtypeClosure
    ): Either[String, Unit] = {
      val read = values ++ views.filterNot(Shape.sumPrism.getOption(_).nonEmpty)
      if (subtypes.directed && (read :+ target).exists(complexCheck.get))
        Left(
          "unsupported subtype evidence interaction: only atomic values and products are supported"
        )
      else if (subtypes.structural && read.exists(producesViewed(_, subtypes.viewed)))
        Left(
          "unsupported subtype evidence interaction: a callable produces a value its bound views " +
            "as a structural type"
        )
      else if (subtypes.structural && sharedCase(values, target, subtypes))
        Left(
          "unsupported subtype evidence interaction: values a bound views as a structural type " +
            "may be the same case"
        )
      else Right(())
    }

    // Once its view is split, a viewed value is known by its case: two values of one case are one
    // value, not two producers of the goal. Producing a viewed type from two or more values that may
    // be read as it is not read; a single one is. A view is the value it views, never a second one.
    private def sharedCase(values: List[Shape], target: Shape, subtypes: SubtypeClosure): Boolean =
      subtypes.viewed.exists { atom =>
        mentions(target, Set(atom)) && values.count(held(_, subtypes.below(atom))) > 1
      }

    // A value whose own data holds a viewed atom: a callable's parameters and a proof's endpoints
    // are not its data.
    private def held(shape: Shape, viewed: Set[Shape]): Boolean =
      occurs(shape, viewed) {
        case _: Function | _: Evidence | _: Applied => true
        case _                                      => false
      }

    // A value an upper bound views is eliminated as its bound; one a callable produces would need
    // the callable's output eliminated, which the solver does not do for opaque results.
    private def producesViewed(shape: Shape, viewed: Set[Shape]): Boolean =
      Shape.exists(shape) {
        case Function(_, result) => mentions(result, viewed)
        case _                   => false
      }

    // An application's arguments are keys, never values: `F[A]` is not viewed through A's bound.
    private def mentions(shape: Shape, viewed: Set[Shape]): Boolean =
      occurs(shape, viewed)(_.isInstanceOf[Applied])

  }

  // A viewed atom in a shape, below no node `opaque` stops at.
  private def occurs(shape: Shape, viewed: Set[Shape])(opaque: Shape => Boolean): Boolean =
    shape match {
      case stop if opaque(stop) => false
      case atom: Atom           => viewed(atom)
      case other                => Shape.children(other).exists(occurs(_, viewed)(opaque))
    }

  /** The atoms a shape holds as values: an application's arguments are keys, not values. */
  def valueAtoms(shape: Shape): List[Shape] = shape match {
    case atom: Atom => List(atom)
    case _: Applied => Nil
    case other      => Shape.children(other).flatMap(valueAtoms)
  }

  private val canonicalized =
    Shape.cata[EqualityClosure => Shape] { (node, kids) => equality =>
      node match {
        case atom: Atom               => equality.canonical(atom)
        case existential: Existential =>
          ExistentialInputs.mapAtoms(existential)(atom => equality.canonical(atom))
        case applied: Applied => equality.canonical(Shape.rebuild(applied, kids)(_(equality)))
        case _                => Shape.rebuild(node, kids)(_(equality))
      }
    }

  private val discharged =
    Shape.cata[(EqualityClosure, SubtypeClosure) => Either[String, Shape]] {
      (node, kids) => (equality, subtypes) =>
        node match {
          case proof: Evidence => rewriteProof(proof, equality, subtypes)
          case _               => Shape.tryRebuild(node, kids.map(_(equality, subtypes)))
        }
    }

  private def rewriteProof(
      proof: Evidence,
      equality: EqualityClosure,
      subtypes: SubtypeClosure
  ): Either[String, Shape] =
    AvailableProofs.validate(proof).flatMap { _ =>
      val proved = if (proof.equality) equality.proves(proof) else subtypes.proves(proof)
      if (proved) Right(Product(Nil))
      else Left("unsupported evidence constraint: no available proof for distinct binders")
    }

}
