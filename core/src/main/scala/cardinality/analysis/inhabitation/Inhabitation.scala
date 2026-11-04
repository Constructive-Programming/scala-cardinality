package cardinality.analysis.inhabitation

import cardinality.capacity.{EffectiveOmega, FinitePart, Size, TinySize}
import cardinality.types.Traversals
import dev.constructive.eo.data.PSVec
import dev.constructive.eo.generics.prism
import dev.constructive.eo.optics.{Getter, Plated}
import dev.constructive.eo.schemes.Schemes

/** Counts observationally distinct inhabitants in a restricted pure, total parametric calculus.
  *
  * Supported: free atoms, products, finite sums in the context, arrow introduction, and reusable
  * first-order producers (including curried ones and projections of product results). Products are
  * eta-expanded; Unit has one implementation. Sum contexts are exhaustively split, with independent
  * implementations in each branch. Provenance distinguishes opaque values.
  *
  * Sequences use finite sequence introduction (Nil/Cons); a supplied one is eliminated by length
  * (`SequenceElimination`) where it is a value of the environment, and refused elsewhere.
  *
  * A supplied callable with a sum or ground result is eliminated once per argument tuple and path
  * (`NeutralSaturation`); the isolated-observation certificate in OpaqueSumForwarding answers where
  * nothing was eliminated yet. A supplied member's own type parameters are read by
  * `PolymorphicMembers`. TerminalHigherOrder certifies terminal atomic higher-order outputs and
  * first-order functional arguments. Other elimination and higher-order application remain
  * unresolved when potentially relevant. No equations on opaque functions, effects, recursion, or
  * casts are assumed. The search itself is `GoalSolver`; its rule graph is counted by `Live`.
  */
object Inhabitation {

  enum Shape {
    case Atom(id: String)
    // A validated accessible stable reference has one value, not one interchangeable Unit.
    // Its widened binding remains usable, with the reference's original provenance.
    case Singleton(id: String, underlying: Option[Shape])
    case Product(fields: List[Shape])
    case Sum(alternatives: List[Shape])
    case Function(parameters: List[Shape], result: Shape)
    case Repeated(element: Shape)
    case Evidence(left: Shape, right: Shape, equality: Boolean)
    case Existential(witnesses: List[String], body: Shape)
    // A transparent scalar: every value is a literal and the type's own equality tells them apart,
    // so a supplied one is split by case analysis and a goal of it is built from its literals.
    case Ground(name: String, size: Card)
    // A rigid type constructor applied to nominal argument keys: no constructor and no eliminator,
    // so a value of it is produced only by one of the same head over the same keys.
    case Applied(head: Head, arguments: List[Shape])
    // A name bound to a type constructor in a binding environment, never a value type.
    case Constructor(token: String)
    // A supplied member's own type parameters over its type (`∀M. τ`): an input is instantiated at
    // the goals its result meets, a goal is built for fresh rigid binders. Binder atoms are unique to
    // their member, so they are never captured (`PolymorphicMembers`).
    case Poly(binders: List[Binder], body: Shape)
  }

  /** A supplied member's own type parameter, named by its member's identity and its own name: the
    * atom it is read as is unique to that member.
    */
  final case class Binder(member: String, name: String) {
    val id: String = s"∀$member:$name"
  }

  /** The head of an application: a rigid binder, a declared or builtin type (a key's head), or —
    * inside an operator's key — a type lambda and its parameters, numbered so that α-equivalent
    * lambdas get one key.
    */
  enum Head {
    case Rigid(binder: String)
    case Nominal(declaration: String)
    case Lambda(id: Int, arity: Int)
    case Bound(id: Int, index: Int)
  }

  object Shape {

    given platedShape: Plated[Shape] = cardinality.types.TypeSubstitution.plated {
      case Singleton(id, Some(u))       => List(u) -> (kids => Singleton(id, Some(kids.head)))
      case Product(fields)              => fields -> Product.apply
      case Sum(alternatives)            => alternatives -> Sum.apply
      case Function(parameters, result) =>
        (parameters :+ result) -> (kids => Function(kids.take(parameters.size), kids.last))
      case Repeated(element)         => List(element) -> (kids => Repeated(kids.head))
      case Evidence(left, right, eq) =>
        List(left, right) -> (kids => Evidence(kids.head, kids(1), eq))
      case Existential(w, body) => List(body) -> (kids => Existential(w, kids.head))
      case Poly(binders, body)  => List(body) -> (kids => Poly(binders, kids.head))
      case Applied(head, args)  => args -> (kids => Applied(head, kids))
      case leaf                 => Nil -> (_ => leaf)
    }

    /** A rigid binder applied to its arguments' keys. */
    def rigid(binder: String, keys: List[Shape]): Shape = Applied(Head.Rigid(binder), keys)

    def cata[A](alg: (Shape, Iterable[A]) => A): Getter[Shape, A] =
      Schemes.cata[Shape, A]((node, kids) => alg(node, kids.toList))

    def rebuild[A](node: Shape, kids: Iterable[A])(f: A => Shape): Shape =
      platedShape.rebuild(node, PSVec.from(kids.iterator.map(f)))

    def children(shape: Shape): List[Shape] =
      platedShape.childrenVec(shape).toList

    def exists(shape: Shape)(predicate: Shape => Boolean): Boolean =
      cardinality.types.Universe.exists(shape)(predicate)

    val atomPrism = prism[Shape, Atom]
    val functionPrism = prism[Shape, Function]
    val sumPrism = prism[Shape, Sum]
    val evidencePrism = prism[Shape, Evidence]
    val repeatedPrism = prism[Shape, Repeated]

    def tryRebuild(node: Shape, kids: Iterable[Either[String, Shape]]): Either[String, Shape] =
      Traversals.sequence(kids).map(rebuild(node, _)(identity))

    def freshName(prefix: String, start: Int = 0)(taken: String => Boolean): (String, Int) = {
      val index = Iterator.from(start).find(i => !taken(s"$prefix$i")).get
      s"$prefix$index" -> (index + 1)
    }

    val dataAtoms: Getter[Shape, Option[List[Atom]]] =
      cata[Option[List[Atom]]] { (node, kids) =>
        node match {
          case a: Atom    => Some(List(a))
          case Product(_) => Traversals.defined(kids).map(_.flatten)
          case _          => None
        }
      }

    val uncurried: Getter[Shape, (List[Shape], Shape)] =
      cata[(List[Shape], Shape)] { (node, kids) =>
        node match {
          case Function(parameters, _) =>
            val (rest, leaf) = kids.last
            (parameters ++ rest, leaf)
          case leaf => (Nil, leaf)
        }
      }

  }

  /** A value of the environment: its identity, its type, and the source name a reason gives it
    * (none for a value the analysis introduces).
    */
  case class Binding(id: String, shape: Shape, label: Option[String] = None)

  enum Count {
    case Finite(value: BigInt)

    /** An exact count too large for a literal: `2^(2^37)` for an `Int` endomorphism. */
    case Symbolic(value: Natural)
    case Countable

    /** A value space the estimate read: the capacity `≤2^32` for an `Int`, `ω` for a sealed family,
      * `ε₀` for a function space. The size's own rendering, bound and not proof — an exact count
      * still renders as `Finite`.
      */
    case Bounded(size: Size)
    case Unresolved(reasons: List[String])

    def render: String = this match {
      case Finite(value)   => Natural.Lit(value).render
      case Symbolic(value) => value.render
      case Countable       => "ω"
      case Bounded(size)   => size.render
      case Unresolved(_)   => "?"
    }

    def isFinite: Boolean = this match {
      case Finite(_) | Symbolic(_)   => true
      case Bounded(space)            => !space.hasInfinite
      case Countable | Unresolved(_) => false
    }

    def isCountable: Boolean = this match {
      case Countable                               => true
      case Bounded(space)                          => space.hasInfinite && space.epsilon == 0
      case Finite(_) | Symbolic(_) | Unresolved(_) => false
    }

    /** Above countable: the ε₀ tier the size algebra places a function space over a countable type.
      */
    def isBeyondCountable: Boolean = this match {
      case Bounded(space) => space.epsilon > 0
      case _              => false
    }

  }

  object Count {

    /** The exact count a solver's card stands for. */
    def of(card: Card): Count = card match {
      case Card.Exact(Natural.Lit(value)) => Finite(value)
      case Card.Exact(value)              => Symbolic(value)
      case Card.Omega                     => Countable
    }

    /** The count a constructor admits: the product of its positions' value spaces. Exact spaces
      * multiply exactly with the shape model's count of the remaining positions; the positions only
      * the estimate read multiply as the size algebra multiplies spaces — `2 × ω = ω`, a tier
      * beside a higher tier answers the higher. A position that stayed unresolved leaves the
      * product unresolved: a number behind an unreadable name is not a count.
      */
    def product(exact: List[Card], estimated: List[Size], implementations: Count): Count =
      implementations match {
        case Unresolved(reasons) => Unresolved(reasons)
        case solved              =>
          Natural
            .attempt {
              val card = exact.foldLeft(cardOf(solved))(_ * _)
              if (estimated.isEmpty) of(card)
              else
                sizeOf(card, estimated.exists(_.hasInfinite)).fold(
                  Unresolved(List(s"constructor space has no estimate form: ${of(card).render}"))
                )(space => bounded(estimated.foldLeft(space)(_ * _)))
            }
            .fold(reason => Unresolved(List(reason)), identity)
      }

    // The solver answers with an exact count, ω or a reason, never with an estimate's bound.
    private def cardOf(count: Count): Card = count match {
      case Finite(value)              => Card.exact(value)
      case Symbolic(value)            => Card.Exact(value)
      case Countable                  => Card.Omega
      case Bounded(_) | Unresolved(_) =>
        throw new IllegalArgumentException(s"a solver count has no exact card: $count")
    }

    // The size an exact card stands for beside an estimate. A symbolic value is at least 2, which
    // is all a product with an infinite tier reads; beside finite capacities it has no size.
    private def sizeOf(card: Card, besideInfinite: Boolean): Option[Size] = card match {
      case Card.Exact(Natural.Lit(value)) => Some(Size(0, 0, FinitePart.exact(value)))
      case Card.Omega                     => Some(EffectiveOmega)
      case Card.Exact(_)                  => Option.when(besideInfinite)(TinySize(2))
    }

    private def bounded(space: Size): Count =
      if (space.hasInfinite) Bounded(space)
      else
        cardinality.capacity.SizeOptics.exactFiniteValue
          .getOption(space)
          .fold[Count](Bounded(space))(Finite(_))

  }

  import Count.*

  private[inhabitation] class Limit(message: String) extends RuntimeException(message)

  private[inhabitation] class Unsupported(message: String) extends RuntimeException(message)

  def count(
      bindings: List[Binding],
      result: Shape,
      maxStates: Int = 256,
      inspect: String => Unit = _ => (),
      maxDepth: Int = Int.MaxValue
  ): Count =
    try
      inspect("solver preparation")
      RepeatedArguments.prepare(bindings, result) match {
        case Left(count)                                                  => count
        case Right(RepeatedArguments.Prepared.Direct(normalized, target)) =>
          new GoalSolver(maxStates, inspect, maxDepth).run(normalized, target)
        case Right(RepeatedArguments.Prepared.Eliminate(rest, sequences, target)) =>
          SequenceElimination.count(
            rest,
            sequences,
            target,
            count(_, _, maxStates, inspect, maxDepth)
          )
      }
    catch {
      case error: Limit            => Unresolved(List(error.getMessage))
      case error: Unsupported      => Unresolved(List(error.getMessage))
      case error: Natural.Exceeded => Unresolved(List(error.getMessage))
    }

}
