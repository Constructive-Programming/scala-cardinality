package cardinality.analysis.inhabitation

import scala.collection.mutable

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
  * Opaque callable sums are supported only by the isolated-observation certificate in
  * OpaqueSumForwarding. TerminalHigherOrder certifies terminal atomic higher-order outputs and
  * first-order functional arguments. Other elimination and higher-order application remain
  * unresolved when potentially relevant. No equations on opaque functions, effects, recursion, or
  * casts are assumed.
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
      Plated.universe(shape).exists(predicate)

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

  case class Binding(id: String, shape: Shape)

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
  import Shape.*

  private class Limit(message: String) extends RuntimeException(message)

  private class Unsupported(message: String) extends RuntimeException(message)

  // How a value stands to the subtype proofs: not yet read through them, read (its supertypes are
  // values beside it), or one of those supertypes — the same value seen as its bound, never read
  // further. A mark of its own, not a step of the path: a path is provenance, and its first steps
  // are binding ids, which any spelling may start.
  private enum Reading {
    case Unread, Read, View
  }

  private case class Value(path: List[String], shape: Shape, reading: Reading = Reading.Unread)

  // A rule's count is `weight · (Π children)^exponent`: a ground goal's literals are its weight, and
  // a uniform split is one child raised to the split value's space.
  private case class Rule(
      children: List[Int],
      reason: Option[String] = None,
      weight: Card = Card.one,
      exponent: Card = Card.one
  ) {

    def count(values: Int => Card): Card = {
      val product = children.distinct.foldLeft(Card.one) { (p, id) =>
        p * (values(id) ^ Card.exact(children.count(_ == id)))
      }
      weight * (product ^ exponent)
    }

  }

  private case class Producer(parameters: List[Shape], output: Shape)

  private val producerPaths: Getter[Shape, List[Shape] => List[Producer]] =
    Shape.cata[List[Shape] => List[Producer]] { (node, kids) =>
      node match {
        case Function(parameters, _) => args => kids.last(args ++ parameters)
        case Product(_)              => args => kids.flatMap(_(args)).toList
        case output                  => args => List(Producer(args, output))
      }
    }

  // With no cycle behind it the reachable graph is a DAG, so a memo-aware hylomorphism counts
  // each goal as the sum of its rules' counts, exactly.
  private val liveCount: ((Live, Int, Int)) => Card =
    cardinality.types.TypeSubstitution.hylo[(Live, Int, Int), Card](
      (live, id, depth) => {
        live.inspect("solver count")
        if (depth >= live.maxDepth) throw new Limit("solver traversal depth budget exhausted")
        if (live.totals.contains(id)) Nil
        else live.rules(id).flatMap(_.children).map(child => (live, child, depth + 1))
      },
      (seed, kids) => {
        val (live, id, _) = seed
        live.totals.getOrElseUpdate(
          id, {
            val values = live.rules(id).flatMap(_.children).zip(kids).toMap
            live.rules(id).foldLeft(Card.zero)((sum, rule) => sum + rule.count(values))
          }
        )
      }
    )

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
          new Solver(maxStates, inspect, maxDepth).run(normalized, target)
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

  private class Solver(maxStates: Int, inspect: String => Unit, maxDepth: Int) {
    private val nodes = mutable.ArrayBuffer.empty[List[Rule]]
    private val memo = mutable.Map.empty[(List[Value], Shape), Int]
    private val absurd = mutable.Map.empty[Int, Int]
    private val witnesses = mutable.Map.empty[(List[String], Int), Atom]
    private val reservedAtoms = mutable.Set.empty[String]
    private var witnessNumber = 0
    private var work = 0
    private var depth = 0

    def run(bindings: List[Binding], result: Shape): Count = {
      reservedAtoms ++= bindings.flatMap(binding => ExistentialInputs.names(binding.shape))
      reservedAtoms ++= ExistentialInputs.names(result)
      val env = bindings.distinct.map(binding => Value(List("input", binding.id), binding.shape))
      val root = goal(env, result)
      val productive = leastProductive(allowUnresolved = true)
      // A root with no productive rule has no implementation at all, not an unknown one.
      if (!productive(root)) Finite(0)
      else {
        val proved = leastProductive(allowUnresolved = false)
        val live = nodes.indices.map { id =>
          // An inhabited empty type proves the context impossible. All expressions under that
          // assumption are observationally equal (the unique map out of the empty type). In
          // particular, two different ways to call an A => Nothing are not two inhabitants.
          if (absurd.get(id).exists(proved)) List(Rule(Nil))
          else nodes(id).filter(_.children.forall(productive))
        }.toVector
        new Live(live, inspect, maxDepth).result(root)
      }
    }

    // The least set of goals that can be built from nothing: a rule counts once every child does.
    private def leastProductive(allowUnresolved: Boolean): mutable.Set[Int] = {
      val productive = mutable.Set.empty[Int]
      var changed = true
      while (changed) {
        changed = false
        nodes.indices.foreach { id =>
          inspect("solver productivity")
          val available = nodes(id).filter(r => allowUnresolved || r.reason.isEmpty)
          if (!productive(id) && available.exists(_.children.forall(productive))) {
            productive += id
            changed = true
          }
        }
      }
      productive
    }

    // A goal is the environment it can read plus the shape it must produce, memoised so a cycle
    // becomes a re-visited node rather than another pass.
    private def goal(raw: List[Value], target: Shape): Int = {
      inspect("solver goal")
      if (depth >= maxDepth) throw new Limit("solver depth budget exhausted")
      depth += 1
      try readGoal(raw, target)
      finally depth -= 1
    }

    private def readGoal(raw: List[Value], target: Shape): Int = {
      val flat = flatten(raw)
      val evidence = EvidenceAnalysis
        .context(flat.map(_.shape))
        .fold(reason => throw new Unsupported(reason), identity)
      def rewrite(shape: Shape): Shape =
        evidence.rewrite(shape).fold(reason => throw new Unsupported(reason), identity)
      // An available proof stays in the environment, so a value a later split or binder brings is
      // read under it too.
      val env = flat.flatMap(value => withViews(value, evidence, rewrite)).distinct
      val normalized = rewrite(target)
      // The interactions are judged on what the goal reads, views and their fields included. A
      // Unit goal has its one construction whatever the context holds.
      if (normalized != Product(Nil)) {
        val (views, own) = env.partition(_.reading == Reading.View)
        evidence
          .validate(own.map(_.shape), views.map(_.shape), normalized)
          .left
          .foreach(reason => throw new Unsupported(reason))
      }
      memo.get((env, normalized)) match {
        case Some(id) => id
        case None     => record(env, normalized, evidence.lowers(normalized).map(rewrite))
      }
    }

    // A view is the same value read as a supertype, once: a structural one is flattened or split
    // like a value, and must not come back after it was eliminated. The fields a view is flattened
    // into are values of their own, read through their own bounds in the same pass (`A <: P[B]`,
    // `B <: C`: `a.x` is a C), so what the goal can read does not depend on whether a later goal is
    // recorded. The proofs rule out a bound whose structure holds its own type, so this ends.
    private def withViews(
        value: Value,
        evidence: EvidenceAnalysis.Context,
        rewrite: Shape => Shape
    ): List[Value] =
      value match {
        case Value(_, _: Evidence, _)           => List(value)
        case Value(path, shape, Reading.Unread) =>
          val own = rewrite(shape)
          val supertypes = evidence.views(own).filterNot(_ == own)
          Value(path, own, Reading.Read) :: supertypes.zipWithIndex.flatMap { (above, i) =>
            flatten(List(Value(path :+ s"view:$i", rewrite(above), Reading.View)))
              .flatMap(withViews(_, evidence, rewrite))
          }
        case read => List(read.copy(shape = rewrite(read.shape)))
      }

    private def record(env: List[Value], target: Shape, lowers: List[Shape]): Int = {
      inspect("solver state")
      tick()
      val id = nodes.size
      nodes += Nil
      memo((env, target)) = id
      val ordinary = rules(Recorded(env, target, id, lowers))
      nodes(id) = ordinary ++ absurdRule(env, target, id)
      id
    }

    // A goal being recorded: what it reads, what it builds, its node, and the structural types a
    // lower bound lets it be built as — one more way per bound.
    private case class Recorded(env: List[Value], target: Shape, id: Int, lowers: List[Shape])

    private def absurdRule(env: List[Value], shape: Shape, id: Int): List[Rule] =
      shape match {
        case Product(Nil) => Nil // Unit already has its unique construction.
        case Sum(Nil)     =>
          absurd(id) = id
          Nil
        case _ =>
          val impossible = goal(env, Sum(Nil))
          absurd(id) = impossible
          List(Rule(List(impossible)))
      }

    private def rules(recorded: Recorded): List[Rule] =
      split(recorded.env, recorded.target) match {
        case Some(rule) => List(rule)
        case None       =>
          OpaqueSumForwarding.count(data(recorded.env), recorded.target) match {
            case Some(count) => List.fill(count)(Rule(Nil))
            case None        => ordinaryRules(recorded)
          }
      }

    private def ordinaryRules(recorded: Recorded): List[Rule] = {
      import recorded.{env, id, lowers, target}
      target match {
        case Existential(_, _) =>
          List(Rule(Nil, Some(ExistentialInputs.resultBoundary)))
        case Singleton(_, _) => List(Rule(Nil))
        case Product(fields) =>
          List(Rule(fields.map(goal(env, _))))
        case Function(parameters, result) =>
          List(Rule(List(goal(env ++ binders(parameters, id), result))))
        case _ =>
          constructors(env, target) ++
            lowers.map(bound => Rule(List(goal(env, bound)))) ++
            producers(env, target)
      }
    }

    // An empty context alternative has a unique absurd eliminator, and the branch's own value
    // joins the environment.
    private def split(env: List[Value], target: Shape): Option[Rule] =
      env.zipWithIndex
        .collectFirst {
          case (Value(path, Sum(alternatives), _), at) =>
            Rule(alternatives.zipWithIndex.map {
              case (shape, branch) =>
                goal(env.patch(at, List(Value(path :+ s"case:$branch", shape)), 1), target)
            })
        }
        .orElse(closedSplit(env, target))

    // Closed values are split uniformly: every branch is the same goal without them, once per
    // combination of their values, so the one memoised branch is raised to their joint space. An
    // empty closed value (a function into Nothing from an inhabited domain) makes the context absurd.
    private def closedSplit(env: List[Value], target: Shape): Option[Rule] = {
      val (closed, open) = env.partition(value => ClosedSpaces.closed(value.shape))
      Option.when(closed.nonEmpty) {
        val exponent =
          closed.map(value => ClosedSpaces.space(value.shape)).foldLeft(Card.one)(_ * _)
        if (exponent.isZero) Rule(Nil) else Rule(List(goal(open, target)), exponent = exponent)
      }
    }

    private def binders(parameters: List[Shape], id: Int): List[Value] =
      parameters.zipWithIndex.map {
        case (shape, index) =>
          Value(List("bound", id.toString, index.toString), shape)
      }

    private def constructors(env: List[Value], target: Shape): List[Rule] = target match {
      case Sum(alternatives) => alternatives.map(shape => Rule(List(goal(env, shape))))
      // Every literal, and nothing the environment adds: its transparent values were split away
      // and a producer of a ground result carries a reason.
      case Ground(_, size) => List(Rule(Nil, weight = size))
      // Nil is a base case. Cons is productive only when an element can be constructed; the
      // ordinary productive-cycle proof then establishes infinitely many distinct lengths.
      case Repeated(element) =>
        List(Rule(Nil), Rule(List(goal(env, element), goal(env, target))))
      case _ => Nil
    }

    private def producers(env: List[Value], target: Shape): List[Rule] =
      env.flatMap { value =>
        value.shape match {
          case atom: Atom if atom == target          => List(Rule(Nil))
          case applied: Applied if applied == target => List(Rule(Nil))
          case function: Function                    => application(env, target, function)
          case _                                     => Nil
        }
      }

    // Functional arguments are synthesized only when the complete environment proves that no
    // higher-order result can feed argument synthesis. Otherwise retain the diagnostic rule.
    private def application(env: List[Value], target: Shape, function: Function): List[Rule] =
      producerPaths.get(function)(Nil).flatMap { value =>
        // Empty-result callables are negations, not opaque tagged choices. A proved call to one
        // is handled by absurd elimination above; only nonempty sum results need case analysis.
        val opaque = value.output match {
          case Sum(alternatives) if alternatives.nonEmpty =>
            Some("Elimination of an opaque sum-producing callable is unsupported")
          case Ground(_, _) =>
            Some("Elimination of an opaque callable's transparent ground result is unsupported")
          case Repeated(_) =>
            Some("Elimination of an opaque callable's sequence result is unsupported")
          case Existential(_, _) => Some(ExistentialInputs.callableBoundary)
          case _                 => None
        }
        // Case analysis of a ground result has every literal's branch in the same environment, and
        // a sequence result may be empty, so neither proves the context absurd: no rule toward the
        // empty goal.
        val noAbsurdRule = target == Sum(Nil) && (value.output match {
          case Ground(_, _) | Repeated(_) => true
          case _                          => false
        })
        if (noAbsurdRule || (value.output != target && opaque.isEmpty)) Nil
        else {
          val certified = TerminalHigherOrder.certified(data(env))
          val dependencies =
            value.parameters.filter(p => certified || !higherOrder(p)).map(goal(env, _))
          List(Rule(dependencies, unsupported(value, opaque, certified)))
        }
      }

    private def unsupported(
        value: Producer,
        opaque: Option[String],
        certified: Boolean
    ): Option[String] =
      opaque.orElse(
        Option.when(!certified && value.parameters.exists(higherOrder))(
          "Higher-order application is unsupported"
        )
      )

    // The values the certificates read: a proof is no datum, only the unit it was discharged to.
    private def data(env: List[Value]): List[Shape] = env.map {
      case Value(_, _: Evidence, _) => Product(Nil)
      case value                    => value.shape
    }

    private def tick(): Unit = {
      work += 1
      if (work > maxStates) throw new Limit(s"Inhabitation state budget exceeded ($maxStates)")
    }

    private def flatten(values: List[Value]): List[Value] =
      values.flatMap {
        case Value(path, Existential(bound, body), _) =>
          // Binder positions, not display names, keep identity stable under alpha-renaming.
          val replacements = bound.zipWithIndex.map { (name, index) =>
            name -> witnesses.getOrElseUpdate((path, index), freshWitness())
          }.toMap
          val opened =
            ExistentialInputs.mapAtoms(body)(atom => replacements.getOrElse(atom.id, atom))
          flatten(List(Value(path :+ "existential", opened)))
        case Value(_, Singleton(id, Some(underlying)), _) =>
          flatten(List(Value(List("input", id), underlying)))
        case Value(_, Singleton(_, None), _) => Nil
        case Value(path, Product(fields), _) =>
          flatten(fields.zipWithIndex.map {
            case (shape, index) =>
              Value(path :+ s"field:$index", shape)
          })
        case value => List(value)
      }.distinct

    private def freshWitness(): Atom = {
      val (name, next) = Shape.freshName("existential:", witnessNumber + 1)(reservedAtoms)
      witnessNumber = next - 1
      reservedAtoms += name
      Atom(name)
    }

    private def higherOrder(shape: Shape): Boolean =
      Shape.exists(shape) {
        case _: Function | _: Existential => true
        case _                            => false
      }

  }

  /** The rules a productive root can actually reach, counted. */
  private class Live(
      private[Inhabitation] val rules: Vector[List[Rule]],
      private[Inhabitation] val inspect: String => Unit,
      private[Inhabitation] val maxDepth: Int
  ) {
    private[Inhabitation] val totals = mutable.Map.empty[Int, Card]

    def result(root: Int): Count = {
      val reasons = reasonsAt(root)
      if (reasons.nonEmpty) Unresolved(reasons)
      else if (cyclic(root)) Countable
      else Count.of(liveCount((this, root, 0)))
    }

    // The reasons on the rules this root reads, in the order they were met.
    private def reasonsAt(root: Int): List[String] = {
      val found = mutable.LinkedHashSet.empty[String]
      walk(root, "solver reasons", skipFinished = false) { rule =>
        rule.reason.foreach(reason => { found += reason; () })
      }
      found.toList
    }

    // A walk that revisits a goal has a productive cycle behind it: countably many constructions.
    private def cyclic(root: Int): Boolean = {
      var found = false
      walk(root, "solver cycle", skipFinished = true)(_ => (), repeat = _ => found = true)
      found
    }

    /** One depth-budgeted walk of the rule graph from a root — the only place the solver's
      * traversal rule lives. Every step is counted for the analyzer's budget, a path longer than
      * `maxDepth` is a resource failure rather than an answer, and a rule is read before its
      * children are walked, so a node's reasons are met in the order a reader would meet them.
      *
      * Reaching a rule that is already on the path is a productive cycle behind the goal: `repeat`
      * sees it, and the walk neither reads that rule nor descends into it. `skipFinished` is the
      * one thing the two callers disagree on. The cycle walk answers each node once, while the
      * reasons walk walks a subtree again when a second path reaches it — its set collapses the
      * repeats, and keeping that reading is what leaves the steps the budget counts unchanged.
      */
    private def walk(root: Int, step: String, skipFinished: Boolean)(
        read: Rule => Unit,
        repeat: Int => Unit = _ => ()
    ): Unit = {
      val onPath = mutable.Set.empty[Int]
      val finished = mutable.Set.empty[Int]
      def visit(id: Int, depth: Int): Unit = {
        inspect(step)
        if (depth >= maxDepth) throw new Limit("solver traversal depth budget exhausted")
        if (onPath(id)) repeat(id)
        else if (!skipFinished || !finished(id)) {
          onPath += id
          rules(id).foreach { rule =>
            read(rule)
            rule.children.foreach(child => visit(child, depth + 1))
          }
          onPath -= id
          finished += id
        }
      }
      visit(root, 0)
    }

  }

}
