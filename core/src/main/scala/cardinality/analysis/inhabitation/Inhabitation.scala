package cardinality.analysis.inhabitation

import scala.collection.mutable

import cardinality.capacity.{EffectiveOmega, FinitePart, Size, UnitSize}
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
  * Repeated arguments use finite sequence introduction (Nil/Cons). Arbitrary sequence elimination
  * is unresolved except for structurally empty elements or singleton results.
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
  }

  object Shape {

    given platedShape: Plated[Shape] = Plated.fromChildren(
      children = {
        case Atom(_)                        => Nil
        case Singleton(_, None)             => Nil
        case Singleton(_, Some(underlying)) => List(underlying)
        case Product(fields)                => fields
        case Sum(alternatives)              => alternatives
        case Function(parameters, result)   => parameters :+ result
        case Repeated(element)              => List(element)
        case Evidence(left, right, _)       => List(left, right)
        case Existential(_, body)           => List(body)
      },
      rebuild = (shape, kids) =>
        shape match {
          case s: Atom                   => s
          case s @ Singleton(_, None)    => s
          case Singleton(id, Some(_))    => Singleton(id, Some(kids.head))
          case Product(_)                => Product(kids)
          case Sum(_)                    => Sum(kids)
          case Function(parameters, _)   => Function(kids.take(parameters.size), kids.last)
          case Repeated(_)               => Repeated(kids.head)
          case Evidence(_, _, equality)  => Evidence(kids.head, kids(1), equality)
          case Existential(witnesses, _) => Existential(witnesses, kids.head)
        }
    )

    def cata[A](alg: (Shape, Iterable[A]) => A): Getter[Shape, A] =
      Schemes.cata[Shape, A]((node, kids) => alg(node, kids.toList))

    def rebuild[A](node: Shape, kids: Iterable[A])(f: A => Shape): Shape =
      platedShape.rebuild(node, PSVec.from(kids.iterator.map(f)))

    def exists(shape: Shape)(predicate: Shape => Boolean): Boolean =
      Plated.universe(shape).exists(predicate)

    val atomPrism = prism[Shape, Atom]
    val functionPrism = prism[Shape, Function]
    val sumPrism = prism[Shape, Sum]
    val evidencePrism = prism[Shape, Evidence]
    val repeatedPrism = prism[Shape, Repeated]

    def sequenceOptions[A](values: Iterable[Option[A]]): Option[List[A]] =
      values.foldRight(Option(List.empty[A]))((value, rest) =>
        for {
          item <- value
          items <- rest
        } yield item :: items
      )

    val dataAtoms: Getter[Shape, Option[List[Atom]]] =
      cata[Option[List[Atom]]] { (node, kids) =>
        node match {
          case a: Atom    => Some(List(a))
          case Product(_) => sequenceOptions(kids).map(_.flatten)
          case _          => None
        }
      }

    val callableAtoms: Getter[Shape, List[Atom]] =
      cata[List[Atom]] { (node, kids) =>
        node match {
          case a: Atom                  => List(a)
          case _: Product | _: Function => kids.flatten.toList
          case _                        => Nil
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
    case Countable

    /** A value space the estimate read: `2^32` for an `Int`, `ω` for a sealed family, `ε₀` for a
      * function space. The size's own rendering, bound and not proof — an exact count still renders
      * as `Finite`.
      */
    case Bounded(size: Size)
    case Unresolved(reasons: List[String])

    def render: String = this match {
      case Finite(value) => value.toString
      case Countable     => "ω"
      case Bounded(size) => size.render
      case Unresolved(_) => "?"
    }

    def isFinite: Boolean = this match {
      case Finite(_)                 => true
      case Bounded(space)            => !space.hasInfinite
      case Countable | Unresolved(_) => false
    }

    def isCountable: Boolean = this match {
      case Countable                 => true
      case Bounded(space)            => space.hasInfinite && space.epsilon == 0
      case Finite(_) | Unresolved(_) => false
    }

    /** Above countable: the ε₀ tier the size algebra places a function space over a countable type.
      */
    def isBeyondCountable: Boolean = this match {
      case Bounded(space) => space.epsilon > 0
      case _              => false
    }

  }

  object Count {

    /** The count a constructor admits: the positions the estimate read multiply as the size algebra
      * multiplies spaces — `2 × ω = ω`, a tier beside a higher tier answers the higher — and
      * everything else goes in as the shape model's own count of the remaining positions. A
      * position that stayed unresolved leaves the product unresolved: a number behind an unreadable
      * name is not a count.
      */
    def product(spaces: List[Size], implementations: Count): Count = implementations match {
      case Unresolved(reasons) => Unresolved(reasons)
      case count               => spaces.foldLeft(count)(times)
    }

    // One read position multiplies in: the count so far times the space, read back as a count.
    private def times(count: Count, space: Size): Count = bounded(sizeOf(count) * space)

    // The space a resolved count stands for, so the rest of the product is the size algebra's. The
    // unresolved case is the one `product` answered before it multiplies.
    private def sizeOf(count: Count): Size = count match {
      case Finite(value)  => Size(0, 0, FinitePart.exact(value))
      case Countable      => EffectiveOmega
      case Bounded(space) => space
      case Unresolved(_)  => UnitSize
    }

    private def bounded(space: Size): Count =
      if (space.hasInfinite) Bounded(space)
      else Size.exactFinite(space).fold[Count](Bounded(space))(Finite(_))

  }

  import Count.*
  import Shape.*

  // Bound intermediate arithmetic as well as the grammar. This is a resource limit, not a
  // replacement for exact arithmetic.
  private val MaxBits = 65536

  private class Limit(message: String) extends RuntimeException(message)

  private class Unsupported(message: String) extends RuntimeException(message)

  private case class Value(path: List[String], shape: Shape)

  private case class Rule(children: List[Int], reason: Option[String] = None)

  private case class Producer(parameters: List[Shape], output: Shape)

  private val producerPaths: Getter[Shape, List[Shape] => List[Producer]] =
    Shape.cata[List[Shape] => List[Producer]] { (node, kids) =>
      node match {
        case Function(parameters, _) => args => kids.last(args ++ parameters)
        case Product(_)              => args => kids.flatMap(_(args)).toList
        case output                  => args => List(Producer(args, output))
      }
    }

  final private case class CountSeed(live: Live, id: Int, depth: Int)

  // With no cycle behind it the reachable graph is a DAG, so a memo-aware hylomorphism counts
  // each goal as the sum of its rules' child products with a budget on intermediate bit-width.
  private val liveCount: Getter[CountSeed, BigInt] =
    Schemes.hylo[CountSeed, BigInt](
      seed => {
        seed.live.enterCount(seed.depth)
        if (seed.live.cached(seed.id).isDefined) PSVec.empty
        else
          PSVec.from(
            seed.live
              .rulesAt(seed.id)
              .iterator
              .flatMap(_.children)
              .map(child => CountSeed(seed.live, child, seed.depth + 1))
          )
      },
      (seed, kids) =>
        seed.live.cached(seed.id).getOrElse {
          val total = seed.live.sumRuleProducts(seed.id, kids)
          seed.live.remember(seed.id, total)
          total
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
        case Left(count)                 => count
        case Right((normalized, target)) =>
          new Solver(maxStates, inspect, maxDepth).run(normalized, target)
      }
    catch {
      case error: Limit       => Unresolved(List(error.getMessage))
      case error: Unsupported => Unresolved(List(error.getMessage))
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
        .context(flat.map(_.shape), target)
        .fold(reason => throw new Unsupported(reason), identity)
      def rewrite(shape: Shape): Shape =
        evidence.rewrite(shape).fold(reason => throw new Unsupported(reason), identity)
      val env = flat.flatMap { value =>
        val shape = rewrite(value.shape)
        evidence.views(shape).map(view => Value(value.path, view))
      }.distinct
      val normalized = rewrite(target)
      memo.get((env, normalized)) match {
        case Some(id) => id
        case None     => record(env, normalized)
      }
    }

    private def record(env: List[Value], target: Shape): Int = {
      inspect("solver state")
      tick()
      val id = nodes.size
      nodes += Nil
      memo((env, target)) = id
      val ordinary = rules(env, target, id)
      nodes(id) = ordinary ++ absurdRule(env, target, id)
      id
    }

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

    private def rules(env: List[Value], target: Shape, id: Int): List[Rule] =
      split(env, target) match {
        case Some(rule) => List(rule)
        case None       =>
          OpaqueSumForwarding.count(env.map(_.shape), target) match {
            case Some(count) => List.fill(count)(Rule(Nil))
            case None        => ordinaryRules(env, target, id)
          }
      }

    private def ordinaryRules(env: List[Value], target: Shape, id: Int): List[Rule] =
      target match {
        case Existential(_, _) =>
          List(Rule(Nil, Some(ExistentialInputs.resultBoundary)))
        case Singleton(_, _) => List(Rule(Nil))
        case Product(fields) =>
          List(Rule(fields.map(goal(env, _))))
        case Function(parameters, result) =>
          List(Rule(List(goal(env ++ binders(parameters, id), result))))
        case _ =>
          constructors(env, target) ++ producers(env, target)
      }

    // An empty context alternative has a unique absurd eliminator, and the branch's own value
    // joins the environment.
    private def split(env: List[Value], target: Shape): Option[Rule] =
      env.zipWithIndex.collectFirst {
        case (Value(path, Sum(alternatives)), at) =>
          Rule(alternatives.zipWithIndex.map {
            case (shape, branch) =>
              goal(env.patch(at, List(Value(path :+ s"case:$branch", shape)), 1), target)
          })
      }

    private def binders(parameters: List[Shape], id: Int): List[Value] =
      parameters.zipWithIndex.map {
        case (shape, index) =>
          Value(List("bound", id.toString, index.toString), shape)
      }

    private def constructors(env: List[Value], target: Shape): List[Rule] = target match {
      case Sum(alternatives) => alternatives.map(shape => Rule(List(goal(env, shape))))
      // Nil is a base case. Cons is productive only when an element can be constructed; the
      // ordinary productive-cycle proof then establishes infinitely many distinct lengths.
      case Repeated(element) =>
        List(Rule(Nil), Rule(List(goal(env, element), goal(env, target))))
      case _ => Nil
    }

    private def producers(env: List[Value], target: Shape): List[Rule] =
      env.flatMap { value =>
        value.shape match {
          case atom: Atom if atom == target => List(Rule(Nil))
          case function: Function           => application(env, target, function)
          case _                            => Nil
        }
      }

    // Functional arguments are synthesized only when the complete environment proves that no
    // higher-order result can feed argument synthesis. Otherwise retain the diagnostic rule.
    private def application(env: List[Value], target: Shape, function: Function): List[Rule] =
      producer(function).flatMap { value =>
        // Empty-result callables are negations, not opaque tagged choices. A proved call to one
        // is handled by absurd elimination above; only nonempty sum results need case analysis.
        val opaque = value.output match {
          case Sum(alternatives) if alternatives.nonEmpty =>
            Some("Elimination of an opaque sum-producing callable is unsupported")
          case Existential(_, _) => Some(ExistentialInputs.callableBoundary)
          case _                 => None
        }
        if (value.output != target && opaque.isEmpty) Nil
        else {
          val certified = TerminalHigherOrder.certified(env.map(_.shape))
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

    private def tick(): Unit = {
      work += 1
      if (work > maxStates) throw new Limit(s"Inhabitation state budget exceeded ($maxStates)")
    }

    private def flatten(values: List[Value]): List[Value] =
      values.flatMap {
        case Value(path, Existential(bound, body)) =>
          // Binder positions, not display names, keep identity stable under alpha-renaming.
          val replacements = bound.zipWithIndex.map { (name, index) =>
            name -> witnesses.getOrElseUpdate((path, index), freshWitness())
          }.toMap
          val opened =
            ExistentialInputs.mapAtoms(body)(atom => replacements.getOrElse(atom.id, atom))
          flatten(List(Value(path :+ "existential", opened)))
        case Value(_, Singleton(id, Some(underlying))) =>
          flatten(List(Value(List("input", id), underlying)))
        case Value(_, Singleton(_, None)) => Nil
        case Value(path, Product(fields)) =>
          flatten(fields.zipWithIndex.map {
            case (shape, index) =>
              Value(path :+ s"field:$index", shape)
          })
        case value => List(value)
      }.distinct

    private def freshWitness(): Atom = {
      witnessNumber += 1
      var name = s"existential:$witnessNumber"
      while (reservedAtoms(name)) {
        witnessNumber += 1
        name = s"existential:$witnessNumber"
      }
      reservedAtoms += name
      Atom(name)
    }

    private def producer(shape: Shape, args: List[Shape] = Nil): List[Producer] =
      producerPaths.get(shape)(args)

    private def higherOrder(shape: Shape): Boolean =
      Plated.universe(shape).exists {
        case _: Function | _: Existential => true
        case _                            => false
      }

  }

  /** The rules a productive root can actually reach, counted. */
  private class Live(rules: Vector[List[Rule]], inspect: String => Unit, maxDepth: Int) {
    private val totals = mutable.Map.empty[Int, BigInt]

    def result(root: Int): Count = {
      val reasons = reasonsAt(root)
      if (reasons.nonEmpty) Unresolved(reasons)
      else if (cyclic(root)) Countable
      else Finite(liveCount.get(CountSeed(this, root, 0)))
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

    def enterCount(depth: Int): Unit = {
      inspect("solver count")
      if (depth >= maxDepth) throw new Limit("solver traversal depth budget exhausted")
    }

    def cached(id: Int): Option[BigInt] = totals.get(id)

    def rulesAt(id: Int): List[Rule] = rules(id)

    def remember(id: Int, total: BigInt): Unit = {
      totals.update(id, total)
    }

    def sumRuleProducts(id: Int, kids: PSVec[BigInt]): BigInt = {
      var index = 0
      rules(id).foldLeft(BigInt(0)) { (sum, rule) =>
        val term = rule.children.foldLeft(BigInt(1)) { (product, _) =>
          val childCount = kids(index)
          index += 1
          checked(product * childCount)
        }
        checked(sum + term)
      }
    }

    private def checked(value: BigInt): BigInt =
      if (value.bitLength > MaxBits)
        throw new Limit(s"Inhabitation arithmetic budget exceeded ($MaxBits bits)")
      else value

  }

}
