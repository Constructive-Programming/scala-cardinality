package cardinality.analysis.inhabitation

import scala.collection.mutable

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

  case class Binding(id: String, shape: Shape)

  enum Count {
    case Finite(value: BigInt)
    case Countable
    case Unresolved(reasons: List[String])

    def render: String = this match {
      case Finite(value) => value.toString
      case Countable     => "ω"
      case Unresolved(_) => "?"
    }

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
      shape match {
        case function: Function =>
          val (parameters, output) = TerminalHigherOrder.uncurry(function)
          producer(output, args ++ parameters)
        case Product(fields) => fields.flatMap(producer(_, args))
        case output          => List(Producer(args, output))
      }

    private def higherOrder(shape: Shape): Boolean =
      shape match {
        case Function(_, _)           => true
        case Product(fields)          => fields.exists(higherOrder)
        case Sum(alternatives)        => alternatives.exists(higherOrder)
        case Repeated(element)        => higherOrder(element)
        case Atom(_)                  => false
        case Singleton(_, underlying) => underlying.exists(higherOrder)
        case Evidence(_, _, _)        => false
        case Existential(_, _)        => true
      }

  }

  /** The rules a productive root can actually reach, counted. */
  private class Live(rules: Vector[List[Rule]], inspect: String => Unit, maxDepth: Int) {
    private val totals = mutable.Map.empty[Int, BigInt]

    def result(root: Int): Count = {
      val reasons = reasonsAt(root)
      if (reasons.nonEmpty) Unresolved(reasons)
      else if (cyclic(root)) Countable
      else Finite(count(root))
    }

    // The reasons on the rules this root reads, in the order they were met.
    private def reasonsAt(root: Int): List[String] = {
      val found = mutable.LinkedHashSet.empty[String]
      val visiting = mutable.Set.empty[Int]
      def visit(id: Int, depth: Int): Unit = {
        inspect("solver reasons")
        if (depth >= maxDepth) throw new Limit("solver traversal depth budget exhausted")
        if (!visiting(id)) {
          visiting += id
          rules(id).foreach { rule =>
            rule.reason.foreach(reason => { found += reason; () })
            rule.children.foreach(child => visit(child, depth + 1))
          }
          visiting -= id
        }
      }
      visit(root, 0)
      found.toList
    }

    // A walk that revisits a goal has a productive cycle behind it: countably many constructions.
    private def cyclic(root: Int): Boolean = {
      var found = false
      val onPath = mutable.Set.empty[Int]
      val done = mutable.Set.empty[Int]
      def visit(id: Int, depth: Int): Unit = {
        inspect("solver cycle")
        if (depth >= maxDepth) throw new Limit("solver traversal depth budget exhausted")
        if (onPath(id)) found = true
        else if (!done(id)) {
          onPath += id
          rules(id).foreach(_.children.foreach(child => visit(child, depth + 1)))
          onPath -= id
          done += id
        }
      }
      visit(root, 0)
      found
    }

    // With no cycle behind it the reachable graph is a tree, so the count is exact arithmetic over
    // the rules, with a budget on the intermediate values rather than a silent overflow.
    private def count(id: Int, depth: Int = 0): BigInt = {
      inspect("solver count")
      if (depth >= maxDepth) throw new Limit("solver traversal depth budget exhausted")
      totals.getOrElseUpdate(
        id,
        rules(id).foldLeft(BigInt(0)) { (sum, rule) =>
          val term = rule.children.foldLeft(BigInt(1)) { (product, child) =>
            checked(product * count(child, depth + 1))
          }
          checked(sum + term)
        }
      )
    }

    private def checked(value: BigInt): BigInt =
      if (value.bitLength > MaxBits)
        throw new Limit(s"Inhabitation arithmetic budget exceeded ($MaxBits bits)")
      else value

  }

}
