package cardinality

import scala.collection.mutable

/** Counts canonical pure constructions, not runtime equality.
  *
  * Supported: free atoms, products, finite sums in the context, arrow introduction, and reusable
  * first-order producers (including curried ones and projections of product results). Products are
  * eta-expanded; Unit has one implementation. Sum contexts are exhaustively split, with independent
  * implementations in each branch. Provenance distinguishes opaque values.
  *
  * Higher-order application and elimination of opaque callable sum results are deliberately
  * unresolved when potentially relevant. No equations on opaque functions, effects, recursion, or
  * unsafe casts are assumed.
  */
object Inhabitation {

  enum Shape {
    case Atom(id: String)
    case Product(fields: List[Shape])
    case Sum(alternatives: List[Shape])
    case Function(parameters: List[Shape], result: Shape)
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

  private case class Value(path: List[String], shape: Shape)

  private case class Rule(children: List[Int], reason: Option[String] = None)

  private case class Producer(parameters: List[Shape], output: Shape)

  def count(bindings: List[Binding], result: Shape, maxStates: Int = 256): Count =
    try new Solver(maxStates).run(bindings, result)
    catch { case error: Limit => Unresolved(List(error.getMessage)) }

  private class Solver(maxStates: Int) {
    private val nodes = mutable.ArrayBuffer.empty[List[Rule]]
    private val memo = mutable.Map.empty[(List[Value], Shape), Int]
    private var work = 0

    def run(bindings: List[Binding], result: Shape): Count = {
      val env = bindings.distinct.map(binding => Value(List("input", binding.id), binding.shape))
      val root = goal(env, result)
      val productive = leastProductive()
      // A root with no productive rule has no implementation at all, not an unknown one.
      if (!productive(root)) Finite(0)
      else new Live(nodes.toVector.map(_.filter(_.children.forall(productive)))).result(root)
    }

    // The least set of goals that can be built from nothing: a rule counts once every child does.
    private def leastProductive(): mutable.Set[Int] = {
      val productive = mutable.Set.empty[Int]
      var changed = true
      while (changed) {
        changed = false
        nodes.indices.foreach { id =>
          if (!productive(id) && nodes(id).exists(_.children.forall(productive))) {
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
      val env = flatten(raw)
      memo.get((env, target)) match {
        case Some(id) => id
        case None     => record(env, target)
      }
    }

    private def record(env: List[Value], target: Shape): Int = {
      tick()
      val id = nodes.size
      nodes += Nil
      memo((env, target)) = id
      nodes(id) = rules(env, target, id)
      id
    }

    private def rules(env: List[Value], target: Shape, id: Int): List[Rule] =
      split(env, target) match {
        case Some(rule) => List(rule)
        case None       =>
          target match {
            case Product(fields) =>
              List(Rule(fields.map(goal(env, _))))
            case Function(parameters, result) =>
              List(Rule(List(goal(env ++ binders(parameters, id), result))))
            case _ =>
              constructors(env, target) ++ producers(env, target)
          }
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
      case _                 => Nil
    }

    private def producers(env: List[Value], target: Shape): List[Rule] =
      env.flatMap { value =>
        value.shape match {
          case atom: Atom if atom == target => List(Rule(Nil))
          case function: Function           => application(env, target, function)
          case _                            => Nil
        }
      }

    // A callable produces the target from its parameters; a parameter that is itself higher-order
    // may be inhabited without being synthesised, which the fragment records as a reason.
    private def application(env: List[Value], target: Shape, function: Function): List[Rule] =
      producer(function).flatMap { value =>
        val opaque = value.output.isInstanceOf[Sum]
        if (value.output != target && !opaque) Nil
        else {
          val dependencies = value.parameters.filterNot(higherOrder).map(goal(env, _))
          List(Rule(dependencies, unsupported(value, opaque)))
        }
      }

    private def unsupported(value: Producer, opaque: Boolean): Option[String] =
      if (opaque) Some("Elimination of an opaque sum-producing callable is unsupported")
      else if (value.parameters.exists(higherOrder)) Some("Higher-order application is unsupported")
      else None

    private def tick(): Unit = {
      work += 1
      if (work > maxStates) throw new Limit(s"Inhabitation state budget exceeded ($maxStates)")
    }

    private def flatten(values: List[Value]): List[Value] =
      values.flatMap {
        case Value(path, Product(fields)) =>
          flatten(fields.zipWithIndex.map {
            case (shape, index) =>
              Value(path :+ s"field:$index", shape)
          })
        case value => List(value)
      }.distinct

    private def producer(shape: Shape, args: List[Shape] = Nil): List[Producer] =
      shape match {
        case Function(parameters, output) => producer(output, args ++ parameters)
        case Product(fields)              => fields.flatMap(producer(_, args))
        case output                       => List(Producer(args, output))
      }

    private def higherOrder(shape: Shape): Boolean =
      shape match {
        case Function(_, _)    => true
        case Product(fields)   => fields.exists(higherOrder)
        case Sum(alternatives) => alternatives.exists(higherOrder)
        case Atom(_)           => false
      }

  }

  /** The rules a productive root can actually reach, counted. */
  private class Live(rules: Vector[List[Rule]]) {
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
      def visit(id: Int): Unit =
        if (!visiting(id)) {
          visiting += id
          rules(id).foreach { rule =>
            rule.reason.foreach(reason => { found += reason; () })
            rule.children.foreach(visit)
          }
          visiting -= id
        }
      visit(root)
      found.toList
    }

    // A walk that revisits a goal has a productive cycle behind it: countably many constructions.
    private def cyclic(root: Int): Boolean = {
      var found = false
      val onPath = mutable.Set.empty[Int]
      val done = mutable.Set.empty[Int]
      def visit(id: Int): Unit =
        if (onPath(id)) found = true
        else if (!done(id)) {
          onPath += id
          rules(id).foreach(_.children.foreach(visit))
          onPath -= id
          done += id
        }
      visit(root)
      found
    }

    // With no cycle behind it the reachable graph is a tree, so the count is exact arithmetic over
    // the rules, with a budget on the intermediate values rather than a silent overflow.
    private def count(id: Int): BigInt = totals.getOrElseUpdate(
      id,
      rules(id).foldLeft(BigInt(0)) { (sum, rule) =>
        val term = rule.children.foldLeft(BigInt(1)) { (product, child) =>
          checked(product * count(child))
        }
        checked(sum + term)
      }
    )

    private def checked(value: BigInt): BigInt =
      if (value.bitLength > MaxBits)
        throw new Limit(s"Inhabitation arithmetic budget exceeded ($MaxBits bits)")
      else value

  }

}
