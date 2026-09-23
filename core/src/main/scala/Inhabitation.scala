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

  import Shape.*
  import Count.*

  // Bound intermediate arithmetic as well as the grammar. This is a resource
  // limit, not a replacement for exact arithmetic.
  private val MaxBits = 65536
  private class Limit(message: String) extends RuntimeException(message)
  private case class Value(path: List[String], shape: Shape)
  private case class Rule(children: List[Int], reason: Option[String] = None)
  private case class Producer(parameters: List[Shape], output: Shape)

  def count(bindings: List[Binding], result: Shape, maxStates: Int = 256): Count = {
    try new Solver(maxStates).run(bindings, result)
    catch { case error: Limit => Unresolved(List(error.getMessage)) }
  }

  private class Solver(maxStates: Int) {
    private val nodes = mutable.ArrayBuffer.empty[List[Rule]]
    private val memo = mutable.Map.empty[(List[Value], Shape), Int]
    private var work = 0

    private def tick(): Unit = {
      work += 1
      if (work > maxStates) throw new Limit(s"Inhabitation state budget exceeded ($maxStates)")
    }

    private def flatten(values: List[Value]): List[Value] =
      values.flatMap {
        case Value(path, Product(fields)) =>
          flatten(fields.zipWithIndex.map { case (s, i) => Value(path :+ s"field:$i", s) })
        case value => List(value)
      }.distinct

    private def producer(shape: Shape, args: List[Shape] = Nil): List[Producer] =
      shape match {
        case Function(parameters, output) => producer(output, args ++ parameters)
        case Product(fields)              => fields.flatMap(producer(_, args))
        case output                       => List(Producer(args, output))
      }

    private def higherOrder(shape: Shape): Boolean = shape match {
      case Function(_, _)    => true
      case Product(fields)   => fields.exists(higherOrder)
      case Sum(alternatives) => alternatives.exists(higherOrder)
      case Atom(_)           => false
    }

    private def goal(raw: List[Value], target: Shape): Int = {
      val env = flatten(raw)
      memo.get((env, target)) match {
        case Some(id) => id
        case None     =>
          tick()
          val id = nodes.size
          nodes += Nil
          memo((env, target)) = id
          val split = env.indexWhere(_.shape.isInstanceOf[Sum])
          val rules =
            if (split >= 0) {
              val value = env(split)
              val Sum(alternatives) = value.shape: @unchecked
              // An empty context alternative has a unique absurd eliminator.
              List(Rule(alternatives.zipWithIndex.map {
                case (shape, branch) =>
                  goal(
                    env.patch(split, List(Value(value.path :+ s"case:$branch", shape)), 1),
                    target
                  )
              }))
            } else
              target match {
                case Product(fields)              => List(Rule(fields.map(goal(env, _))))
                case Function(parameters, output) =>
                  val locals = parameters.zipWithIndex.map {
                    case (shape, index) =>
                      Value(List("bound", id.toString, index.toString), shape)
                  }
                  List(Rule(List(goal(env ++ locals, output))))
                case _ =>
                  val constructors = target match {
                    case Sum(alternatives) => alternatives.map(s => Rule(List(goal(env, s))))
                    case _                 => Nil
                  }
                  val available = env.flatMap { value =>
                    value.shape match {
                      case atom: Atom if atom == target => List(Rule(Nil))
                      case function: Function           =>
                        producer(function).flatMap { p =>
                          val opaqueSum = p.output.isInstanceOf[Sum]
                          if (p.output != target && !opaqueSum) Nil
                          else {
                            val unsupported = p.parameters.exists(higherOrder)
                            val reason =
                              if (opaqueSum)
                                Some(
                                  "Elimination of an opaque sum-producing callable is unsupported"
                                )
                              else if (unsupported) Some("Higher-order application is unsupported")
                              else None
                            // A higher-order argument may be inhabited even if
                            // its own synthesis is outside the supported fragment.
                            val dependencies = p.parameters.filterNot(higherOrder).map(goal(env, _))
                            List(Rule(dependencies, reason))
                          }
                        }
                      case _ => Nil
                    }
                  }
                  constructors ++ available
              }
          nodes(id) = rules
          id
      }
    }

    def run(bindings: List[Binding], result: Shape): Count = {
      val env = bindings.distinct.map(b => Value(List("input", b.id), b.shape))
      val root = goal(env, result)
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
      // A root with no productive derivation has no implementation at all, not an unknown one.
      if (!productive(root)) Finite(0)
      else {
        val live = nodes.map(_.filter(_.children.forall(productive)))
        val visiting = mutable.Set.empty[Int]
        val visited = mutable.Set.empty[Int]
        val reasons = mutable.LinkedHashSet.empty[String]
        var cyclic = false
        def visit(id: Int): Unit = {
          if (visiting(id)) cyclic = true
          else if (!visited(id)) {
            visiting += id
            live(id).foreach { rule =>
              rule.reason.foreach(reasons += _)
              rule.children.foreach(visit)
            }
            visiting -= id
            visited += id
          }
        }
        visit(root)
        if (reasons.nonEmpty) Unresolved(reasons.toList)
        else if (cyclic) Countable
        else {
          val totals = mutable.Map.empty[Int, BigInt]
          def checked(value: BigInt): BigInt = {
            if (value.bitLength > MaxBits)
              throw new Limit(s"Inhabitation arithmetic budget exceeded ($MaxBits bits)")
            value
          }
          def total(id: Int): BigInt = totals.getOrElseUpdate(
            id,
            live(id).foldLeft(BigInt(0)) { (sum, rule) =>
              val term =
                rule.children.foldLeft(BigInt(1))((n, child) => checked(n * total(child)))
              checked(sum + term)
            }
          )
          Finite(total(root))
        }
      }
    }

  }

}
