package cardinality.analysis.inhabitation

import scala.collection.mutable

import Inhabitation.Count

/** One way to build a goal: its subgoals, and why the solver cannot vouch for it.
  *
  * Its count is `weight · (Π children)^exponent`: a ground goal's literals are its weight, and a
  * uniform split is one child raised to the split value's space. `quotient` marks an application of
  * a polymorphic input at the instantiation a goal fixes (capabilities PB2): a genuine term, so it
  * proves productivity and absurdity, but free theorems may merge it with others, so a cycle
  * through it is not counted.
  */
final private[inhabitation] case class Rule(
    children: List[Int],
    reason: Option[String] = None,
    weight: Card = Card.one,
    exponent: Card = Card.one,
    quotient: Option[String] = None
) {

  def count(values: Int => Card): Card = {
    val product = children.distinct.foldLeft(Card.one) { (p, id) =>
      p * (values(id) ^ Card.exact(children.count(_ == id)))
    }
    weight * (product ^ exponent)
  }

}

/** The rules a productive root can actually reach, counted. */
final private[inhabitation] class Live(
    private[inhabitation] val rules: Vector[List[Rule]],
    private[inhabitation] val inspect: String => Unit,
    private[inhabitation] val maxDepth: Int
) {
  import Live.Walk

  private[inhabitation] val totals = mutable.Map.empty[Int, Card]

  /** The root's count: none with reasons; through a cycle, ω unless an instantiation of a
    * polymorphic input is on the way; otherwise the exact total.
    */
  def result(root: Int): Count = {
    val (reasons, quotients) = marks(root)
    if (reasons.nonEmpty) Count.Unresolved(reasons)
    else if (cyclic(root))
      if (quotients.nonEmpty) Count.Unresolved(quotients) else Count.Countable
    else Count.of(Live.count((this, root, 0)))
  }

  // The reasons on the rules this root reads, in the order they were met, and the polymorphic
  // applications it reads (capabilities PB2): one walk.
  private def marks(root: Int): (List[String], List[String]) = {
    val reasons = mutable.LinkedHashSet.empty[String]
    val quotients = mutable.LinkedHashSet.empty[String]
    walk(root, Walk("solver reasons", skipFinished = false)) { rule =>
      reasons ++= rule.reason
      quotients ++= rule.quotient
      ()
    }
    (reasons.toList, quotients.toList)
  }

  // A walk that revisits a goal has a productive cycle behind it: countably many constructions.
  private def cyclic(root: Int): Boolean = {
    var found = false
    walk(root, Walk("solver cycle", skipFinished = true))(_ => (), _ => found = true)
    found
  }

  /** One depth-budgeted walk of the rule graph from a root — the solver's traversal rule. Every
    * step is counted for the analyzer's budget, a path longer than `maxDepth` is a resource failure
    * rather than an answer, and a rule is read before its children are walked, so a node's reasons
    * are met in the order a reader would meet them.
    *
    * Reaching a rule that is already on the path is a productive cycle behind the goal: `repeat`
    * sees it, and the walk neither reads that rule nor descends into it. What the callers disagree
    * on is the walk's `Walk` mode.
    */
  private def walk(root: Int, mode: Walk)(
      read: Rule => Unit,
      repeat: Int => Unit = _ => ()
  ): Unit = {
    val onPath = mutable.Set.empty[Int]
    val finished = mutable.Set.empty[Int]
    def visit(id: Int, depth: Int): Unit = {
      inspect(mode.step)
      if (depth >= maxDepth) throw new Inhabitation.Limit("solver traversal depth budget exhausted")
      if (onPath(id)) repeat(id)
      else if (!mode.skipFinished || !finished(id)) {
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

private[inhabitation] object Live {

  /** A walk's budget step, and whether it answers each goal once (the cycle walk; the reading walk
    * repeats a subtree a second path reaches, as the budget always counted it).
    */
  final case class Walk(step: String, skipFinished: Boolean)

  // With no cycle behind it the reachable graph is a DAG, so a memo-aware hylomorphism counts
  // each goal as the sum of its rules' counts, exactly.
  val count: ((Live, Int, Int)) => Card =
    cardinality.types.TypeSubstitution.hylo[(Live, Int, Int), Card](
      (live, id, depth) => {
        live.inspect("solver count")
        if (depth >= live.maxDepth)
          throw new Inhabitation.Limit("solver traversal depth budget exhausted")
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

}
