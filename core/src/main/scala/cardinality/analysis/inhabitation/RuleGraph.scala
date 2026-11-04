package cardinality.analysis.inhabitation

import scala.collection.mutable

import Inhabitation.Count

/** One way to build a goal: its subgoals, and why the solver cannot vouch for it.
  *
  * Its count is `weight · (Π children)^exponent`: a ground goal's literals are its weight, and a
  * uniform split is one child raised to the split value's space. `quotient` marks an application of
  * a polymorphic input at the instantiation a goal fixes (capabilities PB2): a genuine term, so it
  * proves productivity and absurdity, but free theorems may merge it with others, so a count it
  * contributes to is exact only at 0 or 1.
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

/** What the solver proved of each goal: `proved` holds the goals built by rules that carry no
  * reason, `optimistic` those built when every rule is allowed, and `absurd` maps a goal to the
  * node of the empty goal in its environment.
  */
final private[inhabitation] case class Verdicts(
    proved: Int => Boolean,
    optimistic: Int => Boolean,
    absurd: Int => Option[Int]
)

/** The rules a productive root can actually reach, counted. */
final private[inhabitation] class Live(
    private[inhabitation] val rules: Vector[List[Rule]],
    private[inhabitation] val inspect: String => Unit,
    private[inhabitation] val maxDepth: Int,
    verdicts: Verdicts
) {
  import Live.Walk
  import verdicts.{absurd, optimistic, proved}

  private[inhabitation] val totals = mutable.Map.empty[Int, Card]

  /** The root's count: with reasons, ω only through a vouched cycle (C11); through a cycle, ω
    * unless an instantiation of a polymorphic input is on the way; otherwise the exact total,
    * reported past an instantiation only at 0 or 1 (PB2).
    */
  def result(root: Int): Count = {
    val (reasons, quotients) = marks(root)
    if (reasons.nonEmpty) { if (unbounded(root)) Count.Countable else Count.Unresolved(reasons) }
    else if (cyclic(root)) throughCycle(root, quotients)
    else total(root, quotients)
  }

  private def throughCycle(root: Int, quotients: List[String]): Count =
    if (quotients.nonEmpty && !unbounded(root)) Count.Unresolved(quotients) else Count.Countable

  private def total(root: Int, quotients: List[String]): Count = {
    val card = Live.count((this, root, 0))
    if (quotients.nonEmpty && !card.isZero && !card.isOne) Count.Unresolved(quotients)
    else Count.of(card)
  }

  // The reasons on the rules this root reads, in the order they were met, and the polymorphic
  // applications it reads (capabilities PB2): one walk.
  private def marks(root: Int): (List[String], List[String]) = {
    val reasons = mutable.LinkedHashSet.empty[String]
    val quotients = mutable.LinkedHashSet.empty[String]
    walk(root, Walk("solver reasons", skipFinished = false, rules)) { rule =>
      reasons ++= rule.reason
      quotients ++= rule.quotient
      ()
    }
    (reasons.toList, quotients.toList)
  }

  // A walk that revisits a goal has a productive cycle behind it: countably many constructions.
  private def cyclic(root: Int): Boolean = {
    var found = false
    walk(root, Walk("solver cycle", skipFinished = true, rules))(_ => (), _ => found = true)
    found
  }

  /** ω whatever the reasons elsewhere (capabilities C11, restricted as SYNTHESIS D-capabilities and
    * crosscheck X3 require): the root reaches a cycle along rules that carry no reason and no
    * polymorphic instantiation, every child of every such rule is built by rules without reasons,
    * and the empty goal is not built, even optimistically, at any goal on the way.
    *
    * The cycle's terms are then pairwise distinct terms of every rule along the path, each a real
    * term (its siblings have one), so at least ω; every goal on the way has a consistent context,
    * the only way further producers could merge terms (a context proving the empty type has one
    * term, an empty sibling makes a product empty); and a program text is finite, so at most ω
    * (register D2). Eager reasons never reach the solver: a reading that fails is refused before.
    */
  private def unbounded(root: Int): Boolean = {
    def sound(id: Int): Boolean = absurd(id).forall(node => !optimistic(node))
    def vouched(id: Int): List[Rule] =
      if (!sound(id)) Nil
      else
        rules(id).filter(r => r.reason.isEmpty && r.quotient.isEmpty && r.children.forall(proved))
    var found = false
    if (proved(root))
      walk(root, Walk("solver saturation", skipFinished = true, vouched))(
        _ => (),
        _ => found = true
      )
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
        mode.rulesOf(id).foreach { rule =>
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

  /** A walk's budget step, whether it answers each goal once (the cycle walks; the reading walk
    * repeats a subtree a second path reaches, as the budget always counted it), and the rules it
    * walks a goal through (the ω-saturation walk keeps the vouched ones).
    */
  final case class Walk(step: String, skipFinished: Boolean, rulesOf: Int => List[Rule])

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
