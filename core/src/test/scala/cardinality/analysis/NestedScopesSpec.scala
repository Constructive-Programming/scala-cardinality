package cardinality.analysis

import org.specs2.Specification

/** Definitions nested in a body's inner scopes — blocks, function literals, case clauses,
  * comprehensions (docs/research/definition-inventory.md, *Nested scopes* and *Control flow*): each
  * is counted in its typing context, the signature lambda lifting gives it, which control flow
  * leaves as it is; a pattern's binders are refused, and so is every count under a pattern that may
  * refine a type. Every fixture compiles with Scala 3.9.0 (`$S/probes/b5/spec`,
  * `$S/probes/b5-fix2/spec`); each expected number is derived beside it.
  */
class NestedScopesSpec extends Specification with InventoryFixtures {

  def is = s2"""
    Nested scopes
      definitions of two branches are two rows                        $branchRows
      control flow leaves the typing context as it is                 $branchContext
      a case pattern or guard leaves the typing context as it is      $casePatterns
      a case clause's binder is refused, never counted                $caseBinders
      a given binder of a case clause is refused                      $givenBinders
      a function literal's parameter is a parameter                   $lambdaParameters
      an untyped function literal parameter is refused                $untypedLambda
      a scope of an inline method shares the method's verdict         $inlineOwners
  """

  private val nested = """
    |object Nested {
    |  def f(b: Boolean, x: Int): Boolean =
    |    if (b) {
    |      def loop(i: Int): Unit = ()
    |      loop(0); b
    |    } else {
    |      def loop(i: Int): Unit = ()
    |      loop(1); b
    |    }
    |  def g[A](a: A, d: A, c: Boolean): A =
    |    if (c) {
    |      def pick(z: A): A = z
    |      pick(a)
    |    } else d
    |  def one[A](a: A, c: Boolean): A =
    |    if (c) {
    |      def only: A = a
    |      only
    |    } else a
    |  def always[A](a: A, d: A): A =
    |    if (true) {
    |      def both: A = a
    |      both
    |    } else d
    |  def both(c: Boolean, d: Boolean): Boolean = c && { def q: Boolean = d; q }
    |  def h[A](o: Option[A], d: A): A = o match {
    |    case Some(a) =>
    |      def keep(z: A): A = z
    |      keep(a)
    |    case None => d
    |  }
    |}""".stripMargin

  def branchRows = {
    val found = rendered(nested)
    (found("Nested.f.<block#1>.loop") === "1").and(found("Nested.f.<block#2>.loop") === "1")
  }

  // A nested definition is counted in its typing context, as the method lambda lifting makes of it:
  // the branch around it narrows nothing. `pick` is a, d or z for each c: 3^2 = 9; `only` a alone,
  // 1; `both` a or d, 2; `q` a Boolean function of c and d, 16.
  def branchContext = {
    val found = rendered(nested)
    (found("Nested.g.<block#1>.pick") === "9")
      .and(found("Nested.one.<block#1>.only") === "1")
      .and(found("Nested.always.<block#1>.both") === "2")
      .and(found("Nested.both.<block#1>.q") === "16")
  }

  private val patterns = """
    |object Arms {
    |  def literal(b: Boolean): Boolean = b match {
    |    case true =>
    |      def g: Boolean = b
    |      g
    |    case false => false
    |  }
    |  def first(b: Boolean): Boolean = b match {
    |    case _ =>
    |      def g: Boolean = b
    |      g
    |  }
    |  def later(b: Boolean): Boolean = b match {
    |    case true => true
    |    case _ =>
    |      def g: Boolean = b
    |      g
    |  }
    |  def guarded(b: Boolean, c: Boolean): Boolean = b match {
    |    case _ if c =>
    |      def g: Boolean = b
    |      g
    |    case _ => false
    |  }
    |  def partial[A](a: A, d: A): Option[A] => A = {
    |    case Some(_) => def r: A = a; r
    |    case None => d
    |  }
    |}""".stripMargin

  // A pattern or a guard narrows no input of the typing context: g is a Boolean function of b, 4,
  // under `case true`, a later arm or a first `_`; of b and c, 16, under a guard. `Some(_)` cannot
  // pin A (Some's parent is Option of its own parameter), but the reading does not read the
  // pattern's class: with A in scope, `r` is refused (a or d, 2).
  def casePatterns = {
    val found = rows(patterns)
    (found("Arms.literal.<case#1>.g").render === "4")
      .and(found("Arms.later.<case#1>.g").render === "4")
      .and(found("Arms.guarded.<case#1>.g").render === "16")
      .and(found("Arms.first.<case#1>.g").render === "4")
      .and(
        refused(
          found("Arms.partial.<case#1>.r"),
          "pattern of an enclosing case clause may refine a type: case Some(_)"
        )
      )
  }

  // `a` is the scrutinee's part: reading it as an input of its own would count one value twice.
  def caseBinders =
    refused(
      rows(nested)("Nested.h.<case#1>.keep"),
      "binder of an enclosing case clause not read: a"
    )

  // `case given A` binds one more contextual value: g is that value, 1 (not 0); a Boolean function
  // of it, 4 (not 2). Refused like a named binder.
  def givenBinders = {
    val found = rows(
      """|object Givens {
         |  def pf[A]: A => A = {
         |    case given A =>
         |      def g: A = summon[A]
         |      g
         |  }
         |  def pfBool: Boolean => Boolean = {
         |    case given Boolean =>
         |      def g: Boolean = summon[Boolean]
         |      g
         |  }
         |}""".stripMargin
    )
    val reason = "binder of an enclosing case clause not read: given "
    refused(found("Givens.pf.<case#1>.g"), reason + "A")
      .and(refused(found("Givens.pfBool.<case#1>.g"), reason + "Boolean"))
  }

  def lambdaParameters = {
    val found = rendered(
      getter + """
        |object Lifted {
        |  def lifted[A](xs: List[A], d: A): List[Getter[A, A]] =
        |    xs.map((x: A) => new Getter[A, A] { def get(s: A): A = d })
        |}""".stripMargin
    )
    // s, x or d — and an element of xs, picked by its position: ω.
    found("Lifted.lifted.<lambda#1>.<new#1>.get") === "ω"
  }

  def untypedLambda =
    refused(
      rows(
        getter + """
          |object Untyped {
          |  def lifted[A](xs: List[A]): List[Getter[A, A]] =
          |    xs.map(x => new Getter[A, A] { def get(s: A): A = x })
          |}""".stripMargin
      )("Untyped.lifted.<lambda#1>.<new#1>.get"),
      "missing type of accessible value: x"
    )

  // A definition in a scope of an inline method is copied into every expansion (register D27): it
  // counts when the method's body passes — a Boolean function of c and the capture b, 16 — and
  // carries the method's refusal when it does not. Fixture `$S/probes/b5-land/nest.scala` (3.9.0).
  def inlineOwners = {
    val found = rows(
      """|import scala.compiletime.summonInline
         |object I {
         |  inline def outer(b: Boolean): Boolean =
         |    if (b) {
         |      def inner(c: Boolean): Boolean = c && b
         |      inner(b)
         |    } else b
         |  inline def refused(b: Boolean): Boolean =
         |    if (b) {
         |      def inner(c: Boolean): Boolean = c
         |      inner(summonInline[Boolean])
         |    } else b
         |}""".stripMargin
    )
    (found("I.outer.<block#1>.inner").render === "16").and(
      refused(
        found("I.refused.<block#1>.inner"),
        "definition inside an inline method: refused uses scala.compiletime.summonInline"
      )
    )
  }

}
