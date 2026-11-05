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
      every control-flow form is read alike                           $flowForms
      a pattern that may refine a type refuses every count            $typeRefinement
      a case clause's binder is refused, never counted                $caseBinders
      a given binder of a case clause is refused                      $givenBinders
      a comprehension's binder is refused, never counted              $comprehensionBinders
      a function literal's parameter is a parameter                   $lambdaParameters
      a polymorphic function literal binds its type parameters        $polyLambda
      an untyped function literal parameter is refused                $untypedLambda
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

  // While, `||`, a dotted `&&`, a match spelled `b.match`, a dead branch, a by-name argument, a
  // comprehension's guard and a default the callee may never evaluate all leave the context as it
  // is: a Boolean function of one Boolean, 4; of two, 16; of an Option[Boolean]'s 3 values, 2^3.
  def flowForms = {
    val found = rendered(
      """|object Flow {
         |  def w(c: Boolean): Unit = while (c) { def g: Boolean = c; () }
         |  def o(c: Boolean, d: Boolean): Boolean = c || { def q: Boolean = d; q }
         |  def dot(b: Boolean): Boolean = b.&&({ def g: Boolean = b; g })
         |  def s(b: Boolean): Boolean = b.match { case _ => def g: Boolean = b; g }
         |  def dead(b: Boolean): Unit = if (false) { def g: Boolean = b; () } else ()
         |  def when(c: Boolean)(body: => Unit): Unit = if (c) body else ()
         |  def byName(b: Boolean): Unit = when(b) { def g: Boolean = b; () }
         |  def guard(b: Boolean): List[Boolean] =
         |    for { _ <- List((), ()); if b } yield { def g: Boolean = b; g }
         |  def lazily(o: Option[Boolean]): Boolean = o.getOrElse { def g: Boolean = o.isDefined; g }
         |}""".stripMargin
    )
    (found("Flow.w.<block#1>.g") === "4")
      .and(found("Flow.o.<block#1>.q") === "16")
      .and(found("Flow.dot.<block#1>.g") === "4")
      .and(found("Flow.s.<case#1>.g") === "4")
      .and(found("Flow.dead.<block#1>.g") === "4")
      .and(found("Flow.byName.<block#1>.g") === "4")
      .and(found("Flow.guard.<for#1>.<block#1>.g") === "4")
      .and(found("Flow.lazily.<block#1>.g") === "8")
  }

  private val refinements = """
    |sealed trait Ty[A]
    |final case class TInt() extends Ty[Int]
    |trait Box[A] { def get: A }
    |object Refine {
    |  def zero[A]: Ty[A] => A = {
    |    case TInt() =>
    |      def h: A = 42
    |      h
    |  }
    |  def anon[A](a: A): Ty[A] => Box[A] = { case TInt() => new Box[A] { def get: A = a } }
    |  def unit[A](t: Ty[A]): Unit = t match {
    |    case TInt() =>
    |      def u(x: A): Unit = ()
    |      ()
    |  }
    |  def typed[A]: Ty[A] => A = {
    |    case _: TInt =>
    |      def h: A = 42
    |      h
    |  }
    |  def generator[A](ts: List[Ty[A]]): List[A] =
    |    for (case TInt() <- ts) yield {
    |      def h: A = 42
    |      h
    |    }
    |  def mono(o: Option[Boolean]): Boolean = o match {
    |    case Some(_) =>
    |      def g: Boolean = true
    |      g
    |    case None => false
    |  }
    |  def literal[A](a: A, b: Boolean): A = b match {
    |    case true =>
    |      def h: A = a
    |      h
    |    case false => a
    |  }
    |}""".stripMargin

  // Matching TInt() makes A Int in the arm (3.9.0 accepts `42: A` there): h is any Int, 2^32, not
  // the 0 a rigid A gives; `get` any Int function of a, not 1. So every count is refused, 0 and 1
  // included — except a Unit result, which has one value whatever the context (`u`: 1). Without a
  // type parameter in scope there is nothing to refine (`mono`: a Boolean function of o's 3
  // values, 8), and a literal pattern refines nothing (`literal`: a alone, 1).
  def typeRefinement = {
    val found = rows(refinements)
    val reason = "pattern of an enclosing case clause may refine a type: "
    refused(found("Refine.zero.<case#1>.h"), reason + "case TInt()")
      .and(refused(found("Refine.anon.<case#1>.<new#1>.get"), reason + "case TInt()"))
      .and(found("Refine.unit.<case#1>.u").render === "1")
      .and(refused(found("Refine.typed.<case#1>.h"), reason + "case _: TInt"))
      .and(
        refused(
          found("Refine.generator.<for#1>.<block#1>.h"),
          "pattern of an enclosing for-comprehension may refine a type: TInt()"
        )
      )
      .and(found("Refine.mono.<case#1>.g").render === "8")
      .and(found("Refine.literal.<case#1>.h").render === "1")
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

  def comprehensionBinders =
    refused(
      rows("""|object Comp {
              |  def f(xs: List[Int]): Int = {
              |    for (x <- xs) { def g(u: Unit): Boolean = true; () }
              |    0
              |  }
              |}""".stripMargin)("Comp.f.<for#1>.<block#1>.g"),
      "binder of an enclosing for-comprehension not read: x"
    )

  // The type parameter is a binder of the literal, its value parameter a parameter: a alone, 1.
  def polyLambda =
    rendered("object Poly { val h: [A] => A => A = [A] => (a: A) => { def g: A = a; g } }")(
      "Poly.h.<lambda#1>.<lambda#1>.g"
    ) === "1"

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

}
