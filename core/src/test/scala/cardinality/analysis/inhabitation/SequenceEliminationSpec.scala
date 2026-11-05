package cardinality.analysis.inhabitation

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import org.specs2.Specification
import org.specs2.execute.Result

/** Supplied sequences eliminated by length (`SequenceElimination`): `0` when the empty sequence
  * leaves the goal empty, `1` for a one-valued goal, `ω` when some nonempty length already admits
  * two answers, `?` otherwise and wherever a sequence sits that no length test reaches. Every
  * expected number is derived in its comment; the fixtures are valid Scala 3.9.0.
  */
class SequenceEliminationSpec extends Specification {
  import Inhabitation.{Binding, Count, Shape}
  import Shape.*

  def is = s2"""
    Counted by length
      the empty sequence decides 0                                  $empty
      two answers at a nonempty length decide ω                     $omega
      a one-valued goal is 1                                        $unit
      an empty element type leaves only Nil                         $emptyElements
      a field of a product is a sequence of its own                 $field
      a sequence of sequences is eliminated level by level          $nested
      a callable taking a sequence is applied to built ones         $consumer
      closed sequences stay transparent data                        $closed
      three sequences are within the budget                         $threeSequences
      an absurd element type beside a branch keeps the other's choice $absurdBranch
    Refused with a reason
      a callable's sequence result                                  $callableResult
      a sequence inside a supplied sum                              $insideSum
      a sequence parameter of a built function                      $builtFunction
      more than three sequences                                     $budget
      the fields of a receiver only its own factories build         $constrainedReceiver
      a class only its scope may build: private, nested, local      $scopedReceivers
      a sequence under an existential or a proof                    $hiddenSequence
  """

  private val a = Atom("A")
  private val b = Atom("B")

  private def entry(code: String, name: String): Count =
    MethodAnalysis
      .analyze(
        List(MethodAnalysis.Input("Seq.scala", dialects.Scala3(code).parse[Source].get))
      )
      .find(_.name == name)
      .get
      .count

  private def refused(found: Count, fragment: String): Result =
    found must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(fragment)) must beTrue
    }

  // No A at length 0, so no total implementation: from a List, from varargs, from a Seq that may
  // be infinite (its empty case is still an input).
  def empty = {
    val code = """object E {
      |  def total[A](xs: List[A]): A = ???
      |  def varargTotal[A](xs: A*): A = ???
      |  def seqTotal[A](xs: Seq[A]): A = ???
      |}""".stripMargin
    (entry(code, "E.total") === Count.Finite(0))
      .and(entry(code, "E.varargTotal") === Count.Finite(0))
      .and(entry(code, "E.seqTotal") === Count.Finite(0))
  }

  // headOption: None at 0; None or Some(e₁) at 1. isEmpty: true/false at every length. tail: Nil
  // at 0; every list of e₁ at 1. A lazy Seq is read by the same total observations.
  def omega = {
    val code = """object E {
      |  def headOption[A](xs: List[A]): Option[A] = ???
      |  def isEmpty[A](xs: List[A]): Boolean = ???
      |  def tail[A](xs: List[A]): List[A] = ???
      |  def lazyHead[A](xs: Seq[A]): Option[A] = ???
      |  def varargHead[A](xs: A*): Option[A] = ???
      |  def fromVector[A](xs: Vector[A]): Option[A] = ???
      |}""".stripMargin
    List("headOption", "isEmpty", "tail", "lazyHead", "varargHead", "fromVector")
      .map(name => entry(code, s"E.$name") === Count.Countable)
      .reduce(_.and(_))
  }

  def unit = entry("object E { def unit[A](xs: List[A]): Unit = () }", "E.unit") === Count.Finite(1)

  // `List[Nothing]` is only Nil: Boolean => Boolean, 4 (identity, negation, two constants).
  def emptyElements =
    entry("object E { def f(xs: List[Nothing], b: Boolean): Boolean = ??? }", "E.f") ===
      Count.Finite(4)

  // `p._2` at length 0; `p._2` or e₁ at length 1.
  def field =
    entry("object E { def pair[A](p: (List[A], A)): A = ??? }", "E.pair") === Count.Countable

  // An empty outer list leaves no A (0); one inner list of one element already gives None or
  // Some (ω).
  def nested = {
    val code = """object E {
      |  def total[A](xs: List[List[A]]): A = ???
      |  def first[A](xs: List[List[A]]): Option[A] = ???
      |}""".stripMargin
    (entry(code, "E.total") === Count.Finite(0)).and(entry(code, "E.first") === Count.Countable)
  }

  // g(Nil), g(List(a)), g(List(a, a)), …: the list is built, not eliminated.
  def consumer =
    entry("object E { def c[A, B](g: List[A] => B, a: A): B = ??? }", "E.c") === Count.Countable

  // A List[Int] is split like any closed value: ω inputs, 2^32 answers each.
  def closed =
    entry("object E { def ints(xs: List[Int]): Int = ??? }", "E.ints") === Count.Countable

  // None at length 0 of all three; None or Some(e) once any one holds an element: ω, at the budget.
  def threeSequences =
    entry(
      "object E { def three[A](a: List[A], b: List[A], c: List[A]): Option[A] = ??? }",
      "E.three"
    ) === Count.Countable

  // `A => Nothing` elements beside an A make every nonempty length absurd (one answer), so only
  // length 0 has a choice: the true count is 2 (a or b), not ω — refused rather than overcounted.
  def absurdElems =
    entry(
      "object E { def f[A](a: A, b: A, xs: List[A => Nothing]): A = ??? }",
      "E.f"
    )

  // Cross-check X2: the Left branch is absurd once an element exists, the Right branch keeps
  // true/false, so c(1) = 1 · 2 = 2 at a nonempty length — ω. Without the absurd element: the
  // length alone (c(0) = c(1) = 2) and headOption (c(0) = 1, c(1) = 2) are ω too.
  def absurdBranch = {
    val code = """object T {
      |  def varBranch[A](y: Either[A => Nothing, Unit], xs: A*): Boolean = true
      |  def varPlain[A](xs: A*): Boolean = true
      |  def varHead[A](xs: A*): Option[A] = None
      |}""".stripMargin
    List("varBranch", "varPlain", "varHead")
      .map(name => entry(code, s"T.$name") === Count.Countable)
      .reduce(_.and(_))
  }

  // b at every length counted: the elements reach no B.
  def unique =
    refused(
      entry("object E { def f[A, B](xs: List[A], b: B): B = ??? }", "E.f"),
      "sequence elimination: goal unique up to length 2 but not structurally singleton"
    )

  def callableResult =
    refused(
      entry("object E { def f[A, B](f: A => List[B], a: A): Option[B] = ??? }", "E.f"),
      "Repeated-argument sequence elimination is unsupported: a sequence a supplied callable returns"
    )

  def insideSum =
    refused(
      entry("object E { def f[A](o: Option[List[A]]): Option[A] = ??? }", "E.f"),
      "Repeated-argument sequence elimination is unsupported: a sequence inside a supplied sum"
    )

  // The returned function's sequence is an input of another environment than the method's.
  def builtFunction =
    refused(
      Inhabitation.count(List(Binding("x", a)), Product(List(a, Function(List(Repeated(b)), b)))),
      "Repeated-argument sequence elimination is unsupported: a sequence parameter of a built function"
    )

  def budget =
    refused(
      entry(
        "object E { def f[A](a: List[A], b: List[A], c: List[A], d: List[A]): Option[A] = ??? }",
        "E.f"
      ),
      "sequence elimination budget exceeded: 4 sequences (at most 3)"
    )

  // `NonEmpty.of` is the only builder, so `xs` is never Nil: `head` is total (xs.head, xs.last, …),
  // not 0. A public constructor admits Nil, where no A exists: 0.
  def constrainedReceiver = {
    val code = """final class NonEmpty[A] private (val xs: List[A]) { def head: A = xs.head }
      |object NonEmpty { def of[A](a: A, rest: A*): NonEmpty[A] = new NonEmpty(a :: rest.toList) }
      |final class Open[A](val xs: List[A]) { def head: A = ??? }""".stripMargin
    refused(
      entry(code, "NonEmpty.head"),
      "receiver fields constrained by a non-public constructor: NonEmpty"
    ).and(entry(code, "Open.head") === Count.Finite(0))
  }

  // Review fixture c6 (valid Scala 3.9.0): a private class, a package-private one and a class local
  // to a method are built only by their scope's code — `Factory.first` builds `a :: rest`, `run`
  // builds `List(a)` — so `head` is total there (true counts ω and 1), never 0 or ω over every
  // field value. A class nested in a private object and a protected class likewise; a public class
  // nested in a public object keeps its count (0: `new Public(Nil)` exists).
  def scopedReceivers = {
    val code = """package c6
      |object Factory {
      |  private final class NonEmpty[A](val xs: List[A]) { def head: A = xs.head }
      |  def first[A](a: A, rest: List[A]): A = new NonEmpty(a :: rest).head
      |}
      |private[c6] final class Pkg[A](val xs: List[A]) { def head: A = xs.head }
      |object Outer {
      |  def run[A](a: A): A = {
      |    final class Local(val xs: List[A]) { def head: A = xs.head }
      |    new Local(List(a)).head
      |  }
      |}
      |private object Hidden { final class Inner[A](val xs: List[A]) { def head: A = xs.head } }
      |object Open {
      |  final class Public[A](val xs: List[A]) { def head: A = xs.head }
      |  protected final class Prot[A](val xs: List[A]) { def head: A = xs.head }
      |}""".stripMargin
    def constrained(name: String, why: String) =
      refused(entry(code, s"c6.$name.head"), s"receiver fields constrained by $why: c6.$name")
    constrained("Factory.NonEmpty", "a non-public class")
      .and(constrained("Pkg", "a non-public class"))
      .and(constrained("Outer.run.Local", "a class local to c6.Outer.run"))
      .and(constrained("Hidden.Inner", "a class nested in non-public c6.Hidden"))
      .and(constrained("Open.Prot", "a non-public class"))
      .and(entry(code, "c6.Open.Public.head") === Count.Finite(0))
      .and(entry(code, "c6.Outer.run") === Count.Finite(1))
  }

  // `List[?]` opens to a list of an unknown witness: the elimination does not open existentials.
  def hiddenSequence =
    refused(
      entry("object H { def f(xs: List[?]): Boolean = ??? }", "H.f"),
      "Repeated-argument sequence elimination is unsupported: " +
        "a sequence under an existential or a proof"
    )

}
