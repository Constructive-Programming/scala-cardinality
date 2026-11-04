package cardinality.analysis.inhabitation

import cardinality.reporting.*
import java.nio.file.Files
import org.specs2.Specification

class OpaqueSumForwardingSpec extends Specification {
  import Inhabitation.{Binding, Count, Shape}
  import Shape.*

  def is = s2"""
    Certified opaque sum forwarding
      count forwarding and constant None separately               $option
      count Either forwarding modulo sum eta                       $either
      prove unrelated applications unreachable                    $unreachable
      retain a unique unrelated result after observing a sum       $irrelevant
    Beyond the certificate: neutrals eliminated once (capabilities C10)
      multiple opaque observations combine their cases             $multipleCalls
      multiple argument values are one neutral each                $multipleArguments
      unrelated results are chosen per case                        $multipleResults
      an application its payloads enable is eliminated in turn     $crossCall
      a payload endomorphism cycles; a payload recall is unbounded $payloadConsumer
      ambient payload producers add choices per case               $payloadProducer
      nested payloads split; a higher-order observer is irrelevant $nestedAndHigherOrder
      Optional's reverseGet reads the cases of getOrModify         $optional
      report reduced optic constructors end to end                 $report
      compile and execute forwarding witnesses and counterexample  $compiled
  """

  private val s = Atom("S")
  private val a = Atom("A")
  private val b = Atom("B")
  private val out = Atom("T")
  private val unit = Product(Nil)
  private val opt = Sum(List(unit, a))
  private val sum = Sum(List(out, a))
  private val pick = Function(List(s), opt)
  private val tear = Function(List(s), sum)

  private def count(shapes: List[Shape], target: Shape): Count =
    Inhabitation.count(shapes.zipWithIndex.map((shape, i) => Binding(i.toString, shape)), target)

  def option = count(List(pick), Function(List(s), opt)) === Count.Finite(2)
  def either = count(List(tear), Function(List(s), sum)) === Count.Finite(1)

  def unreachable =
    count(List(tear, Function(List(b), out)), Product(List(tear, Function(List(b), out)))) ===
      Count.Finite(1)

  def irrelevant =
    count(List(pick, Function(List(b), s)), Product(List(pick, Function(List(b), s)))) ===
      Count.Finite(2)

  // Two observations (two pickers, or one picker of two values): the four case combinations answer
  // None (1); None or Some(a₂) (2); None or Some(a₁) (2); None, Some(a₁) or Some(a₂) (3) — 12.
  def multipleCalls = count(List(s, pick, pick), opt) === Count.Finite(12)
  def multipleArguments = count(List(s, s, pick), opt) === Count.Finite(12)
  // Either case picks one of the two B values: 2 · 2.
  def multipleResults = count(List(s, b, b, pick), b) === Count.Finite(4)

  // `tear(s)`: Left(t) gives Left(t); Right(a) enables `q(a)`, whose cases both give Right(a): 1.
  def crossCall =
    count(List(s, tear, Function(List(a), Sum(List(unit, b)))), sum) === Count.Finite(1)

  // `pick(s)`: None (1); Some(a) reaches a, f(a), f(f(a)), … — ω. A picker of its own argument type
  // has the unbounded family p(s), p(s'), … of neutrals.
  def payloadConsumer =
    (count(List(s, pick, Function(List(a), a)), opt) === Count.Countable).and(
      count(List(s, Function(List(s), Sum(List(unit, s)))), Sum(List(unit, s))) must beLike {
        case Count.Unresolved(reasons) =>
          reasons.exists(_.contains("sum elimination over an unbounded argument space")) must beTrue
      }
    )

  // None: None or Some(g(s)) (2); Some(a): also Some(a) (3) — 6, with an ambient `a` alike.
  def payloadProducer =
    (count(List(s, pick, Function(List(s), a)), opt) === Count.Finite(6))
      .and(count(List(s, a, pick), opt) === Count.Finite(6))

  // A product payload is flattened: None (1), Some((a, b)) gives None or Some(a) (2). A callable
  // observing a function builds only a `B` no goal reads.
  def nestedAndHigherOrder =
    (count(List(s, Function(List(s), Sum(List(unit, Product(List(a, b)))))), opt) ===
      Count.Finite(2))
      .and(count(List(s, pick, Function(List(Function(List(a), a)), b)), opt) === Count.Finite(2))

  // Memo E5: `S ⇒ Either[T, A]` is tear itself (1); `(S, B) ⇒ T` answers t or reverseGet in the
  // Left case and reverseGet in the Right one (2) — 2.
  def optional =
    count(
      List(tear, Function(List(s, b), out)),
      Product(List(tear, Function(List(s, b), out)))
    ) === Count.Finite(2)

  def report = {
    val directory = Files.createTempDirectory("opaque-forwarding")
    val path = directory.resolve("Optics.scala")
    val source = """case class PickFold[S,A](pick:S=>Option[A])
      |case class MendTearPrism[S,T,A,B](tear:S=>Either[T,A], mend:B=>T)
      |case class PickMendPrism[S,A,B](pick:S=>Option[A], mend:B=>S)
      |case class Optional[S,T,A,B](getOrModify:S=>Either[T,A], reverseGet:(S,B)=>T)
      |case class ManyCalls[S,A](first:S=>Option[A], second:S=>Option[A])
      |case class TransformPayload[S,A](pick:S=>Option[A], step:A=>A)
      |""".stripMargin
    val _ = Files.writeString(path, source)
    val found = Report.of(List(path)).methods.map(entry => entry.name -> entry.count).toMap
    (found("PickFold.<init>") === Count.Finite(2))
      .and(found("MendTearPrism.<init>") === Count.Finite(1))
      .and(found("PickMendPrism.<init>") === Count.Finite(2))
      .and(found("Optional.<init>") === Count.Finite(2))
      // Each `S ⇒ Option[A]` field reads both pickers: 12, twice.
      .and(found("ManyCalls.<init>") === Count.Finite(144))
      // `step` cycles on the picked A (and on A alone): ω.
      .and(found("TransformPayload.<init>") === Count.Countable)
  }

  def compiled = {
    import ForwardingExamples.*
    val picked = PickFold[Int, String](n => if (n > 0) Some("yes") else None)
    val forwarded = forward(picked)
    val discarded = discard(picked)
    val prism = MendTearPrism[Int, Long, String, Boolean](
      n => if (n > 0) Right("yes") else Left(0L),
      b => if (b) 1L else 0L
    )
    val optional = Optional[Int, Long, String, Boolean](_ => Left(42L), (_, _) => 0L)
    (forwarded.pick(1) === Some("yes"))
      .and(discarded.pick(1) === None)
      .and(forwardPrism(prism).tear(-1) === Left(0L))
      .and(forwardPrism(prism).mend(true) === 1L)
      .and(reuseLeft(optional)(0, true) === 42L)
      .and(optional.reverseGet(0, true) === 0L)
  }

}
