package cardinality

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
      reject multiple opaque observations                          $multipleCalls
      reject multiple argument values                              $multipleArguments
      reject multiple unrelated results selectable by a tag        $multipleResults
      reject applications enabled by exposed payloads               $crossCall
      reject payload endomorphisms and recall                       $payloadConsumer
      reject ambient payload producers                             $payloadProducer
      reject nested and higher-order capabilities                  $unknown
      keep Optional's dependent reverseGet unresolved              $optional
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

  private def unresolved(value: Count) = value must beLike {
    case Count.Unresolved(reasons) =>
      reasons.exists(_.contains("opaque sum-producing")) must beTrue
  }

  def option = count(List(pick), Function(List(s), opt)) === Count.Finite(2)
  def either = count(List(tear), Function(List(s), sum)) === Count.Finite(1)

  def unreachable =
    count(List(tear, Function(List(b), out)), Product(List(tear, Function(List(b), out)))) ===
      Count.Finite(1)

  def irrelevant =
    count(List(pick, Function(List(b), s)), Product(List(pick, Function(List(b), s)))) ===
      Count.Finite(2)

  def multipleCalls = unresolved(count(List(s, pick, pick), opt))
  def multipleArguments = unresolved(count(List(s, s, pick), opt))
  def multipleResults = unresolved(count(List(s, b, b, pick), b))

  def crossCall =
    unresolved(count(List(s, tear, Function(List(a), Sum(List(unit, b)))), sum))

  def payloadConsumer =
    unresolved(count(List(s, pick, Function(List(a), a)), opt)).and(
      unresolved(count(List(s, Function(List(s), Sum(List(unit, s)))), Sum(List(unit, s))))
    )

  def payloadProducer =
    unresolved(count(List(s, pick, Function(List(s), a)), opt)).and(
      unresolved(count(List(s, a, pick), opt))
    )

  def unknown =
    unresolved(count(List(s, Function(List(s), Sum(List(unit, Product(List(a, b)))))), opt))
      .and(unresolved(count(List(s, pick, Function(List(Function(List(a), a)), b)), opt)))

  def optional =
    unresolved(
      count(
        List(tear, Function(List(s, b), out)),
        Product(List(tear, Function(List(s, b), out)))
      )
    )

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
      .and(unresolved(found("Optional.<init>")))
      .and(unresolved(found("ManyCalls.<init>")))
      .and(unresolved(found("TransformPayload.<init>")))
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
