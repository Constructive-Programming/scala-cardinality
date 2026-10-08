package cardinality.analysis.inhabitation

import scala.meta.*

import cardinality.analysis.*
import org.specs2.Specification

class EvidenceTransportCardinalitySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Identity-preserving evidence
      synthesize reflexive equality and subtyping proofs                $reflexive
      normalize repeated reflexive coercions rather than invent omega  $identity
      transport distinct binders only under an available equality proof $transport
      preserve independent values and erase duplicate proof choices    $provenance
      transport through products and arrow shapes                       $structuralTransport
      count constructor inputs without counting proofs as fresh data    $constructor
      keep subtype transport directed and provenance preserving         $subtyping
      compose subtype paths without creating identity cycles            $subtypePaths
      resolve qualified prefix evidence with the same rules              $prefix
    Constraints and unsupported relationships
      do not merge binders without supplied evidence                     $absent
      do not use evidence mentioned only in a result                     $resultOnly
      do not use an inaccessible receiver proof                          $inaccessible
      report structural evidence endpoints as unsupported                $structuralEvidence
      enforce endpoint limits for direct solver callers                  $directSolver
      report subtype variance and callable interactions as unsupported   $subtypeFunction
      do not turn a callable returning evidence into an assumption       $callableProof
  """

  private def count(code: String, name: String = "use"): Count =
    MethodAnalysis
      .analyze(
        List(MethodAnalysis.Input("Evidence.scala", dialects.Scala3(code).parse[Source].get))
      )
      .find(_.name == name)
      .get
      .count

  private def unsupported(code: String, reason: String) =
    count(code) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains(reason)) must beTrue
    }

  def reflexive =
    (count("def use[A]: A =:= A = summon[A =:= A]") === Count.Finite(1))
      .and(count("def use[A]: A <:< A = summon[A <:< A]") === Count.Finite(1))

  def identity =
    count("def use[A](x: A, ev: A =:= A, sub: A <:< A): A = ev(sub(x))") === Count.Finite(1)

  def transport =
    (count("def use[A, B](x: A)(using ev: A =:= B): B = ev(x)") === Count.Finite(1))
      .and(count("def use[A, B](x: B)(using ev: A =:= B): A = ev.flip(x)") === Count.Finite(1))

  def provenance =
    count("def use[A, B](x: A, y: B, ev: A =:= B, again: A =:= B): B = ev(x)") === Count
      .Finite(2)

  def structuralTransport =
    (count("def use[A, B](x: (A, A))(using ev: A =:= B): (B, B) = ???") === Count.Finite(4))
      .and(
        count("def use[A, B](using ev: A =:= B): A => B = a => ev(a)") === Count.Finite(1)
      )

  def constructor =
    count("class Carrier[A, B](x: A, y: B, ev: A =:= B)", "Carrier.<init>") === Count.Finite(4)

  def subtyping =
    (count("def use[A, B](x: A, ev: A <:< B): B = ev(x)") === Count.Finite(1))
      .and(count("def use[A, B](x: B, ev: A <:< B): A = ???") === Count.Finite(0))
      .and(count("def use[A, B](x: A, y: B, ev: A <:< B): B = y") === Count.Finite(2))

  def subtypePaths =
    count(
      "def use[A, B, C](x: A, ab: A <:< B, bc: B <:< C, ca: C <:< A): C = bc(ab(x))"
    ) === Count.Finite(1)

  def prefix =
    count("def use[A, B](x: A, ev: scala.=:=[A, B]): B = ev(x)") === Count.Finite(1)

  def absent =
    count("def use[A, B](x: A): B = ???") === Count.Finite(0)

  def resultOnly =
    unsupported("def use[A, B](x: A): (A =:= B, B) = ???", "no available proof")

  def inaccessible =
    count(
      "class Hidden[A, B](private val ev: A =:= B)\ndef use[A, B](x: A): B = ???"
    ) === Count.Finite(0)

  def structuralEvidence =
    unsupported(
      "def use[A, B](x: A, ev: (A, A) =:= (B, B)): B = ???",
      "free-atom endpoints"
    )

  def directSolver = {
    import Inhabitation.{Binding, Shape}
    val unit = Shape.Product(Nil)
    Inhabitation.count(
      List(Binding("proof", Shape.Evidence(unit, unit, equality = true))),
      unit
    ) must beLike { case Count.Unresolved(_) => ok }
  }

  def subtypeFunction =
    unsupported(
      "def use[A, B](ev: A <:< B): A => B = a => ev(a)",
      "only atomic values and products"
    )

  def callableProof =
    unsupported(
      "def use[A, B](x: A, proof: Unit => (A =:= B)): B = ???",
      "no available proof"
    )

}

// These reduced fixtures compile against Scala's real sealed evidence classes, not just scalameta.
private object AtomicEvidenceCompilationFixture {
  def equality[A, B](x: A)(using ev: A =:= B): B = ev(x)
  def reverse[A, B](x: B)(using ev: A =:= B): A = ev.flip(x)
  def subtype[A, B](x: A)(using ev: A <:< B): B = ev(x)
  def reflexive[A]: A =:= A = summon[A =:= A]
}
