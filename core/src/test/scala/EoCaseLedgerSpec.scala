package cardinality

import scala.meta.*

import org.specs2.Specification

/** Reduced relevant scopes from the pinned EO archive; the opt-in EoBaseline runner reads all 53
  * original sources. These tests check the ledger's derivations, not just report snapshots.
  */
class EoCaseLedgerSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Reviewed EO cases
      CanGet has no producer of its independent result type       $canGet
      CanGetOption can return only the empty alternative          $canGetOption
      CanPlace counts its identity and supplied iteration apart  $canPlace
      CanModifyP.replace synthesizes a constant callback          $replace
      Optic.to remains unresolved without its carrier model      $optic
    Completeness guards
      an additional captured value changes a zero                $capturedResult
      an additional endomorphism changes an identity count        $capturedStep
      an additional result producer changes replace's count      $capturedFallback
    Review identity
      moving anonymous extension and given owners keeps keys     $movedOwners
      different extension receivers retain different keys        $differentReceivers
    Input integrity
      reject bytes other than the pinned EO archive               $wrongChecksum
  """

  private def entries(code: String): Map[String, MethodAnalysis.Entry] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Ledger.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry)
      .toMap

  def canGet =
    entries("trait CanGet[S, A] { def get(s: S): A }")("CanGet.get").count === Count.Finite(0)

  def canGetOption =
    entries("trait CanGetOption[S, A] { def getOption(s: S): Option[A] }")(
      "CanGetOption.getOption"
    ).count === Count.Finite(1)

  def canPlace = {
    val measured = entries("""
      trait CanPlace[T, B]:
        def place(b: B): T => T
        def transfer[C](f: C => B): T => C => T = t => c => place(f(c))(t)
    """)
    (measured("CanPlace.place").count === Count.Finite(1))
      .and(measured("CanPlace.transfer").count === Count.Countable)
      .and(measured("CanPlace.transfer").captures === List("place"))
  }

  def replace =
    entries("""
      trait CanModifyP[S, T, A, B]:
        def modify(f: A => B): S => T
        def replace(b: B): S => T = modify(_ => b)
    """)("CanModifyP.replace").count === Count.Finite(1)

  def optic =
    entries("""
      trait Optic[S, T, A, B, F[_, _]]:
        type X
        def to(s: S): F[X, A]
        def from(b: F[X, B]): T
    """)("Optic.to").count must beAnInstanceOf[Count.Unresolved]

  def capturedResult =
    entries("""
      trait CanGet[S, A]:
        val default: A
        def get(s: S): A
    """)("CanGet.get").count === Count.Finite(1)

  def capturedStep =
    entries("""
      trait CanPlace[T, B]:
        val step: T => T
        def place(b: B): T => T
    """)("CanPlace.place").count === Count.Countable

  def capturedFallback =
    entries("""
      trait CanModifyP[S, T, A, B]:
        val fallback: T
        def modify(f: A => B): S => T
        def replace(b: B): S => T = modify(_ => b)
    """)("CanModifyP.replace").count === Count.Finite(2)

  private def keys(code: String) = {
    val tree = dialects.Scala3(code).parse[Source].get
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Owners.scala", tree)))
      .map(entry => EoBaseline.identity(entry, tree))
      .toSet
  }

  def movedOwners = {
    val code = """
      trait Maker[A] { def make(a: A): A }
      given [A]: Maker[A] with
        def make(a: A): A = a
      extension (a: Boolean) def value: Boolean = a
    """
    keys(code) === keys("\n\n" + code)
  }

  def differentReceivers = {
    val measured = keys("""
      extension (a: Boolean) def value: Unit = ()
      extension (a: Unit) def value: Unit = ()
    """)
    measured.size === 2
  }

  def wrongChecksum =
    EoBaseline.verifyChecksum(Array[Byte](0)) must
      throwA[IllegalArgumentException](".*EO sources checksum mismatch.*")

}
