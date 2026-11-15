package cardinality.analysis

import cardinality.analysis.inhabitation.*
import cardinality.reporting.Report
import org.specs2.Specification

/** Reduced relevant scopes from the pinned EO archive. These tests check the ledger's derivations,
  * not just report snapshots.
  */
class EoCaseLedgerSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Reviewed EO cases
      CanGet has no producer of its independent result type       $canGet
      CanGetOption can return only the empty alternative          $canGetOption
      CanPlace counts its identity and supplied iteration apart  $canPlace
      CanModifyP.replace synthesizes a constant callback          $replace
      Optic.to has no producer of its rigid carrier               $optic
    Completeness guards
      an additional captured value changes a zero                $capturedResult
      an additional endomorphism changes an identity count        $capturedStep
      an additional result producer changes replace's count      $capturedFallback
  """

  private def entries(code: String): Map[String, Report.Entry] =
    cardinality.Compiled
      .analyze(List(cardinality.Compiled.Input("Ledger.scala", code)))
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

  // `X` is `Optic.this.X`, an atom a subclass picks (members M1), and `F` a rigid constructor (hkt
  // R1): `F[X, A]` has no producer — `from` yields a T — so no uniform body exists, as for the
  // reviewed `CanGet.get`: 0.
  def optic =
    entries("""
      trait Optic[S, T, A, B, F[_, _]]:
        type X
        def to(s: S): F[X, A]
        def from(b: F[X, B]): T
    """)("Optic.to").count === Count.Finite(0)

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

}
