package cardinality.analysis

import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** Which conversions an inline body may meet. Under the compiled reading (decision D2 of
  * docs/plans/compiled-own-types.md) a candidate is a conversion the compiled body applies: a
  * subset of register D43's reach (v1.md §2), so refusals only leave. `toS` (in `Far`'s companion)
  * reads its call site, so a body that applies it is refused; a body that only may reach it counts
  * as its `def` twin (every (Int, Int) => Int, 2^(2^69)).
  */
class ConversionClosureSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    An inline conversion is a candidate for a body only where the body may find it
      in the companion of a type the body never reaches: not a candidate     $unrelated
      in the companion of a type the body writes, not applied                 $written
      reached through a referenced member's declared type, not applied       $referenced
      reached through the signature of a given it may summon, not applied    $summoned
      reached through a given of a reached companion, not applied            $transitive
      imported into the body's scope, not applied                             $imported
      imported into the body's scope and applied: refused                     $applied
    An inline extension is a candidate for a body only where the body may find it
      in the companion of a type the body never reaches: not a candidate     $extensionUnrelated
      in the companion of a type the body's signature writes: a member wins  $extensionWritten
      imported into the body's scope, not applied: a member wins              $extensionImported
  """

  private val lib = "lib.scala" -> """package lib
    |final class Far
    |object Far {
    |  implicit inline def toS(x: Int): String = scala.compiletime.codeOf(x)
    |}
    |final class Wrap(val far: Far)
    |object Helper { def mk: Wrap = new Wrap(new Far) }
    |final class Mid
    |object Mid { given far: Far = new Far }""".stripMargin

  private def app(imports: String, body: String) = "app.scala" -> s"""package app
    |$imports
    |object C {
    |  $body
    |  def firstD(t: (Int, Int)): Int = t._1
    |}""".stripMargin

  private val twin: Count = Count.Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, 69)))

  // Each body below reaches toS by D43 but the compiled tree applies no conversion: `t._1` is an
  // Int where an Int is expected. Not a candidate (D2), so first is its `def` twin.
  private def skips(imports: String, body: String) =
    counts(lib, app(imports, body))("app.C.first") === twin

  def unrelated = {
    val found = counts(lib, app("", "inline def first(t: (Int, Int)): Int = t._1"))
    (found("app.C.first") === found("app.C.firstD")).and(found("app.C.first") === twin)
  }

  def written = skips("", "inline def first(t: (Int, Int), f: lib.Far): Int = t._1")

  // Helper.mk declares Wrap, whose member far the body selects: far's declared type is Far.
  def referenced =
    skips(
      "",
      "inline def first(t: (Int, Int)): Int = { val w = lib.Helper.mk; val f = w.far; t._1 }"
    )

  // A given imported into the body's scope may be summoned: its type Far is reachable.
  def summoned = skips("import lib.Mid.given", "inline def first(t: (Int, Int)): Int = t._1")

  // The body writes Mid; Mid's companion is its implicit scope, whose given far has type Far.
  def transitive = skips("", "inline def first(t: (Int, Int), m: lib.Mid): Int = t._1")

  def imported = skips("import lib.Far.toS", "inline def first(t: (Int, Int)): Int = t._1")

  // A String result: the compiled body is toS(t._1), whose expansion reads its call site.
  def applied = refused(
    counts(lib, app("import lib.Far.toS", "inline def first(t: (Int, Int)): String = t._1"))(
      "app.C.first"
    ),
    "calls inline method lib.Far.toS, which uses scala.compiletime.codeOf"
  )

  // `n + y` on an untyped local (a qualifier whose own implicit scope is not read): an inline
  // extension + in Far's companion may answer when no member accepts the arguments, and reads its
  // call site. Unreached, firstE counts as its `def` twin (every Int => Int, 2^(2^37)).
  private val extLib = "lib.scala" -> """package lib
    |final class Far
    |object Far {
    |  extension (i: Int) inline def +(z: Long): Int = scala.compiletime.codeOf(z).length
    |}""".stripMargin

  private def extApp(imports: String, params: String) = "app.scala" -> s"""package app
    |$imports
    |object E {
    |  inline def firstE(y: Int$params): Int = { val n = y; n + y }
    |  def firstD(y: Int): Int = { val n = y; n + y }
    |}""".stripMargin

  def extensionUnrelated = {
    val found = counts(extLib, extApp("", ""))
    (found("app.E.firstE") === found("app.E.firstD"))
      .and(found("app.E.firstE") === Count.Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, 37))))
  }

  // `Far` is in the signature, so its companion's extension is in reach; but scalac types the
  // local n an Int, whose own `+(Int)` applies to `n + y`, and an extension is tried only where no
  // member applies: firstE is firstD, every Int => Int (2^(2^37)).
  def extensionWritten = {
    val found = counts(extLib, extApp("", ", f: lib.Far"))
    found("app.E.firstE") === Count.Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, 37)))
  }

  // In lexical scope, but as in extensionWritten the compiled `n + y` is Int's own `+`: no
  // extension is applied (D2), so firstE is every Int => Int (2^(2^37)).
  def extensionImported =
    counts(extLib, extApp("import lib.Far.*", ""))("app.E.firstE") ===
      Count.Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, 37)))

}
