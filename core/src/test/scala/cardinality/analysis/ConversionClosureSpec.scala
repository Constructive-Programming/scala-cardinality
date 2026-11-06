package cardinality.analysis

import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** Which conversions an inline body may meet (register D43, v1.md §2): one in its lexical scope, or
  * one whose object is in the implicit scope of a type the body can reach — the types it writes,
  * the signatures around it, and the signatures of what it references or may summon, to a fixed
  * point. `toS` (in `Far`'s companion) reads its call site, so a body that may meet it is refused;
  * one that may not counts as its `def` twin (every (Int, Int) => Int, 2^(2^69)).
  */
class ConversionClosureSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    An inline conversion is a candidate for a body only where the body may find it
      in the companion of a type the body never reaches: not a candidate     $unrelated
      in the companion of a type the body writes                              $written
      reached through a referenced member's declared type                    $referenced
      reached through the signature of a given the body may summon           $summoned
      reached through a given of a reached companion, transitively            $transitive
      imported into the body's scope                                          $imported
    An inline extension is a candidate for a body only where the body may find it
      in the companion of a type the body never reaches: not a candidate     $extensionUnrelated
      in the companion of a type the body's signature writes                 $extensionWritten
      imported into the body's scope                                          $extensionImported
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

  private def meets(imports: String, body: String) =
    refused(counts(lib, app(imports, body))("app.C.first"), "may expand the inline conversion toS")

  def unrelated = {
    val found = counts(lib, app("", "inline def first(t: (Int, Int)): Int = t._1"))
    (found("app.C.first") === found("app.C.firstD")).and(found("app.C.first") === twin)
  }

  def written = meets("", "inline def first(t: (Int, Int), f: lib.Far): Int = t._1")

  // Helper.mk declares Wrap, whose member far the body selects: far's declared type is Far.
  def referenced =
    meets(
      "",
      "inline def first(t: (Int, Int)): Int = { val w = lib.Helper.mk; val f = w.far; t._1 }"
    )

  // A given imported into the body's scope may be summoned: its type Far is reachable.
  def summoned = meets("import lib.Mid.given", "inline def first(t: (Int, Int)): Int = t._1")

  // The body writes Mid; Mid's companion is its implicit scope, whose given far has type Far.
  def transitive = meets("", "inline def first(t: (Int, Int), m: lib.Mid): Int = t._1")

  def imported = meets("import lib.Far.toS", "inline def first(t: (Int, Int)): Int = t._1")

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

  private val extended = "which an extension of that name may answer"

  def extensionUnrelated = {
    val found = counts(extLib, extApp("", ""))
    (found("app.E.firstE") === found("app.E.firstD"))
      .and(found("app.E.firstE") === Count.Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, 37))))
  }

  def extensionWritten =
    refused(counts(extLib, extApp("", ", f: lib.Far"))("app.E.firstE"), extended)

  def extensionImported =
    refused(counts(extLib, extApp("import lib.Far.*", ""))("app.E.firstE"), extended)

}
