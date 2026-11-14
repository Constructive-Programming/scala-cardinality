package cardinality

import scala.meta.*
import scala.meta.dialects.Scala3

import cardinality.analysis.MethodAnalysis
import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.mutable.Specification

/** The fixture harness's withheld support (`Compiled.withheld`): a declaration the fixture names is
  * compiled, so the fixture compiles, and is kept from the analysis, so the analyzer still reads it
  * as not supplied.
  *
  * `Schema` holds one `Boolean`. Supplied, `keep(s: Schema): Schema` has `|Schema → Schema| = 2^2 =
  * 4` implementations; withheld, its type is a declaration the analysis does not have.
  */
class CompiledSpec extends Specification {

  private val schema =
    "package ext\nfinal class Schema(val b: Boolean)\nobject Make { def one: Schema = new Schema(true) }"

  private val user = "U.scala" ->
    "import ext.Schema\nobject U { def keep(s: Schema): Schema = s; val made = ext.Make.one }"

  private def rows(sources: (String, String)*): List[MethodAnalysis.Entry] =
    Compiled.analyze(Compiled.inputs(sources*))

  private lazy val withheld = rows(user._1 -> (user._2 + Compiled.withheld(schema)))

  "a fixture with withheld support" should {
    "keep the support out of the analysis: no row of its own, its declaration not supplied" in Pending
      .knownLoss {
        (withheld.map(_.path).distinct === List("U.scala"))
          .and(withheld.find(_.name == "U.keep").map(_.count) must beSome(beLike[Count] {
            case Count.Unresolved(reasons) => reasons.mkString must contain("Schema")
          }))
      }

    "read the same declaration when it is supplied as a source: 4" in {
      val supplied = rows(user, "ext/Schema.scala" -> schema)
      (supplied.find(_.name == "U.keep").map(_.count) must beSome(Count.Finite(4)))
        .and(supplied.map(_.name) must contain("ext.Schema.<init>"))
    }

    "not parse where the text is read without `Compiled.inputs`" in {
      Scala3(user._2 + Compiled.withheld(schema)).parse[Source].toOption must beNone
    }

    "fail the test where scalac rejects a fixture that is not listed as rejected" in {
      rows("V.scala" -> "object V { def f: Missing = ??? }") must
        throwAn[AssertionError](message = "does not list")
    }
  }

  // What `Overriders` writes: nothing for the final class, the final method and the abstract one; a
  // nested trait inside its class's overrider, a class in an object beside the object.
  "the overriders of a fixture" should {
    "extend each class and trait a subclass may extend, and override its concrete methods" in {
      val code = "trait T[A] { def get: A; def m(a: A)(using o: Ordering[A]): A = a }\n" +
        "class C(val n: Int) { def ??? : Int = n; final def p: Int = n; trait I { def k: Int = n } }\n" +
        "final class F { def m: Int = 1 }\n" +
        "object O { class D { def m[B](b: B): Int = 1 } }"
      val written =
        Overriders.opened(Compiled.inputs("Open.scala" -> code)).head.text
      written.stripPrefix(code).linesIterator.filter(_.nonEmpty).toList === List(
        "trait T__O[A] extends T[A] { override def m(a: A)(using o: Ordering[A]) : A = super.m(a)(using o) }",
        "class C__O(n: Int) extends C(n) { override def ??? : Int = super.???; " +
          "trait I__O extends I { override def k : Int = super.k } }",
        "class D__O extends O.D { override def m[B](b: B) : Int = super.m[B](b) }"
      )
    }
  }

  // `val n = 1` is an Int to scalac and `val n = "s"` a String. The report over the files on disk
  // is given their classes, and matches each source to its own although one path ends the other:
  // Top.n counts an Int's 2^32 and Low.n a String's ω; matched to the other's, `n` would have its
  // type.
  "a report over a fixture's files" should {
    "read each source with the compiler's types of its own classes" in {
      val rendered = Compiled
        .report(
          "Exp.scala" -> "object Top { val n = 1 }",
          "lib/Exp.scala" -> "object Low { val n = \"s\" }"
        )
        .render
        .linesIterator
        .toList
      (rendered.find(_.contains("Top.n ")) must beSome(startWith("2^32 ")))
        .and(rendered.find(_.contains("Low.n ")) must beSome(startWith("ω ")))
    }
  }

}
