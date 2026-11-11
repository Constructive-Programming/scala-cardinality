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
    "compile: a member written without a type takes the compiler's, spelled by its name" in {
      withheld.find(_.name == "U.made").map(_.signature) must beSome(
        contain("made: _root_.ext.Schema")
      )
    }

    "keep the support out of the analysis: no row of its own, its declaration not supplied" in {
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

    "fail the test where the fixture is rejected all the same" in {
      rows(
        "V.scala" -> ("object V { def f: Missing = ??? }" + Compiled.withheld("class Other"))
      ) must
        throwAn[AssertionError]
    }
  }

}
