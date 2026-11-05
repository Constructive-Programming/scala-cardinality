package cardinality.analysis

import scala.meta.*

import cardinality.capacity.Library
import org.specs2.mutable.Specification

/** Workers measure shares of the targets over one shared library: the rows are the sequential
  * measurement's, in its order, however the targets are split and whoever solves a package first.
  */
class MeasuringSpec extends Specification {

  private def input(path: String, code: String) =
    MethodAnalysis.Input(path, dialects.Scala3(code).parse[Source].get)

  // Packages reading each other (a cycle, solved in rounds), sealed families, records and
  // polymorphic members, spread over files so every worker reads packages another one solves.
  private val inputs = List(
    input(
      "a/A.scala",
      "package pa\nimport pb.*\nsealed trait S\ncase object S1 extends S\n" +
        "final case class S2(b: Boolean, t: T) extends S\ndef f(s: S): Boolean = ???"
    ),
    input(
      "b/B.scala",
      "package pb\nimport pa.*\nfinal case class T(n: Boolean)\n" +
        "def g(s: S, t: T): T = ???\ndef h[A](x: A, y: A): A = ???"
    ),
    input(
      "c/C.scala",
      "package pc\nimport pa.*, pb.*\ntrait Rec { def a: S; def b: T }\n" +
        "def k(r: Rec): Boolean = ???\ndef m(b: Boolean): S = ???"
    ),
    input(
      "d/D.scala",
      "package pd\nenum Suit { case H, S }\nopaque type Flag = Boolean\n" +
        "def n(s: Suit, f: Flag): Suit = ???\ndef o(f: Boolean => Boolean): Boolean = ???"
    )
  )

  private def render(rows: List[MethodAnalysis.Entry]) = rows.map(e => (e.path, e.name, e.count))

  "workers give the sequential rows, in its order" in {
    val sequential = render(MethodAnalysis.analyze(inputs))
    (render(Measuring.rows(inputs, workers = 1)) must_== sequential)
      .and(render(Measuring.rows(inputs, workers = 4)) must_== sequential)
      .and(render(Measuring.rows(inputs.reverse, workers = 3)) must_== sequential)
  }

  // The steps measuring `Host.unit` spends, with `params` as Host's constructor parameters.
  private def spent(params: String): Long = {
    val code =
      "package pr\nfinal case class Leaf(a: Mid, b: Mid, c: Option[Mid])\n" +
        "final case class Mid(x: Boolean, y: Option[Boolean], z: Either[Boolean, Boolean])\n" +
        (1 to 12).map(i => s"trait Rec$i { def m$i(x: Leaf): Leaf; def n$i: Leaf }\n").mkString +
        "trait Rec extends " + (1 to 12).map(i => s"Rec$i").mkString(" with ") + "\n" +
        s"class Host($params) { def unit(): Unit = () }"
    val sources = List(input("r/R.scala", code))
    val work = new AnalysisWork(Long.MaxValue, MethodAnalysis.Limits().maxTypeDepth)
    val index = new MethodAnalysis.Index(sources, MethodAnalysis.Limits(), Some(work))
    val target = index.selected(Set("r/R.scala"), Set("pr.Host.unit")).head
    work.target(Measuring.TargetWork)
    index.measure(target).count.render must_== "1"
    work.consumed
  }

  // A receiver's record is read once per target: a second parameter of the same record type
  // meets the readings of the first again (member function types are built once per member, and
  // a target's readings and name lookups are kept), so it costs a fraction of the first: only the
  // readings at its own reference (the record itself, a member's built function type) are new.
  "a target reads a record met again from what it already read" in {
    val none = spent("")
    val one = spent("r: Rec") - none
    val two = spent("r: Rec, s: Rec") - none
    // Read afresh, the second record costs what the first does (20,945 steps each before).
    (one must be_>(0L)).and(two - one must be_<(one * 2 / 3))
  }

  "a library shared by every worker reads as one built per worker" in {
    val library = Library.of(inputs.map(_.tree))
    val sequential = render(MethodAnalysis.analyze(inputs))
    (1 to 5)
      .map(_ =>
        render(Measuring.rows(inputs, workers = 4, library = Some(library))) must_== sequential
      )
      .reduce(_ and _)
  }
}
