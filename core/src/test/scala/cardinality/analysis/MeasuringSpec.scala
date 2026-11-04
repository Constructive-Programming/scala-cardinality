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
