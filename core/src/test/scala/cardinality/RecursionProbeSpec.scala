package cardinality.analysis

import scala.meta.*
import scala.meta.dialects.Scala3

import org.specs2.mutable.Specification

/** The current reading of self-referential (and mutually recursive) trait types, pinned while the
  * structural-proof lane is open (docs/research/recursive-structural-proof.md).
  *
  * A concrete instance of a recursive trait reads like any record (its member counts over its own
  * fields). A row read through the abstract recursive type refuses: the parameter's record shape
  * would have to count the atom stream the recursive member generates (r.self, r.self.self, …), and
  * no tier of that stream is implemented yet — the finite first-order atom set would undercount a
  * body that legally walks the stream (`r.self.k + 1` distinguishes more receivers than the
  * first-order atoms admit). The refusal names the structural proof the counting needs.
  */
class RecursionProbeSpec extends Specification {

  private def counts(code: String): Map[String, String] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("P.scala", Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count.toString)
      .toMap

  private val recursive = "trait R { def self: R; def k: Int }"

  "a concrete instance's member reads over its own fields" in {
    val rows = counts(
      s"$recursive\nobject R0 extends R { def self = this; def k = 1 }\n"
    )
    rows("R0.k") must contain("Finite(4294967296)")
  }

  "a member row of the recursive trait refuses with the record's self-view" in {
    val rows = counts(recursive)
    rows("R.k") must contain(
      "capability record member may be the measured declaration on its receiver: R.k"
    )
  }

  "a row read through the abstract recursive type refuses with the structural proof" in {
    val rows = counts(s"$recursive\ndef f(r: R): Int = ???")
    rows("f") must contain("recursive type requires a structural proof: R")
  }

  "the refusal is the same with a seed parameter, at a goal, or through an alias" in {
    val withSeed = counts(s"$recursive\ndef f(r: R, b: Boolean): R = ???")
    val atGoal = counts(s"$recursive\ndef f(b: Boolean): R = ???")
    val throughAlias = counts(s"$recursive\ntype S = R\ndef f(b: Boolean): S = ???")
    (withSeed("f") must contain("recursive type requires a structural proof: R"))
      .and(atGoal("f") must contain("recursive type requires a structural proof: R"))
      .and(throughAlias("f") must contain("recursive type requires a structural proof: R"))
  }

  "mutually recursive traits refuse the same way" in {
    val rows = counts(
      "trait A { def b: B; def k: Int }\ntrait B { def a: A; def j: Int }\ndef f(a: A): Int = ???"
    )
    rows("f") must contain("recursive type requires a structural proof: A")
  }
}
