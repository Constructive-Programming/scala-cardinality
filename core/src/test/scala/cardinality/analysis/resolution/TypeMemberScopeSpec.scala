package cardinality.analysis.resolution

import scala.compiletime.testing.typeChecks
import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

// The scope a member reading binds names in (second review of B2): refinement bodies, fields read
// for a receiver, templates only some scope may extend. Every fixture is valid Scala 3.9.0 unless
// marked (`$S/probes/b2fix2/spec`); the compiler facts are pinned against `memberfixture`.
class TypeMemberScopeSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Templates only some scope may extend
      a sealed family with an open case does not fix its member      $openFamily
    Stuck projections
      type evidence about the scrutinee refuses                     $evidence
    Compiler facts                                                  $facts
  """

  private def entries(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Scopes.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def refusal(reason: String): Count = Count.Unresolved(List(reason))

  // Any file may extend Open and pick X (a second file `class Elsewhere extends Open { type X =
  // Boolean }` compiles beside it): X is not fixed by S's cases. Only the open case's reason is true.
  def openFamily =
    entries("""
      sealed trait S:
        type X
      trait Open extends S
      def h(p: S, x: p.X, y: p.X): p.X = ???
    """)("h") === refusal("sealed subtype outside the closed fragment: Open")

  // `e.substituteCo[Fst](x)` is a Fst[U] (`facts`), which the atom proj(T) hides from `=:=`: 1, not
  // 0 — refused.
  def evidence =
    entries("""
      type Fst[T] = T match { case (f, s) => f }
      def ev[T, U](x: Fst[T], e: T =:= U): Fst[U] = ???
    """)("ev") === refusal("stuck match types on T meet type evidence that mentions it")

  def facts =
    (typeChecks(
      "def f(p: cardinality.analysis.resolution.memberfixture.TY { type X = this.Y }, x: p.X): " +
        "p.Y = x"
    ) must beTrue)
      .and(
        typeChecks(
          "abstract class G extends cardinality.analysis.resolution.memberfixture.WithVal { " +
            "val p: cardinality.analysis.resolution.memberfixture.U { type Y = Boolean }; " +
            "def f(x: X): Boolean = x }"
        ) must beTrue
      )
      .and(
        typeChecks(
          "def e[T, U](x: cardinality.analysis.resolution.memberfixture.Fst[T], c: T =:= U): " +
            "cardinality.analysis.resolution.memberfixture.Fst[U] = " +
            "c.substituteCo[cardinality.analysis.resolution.memberfixture.Fst](x)"
        ) must beTrue
      )
      .and(
        typeChecks(
          "val a = new cardinality.analysis.resolution.memberfixture.FinalAbstract {}"
        ) must beFalse
      )
      .and(
        typeChecks(
          "class Sub extends cardinality.analysis.resolution.memberfixture.BelowClosed { type X = Int }"
        ) must beFalse
      )
      .and(
        typeChecks(
          "class S extends cardinality.analysis.resolution.memberfixture.Hidden.P"
        ) must beFalse
      )
      .and(
        typeChecks(
          "val b = new cardinality.analysis.resolution.memberfixture.Bounded {}"
        ) must beFalse
      )

}
