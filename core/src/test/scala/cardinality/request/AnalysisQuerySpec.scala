package cardinality.request

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

class AnalysisQuerySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Selected source queries
      retain support declarations without reporting their targets       $support
      select all overloads of an explicit target name                    $overloads
      preserve the legacy counts                                         $equivalence
      reject unknown selectors and duplicate source origins              $invalid
    Deterministic budgets
      stop indexing without claiming a partial environment               $indexLimit
      bound selection before measuring                                  $targetLimit
      allocate fuel before measurement without transferring leftovers    $quotas
      expose a frontier and consumed work on exhaustion                  $frontier
      bound recursive projection resolution                             $projection
      bound flat forward capture-alias chains                            $aliasDepth
      charge solver work to each target quota                            $solver
      stop exponential product-shape expansion before solver entry        $expansion
    Snapshot keys and indexed lookup
      ignore input order but include support edits and limits            $keys
      do not visit unrelated type declarations during lookup             $lookup
  """

  private def input(path: String, code: String): MethodAnalysis.Input =
    MethodAnalysis.Input(path, dialects.Scala3(code).parse[Source].get)

  private val main = input(
    "Main.scala",
    """
    package p
    def get[A](a: A): A = a
    def other[A](a: A, b: A): A = a
  """
  )

  private def request(budget: AnalysisQuery.Budget = AnalysisQuery.Budget()) =
    AnalysisQuery.Request(List(main), Set(main.path), budget = budget)

  def support = {
    val target = input(
      "Target.scala",
      """
      package p
      def get[A](a: Box[A]): A = a.value
    """
    )
    val context = input(
      "Support.scala",
      """
      package p
      case class Box[A](value: A)
      def unrelated[A](a: A): A = a
    """
    )
    val found = AnalysisQuery.run(AnalysisQuery.Request(List(target, context), Set(target.path)))
    (found.errors === Nil)
      .and(found.entries.map(_.name) === List("p.get"))
      .and(found.entries.head.count === Count.Finite(1))
      .and(found.usage.size === 1)
  }

  def overloads = {
    val source = input(
      "Overloads.scala",
      """
      def get[A](a: A): A = a
      def get[A](a: A, b: A): A = a
      def other[A](a: A): A = a
    """
    )
    val found = AnalysisQuery.run(AnalysisQuery.Request(List(source), Set(source.path), Set("get")))
    found.entries.map(_.count).toSet === Set(Count.Finite(1), Count.Finite(2))
  }

  def equivalence =
    AnalysisQuery.run(request()).entries.toSet === MethodAnalysis.analyze(List(main)).toSet

  def invalid = {
    val missingPath = AnalysisQuery.run(request().copy(targetPaths = Set("missing")))
    val missingName = AnalysisQuery.run(request().copy(targetNames = Set("missing")))
    val duplicate = AnalysisQuery.run(request().copy(inputs = List(main, main)))
    List(missingPath, missingName, duplicate).forall(r =>
      r.errors.nonEmpty && r.entries.isEmpty
    ) must beTrue
  }

  def indexLimit = {
    val found = AnalysisQuery.run(request(AnalysisQuery.Budget(maxIndexWork = 1)))
    (found.entries === Nil)
      .and(found.errors.exists(_.contains("index budget exhausted")) must beTrue)
      .and(found.key === "")
  }

  def targetLimit = {
    val found = AnalysisQuery.run(request(AnalysisQuery.Budget(maxTargets = 1)))
    (found.entries === Nil).and(found.errors === List("selected target limit exceeded"))
  }

  def quotas = {
    val both = AnalysisQuery.run(request(AnalysisQuery.Budget(maxRequestWork = 100)))
    val one = AnalysisQuery.run(
      request(AnalysisQuery.Budget(maxRequestWork = 100)).copy(
        targetNames = Set("p.get")
      )
    )
    (both.usage.map(_.quota) === List(50L, 50L))
      .and(one.usage.map(_.quota) === List(100L))
      .and(both.usage.map(_.consumed).sum must beLessThanOrEqualTo(100L))
      .and(both.key must not(beEqualTo(one.key)))
  }

  def frontier = {
    val found = AnalysisQuery.run(request(AnalysisQuery.Budget(maxRequestWork = 1)))
    (found.errors === Nil)
      .and(found.usage.map(_.quota) === List(0L, 0L))
      .and(found.entries.forall(_.count match {
        case Count.Unresolved(reasons) => reasons.exists(_.contains("budget exhausted at"))
        case _                         => false
      }) must beTrue)
  }

  def projection = {
    val source = input(
      "Projection.scala",
      """
      object Library { type Alias = Unit }
      trait Base { def get[`Library.Alias`](a: Library.Alias): `Library.Alias` }
      abstract class Instance extends Base { def get[A](a: A): A = a }
    """
    )
    val found = AnalysisQuery.run(
      AnalysisQuery.Request(
        List(source),
        Set(source.path),
        Set("Base.get"),
        MethodAnalysis.Limits(maxTypeDepth = 8)
      )
    )
    found.entries.head.count must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains("budget exhausted")) must beTrue
    }
  }

  def keys = {
    val context = input("Support.scala", "package q; type Alias = Boolean")
    val base = request().copy(inputs = List(main, context))
    val changed = input(context.path, "package q; type Alias = Unit")
    val first = AnalysisQuery.run(base)
    (first.key === AnalysisQuery.run(base.copy(inputs = base.inputs.reverse)).key)
      .and(
        first.key must not(
          beEqualTo(AnalysisQuery.run(base.copy(inputs = List(main, changed))).key)
        )
      )
      .and(
        first.key must not(
          beEqualTo(
            AnalysisQuery
              .run(
                base.copy(
                  limits = MethodAnalysis.Limits(maxStates = 257)
                )
              )
              .key
          )
        )
      )
  }

  def aliasDepth = {
    val aliases = (1 to 300).map(i => f"val a$i%04d: A = a${i + 1}%04d").mkString("\n")
    val source = input(
      "Aliases.scala",
      s"""
      class Container[A](arg: A) {
        $aliases
        val a0301: A = arg
        def get: A = arg
      }
    """
    )
    val found = AnalysisQuery.run(
      AnalysisQuery.Request(
        List(source),
        Set(source.path),
        Set("Container.get"),
        MethodAnalysis.Limits(maxTypeDepth = 16)
      )
    )
    found.entries.head.count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("capture alias")) must beTrue
    }
  }

  def solver = {
    val full = AnalysisQuery.run(request().copy(targetNames = Set("p.get")))
    val solverWork = full.counters.toList.filter(_._1.startsWith("solver")).map(_._2).sum
    val beforeSolver = full.usage.head.consumed - solverWork
    val limited = AnalysisQuery.run(
      request(
        AnalysisQuery.Budget(
          maxWorkPerTarget = beforeSolver,
          maxRequestWork = beforeSolver
        )
      ).copy(targetNames = Set("p.get"))
    )
    (solverWork must beGreaterThan(0L))
      .and(limited.usage.head.consumed === beforeSolver)
      .and(limited.entries.head.count must beLike {
        case Count.Unresolved(reasons) =>
          reasons.exists(_.contains("solver")) must beTrue
      })
  }

  def lookup = {
    val queried = input(
      "Query.scala",
      "package p; case class Box[A](value: A); def get[A](a: Box[A]): A = a.value"
    )
    val unrelated = input(
      "Types.scala",
      "package q\n" + (1 to 2000).map(i => s"type T$i = Boolean").mkString("\n")
    )
    val small =
      AnalysisQuery.run(AnalysisQuery.Request(List(queried), Set(queried.path), Set("p.get")))
    val large = AnalysisQuery.run(
      AnalysisQuery.Request(List(queried, unrelated), Set(queried.path), Set("p.get"))
    )
    (large.entries === small.entries)
      .and(large.counters.get("lookup bucket") === small.counters.get("lookup bucket"))
      .and(large.counters.get("lookup candidate") === small.counters.get("lookup candidate"))
  }

  def expansion = {
    val expanded = (1 to 20).foldLeft("A")((tpe, _) => s"Dup[$tpe]")
    val source = input(
      "Expansion.scala",
      s"""
      case class Dup[A](left: A, right: A)
      def get[A](a: $expanded): Unit = ()
    """
    )
    val found = AnalysisQuery.run(
      AnalysisQuery.Request(
        List(source),
        Set(source.path),
        Set("get"),
        budget = AnalysisQuery.Budget(maxWorkPerTarget = 500, maxRequestWork = 500)
      )
    )
    (found.usage.head.consumed === 500L).and(found.entries.head.count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("shape validation")) must beTrue
    })
  }

}
