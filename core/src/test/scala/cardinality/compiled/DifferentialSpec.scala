package cardinality.compiled

import cardinality.analysis.MethodAnalysis.Entry
import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.reporting.{FrontEnd, Report}
import org.specs2.Specification

import Differential.Verdict

/** The differential gate's own rules, on rows made by hand: how two answers are paired and
  * classified, and which rows fail a run (docs/plans/compiled-own-types.md, "The gate").
  */
class DifferentialSpec extends Specification {

  def is = s2"""
  The differential gate
    classifies identical, better, worse and uncovered rows        $verdicts
    leaves a row only one side has unpaired                       $unpaired
    pairs a row by position where its scope is named otherwise    $position
    does not pair a lost row with an extra one of another owner   $owners
    fails a better row past a model limit until it is derived     $limits
    fails a row measured before, uncovered or counted otherwise   $memory
    passes a bound that became the exact number of its value      $tightened
    passes a changed number only as a listed correction           $corrected
  """

  private def entry(name: String, count: Count, line: Int = 1): Entry =
    Entry("A.scala", name, name, line, "method", count, Nil)

  private def answer(entries: Entry*): FrontEnd.Rows =
    FrontEnd.Rows(List(Right(Report.Source("A.scala", Nil, entries.toList))), Nil)

  private val unknown = Count.Unresolved(List("unresolved type: T"))
  private val declined = Count.Unresolved(List(s"${TastyFrontEnd.Uncovered}: a class"))

  private def verdicts = {
    val old = answer(
      entry("same", Count.Finite(2)),
      entry("gained", unknown),
      entry("lost", Count.Finite(2)),
      entry("moved", Count.Finite(2)),
      entry("declined", Count.Finite(2)),
      entry("unknown", unknown)
    )
    val next = answer(
      entry("same", Count.Finite(2)),
      entry("gained", Count.Finite(2)),
      entry("lost", Count.Unresolved(List("a reason of its own"))),
      entry("moved", Count.Finite(3)),
      entry("declined", declined),
      entry("unknown", Count.Unresolved(List("another reason")))
    )
    Differential.compare(old, next).map(c => c.row.name -> c.verdict).toMap must_== Map(
      "same" -> Verdict.Identical,
      "gained" -> Verdict.Better,
      "lost" -> Verdict.Worse,
      "moved" -> Verdict.Worse,
      "declined" -> Verdict.Uncovered,
      "unknown" -> Verdict.Identical
    )
  }

  // An upper bound that becomes the exact number of the same value is tighter, not worse; one
  // that becomes another number, or a number that becomes a bound, is worse.
  private def tightened = {
    val bound = Count.Bounded(cardinality.capacity.FiniteSize(32))
    val exact = Count.Finite(BigInt(2).pow(32))
    val compared = Differential.compare(
      answer(entry("tight", bound), entry("other", bound, 2), entry("loose", exact, 3)),
      answer(entry("tight", exact), entry("other", Count.Finite(2), 2), entry("loose", bound, 3))
    )
    (compared.map(c => c.row.name -> c.verdict) must contain(
      exactly("tight" -> Verdict.Tightened, "other" -> Verdict.Worse, "loose" -> Verdict.Worse)
    )).and(Differential.failures(compared).map(_.row.name).toSet must_== Set("other", "loose"))
  }

  // A number changes only where the old one is derivably wrong, and then the row is listed with
  // both values: `fixed` is; `silent` is not listed; `stale` is listed with a new value it no
  // longer has and `misread` with an old value it never had. A listed row that is not worse
  // (`same`) is a dead line, reported; one of a source this run does not hold is not.
  private def corrected = {
    val compared = Differential.compare(
      answer(
        entry("fixed", Count.Finite(0)),
        entry("silent", Count.Finite(0), 2),
        entry("stale", Count.Finite(0), 3),
        entry("misread", Count.Finite(0), 4),
        entry("same", Count.Finite(2), 5)
      ),
      answer(
        entry("fixed", Count.Finite(8)),
        entry("silent", Count.Finite(8), 2),
        entry("stale", Count.Finite(8), 3),
        entry("misread", Count.Finite(8), 4),
        entry("same", Count.Finite(2), 5)
      )
    )
    def key(name: String) = compared.filter(_.row.name == name).head.row.key
    val listed = Differential.listed(
      List(
        s"${key("fixed")} | 0 -> 8: the cases are 8",
        s"${key("stale")} | 0 -> 4: once",
        s"${key("misread")} | 1 -> 8: never",
        s"${key("same")} | 0 -> 2: fixed since",
        "method B.scala:1 elsewhere [method] | 0 -> 1: another input"
      )
    )
    (Differential.failures(compared).size must_== 4)
      .and(
        Differential.failures(compared, corrected = listed).map(_.row.name).toSet must_==
          Set("silent", "stale", "misread")
      )
      .and(Differential.corrections(compared, listed).map(_.row.name) must_== List("fixed"))
      .and(
        Differential.unused(compared, listed).toSet must_==
          Set(key("stale"), key("misread"), key("same"))
      )
  }

  private def unpaired =
    Differential
      .compare(answer(entry("gone", Count.Finite(2))), answer(entry("made", Count.Finite(2), 7)))
      .map(c => c.row.name -> c.verdict) must contain(
      exactly("gone" -> Verdict.OnlyOld, "made" -> Verdict.OnlyNew)
    )

  private def position =
    Differential.compare(
      answer(entry("f.<for#1>.g", Count.Finite(2), 3)),
      answer(entry("f.<lambda#1>.<case#1>.g", Count.Finite(2), 3))
    ) must beLike {
      case List(row) => (row.verdict, row.by) must_== (Verdict.Identical, "position")
    }

  // `trait T(); trait U[A: Ordering]` on one line: a constructor row lost for `T` and one made for
  // `U` are two failures, not one pair.
  private def owners =
    Differential
      .compare(answer(entry("T.<init>", unknown)), answer(entry("U.<init>", declined)))
      .map(c => c.row.name -> c.verdict) must contain(
      exactly("T.<init>" -> Verdict.OnlyOld, "U.<init>" -> Verdict.OnlyNew)
    )

  // The old front end refused `named` for a name it could not resolve, which the compiler knows:
  // a number is expected. It refused `modelled` because the counting model does not read a
  // recursive type, and `transparent` by rule X: a number there counts what the model refuses,
  // until someone derives it, with that number.
  private def limits = {
    val recursive = Count.Unresolved(List("recursive type not read: T"))
    val ruleX =
      Count.Unresolved(List("inferred result type not resolved: transparent inline method"))
    val compared = Differential.compare(
      answer(
        entry("named", unknown),
        entry("modelled", recursive, 2),
        entry("transparent", ruleX, 3)
      ),
      answer(
        entry("named", Count.Finite(2)),
        entry("modelled", Count.Finite(2), 2),
        entry("transparent", Count.Finite(2), 3)
      )
    )
    def derived(count: String) =
      compared.filter(_.row.name != "named").map(_.row.key -> count).toMap
    (Differential.failures(compared).map(_.row.name) must_== List("modelled", "transparent"))
      .and(Differential.failures(compared, derived("2")) must_== Nil)
      .and(Differential.failures(compared, derived("3")).size must_== 2)
      .and(Differential.samples(compared).map(_._1) must_== List("unresolved type"))
  }

  // `lost` was measured and is uncovered now, `refused` was the front end's own `?` and is
  // uncovered now, `moved` is counted otherwise; `fresh` is uncovered and was never measured;
  // `kept` is as before. `twin` is two rows of one key, one of them uncovered: the key cannot tell
  // them apart, so it is not remembered. What is written is read back as it was.
  private def memory = {
    val refusal = Count.Unresolved(List("recursive type not read: T"))
    val compared = Differential.compare(
      answer(
        entry("kept", Count.Finite(2)),
        entry("lost", Count.Finite(2), 2),
        entry("moved", unknown, 3),
        entry("fresh", Count.Finite(2), 4),
        entry("refused", refusal, 5),
        entry("twin", Count.Finite(2), 6),
        entry("twin", Count.Finite(2), 6)
      ),
      answer(
        entry("kept", Count.Finite(2)),
        entry("lost", declined, 2),
        entry("moved", Count.Finite(3), 3),
        entry("fresh", declined, 4),
        entry("refused", declined, 5),
        entry("twin", Count.Finite(2), 6),
        entry("twin", declined, 6)
      )
    )
    def key(name: String) = compared.find(_.row.name == name).get.row.key
    val before =
      Map(key("kept") -> "2", key("lost") -> "2", key("moved") -> "2", key("refused") -> "?")
    val now = Differential.measured(compared)
    (Differential.regressed(compared, before).map(_.row.name) must contain(
      exactly("lost", "moved", "refused")
    )).and(now must contain(exactly(key("kept") -> "2", key("moved") -> "3")))
      .and(Differential.listed(now.map(Differential.line)) must_== now.toMap)
      .and(
        Differential.listed(List("method F.scala:3 | [method] | 1: its rule")) must_==
          Map("method F.scala:3 | [method]" -> "1")
      )
  }

}
