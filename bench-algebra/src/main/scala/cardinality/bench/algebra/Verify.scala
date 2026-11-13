package cardinality.bench.algebra

import cardinality.analysis.inhabitation.Natural
import cardinality.capacity.*
import cardinality.reporting.Report
import java.nio.file.{Files, Path}
import org.matheclipse.core.interfaces.IExpr
import org.matheclipse.core.expression.F

/** Correctness of the two use cases over the twelve-case corpus. Every expectation is derived
  * independently of the backends: polynomial projections and comparison signs by hand from the
  * shapes, the beyond-polynomial model values from the tier rules (a huge finite power of ω is one
  * ωⁿ unit; a polynomial or infinite exponent reaches ε₀). No backend is asked to solve an
  * expression to a number.
  */
object Verify {

  // The eight polynomial cases, projected by hand:
  //   w+w -> 2w;  w+1;  w·w;  (1+w)^2 = w^2+2w+1;  (1+w)^3 = w^3+3w^2+3w+1 (P sums 1+3);
  //   2(1+w) = 2w+2;  w^2+w;  (1+w)(2+w) = w^2+3w+2.
  private def expectedProjection(name: String): String = name match {
    case "eitherStrings"    => "0 P + 2 w + 0"
    case "optionString"     => "0 P + 1 w + 1"
    case "pairStrings"      => "1 P + 0 w + 0"
    case "pairOptions"      => "1 P + 2 w + 1"
    case "tripleOptions"    => "4 P + 3 w + 1"
    case "booleanOption"    => "0 P + 2 w + 2"
    case "eitherStringPair" => "1 P + 1 w + 0"
    case "optionEither"     => "1 P + 3 w + 2"
    case other              => throw new IllegalArgumentException(other)
  }

  // The model's own projection for the beyond-polynomial cases: w^(2^32) is a single
  // degree-2^32 term, one ωⁿ unit; w^(1+w), w^(w^2+w) and w^w reach ε₀.
  private def modelProjection(name: String): String = name match {
    case "intToString"                                      => "1 P + 0 w + 0"
    case "optionDomain" | "eitherDomain" | "stringToString" => "ε₀"
    case other => throw new IllegalArgumentException(other)
  }

  // Comparison signs by hand: 2w < w^2; w^2 < w^2+2w+1; degree 2 below degree 3;
  // w^2+w < w^2+2w+1 (coefficients at w); w+1 < 2w+2; w^2+3w+2 > w^2+2w+1; and
  // (1+w)^2 equals its expansion.
  private def expectedSign(pair: String): Int = pair match {
    case "eitherBelowPair" | "pairBelowSumOfPowers" | "sumBelowCube" | "eitherPairBelowOptionPair" |
        "optionBelowBooleanOption" =>
      -1
    case "squaredEqualsExpansion" => 0
    case "mixedAboveOptionPair"   => 1
    case other                    => throw new IllegalArgumentException(other)
  }

  /** A backend may refuse a case it cannot decide, but it must not answer wrong. */
  private def beyondOutcome(a: Algebra, name: String, backend: String): String =
    try {
      val expr = Cases.corpusExpr(a)(name)
      try {
        val got = a.project(expr)
        if (got == modelProjection(name)) s"$backend $name: projects to $got"
        else s"$backend $name: SILENT WRONG ANSWER $got, the model says ${modelProjection(name)}"
      } catch { case _: Exception => s"$backend $name: projection refused" }
    } catch { case _: Exception => s"$backend $name: construction refused" }

  def main(args: Array[String]): Unit = {
    val out = Path.of("bench-algebra/results")
    val _ = Files.createDirectories(out)
    val lines = Vector.newBuilder[String]
    val silentWrong = Vector.newBuilder[String]
    for (name <- List("symja", "rings", "model")) {
      val a = Algebra.named(name)
      for (caseName <- Cases.polynomial) {
        val got = a.project(Cases.corpusExpr(a)(caseName))
        require(
          got == expectedProjection(caseName),
          s"$name $caseName: got $got, want ${expectedProjection(caseName)}"
        )
      }
      for (pair <- Cases.corpusPairs) {
        val got = a.compare(Cases.corpusLeft(a)(pair), Cases.corpusRight(a)(pair))
        require(got == expectedSign(pair), s"$name $pair: got $got, want ${expectedSign(pair)}")
      }
      for (caseName <- Cases.beyond) {
        val outcome = beyondOutcome(a, caseName, name)
        lines += outcome
        if (outcome.contains("SILENT WRONG")) silentWrong += outcome
      }
      val p = Cases.corpusExpr(a)("pairOptions")
      val before = a.render(p)
      val _ = a.project(p)
      val _ =
        a.compare(Cases.corpusLeft(a)("eitherBelowPair"), Cases.corpusRight(a)("eitherBelowPair"))
      val _ = a.multiply(a.add(p, a.omega), a.number(2))
      require(a.render(p) == before, s"$name mutated its operand")
      lines +=
        s"$name: 8 polynomial projections and ${Cases.corpusPairs.size} comparison signs match; operands unchanged"
    }
    val wrong = silentWrong.result()
    lines += (if (wrong.isEmpty) "no silent wrong answers among the beyond-polynomial cases"
              else s"silent wrong answers (${wrong.size}):\n${wrong.mkString("\n")}")
    // The model's own comparison signs on the beyond-polynomial cases, derived from the
    // tier rules: ε₀ outranks the ωⁿ tier, +1 stays above ε₀, and the tier collapse
    // identifies w^(2^32) with w^2 — the convention that does not distinguish powers.
    val model = Algebra.named("model")
    val squared = Cases.corpusExpr(model)("pairStrings")
    require(
      model.compare(Cases.corpusExpr(model)("intToString"), squared) == 0,
      "the ωⁿ tier identifies w^(2^32) with w^2 by design"
    )
    for (name <- List("optionDomain", "eitherDomain", "stringToString"))
      require(
        model.compare(Cases.corpusExpr(model)(name), squared) > 0,
        s"$name should outrank w^2"
      )
    for (name <- Cases.beyond) {
      val expr = Cases.corpusExpr(model)(name)
      require(
        model.compare(expr, model.add(expr, model.number(1))) < 0,
        s"$name should stay below itself + 1"
      )
    }
    lines += "model beyond signs: intToString = pairStrings (the tier collapse); every beyond case outranks w^2 or equals it by tier, and stays below itself + 1"
    val current = new CurrentAlgebra
    require(current.multiply(current.omega, current.omega) == EffectiveOmega)
    require(current.project(Cases.corpusExpr(current)("pairStrings")) == "ω")
    require(
      current.project(Cases.corpusExpr(current)("intToString")) == "ω",
      "the huge finite power is where current lacks the ωⁿ tier"
    )
    for (caseName <- List("optionDomain", "eitherDomain", "stringToString"))
      require(current.project(Cases.corpusExpr(current)(caseName)) == "ε₀", caseName)
    lines += s"current corpus projections: ${Cases.corpus
        .map(name => s"$name=${current.project(Cases.corpusExpr(current)(name))}")
        .mkString(", ")}"
    lines += s"current corpus signs: ${Cases.corpusPairs
        .map(pair =>
          s"$pair=${current.compare(Cases.corpusLeft(current)(pair), Cases.corpusRight(current)(pair))}"
        )
        .mkString(", ")}; exact wants - - - - - + 0"
    val enormous = Natural(2) ^ Natural(65537)
    lines += s"Natural 2^65537: ${enormous.render}; nodes=${enormous.nodes}"
    val report = lines.result().mkString("\n") + "\n"
    val _ = Files.writeString(out.resolve("correctness.txt"), report)
    println(report)
    if (args.contains("self")) {
      val base = Path.of("bench-algebra/src/main/scala/cardinality/bench/algebra")
      val paths = List(
        "Algebra.scala",
        "SymjaAlgebra.scala",
        "RingsAlgebra.scala",
        "ModelAlgebra.scala",
        "CurrentAlgebra.scala"
      ).map(base.resolve)
      val measured = Report.of(paths)
      val _ = Files.writeString(out.resolve("cardinality-sources-only.txt"), measured.render)
      require(measured.errors.isEmpty, measured.errors.toString)
      println(
        "Wrote sources-only cardinality report; unresolved external declarations are reported honestly."
      )
    }
  }

}

/** Run in a separately bounded process: the huge, symbolic and polynomial-exponent cases cannot be
  * expanded, and a probe that hangs or dies is itself a result. The default mode probes the
  * corpus's beyond-polynomial cases — the only backend that can hold them is Symja's expression
  * trees.
  */
object Stress {

  def main(args: Array[String]): Unit = {
    val a = new SymjaAlgebra
    if (args.contains("boundary")) boundaryLadder(a)
    else if (args.contains("huge")) binomialExponent(a, F.ZZ(4294967296L))
    else if (args.contains("symbolic")) binomialExponent(a, F.symbol("n"))
    else
      for (name <- Cases.beyond) {
        val p = Cases.corpusExpr(a)(name)
        val smaller = a.multiply(a.omega, a.omega) // w^2, the pairStrings case
        println(s"case=$name expression=$p")
        println(s"leaves=${a.evaluator.eval(F.LeafCount(p))}")
        probe(s"project($name)")(a.project(p))
        probe(s"compare($name, w^2), expected +1")(a.compare(p, smaller).toString)
        probe(s"compare($name, $name + 1), expected -1")(
          a.compare(p, a.add(p, a.number(1))).toString
        )
        System.out.flush()
      }
    println(s"default zero-power-zero=${a.evaluator.eval(F.Power(F.C0, F.C0))}")
    println(s"formal infinite power=${a.evaluator.eval(F.Power(a.omega, a.omega))}")
  }

  private def binomialExponent(a: SymjaAlgebra, exponent: IExpr): Unit = {
    val base = a.add(a.number(1), a.omega)
    val p = a.evaluator.eval(F.Power(base, exponent))
    val plusW = a.add(p, a.omega)
    val smaller = a.evaluator.eval(F.Power(base, F.Plus(exponent, F.ZZ(-1))))
    println(s"expression=$p")
    println(s"leaves=${a.evaluator.eval(F.LeafCount(p))}")
    probe("compare(p, p+w), expected -1")(a.compare(p, plusW).toString)
    probe("compare(p, (1+w)^(e-1)), expected +1")(a.compare(p, smaller).toString)
    probe("project(p)")(a.project(p))
    System.out.flush()
  }

  private def boundaryLadder(a: SymjaAlgebra): Unit =
    for (exponent <- List(F.ZZ(1 << 20), F.ZZ(Int.MaxValue.toLong), F.ZZ(Int.MaxValue.toLong + 1)))
      binomialExponent(a, exponent)

  private def probe(label: String)(body: => String): Unit =
    try println(s"$label=${body}")
    catch {
      case e: Exception => println(s"$label refused: ${e.getMessage}")
    }

}
