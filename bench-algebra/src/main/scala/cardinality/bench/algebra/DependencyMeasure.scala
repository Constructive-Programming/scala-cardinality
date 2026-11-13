package cardinality.bench.algebra

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.reporting.Report
import cardinality.request.AnalysisQuery
import cardinality.tasty.TastyStubs
import java.io.File
import java.nio.file.{Files, Path}

/** Bounded self-measurement with real compiled signatures, rather than invented API stubs. */
object DependencyMeasure {

  def main(args: Array[String]): Unit = {
    val base = Path.of("bench-algebra/src/main/scala/cardinality/bench/algebra")
    val targets = List(
      "SymjaAlgebra.scala",
      "RingsAlgebra.scala",
      "ModelAlgebra.scala",
      "CurrentAlgebra.scala"
    )
      .map(base.resolve)
    val support = List(
      base.resolve("Algebra.scala"),
      Path.of("core/src/main/scala/cardinality/capacity/Size.scala")
    )
    val inputs = (targets ++ support).map { path =>
      MethodAnalysis.Input(path.toString, dialects.Scala3(Files.readString(path)).parse[Source].get)
    }
    val classpath =
      System.getProperty("java.class.path").split(File.pathSeparator).toList.map(Path.of(_))
    val dependencies = TastyStubs.of(
      classpath,
      p => List("matheclipse-core-", "rings-").exists(p.getFileName.toString.startsWith),
      p =>
        p.startsWith("org.matheclipse.core.interfaces") || p == "org.matheclipse.core.expression" ||
          p == "org.matheclipse.core.eval" || p.startsWith("cc.redberry.rings"),
      inputs.map(_.tree.syntax)
    )
    val request = AnalysisQuery.Request(
      inputs ++ dependencies.inputs,
      targets.map(_.toString).toSet,
      budget = AnalysisQuery.Budget(maxIndexWork = 10000000, maxWorkPerTarget = 10000)
    )
    val result = AnalysisQuery.run(request)
    val sources = targets.map { path =>
      Report.Source(path.toString, Nil, result.entries.filter(_.path == path.toString))
    }
    val report =
      Report(sources, result.errors.map(Report.Error("", _)), warnings = dependencies.errors)
    val out = Path.of("bench-algebra/results/cardinality-with-dependencies.txt")
    val _ = Files.writeString(out, report.render)
    println(
      s"inputs=${inputs.size}, dependency stubs=${dependencies.inputs.size}, errors=${result.errors}, args=${args.toList}"
    )
    println(report.render.take(1500))
  }

}
