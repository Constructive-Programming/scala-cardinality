package cardinality.bench.algebra

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*

/** Use case 2: render the reporting projection b·ωⁿ + c·ω + d of a corpus expression. The
  * beyond-polynomial corpus cases (w in the exponent) are not benchmarked here: the materialized
  * carriers refuse to construct them, so they are classified in `Verify` and probed in `Stress`.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 300, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 300, timeUnit = TimeUnit.MILLISECONDS)
@Fork(value = 2, jvmArgsAppend = Array("-Xms256m", "-Xmx768m"))
class ProjectBench {

  @Param(Array("symja", "rings", "model", "current"))
  var backend: String = ""

  @Param(
    Array(
      "eitherStrings",
      "optionString",
      "pairStrings",
      "pairOptions",
      "tripleOptions",
      "booleanOption",
      "eitherStringPair",
      "optionEither"
    )
  )
  var expr: String = ""

  private var run: () => Any = () => ()

  @Setup(Level.Trial)
  def setup(): Unit = {
    val algebra = Algebra.named(backend)
    val input = Cases.corpusExpr(algebra)(expr)
    run = () => algebra.project(input)
  }

  @Benchmark
  def measure(): Any = run()

}
