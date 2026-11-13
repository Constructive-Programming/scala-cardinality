package cardinality.bench.algebra

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*

/** Use case 1: decide the model's order on a corpus pair. Neither side is solved to a number; the
  * backend answers <, = or > however it can.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 300, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 300, timeUnit = TimeUnit.MILLISECONDS)
@Fork(value = 2, jvmArgsAppend = Array("-Xms256m", "-Xmx768m"))
class CompareBench {

  @Param(Array("symja", "rings", "model", "current"))
  var backend: String = ""

  @Param(
    Array(
      "eitherBelowPair",
      "pairBelowSumOfPowers",
      "sumBelowCube",
      "eitherPairBelowOptionPair",
      "optionBelowBooleanOption",
      "mixedAboveOptionPair",
      "squaredEqualsExpansion"
    )
  )
  var pair: String = ""

  private var run: () => Any = () => ()

  @Setup(Level.Trial)
  def setup(): Unit = {
    val algebra = Algebra.named(backend)
    val left = Cases.corpusLeft(algebra)(pair)
    val right = Cases.corpusRight(algebra)(pair)
    run = () => algebra.compare(left, right)
  }

  @Benchmark
  def measure(): Any = run()

}
