package cardinality.bench.algebra

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*

/** Construction of the expressions the counting rules build, plus native rendering. Preparation is
  * outside timing; inputs are never mutated.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 300, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 300, timeUnit = TimeUnit.MILLISECONDS)
@Fork(value = 2, jvmArgsAppend = Array("-Xms256m", "-Xmx768m"))
class ConstructBench {

  @Param(Array("symja", "rings", "model", "current"))
  var backend: String = ""

  @Param(Array("8", "64"))
  var n: Int = 0

  @Param(Array("binomial", "product", "sum", "render"))
  var operation: String = ""

  private var run: () => Any = () => ()

  @Setup(Level.Trial)
  def setup(): Unit = {
    val algebra = Algebra.named(backend)
    val input = algebra.binomial(n)
    run = operation match {
      case "binomial" => () => algebra.binomial(n)
      case "product"  => () => algebra.product(n)
      case "sum"      => () => algebra.sum(n)
      case "render"   => () => algebra.render(input)
      case other      => throw new IllegalArgumentException(other)
    }
  }

  @Benchmark
  def measure(): Any = run()

}
