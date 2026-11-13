package cardinality.bench.algebra

import cardinality.analysis.inhabitation.Natural
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*

/** Exact finite arithmetic: the coefficient arithmetic a projection needs (the high-tier
  * coefficient of (1+w)^N is 2^N-1-N). Natural is the existing non-distributing carrier.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 300, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 300, timeUnit = TimeUnit.MILLISECONDS)
@Fork(value = 2, jvmArgsAppend = Array("-Xms256m", "-Xmx768m"))
class FiniteBench {

  @Param(Array("symja", "rings", "model", "current", "natural"))
  var backend: String = ""

  @Param(Array("64", "4096"))
  var n: Int = 0

  private var run: () => Any = () => ()

  @Setup(Level.Trial)
  def setup(): Unit = {
    if (backend == "natural") {
      val two = Natural(2)
      val exponent = Natural(n)
      run = () => two ^ exponent
    } else {
      val a = Algebra.named(backend)
      val two = a.number(2)
      val exponent = a.number(n)
      run = () => a.power(two, exponent)
    }
  }

  @Benchmark
  def finitePower(): Any = run()

}
