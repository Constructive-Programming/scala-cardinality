package cardinality.bench

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import scala.meta.*
import scala.sys.process.Process

import org.openjdk.jmh.annotations.*

import cardinality.analysis.{Measuring, MethodAnalysis}
import cardinality.capacity.{Counter, Library}
import cardinality.tasty.TastyStubs

/** The analyzer over a real checkout, the inputs the eo runner reads (bench/README.md):
  * `CARDINALITY_BENCH_CHECKOUT` names the checkout, `CARDINALITY_BENCH_CLASSPATH` a file listing
  * its dependency jars. Each operation builds a fresh index, so its lazy caches are paid for.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 2)
@Measurement(iterations = 5)
@Fork(value = 1, jvmArgsAppend = Array("-Xmx3g", "-Xss8m"))
class AnalysisBench {

  private var inputs: List[MethodAnalysis.Input] = Nil
  private var stubs: List[MethodAnalysis.Input] = Nil
  private var sampled: Set[String] = Set.empty

  @Setup(Level.Trial)
  def setup(): Unit = {
    val checkout = Path.of(sys.env("CARDINALITY_BENCH_CHECKOUT"))
    val listing = Process("git", List("-C", checkout.toString, "ls-files", "-z", "*.scala")).!!
    inputs = listing
      .split('\u0000')
      .filter(p => p.contains("/src/main/") && !List("benchmarks/", "tests/", "demos/").exists(p.startsWith))
      .toList
      .sorted
      .map(p =>
        MethodAnalysis.Input(p, dialects.Scala3(Files.readString(checkout.resolve(p), UTF_8)).parse[Source].get)
      )
    val classpath = Files.readAllLines(Path.of(sys.env("CARDINALITY_BENCH_CLASSPATH")), UTF_8)
      .toArray(Array.empty[String]).toList.map(_.trim).filter(_.nonEmpty).map(Path.of(_))
    stubs = TastyStubs.of(classpath, _ => true).inputs
    // Every fourth source: a fixed sample of targets for the measurement with stubs.
    sampled = inputs.map(_.path).zipWithIndex.collect { case (p, i) if i % 4 == 0 => p }.toSet
  }

  /** Every method row of the sources alone (the sources-only runner's forward pass). */
  @Benchmark
  def sourcesOnly(): Int = MethodAnalysis.analyze(inputs).size

  /** Every stored-value row of the sources alone. */
  @Benchmark
  def definitions(): Int = {
    val library = Library.of(inputs.map(_.tree))
    inputs.map(i => Counter.definitions(i.tree, library).size).sum
  }

  /** Rendering the dependency jars' TASTy as stubs. */
  @Benchmark
  def renderStubs(): Int = {
    val classpath = Files.readAllLines(Path.of(sys.env("CARDINALITY_BENCH_CLASSPATH")), UTF_8)
      .toArray(Array.empty[String]).toList.map(_.trim).filter(_.nonEmpty).map(Path.of(_))
    TastyStubs.of(classpath, _ => true).inputs.size
  }

  /** The sampled targets' rows with the stubs in the environment, under the runner's budget: one
    * worker, the sequential measurement.
    */
  @Benchmark
  def withStubs(): Int = Measuring.rows(inputs, stubs, workers = 1, only = sampled).size

  /** The same with the default workers in parallel, each with an index of its own. */
  @Benchmark
  def withStubsParallel(): Int = Measuring.rows(inputs, stubs, only = sampled).size

  /** Every method row of the sources alone, with the default workers in parallel. */
  @Benchmark
  def sourcesOnlyParallel(): Int = Measuring.rows(inputs).size
}
