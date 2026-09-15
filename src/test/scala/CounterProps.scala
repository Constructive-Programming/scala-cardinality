import scala.meta.*
import scala.meta.dialects.Scala3

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.specs2.scalacheck.Parameters
import org.specs2.{ScalaCheck, Specification}

/** Property-based tests that drive the counter from generated source strings.
  *
  * Every generated string carries what the counter must report for it, computed by `CodeGen`
  * without running the counter, so these properties exercise the whole `parse -> counter` path
  * rather than reusing the code under test to build the expected value.
  */
class CounterProps extends Specification with ScalaCheck {

  // Two properties run 256 cases each, so the suite reports 512 expectations.
  implicit override def defaultParameters: Parameters = Parameters().copy(minTestsOk = 256)

  def is = s2"""
    A single class is counted exactly            $exactProduct
    A whole program counts as its parts          $programSize
  """

  /** One class, so the count is a product with no rounding from an enclosing sum: the reported size
    * is exactly the canonical form of the product's true cardinality.
    */
  def exactProduct: Prop = forAll(CodeGen.genClassOnly) { snippet =>
    val reported = count(snippet.source)
    val expected = CodeGen.canonical(snippet.exactCount)
    (reported == expected) :| s"$snippet -> $reported, expected $expected"
  }

  /** Any mix of statements. The expected size is built directly from the parts, so this pins the
    * traversal (packages, classes, definitions) and the upward rounding of the sums.
    */
  def programSize: Prop = forAll(CodeGen.genProgram) { snippet =>
    val reported = count(snippet.source)
    (reported == snippet.expected) :| s"$snippet -> $reported, expected ${snippet.expected}"
  }

  private def count(source: String): Size = Counter.source(parse(source))

  /** The generator emits Scala 3 sources with top-level `val`/`def`/`object`, which the Scala 2
    * default dialect would reject, so parse with the matching dialect.
    */
  private def parse(source: String): Source =
    given Dialect = Scala3
    source.parse[Source].get

}
