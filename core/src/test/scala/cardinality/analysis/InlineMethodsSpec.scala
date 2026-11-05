package cardinality.analysis

import org.specs2.Specification

/** An inline method counts as a method when its body uses no compile-time operation
  * (docs/research/syntax-features.md, register D27). Every fixture is valid Scala 3.9.0 except
  * where a name outside the sources is the point (fixture a6 `r`, run: the printed values are the
  * ones the comments quote); each expected number is derived beside it.
  */
class InlineMethodsSpec extends Specification with InventoryFixtures {

  def is = s2"""
    Compile-time operations refuse, wherever they hide
      a passing-looking method calling a failing one                 $transitive
      an abstract inline declaration and its implementation          $abstractInline
  """

  // Fixture a6 r: wrapper[Int](1) is 1, and 7 under a call-site `given Int = 7`.
  def transitive = {
    val found = rows("""import scala.compiletime.summonFrom
      |object Neg {
      |  inline def leaf[A](x: A): A = summonFrom { case g: A => g; case _ => x }
      |  inline def wrapper[A](x: A): A = leaf(x)
      |}""".stripMargin)
    refused(
      found("Neg.wrapper"),
      "calls inline method leaf, which uses scala.compiletime.summonFrom"
    )
  }

  // Fixture a6 r compiles both; Impl.abs prints 3.
  def abstractInline = {
    val found = rows("""trait Abs { inline def abs: Int }
      |object Impl extends Abs { inline def abs: Int = 3 }
      |""".stripMargin)
    refused(found("Abs.abs"), "is an abstract inline declaration, with no body to check")
      .and(refused(found("Impl.abs"), "implements the abstract inline declaration Abs.abs"))
  }

}
