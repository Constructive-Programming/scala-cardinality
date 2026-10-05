package cardinality

import scala.meta.*
import org.specs2.Specification

/** First-order beta reduction and the closed parametric identity fragment, not general rank-n. */
class PolyLambdaCardinalitySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Type lambda beta reduction
      applies a direct lambda to caller arguments                 $direct
      applies an alias lambda to a structural constructor         $alias
      does not capture a caller binder of the same spelling       $capture
      keeps nested lambda shadowing lexical                      $shadowing
      measures constructor fields through beta reduction          $constructor
      refuses an external constructor inside the lambda           $external
      refuses constrained lambda binders                         $bounds
      refuses unapplied lambdas and wrong arity                    $arity
      resolves an alias body in its declaration's scope            $aliasScope
    Closed polymorphic identity
      introduces the unique total parametric identity             $identity
      application adds no choices or opaque iteration             $application
      does not supply an arbitrary missing result                  $missing
      keeps poly binder shadowing separate from outer binders      $polyShadowing
      leaves other rank-polymorphic functions unresolved           $rankN
      retains nested context evidence diagnostics                 $evidence
      retains unknown external dependencies                       $dependency
      refuses incomplete environments even for identity results   $environment
  """

  private def count(code: String, name: String = "use"): Count =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Poly.scala", dialects.Scala3(code).parse[Source].get)))
      .find(_.name == name)
      .get
      .count

  private def unresolved(code: String, reason: String) =
    count(code) must beLike {
      case Count.Unresolved(reasons) => reasons.mkString(" ") must contain(reason)
    }

  def direct =
    count("def use[A](x: A, y: A): ([x] =>> (x, x))[A] = ???") === Count.Finite(4)

  def alias =
    count("type F = [x] =>> Option[x]; def use[A](x: A): F[A] = ???") === Count.Finite(2)

  def capture =
    count("def use[A, B](x: A, y: B): ([B] =>> B)[A] = ???") === Count.Finite(1)

  def shadowing =
    count("def use[A, B](x: A, y: B): ([x] =>> ([x] =>> x)[B])[A] = ???") ===
      Count.Finite(1)

  def constructor =
    count("case class C[A](x: ([x] =>> x)[A], y: A)", "C.<init>") === Count.Finite(4)

  def external =
    unresolved("def use[A](x: A): ([x] =>> ZIO[Unit, Unit, x])[A] = ???", "ZIO")

  def bounds =
    unresolved("def use[A](x: A): ([x <: A] =>> x)[A] = ???", "constrained type lambda")

  def arity =
    unresolved("def use: [x] =>> x = ???", "unapplied type lambda").and(
      unresolved("def use[A](x: A): ([x] =>> x)[A, A] = ???", "type lambda argument arity")
    )

  def aliasScope =
    count(
      "class Env[A](seed: A) { type F = [x] =>> (A, x); " +
        "def use[A](x: A): F[A] = ??? }",
      "Env.use"
    ) === Count.Finite(1)

  def identity =
    count("def use: [a] => a => a = ???") === Count.Finite(1)

  def application =
    count("def use[A](id: [a] => a => a, x: A, y: A): A = id[A](x)") === Count.Finite(2)

  def missing =
    count("def use[A](id: [a] => a => a): A = ???") === Count.Finite(0)

  def polyShadowing =
    count("def use[A](id: [A] => A => A, x: A): A = id[A](x)") === Count.Finite(1)

  def rankN =
    unresolved(
      "def use[A](f: [a] => (a, a) => a, x: A): A = ???",
      "outside the closed identity fragment"
    )

  def evidence =
    unresolved(
      "def use: [b] => b => Type[b] ?=> Expr[Any] = ???",
      "context function requires evidence analysis"
    )

  def dependency =
    unresolved("def use: [a] => a => External[a] = ???", "External")

  def environment =
    unresolved("def use(x: External): [a] => a => a = ???", "External")

}
