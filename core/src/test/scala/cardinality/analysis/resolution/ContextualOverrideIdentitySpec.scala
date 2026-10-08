package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

class ContextualOverrideIdentitySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Contextual inherited declaration identity
      excludes a matching using method without excluding its inputs   $named
      ignores names of contextual parameters                           $anonymous
      alpha-renames method type parameters                             $methodBinders
      supports a method with only contextual inputs                    $contextOnly
      retains inputs from multiple using clauses                       $multipleClauses
      substitutes the inherited receiver constructor                   $substitution
      retains a separate inherited producer                            $capability
      retains a same-name overload with a different domain              $overload
      retains a curried overload with a different clause layout         $curriedOverload
      keeps equal-shaped nominal evidence types distinct               $nominalEvidence
      compares legacy implicit and using clauses as contextual         $legacy
      compares using and legacy implicit in the reverse direction      $reverseLegacy
      ignores annotations on contextual parameters                     $annotated
      expands transparent aliases in contextual inputs                 $alias
    Conservative boundaries
      does not guess identity across ordinary and contextual clauses   $ordinary
      keeps dependent contextual results unresolved                    $dependent
      keeps imported evidence identity unresolved                      $imported
      preserves additional unsupported contextual modifiers            $inlineParameter
  """

  private def count(code: String): Count =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Context.scala", dialects.Scala3(code).parse[Source].get)))
      .find(_.name == "Instance.get")
      .get
      .count

  private def identityUnresolved(code: String) =
    count(code) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("inherited override identity")) must beTrue
    }

  def named =
    count("""
      trait Base[A] { def get(a: A)(using fallback: A): A }
      abstract class Instance[A] extends Base[A] {
        def get(value: A)(using other: A): A = value
      }
    """) === Count.Finite(2)

  def anonymous =
    count("""
      trait Base[A] { def get(a: A)(using A): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(using fallback: A): A = a
      }
    """) === Count.Finite(2)

  def methodBinders =
    count("""
      trait Base { def get[A](a: A)(using fallback: A): A }
      object Instance extends Base {
        def get[B](b: B)(using other: B): B = b
      }
    """) === Count.Finite(2)

  def substitution =
    count("""
      trait Base[F[_, _]] { def get[X, A](fa: F[X, A])(using fallback: A): A }
      object Instance extends Base[Tuple2] {
        def get[Y, B](fa: (Y, B))(using fallback: B): B = fa._2
      }
    """) === Count.Finite(2)

  def capability =
    count("""
      trait Base[A] { def get(a: A)(using fallback: A): A; def seed: A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(using fallback: A): A = a
      }
    """) === Count.Finite(3)

  def overload =
    count("""
      trait Base[A] { def get(a: A, b: A)(using fallback: A): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(using fallback: A): A = a
      }
    """) === Count.Countable

  def curriedOverload =
    count("""
      trait Base[A] { def get(a: A, b: A)(using fallback: A): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(b: A)(using x: A, y: A): A = a
      }
    """) === Count.Countable

  def legacy =
    count("""
      trait Base[A] { def get(a: A)(implicit fallback: A): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(using fallback: A): A = a
      }
    """) === Count.Finite(2)

  def annotated =
    count("""
      trait Base[A] { def get(a: A)(using fallback: A): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(using @scala.annotation.unused fallback: A): A = a
      }
    """) === Count.Finite(2)

  def ordinary =
    identityUnresolved("""
      trait Base[A] { def get(a: A)(using fallback: A): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(fallback: A): A = a
      }
    """)

  def alias =
    count("""
      type Alias[A] = A
      trait Base[A] { def get(a: A)(using fallback: Alias[A]): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(using fallback: A): A = a
      }
    """) === Count.Finite(2)

  def dependent =
    identityUnresolved("""
      trait Evidence[A] { type Out }
      trait Base[A] { def get(a: A)(using ev: Evidence[A]): ev.Out }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(using ev: Evidence[A]): ev.Out = ???
      }
    """)

  def imported =
    identityUnresolved("""
      import external.Evidence
      trait Base[A] { def get(a: A)(using ev: Evidence[A]): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(using ev: Evidence[A]): A = a
      }
    """)

  def contextOnly =
    count("""
      trait Base[A] { def get(using A): A }
      abstract class Instance[A] extends Base[A] {
        def get(using seed: A): A = seed
      }
    """) === Count.Finite(1)

  def multipleClauses =
    count("""
      trait Base[A] { def get(a: A)(using x: A)(using y: A): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(using first: A)(using second: A): A = a
      }
    """) === Count.Finite(3)

  def reverseLegacy =
    count("""
      trait Base[A] { def get(a: A)(using fallback: A): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(implicit fallback: A): A = a
      }
    """) === Count.Finite(2)

  def nominalEvidence =
    count("""
      case class First[A](value: A)
      case class Second[A](value: A)
      trait Base[A] { def get(a: A)(using ev: First[A]): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: A)(using ev: Second[A]): A = a
      }
    """) === Count.Countable

  def inlineParameter =
    identityUnresolved("""
      trait Base[A] { def get(a: A)(using fallback: A): A }
      abstract class Instance[A] extends Base[A] {
        inline def get(a: A)(using inline fallback: A): A = a
      }
    """)

}
