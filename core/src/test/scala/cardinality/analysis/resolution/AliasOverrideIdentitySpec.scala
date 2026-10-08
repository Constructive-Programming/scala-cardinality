package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

class AliasOverrideIdentitySpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Transparent aliases in inherited signatures
      expands a closed alias in parameters and result                 $closed
      substitutes alias parameters and alpha-renames method binders   $parameterized
      expands an alias on the implementation side                     $reverse
      follows alias chains                                            $chain
      expands qualified aliases in their declaration scope            $qualified
      retains equal-shaped but nominally distinct overloads            $nominal
      substitutes a receiver binder captured by a parent alias         $captured
      keeps captured binders separate from shadowing method binders    $shadowed
      does not capture a caller binder in an alias body                 $caller
      retains a separate inherited capability                          $capability
      normalizes an alias to a builtin tuple                            $tuple
      permits nested applications of the same nonrecursive alias        $nested
      keeps a nearer source alias distinct from an outer binder         $nearerAlias
    Conservative boundaries
      guards recursive alias expansion                                 $recursive
      guards mutually recursive aliases                                $mutual
      guards opaque aliases                                            $opaque
      guards abstract type members                                     $abstractMember
      guards higher-kinded alias parameters                            $higherKinded
      guards type-lambda aliases                                        $lambda
      guards bounded alias parameters                                  $bounded
      guards incorrect alias arity                                     $arity
      guards imported names in the alias declaration scope              $imported
      respects the configured expansion depth limit                     $depth
      guards nearer imports shadowing an outer binder                   $nearerImport
      limits nested argument expansion                                  $argumentDepth
  """

  private def count(code: String, name: String = "Instance.get"): Count =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Aliases.scala", dialects.Scala3(code).parse[Source].get)))
      .find(_.name == name)
      .get
      .count

  private def unresolved(code: String) =
    count(code) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("inherited override identity")) must beTrue
    }

  def closed =
    count("""
      type Flag = Boolean
      trait Base { def get(a: Flag): Flag }
      object Instance extends Base { def get(a: Boolean): Boolean = a }
    """) === Count.Finite(4)

  def parameterized =
    count("""
      type Id[A] = A
      trait Base { def get[A](a: Id[A]): Id[A] }
      object Instance extends Base { def get[B](b: B): B = b }
    """) === Count.Finite(1)

  def reverse =
    count("""
      type Id[A] = A
      trait Base { def get[A](a: A): A }
      object Instance extends Base { def get[B](b: Id[B]): Id[B] = b }
    """) === Count.Finite(1)

  def chain =
    count("""
      type Id[A] = A
      type More[A] = Id[A]
      trait Base[A] { def get(a: More[A]): More[A] }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """) === Count.Finite(1)

  def qualified =
    count("""
      object Library {
        case class Token[A](value: A)
        type Alias[A] = Token[A]
      }
      trait Base[A] { def get(a: Library.Alias[A]): A }
      abstract class Instance[A] extends Base[A] {
        def get(a: Library.Token[A]): A = a.value
      }
    """) === Count.Finite(1)

  def nominal =
    count("""
      object Library {
        case class Token[A](value: A)
        type Alias[A] = Token[A]
      }
      trait Base[A] { def get(a: Library.Alias[A]): A }
      abstract class Instance[A] extends Base[A] {
        case class Token[B](value: B)
        def get(a: Token[A]): A = a.value
      }
    """) === Count.Countable

  def captured =
    count("""
      trait Base[T] { type Captured = T; def get(a: Captured): Captured }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """) === Count.Finite(1)

  def shadowed =
    count("""
      trait Base[T] {
        type Captured = T
        def get[T](a: Captured, b: T): Captured
      }
      abstract class Instance[A] extends Base[A] {
        def get[B](a: A, b: B): A = a
      }
    """) === Count.Finite(1)

  def caller =
    unresolved("""
      type Alias = Missing
      trait Base[A] { def get(a: Alias): A }
      abstract class Instance[Missing] extends Base[Missing] {
        def get(a: Missing): Missing = a
      }
    """)

  def capability =
    count("""
      type Id[A] = A
      trait Base[A] { def get(a: Id[A]): A; def seed: A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """) === Count.Finite(2)

  def tuple =
    count("""
      type Pair[A] = (A, A)
      trait Base[A] { def get(a: Pair[A]): A }
      abstract class Instance[A] extends Base[A] { def get(a: (A, A)): A = a._1 }
    """) === Count.Finite(2)

  def recursive =
    unresolved("""
      type Loop[A] = Loop[A]
      trait Base[A] { def get(a: Loop[A]): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """)

  def mutual =
    unresolved("""
      type First[A] = Second[A]
      type Second[A] = First[A]
      trait Base[A] { def get(a: First[A]): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """)

  def opaque =
    unresolved("""
      opaque type Hidden[A] = A
      trait Base[A] { def get(a: Hidden[A]): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """)

  def abstractMember =
    unresolved("""
      trait Base[A] { type Member; def get(a: Member): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """)

  def higherKinded =
    unresolved("""
      type Alias[F[_], A] = F[A]
      trait Base[A] { def get(a: Alias[Option, A]): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """)

  def lambda =
    unresolved("""
      type Alias = [A] =>> A
      trait Base[A] { def get(a: Alias[A]): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """)

  def bounded =
    unresolved("""
      type Alias[A <: Any] = A
      trait Base[A] { def get(a: Alias[A]): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """)

  def arity =
    unresolved("""
      type Alias[A, B] = A
      trait Base[A] { def get(a: Alias[A]): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """)

  def imported =
    unresolved("""
      object Library { import external.Token; type Alias[A] = Token[A] }
      trait Base[A] { def get(a: Library.Alias[A]): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """)

  def depth = {
    val code = """
      type First[A] = Second[A]
      type Second[A] = Third[A]
      type Third[A] = Fourth[A]
      type Fourth[A] = A
      trait Base[A] { def get(a: First[A]): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """
    MethodAnalysis
      .analyze(
        List(MethodAnalysis.Input("Depth.scala", dialects.Scala3(code).parse[Source].get)),
        MethodAnalysis.Limits(maxTypeDepth = 3)
      )
      .find(_.name == "Instance.get")
      .get
      .count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("inherited override identity")) must beTrue
    }
  }

  def nested =
    count("""
      type Id[A] = A
      trait Base[A] { def get(a: Id[Id[A]]): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """) === Count.Finite(1)

  // A nearer source alias (`Library.Alias = Unit`) must win over the outer class binder `T`,
  // so the inherited slot is retained, not collapsed into the implementation. Asserting only
  // "not a false exclusion" keeps this independent of how the general resolver measures the
  // retained capability's own signature.
  def nearerAlias = {
    val c = count(
      """
        class Outer[T] {
          object Library { type T = Unit; type Alias = T }
          trait Base { def get(a: Library.Alias): T }
          abstract class Instance extends Base { def get(a: T): T = a }
        }
      """,
      "Outer.Instance.get"
    )
    c !== Count.Finite(1)
  }

  def nearerImport =
    MethodAnalysis
      .analyze(
        List(
          MethodAnalysis.Input(
            "Import.scala",
            dialects
              .Scala3("""
          class Outer[T] {
            object Library { import external.T; type Alias = T }
            trait Base { def get(a: Library.Alias): T }
            abstract class Instance extends Base { def get(a: T): T = a }
          }
        """).parse[Source]
              .get
          )
        )
      )
      .find(_.name == "Outer.Instance.get")
      .get
      .count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("inherited override identity")) must beTrue
    }

  def argumentDepth = {
    val code = """
      type Id[A] = A
      trait Base[A] { def get(a: Id[Id[Id[Id[A]]]]): A }
      abstract class Instance[A] extends Base[A] { def get(a: A): A = a }
    """
    MethodAnalysis
      .analyze(
        List(MethodAnalysis.Input("Depth.scala", dialects.Scala3(code).parse[Source].get)),
        MethodAnalysis.Limits(maxTypeDepth = 3)
      )
      .find(_.name == "Instance.get")
      .get
      .count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("inherited override identity")) must beTrue
    }
  }

}
