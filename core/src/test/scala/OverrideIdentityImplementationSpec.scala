package cardinality

import scala.meta.*

import org.specs2.Specification

class OverrideIdentityImplementationSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Direct inherited declaration identity
      substitutes tuple constructors and alpha-renames method binders $tuple
      retains qualified parent and constructor identities             $qualified
      substitutes sum constructors without supplying reverseGet       $sum
      keeps unrelated inherited capabilities                         $capability
      does not identify nominal products with tuples                  $nominal
      retains unresolved imported constructor identity                 $imported
      keeps a foreign method binder distinct from a class binder       $foreignBinder
      does not drop declarations at matching offsets in other files    $fileIdentity
      preserves a same-name overload with different arity              $overload
      retains an obligation for transparent alias equivalence           $alias
      does not classify a covariant return as an unrelated overload      $covariance
      ignores parameter annotations when identifying the target slot     $annotated
  """

  private def analyze(files: (String, String)*): List[MethodAnalysis.Entry] =
    MethodAnalysis.analyze(files.toList.map { (path, source) =>
      MethodAnalysis.Input(path, dialects.Scala3(source).parse[Source].get)
    })

  private def count(source: String): Count =
    analyze("Identity.scala" -> source).find(_.name == "Instance.get").get.count

  def tuple =
    count("""
      trait Base[F[_,_]] { def get[X,A](fa:F[X,A]):A }
      object Instance extends Base[Tuple2] {
        def get[Y,B](fa:(Y,B)):B = fa._2
      }
    """) === Count.Finite(1)

  def sum =
    count("""
      trait Base[F[_,_]] { def get[X,A](a:A):F[X,A] }
      object Instance extends Base[Either] {
        def get[X,A](a:A):Either[X,A] = Right(a)
      }
    """) === Count.Finite(1)

  def qualified =
    count("""
      object Parent { trait Base[F[_,_]] { def get[X,A](fa:F[X,A]):A } }
      object Instance extends Parent.Base[scala.Tuple2] {
        def get[X,A](fa:(X,A)):A = fa._2
      }
    """) === Count.Finite(1)

  def capability =
    count("""
      trait Base[A] { def get(a:A):A; def seed:A }
      abstract class Instance[A] extends Base[A] {
        def get(a:A):A = a
      }
    """) === Count.Finite(2)

  def annotated =
    count("""
      trait Base[A] { def get(a:A):A }
      abstract class Instance[A] extends Base[A] {
        def get(@scala.annotation.unused a:A):A = a
      }
    """) === Count.Finite(1)

  def nominal =
    count("""
      case class Pair[X,A](x:X,a:A)
      trait Base[F[_,_]] { def get[X,A](fa:F[X,A]):A }
      object Instance extends Base[Pair] {
        def get[X,A](fa:(X,A)):A = fa._2
      }
    """) must beLike { case Count.Unresolved(_) => ok }

  def imported =
    count("""
      import external.Foreign
      trait Base[F[_,_]] { def get[X,A](fa:F[X,A]):A }
      object Instance extends Base[Foreign] {
        def get[X,A](fa:Foreign[X,A]):A = ???
      }
    """) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("inherited override identity")) must beTrue
    }

  def foreignBinder =
    count("""
      trait Base[A] { def other[A]():A }
      abstract class Instance[A] extends Base[A] { def get(a:A):A = a }
    """) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("polymorphic capability binder")) must beTrue
    }

  def overload =
    count("""
      trait Base[A] { def get(a:A,b:A):A }
      abstract class Instance[A] extends Base[A] { def get(a:A):A = a }
    """) === Count.Countable

  def alias =
    count("""
      type Alias[A] = A
      trait Base[A] { def get(a:Alias[A]):A }
      abstract class Instance[A] extends Base[A] { def get(a:A):A = a }
    """) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("inherited override identity")) must beTrue
    }

  def covariance =
    count("""
      trait Base[A] { def get(a:A):Any }
      abstract class Instance[A] extends Base[A] { def get(a:A):A = a }
    """) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("inherited override identity")) must beTrue
    }

  def fileIdentity = {
    val entries = analyze(
      "One.scala" -> "def get[A](a:A):A = a",
      "Two.scala" -> "def seed: Boolean"
    )
    entries.find(_.name == "get").get.count must beLike {
      case Count.Unresolved(_) => ok
    }
  }

}
