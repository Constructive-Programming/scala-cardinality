package cardinality

import scala.meta.*

import org.specs2.Specification

class ExistentialInputSpec extends Specification {
  import Inhabitation.{Binding, Count, Shape}
  import Shape.*

  def is = s2"""
    Scoped existential inputs
      open a package and use its own hidden witness             $ownWitness
      keep two input packages independent                       $independentPackages
      keep two wildcard slots in one package independent         $independentSlots
      share one witness across fields of a package              $sharedFields
      do not mix independent producer and consumer witnesses    $independentRoles
      keep inputs of the same existential alias independent     $aliasPackages
      capture one wildcard before substituting an alias         $aliasArgument
      open packages supplied by arrow introduction              $introducedInput
      preserve a stable singleton's package identity            $singletonIdentity
      preserve enclosing package capture identity               $enclosingCapture
      do not equate a hidden type with a universal method binder $rigidWitness
      do not choose Unit to feed an existential callable         $noInstantiation
      keep result construction separate from input opening      $resultBoundary
      reject unimplemented wildcard bounds explicitly           $boundsBoundary
      reject fresh packages returned by opaque callables         $callableBoundary
      shield nested witness binders during substitution          $nestedScopes
      avoid collisions with free atom identities                $atomCollisions
      avoid capture by nested existential binder names            $nestedBinderCollision
      alpha-rename binders deterministically during rewriting     $captureAvoidingRewrite
      retain singleton provenance after capture-avoiding rewriting $rewrittenSingleton
      preserve productive hidden-type cycles after opening        $productiveCycle
      reject unseeded hidden-type cycles                           $unseededCycle
      compile and execute uniform input opening in real Scala     $compiledOpening
  """

  private val packed =
    "case class Packed[A, R](value: A, consume: A => R)\n"

  private def count(code: String, name: String = "run"): Count = {
    val source = dialects.Scala3(code).parse[Source].get
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Existentials.scala", source)))
      .find(_.name == name)
      .get
      .count
  }

  private def unresolved(value: Count, reason: String) =
    value must beLike {
      case Count.Unresolved(reasons) => reasons.exists(_.contains(reason)) must beTrue
    }

  def ownWitness =
    count(packed + "def run[R](p: Packed[?, R]): R = ???") === Count.Finite(1)

  def independentPackages =
    count(packed + "def run[R](p: Packed[?, R], q: Packed[?, R]): R = ???") === Count.Finite(2)

  def independentSlots =
    count(
      "case class Mixed[A, B, R](value: A, consume: B => R)\n" +
        "def run[R](p: Mixed[?, ?, R]): R = ???"
    ) === Count.Finite(0)

  def sharedFields =
    count(
      "case class Packed[A, R](left: A, right: A, consume: A => R)\n" +
        "def run[R](p: Packed[?, R]): R = ???"
    ) === Count.Finite(2)

  def independentRoles =
    count(
      "case class Source[A](value: A)\ncase class Sink[A, R](consume: A => R)\n" +
        "def run[R](source: Source[?], sink: Sink[?, R]): R = ???"
    ) === Count.Finite(0)

  def aliasPackages =
    count(
      packed + "type Hidden[R] = Packed[?, R]\n" +
        "def run[R](p: Hidden[R], q: Hidden[R]): R = ???"
    ) === Count.Finite(2)

  def aliasArgument =
    count(
      "type Shared[A, R] = (A, A, A => R)\n" +
        "def run[R](p: Shared[?, R]): R = ???"
    ) === Count.Finite(2)

  def introducedInput =
    count(packed + "def run[R]: Packed[?, R] => R = ???") === Count.Finite(1)

  def singletonIdentity =
    count(packed + "def run[R](p: Packed[?, R])(q: p.type): R = ???") === Count.Finite(1)

  def enclosingCapture =
    count(packed + "class Env[R](p: Packed[?, R]) { def run: R = ??? }", "Env.run") === Count
      .Finite(1)

  def rigidWitness =
    count("case class Box[A](value: A)\ndef run[A](box: Box[?]): A = ???") === Count.Finite(0)

  def noInstantiation =
    count("def run[R](consume: Function1[?, R]): R = ???") === Count.Finite(0)

  def resultBoundary =
    unresolved(count("def run(p: Option[?]): Option[?] = p"), "result construction and repackaging")

  def boundsBoundary =
    unresolved(count("def run[A](p: Option[? <: A]): Unit = ()"), "? <: A").and(
      unresolved(count("def run[A](p: Option[? >: A]): Unit = ()"), "? >: A")
    )

  def callableBoundary =
    unresolved(
      count(packed + "def run[R](make: Unit => Packed[?, R]): R = ???"),
      "scoped witness-generation"
    )

  def nestedScopes = {
    val nested = Existential(List("W"), Product(List(Atom("W"), Atom("Outer"))))
    ExistentialInputs.mapAtoms(nested)(atom =>
      if (atom.id == "W") Atom("Wrong") else Atom("Changed")
    ) === Existential(List("W"), Product(List(Atom("W"), Atom("Changed"))))
  }

  def atomCollisions =
    Inhabitation.count(
      List(Binding("package", Existential(List("W"), Atom("W")))),
      Atom("existential:1")
    ) === Count.Finite(0)

  def nestedBinderCollision = {
    val nested = Existential(
      List("W"),
      Existential(
        List("existential:1"),
        Product(List(Atom("W"), Function(List(Atom("existential:1")), Atom("R"))))
      )
    )
    Inhabitation.count(List(Binding("package", nested)), Atom("R")) === Count.Finite(0)
  }

  def captureAvoidingRewrite = {
    val nested = Existential(List("W"), Product(List(Atom("W"), Atom("Outer"))))
    def rewrite() =
      ExistentialInputs.mapAtoms(nested)(atom => if (atom.id == "Outer") Atom("W") else atom)
    (rewrite() === rewrite()).and(rewrite() must beLike {
      case Existential(List(bound), Product(List(Atom(inner), Atom(outer)))) =>
        (bound !== "W").and(inner === bound).and(outer === "W")
    })
  }

  def rewrittenSingleton = {
    val packed =
      Existential(List("W"), Product(List(Atom("W"), Function(List(Atom("W")), Atom("Outer")))))
    val rewritten =
      ExistentialInputs.mapAtoms(packed)(atom => if (atom.id == "Outer") Atom("W") else atom)
    Inhabitation.count(
      List(Binding("p", rewritten), Binding("alias", Singleton("p", Some(rewritten)))),
      Atom("W")
    ) === Count.Finite(1)
  }

  def productiveCycle =
    count(
      "case class Steps[A, R](seed: A, step: A => A, consume: A => R)\n" +
        "def run[R](p: Steps[?, R]): R = ???"
    ) === Count.Countable

  def unseededCycle =
    count(
      "case class Steps[A, R](step: A => A, consume: A => R)\n" +
        "def run[R](p: Steps[?, R]): R = ???"
    ) === Count.Finite(0)

  def compiledOpening = {
    val packed = ExistentialInputExamples.Packed(3, (value: Int) => value + 1)
    (ExistentialInputExamples.run(packed) === 4).and(
      ExistentialInputExamples.same(packed)(packed) === 4
    )
  }

}

private object ExistentialInputExamples {
  case class Packed[A, R](value: A, consume: A => R)

  def run[R](packed: Packed[?, R]): R = {
    def opened[A](value: Packed[A, R]): R = value.consume(value.value)
    opened(packed)
  }

  def same[R](packed: Packed[?, R])(alias: packed.type): R = run(alias)
}
