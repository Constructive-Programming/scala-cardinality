package cardinality.analysis.inhabitation

import cardinality.analysis.*
import org.specs2.Specification

class ExistentialInputSpec extends Specification {
  import Inhabitation.Shape
  import Shape.*

  def is = s2"""
    Scoped existential inputs
      shield nested witness binders during substitution          $nestedScopes
      alpha-rename binders deterministically during rewriting     $captureAvoidingRewrite
      compile and execute uniform input opening in real Scala     $compiledOpening
  """

  def nestedScopes = {
    val nested = Existential(List("W"), Product(List(Atom("W"), Atom("Outer"))))
    ExistentialInputs.mapAtoms(nested)(atom =>
      if (atom.id == "W") Atom("Wrong") else Atom("Changed")
    ) === Existential(List("W"), Product(List(Atom("W"), Atom("Changed"))))
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
