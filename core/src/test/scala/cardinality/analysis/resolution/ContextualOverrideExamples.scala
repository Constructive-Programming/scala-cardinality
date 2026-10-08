package cardinality.analysis.resolution

// Compiled fixtures pin the contextual syntax used by the source-to-count regressions.
object ContextualOverrideExamples {

  trait Base {
    def get[A](a: A)(using fallback: A): A
  }

  object Renamed extends Base {
    def get[B](b: B)(using other: B): B = other
  }

  trait LegacyBase {
    def get[A](a: A)(implicit fallback: A): A
  }

  object Modern extends LegacyBase {
    def get[A](a: A)(using fallback: A): A = fallback
  }

  object Legacy extends Base {
    def get[A](a: A)(implicit fallback: A): A = fallback
  }

  case class First[A](value: A)
  case class Second[A](value: A)

  trait EvidenceBase[A] {
    def get(a: A)(using evidence: First[A]): A
  }

  abstract class NominalOverload[A] extends EvidenceBase[A] {
    def get(@scala.annotation.unused a: A)(using evidence: Second[A]): A = evidence.value
  }

}
