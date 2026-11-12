package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.reporting.{FrontEnd, Report}
import cardinality.tasty.CompiledTypes
import java.nio.file.Path
import org.specs2.Specification

/** Rows the compiled front end reads over the whole program: a module's classes with its
  * dependencies' (`ClosedProgram`, `DependencyClasses`, `World.readByDeclaration`) and what an
  * abstract or extensible template inherits (`Receivers`). Each fixture is compiled by scalac, and
  * each count is derived in the comment beside it.
  */
class ProgramRowsSpec extends Specification {

  def is = s2"""
    A dependency's class is read by its declaration or its symbols
      a case class with TASTy is the product of its fields            $caseClass
      an opaque type of a dependency is one value                     $opaque
      a Java enum is its constants; a Java class's state is not read  $java
    What a template inherits
      a concrete member wins over an abstract one mixed in after it   $concreteWins
      an abstract redeclaration makes a member abstract again         $reabstracted
      an inherited deferred given is a capability                     $deferredGiven
      a member a parent mixed in beside the template overrides refuses $mixedIn
      a parent reached through two parents is not read yet            $twoPaths
  """

  // Each method of `code`, compiled against `classpath` and read with it, by its row name.
  private def rows(code: String, classpath: List[Path] = Nil): Map[String, Count] = {
    val classes = Programs.compiled(code, classpath)
    val libraries = classpath ++ Programs.libraries
    val module = FrontEnd.Module(
      List("Example.scala" -> code),
      Report.Environment(libraries, classes = Some(CompiledTypes.Classes(List(classes), libraries)))
    )
    TastyFrontEnd
      .rows(module)
      .sources
      .flatMap(_.toOption)
      .flatMap(_.methods)
      .map(entry => entry.name -> entry.count)
      .toMap
  }

  private def uncovered(count: Count, what: String) = count must beLike {
    case Count.Unresolved(List(reason)) => reason must contain(what)
  }

  private lazy val dependency = Programs.compiled("""package dep
    |final case class Pair(a: Boolean, b: Boolean)
    |object Ids { opaque type Id = Int }
    |""".stripMargin)

  // `Pair` is two Booleans, 4 values, so `f` is one of the 2^4 predicates on it.
  def caseClass =
    rows("object M { def f(p: dep.Pair): Boolean = ??? }", List(dependency))("M.f") must_==
      Count.Finite(16)

  // An `Id` outside `Ids` is one value (rule F): `g` has the one implementation, its argument.
  def opaque =
    rows("object M { def g(i: dep.Ids.Id): dep.Ids.Id = ??? }", List(dependency))("M.g") must_==
      Count.Finite(1)

  // `Coin` has the constants HEADS and TAILS, so `c` is one of 2^2 predicates. A `Box.Point`
  // keeps state its constructor sets, which no signature shows: not read.
  def java = {
    val found = rows(
      "object M { def c(x: javafixtures.Coin): Boolean = ???; def b(x: javafixtures.Box.Point): Boolean = ??? }",
      List(Programs.packaged("javafixtures"))
    )
    (found("M.c") must_== Count.Finite(4)).and(uncovered(found("M.b"), "hidden constructor state"))
  }

  // `C` inherits `seed` and `f` from `A` and an abstract `f` from `B`, mixed in after `A`: the
  // concrete `A.f` implements it, computing `seed`. So `seed` is the one capability, and `g`
  // returns it: 1 (a capability `f` beside it would make 2).
  def concreteWins =
    rows("""trait A[T] { def seed: T; def f: T = seed }
      |trait B[T] { def f: T }
      |trait C[T] extends A[T] with B[T] { def g: T = seed }
      |class D extends C[Int] { def seed: Int = 1 }
      |""".stripMargin)("C.g") must_== Count.Finite(1)

  // In a `D`, `X.f` (another member's value) replaces the `A.f` that `C` inherits, though `X`
  // does not extend `C`: `f` varies across instances of `C`, which is not read yet (it would be
  // a capability: `g` returns `seed` or `f`, 2, where reading `f` as `seed` gives 1).
  def mixedIn =
    uncovered(
      rows("""trait A[T] { def seed: T; def f: T = seed }
        |trait X[T] extends A[T] { def other: T; override def f: T = other }
        |trait C[T] extends A[T] { def g: T = seed }
        |class D extends X[Int] with C[Int] { def seed: Int = 1; def other: Int = 2 }
        |""".stripMargin)("C.g"),
      "an enclosing class with a member something overrides"
    )

  // `B` redeclares `A`'s concrete `f` abstract: in `C` it is abstract, a capability beside
  // `seed`, and `g` returns either: 2.
  def reabstracted =
    rows("""trait A[T] { def seed: T; def f: T = seed }
      |trait B[T] extends A[T] { override def f: T }
      |trait C[T] extends B[T] { def g: T = seed }
      |""".stripMargin)("C.g") must_== Count.Finite(2)

  // A given written `= deferred` is one each class extending `A` gives: in `C` it is a
  // capability, and `g` returns it: 1.
  def deferredGiven =
    rows("""trait A[T] { given t: T = scala.compiletime.deferred }
      |trait C[T] extends A[T] { def g: T = t }
      |""".stripMargin)("C.g") must_== Count.Finite(1)

  // `D` reaches `A` through `B` and through `C`, and `E` extends `D`.
  def twoPaths = {
    val found = rows("""trait A { def a: Boolean }
      |trait B extends A
      |trait C extends A
      |trait D extends B with C { def g: Boolean = a }
      |trait E extends D { def h: Boolean = a }
      |""".stripMargin)
    uncovered(found("D.g"), "reaches a parent through two parents")
      .and(uncovered(found("E.h"), "reaches a parent through two parents"))
  }

}
