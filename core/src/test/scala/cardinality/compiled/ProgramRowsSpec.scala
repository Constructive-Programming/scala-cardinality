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
      a library's namespace under `scala` is a dependency like another $underScala
      a Java enum is its constants; a Java class's state is not read  $java
    What a template inherits
      a concrete member wins over an abstract one mixed in after it   $concreteWins
      an abstract redeclaration makes a member abstract again         $reabstracted
      an inherited deferred given is a capability                     $deferredGiven
      a member a parent mixed in beside the template overrides varies $mixedIn
      a parent reached through two parents is not read yet            $twoPaths
    An overridden member whose own body always calls the measured one
      supplies nothing                                                $definite
    A parent's constructor parameters
      are what the template passes, or free public fields             $parents
    An overridden member (a capability: ReceiversSpec, `Wide.get`)
      is not read where a behavior of it may call the measured one    $reentry
  """

  // Each method of `code`, compiled against `classpath` and read with it, by its row name.
  private def rows(code: String, classpath: List[Path] = Nil): Map[String, Count] =
    module(code, classpath).flatMap(_.methods).map(entry => entry.name -> entry.count).toMap

  // `code` compiled against `classpath` and read with it by the compiled front end.
  private def module(code: String, classpath: List[Path]): List[Report.Source] = {
    val classes = Programs.compiled(code, classpath)
    val libraries = classpath ++ Programs.libraries
    val module = FrontEnd.Module(
      List("Example.scala" -> code),
      Report.Environment(libraries, classes = Some(CompiledTypes.Classes(List(classes), libraries)))
    )
    TastyFrontEnd.rows(module).sources.flatMap(_.toOption)
  }

  // Each definition of `code`, compiled and read as `rows` reads it, with its size as rendered.
  private def definitions(code: String, classpath: List[Path]): Map[String, Option[String]] =
    module(code, classpath).flatMap(_.definitions).map(d => d.name -> d.size.map(_.render)).toMap

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

  // `scala.mylib` is no package of the standard library: its `Flag` is one Boolean, read by its
  // declaration on both sides, so `h` is one of 2^2 predicates and `Holds`, which stores a `Flag`,
  // has 2 values.
  def underScala = {
    val library = Programs.compiled("package scala.mylib\nfinal case class Flag(on: Boolean)")
    val code = "object M { def h(f: scala.mylib.Flag): Boolean = ??? }\n" +
      "final case class Holds(f: scala.mylib.Flag)"
    (rows(code, List(library))("M.h") must_== Count.Finite(4))
      .and(definitions(code, List(library))("Holds") must beSome("2"))
  }

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
  // does not extend `C`: `f` varies across instances of `C`, so it is a capability, and `g`
  // returns `seed` or `f`: 2 (reading `f` as `seed` would give 1).
  def mixedIn =
    rows("""trait A[T] { def seed: T; def f: T = seed }
      |trait X[T] extends A[T] { def other: T; override def f: T = other }
      |trait C[T] extends A[T] { def g: T = seed }
      |class D extends X[Int] with C[Int] { def seed: Int = 1; def other: Int = 2 }
      |""".stripMargin)("C.g") must_== Count.Finite(2)

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

  // Each open member here has a behavior that may call the measured one, which makes it a
  // behavior of the measured member's own: `Loop.more` calls `get` in a branch; `Sup.m` through
  // `super.helper`; `Esc.m` gives `this` to code outside the module; in a `Narrow3`, `Wide.more`
  // is `aux`, which `Narrow3` makes `get`; in a `D`, `C`'s `f` is `X.f`, mixed in beside `C`,
  // which calls `g`; `K.m` hands `this` to `sorted`, which calls `compare`. Each row is left.
  def reentry = {
    val found = rows("""class Loop(b: Boolean) { def get: Boolean = more; def more: Boolean = if b then get else b }
      |class Loop2 extends Loop(true) { override def more: Boolean = false }
      |trait A { def get: Boolean; def helper: Boolean = get }
      |trait Sup extends A { def get: Boolean = true; def m: Boolean = super.helper }
      |trait Sup2 extends Sup { override def m: Boolean = false }
      |trait Esc { def x: Boolean; def get: Boolean = x; def m: Boolean = Option(this).isEmpty }
      |trait Esc2 extends Esc { override def m: Boolean = true }
      |class Wide(b: Boolean) { def get: Boolean = b; def more: Boolean = b }
      |class Narrow extends Wide(true) { override def more: Boolean = aux; def aux: Boolean = false }
      |class Narrow3 extends Narrow { override def aux: Boolean = get }
      |trait F[T] { def seed: T; def g: T; def f: T = seed }
      |trait X[T] extends F[T] { override def f: T = g }
      |trait C[T] extends F[T] { def g: T = seed }
      |class D extends X[Int] with C[Int] { def seed: Int = 1 }
      |class K(n: Int) extends Ordered[K] { def compare(o: K): Int = n; def m: Boolean = List(this, this).sorted.isEmpty }
      |class K2 extends K(0) { override def m = true }
      |""".stripMargin)
    List("Loop.get", "Sup.get", "Esc.get", "Wide.get", "C.g", "K.compare")
      .map(n => uncovered(found(n), "an overridden member that may call it"))
      .reduce(_ and _)
  }

  // `Loop.more` returns `get` (its body's result) and `Wrap.m` evaluates it as a block statement
  // and a `val`'s value: each re-enters `get` in a receiver that overrides nothing, so no
  // implementation uses it. `Loop.get` is a Boolean computed from `b` alone, 2^2, and so is
  // `Wrap.get`. `Lazy.m` declares a lazy `val` of `get`, which runs only where it is read: no
  // definite view, so `m` is not left out (which would give 2^2), and since it names `get` it is a
  // possible one, which leaves the row.
  def definite = {
    val found = rows("""class Loop(b: Boolean) { def get: Boolean = more; def more: Boolean = get }
      |class Loop2 extends Loop(true) { override def more: Boolean = false }
      |class Wrap(b: Boolean) { def get: Boolean = b; def m: Boolean = { get; val r = get; r } }
      |class Wrap2 extends Wrap(true) { override def m: Boolean = true }
      |class Lazy(b: Boolean) { def get: Boolean = b; def m: Boolean = { lazy val r = get; b } }
      |class Lazy2 extends Lazy(true) { override def m: Boolean = true }
      |""".stripMargin)
    (found("Loop.get") must_== Count.Finite(4))
      .and(found("Wrap.get") must_== Count.Finite(4))
      .and(uncovered(found("Lazy.get"), Receivers.Reenters))
  }

  // `Abs` passes its `c` to `Base`, so `Base` holds `c`: `x` is a Boolean computed from `c`, 2^2,
  // and `more`, beside the capability `x`, one computed from both, 2^(2 × 2). `Free` extends `TP`
  // without passing its argument: its public field `t` is a value each instance chooses, and `f`
  // is a Boolean computed from it, 2^2. `Hid` leaves `HP`'s private parameter to a subclass: not
  // read. `RO` overrides the field `p` that `R` inherits from `P`: what `P` was given and what
  // `p` answers are two values in an `RO`, which is not read. `T` overrides the field `q` of `Q`,
  // which `T` does not pass: what `Q`'s body read of its parameter is hidden, and not read.
  def parents = {
    val found = rows("""class Base(b: Boolean) { def get: Boolean = b }
      |abstract class Abs(c: Boolean) extends Base(c) { def x: Boolean; def more: Boolean = x }
      |trait TP(val t: Boolean) { def tt: Boolean = t }
      |trait Free extends TP { def f: Boolean = t }
      |class HP(h: Boolean) { def peek: Boolean = h }
      |trait Hid extends HP { def g: Boolean = peek }
      |class P(val p: Boolean)
      |abstract class R extends P(true) { def target(b: Boolean): Boolean }
      |abstract class RO extends R { override val p: Boolean = false }
      |class Q(val q: Boolean) { private val z = q; def zz: Boolean = z }
      |trait T extends Q { override val q: Boolean = true; def m: Boolean = zz }
      |""".stripMargin)
    (found("Abs.x") must_== Count.Finite(4))
      .and(found("Abs.more") must_== Count.Finite(16))
      .and(found("Free.f") must_== Count.Finite(4))
      .and(uncovered(found("Hid.g"), "a parent with constructor parameters or in a class"))
      .and(uncovered(found("R.target"), "a constructor field something overrides"))
      .and(uncovered(found("T.m"), "a parent with constructor parameters or in a class"))
  }

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
