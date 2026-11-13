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
      a record without TASTy is what its members supply              $records
    What a template inherits
      a concrete member wins over an abstract one mixed in after it   $concreteWins
      an abstract redeclaration makes a member abstract again         $reabstracted
      an inherited deferred given is a capability                     $deferredGiven
      a member a parent mixed in beside the template overrides varies $mixedIn
      a parent reached through two parents is not read yet            $twoPaths
      a member a case overrides computes from the case a sum holds    $sumOverridden
      a bounded abstract type member is an atom with its bound's proof $boundedMember
    An overridden member whose own body always calls the measured one
      supplies nothing                                                $definite
    A parent's constructor parameters
      are what the template passes, or free public fields             $parents
    An overridden member (a capability: ReceiversSpec, `Wide.get`)
      is not read where a behavior of it may call the measured one    $reentry
      a `new` whose class the classes read do not hold may            $unresolvedNew
      is read where no reference by name leads from it to the site  $unnamedCaller
  """

  // Each method of `code`, compiled against `classpath` and read with it, by its row name.
  private def rows(
      code: String,
      classpath: List[Path] = Nil,
      read: Option[List[Path]] = None
  ): Map[String, Count] =
    module(code, classpath, read).flatMap(_.methods).map(entry => entry.name -> entry.count).toMap

  // `code` compiled against `classpath`, read with `read` (by default the same) by the compiled
  // front end.
  private def module(
      code: String,
      classpath: List[Path],
      read: Option[List[Path]] = None
  ): List[Report.Source] = {
    val classes = Programs.compiled(code, classpath)
    val libraries = read.getOrElse(classpath) ++ Programs.libraries
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

  // A Java interface is a capability record: `Toggle` is its abstract `flip`, one of the 2^2
  // functions of a Boolean (its default `same`, which nothing overrides, is code that is not
  // supplied and computes from it), so `t` is one of 2^4 predicates on it. `Dial.Fast` overrides
  // `Dial`'s default `twice`, which then varies beside `turn`: `Dial` is one of 4 × 4 values and `d`
  // one of 2^16 predicates on it. scalameta's `Tree`, a Scala 2 trait, has abstract members the
  // site may not call (`private[meta]`), which code that is not supplied may read: what a `Tree`
  // is, the sources do not say.
  def records = {
    val t = rows(
      "object M { def t(x: javafixtures.Toggle): Boolean = ??? }",
      List(Programs.packaged("javafixtures"))
    )("M.t")
    val d = rows(
      "object M { def d(x: javarecords.Dial): Boolean = ??? }",
      List(Programs.packaged("javarecords"))
    )("M.d")
    val meta = List(classOf[scala.meta.Tree], classOf[scala.meta.internal.trees.Metadata.Ast])
      .map(Programs.jarOf)
    val m = rows("object M { def m(x: scala.meta.Tree): Boolean = ??? }", meta)("M.m")
    (t must_== Count.Finite(16))
      .and(d must_== Count.Finite(65536))
      .and(
        m must_== Count.Unresolved(
          List("capability record with members hidden at the use site: scala.meta.Tree")
        )
      )
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

  // `S` is a sealed family of two case objects: as a receiver it is one value of their sum, 2.
  // `A` overrides `g`, but what `g` returns computes from the case, which the sum holds, so `g` is
  // no capability beside it: `m` is a Boolean computed from the case, 2^2 = 4.
  def sumOverridden =
    rows("""sealed trait S { def g: Boolean = true; def m: Boolean = g }
      |case object A extends S { override def g: Boolean = false }
      |case object B extends S
      |""".stripMargin)("S.m") must_== Count.Finite(4)

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

  // A `new` whose class may be the site's re-enters it (its constructor runs). On a value of the
  // record `Back` (another instance than the site's), `BackI.k` builds: a `UseOf`, which is a `Use`
  // (left); with `gone` unread, a `UseOf[gone.Gone]` whose type does not resolve and a
  // `gone.Gone` that names no class read, either of which may be anything (left); an `Other`,
  // which reaches nothing, so `Back` is its `k` (2 values, `j` computing from it) and `target` one
  // of 2^2, with `gone` read or not (a type argument runs no code). On the site's own instance,
  // `Own2.k` builds a `gone.Gone` unread: `Own.get` is left. (`Opens` names `target` in a function
  // literal, which whatever applies the function may run: no reading by name settles these rows.)
  def unresolvedNew = {
    val gone = Programs.compiled("package gone\nclass Gone")
    def found(built: String, read: List[Path]) = rows(
      s"""trait Back { def k: Boolean = true; def j: Boolean = k }
        |class Use { def target(b: Back): Boolean = true }
        |class UseOf[A] extends Use
        |class Other[A]
        |object BackI extends Back { override def k: Boolean = { new $built; true } }
        |class Own(b: Boolean) { def get: Boolean = b; def k: Boolean = b }
        |class Own2 extends Own(true) { override def k: Boolean = { new $built; true } }
        |object Opens { val f: () => Boolean = () => new Use().target(BackI) }
        |""".stripMargin,
      List(gone),
      Some(read)
    )
    def left(built: String, read: List[Path], row: String = "Use.target") =
      uncovered(found(built, read)(row), Receivers.Reenters)
    left("UseOf[gone.Gone]", List(gone))
      .and(left("UseOf[gone.Gone]", Nil))
      .and(left("gone.Gone", Nil))
      .and(found("Other[gone.Gone]", List(gone))("Use.target") must_== Count.Finite(4))
      .and(found("Other[gone.Gone]", Nil)("Use.target") must_== Count.Finite(4))
      .and(left("gone.Gone", Nil, "Own.get"))
  }

  // On a value of the record `E[Unit]` (another instance than the site's), the anonymous class of
  // `instance` overrides `neqv` with `!f(x, y)`, applying a function it is given. Nothing that
  // calls `target` is reached from a behavior of `neqv`: `probe` names it, but nothing names
  // `probe`; `check` runs a function literal naming it inside `forall`, whose Boolean result keeps
  // no function; `later` gives it by name to `twice`, which forces it where it names `b`. So `neqv`
  // is a capability beside the abstract `eqv`, each one of the 2 functions (Unit, Unit) => Boolean
  // (its input is one value): `E[Unit]` is 2 × 2 values and `target` one of 2^4. Left: `Mine.neqv` calls `probe`; `back` passes `lift`
  // a function literal naming `target`, which whatever applies the function may run.
  def unnamedCaller = {
    def found(more: String) = rows(
      s"""trait E[A] { def eqv(x: A, y: A): Boolean; def neqv(x: A, y: A): Boolean = !eqv(x, y) }
      |object E {
      |  def instance[A](f: (A, A) => Boolean): E[A] = new E[A] {
      |    def eqv(x: A, y: A): Boolean = f(x, y)
      |    override def neqv(x: A, y: A): Boolean = !f(x, y)
      |  }
      |  def lift[A](g: (A, A) => Boolean): E[A] = instance(g)
      |  val units: E[Unit] = lift((x, y) => x == y)
      |  def probe: Boolean = Use.target(units)
      |  def check: Boolean = List(units).forall(e => Use.target(e))
      |  def twice(b: => Boolean): Option[Boolean] = Some(b && b)
      |  def later: Boolean = twice(Use.target(units)).isEmpty
      |  $more
      |}
      |object Use { def target(e: E[Unit]): Boolean = true }
      |""".stripMargin
    )("Use.target")
    (found("") must_== Count.Finite(16))
      .and(
        uncovered(
          found(
            "object Mine extends E[Unit] { def eqv(x: Unit, y: Unit) = true; override def neqv(x: Unit, y: Unit) = probe }"
          ),
          Receivers.Reenters
        )
      )
      .and(
        uncovered(
          found("val back: E[Unit] = lift((x, y) => Use.target(units))"),
          Receivers.Reenters
        )
      )
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

  // A bounded abstract type member of a receiver is an atom, and its bound a proof, as a bounded
  // type parameter's is. In `Lo`, `T <: Boolean` makes `t` a Boolean through the proof, which the
  // solver refuses to combine with the value a member produces, as for a bounded type parameter.
  // `U <: AnyRef` says nothing of the values: `pick` returns `u`, 1. In
  // `Var`, `F[+X]` relates its applications by subtyping: not read. A constructor row hands no
  // proof to its positions: `I`'s `x: T` with `T >: Boolean` is not read (true and false are Ts).
  def boundedMember = {
    val found = rows("""trait Lo { type T <: Boolean; def t: T; def get: Boolean = true }
      |trait Ref { type U <: AnyRef; def u: U; def pick: U = u }
      |trait Var { type F[+X]; def f: F[Int]; def g: F[Int] = f }
      |trait O { type T >: Boolean; class I(val x: T) }
      |""".stripMargin)
    (found("Lo.get") must_== Count.Unresolved(
      List(
        "unsupported subtype evidence interaction: a callable produces a value its bound views as a structural type"
      )
    ))
      .and(found("Ref.pick") must_== Count.Finite(1))
      .and(uncovered(found("Var.g"), "variant"))
      .and(uncovered(found("O.I.<init>"), "a bounded abstract type member around a constructor"))
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
