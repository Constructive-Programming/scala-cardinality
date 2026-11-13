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
      a parent reached twice is read once, unless its arguments differ $twoPaths
      a member a case overrides computes from the case a sum holds    $sumOverridden
      a case's `private[p]` constructor builds every value inside `p`  $scopedCase
      a field selected on `this` alone is not observed on an argument  $thisOnly
      a bounded abstract type member is an atom with its bound's proof $boundedMember
    An overridden member whose own body always calls the measured one
      supplies nothing                                                $definite
      in a position the body always evaluates; else 0 if 0 even so  $definitePositions
      but not a local one inside a method that takes parameters       $capturedAgain
    What else a class's members supply
      an extension method; none of a one-valued result or no position $suppliedOrNot
    A parent's constructor parameters
      are what the template passes, or free public fields             $parents
    A class written in a body
      captures the parameters of the methods around it                $local
    An overridden member (a capability: ReceiversSpec, `Wide.get`)
      is not read where a behavior of it may call the measured one    $reentry
      a `new` whose class the classes read do not hold may            $unresolvedNew
      is read where no reference by name leads from it to the site  $unnamedCaller
      not where code outside the module may call the site            $outsideCaller
      nor where a function literal may implement a member it calls    $samRedeclared
      nor where an object whose parent does not resolve is accessed   $unreadObject
      is read where it re-enters only on another instance             $anotherInstance
      not where it hands on the same instance                         $sameInstance
      nor through aliases, literals, inner classes, parents or objects $sameInstanceHidden
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

  // `V = E + Two`, `Two` built and read only inside `fx` (`private[fx]`, rule B), as eo's
  // `PSVec.Slice` is: inside `fx` it holds two Booleans, so `V` is 1 + 4 and `g` one of 2^5
  // predicates. From `out` no constructor of `Two` may be called, nor `Open.a` selected, and inside
  // `Own` its `protected[this]` field is selected on `this` alone: none of them is read.
  def scopedCase = {
    val found = rows("""package fx {
      |  sealed trait V
      |  object V {
      |    case object E extends V
      |    final class Two private[fx] (private[fx] val a: Boolean, private[fx] val b: Boolean) extends V
      |  }
      |  sealed trait W
      |  final class Open(private[fx] val a: Boolean) extends W
      |  sealed trait T
      |  final class Own(protected[this] val a: Boolean) extends T { def h(t: T): Boolean = ??? }
      |  object Use { def g(v: V): Boolean = ??? }
      |}
      |package out { object Use { def f(v: fx.V): Boolean = ???; def k(w: fx.W): Boolean = ??? } }
      |""".stripMargin)
    (found("fx.Use.g") must_== Count.Finite(32))
      .and(uncovered(found("out.Use.f"), "a restricted constructor"))
      .and(uncovered(found("out.Use.k"), "a restricted field"))
      .and(uncovered(found("fx.Own.h"), "a restricted field"))
  }

  // A field private or protected to the instance is selected on `this` alone: the receiver
  // observes it (`k` is a Boolean of a receiver holding two Booleans: 2^4), an argument of the
  // class does not, unless a member it may call reads it (rule B). `m`'s argument `o` has fields
  // no member reads, which a public constructor's reading leaves uncovered (read as observed, `m`
  // was 2^16). `Q.not` is public and reads `a`: `o` observes `a`, so `n` is one of 2^(2·2).
  // `N.get` is private, which the compiler marks private to the instance (read on `this` alone),
  // yet `m` may write `o.get` (2^(2·2)); TASTy does not tell it from an explicit `private[this]`,
  // which `m` may not select (2^2), so `N.m` is uncovered.
  def thisOnly = {
    val found = rows("""final class P(a: Boolean, protected[this] val b: Boolean) {
      |  def k: Boolean = ???
      |  def m(o: P): Boolean = ???
      |}
      |final class N(a: Boolean) {
      |  private def get: Boolean = a
      |  def m(o: N): Boolean = ???
      |}
      |final class Q(a: Boolean) {
      |  def not: Boolean = !a
      |  def n(o: Q): Boolean = ???
      |}
      |""".stripMargin)
    (found("P.k") must_== Count.Finite(16))
      .and(uncovered(found("P.m"), "a parameter that is no public field"))
      .and(found("Q.n") must_== Count.Finite(16))
      .and(uncovered(found("N.m"), "a parameter that is no public field"))
  }

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
  // (its input is one value): `E[Unit]` is 2 × 2 values and `target` one of 2^4. `Mine2.neqv` calls
  // `Other.target`, a member of the same name whose calls never run `Use.target` (no instance is of
  // both objects): 2^4 still. Left: `Mine.neqv` calls `probe`; `back` passes `lift` a function
  // literal naming `target`, which whatever applies the function may run; `Mine3.neqv` gives `K` by
  // name a call of `target`, which `K.get` runs where it names its field `f`.
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
        found("""object Other { def target(i: Int): Boolean = i > 0 }
          |  object Mine2 extends E[Unit] {
          |    def eqv(x: Unit, y: Unit) = true; override def neqv(x: Unit, y: Unit) = Other.target(1)
          |  }""".stripMargin) must_== Count.Finite(16)
      )
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
      .and(
        uncovered(
          found("""final class K(f: => Boolean) { def get: Boolean = f }
          |  object Mine3 extends E[Unit] {
          |    def eqv(x: Unit, y: Unit) = true
          |    override def neqv(x: Unit, y: Unit) = new K(Use.target(this)).get
          |  }""".stripMargin),
          Receivers.Reenters
        )
      )
  }

  // `Ord.compare` overrides `Ordering.compare`, so `sorted`, code outside the module, calls it
  // where nothing of the module names it: `Srt.m` hands `Ord` to `sorted`, so it may call the site
  // on a value of `S`. `Base.compare` overrides nothing, but implements `Ordering.compare` in `X`,
  // which `Srt2.m` hands `sorted` likewise. On the site's own instance (`Kc2` overrides `m`, a
  // capability of `Kc`), `Kc.m` hands `sorted` the `Cmp` that `c` returns, whose `compare` calls
  // `get`; `Vector` is imported so that no path of a package, which the receiver reading takes for
  // anything, leaves the row first. Each row is left.
  def outsideCaller = {
    val found = rows("""import scala.collection.immutable.Vector
      |trait S { def m: Boolean = true; def n: Boolean = m }
      |object Srt extends S { override def m: Boolean = List[S](Srt).sorted(Ord).isEmpty }
      |object Ord extends Ordering[S] { def compare(a: S, b: S): Int = 0 }
      |trait S2 { def m: Boolean = true; def n: Boolean = m }
      |object Srt2 extends S2 { override def m: Boolean = List[S2](Srt2).sorted(X).isEmpty }
      |trait Base { def compare(a: S2, b: S2): Int = 0 }
      |object X extends Base with Ordering[S2]
      |class Kc { def get: Boolean = true; def c: Ordering[Int] = Cmp; def m: Boolean = Vector(2, 1).sorted(c).isEmpty }
      |class Kc2 extends Kc { override def m: Boolean = true }
      |object Cmp extends Ordering[Int] { def compare(a: Int, b: Int): Int = if (new Kc().get) 0 else 1 }
      |""".stripMargin)
    List("Ord.compare", "Base.compare", "Kc.get")
      .map(row => uncovered(found(row), Receivers.Reenters))
      .reduce(_ and _)
  }

  // `Use.reab` is given an `S`, whose `m` `Srt` overrides with a call of `A.run`, concrete there;
  // `Sam` makes it abstract again and `Mk`'s function literal implements it, calling `reab`: the row
  // is left. So is `Use.reab2`, where `Sam2 { type T = Int }`, a refinement, is implemented. `Use.keep`
  // is given a `K` whose overrider calls `B.run`; the only function literal implements `Sam`, which
  // no class of `B` is: `K.m` is a Boolean capability and `keep` one of 2^2 functions of it.
  def samRedeclared = {
    val found = rows("""object Use {
      |  def reab(s: S): Boolean = true; def reab2(s: S2): Boolean = true; def keep(k: K): Boolean = true
      |}
      |trait S { def m: Boolean = true; def n: Boolean = m }
      |trait A { def run(): Boolean = true }
      |trait Sam extends A { override def run(): Boolean }
      |final class Srt(f: A) extends S { override def m: Boolean = f.run() }
      |object Mk { def s: Srt = new Srt((() => Use.reab(s)): Sam) }
      |trait S2 { def m: Boolean = true; def n: Boolean = m }
      |trait Sam2 { type T; def run(): Boolean }
      |final class Srt2(f: Sam2) extends S2 { override def m: Boolean = f.run() }
      |object Mk2 { def s: Srt2 = new Srt2((() => Use.reab2(s)): Sam2 { type T = Int }) }
      |trait K { def m: Boolean = true; def n: Boolean = m }
      |trait B { def run(): Boolean = true }
      |final class Kb(b: B) extends K { override def m: Boolean = b.run() }
      |""".stripMargin)
    uncovered(found("Use.reab"), Receivers.Reenters)
      .and(uncovered(found("Use.reab2"), Receivers.Reenters))
      .and(found("Use.keep") must_== Count.Finite(4))
  }

  // `Srt.m` names `Init.x`, whose first access runs `Init`'s body, which calls `Use.reab` on a
  // value of `S`: the row is left. `Init` extends a class of a dependency the classes read do not
  // hold, so its linearization does not resolve: the row is still left, not lost.
  def unreadObject = {
    val parent = Programs.compiled("package dep\nclass Base")
    val code = """object Use { def reab(s: S): Boolean = true }
      |trait S { def m: Boolean = true; def n: Boolean = m }
      |object Init extends dep.Base { val x: Int = if (Use.reab(new Srt)) 1 else 0 }
      |final class Srt extends S { override def m: Boolean = Init.x == 1 }
      |""".stripMargin
    uncovered(rows(code, List(parent), read = Some(Nil))("Use.reab"), Receivers.Reenters)
  }

  // A record trait whose `neqv` a class of the program may override, and a behavior overriding it.
  private val record =
    "trait E { def eqv(a: Boolean, b: Boolean): Boolean; def neqv(a: Boolean, b: Boolean): Boolean = !eqv(a, b) }\n"

  private def neqv(body: String) =
    s"def eqv(a: Boolean, b: Boolean) = a; override def neqv(a: Boolean, b: Boolean) = $body"

  // A record value the site is given is universally quantified (the owner's ruling): `Fwd.neqv`
  // calls `same` on its field `o`, another instance, so `E` is its `eqv` and `neqv`, each one of
  // the 2^4 functions of two Booleans: `same` observes the 8 Booleans they give and is one of
  // 2^(2^8) = 2^256 functions of them.
  def anotherInstance = {
    val fwd = record + s"""final class Fwd(o: E) extends E { ${neqv("Use.same(o)")} }
      |object Use { def same(e: E): Boolean = true }
      |""".stripMargin
    val found = rows(fwd)
    // `Use.all` has no input that may hold the `Cap` the module hands to `W`, whose `go` calls it:
    // the record it returns is another instance, so `all` is one value of `E`, 2^4 * 2^4 = 2^8.
    val noInput = rows(
      record + s"""final class W(e: E) { def go: Boolean = Use.all.eqv(true, true) }
      |object Cap extends E { ${neqv("new W(Cap).go")} }
      |object Use { def all: E = new E { def eqv(a: Boolean, b: Boolean) = a } }
      |""".stripMargin
    )
    // `Mk.mk` hands a literal implementing `E`, whose body reaches `same`, to `Some`, which cannot
    // call it: `E`'s parents outside the module declare nothing `Any` does not. The value it returns
    // may reach `same` only through code of the module, and nothing in the program calls `Mk.mk` or
    // reads what it returns: `same` is 2^256 as above.
    val wrapped = rows(
      fwd + "object Mk { def mk(o: E): Option[E] = Some[E]((a, b) => Use.same(o)) }\n"
    )
    (found("Use.same") must_== Count.Finite(BigInt(2).pow(256)))
      .and(noInput("Use.all") must_== Count.Finite(BigInt(2).pow(8)))
      .and(wrapped("Use.same") must_== Count.Finite(BigInt(2).pow(256)))
  }

  // Each behavior hands the value itself on to the site: `BackV.neqv` through a local alias of
  // `this`; `SrtC.neqv` builds an `SrtC` whose `helper` passes its `this`; `Lam.neqv` gives `h` a
  // function literal returning `Lam`, which `h` applies and passes; `Cap.neqv` builds the site's
  // receiver `Home` around `this` and calls `target` on it. Each row is left.
  def sameInstance = {
    val found = rows(
      record + s"""object Use { def alias(e: E): Boolean = true; def built(e: E): Boolean = true; def lam(e: E): Boolean = true }
      |final class BackV extends E { ${neqv("{ val me: E = this; Use.alias(me) }")} }
      |final class SrtC extends E { ${neqv(
                   "new SrtC().helper"
                 )}; def helper: Boolean = Use.built(this) }
      |object Lam extends E { ${neqv("H.h(() => Lam)")} }
      |object H { def h(f: () => E): Boolean = Use.lam(f()) }
      |final class Home(b: E) { def target(x: Boolean): Boolean = b.neqv(x, x) }
      |object Cap extends E { ${neqv("new Home(this).target(a)")} }
      |""".stripMargin
    )
    List("Use.alias", "Use.built", "Use.lam", "Home.target")
      .map(row => uncovered(found(row), Receivers.Reenters))
      .reduce(_ and _)
  }

  // More ways a behavior hands the value itself to the site, each one program, each row left: a
  // module-built `Cap` given to a class whose member passes it on; a member with a parameter
  // returning `this`; a pattern variable bound to `this`; an inner class's and an inner object's
  // outer instance; a parent's constructor argument; `Cap` named (not `this`) into the site's
  // receiver `Home`, directly, through mutually recursive factories, and through a pattern; `this`
  // returned by a member outside the module (`this.type`), as an argument or a receiver; `this`
  // given to an abstract member a function literal implements; `this` or `Cap` given to a function
  // literal through code outside the module or a method of it; an abstract member an overrider
  // implements as `this`; an inner class's outer instance built around `this` or `Cap`; an inner
  // class or object whose parent's argument passes the outer `this`; a literal a method of the
  // module transforms or gives on as an `Ordering`; `this` or `Cap` given through code outside the
  // module to a function literal or a partial function that wraps it in a `Wrap` whose `go`
  // reaches the site; a site
  // without value parameters whose result is a function or a context function of an `E`, given
  // `Cap`, `this` through a member of `Cap`, or a module-built instance; `Cap` given to code
  // outside the module beside an `Ordering` wrapping a function literal, or a literal implementing
  // one, or as an `Ordering` itself, any of which that code may call with `Cap`; an inherited member
  // outside the module (`lt`, with or without `this`) calling back into `compare`, which hands on
  // the module-built `Reg.c`; an alias of `this` given to `ScalaRunTime._toString`, which calls
  // the class's own `productElement`; an alias of `this` typed `Any` given to
  // `ScalaRunTime.stringOf`, which tests it for an `Iterable` and calls its `iterator`; a member of
  // `Equals` or `Product` the class implements by handing on `this`, called on an alias typed by
  // that trait; and `this` typed by the record trait `E` (aliased, ascribed, as a bounded type
  // argument, or in a `Some`) given to `stringOf`, where a class below `E` is an `Iterable`.
  def sameInstanceHidden = {
    val use = "object Use { def same(e: E): Boolean = true }\n"
    val wrap = "final class Wrap(e: E) { def go: Boolean = Use.same(e) }"
    val all = "object Use { def all: E => Boolean = e => true }\n"
    val home = "final class Home(b: E) { def target(x: Boolean): Boolean = b.neqv(x, x) }\n"
    val outer =
      "final class Outer(e: E) { final class Inner { def target: Boolean = e.neqv(true, true) } }\n"
    def grows(body: String) =
      s"final class G extends E with scala.collection.mutable.Growable[Int] { def addOne(x: Int): this.type = this; def clear(): Unit = (); $body }"
    val sam = "trait K { def run(e: E): Boolean }\nobject Ks { val k: K = e => Use.same(e) }"
    (List(
      use -> s"final class W(e: E) { def go: Boolean = Use.same(e) }\nobject Cap extends E { ${neqv("new W(Cap).go")} }",
      use -> s"final class M extends E { def me(x: Boolean): E = this; ${neqv("Use.same(me(a))")} }",
      use -> s"final class P extends E { ${neqv("(this, a) match { case (x, _) => Use.same(x) }")} }",
      use -> s"final class O extends E { final class In { def go: Boolean = Use.same(O.this) }; ${neqv("new In().go")} }",
      use -> s"final class O extends E { object Inner { def go: Boolean = Use.same(O.this) }; ${neqv("Inner.go")} }",
      use -> s"class Base(val r: Boolean)\nfinal class W(e: E) extends Base(Use.same(e))\nfinal class X extends E { ${neqv("new W(this).r")} }",
      home -> s"object Cap extends E { ${neqv("new Home(Cap).target(a)")} }",
      home -> s"object Cap extends E { ${neqv("new Home(F.f(a)).target(a)")} }\nobject F { def f(b: Boolean): E = if (b) g(b) else Cap; def g(b: Boolean): E = f(!b) }",
      home -> s"object Cap extends E { ${neqv("(Cap: E) match { case y => new Home(y).target(a) }")} }",
      use -> grows(neqv("Use.same(this.addAll(Nil))")),
      use -> grows(neqv("Use.same(addAll(Nil))")),
      use -> grows(neqv("this.addAll(Nil).kick") + "; def kick: Boolean = Use.same(this)"),
      use -> s"$sam\nobject S extends E { ${neqv("Ks.k.run(this)")} }",
      use -> s"$sam\nfinal class S extends E { ${neqv("Ks.k.run(this)")} }\nobject Main { def go: Boolean = new S().neqv(true, true) }",
      use -> s"final class X extends E { ${neqv("List[E](this).forall(e => Use.same(e))")} }",
      use -> s"object H { def h(f: E => Boolean, x: E): Boolean = f(x) }\nfinal class X extends E { ${neqv("H.h(e => Use.same(e), this)")} }",
      use -> s"object Cap extends E { ${neqv("List[E](Cap).forall(e => Use.same(e))")} }",
      use -> s"abstract class B extends E { def me: E; ${neqv("Use.same(me)")} }\nfinal class G extends B { def me: E = this }",
      outer -> s"final class X extends E { ${neqv("{ val o = new Outer(this); new o.Inner().target }")} }",
      outer -> s"object Cap extends E { ${neqv("{ val o = new Outer(Cap); new o.Inner().target }")} }",
      use -> s"class Base(val r: Boolean)\nfinal class X extends E { final class In extends Base(Use.same(X.this)); ${neqv("new In().r")} }",
      use -> s"class Base(val r: Boolean)\nfinal class X extends E { object In extends Base(Use.same(X.this)); ${neqv("In.r")} }",
      use -> s"object H { def h(f: E => Boolean, x: E): Boolean = f.andThen(b => b)(x) }\nfinal class X extends E { ${neqv("H.h(e => Use.same(e), this)")} }",
      use -> s"object H { def h(o: Ordering[E], x: E): Boolean = List(x, x).sorted(o).isEmpty }\nfinal class X extends E { ${neqv("H.h((p, q) => if (Use.same(p)) 0 else 1, this)")} }",
      use -> s"$wrap\nfinal class X extends E { ${neqv("Some[E](this).map(e => new Wrap(e)).get.go")} }",
      use -> s"$wrap\nfinal class X extends E { ${neqv("List[E](this).map(new Wrap(_)).head.go")} }",
      use -> s"$wrap\nfinal class X extends E { ${neqv("(for (e <- Option[E](this)) yield new Wrap(e)).get.go")} }",
      use -> s"$wrap\nobject Cap extends E { ${neqv("Option[E](Cap).map(e => new Wrap(e)).get.go")} }",
      use -> s"$wrap\nfinal class X extends E { ${neqv("Some[E](this).collect { case e => new Wrap(e) }.get.go")} }",
      use -> s"$wrap\nobject Cap extends E { ${neqv("Some[E](Cap).collect { case e => new Wrap(e) }.get.go")} }",
      all -> s"object Cap extends E { ${neqv("Use.all(Cap)")} }",
      "object Use { def all: E ?=> Boolean = true }\n" -> s"object Cap extends E { ${neqv("Use.all(using Cap)")} }",
      all -> s"final class X extends E { ${neqv("Use.all(Reg.x)")} }\nobject Reg { val x: E = new X }",
      all -> s"object Cap extends E { def helper: Boolean = Use.all(this); ${neqv("helper")} }",
      use -> s"object Cap extends E { ${neqv("{ val o: Ordering[E] = Ordering.by[E, Boolean](e => Use.same(e)); Some[E](Cap).toList.sorted(o).isEmpty }")} }",
      use -> s"object Cap extends E { ${neqv("Some[E](Cap).toList.sorted((p: E, q: E) => if (Use.same(p)) 0 else 1).isEmpty")} }",
      use -> s"object Cap extends E with Ordering[E] { def compare(p: E, q: E) = if (Use.same(p)) 0 else 1; ${neqv("Some[E](Cap).toList.sorted(Cap).isEmpty")} }",
      use -> s"final class P extends E with Product { ${neqv("{ val me: P = this; scala.runtime.ScalaRunTime._toString(me).isEmpty }")}; def productArity = 1; def productElement(n: Int): Any = Use.same(this); def canEqual(x: Any) = true }",
      use -> s"final class QE extends E with Equals { ${neqv("{ val me: Equals = this; me.canEqual(1) }")}; def canEqual(x: Any) = Use.same(this) }",
      use -> s"final class QP extends E with Product { ${neqv("{ val me: Product = this; me.productArity == 1 }")}; def productArity = if (Use.same(this)) 1 else 0; def productElement(n: Int): Any = n; def canEqual(x: Any) = true }"
    ) ++ List(
      "{ val me: Any = this; scala.runtime.ScalaRunTime.stringOf(me).isEmpty }",
      "{ val me: E = this; scala.runtime.ScalaRunTime.stringOf(me).isEmpty }",
      "scala.runtime.ScalaRunTime.stringOf(this: E).isEmpty",
      "{ def h[A <: E](x: A): Boolean = scala.runtime.ScalaRunTime.stringOf(x).isEmpty; h[E](this) }",
      "{ val me: E = this; scala.runtime.ScalaRunTime.stringOf(Some[E](me)).isEmpty }"
    ).map(body =>
      use -> s"final class I extends E with Iterable[Boolean] { ${neqv(body)}; def iterator: Iterator[Boolean] = Iterator(Use.same(this)) }"
    ) ++ List("lt", "this.lt")
      .map(call =>
        use -> s"object Reg { val c: Cap = new Cap }\nfinal class Cap extends E with Ordering[Int] { def compare(p: Int, q: Int) = if (Use.same(Reg.c)) 0 else 1; ${neqv(s"$call(1, 2)")} }"
      ))
      .map { (site, program) =>
        val row =
          if (site == use) "Use.same"
          else if (site == home) "Home.target"
          else if (site.startsWith("object Use")) "Use.all"
          else "Outer.Inner.target"
        uncovered(rows(record + site + program)(row), Receivers.Reenters)
      }
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

  // An overridden member that evaluates the measured one whenever it is evaluated, with the
  // measured one's own parameters, supplies nothing. With another argument the measured body may
  // return without calling itself again, so the member is a real capability, which is left: `Arg.m`
  // passes `target(true)` by value to the final `g`, `Un.m` selects `unary_!` on it, `Outer.m` runs
  // `Inner.k`, which runs `target(true)` on the one object `Inner`, `PQ`'s inherited `neqv` runs
  // `eqv`, whose body in a `PQ` is `cmp(true)`, and `ArgF.m` is `target(true)`, which `ArgF2.m =
  // false` overrides (`target` = `b => b && !m` is total and not a function of `b` alone).
  // `Out.C.f` builds a `C` on the object `WO`, whose `k` differs, so its `m` is no call on the
  // instance: left too. `OpenGet.get` throws
  // (`Predef.???`, an object's member, calls nothing back): a free Boolean, so `m` is a function of
  // (b, get), 2^4. `C2.other` builds a `Sub2`, whose construction runs nothing: a free Boolean, so
  // `h` is 2^(2 × 2). `Rec.m` names `h` only inside `h`'s own body: `m` is a free function
  // Boolean => Boolean, and `h` a function of (y, b, m), 2^(2 × 2 × 4) = 2^16. Left: `Init.other`
  // builds an `Init`, whose construction runs the measured `x`. `Z.other` calls `target` in a
  // branch (left by itself), but even taken for a capability nothing builds a `Nothing`: 0. Not
  // definite, each left: `L.m` reads `step` without applying it; `A.m` runs `target` only where
  // `flag` holds; `F.m` runs it on another `F`'s object, `I.m` on another `I` through an import; `CP.other`
  // builds a `P`, whose constructor's `k` is `P.k`, not the `CP.k` that calls `h`.
  // `OC.f` calls `m` on the object `O`, which extends `OC` and overrides `f`: `m` runs there with
  // `O`'s members, not the instance's. `OK.f` builds a `K`, which runs no member of `K`'s
  // constructor row: its positions are inputs.
  def definitePositions = {
    val found = rows(
      """trait Arg { def target(b: Boolean): Boolean; final def g(x: Boolean): Boolean = x; def m: Boolean = g(target(true)) }
      |trait Arg2 extends Arg { override def m: Boolean = true }
      |trait Un { def target(b: Boolean): Boolean; def m: Boolean = !target(true) }
      |trait Un2 extends Un { override def m: Boolean = true }
      |class Outer { def m: Boolean = Inner.k; object Inner { def target(b: Boolean): Boolean = b; def k: Boolean = target(true) } }
      |class Outer2 extends Outer { override def m: Boolean = true }
      |trait Q { def eqv: Boolean; def neqv: Boolean = !eqv }
      |trait PQ extends Q { def cmp(b: Boolean): Boolean; def eqv: Boolean = cmp(true) }
      |trait PQ2 extends PQ { override def eqv: Boolean = true; override def neqv: Boolean = true }
      |class OpenGet { def get: Boolean = ???; def m(b: Boolean): Boolean = b }
      |class OpenGet2 extends OpenGet { override def get: Boolean = true }
      |class C2 { def h(y: Boolean): Boolean = y; def other: Boolean = new Sub2().x }
      |class Sub2 extends C2 { def x: Boolean = true }
      |class C3 extends C2 { override def other: Boolean = true }
      |class Rec { def m(b: Boolean): Boolean = { def h(y: Boolean): Boolean = if y then h(!y) else y; b } }
      |class Rec2 extends Rec { override def m(b: Boolean): Boolean = b }
      |class Init(val n: Boolean) { val x: Boolean = n; def other: Boolean = new Init(true).n }
      |class Init2 extends Init(true) { override def other: Boolean = false }
      |trait Z { def target(b: Boolean): Nothing; def other(c: Boolean): Boolean = if (c) target(c) else c }
      |trait Z2 extends Z { override def other(c: Boolean): Boolean = c }
      |trait L { def target(b: Boolean): Boolean; def step: Boolean => Boolean = x => target(x); def m: Boolean = { val f = step; true } }
      |trait L2 extends L { override def m: Boolean = false }
      |trait A { def target(b: Boolean): Boolean; def flag: Boolean; def m: Boolean = flag && target(true) }
      |trait A2 extends A { override def m: Boolean = true }
      |object Fs { val friend: F = ??? }
      |trait F { def m: Boolean = Fs.friend.Inner.k; object Inner { def target(b: Boolean): Boolean = b; def k: Boolean = target(true) } }
      |trait F2 extends F { override def m: Boolean = false }
      |object Is { val friend: I = ??? }
      |trait I { def target(b: Boolean): Boolean; def m: Boolean = { import Is.friend.{target => t}; t(true) } }
      |trait I2 extends I { override def m: Boolean = true }
      |class P(val n: Boolean) { def k: Boolean = n; def this(u: Unit) = { this(true); val _ = k } }
      |class CP extends P(true) { def h(y: Boolean): Boolean = y; override def k: Boolean = h(true); def other: Boolean = new P(()).n }
      |class CP2 extends CP { override def other: Boolean = false }
      |class OC { def f: Boolean = O.g; def m(b: Boolean): Boolean = b; object O extends OC { override def f: Boolean = true; def g: Boolean = this.m(true) } }
      |abstract class OK[A] { def a: A; def f: A = new K(a).x; class K(val x: A) }
      |class OK2[A](v: A) extends OK[A] { def a: A = v; override def f: A = v }
      |trait ArgF { def target(b: Boolean): Boolean; def m: Boolean = target(true) }
      |trait ArgF2 extends ArgF { override def m: Boolean = false }
      |class Out { def k: Boolean = true; class C { def f: Boolean = new WO.C().m; def m: Boolean = ??? }; class D extends C { override def f: Boolean = true } }
      |object WO extends Out { override def k: Boolean = false }
      |""".stripMargin
    )
    (found("OpenGet.m") must_== Count.Finite(16))
      .and(found("C2.h") must_== Count.Finite(16))
      .and(found("Rec.m.h") must_== Count.Finite(1 << 16))
      .and(uncovered(found("Init.x"), Receivers.Reenters))
      .and(found("Z.target") must_== Count.Finite(0))
      .and(
        List("Arg.target", "Un.target", "Outer.Inner.target", "PQ.cmp", "ArgF.target", "Out.C.m")
          .++(
            List(
              "L.target",
              "A.target",
              "F.Inner.target",
              "I.target",
              "CP.h",
              "OC.m",
              "OK.K.<init>"
            )
          )
          .map(row => uncovered(found(row), Receivers.Reenters))
          .reduce(_ and _)
      )
  }

  // `h` captures `b`, which a call of `m` gives anew: `if b then m(false) else true` is total in a
  // `C` (`m(false)` is the inner `h`, `true`) and in a `D` (`false`), and no function of `b` alone,
  // so `m` is no definite view of `h` and 4 would be wrong. `N.m.h` is written in a method without
  // parameters: each call of `m` gives `h` what it had, and `m` re-enters it, so `h` is one of the
  // 2^2 functions of the field `c`.
  def capturedAgain = {
    val found = rows("""class C { def m(b: Boolean): Boolean = { def h: Boolean = ???; h } }
      |class D extends C { override def m(b: Boolean): Boolean = false }
      |class N(val c: Boolean) { def m: Boolean = { def h: Boolean = ???; h } }
      |class N2 extends N(true) { override def m: Boolean = false }
      |""".stripMargin)
    uncovered(found("C.m.h"), Receivers.Reenters).and(found("N.m.h") must_== Count.Finite(4))
  }

  // `T`'s extension methods are methods of its instance, which a body in `T` may call on any
  // Boolean: `flip` computes from `b` and `choose`, a function of 2 × 2 Booleans (2^4 values), so
  // over 2 × 2^4 points, 2^32; `choose` from `b`, `x` and `flip` (2^2 values), 2^(2 × 2 × 4) =
  // 2^16. `collect` returns `Unit`, so whatever it calls, and whatever its inputs (a
  // `StringBuilder` is not read), it supplies nothing: `to` and `from` have no S or A to start
  // from, 0. `H`'s constructor takes no parameters: one value, though `m`
  // builds an `H` that calls `target`.
  def suppliedOrNot = {
    val found = rows("""trait T { extension (b: Boolean) { def flip: Boolean; def choose(x: Boolean): Boolean } }
      |trait MF[S, A] {
      |  def to(s: S): A
      |  def from(a: A): S
      |  def collect(s: S, b: StringBuilder): Unit = { val a = to(s); () }
      |}
      |class MF2 extends MF[Int, Int] {
      |  def to(s: Int): Int = s; def from(a: Int): Int = a
      |  override def collect(s: Int, b: StringBuilder): Unit = ()
      |}
      |trait TH { def target(b: Boolean): Boolean; class H { def run: Boolean = target(true) }; def m: Boolean = new H().run }
      |trait TH2 extends TH { override def m: Boolean = false }
      |""".stripMargin)
    (found("T.<extension@1>.flip") must_== Count.Finite(1L << 32))
      .and(found("T.<extension@1>.choose") must_== Count.Finite(1 << 16))
      .and(found("MF.to") must_== Count.Finite(0))
      .and(found("MF.from") must_== Count.Finite(0))
      .and(found("TH.H.<init>") must_== Count.Finite(1))
  }

  // A class written in a method captures its parameters, a type parameter's values among them,
  // and may branch on the captured b. `Box`'s x is x or a at each b, 2^2 = 4; `Two`'s x and y each
  // x, y or a at each b, (3^2)^2 = 81. `H.m` returns c or a at each b (`seed` is final and computes
  // a): 2^2 = 4. `In` is a case class of one Boolean, which captures
  // nothing it shows, so `Out` is 2. In `C`, `import Imp.seed` makes the captured seed ambiguous
  // (unnameable) and nothing else holds an X: `g` is 0. An import after the class, a wildcard over
  // a private member, or one that excludes the name binds nothing of it: each `g` returns the
  // captured X, 1. The anonymous `Q`'s inherited field n hides
  // the captured n, but `Q(n)` passes it to that public field: `inh`'s m is one of the 2^(2 × 2)
  // functions of (b, n). `Q(true)` holds nothing of it: `lit`'s m is one of 2^2. Where the field is
  // overridden (`R`), the parent called is a secondary constructor (`S`), or another template is
  // nearer than the parent (`Inner`), the captured value the parent holds is not read; nor is an abstract type member of a local
  // trait, which only the body may implement. An import of a type named like the capture binds no
  // term: `typed`'s g returns the captured seed, 1, and `mk`'s `D` takes x or seed, 2. Inside `Hid2`
  // its wildcard imports the private hid, which makes the captured one ambiguous: 0; so does
  // `import Imp.*` the public seed (`all`).
  def local = {
    val found = rows(
      """trait P[A] { def seed: A }
      |object Imp { val seed: Int = 0 }
      |class Q(val n: Boolean) { def m(b: Boolean): Boolean = b }
      |class R(val n: Boolean) { def m(b: Boolean): Boolean = b }
      |class S(val n: Boolean) { def this(n: Boolean, z: Int) = this(true); def m(b: Boolean): Boolean = b }
      |object Hid { private val hid: Int = 1; val other: Int = 2 }
      |object L {
      |  def f[A](a: A, b: Boolean): Int = {
      |    final case class Box(x: A)
      |    final class Two(x: A, y: A)
      |    final class H extends P[A] { def seed: A = a; def m(c: A): A = ??? }
      |    0
      |  }
      |  def g(n: Boolean): Int = { final case class In(b: Boolean); final case class Out(i: In); 0 }
      |  def run[X](seed: X): Int = { final class C { import Imp.seed; def g(): X = ??? }; 0 }
      |  def after[X](seed: X): Int = { final class C { def g(): X = ??? }; import Imp.seed; 0 }
      |  def hid[X](hid: X): Int = { final class C { import Hid.*; def g(): X = ??? }; 0 }
      |  def but[X](seed: X): Int = { final class C { import Imp.{seed as _, *}; def g(): X = ??? }; 0 }
      |  def sec(n: Boolean): S = new S(n, 0) { override def m(b: Boolean): Boolean = !b }
      |  def inner(n: Boolean): Q = new Q(n) {
      |    def k(): Int = { final class Inner extends Q(false) { def g(b: Boolean): Boolean = ??? }; 0 }
      |  }
      |  def inh(n: Boolean): Q = new Q(n) { override def m(b: Boolean): Boolean = !b }
      |  def lit(n: Boolean): Q = new Q(true) { override def m(b: Boolean): Boolean = b }
      |  def over(n: Boolean): R = new R(n) { override val n: Boolean = true; override def m(b: Boolean): Boolean = !b }
      |  def loc(): Int = { trait T { type X; def k(x: X, y: X): X }; 0 }
      |  def typed[X](seed: X): Int = { final class C { import ImpT.seed; def g(): X = ??? }; 0 }
      |  def mk[X](seed: X): Int = { import ImpT.seed; final class D(x: X); 0 }
      |  def all[X](seed: X): Int = { final class C { import Imp.*; def g(): X = ??? }; 0 }
      |}
      |object ImpT { type seed = Int }
      |object Hid2 {
      |  private val hid: Int = 1
      |  def run[X](hid: X): Int = { final class C { import Hid2.*; def g(): X = ??? }; 0 }
      |}
      |""".stripMargin
    )
    List(
      "L.f.Box.<init>" -> 4,
      "L.f.Two.<init>" -> 81,
      "L.f.H.m" -> 4,
      "L.g.Out.<init>" -> 2,
      "L.run.C.g" -> 0,
      "L.after.C.g" -> 1,
      "L.hid.C.g" -> 1,
      "L.but.C.g" -> 1,
      "L.inh.<new#1>.m" -> 16,
      "L.lit.<new#1>.m" -> 4,
      "L.typed.C.g" -> 1,
      "L.mk.D.<init>" -> 2,
      "Hid2.run.C.g" -> 0,
      "L.all.C.g" -> 0
    ).map((row, n) => found(row) must_== Count.Finite(n))
      .reduce(_ and _)
      .and(
        List("L.over.<new#1>.m", "L.sec.<new#1>.m", "L.inner.<new#1>.k.Inner.g")
          .map(row => uncovered(found(row), "held by other code"))
          .reduce(_ and _)
      )
      .and(found("L.loc.T.k") must beLike {
        case Count.Unresolved(List(r)) => r must contain("is local")
      })
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

  // `D` reaches `A` through `B` and through `C`, each with no type argument: one parent, so `D.g`
  // has `a`, one of 2^2, and so has `E.h` (`E` extends `D`, whose `g` nothing overrides). `GD`
  // reaches the generic `GA` through `GB[X]` and `GC[X]`, the same `GA[X]` on both paths: one
  // parent, and `g: X` can only return `a`, 1. `CS` reaches `CP` as a `CP[Boolean]` and as a
  // `CP[Any]`: not read.
  def twoPaths = {
    val found = rows("""trait A { def a: Boolean }
      |trait B extends A
      |trait C extends A
      |trait D extends B with C { def g: Boolean = a }
      |trait E extends D { def h: Boolean = a }
      |trait GA[X] { def a: X }
      |trait GB[X] extends GA[X]
      |trait GC[X] extends GA[X]
      |trait GD[X] extends GB[X] with GC[X] { def g: X = a }
      |trait CP[+X] { def p: X }
      |trait CQ extends CP[Boolean]
      |trait CR extends CP[Any]
      |trait CS extends CQ with CR { def s: Boolean = true }
      |""".stripMargin)
    (List("D.g", "E.h", "GD.g").map(found) must_== List(
      Count.Finite(4),
      Count.Finite(4),
      Count.Finite(1)
    )).and(
      uncovered(found("CS.s"), "reaches a parent through two parents with other type arguments")
    )
  }

}
