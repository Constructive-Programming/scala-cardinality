package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import cardinality.capacity.{ByteSize, EffectiveOmega}
import org.specs2.Specification
import org.specs2.execute.Result

/** The environments the closed split reads: every closed value it splits must be one supplied
  * value, and every builtin name it reads must be the builtin. Each fixture compiles with Scala
  * 3.9.0, except `wildcard` and `foreignWildcard`, which assume an unsupplied `org.foo`, and
  * `moreChannels`' `bp`, which assumes an unsupplied `nowhere`.
  */
class GroundEnvironmentSpec extends Specification {
  import Inhabitation.Count
  import NaturalSyntax.pow

  def is = s2"""
    Inherited closed declarations
      a fixed receiver implements what it inherits                   $fixedReceiver
      a field is the member it implements                            $fieldAlias
      one member reached along several parents is one value          $restated
      an open template's overridable implementation is a capability  $openImplemented
      an overload is a second member; another shape is refused      $overloaded
      an unimplemented declaration of an open template is supplied   $supplied
      nested templates sharing a parent judge it for each receiver   $nestedReceivers
      an open declaration reached from two parents is one member     $openRestated
      a plain constructor parameter is not the member it is named as $plainParameter
    Enum and sealed receivers
      a member reading the receiver is refused                       $receiverRefused
      an uninhabited enum's member is not ω                          $uninhabited
      terminal and String goals of an inhabited enum keep their count $receiverCertified
    Shadowed builtin names
      a member type named like a builtin is not the builtin          $inheritedBuiltin
      a self type's members shadow the builtin                       $selfTypes
      a wildcard-imported object's inherited members bind it         $importedInherited
      a package object's parents shadow it in its package            $packageObject
      a parent named through an import declares the member           $importedParents
      a scala.Dynamic parent supplies nothing the count reads        $dynamicParent
      a nested type of the parent's name does not doubt it           $nestedParentName
      exports bind it where they forward                             $exports
      an enum's cases shadow it after a wildcard import              $enumCases
      a wildcard import of a supplied value is refused               $valueImport
      a name no builtin spells keeps lookup's own reason             $notBuiltin
      aliases, enum bodies, explicit exports and unread steps        $moreChannels
      a constructor position reads the inherited member on both sides $shadowedPosition
      constructor parameters do not see the template's members       $constructorScope
      a foreign wildcard refuses a builtin beside a supplied one     $foreignWildcard
    Name precedence
      chained package clauses read the innermost package first       $chainedPackages
      an explicit import wins over another file's package member     $explicitImport
    Constructor readings
      an unreadable infinite position takes the size algebra's tier   $fallbackTier
      a representation budget breach names its limit                 $budget
      floating point positions count equals classes exactly          $floats
      an enclosing template's parameters are captures                $outerCaptures
      a singleton beside a value takes the estimate's reading        $singletonPositions
      a symbolic value space agrees only with a wide estimate        $symbolicDisagreement
    Anonymous evidence
      two anonymous evidences of one ground type are unreachable     $anonymousGround
    Pinned limits
      ground evidence endpoints keep the evidence reason             $evidence
      an empty-domain function position has two conventions          $emptyDomain
      an unsupplied wildcard import refuses the builtin              $wildcard
  """

  private def all(sources: (String, String)*) =
    MethodAnalysis.analyze(sources.toList.map { (path, text) =>
      MethodAnalysis.Input(path, dialects.Scala3(text).parse[Source].get)
    })

  private def counts(sources: (String, String)*): Map[String, Count] =
    all(sources*).map(entry => entry.name -> entry.count).toMap

  private def count(code: String, name: String): Count = counts("Example.scala" -> code)(name)

  private def unresolvedOn(c: Count, text: String): Result =
    c must beLike { case Count.Unresolved(rs) => rs.exists(_.contains(text)) must beTrue }

  private val intPredicate = Count.Symbolic(pow(2, pow(2, 32)))

  def fixedReceiver =
    (count(
      "trait T { def n: Boolean }\nobject O extends T { def n: Boolean = true; def f: Boolean = false }",
      "O.f"
    ) === Count.Finite(2))
      .and(
        count(
          "trait T { def n: Int }\nfinal class C extends T { val n: Int = 3; def f: Boolean = false }",
          "C.f"
        ) === Count.Finite(2)
      )
      .and(
        count(
          "abstract class P { def pick: Boolean }\n" +
            "final class Q extends P { def pick: Boolean = true; def other: Boolean = true }",
          "Q.other"
        ) === Count.Finite(2)
      )
      .and(
        count(
          "trait T { def g(x: Int): Int }\nobject O extends T { def g(x: Int): Int = x; def f: Boolean = true }",
          "O.f"
        ) === Count.Finite(2)
      )

  // `HasB.b` and the field are one value: f ranges over functions of C(true) / C(false).
  def fieldAlias =
    (count(
      "trait HasB { def b: Boolean }\nfinal case class C(b: Boolean) extends HasB { def f: Boolean = true }",
      "C.f"
    ) === Count.Finite(4))
      .and(
        count(
          "trait HasN { def n: Int }\nfinal case class C(n: Int) extends HasN { def f: Boolean = true }",
          "C.f"
        ) === intPredicate
      )
      .and(
        count(
          "abstract class P { val n: Int }\nfinal class C(val n: Int) extends P { def f: Boolean = true }",
          "C.f"
        ) === intPredicate
      )

  def restated =
    (count(
      "trait A { def n: Boolean }\ntrait B { def n: Boolean }\ntrait C extends A with B { def f: Boolean = true }",
      "C.f"
    ) === Count.Finite(4))
      .and(
        count(
          "trait A { def n: Boolean }\ntrait B extends A { def n: Boolean }\n" +
            "trait C extends B { def f: Boolean = true }",
          "C.f"
        ) === Count.Finite(4)
      )
      .and(
        count(
          "trait A { def n: Boolean }\ntrait B extends A { def n: Boolean; def f: Boolean = true }",
          "B.f"
        ) === Count.Finite(4)
      )
      .and(
        count(
          "trait A { val n: Boolean }\ntrait B extends A { val n: Boolean }\n" +
            "trait C extends B { def f: Boolean = n }",
          "C.f"
        ) === Count.Finite(4)
      )

  // A subclass of the open Q may override pick with either literal (capabilities C5), so pick is a
  // supplied Boolean: other is true, false, pick or !pick — 4.
  def openImplemented =
    count(
      "abstract class P { def pick: Boolean }\n" +
        "class Q extends P { def pick: Boolean = true; def other: Boolean = true }",
      "Q.other"
    ) === Count.Finite(4)

  // Two overloads are two supplied functions: 2^(|Int => Int| · |Long => Int|) = 2^(2^(2^37 + 2^69)).
  // A parameterless `def` and a `val` of one name and type are one member: n, !n, true, false — 4.
  // Read at another type (`Any` against `Boolean`) the one member's readings differ: refused.
  def overloaded =
    (count(
      "trait A { def g(x: Int): Int }\ntrait B { def g(x: Long): Int }\n" +
        "trait C extends A with B { def f: Boolean = true }",
      "C.f"
    ) === Count.Symbolic(pow(2, pow(2, BigInt(2).pow(37) + BigInt(2).pow(69)))))
      .and(
        count(
          "trait A { def n: Boolean }\ntrait B { val n: Boolean }\n" +
            "trait C extends A with B { def f: Boolean = n }",
          "C.f"
        ) === Count.Finite(4)
      )
      .and(
        unresolvedOn(
          count(
            "trait A { def n: Any }\ntrait B { val n: Boolean }\n" +
              "trait C extends A with B { def f: Boolean = n }",
            "C.f"
          ),
          "inherited declaration identity not resolved: n"
        )
      )

  // Two receivers, two members: Outer.this.k and Inner.this.n are independent, 2^(2·2).
  def supplied =
    (count("trait A { def n: Boolean }\ntrait C extends A { def f: Boolean = true }", "C.f") ===
      Count.Finite(4))
      .and(
        count(
          "trait A { def n: Boolean }\n" +
            "trait Outer { def k: Boolean; trait Inner extends A { def f: Boolean = true } }",
          "Outer.Inner.f"
        ) === Count.Finite(16)
      )

  // `Outer.this.n` and `Inner.this.n` are two members, each its own template's slot. A fixed outer
  // one leaves the inner one (4, not 2); two free ones are 2^(2·2) = 16; a generic parent is read
  // with each template's own arguments (Inner.this.n: Boolean, 4; not 2^(2^32)).
  def nestedReceivers = {
    def inner(code: String, pkg: String) =
      counts("Example.scala" -> s"package $pkg\n$code")(s"$pkg.Outer.Inner.f")
    (inner(
      "trait A { def n: Boolean }\n" +
        "object Outer extends A { def n: Boolean = true; trait Inner extends A { def f: Boolean = true } }",
      "n1"
    ) === Count.Finite(4))
      .and(
        inner(
          "trait A { def n: Boolean }\n" +
            "trait Outer extends A { trait Inner extends A { def f: Boolean = true } }",
          "n3"
        ) === Count.Finite(16)
      )
      .and(
        inner(
          "trait Outer { def n: Boolean; trait Inner extends Outer { def f: Boolean = true } }",
          "n5"
        ) === Count.Finite(16)
      )
      .and(
        inner(
          "trait A[X] { def n: X }\nobject Outer extends A[Int] {\n  def n: Int = 3\n" +
            "  trait Inner extends A[Boolean] { def f: Boolean = true }\n}",
          "n6"
        ) === Count.Finite(4)
      )
      .and(
        inner(
          "trait A[X] { def n: X }\ntrait Outer extends A[Int] {\n" +
            "  final class Inner extends A[Boolean] { def n: Boolean = true; def f: Boolean = true }\n}",
          "n7"
        ) === intPredicate
      )
  }

  // True counts (2^32)^(2^32 + 1) for E.f and 4 for Color.f: a body matches on `this`.
  def receiverRefused = {
    val reason = "receiver is a sealed family this measurement does not split: "
    unresolvedOn(count("enum E { case A(x: Int); case B; def f: Int = 0 }", "E.f"), reason)
      .and(
        unresolvedOn(
          count(
            "enum Color(val rgb: Int) { case Red extends Color(1); case Green extends Color(2); " +
              "def f: Boolean = true }",
            "Color.f"
          ),
          reason
        )
      )
      .and(
        unresolvedOn(
          count(
            "sealed trait S { def h: Int = 0 }\nfinal case class SA(x: Int) extends S\ncase object SB extends S",
            "S.h"
          ),
          reason
        )
      )
      .and(
        unresolvedOn(
          count("enum Gen[A] { case Has(a: A); def show: String = \"\" }", "Gen.show"),
          reason
        )
      )
      .and(
        count(
          "sealed trait S\nfinal case class SA(x: Int) extends S { def k: Boolean = true }",
          "SA.k"
        ) === intPredicate
      )
  }

  // No E exists, so the true count is 1; a free reading would say ω and 2^32.
  def uninhabited = {
    val rows = counts(
      "Example.scala" -> "enum E { case A(n: Nothing); def m: String = \"\"; def k: Int = 0 }"
    )
    val reason = "receiver is a sealed family this measurement does not split: E"
    unresolvedOn(rows("E.m"), reason).and(unresolvedOn(rows("E.k"), reason))
  }

  def receiverCertified = {
    val rows = counts(
      "Example.scala" ->
        "enum E { case A(x: Int); case B; def name: String = \"\"; def unit(i: Int): Unit = () }"
    )
    (rows("E.name") === Count.Countable).and(rows("E.unit") === Count.Finite(1))
  }

  def inheritedBuiltin = {
    val rows = counts(
      "Example.scala" ->
        ("class Base { type Int = Boolean }\nfinal class Sub extends Base { def f(x: Int): Int = x }\n" +
          "class Mid extends Base\ntrait Deep extends Mid\n" +
          "final class Leaf extends Deep { def k(x: Int): Int = x; def h(x: Boolean): Boolean = x }\n" +
          "final class Other { def g(x: Int): Int = x }")
    )
    (rows("Sub.f") === Count.Finite(4))
      .and(rows("Leaf.k") === Count.Finite(4))
      .and(rows("Leaf.h") === Count.Finite(4))
      .and(rows("Other.g") === Count.Symbolic(pow(2, pow(2, 37))))
  }

  // In every fixture below `Int` (or `Boolean`) is the supplied member, so `f: Boolean => Boolean`
  // has 4 implementations (`Unit => Unit`: 1); the builtin reading claimed 2^(2^37) (or 4).
  private val base = "trait Base { type Int = Boolean }\n"

  private def shadowed(rows: Map[String, Count], row: String, by: String): Result =
    unresolvedOn(rows(row), s"builtin name Int may be shadowed by a declaration: $by")

  def selfTypes = {
    val rows = counts(
      "a.scala" -> s"package w3\n${base}trait Sub { self: Base => def f(x: Int): Int }",
      "b.scala" ->
        s"package st\n${base}trait Other\ntrait Sub { self: Other & Base => def f(x: Int): Int }"
    )
    shadowed(rows, "w3.Sub.f", "w3.Base.Int (self type of w3.Sub)")
      .and(shadowed(rows, "st.Sub.f", "st.Base.Int (self type of st.Sub)"))
  }

  // `import O.*` brings the members `O` inherits: `Int` is `Base.Int = Boolean`, so each `f` is a
  // Boolean endomorphism (4), and `w8b`'s `Boolean` is `Unit` (1).
  def importedInherited = {
    val rows = counts(
      "a.scala" -> s"package w8\n${base}object O extends Base\nimport O.*\ndef f(x: Int): Int = x",
      "b.scala" ->
        (s"package a2\n${base}object O extends Base\nobject Q { import O.*; type Age = Int }\n" +
          "def f(a: Q.Age): Q.Age = a"),
      "c.scala" ->
        ("package w8b\ntrait Base { type Boolean = Unit }\nobject O extends Base\nimport O.*\n" +
          "def f(x: Boolean): Boolean = x"),
      "d.scala" -> "package zz\nimport _root_.w8.O.*\ndef f(x: Int): Int = x"
    )
    (rows("w8.f") === Count.Finite(4))
      .and(rows("a2.f") === Count.Finite(4))
      .and(rows("zz.f") === Count.Finite(4))
      .and(rows("w8b.f") === Count.Finite(1))
  }

  // A `scala.Dynamic` parent is the structural-member marker: it declares no member a count
  // reads, and the structural calls its subclasses enable (selectDynamic, …) are outside the
  // counting model - the run logs the notice. The builtin beside it is not shadowed, and the
  // record of the receiver is its declared members alone.
  def dynamicParent = {
    val rows = counts(
      "d.scala" -> "trait Rec extends Dynamic\ndef f(r: Rec, b: Boolean): Boolean = ???"
    )
    rows("f") === Count.Finite(4)
  }

  // A bare parent name found in no scope is the stdlib's. A supplied type of the same simple name
  // that lookup does not reach never rebinds it (the never-shadowed assumption): neither one nested
  // in an object (izumi's `CollectionCompat.IterableOnce`) nor another package's top-level alias,
  // which Scala 3.9.0 does not make visible in `pv` either. `length` is then any Int (2^32).
  def nestedParentName = {
    val parent = "sealed trait V[+B] extends IterableOnce[B]\n" +
      "object V { case object E extends V[Nothing] { def length: Int = 0 } }"
    val nested = counts(
      "lib.scala" -> "package nlib\nobject Compat { type IterableOnce[A] = List[A] }",
      "v.scala" -> s"package nv\n$parent"
    )
    val packaged = counts(
      "lib.scala" -> "package plib\ntype IterableOnce[A] = List[A]",
      "v.scala" -> s"package pv\n$parent"
    )
    (nested("nv.V.E.length") === Count.Finite(BigInt(2).pow(32)))
      .and(packaged("pv.V.E.length") === Count.Finite(BigInt(2).pow(32)))
  }

  def packageObject = {
    val rows = counts(
      "a.scala" -> "package object w10 extends w10.Base",
      "b.scala" -> s"package w10\n$base",
      "c.scala" -> "package w10\ndef f(x: Int): Int = x"
    )
    shadowed(rows, "w10.f", "w10.Base.Int (parents of package object w10)")
  }

  def importedParents = {
    val defs = s"object Defs { $base}\n"
    val rows = counts(
      "a.scala" ->
        s"package w14\n${defs}import Defs.{Base => B}\nfinal class Sub extends B { def f(x: Int): Int = x }",
      "b.scala" ->
        s"package w15\n${defs}import Defs.*\nfinal class Sub extends Base { def f(x: Int): Int = x }"
    )
    (rows("w14.Sub.f") === Count.Finite(4)).and(rows("w15.Sub.f") === Count.Finite(4))
  }

  def exports = {
    val o = "object O { type Int = Boolean }\n"
    val rows = counts(
      "a.scala" -> s"package w17\n${o}object P { export O.* }\nimport P.*\ndef f(x: Int): Int = x",
      "b.scala" -> s"package w18\n${o}export O.*",
      "c.scala" -> "package w18\ndef f(x: Int): Int = x",
      "d.scala" -> s"package w19\n${o}object Q { export O.*; def f(x: Int): Int = x }",
      "e.scala" ->
        ("package er\nobject O { type B = Boolean }\nobject P { export O.{B => Int} }\n" +
          "import P.*\ndef f(x: Int): Int = x")
    )
    // An export defines an alias of `O.Int = Boolean` where it stands (4); a package-level export
    // of another file is a member the lookup does not follow, so the builtin is refused there.
    (rows("w17.f") === Count.Finite(4))
      .and(shadowed(rows, "w18.f", "w18.O.Int (export of package w18)"))
      .and(rows("w19.Q.f") === Count.Finite(4))
      .and(rows("er.f") === Count.Finite(4))
  }

  // Scala 3.9.0 puts a class case of an enum behind `import E.*` (and inside the enum's body): the
  // lookup finds the case there, a type it does not read, before the builtin is consulted.
  def enumCases =
    unresolvedOn(
      count(
        "package en2\nenum E { case Int(b: Boolean); case K }\nimport E.*\ndef g(x: Int): Int = x",
        "en2.g"
      ),
      "enum case type not read: en2.E.Int"
    )

  def valueImport =
    unresolvedOn(
      count(
        s"package vi\n${base}object Z { val o: Base = new Base {}; import o.*; def f(x: Int): Int = x }",
        "vi.Z.f"
      ),
      "path through a value not followed: o"
    )

  // An enum's class case is in scope in its body; an explicit export forwards the member (`ex.f`
  // reads `O.Int = Boolean`: 4); a refined self type is a step the walk cannot follow, which no
  // longer refuses the builtin (the never-shadowed assumption) but is refused by the
  // inherited-member reading, which does not read the refinement either. A parent the
  // inherited-member reading does not have — an alias of the parent, or an import outside the
  // sources (`bp` assumes an unsupplied `nowhere`) — may declare `Int`, and is refused there before
  // any builtin reading (true counts 4).
  def moreChannels = {
    val rows = counts(
      "ap.scala" -> s"package ap\n${base}type B = Base\nfinal class Sub extends B { def f(x: Int): Int = x }",
      "eb.scala" -> "package eb\nenum E { case Int(b: Boolean); case K; def f(x: Int): Int = x }",
      "ex.scala" ->
        "package ex\nobject O { type Int = Boolean }\nobject P { export O.Int }\nimport P.*\ndef f(x: Int): Int = x",
      "rs.scala" ->
        s"package rs\n${base}trait Sub { self: Base { type X = Unit } => def f(x: Int): Int }",
      "bp.scala" ->
        s"package bp\n${base}import nowhere.Base\nfinal class Sub extends Base { def f(x: Int): Int = x }"
    )
    unresolvedOn(rows("ap.Sub.f"), "inherited type member not resolved: Int (unsupplied parent B)")
      .and(shadowed(rows, "eb.E.f", "eb.E.Int (case of eb.E)"))
      .and(rows("ex.f") === Count.Finite(4))
      .and(
        unresolvedOn(
          rows("rs.Sub.f"),
          "self type not resolved: Base { type X = Unit }"
        )
      )
      .and(
        unresolvedOn(
          rows("bp.Sub.f"),
          "inherited type member not resolved: Int (unsupplied parent Base)"
        )
      )
  }

  // `Frame` is no builtin: a wildcard import through the value `o` may bind it, and lookup, which
  // reads no value's members, refuses there with its own reason.
  def notBuiltin =
    counts(
      "a.scala" -> "package fr\nclass Frame(b: Boolean)",
      "b.scala" ->
        s"package q\n${base}object Z { val o: Base = new Base {}; import o.*; def f(x: Frame): Frame = x }"
    )("q.Z.f") === Count.Unresolved(List("path through a value not followed: o"))

  // `Int` is `Long` inside `O`, so P's value space is 2^64: the method side reads the inherited
  // member, and so does the estimate it is checked against (register D29 — it used to read the
  // builtin's 2^32, and the disagreement was refused). Through `import O.*` the estimate refuses the
  // name itself: a wildcard over an object with parents may bind a member it does not read.
  def shadowedPosition = {
    val base = "package w12\ntrait Base { type Int = Long }\n"
    (counts("a.scala" -> s"${base}object O extends Base { final case class P(x: Int) }")(
      "w12.O.P.<init>"
    ) === Count.Finite(BigInt(2).pow(64))).and(
      unresolvedOn(
        counts(
          "a.scala" -> s"${base}object O extends Base\nimport O.*\nfinal case class P(x: Int)"
        )(
          "w12.P.<init>"
        ),
        "unresolved type: Int (may be imported by O.*, an inherited member not followed)"
      )
    )
  }

  // `new Sub(3)` compiles: a primary constructor's parameters are read outside the template's
  // members (2^32). A case class nested in a template that inherits `Int` reads the member on both
  // sides: `Base.Int = Boolean`, so 2 (the estimate read the builtin's 2^32 before register D29,
  // and the disagreement was refused).
  def constructorScope = {
    val rows = counts(
      "a.scala" ->
        s"package cp\n${base}class Sub(val x: Int) extends Base\nobject O extends Base { final case class P(x: Int) }"
    )
    (rows("cp.Sub.<init>") === Count.Finite(BigInt(2).pow(32)))
      .and(rows("cp.O.P.<init>") === Count.Finite(2))
  }

  // A namespace no supplied term heads is foreign. A wildcard import outranks the root import, but
  // one over a namespace the sources do not supply never rebinds a builtin name (the
  // never-shadowed assumption), and the supplied `Base.Int` is not in scope in `C`: `Int` is
  // scala.Int and f picks a or b per Int, 2^(2^32).
  def foreignWildcard =
    count(
      s"package fw\n${base}import org.foo.*\nobject C { def f[X](x: Int, a: X, b: X): X = a }",
      "fw.C.f"
    ) === intPredicate

  // C has one `g`: two abstract members with the same signature merge (f = g ∘ h, h: Bool => Int,
  // so |Int|^|Bool| = 2^64, not (2·2^32)^2). Overloads stay two members. A restatement two levels
  // up is read under the whole chain's substitution (A's X is C's X), so it is the same member.
  def openRestated = {
    def c(code: String) =
      count(code + "\ntrait C[X] extends A[X], B[X] { def f(x: Boolean): X }", "C.f")
    (c("trait A[X] { def g(x: Int): X }\ntrait B[X] { def g(x: Int): X }") ===
      Count.Finite(BigInt(2).pow(64)))
      .and(
        c("trait A[X] { def g(x: Boolean): X }\ntrait B[X] { def g(x: Boolean): X }") === Count
          .Finite(4)
      )
      .and(
        c("trait A[X] { def g(x: Boolean): X }\ntrait B[X] { def g(x: Unit): X }") ===
          Count.Finite(9)
      )
      .and(
        count(
          "trait A[X] { def g(x: Int): X }\ntrait B[X] extends A[X] { def g(x: Int): X }\n" +
            "trait C[X] extends B[X] { def f(x: Boolean): X }",
          "C.f"
        ) === Count.Finite(BigInt(2).pow(64))
      )
  }

  // `get: S => A` is a parameter, not a member, so it is no restatement of the inherited `get`
  // (directly, or through an intermediate class as eo's `SimpleLens`); no S is built: `s => s`.
  def plainParameter = {
    val rows = counts(
      "Example.scala" ->
        ("package sl\ntrait CanGet[S, A] { def get(s: S): A }\n" +
          "class SCL[S, A](read: S => A) extends CanGet[S, A] { def get(s: S): A = read(s) }\n" +
          "final class SL[S, A](get: S => A) extends SCL[S, A](get) { def place(a: A): S => S = s => s }\n" +
          "abstract class P[S, A] { def get(s: S): A }\n" +
          "abstract class D[S, A](get: S => A) extends P[S, A] { def place(a: A): S => S = s => s }")
    )
    (rows("sl.SL.place") === Count.Finite(1)).and(rows("sl.D.place") === Count.Finite(1))
  }

  def chainedPackages = {
    val rows = counts(
      "f1.scala" -> "package p\nfinal case class Inner(b: Byte)",
      "f2.scala" -> "package p.q\nfinal case class Inner(b: Boolean)",
      "f3.scala" -> "package p\npackage r\nfinal case class Outer2(i: Inner)",
      "f4.scala" -> "package p\npackage q\nfinal case class Outer4(i: Inner)\ndef f(i: Inner): Boolean = true"
    )
    (rows("p.q.Outer4.<init>") === Count.Finite(2))
      .and(rows("p.q.f") === Count.Finite(4))
      .and(rows("p.r.Outer2.<init>") === Count.Finite(256))
  }

  def explicitImport = {
    val rows = counts(
      "a.scala" -> "package x\ntype Inner = Byte",
      "b.scala" -> "package y\nfinal case class Inner(s: String)",
      "c.scala" -> "package x\nimport y.Inner\ndef f(i: Inner): Boolean = true\nfinal case class Outer(i: Inner)",
      "d.scala" -> "package xx\ntype Inner = Unit",
      "e.scala" -> "package yy\nfinal case class Inner(b: Boolean)",
      "f.scala" -> "package xx\nimport yy.Inner\ndef f(i: Inner): Boolean = true"
    )
    // Both readings follow the import to `y.Inner(s: String)`, ω, over package `x`'s `Inner`
    // (2^8): an explicit import outranks another file's package member.
    (rows("x.f") === Count.Countable)
      .and(rows("x.Outer.<init>") === Count.Bounded(EffectiveOmega))
      .and(rows("xx.f") === Count.Finite(4))
  }

  // Package `a`'s object `x` and package `a.b`'s `val x: Foo`: inside `package a; package b` the
  // nearer `val` is `x` (scalac reads `x.T` as Foo's).
  private val valPath = List(
    "A.scala" -> "package a\nobject x { type T = (Int => Int) => Int; type S = String }",
    "B.scala" -> "package a.b\nfinal class Foo { type T = Byte; type S = Byte }\nval x: Foo = new Foo",
    "C.scala" ->
      "package a\npackage b\nfinal case class UsesT(t: x.T)\nfinal case class UsesS(s: x.S)"
  )

  // Two packages declaring `C` keep the estimate from reading the positions; the answer is the one a
  // single package gets: ω^ω is ε₀ in the size algebra, 2^ω is ω.
  def fallbackTier = {
    def pkg(name: String) =
      s"package $name\nfinal case class C(n: String)\nfinal case class R(run: C => String)\n" +
        "final case class R4(run: C => C)\nfinal case class B(p: C => Boolean)"
    val colliding = counts("ka.scala" -> pkg("ka"), "kb.scala" -> pkg("kb"))
    val single = counts("kc.scala" -> pkg("kc"))
    List("R", "R4", "B")
      .map(n => colliding(s"ka.$n.<init>").render === single(s"kc.$n.<init>").render)
      .reduce(_.and(_))
      .and(colliding("ka.R.<init>").render === "ε₀")
      .and(colliding("ka.R4.<init>").render === "ε₀")
      .and(colliding("ka.B.<init>").render === "ω")
  }

  def budget =
    unresolvedOn(
      count(
        "case class T(f: ((((((((((Int => Int) => Int) => Int) => Int) => Int) => Int) => Int) => " +
          "Int) => Int) => Int))",
        "T.<init>"
      ),
      "symbolic form: power tower higher than 8"
    )

  // Exact equals classes on the method side; the stored-value row keeps its lossy capacity.
  def floats =
    (count("case class P(x: Double)", "P.<init>") === Count.Finite(BigInt("18437736874454810627")))
      .and(
        count("case class Q(x: Float, n: Int)", "Q.<init>") ===
          Count.Finite(BigInt("4278190083") * BigInt(2).pow(32))
      )

  // Inner's slot `a: A` is filled from {a, g(c) | c: Int} for each outer n: (1 + 2^32)^(2^32);
  // InnerB's from {a, g(true), g(false)} for each outer b: 3^2.
  def outerCaptures = {
    val rows = counts(
      "Example.scala" ->
        ("final class Outer[A](val n: Int, val g: Int => A) { final case class Inner(a: A) }\n" +
          "final class OuterB[A](val b: Boolean, val g: Boolean => A) { final case class InnerB(a: A) }")
    )
    (rows("Outer.Inner.<init>") === Count.Symbolic(pow(BigInt(2).pow(32) + 1, pow(2, 32))))
      .and(rows("OuterB.InnerB.<init>") === Count.Finite(9))
  }

  // Each `W` position is read in its own package, as the estimate binds `C`: |Inner.type|·|C| (2^8 —
  // a capacity —, 2). The shape model would count the functions of the position instead. A position
  // the estimate declines (an unsupplied `Missing`) is `?` with the estimate's reason.
  def singletonPositions = {
    val rows = counts(
      "kg.scala" ->
        ("package kg\nfinal case class C(n: Byte)\nobject Inner\nfinal case class W(p: (Inner.type, C))\n" +
          "final case class V(p: C)\nfinal case class M(p: (Inner.type, Missing))"),
      "kb.scala" ->
        "package kb\nfinal case class C(n: Boolean)\nobject Inner\nfinal case class W(p: (Inner.type, C))",
      "kh.scala" -> "package kh\nfinal case class C(n: Byte)"
    )
    (rows("kg.W.<init>") === Count.Bounded(ByteSize))
      .and(rows("kb.W.<init>") === Count.Finite(2))
      .and(rows("kg.V.<init>") === Count.Finite(256))
      .and(
        rows("kg.M.<init>") ===
          Count.Unresolved(List(s"declaration not supplied: Missing (${StdlibModel.undeclared})"))
      )
  }

  // Both readings take `O.Inner` as the object's member, a symbolic value space the estimate saturates
  // at an infinite tier: they agree. Both read `q.X` through the parameter `q: Other` (`X = Byte`,
  // 2^8), not the object `q` (a symbolic space): they agree, at the estimate's 2^8. Where the method
  // side reads `Int` as the member `O` inherits (`Char => Boolean`, 2^(2^16)) and the estimate as the
  // builtin (2^32), the symbolic space is wider than any literal: they cannot be one type. A path
  // through `a.b`'s `val x: Foo` is no longer read through package `a`'s object: 2^8, as scalac
  // (printed as the estimate's bound, `≤2^8`).
  def symbolicDisagreement =
    (count(
      "final case class Inner(b: Byte)\nobject O { final case class Inner(f: (Int => Int) => Int) }\n" +
        "final case class Outer(i: O.Inner)",
      "Outer.<init>"
    ) === Count.Symbolic(pow(2, pow(2, BigInt(2).pow(37) + 5))))
      .and(
        count(
          "package d2\nobject q { type X = (Int => Int) => Int }\nfinal class Other { type X = Byte }\n" +
            "final case class Dep(q: Other)(val x: q.X)",
          "d2.Dep.<init>"
        ).render === "2^8"
      )
      .and(
        count("final case class P(p: Char => Boolean)", "P.<init>") === Count.Symbolic(
          pow(2, 65536)
        )
      )
      // The inherited `Int = Char => Boolean` on both sides: 2^(2^16) (a disagreement with the
      // builtin's 2^32 before register D29).
      .and(
        count(
          "package w12s\ntrait Base { type Int = Char => Boolean }\n" +
            "object O extends Base { final case class P(x: Int) }",
          "w12s.O.P.<init>"
        ) === Count.Symbolic(pow(2, 65536))
      )
      .and(counts(valPath*)("a.b.UsesT.<init>").render === "≤2^8")

  // `summon[Int]` is ambiguous between two anonymous `Int`s (3.9.0: E172), so a body reaches
  // neither: the true count of `f` is 2^32 (a literal), not 2^(2^37) (one Int read) or 2^(2^64). One
  // anonymous Int is reachable: `g` reads it, (2^32)^(2^32), and `k` beside `x`, 2^(2^32 · 2^32).
  def anonymousGround = {
    val rows = counts(
      "r19.scala" ->
        ("package r19\ndef f(using Int, Int): Int = 0\ndef g(using Int): Int = summon[Int]\n" +
          "def h(using Boolean, Boolean): Boolean = true\ndef k(x: Int)(using Int): Boolean = true")
    )
    unresolvedOn(rows("r19.f"), "ambiguous anonymous evidence: Int")
      .and(unresolvedOn(rows("r19.h"), "ambiguous anonymous evidence: Boolean"))
      .and(rows("r19.g") === Count.Symbolic(pow(2, pow(2, 37))))
      .and(rows("r19.k") === Count.Symbolic(pow(2, pow(2, 64))))
  }

  def evidence = {
    val reason = "=:= requires free-atom or rigid-application endpoints"
    unresolvedOn(count("def f[A](ev: A =:= Int, a: A): Int = 0", "f"), reason)
      .and(unresolvedOn(count("def g(ev: Int =:= Int): Int = 0", "g"), reason))
  }

  // The stored-value algebra counts `Nothing => B` as 0 (it can never run); the method side counts
  // its one function, so a supplied one does not make a context absurd. The constructor position
  // has two readings and is refused.
  def emptyDomain =
    unresolvedOn(
      count("final case class Y(f: Nothing => String)", "Y.<init>"),
      "constructor position readings disagree: f (value space 1, estimate 0)"
    ).and(count("def f(g: Nothing => String): Int = 0", "f") === Count.Finite(BigInt(2).pow(32)))

  // An unsupplied `org.foo` is assumed not to declare `Int` (the never-shadowed assumption; were it
  // `type Int = Unit` the count would be 1): `Int` is scala.Int, 2^(2^32).
  def wildcard =
    count(
      "package imp\nimport org.foo.*\nobject C { def f[X](x: Int, a: X, b: X): X = a }",
      "imp.C.f"
    ) === intPredicate

}
