package cardinality.capacity

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification
import org.specs2.execute.Result

// Where a stored-value read finds a name, and what a value row holds (docs/research/
// stored-value-names.md, pure-code.md): an explicit enum companion holds the enum's cases; a
// package holds what its package object's parents pass on and what its files' top-level exports
// define; a template body binds its parents' and self type's members; an opaque alias is its
// representation only where that is visible (what a value row holds: `ValueRowsSpec`). Each
// number is derived beside it; fixtures checked with scalac 3.9.0 (the integrated review's probes
// and z1b/fx).
class StoredScopesSpec extends Specification {

  import Rows.*
  import Definition.Kind.*

  def is = s2"""
    An explicit enum companion holds the enum's cases
      a wildcard, a selector, a path and a bare name bind the case     $enumCompanion
      a singleton case is a term only, a plain object no enum          $enumTerms
      constructor slots lent to the method side refuse alike          $enumLending
      case parameters read in the companion, the body imports cases    $enumCases

    A package holds what its package object and exports pass on
      a package object's parents may bind any name                    $packageParents
      a top-level export over a package's object is followed          $packageExports
      an export read with an import, or a private member, is refused  $packageRefusals
      a path's first name through a followed export                   $packageHeads

    A template body binds its parents' and self type's members
      inherited aliases over a package's or the root's names          $inheritedAliases
      transitive parents, self types and nearer declarations          $inheritedScopes
      a path's first name, and constructor parameters outside         $inheritedTerms
      a parent's export may define the name, which is refused          $inheritedExports
      an unsupplied parent refuses; marker parents declare nothing    $unsuppliedParents
      a parent through an alias or a value is not read               $unreadParents
      a qualified-private member is inherited, a private one not      $qualifiedPrivate
      abstract, opaque and parameterised members are not read         $unreadMembers
      constructor slots read the member on both sides                 $inheritedLending

    An opaque alias is its representation only inside its scope
      a member alias inside its template, refused outside             $opaqueMembers
      a top-level alias in its file's package object                  $opaqueTopLevel
      constructor slots outside the scope are refused                 $opaqueLending
    """

  private def refusedWith(verdict: String, reason: String): Result =
    (verdict must startWith("unresolved[")).and(verdict must contain(reason))

  private def refusedCount(count: Count, reason: String): Result =
    count must beLike { case Count.Unresolved(rs) => rs.exists(_.contains(reason)) must beTrue }

  // A count refused with each of these reasons, verbatim.
  private def refusedExactly(count: Count, reasons: String*): Result =
    count must beLike { case Count.Unresolved(rs) => rs must containAllOf(reasons) }

  // `object E` beside `enum E` holds `E.A`: under `import E.*`, an explicit selector, a qualified
  // path, and bare inside `object E`, `A` is the case class — not another file's `type A = Unit`
  // (1) — and the case class is not read as a type of its own, so each row is refused. The sealed
  // `S` sums `L(a: A)`, refused too. `E.X` stays the companion's own alias (2).
  def enumCompanion = {
    val found = rowsOf(
      "en/a.scala" ->
        """|package en
           |enum E { case A(i: Int); case B }
           |object E { type X = Boolean; final case class W(a: A) }
           |object Sel { import E.A; final case class U1(a: A) }
           |object Qual { final case class U2(a: E.A) }
           |object Wild {
           |  import E.*
           |  final case class U3(a: A, x: X)
           |  sealed trait S
           |  final case class L(a: A) extends S
           |  case object M extends S
           |}
           |object Exp { export E.*; final case class V(a: A) }
           |""".stripMargin,
      "en/b.scala" -> "package en\ntype A = Unit"
    )
    val refusal = "enum case type not read: en.E.A"
    refusedWith(found(("en.Sel.U1", Class)), refusal)
      .and(refusedWith(found(("en.Qual.U2", Class)), refusal))
      .and(refusedWith(found(("en.Wild.U3", Class)), refusal))
      .and(refusedWith(found(("en.Wild.L", Class)), refusal))
      .and(refusedWith(found(("en.Wild.S", Abstract)), refusal))
      .and(refusedWith(found(("en.E.W", Class)), refusal))
      .and(refusedWith(found(("en.Exp.V", Class)), refusal))
      .and(found(("en.E.X", Alias)) === "2")
      .and(found(("en.A", Alias)) === "1")
  }

  // `case Boolean` is a value: `import G.*` binds no type `Boolean`, so `Ug(b: Boolean)` is the
  // builtin's 2 (scalac accepts `UseG.Ug(true)`). `object Color` accompanies no enum, so its class
  // `Rgb(r: Byte, g: Byte)` is read (≤2^16) and `P(c: Rgb, f: Flag)` is 2^16·2 = ≤2^17 under its
  // wildcard. With no explicit companion the enum is no namespace the estimate reads, and the
  // wildcard over it may bind anything.
  def enumTerms = {
    val found = rowsOf(
      "g/a.scala" ->
        """|package g
           |enum G { case Boolean; case C(i: Int) }
           |object G
           |object UseG { import G.*; final case class Ug(b: Boolean) }
           |object Color { final case class Rgb(r: Byte, g: Byte); type Flag = Boolean }
           |object UseC { import Color.*; final case class P(c: Rgb, f: Flag) }
           |enum H { case Boolean(i: Int) }
           |object UseH { import H.*; final case class Uh(b: Boolean) }
           |""".stripMargin,
      "g/b.scala" -> "package g\ntype Rgb = Boolean"
    )
    (found(("g.UseG.Ug", Class)) === "2")
      .and(found(("g.UseC.P", Class)) === "≤2^17")
      .and(refusedWith(found(("g.UseH.Uh", Class)), "Boolean (may be imported by H.*)"))
  }

  // The method side reads a constructor's slots through the estimate: refused there, the slot has
  // no number either (they were 1 when the wildcard missed the case and read another file's `A`).
  // The case class's own constructor reads `i: Int`: 2^32.
  def enumLending = {
    val files = List(
      "en/a.scala" ->
        "package en\nenum E { case A(i: Int) }\nobject E { final case class W(a: A) }\nobject Use { import E.*; final case class U(a: A) }",
      "en/b.scala" -> "package en\ntype A = Unit"
    )
    refusedCount(constructor("en.Use.U.<init>", files*), "enum case type not read: en.E.A")
      .and(refusedCount(constructor("en.E.W.<init>", files*), "enum case type not read: en.E.A"))
      .and(constructor("en.E.A.<init>", files*) === Count.Finite(BigInt(2).pow(32)))
  }

  // Scala 3 puts a class case in the companion, so `case C(a: A)` reads the case `E.A`, not another
  // file's `type A = Unit` (scalac builds `E.C(E.A(true))`), with an explicit companion or none
  // (`E2`); the enum's body imports the cases by name, so `type W = Option[A]` and `In(a: A)` read
  // the case too (scalac rejects `e.In(())`). Each refuses it: the enum, `Use(e: E)`, `E#W`, `E#In`
  // — they were 3, 3, 2 and 1, reading `A` as 1. A companion's parent passes its members on to the
  // cases: `K(t: T)` reads `GBase`'s `T = Boolean` (scalac: `G.K(true)`), so G = 2 + 1. The case's
  // copy of the enum's parameter binds over a case of its name: `HI = H[Boolean]` is `A(i: Int)` +
  // `B(a: Boolean)`, 2^32 + 2 = ≤2^33. The method side lends the refusal: `E.C.<init>` (was 1).
  def enumCases = {
    val files = List(
      "ec/a.scala" ->
        """|package ec
           |enum E { case A(b: Boolean); case C(a: A); type W = Option[A]; final case class In(a: A) }
           |object E
           |enum E2 { case A(b: Boolean); case C(a: A) }
           |final case class Use(e: E)
           |trait GBase { type T = Boolean }
           |enum G { case K(t: T); case L }
           |object G extends GBase
           |enum H[+A] { case A(i: Int); case B(a: A) }
           |type HI = H[Boolean]
           |""".stripMargin,
      "ec/b.scala" -> "package ec\ntype A = Unit\ntype T = Unit"
    )
    val found = rowsOf(files*)
    val refusal = "enum case type not read: ec.E.A"
    refusedWith(found(("ec.E", Enum)), refusal)
      .and(refusedWith(found(("ec.E2", Enum)), "enum case type not read: ec.E2.A"))
      .and(refusedWith(found(("ec.Use", Class)), refusal))
      .and(refusedWith(found(("ec.E#W", Alias)), refusal))
      .and(refusedWith(found(("ec.E#In", Class)), refusal))
      .and(found(("ec.G", Enum)) === "3")
      .and(found(("ec.HI", Alias)) === "≤2^33")
      .and(refusedExactly(constructor("ec.E.C.<init>", files*), refusal))
  }

  private val base = "basep/Base.scala" -> "package basep\ntrait Base { type Boolean = Int }"

  // `package object lib extends basep.Base` makes `Boolean` a member of `lib`, which outranks the
  // root import in another file of `lib` (scalac reads `lib.U(42).b` as an `Int`): the estimate
  // does not follow parents, so the name is refused — never the builtin's 2. A wildcard over
  // `lib` from outside says the same. A package object's own alias is a member it defines: ≤2^32.
  def packageParents = {
    val found = rowsOf(
      base,
      "lib/package.scala" -> "package object lib extends basep.Base",
      "lib/U.scala" -> "package lib\nfinal case class U(b: Boolean)",
      "use/V.scala" -> "package use\nimport lib.*\nfinal case class V(b: Boolean)",
      "rlib/package.scala" -> "package object rlib { type Boolean = Int }",
      "rlib/U.scala" -> "package rlib\nfinal case class U(b: Boolean, s: scala.Boolean)",
    )
    refusedWith(
      found(("lib.U", Class)),
      "Boolean (may be a member of package lib, an inherited member not followed)"
    )
      .and(refusedWith(found(("use.V", Class)), "an inherited member not followed"))
      .and(found(("rlib.U", Class)) === "≤2^33")
  }

  // `export Src.*` in one file of `lib3` defines `lib3.Boolean = Src.Boolean = Int` for the others:
  // `W(b: Boolean)` is ≤2^32 (it read the builtin's 2). A renamed selector exports `Flag` only:
  // `W2(b: Boolean, f: Flag)` is 2·2^32 = ≤2^33 (scalac accepts `W2(true, 3)`). The exporting
  // file's own class reads its export as before: ≤2^32.
  def packageExports = {
    val found = rowsOf(
      "lib3/Exp.scala" ->
        "package lib3\nobject Src { type Boolean = Int }\nexport Src.*\nfinal case class Same(b: Boolean)",
      "lib3/W.scala" -> "package lib3\nfinal case class W(b: Boolean)",
      "lib4/Exp.scala" -> "package lib4\nobject Src { type Boolean = Int }\nexport Src.{Boolean as Flag}",
      "lib4/W.scala" -> "package lib4\nfinal case class W2(b: Boolean, f: Flag)",
    )
    (found(("lib3.W", Class)) === "≤2^32")
      .and(found(("lib3.Same", Class)) === "≤2^32")
      .and(found(("lib4.W2", Class)) === "≤2^33")
  }

  // An export written after an import reads its path with that import: not followed from the
  // package alone, so a name it may define is refused — a path's first name too. A private member
  // is not exported; the estimate does not follow which one is, and refuses. An exported object's
  // own parents may pass on any name (`Src extends B` exports `B`'s `Boolean = Int`: scalac reads
  // `W(5)`), which is not followed either. An export over a value is not followed: in the empty
  // package, the reason names it so.
  def packageRefusals = {
    val found = rowsOf(
      "lib5/Exp.scala" ->
        "package lib5\nimport scala.collection.immutable.*\nobject Src { type Boolean = Int }\nexport Src.*",
      "lib5/W.scala" -> "package lib5\nfinal case class W(b: Boolean)\nfinal case class R(x: q.X)",
      "lib6/Exp.scala" -> "package lib6\nobject Src { private type Boolean = Int }\nexport Src.*",
      "lib6/W.scala" -> "package lib6\nfinal case class W(b: Boolean)",
      "l7/Exp.scala" -> "package l7\ntrait B { type Boolean = Int }\nobject Src extends B\nexport Src.*",
      "l7/W.scala" -> "package l7\nfinal case class W(b: Boolean)",
      "Exp.scala" -> "object Src { object q { type X = Int } }\nval s = Src\nexport s.*",
      "U.scala" -> "final case class U(x: q.X)",
    )
    refusedWith(
      found(("lib5.W", Class)),
      "Boolean (may be a member of package lib5, an exported member not followed)"
    )
      .and(
        refusedWith(
          found(("lib5.R", Class)),
          "q.X (may be a member of package lib5, an exported member not followed)"
        )
      )
      .and(
        refusedWith(
          found(("lib6.W", Class)),
          "Boolean (exported from Src, a private member not followed)"
        )
      )
      .and(
        refusedWith(
          found(("l7.W", Class)),
          "Boolean (may be exported from Src, an inherited member not followed)"
        )
      )
      .and(
        refusedWith(
          found(("U", Class)),
          "q.X (may be a member of the empty package, an exported member not followed)"
        )
      )
  }

  // A path's first name the package's files do not define is read through a followed export like
  // a type: `Src` exports no term `scala`, so `scala.Boolean` and `scala.Byte` are the root
  // package's (Q = 2·2^8, as before exports were read; refusing them lost the number), and `q.X`
  // is `Src.q.X = Short` (scalac: `R(3.toShort)`). In the empty package alike: `U(x: q.X)` ≤2^32.
  def packageHeads = {
    val files = List(
      "lib9/Exp.scala" ->
        "package lib9\nobject Src { type Flag = Int; object q { type X = Short } }\nexport Src.*",
      "lib9/Q.scala" ->
        "package lib9\nfinal case class Q(i: scala.Boolean, b: scala.Byte)\nfinal case class R(x: q.X)",
      "Exp.scala" -> "object Src { object q { type X = Int } }\nexport Src.*",
      "U.scala" -> "final case class U(x: q.X)",
    )
    val found = rowsOf(files*)
    (found(("lib9.Q", Class)) === "≤2^9")
      .and(found(("lib9.R", Class)) === "≤2^16")
      .and(found(("U", Class)) === "≤2^32")
      .and(constructor("lib9.Q.<init>", files*) === Count.Finite(BigInt(2).pow(9)))
  }

  // Inside `O extends Base`, `Boolean` is `Base`'s `Int` and `Unit` its `Long` (scalac: `O.U(42).b:
  // Int`, `O.V(7L).u: Long`), over the root import and over the package's own `type Boolean =
  // Short` (an inherited member outranks a package member): U ≤2^32, V ≤2^64, `W = Option[Boolean]`
  // 1 + 2^32 = ≤2^33. A class body inherits alike: K#U ≤2^32. `C` has no parent: CU 2. `D =
  // O.Pair[scala.Boolean]` with `Pair[A](a: A, u: Unit)`: 2·2^64 = ≤2^65.
  def inheritedAliases = {
    val found = rows(
      """|package inh
         |type Boolean = Short
         |trait Base { type Boolean = Int; type Unit = Long }
         |object O extends Base {
         |  final case class U(b: Boolean)
         |  final case class V(u: Unit)
         |  type W = Option[Boolean]
         |  final case class Pair[A](a: A, u: Unit)
         |}
         |class K extends Base { final case class U(b: Boolean) }
         |object C { final case class CU(b: scala.Boolean) }
         |type D = O.Pair[scala.Boolean]
         |""".stripMargin
    )
    (found(("inh.O.U", Class)) === "≤2^32")
      .and(found(("inh.O.V", Class)) === "≤2^64")
      .and(found(("inh.O.W", Alias)) === "≤2^33")
      .and(found(("inh.K#U", Class)) === "≤2^32")
      .and(found(("inh.C.CU", Class)) === "2")
      .and(found(("inh.D", Alias)) === "≤2^65")
  }

  // `B2 extends B1` passes on `B1`'s `Boolean = Int`: ≤2^32. A self type's members are members of
  // `this`: T#U ≤2^32. A nested object's own `Boolean = Short` is nearer than what `O2` inherits
  // (≤2^16), and a nested object without one reads the inherited `Int` (≤2^32) — scalac reads
  // `O2.In.U(3.toShort).b: Short` and `O2.Out.U(5).b: Int`.
  def inheritedScopes = {
    val found = rows(
      """|package sc
         |trait B1 { type Boolean = Int }
         |trait B2 extends B1
         |object O1 extends B2 { final case class U(b: Boolean) }
         |trait T { self: B1 => final case class U(b: Boolean) }
         |object O2 extends B1 {
         |  object In { type Boolean = Short; final case class U(b: Boolean) }
         |  object Out { final case class U(b: Boolean) }
         |}
         |""".stripMargin
    )
    (found(("sc.O1.U", Class)) === "≤2^32")
      .and(found(("sc.T#U", Class)) === "≤2^32")
      .and(found(("sc.O2.In.U", Class)) === "≤2^16")
      .and(found(("sc.O2.Out.U", Class)) === "≤2^32")
  }

  // An inherited `val p: H.type` is the path `H` inside `O`, over the package's `object p` (scalac:
  // `O.U(42).x: Int`): `p.X` is `H.X`, ≤2^32 (it read `p.X = Unit`, 1). An inherited object is the
  // parent's: `Inner.T` is `Int`. A primary constructor's parameters are read outside the body: `K2(b:
  // Boolean)` is the builtin's 2. A `val` parameter is a member too, and so is a case class's (scalac:
  // `OP.UP(42).x: Int`, `OK.UK(42).x: Int`), ≤2^32 each; a plain or a private one is not, so the
  // package's `p` binds (`OC.UC(()).x: Unit`, `OR.UR(()).x: Unit`), 1 — and so do the `<init>` rows.
  def inheritedTerms = {
    val files = List(
      "vp.scala" ->
        """|package vp
           |object p { type X = Unit }
           |object H { type X = Int }
           |class BP(val p: H.type)
           |object OP extends BP(H) { final case class UP(x: p.X) }
           |class BC(p: H.type)
           |object OC extends BC(H) { final case class UC(x: p.X) }
           |case class BK(p: H.type)
           |object OK extends BK(H) { final case class UK(x: p.X) }
           |class BR(private val p: H.type)
           |object OR extends BR(H) { final case class UR(x: p.X) }
           |""".stripMargin
    )
    val found = rows(
      """|package it
         |object p { type X = Unit }
         |object H { type X = Int }
         |object Inner { type T = Unit }
         |trait Base { val p: H.type = H; object Inner { type T = Int }; type Boolean = Int }
         |object O extends Base { final case class U(x: p.X, t: Inner.T) }
         |final class K2(b: Boolean) extends Base
         |""".stripMargin
    )
    val parameters = rowsOf(files*)
    (found(("it.O.U", Class)) === "≤2^64")
      .and(found(("it.K2", Class)) === "2")
      .and(parameters(("vp.OP.UP", Class)) === "≤2^32")
      .and(parameters(("vp.OK.UK", Class)) === "≤2^32")
      .and(parameters(("vp.OC.UC", Class)) === "1")
      .and(parameters(("vp.OR.UR", Class)) === "1")
      .and(constructor("vp.OP.UP.<init>", files*).render === "≤2^32")
      .and(constructor("vp.OC.UC.<init>", files*) === Count.Finite(1))
  }

  // An export clause defines alias members of its template (Scala 3 reference, export clauses),
  // which a subclass inherits: scalac reads `Boolean` in `O.U` as `Src.Boolean = Int` (`O.U(42).b:
  // Int`), `q` in `OQ.UQ` as `Src.q` (`x: Int`), `Flag` in `OW.UW` as `Src.Flag = Long`, and the
  // self type's `Boolean` in `T#US` alike. The estimate does not follow a parent's exports: each is
  // refused with the parent named, as the method side refuses it — never the builtin's 2, nor the
  // package's `q.X = Unit` or `Flag = Unit` (1). A name the exported object lacks is no export:
  // `Int` in `OW.UI` stays the builtin's, ≤2^32.
  def inheritedExports = {
    val files = List(
      "ex.scala" ->
        """|package ex
           |object Src { type Boolean = Int; object q { type X = Int }; type Flag = Long }
           |object q { type X = Unit }
           |type Flag = Unit
           |class B { export Src.Boolean }
           |object O extends B { final case class U(b: Boolean) }
           |class BQ { export Src.q }
           |object OQ extends BQ { final case class UQ(x: q.X) }
           |class BW { export Src.* }
           |object OW extends BW { final case class UW(f: Flag); final case class UI(i: Int) }
           |class BE { export Src.Boolean }
           |trait T { self: BE => final case class US(b: Boolean) }
           |""".stripMargin
    )
    val found = rowsOf(files*)
    (found(("ex.O.U", Class)) ===
      "unresolved[inherited type member not resolved: Boolean (exports of B)]")
      .and(
        found(("ex.OQ.UQ", Class)) must contain(
          "inherited term member not resolved: q (exports of BQ)"
        )
      )
      .and(
        found(("ex.OW.UW", Class)) ===
          "unresolved[inherited type member not resolved: Flag (exports of BW)]"
      )
      .and(
        found(("ex.T#US", Class)) ===
          "unresolved[inherited type member not resolved: Boolean (exports of BE)]"
      )
      .and(found(("ex.OW.UI", Class)) === "≤2^32")
      .and(
        refusedExactly(
          constructor("ex.O.U.<init>", files*),
          "inherited type member not resolved: Boolean (exports of B)"
        )
      )
      .and(
        refusedExactly(
          constructor("ex.OQ.UQ.<init>", files*),
          "inherited term member not resolved: q (exports of BQ)"
        )
      )
  }

  // `zz.Base` is not supplied and may declare `type Boolean = Int`: inside `O4` the name is
  // refused, never the builtin's 2 — at any depth below it, and for a path's first name. A parent
  // the stdlib model knows declares no type member (`Serializable`, `RuntimeException`) passes
  // nothing on: 2. Spelled under a wildcard over an unsupplied namespace, `Serializable` may be
  // another declaration, which may.
  def unsuppliedParents = {
    val found = rows(
      """|package un
         |object p { type X = Unit }
         |object O4 extends zz.Base {
         |  final case class U(b: Boolean)
         |  object In { final case class V(b: Boolean) }
         |  final case class Q(x: p.X)
         |}
         |object M extends Serializable { final case class U(b: Boolean) }
         |class E2 extends RuntimeException { final case class U(b: Boolean) }
         |object W { import foo.*; object M2 extends Serializable { final case class U(b: Boolean) } }
         |""".stripMargin
    )
    val refusal = "inherited type member not resolved: Boolean (unsupplied parent zz.Base)"
    refusedWith(found(("un.O4.U", Class)), refusal)
      .and(refusedWith(found(("un.O4.In.V", Class)), refusal))
      .and(refusedWith(found(("un.O4.Q", Class)), "unsupplied parent zz.Base"))
      .and(found(("un.M.U", Class)) === "2")
      .and(found(("un.E2#U", Class)) === "2")
      .and(
        refusedWith(found(("un.W.M2.U", Class)), "parent Serializable, may be imported by foo.*")
      )
  }

  // A parent spelled through an alias, or through a value (`h.B`, with `h: H.type`), is not read,
  // so any name it may declare is refused (scalac reads `Int` in both); so is one whose name an
  // unsupplied parent may bind (`Inner` inside `O5 extends zz.Base`). A path's first name an
  // unsupplied parent may declare is a term member: `scala` in `scala.Boolean`.
  def unreadParents = {
    val found = rows(
      """|package ir
         |object H { trait B { type Boolean = Int } }
         |object O { val h: H.type = H; object In2 extends h.B { final case class U(b: Boolean) } }
         |trait B2 { type Boolean = Int }
         |object P { type BA = B2; object In extends BA { final case class U(b: Boolean) } }
         |object O4 extends zz.Base { final case class V(b: scala.Boolean) }
         |object O5 extends zz.Base { object In extends Inner { final case class U(b: Boolean) } }
         |""".stripMargin
    )
    refusedWith(
      found(("ir.O.In2.U", Class)),
      "inherited type member not resolved: Boolean (parent h.B, whose prefix is a value)"
    )
      .and(
        refusedWith(
          found(("ir.P.In.U", Class)),
          "inherited type member not resolved: Boolean (parent BA, whose declaration is not read)"
        )
      )
      .and(
        refusedWith(
          found(("ir.O4.V", Class)),
          "scala.Boolean (inherited term member not resolved: scala (unsupplied parent zz.Base))"
        )
      )
      .and(
        refusedWith(
          found(("ir.O5.In.U", Class)),
          "inherited type member not resolved: Boolean (parent Inner, whose declaration is not resolved)"
        )
      )
  }

  // `private[qp]` members are inherited: Scala binds the name to them from anywhere and rejects an
  // access from outside `qp` (E173), so in a valid program the binding stands. `U(b: Boolean)` reads
  // `Base`'s `Int` (scalac: `O.U(42)`), ≤2^32, and `V(x: p.X)` the inherited `p: H.type`'s `X =
  // Int` over the package's `object p` (`Unit`), ≤2^32 — on both sides (they were 2 and 1, and the
  // constructor of `U` refused its two readings). A `private` member is not inherited: `B(c: Byte)`
  // is the builtin's 2^8 (scalac: `O.B(1.toByte)`).
  def qualifiedPrivate = {
    val files = List(
      "a.scala" ->
        """|package qp
           |object H { type X = Int }
           |object p { type X = Unit }
           |trait Base { private[qp] type Boolean = Int; private type Byte = Int; private[qp] val p: H.type = H }
           |object O extends Base { final case class U(b: Boolean); final case class B(c: Byte); final case class V(x: p.X) }
           |""".stripMargin
    )
    val found = rowsOf(files*)
    (found(("qp.O.U", Class)) === "≤2^32")
      .and(found(("qp.O.V", Class)) === "≤2^32")
      .and(found(("qp.O.B", Class)) === "≤2^8")
      .and(constructor("qp.O.U.<init>", files*) === Count.Finite(BigInt(2).pow(32)))
      .and(constructor("qp.O.V.<init>", files*).render === "≤2^32")
  }

  // An abstract inherited member is the implementing instance's to choose (as the template's
  // own); an inherited opaque alias is outside the template that declares it (scalac rejects `o:
  // Int` in `UsesOp`), so it is one value (rule F); a parent applied to type arguments is not read
  // under them.
  def unreadMembers = {
    val found = rows(
      """|package um
         |trait Abs { type T }
         |trait Sub extends Abs { final case class U(t: T) }
         |trait Op { opaque type Ob = Int }
         |object UsesOp extends Op { final case class U(o: Ob) }
         |trait BaseP[A] { type Boolean = A }
         |object Applied extends BaseP[Int] { final case class U(b: Boolean) }
         |""".stripMargin
    )
    (found(("um.Sub#U", Class)) === "instantiation[member T]")
      .and(found(("um.UsesOp.U", Class)) === "1")
      .and(
        refusedWith(
          found(("um.Applied.U", Class)),
          "inherited type member not resolved: Boolean (type arguments of BaseP)"
        )
      )
  }

  // The method side reads `Base`'s `Int` too, so the slot stands: `P(x: Int)` is 2^64 with `Int =
  // Long` (the estimate's builtin 2^32 used to disagree). Under an unsupplied parent the slot is
  // refused on both sides; the inherited path `p.X` is the estimate's ≤2^32 (the method side does
  // not follow a path through a value, so the estimate's capacity stands, as for any such slot).
  def inheritedLending = {
    val files = List(
      "a.scala" ->
        """|package il
           |object p { type X = Unit }
           |object H { type X = Int }
           |trait Base { type Int = Long; val p: H.type = H }
           |object O extends Base { final case class P(x: Int); final case class Q(x: p.X) }
           |object O4 extends zz.Base { final case class U(b: Boolean) }
           |""".stripMargin
    )
    (constructor("il.O.P.<init>", files*) === Count.Finite(BigInt(2).pow(64)))
      .and(constructor("il.O.Q.<init>", files*).render === "≤2^32")
      .and(
        refusedExactly(
          constructor("il.O4.U.<init>", files*),
          "inherited type member not resolved: Boolean (unsupplied parent zz.Base)"
        )
      )
  }

  // `Id`'s representation is visible in `Ids`: `Inside(id: Id)` ≤2^32. Outside, an opaque type is
  // one value (rule F): `U(id: Ids.Id)` is 1. `Box.B[Boolean] = Option[Boolean]` is 3 inside `Box`,
  // 1 outside. The opaque rows read the representation.
  def opaqueMembers = {
    val found = rows(
      """|package op
         |object Ids { opaque type Id = Int; final case class Inside(id: Id) }
         |final case class U(id: Ids.Id)
         |object Box { opaque type B[A] = Option[A]; final case class In(b: B[Boolean]) }
         |final case class UB(b: Box.B[Boolean])
         |""".stripMargin
    )
    (found(("op.Ids.Inside", Class)) === "≤2^32")
      .and(found(("op.U", Class)) === "1")
      .and(found(("op.Box.In", Class)) === "3")
      .and(found(("op.UB", Class)) === "1")
      .and(found(("op.Ids.Id", Opaque)) === "≤2^32")
  }

  // A top-level `Flag` is transparent in its file's package object: a top-level alias (2), the
  // companion of a top-level type (`object Flag`, `object Al` beside `type Al`), and an implicit
  // class or object, which Scala 3 wraps there too (scalac reads `f: Boolean` in `implicit class
  // R(f: Flag)`, and in `implicit object IO`): R, R#In and IO.In are 2 — not a top-level class,
  // another object, or another file of the package (scalac rejects `asBool: Boolean = f` in `V`,
  // and `Boolean` where `Flag` is expected in `object W`), where `Flag` is one value (rule F).
  def opaqueTopLevel = {
    val found = rowsOf(
      "f/a.scala" ->
        """|package f
           |opaque type Flag = Boolean
           |type Same = Flag
           |object Flag { final case class In(f: Flag) }
           |type Al = Int
           |object Al { final case class In2(f: Flag) }
           |final case class V(f: Flag)
           |object W { final case class V2(f: Flag) }
           |implicit class R(f: Flag) { final case class In(x: Flag) }
           |implicit object IO { final case class In(x: Flag) }
           |""".stripMargin,
      "f/b.scala" -> "package f\nfinal case class V3(f: Flag)"
    )
    (found(("f.Same", Alias)) === "2")
      .and(found(("f.Flag.In", Class)) === "2")
      .and(found(("f.Al.In2", Class)) === "2")
      .and(found(("f.R", Class)) === "2")
      .and(found(("f.R#In", Class)) === "2")
      .and(found(("f.IO.In", Class)) === "2")
      .and(found(("f.V", Class)) === "1")
      .and(found(("f.W.V2", Class)) === "1")
      .and(found(("f.V3", Class)) === "1")
  }

  // Outside its scope, both sides read the opaque type as one value (rule F): `U(id: Ids.Id)` is 1,
  // and `h(f: Flag): Boolean` is 2^1. Inside, both read 2^32.
  // In an implicit class, the method side reads the representation as well: `R(f: Flag)` and
  // `In(x: Flag)` are 2, and `b: Boolean` over the captured `f` is 2^2 (it was refused on both
  // sides). So does a `given … with` body, wrapped in the package object too (scalac reads `f` as a
  // `Boolean` there): `g(f: Flag): Boolean` is 2^2. A plain top-level class of the alias's own
  // file is outside it (scalac rejects `def h(f: Flag): Boolean = f`): one value.
  def opaqueLending = {
    val files = List(
      "a.scala" ->
        "package ol\nobject Ids { opaque type Id = Int; final case class Inside(id: Id) }\nfinal case class U(id: Ids.Id)",
      "b.scala" ->
        "package ic\nopaque type Flag = Boolean\nimplicit class R(f: Flag) { def b: Boolean = f; final case class In(x: Flag) }",
      "c.scala" ->
        """|package gw
           |opaque type Flag = Boolean
           |trait Show[A] { def show(a: A): Boolean }
           |given Show[Int] with { def show(a: Int): Boolean = true; def g(f: Flag): Boolean = f }
           |class Plain { def h(f: Flag): Boolean = ??? }
           |""".stripMargin
    )
    (constructor("ol.U.<init>", files*) === Count.Finite(1))
      .and(constructor("ol.Ids.Inside.<init>", files*) === Count.Finite(BigInt(2).pow(32)))
      .and(constructor("ic.R.<init>", files*) === Count.Finite(2))
      .and(constructor("ic.R.In.<init>", files*) === Count.Finite(2))
      .and(constructor("ic.R.b", files*) === Count.Finite(4))
      .and(constructor("gw.<given@4>.g", files*) === Count.Finite(4))
      .and(constructor("gw.Plain.h", files*) === Count.Finite(2))
  }

}
