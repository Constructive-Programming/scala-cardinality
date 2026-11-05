package cardinality.capacity

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification
import org.specs2.execute.Result

// Where a stored-value read finds a name, and what a value row holds (docs/research/
// stored-value-names.md, pure-code.md): an explicit enum companion holds the enum's cases; a
// package's object's parents and its files' top-level exports may bind any name, as a template
// body's parents may, which is refused; an opaque alias is its
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

    A template body's parents may bind names, which are refused
      a parent's export may define the name, which is refused          $inheritedExports
      an unsupplied parent refuses; marker parents declare nothing    $unsuppliedParents
      a parent through an alias or a value is not read               $unreadParents

    An opaque alias is its representation only inside its scope
      a member alias inside its template, refused outside             $opaqueMembers
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

  // An export clause defines alias members of its template (Scala 3 reference, export clauses),
  // which a subclass inherits: scalac reads `Boolean` in `O.U` as `Src.Boolean = Int` (`O.U(42).b:
  // Int`), `q` in `OQ.UQ` as `Src.q` (`x: Int`) and `Flag` in `OW.UW` as `Src.Flag = Long`. The
  // estimate does not follow a parent's exports: each is
  // refused with the parent named, as the method side refuses it — never the builtin's 2, nor the
  // package's `q.X = Unit` or `Flag = Unit` (1).
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
      "inherited type member not resolved: Boolean (unsupplied parent h.B)"
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

  // `Id`'s representation is visible in `Ids`: `Inside(id: Id)` ≤2^32. Outside, `U(id: Ids.Id)` is
  // refused (it was 1, as though the alias had one value). `Box.B[Boolean] = Option[Boolean]` is 3
  // inside `Box`, refused outside. The opaque rows read the representation.
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
      .and(refusedWith(found(("op.U", Class)), "opaque type read outside its scope: op.Ids.Id"))
      .and(found(("op.Box.In", Class)) === "3")
      .and(refusedWith(found(("op.UB", Class)), "opaque type read outside its scope: op.Box.B"))
      .and(found(("op.Ids.Id", Opaque)) === "≤2^32")
  }

}
