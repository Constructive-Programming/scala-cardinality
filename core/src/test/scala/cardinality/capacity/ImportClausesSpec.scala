package cardinality.capacity

import org.specs2.Specification

// Explicit import and export clauses in the stored-value estimate
// (docs/research/stored-value-families.md): a clause names the member of the supplied path it
// reads, read with the clauses before it; a clause the sources do not follow stays its reason; an
// export outranks an import; and a selector binds only the namespaces its member has, a body's
// own definition outranking it in that namespace alone. Fixtures checked with scalac 3.9.0.
class ImportClausesSpec extends Specification {

  import Rows.*
  import Definition.Kind.*

  def is = s2"""
    Explicit imports are followed into the supplied sources
      an import names the member of the path it reads             $importsFollowed
      a clause the sources do not supply stays its reason          $importsRefused
      an import of an inherited member says so                    $inheritedImport
      an export outranks an import or an outer definition          $exportsOutrank
      an export's path reads with the imports before it           $exportPaths
      a renamed member leaves its importer's wildcard             $renamedHidden
      a selector binds only the namespaces its member has         $namespaces
    """

  private val widgets = List(
    "a/Widget.scala" -> "package a\ncase class Widget(x: Boolean)",
    "b/Widget.scala" -> "package b\ncase class Widget(x: Unit)",
  )

  // `import b.Widget` outranks package `a`'s own `Widget` from another file: 1. `import a.Widget`
  // and its rename are `a.Widget`: 2. `data.Vec` reads relative to `package dev` (2); a class body
  // imports its companion's alias (B = Boolean): (B, Boolean) = 4; a chain of clauses reaches an
  // object's generic alias and its family: Pair[Boolean, Unit]·In·S = 2·2·2 = 8.
  def importsFollowed = {
    val found = rowsOf(
      (widgets ++ List(
        "a/Use.scala" -> "package a\nimport b.Widget\ncase class Use(w: Widget)",
        "c/Use.scala" -> "package c\nimport a.Widget\ncase class Use(w: Widget)\nimport a.{Widget as W2}\ncase class Use2(w: W2)",
        "dev/Vec.scala" -> "package dev.data\ncase class Vec(x: Boolean)",
        "dev/Use.scala" -> "package dev\npackage app\nimport data.Vec\ncase class Use(v: Vec)",
        "dev/C.scala" -> "package dev\nfinal class C { import C.B; type X = (B, Boolean) }\nobject C { type B = Boolean }",
        "o/A.scala" -> "package o\nobject O { type Pair[A, B] = (A, B); final case class In(b: Boolean); sealed trait S; object S { case object S1 extends S; case object S2 extends S } }",
        "o/B.scala" -> "package o2\nimport o.O\nimport O.Pair\nimport O.{In, S}\nfinal case class P(p: Pair[Boolean, Unit], i: In, s: S)",
      ))*
    )
    (found(("a.Use", Class)) === "1")
      .and(found(("c.Use", Class)) === "2")
      .and(found(("c.Use2", Class)) === "2")
      .and(found(("dev.app.Use", Class)) === "2")
      .and(found(("dev.C.X", Alias)) === "4")
      .and(found(("o2.P", Class)) === "8")
  }

  // A namespace no source supplies, a member a supplied package does not define, a path whose
  // first name an unsupplied wildcard may bind (it might still name `a.Widget`), and a type through
  // a value: each a reason that says which — never a source type of the same name.
  def importsRefused = {
    val found = rowsOf(
      (widgets ++ List(
        "c/Use.scala" -> "package c\nimport zz.Widget\ncase class Use(w: Widget)\nimport a.Missing\ncase class Use2(m: Missing)",
        "d/Use.scala" -> "package d\nimport scala.jdk.CollectionConverters.*\nimport a.Widget\ncase class Use(w: Widget)",
        "e/H.scala" -> "package e\nfinal class C { type T = Boolean }\nfinal class H(val x: C) { import x.T; type Y = T }",
      ))*
    )
    (found(("c.Use", Class)) === "unresolved[Widget (imported from zz.Widget, not supplied)]")
      .and(
        found(("c.Use2", Class)) === "unresolved[Missing (imported from a.Missing, not supplied)]"
      )
      .and(
        found(("d.Use", Class)) ===
          "unresolved[Widget (imported from a.Widget, a may be imported by scala.jdk.CollectionConverters.*)]"
      )
      .and(found(("e.H.Y", Alias)) === "unresolved[T (imported from x.T, not followed)]")
  }

  // An export defines a member of its template, which outranks an import of the name there and an
  // outer definition of it (crosscheck X1; scalac: `def g(t: T): Boolean = t` compiles in both):
  // |C.P| = |Y.T| = 2, not |X.T| = 2^32, and |Outer.D.Q| = |A.T| = 2.
  def exportsOutrank = {
    val found = rows(
      """|object X { type T = Int }
         |object Y { type T = Boolean }
         |class C {
         |  import X.T
         |  export Y.T
         |  final case class P(t: T)
         |}
         |object Outer {
         |  object A { type T = Boolean }
         |  type T = Int
         |  class D {
         |    export A.T
         |    final case class Q(t: T)
         |  }
         |}
         |""".stripMargin
    )
    (found(("C.P", Class)) === "2").and(found(("Outer.D.Q", Class)) === "2")
  }

  // An export's path reads with the template's imports before it: `import ea.Inner; export
  // Inner.T` exports `ea.Inner.T` (2) — never the package's other `eb.Inner.T` (1), a type or, through
  // the exported object `M`, a term path (scalac: `Q(ea.Inner.T(true))` compiles).
  def exportPaths = {
    val found = rowsOf(
      "ea/A.scala" -> "package ea\nobject Inner { final case class T(b: Boolean); object M { final case class T(b: Boolean) } }",
      "eb/B1.scala" -> "package eb\nobject Inner { final case class T(u: Unit); object M { final case class T(u: Unit) } }",
      "eb/B2.scala" -> "package eb\nobject O {\n  import ea.Inner\n  export Inner.T\n  export Inner.M\n  final case class Q(t: T)\n  final case class R(t: M.T)\n}",
    )
    (found(("eb.O.Q", Class)) === "2").and(found(("eb.O.R", Class)) === "2")
  }

  // `import ra.{W => V, *}` imports every member of `ra` but `W` under its own name (scalac: `C(W(()))`
  // reads the package's `rb.W`): |C| = 1, not 2.
  def renamedHidden =
    rowsOf(
      "ra/A.scala" -> "package ra\nfinal case class W(b: Boolean)",
      "rb/B1.scala" -> "package rb\nfinal case class W(u: Unit)",
      "rb/B2.scala" -> "package rb\nimport ra.{W => V, *}\nfinal case class C(w: W)",
    )(("rb.C", Class)) === "1"

  // A body's `val W` outranks `import to1.W` for the term only: the type `W` is still `to1.W` (2,
  // not the package's other `to2.W`, 1). An import of a member that is only a term (`object W`)
  // binds no type: `W` is the package's `tb.W` (2), not `not supplied`. An import of an object's
  // exported member says that is what it does not follow.
  def namespaces = {
    val found = rowsOf(
      "to1/A.scala" -> "package to1\nfinal case class W(b: Boolean)",
      "to2/B1.scala" -> "package to2\nfinal case class W(u: Unit)",
      "to2/B2.scala" -> "package to2\nobject O {\n  import to1.W\n  val W: Int = 1\n  final case class C(w: W)\n}",
      "ta/A.scala" -> "package ta\nobject W { val x: Int = 1 }",
      "tb/B1.scala" -> "package tb\nfinal case class W(b: Boolean)",
      "tb/B2.scala" -> "package tb\nimport ta.W\nfinal case class C(w: W)",
      "ie/X.scala" -> "package ie\nobject X { final case class U(b: Boolean) }\nobject Impl { export X.U }",
      "ie2/C.scala" -> "package ie2\nimport ie.Impl.U\nfinal case class C(u: U)",
    )
    (found(("to2.O.C", Class)) === "2")
      .and(found(("tb.C", Class)) === "2")
      .and(
        found(("ie2.C", Class)) ===
          "unresolved[U (imported from ie.Impl.U, exported member not followed)]"
      )
  }

  // `Impl` and `U` are both supplied; `Impl` inherits `U` from `Holder`, which the estimate does not
  // follow: the reason says that, not that no source supplies it.
  def inheritedImport =
    rows(
      "trait Holder { type W; final case class U(w: W, b: Boolean) }\nobject Impl extends Holder { type W = Boolean }\nobject Use { import Impl.U; final case class X(u: U) }"
    )(("Use.X", Class)) === "unresolved[U (imported from Impl.U, inherited member not followed)]"

}
