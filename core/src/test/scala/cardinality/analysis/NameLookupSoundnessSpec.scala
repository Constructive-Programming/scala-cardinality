package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** Names a signature spells, read as the declaration Scala binds them to. Every fixture compiles
  * with Scala 3.9.0 with the bodies given in the comments (`= true` where a `Boolean` is claimed);
  * each expected number is derived beside it.
  */
class NameLookupSoundnessSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Import paths
      anchor a `_root_` import at the root, a relative one nearest first $importAnchors
      anchor a relative import only where a clause opens a package      $qualifiedClauseImport
      read a simple name only in the packages a clause opens            $qualifiedClauseName
      read an opaque type a `_root_` import names as opaque              $rootOpaque
      read a `_root_` type path at the root                             $rootPath
      read an import prefix an earlier import binds                     $importedPrefix
      read a path whose first segment an import binds                   $importedObjectPath
    Precedence
      read a wildcard import before a package member of another file    $wildcardOverPackage
      resolve a parent clause through a wildcard import                 $wildcardParent
    Inherited member objects
      read a path through an object the template inherits               $inheritedObjectPath
      refuse a path an unsupplied parent may bind                       $unsuppliedObjectPath
  """

  private def inputs(sources: (String, String)*): List[MethodAnalysis.Input] =
    sources.toList.map { (path, text) =>
      MethodAnalysis.Input(path, dialects.Scala3(text).parse[Source].get)
    }

  private def counts(sources: (String, String)*): Map[String, Count] =
    MethodAnalysis.analyze(inputs(sources*)).map(e => e.name -> e.count).toMap

  private def refused(found: Count, fragment: String): MatchResult[Count] =
    found must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains(fragment)) must beTrue
    }

  private val options = List(
    "a.scala" -> "package p\nobject Opt { final case class Option[A](u: Unit) }",
    "b.scala" -> "package q.p\nobject Opt { final case class Option[A](a: A) }"
  )

  // `p.Opt.Option[A](u: Unit)` at the root and `q.p.Opt.Option[A](a: A)` nested in `q`. In `q`,
  // `_root_.p` is the top-level package — one value, `Option(())`; dropping `_root_` read
  // `scala.Option` (3). Plain `p` is `q.p`, the nearer package: `Option(x)` or `Option(y)` — 2.
  def importAnchors = {
    val use = "object U { def f[A](x: A, y: A): Option[A] = ??? }"
    val root = counts(options :+ ("c.scala" -> s"package q\nimport _root_.p.Opt.Option\n$use")*)
    val relative = counts(options :+ ("d.scala" -> s"package q\nimport p.Opt.Option\n$use")*)
    (root("q.U.f") === Count.Finite(1)).and(relative("q.U.f") === Count.Finite(2))
  }

  // A single `package a.b` clause opens `a.b` alone, never `a`: `import p.X` binds the top-level
  // `p.X(b: Boolean)` (3.9.0: `X(())` is a type error there), not `a.p.X(u: Unit)` — 2. Chained
  // clauses `package a; package b` open both, so there the import reaches `a.p.X` — 1.
  def qualifiedClauseImport = {
    val sources = List(
      "ap.scala" -> "package a.p\nfinal case class X(u: Unit)",
      "p.scala" -> "package p\nfinal case class X(b: Boolean)"
    )
    val single = counts(
      sources :+ ("u.scala" -> "package a.b\nimport p.X\nobject U { def f(): X = ??? }")*
    )
    val chained =
      counts(
        sources :+ ("u.scala" -> "package a\npackage b\nimport p.X\nobject U { def f(): X = ??? }")*
      )
    (single("a.b.U.f") === Count.Finite(2)).and(chained("a.b.U.f") === Count.Finite(1))
  }

  // `a.Option` is not visible in `package a.b` (3.9.0 compiles `Some(x)` there): `scala.Option`,
  // None or Some of x or y — 3. In chained clauses `a`'s own `Option(u: Unit)` answers — 1.
  def qualifiedClauseName = {
    val declared = "a.scala" -> "package a\nfinal case class Option[A](u: Unit)"
    val use = "object U { def f[A](x: A, y: A): Option[A] = ??? }"
    (counts(declared, "b.scala" -> s"package a.b\n$use")("a.b.U.f") === Count.Finite(3))
      .and(
        counts(declared, "b.scala" -> s"package a\npackage b\n$use")("a.b.U.f") === Count.Finite(1)
      )
  }

  // `D` is opaque in `d.data`; from `u` its representation `(A, A)` is hidden, so a `_root_` import
  // that binds it must reach the declaration and say so — not read some other `D` or a builtin.
  def rootOpaque = {
    val found = counts(
      "a.scala" -> "package d.data\nopaque type D[A] = (A, A)",
      "b.scala" -> "package u\nimport _root_.d.data.D\nobject U { def f[A](x: D[A]): D[A] = ??? }"
    )
    refused(found("u.U.f"), "opaque representation is not visible at use site: d.data.D")
  }

  // The type path `_root_.p.Opt.Option[A]` in `q` is the top-level one: `Option(())` — 1, while
  // `p.Opt.Option[A]` there is `q.p`'s: 2.
  def rootPath = {
    val found = counts(
      options :+ ("c.scala" -> """package q
        |object U { def f[A](x: A, y: A): _root_.p.Opt.Option[A] = ??? }
        |object V { def f[A](x: A, y: A): p.Opt.Option[A] = ??? }""".stripMargin)*
    )
    (found("q.U.f") === Count.Finite(1)).and(found("q.V.f") === Count.Finite(2))
  }

  // `import p.q; import q.X`: the second prefix is the `p.q` the first import binds (an explicit
  // import outranks a package of another file), so X is `p.q.X = Boolean` (3.9.0 accepts `= true`),
  // not the root `q.X = Unit`: true, false — 2 (it read 1).
  def importedPrefix = {
    val found = counts(
      "pq.scala" -> "package p.q\ntype X = Boolean",
      "q.scala" -> "package q\ntype X = Unit",
      "z.scala" -> "package z\nimport p.q\nimport q.X\nobject U { def f(): X = ??? }"
    )
    found("z.U.f") === Count.Finite(2)
  }

  // `import p.Obj` binds `Obj` before the package's own `z.Obj` of another file, for a later import
  // prefix and for a type path alike: `Obj.X = Boolean` — 2 each (both read `z.Obj.X = Unit`, 1).
  def importedObjectPath = {
    val found = counts(
      "p.scala" -> "package p\nobject Obj { type X = Boolean }",
      "zobj.scala" -> "package z\nobject Obj { type X = Unit }",
      "z.scala" -> """package z
        |import p.Obj
        |import Obj.X
        |object U { def f(): X = ??? }
        |object V { def g(): Obj.X = ??? }""".stripMargin
    )
    (found("z.U.f") === Count.Finite(2)).and(found("z.V.g") === Count.Finite(2))
  }

  // Scala ranks a wildcard import above a package member of another compilation unit (an outer
  // wildcard against an inner package member, or a same-file member, is E049): `X` is `lib.X =
  // Boolean` — 2 (it read `p.X = Unit`, 1).
  def wildcardOverPackage =
    counts(
      "lx.scala" -> "package lib\ntype X = Boolean",
      "px.scala" -> "package p\ntype X = Unit",
      "u.scala" -> "package p\nimport lib.*\nobject U { def f(): X = ??? }"
    )("p.U.f") === Count.Finite(2)

  // The parent `Base[A]` is `lib.Base` (3.9.0 accepts `final class D[A](a: A) extends Base[A] { def
  // seed: A = a }` there), not `p.Base` of another file: m has a and seed — 2 (it read a, o1, o2).
  def wildcardParent =
    counts(
      "lib.scala" -> "package lib\ntrait Base[A] { def seed: A }",
      "pb.scala" -> "package p\ntrait Base[A] { def o1: A; def o2: A }",
      "c.scala" ->
        "package p\nimport lib.*\nabstract class C[A](a: A) extends Base[A] { def m: A = ??? }"
    )("p.C.m") === Count.Finite(2)

  private val members = List(
    "c.scala" -> "package p\nobject c { type X = Unit }",
    "has.scala" -> "package p\ntrait HasC { object c { type X = Boolean } }"
  )

  // Inside a template extending `HasC`, `c` is the inherited member object, before the package's
  // `p.c` of another file: `c.X`, `import c.X` and `import c.*` all reach `Boolean` — 2 each
  // (3.9.0 accepts `= true`; all three read 1).
  def inheritedObjectPath = {
    val found = counts(
      members :+ ("in.scala" -> """package p
        |object In extends HasC { def f(): c.X = ??? }
        |object In2 extends HasC { import c.X; def g(): X = ??? }
        |object In3 extends HasC { import c.*; def h(): X = ??? }""".stripMargin)*
    )
    (found("p.In.f") === Count.Finite(2))
      .and(found("p.In2.g") === Count.Finite(2))
      .and(found("p.In3.h") === Count.Finite(2))
  }

  // `lib.Base` is not supplied and may declare an `object c` (3.9.0: with `trait Base { object c {
  // type X = Boolean } }`, `def g(): c.X = true` compiles beside `w.c.X = Unit`), so the path is
  // refused with the parent named rather than read as `w.c.X` (it read 1).
  def unsuppliedObjectPath =
    refused(
      counts(
        "c.scala" -> "package w\nobject c { type X = Unit }",
        "w.scala" -> "package w\nclass W extends lib.Base { def g(): c.X = ??? }"
      )("w.W.g"),
      "inherited type member not resolved: c (unsupplied parent lib.Base)"
    )

}
