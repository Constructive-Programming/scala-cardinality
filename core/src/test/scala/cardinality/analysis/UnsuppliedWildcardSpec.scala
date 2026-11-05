package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** Wildcard imports of namespaces the sources do not supply, read on the method side as Scala binds
  * names (register D11): a wildcard ranks above a package member of another file and above a root
  * package, so `import zzz.*` with `zzz` unsupplied may bind any name the reading would otherwise
  * take from those — a type name, or a path's first name such as `compose` in `import compose.*`. A
  * definition of the file, an explicit import, and a supplied wildcard of the same scope keep their
  * names: Scala rejects a reference both of them and the unsupplied wildcard bind (E049). Every
  * fixture compiles with Scala 3.9.0 against a `zzz` stub that declares nothing of the names
  * (probes/a7/spec, probes/a7/prec); `pick(x: Two): Two` with `Two = Boolean` counts
  * `|Boolean → Boolean| = 4` wherever `Two` is read.
  */
class UnsuppliedWildcardSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    A wildcard over a namespace no source supplies
      refuses a package member of another file it may shadow           $packageMember
      refuses a member of a record its field type names               $recordField
      refuses a prefix it may rebind, and a name under that prefix    $rebindsPrefix
      refuses the first name of a named import or a qualified type    $rebindsHead
      shadows an outer scope's supplied wildcard                       $shadowsOuterWildcard
      refuses a builtin a wildcard behind a doubted prefix may bind    $doubtedBuiltins
    Selectors and prefixes the reading does not follow
      a `given` selector may bind a path's first name                  $givenSelectors
      a supplied wildcard binding a value, an enum case                $valuesAndCases
      an import whose prefix is no path                                $unfollowedPrefixes
      a package the sources spell only as a prefix is open             $prefixPackages
      a parameter whose declared type is in doubt keeps that reason    $doubtedParameter
    Term heads a supplied clause binds (probes/a7fix2/spec)
      an object's inherited members, by kind, and an unsupplied parent $inheritedHeads
      the aliases an object's export clauses define                    $exportedHeads
      what a package object's parents pass on                          $packageObjectHeads
      the nearest package that has the prefix's first name             $nearestAnchor
      terms only: a type or a class of the name binds no head          $typeHeads
      no private member outside where it is accessible                 $privateHeads
    The names it cannot take keep their numbers
      a definition of the file, in its scope or an outer one          $fileDefinitions
      a definition in another block of the file's package             $siblingBlocks
      an explicit import and a supplied wildcard of the same scope   $sameScopeImports
      a supplied namespace that declares nothing of the name           $suppliedNamespace
    Listed stdlib namespaces bind what the table lists
      CollectionConverters and scala.quoted bind no source name        $listedKeep
      nor a builtin a constructor slot reads, unlike `zz.*`            $listedConstructors
      quotes.reflect, whose members are not listed, may bind any       $reflectRefuses
      quotes.reflect binds no root spelling the model reads            $reflectRoots
      a reflect import in a class body is read without looping         $reflectInClass
    The B3 delta audit's rows (audit/ad931e2)
      ModifyF.map.map and Direct.first are refused                    $modifyF
      Optic.id is refused, a `_root_` twin keeps 1                     $opticId
  """

  private def inputs(sources: (String, String)*): List[MethodAnalysis.Input] =
    sources.toList.map { (path, text) =>
      MethodAnalysis.Input(path, dialects.Scala3(text).parse[Source].get)
    }

  private def counts(sources: (String, String)*): Map[String, Count] =
    MethodAnalysis.analyze(inputs(sources*)).map(e => e.name -> e.count).toMap

  private def refused(found: Count, reason: String): MatchResult[Count] =
    found must beLike { case Count.Unresolved(reasons) => reasons must contain(reason) }

  private val two =
    "p/Two.scala" -> "package p\ntype Two = Boolean\npackage compose:\n  type C = Boolean"

  private val q = "q/Two.scala" -> "package q\ntype Two = Boolean"
  private val pick = "def pick(x: Two): Two = x"

  // `p.Two` is another file's package member: `import zzz.*` outranks it (scalac binds a `zzz`
  // member `Fst` over the package's in audit/ad931e2/probes/chunk-00/prec), so the name is not
  // read. Without the
  // wildcard, and with a `zzz` the sources supply that declares no `Two`, it is `p.Two` again: 4.
  def packageMember = {
    val found = counts(
      two,
      "p/U.scala" -> s"package p\nimport zzz.*\nobject U { $pick }",
      "p/V.scala" -> s"package p\nobject V { $pick }"
    )
    val supplied = counts(
      two,
      "zzz/N.scala" -> "package zzz\nobject Neutral",
      "p/U.scala" -> s"package p\nimport zzz.*\nobject U { $pick }"
    )
    refused(found("p.U.pick"), "unresolved type: Two (may be imported by zzz.*)")
      .and(found("p.V.pick") === Count.Finite(4))
      .and(supplied("p.U.pick") === Count.Finite(4))
  }

  // A record's field is read where the field is written (capability records, D20): `WBox`'s file
  // imports `zzz.*`, so its `Two` is not read even from a file without the wildcard — `Box`'s is.
  def recordField = {
    val found = counts(
      two,
      "p/Box.scala" -> "package p\nfinal class Box(val v: Two)",
      "p/WBox.scala" -> "package p\nimport zzz.*\nfinal class WBox(val v: Two)",
      "p/V.scala" -> "package p\nobject V { def get(b: Box): Two = b.v; def wget(b: WBox): Two = b.v }"
    )
    (found("p.V.get") === Count.Finite(4))
      .and(refused(found("p.V.wget"), "unresolved type: Two (may be imported by zzz.*)"))
      .and(refused(found("p.WBox.<init>"), "unresolved type: Two (may be imported by zzz.*)"))
  }

  // `compose` is package `p.compose` unless `zzz` declares a term `compose`, which the wildcard
  // written before `import compose.*` would bind instead (scalac: probes/a7/prec/p5.scala binds
  // `zzz.v.compose.Fst`). `_root_.p.compose.*` cannot be rebound: 4.
  def rebindsPrefix = {
    val found = counts(
      two,
      "p/U.scala" -> "package p\nimport zzz.*\nimport compose.*\nobject U { def pick(x: C): C = x }",
      "p/R.scala" -> "package p\nimport zzz.*\nimport _root_.p.compose.*\nobject R { def pick(x: C): C = x }"
    )
    refused(
      found("p.U.pick"),
      "unresolved type: C (may be imported by compose.*: compose may be imported by zzz.*)"
    ).and(found("p.R.pick") === Count.Finite(4))
  }

  // The first name of an import's path, or of a qualified type, binds like any identifier: after
  // `import zzz.*`, `q` may be `zzz.q` (scalac: probes/a7/prec/p6.scala and probes/a7/plain bind
  // the wildcard's `dev` over the root package). `_root_.q` is the root package: 4.
  def rebindsHead = {
    val found = counts(
      q,
      "a/U.scala" -> s"package a\nimport zzz.*\nimport q.Two\nobject U { $pick }",
      "a/R.scala" -> s"package a\nimport zzz.*\nimport _root_.q.Two\nobject R { $pick }",
      "a/Q.scala" ->
        """|package a
           |import zzz.*
           |object Q {
           |  def pick(x: q.Two): q.Two = x
           |  def rooted(x: _root_.q.Two): _root_.q.Two = x
           |}""".stripMargin
    )
    refused(
      found("a.U.pick"),
      "unresolved type: Two (imported from q.Two, q may be imported by zzz.*)"
    )
      .and(found("a.R.pick") === Count.Finite(4))
      .and(refused(found("a.Q.pick"), "unresolved type: q.Two (q may be imported by zzz.*)"))
      .and(found("a.Q.rooted") === Count.Finite(4))
  }

  // Wildcards rank alike, so an inner scope's shadows an outer scope's (scalac: probes/a7/prec/p8
  // binds the inner `zzz.w.Fst`).
  def shadowsOuterWildcard = {
    val found = counts(
      "s/U.scala" ->
        s"""|package s
            |object a { type Two = Boolean }
            |import a.*
            |object U {
            |  import zzz.*
            |  $pick
            |}
            |object V { $pick }""".stripMargin
    )
    refused(found("s.U.pick"), "unresolved type: Two (may be imported by zzz.*)")
      .and(found("s.V.pick") === Count.Finite(4))
  }

  // `import zzz.{Boolean as _, *}` leaves `Boolean` to the root imports, but `compose` in the
  // later `import compose.*` may be `zzz.compose`, whose wildcard may bind it (scalac:
  // probes/review-A7/r8 binds `zzz.compose.Boolean`, an `Int`), and so may
  // `scala.jdk.CollectionConverters.*` when `scala` may be `zzz.scala` (r5): the builtin reading
  // asks the lookup about the prefix's first name. `_root_.p.compose` cannot be rebound: 4.
  def doubtedBuiltins = {
    val hidden = "import zzz.{Boolean as _, *}"
    val found = counts(
      two,
      "p/U.scala" -> s"package p\n$hidden\nimport compose.*\nobject U { def f(b: Boolean): Boolean = b }",
      "p/C.scala" ->
        s"package p\n$hidden\nimport scala.jdk.CollectionConverters.*\nobject C { def f(b: Boolean): Boolean = b }",
      "p/R.scala" ->
        s"package p\n$hidden\nimport _root_.p.compose.*\nobject R { def f(b: Boolean): Boolean = b }"
    )
    refused(
      found("p.U.f"),
      "builtin name Boolean may be shadowed through a reference not supplied: compose (wildcard " +
        "import of compose: compose may be imported by zzz.*)"
    ).and(
      refused(
        found("p.C.f"),
        "builtin name Boolean may be shadowed through a reference not supplied: " +
          "scala.jdk.CollectionConverters (wildcard import of scala.jdk.CollectionConverters: " +
          "scala may be imported by zzz.*)"
      )
    ).and(found("p.R.f") === Count.Finite(4))
  }

  // A `given` selector binds the given instances of its prefix, terms of any name: `zzz.given` may
  // bind `compose` (scalac: probes/review-A7/r11 binds a given `zzz.compose` over package
  // `p.compose`), a supplied `Lib.given` binds Lib's given `compose` — a value, not followed —
  // and `Quiet.given`, whose given is anonymous, binds no `compose`: `C = Boolean`, 4. A wildcard
  // binds no given (Scala 3 reference, given imports): under `import Lib.*`, 4 too.
  def givenSelectors = {
    val found = counts(
      two,
      "p/Lib.scala" ->
        "package p\ntrait Comp { type C = Unit }\nobject Lib { given compose: Comp = new Comp {} }\nobject Quiet { given Comp = new Comp {} }",
      "p/U.scala" -> "package p\nimport zzz.given\nimport compose.*\nobject U { def g(c: C): C = c }\nfinal case class K(c: C)",
      "p/L.scala" -> "package p\nimport Lib.given\nimport compose.*\nobject L { def g(c: C): C = c }",
      "p/V.scala" -> "package p\nimport Quiet.given\nimport compose.*\nobject V { def g(c: C): C = c }",
      "p/W.scala" -> "package p\nimport Lib.*\nimport compose.*\nobject W { def g(c: C): C = c }"
    )
    val doubt =
      "unresolved type: C (may be imported by compose.*: compose may be imported by zzz.given)"
    refused(found("p.U.g"), doubt)
      .and(refused(found("p.K.<init>"), doubt))
      .and(refused(found("p.L.g"), "path through a value not followed: compose"))
      .and(found("p.V.g") === Count.Finite(4))
      .and(found("p.W.g") === Count.Finite(4))
  }

  // A supplied wildcard binds what its namespace holds under the name, a value included (scalac:
  // r15 binds `val Lib.compose` over package `p.compose`), and an enum companion's wildcard binds
  // its case classes (r16 binds the case `E.A` over another file's `type A`): neither is read. A
  // case with neither parameters nor type parameters is a value, no type (probes/a7fix2/spec/cases
  // compiles `val b: B = true`, `val d: D = true` under `import E.*`): `B`, `D` stay `Unit`, 1.
  def valuesAndCases = {
    val found = counts(
      "p/Lib.scala" ->
        "package p\ntrait Comp { type C = Boolean }\nobject Lib { val compose: Comp = new Comp {} }\npackage compose:\n  type C = Unit",
      "p/E.scala" ->
        "package p\nenum E { case A(b: Boolean); case B; case D extends E }\ntype A = Unit\ntype B = Unit\ntype D = Unit",
      "p/U.scala" -> "package p\nimport Lib.*\nimport compose.*\nobject U { def pick(x: C): C = x }",
      "p/V.scala" ->
        "package p\nimport E.*\nobject V { def pick(x: A): A = x; def b(x: B): B = x; def d(x: D): D = x }",
      "p/W.scala" -> "package p\nobject W { def pick(x: A): A = x }"
    )
    refused(found("p.U.pick"), "path through a value not followed: compose")
      .and(refused(found("p.V.pick"), "enum case type not read: p.E.A"))
      .and(found("p.V.b") === Count.Finite(1))
      .and(found("p.V.d") === Count.Finite(1))
      .and(found("p.W.pick") === Count.Finite(1))
  }

  // `import Outer3.this.o.Two` names a member of a value (scalac: r12 binds `o.Two`, an `Int`, over
  // the package's `Two`), and its wildcard twin may bind any name. A selector naming another member
  // binds nothing else (probes/a7fix2/spec/outer5 compiles `val w: Two = true`): 4.
  def unfollowedPrefixes = {
    val found = counts(
      two,
      "p/O.scala" ->
        """|package p
           |final class Lib { type Two = Int; type Other = Unit }
           |final class Outer3(val o: Lib) {
           |  import Outer3.this.o.Two
           |  def pick(x: Two): Two = x
           |}
           |final class Outer4(val o: Lib) {
           |  import Outer4.this.o.*
           |  def pick(x: Two): Two = x
           |}
           |final class Outer5(val o: Lib) {
           |  import Outer5.this.o.Other
           |  def pick(x: Two): Two = x
           |}""".stripMargin
    )
    refused(found("p.Outer3.pick"), "path through a value not followed: Outer3.this.o")
      .and(
        refused(found("p.Outer4.pick"), "unresolved type: Two (may be imported by Outer4.this.o.*)")
      )
      .and(found("p.Outer5.pick") === Count.Finite(4))
  }

  // A package no file declares a member in — `zzz`, spelled only by `package zzz.inner` — holds
  // whatever a dependency declares there (scalac: r14/jar binds a `zzz.Two` from another unit):
  // its wildcard may bind `Two`. With a member declared in `zzz` itself it is read as complete: 4.
  def prefixPackages = {
    val use =
      "p/U.scala" -> s"package p\nimport zzz.*\nobject U { $pick }\nfinal case class K(t: Two)"
    val open = counts(two, use, "zzz/inner/M.scala" -> "package zzz.inner\nobject Mine")
    val closed = counts(two, use, "zzz/M.scala" -> "package zzz\nobject Mine")
    val doubt = "unresolved type: Two (may be imported by zzz.*)"
    refused(open("p.U.pick"), doubt)
      .and(refused(open("p.K.<init>"), doubt))
      .and(closed("p.U.pick") === Count.Finite(4))
      .and(closed("p.K.<init>") === Count.Finite(2))
  }

  // Another block of the same package in the reading file holds a definition of that compilation
  // unit, which outranks every wildcard (spec ch. 2; scalac: r9 reads it over `zzz.Two = Int`),
  // as a simple name and as a path's first name.
  def siblingBlocks = {
    val found = counts(
      "s/S.scala" ->
        s"""|package s {
            |  type Two = Boolean
            |  object q { type Two = Boolean }
            |}
            |package s {
            |  import zzz.*
            |  object U {
            |    $pick
            |    def path(x: q.Two): q.Two = x
            |  }
            |}""".stripMargin
    )
    (found("s.U.pick") === Count.Finite(4)).and(found("s.U.path") === Count.Finite(4))
  }

  // A parameter's declared type the lookup refuses leaves its path `rc.Out` refused for that reason
  // alone (review-A7-quality/q): the path is not read again as a name.
  def doubtedParameter = {
    val found = counts(
      "q/RC.scala" -> "package q\npackage compose\ntrait RC[F] { type Out[S, C] }",
      "q/V.scala" ->
        "package q\nimport zzz.*\nimport compose.*\ntrait V[S] { def andThen[C, F](using rc: RC[F]): rc.Out[S, C] }"
    )
    found("q.V.andThen") === Count.Unresolved(
      List("unresolved type: RC (may be imported by compose.*: compose may be imported by zzz.*)")
    )
  }

  // Term heads (`TermMembers`): every fixture compiles with Scala 3.9.0 (probes/a7fix2/spec, with
  // a `zzz` stub declaring `trait Base` for scalac only), and its `val w` pins the binding. `C =
  // Int` inside `Lib`'s namespaces, `Boolean` in the packages of the same names: `|Int => Int| =
  // 2^(32·2^32) = 2^(2^37)` where the clause binds the head, 4 where it does not.
  private val packages =
    "p/P.scala" ->
      "package p\npackage compose:\n  type C = Boolean\npackage two:\n  type C = Boolean\npackage other:\n  type C = Boolean"

  private val intEndo = Count.Symbolic(NaturalSyntax.pow(2, BigInt(2).pow(37)))

  // `Lib` inherits an object `two`, a given `compose` and a value `other`: a wildcard binds `two`
  // (read) and `other` (refused), no given; a `given` selector binds `compose` (refused). A parent
  // outside the sources may declare any name.
  def inheritedHeads = {
    val found = counts(
      packages,
      "p/Lib.scala" ->
        """|package p
           |trait Base { given compose: Ops = new Ops; val other: Ops = new Ops; object two { type C = Int } }
           |class Ops { type C = Int }
           |object Lib extends Base
           |object Lib2 extends zzz.Base""".stripMargin,
      "p/U.scala" ->
        """|package p
           |object W { import Lib.*; def h(x: two.C): two.C = x }
           |object X { import Lib.*; import two.*; def h(x: C): C = x }
           |object G { import Lib.*; def h(x: compose.C): compose.C = x }
           |object V { import Lib.given; def h(x: compose.C): compose.C = x }
           |object T { import Lib.*; def h(x: other.C): other.C = x }
           |object F { import Lib2.*; def h(x: compose.C): compose.C = x }""".stripMargin
    )
    (found("p.W.h") === intEndo)
      .and(found("p.X.h") === intEndo)
      .and(found("p.G.h") === Count.Finite(4))
      .and(refused(found("p.V.h"), "path through a value not followed: compose"))
      .and(refused(found("p.T.h"), "path through a value not followed: other"))
      .and(
        refused(
          found("p.F.h"),
          "inherited term member not resolved: compose (unsupplied parent zzz.Base)"
        )
      )
  }

  // `export Other.*` and `export Other.compose` define `compose` (with its `Boolean = Int`); the
  // wildcard export defines no given, which `export Other.given` does, for a `given` selector.
  def exportedHeads = {
    val found = counts(
      "p/P.scala" -> "package p\npackage compose:\n  type C = Boolean\npackage gift:\n  type C = Boolean",
      "p/Lib.scala" ->
        """|package p
           |class Ops { type C = Int }
           |object Other { object compose { type C = Int; type Boolean = Int }; given gift: Ops = new Ops }
           |object Lib { export Other.* }
           |object Lib2 { export Other.compose }
           |object Lib3 { export Other.given }""".stripMargin,
      "p/U.scala" ->
        """|package p
           |object U { import Lib.*; def h(x: compose.C): compose.C = x }
           |object V { import Lib.*; import compose.*; def g(x: Boolean): Boolean = x }
           |object W { import Lib2.*; def h(x: compose.C): compose.C = x }
           |object Y { import Lib.*; def h(x: gift.C): gift.C = x }
           |object Z { import Lib3.given; def h(x: gift.C): gift.C = x }""".stripMargin
    )
    (found("p.U.h") === intEndo)
      .and(found("p.V.g") === intEndo)
      .and(found("p.W.h") === intEndo)
      .and(found("p.Y.h") === Count.Finite(4))
      .and(refused(found("p.Z.h"), "path through a value not followed: gift"))
  }

  // `package object lib extends r.Base` passes `Two` and `compose` on to package `q.lib`; one
  // extending an unsupplied parent may pass any name on.
  def packageObjectHeads = {
    val found = counts(
      "r/Base.scala" -> "package r\ntrait Base { type Two = Int; object compose { type C = Int } }",
      "q/lib.scala" -> "package q\npackage object lib extends r.Base\npackage object lib2 extends zzz.Base",
      "q/Thing.scala" -> "package q.lib\nfinal case class Thing(b: Boolean)",
      "q/Thing2.scala" -> "package q.lib2\nfinal case class Thing2(b: Boolean)",
      "p/P.scala" -> "package p\ntype Two = Boolean\npackage compose:\n  type C = Boolean",
      "p/U.scala" ->
        """|package p
           |object U { import q.lib.*; def f(x: Two): Two = x; def h(x: compose.C): compose.C = x }
           |object V { import q.lib2.*; def f(x: Two): Two = x }""".stripMargin
    )
    (found("p.U.f") === intEndo)
      .and(found("p.U.h") === intEndo)
      .and(
        refused(
          found("p.V.f"),
          "inherited type member not resolved: Two (unsupplied parent zzz.Base)"
        )
      )
  }

  // In package `a.b`, `c` is the package `a.b.c` the sources declare, which has neither `Two` nor
  // `compose`, whatever the root package `c` holds: `a.b.Two` and `a.b.compose.C`, 4 each.
  def nearestAnchor = {
    val found = counts(
      "f.scala" ->
        """|package a.b.c:
           |  type Other = Unit
           |package c:
           |  type Two = Int
           |  object compose { type C = Int }
           |package a.b:
           |  type Two = Boolean
           |  object U {
           |    import c.*
           |    def f(x: Two): Two = x
           |    def h(x: compose.C): compose.C = x
           |  }
           |package a.b.compose:
           |  type C = Boolean""".stripMargin
    )
    (found("a.b.U.f") === Count.Finite(4)).and(found("a.b.U.h") === Count.Finite(4))
  }

  // `Lib` declares a type `compose` and a class `two`, no term of either name: the heads are the
  // packages `p.compose` and `p.two`.
  def typeHeads = {
    val found = counts(
      packages,
      "p/U.scala" ->
        """|package p
           |object Lib { type compose = Ops; class two { type C = Int } }
           |class Ops { type C = Int }
           |object U { import Lib.*; def h(x: compose.C): compose.C = x; def t(x: two.C): two.C = x }
           |object V { import Lib.*; import two.*; def t(x: C): C = x }""".stripMargin
    )
    (found("p.U.h") === Count.Finite(4))
      .and(found("p.U.t") === Count.Finite(4))
      .and(found("p.V.t") === Count.Finite(4))
  }

  // A private member is no import outside the object: neither the object `compose`, the value
  // `other`, nor the given `compose` under a `given` selector.
  def privateHeads = {
    val found = counts(
      packages,
      "p/U.scala" ->
        """|package p
           |class Ops { type C = Int }
           |object Lib { private object compose { type C = Int }; private val other: Ops = new Ops }
           |object Gifts { private given compose: Ops = new Ops }
           |object U { import Lib.*; def h(x: compose.C): compose.C = x; def o(x: other.C): other.C = x }
           |object V { import Gifts.given; def h(x: compose.C): compose.C = x }""".stripMargin
    )
    (found("p.U.h") === Count.Finite(4))
      .and(found("p.U.o") === Count.Finite(4))
      .and(found("p.V.h") === Count.Finite(4))
  }

  // A definition of the reading file outranks every wildcard; one in an outer scope, against an
  // inner wildcard that binds the name, is E049 (probes/a7/prec/neg/n1), so where the program
  // compiles the definition is the name. An enum keeps builtin names out of the reading.
  def fileDefinitions = {
    val found = counts(
      "d/U.scala" -> s"package d\nimport zzz.*\nenum Two { case A, B }\nobject U { $pick }",
      "e/U.scala" -> s"package e\nenum Two { case A, B }\nobject U {\n  import zzz.*\n  $pick\n}"
    )
    (found("d.U.pick") === Count.Finite(4)).and(found("e.U.pick") === Count.Finite(4))
  }

  // A named import outranks a wildcard of its scope, and two wildcards of one scope that both
  // bind a name are E049 (probes/a7/prec/neg/n3): `q.Two` stands in both.
  def sameScopeImports = {
    val found = counts(
      q,
      "f/U.scala" -> s"package f\nimport _root_.q.Two\nimport zzz.*\nobject U { $pick }",
      "g/U.scala" -> s"package g\nimport _root_.q.*\nimport zzz.*\nobject U { $pick }",
      "h/U.scala" -> s"package h\nimport q.*\nimport zzz.*\nobject U { $pick }"
    )
    (found("f.U.pick") === Count.Finite(4))
      .and(found("g.U.pick") === Count.Finite(4))
      .and(found("h.U.pick") === Count.Finite(4))
  }

  // A supplied `zzz` declaring no `Two` and no `q` binds neither: `q.Two` is the root package's.
  // A package some file declares a member in is read as complete (the closed-package assumption).
  def suppliedNamespace = {
    val found = counts(
      q,
      "zzz/N.scala" -> "package zzz\nobject Neutral",
      "k/U.scala" -> s"package k\nimport zzz.*\nimport q.Two\nobject U { $pick }"
    )
    found("k.U.pick") === Count.Finite(4)
  }

  // `scala.jdk.CollectionConverters` declares its conversion classes and `scala.quoted` its six
  // types, eight terms (scala-library 3.9.0 class files): neither declares `Two`, so the package
  // member stands.
  def listedKeep = {
    val found = counts(
      two,
      "p/C.scala" -> s"package p\nimport scala.jdk.CollectionConverters.*\nobject C { $pick }",
      "p/Q.scala" -> s"package p\nimport scala.quoted.*\nobject Q { $pick }"
    )
    (found("p.C.pick") === Count.Finite(4)).and(found("p.Q.pick") === Count.Finite(4))
  }

  // The method side reads constructor slots through the table too (eo's avro constructors): the
  // conversions declare no `Any`, `Array` or `Int`, so `WalkRes` holds `1 · ω · 2^32` = ω values
  // and `DirectPlan` 2^32; under the unsupplied `zz.*` the builtin `Int` is refused.
  def listedConstructors = {
    val found = counts(
      "p/W.scala" ->
        """|package p
           |import scala.jdk.CollectionConverters.*
           |final class WalkRes(val cur: Any, val parents: Array[Any], val n: Int)
           |final case class DirectPlan(slot: Int)""".stripMargin,
      "p/Z.scala" -> "package p\nimport zz.*\nfinal case class ZPlan(slot: Int)"
    )
    (found("p.WalkRes.<init>").render === "ω")
      .and(found("p.DirectPlan.<init>") === Count.Finite(BigInt(2).pow(32)))
      .and(
        refused(
          found("p.ZPlan.<init>"),
          "builtin name Int may be shadowed through a reference not supplied: zz (wildcard import of zz)"
        )
      )
  }

  // `quotes.reflect.*` is `Quotes#reflectModule`'s members, which the table does not list: any
  // name but a root spelling may be one of them.
  def reflectRefuses = {
    val found = counts(
      two,
      "p/R.scala" ->
        s"""|package p
            |import scala.quoted.*
            |object R {
            |  def outer(using Quotes): Unit = {
            |    import quotes.reflect.*
            |    $pick
            |    ()
            |  }
            |}""".stripMargin
    )
    refused(found("p.R.outer.pick"), "unresolved type: Two (may be imported by quotes.reflect.*)")
  }

  // Of the names the root imports bind, reflect declares only `Singleton` and `Symbol` (its 323
  // type members, scala-library 3.9.0 TASTy), so a root spelling the model reads is not its:
  // another file's `type Boolean = Unit` of the package stands (scalac: probes/a7fix1/root
  // compiles `val w: Boolean = ()` there), `Unit => Unit`, 1 — as on the stored side.
  def reflectRoots = {
    val found = counts(
      "p/Bool.scala" -> "package p\ntype Boolean = Unit",
      "p/R.scala" ->
        """|package p
           |import scala.quoted.*
           |object R {
           |  def outer(using Quotes): Unit = {
           |    import quotes.reflect.*
           |    def pick(x: Boolean): Boolean = x
           |    ()
           |  }
           |}""".stripMargin
    )
    found("p.R.outer.pick") === Count.Finite(1)
  }

  // A clause's prefix is read where the clause stands even when the reading has no position:
  // `quotes.reflect.*` in a class body once asked after its own prefix forever (r13).
  def reflectInClass = {
    val found = counts(
      "r/M.scala" ->
        """|package r
           |import scala.quoted.*
           |class M(using val q: Quotes) {
           |  import quotes.reflect.*
           |  def two(a: Boolean): Boolean = a
           |}""".stripMargin
    )
    refused(
      found("r.M.two"),
      "outside the counting model: scala.quoted.Quotes (compile-time reflection)"
    )
  }

  // audit/ad931e2/probes/chunk-00/modifyf2 and verify-indep/modifyf.scala, `cats` as `zzz`:
  // `Fst` is another file's match type, which `compose.*` may shadow — its `compose` itself bound
  // perhaps by `zzz.instances.function.*`. The control imports `Fst` by name: 2.
  def modifyF = {
    val found = counts(
      "dev/p/affine.scala" ->
        "package dev.p\npackage data\n\ntype Fst[T] = T match\n  case (f, s) => f\n\ntype Snd[T] = T match\n  case (f, s) => s",
      "dev/p/compose.scala" -> "package dev.p\npackage compose\n\ntrait Composer[F[_, _], G[_, _]]",
      "dev/p/forgetful.scala" ->
        "package dev.p\npackage forgetful\n\ntrait ForgetfulFunctor[F[_, _]]:\n  def map[X, A, B](fa: F[X, A], f: A => B): F[X, B]",
      "dev/p/modifyf.scala" ->
        """|package dev.p
           |package data
           |
           |import zzz.instances.function.*
           |import zzz.syntax.functor.*
           |
           |import forgetful.*
           |import compose.*
           |
           |class ModifyF[A, B](val modifier: (Fst[A], Snd[A] => B)) extends AnyVal
           |
           |object ModifyF:
           |
           |  given map: ForgetfulFunctor[ModifyF] with
           |
           |    def map[X, B, C](fa: ModifyF[X, B], f: B => C): ModifyF[X, C] =
           |      val inner = fa.modifier._2
           |      ModifyF(fa.modifier._1, a => f(inner(a)))
           |
           |object Direct:
           |  def first[X](p: Fst[X], q: Fst[X]): Fst[X] = p""".stripMargin,
      "dev/p/ctl.scala" ->
        "package dev.p\npackage ctl\n\nimport dev.p.data.Fst\n\nobject Ctl:\n  def first[X](p: Fst[X], q: Fst[X]): Fst[X] = p"
    )
    val fst =
      "unresolved type: Fst (may be imported by compose.*: compose may be imported by zzz.instances.function.*)"
    refused(
      found("dev.p.data.ModifyF.map.map"),
      "unresolved type: ForgetfulFunctor (may be imported by compose.*: compose may be imported " +
        "by zzz.instances.function.*)"
    )
      .and(refused(found("dev.p.data.ModifyF.<init>"), fst))
      .and(refused(found("dev.p.data.Direct.first"), fst))
      .and(found("dev.p.ctl.Ctl.first") === Count.Finite(2))
  }

  // audit/ad931e2/synth/optic-id, with a `_root_` twin: `BijectionIso` is another file's class of
  // the package, which `zzz.syntax.functor.*` may shadow, and so may it the head `dev` of the
  // qualified spelling (scalac: probes/a7/plain binds `zzz.syntax.functor.dev.r.optics`). Only the
  // `_root_` spelling is read: (A ⇒ A) × (A ⇒ A) has one inhabitant.
  def opticId = {
    val found = counts(
      "dev/r/iso.scala" ->
        """|package dev.r
           |package optics
           |
           |final class BijectionIso[S, T, A, B](read: S => A, build: B => T) {
           |  def get(s: S): A = read(s)
           |  def reverseGet(b: B): T = build(b)
           |}
           |
           |object Iso {
           |  def apply[S, T, A, B](f: S => A, g: B => T): BijectionIso[S, T, A, B] = new BijectionIso(f, g)
           |}""".stripMargin,
      "dev/r/optic.scala" ->
        """|package dev.r
           |package optics
           |
           |import zzz.syntax.functor.*
           |
           |object Optic {
           |  def id[A]: BijectionIso[A, A, A, A] = Iso[A, A, A, A](identity, identity)
           |}
           |
           |object Plain {
           |  def id[A]: dev.r.optics.BijectionIso[A, A, A, A] = Iso[A, A, A, A](identity, identity)
           |}
           |
           |object Rooted {
           |  def id[A]: _root_.dev.r.optics.BijectionIso[A, A, A, A] = Iso[A, A, A, A](identity, identity)
           |}""".stripMargin
    )
    refused(
      found("dev.r.optics.Optic.id"),
      "unresolved type: BijectionIso (may be imported by zzz.syntax.functor.*)"
    )
      .and(
        refused(
          found("dev.r.optics.Plain.id"),
          "unresolved type: dev.r.optics.BijectionIso (dev may be imported by zzz.syntax.functor.*)"
        )
      )
      .and(found("dev.r.optics.Rooted.id") === Count.Finite(1))
      .and(found("dev.r.optics.Iso.apply") === Count.Finite(1))
  }

}
