package cardinality.analysis

import cardinality.Pending
import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** Wildcard imports of namespaces the sources do not supply (register D11). A wildcard ranks above
  * a package member of another file and above a root package, so `import zzz.*` could bind a type
  * name or a path's first name such as `compose` in `import compose.*`. Every fixture compiles with
  * Scala 3.9.0 against a `zzz` stub that declares nothing of the names (probes/a7/spec,
  * probes/a7/prec), and the TASTy front end reads the name the compiler bound (D3): `pick(x: Two):
  * Two` with `Two = Boolean` counts `|Boolean → Boolean| = 4` wherever `Two` is the package's.
  */
class UnsuppliedWildcardSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    A wildcard over a namespace no source supplies binds what the compiler bound (D3)
      another file's package member the stub does not declare          $packageMember
      a record's field type, wherever the record is written            $recordField
      the type a projection reads its member from                      $projection
      a prefix, and a name under that prefix                           $rebindsPrefix
      the first name of a named import or a qualified type             $rebindsHead
      an outer scope's supplied wildcard                               $shadowsOuterWildcard
      a builtin a wildcard behind a doubted prefix may bind            $doubtedBuiltins
      refuses Singleton for the marker where a wildcard may bind it    ${Pending.knownLoss(
      singletonMarker
    )}
    Selectors and prefixes, as the compiler bound them
      a `given` selector may bind a path's first name                  $givenSelectors
      a supplied wildcard binding a value, an enum case                $valuesAndCases
      an enum companion's wildcard binds a case class                  ${Pending.knownLoss(
      caseClassType
    )}
      an import whose prefix is a value                                $unfollowedPrefixes
      a package the sources spell only as a prefix                     $prefixPackages
      a parameter whose path type has no producer                      $doubtedParameter
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
      nor a builtin a constructor slot reads, nor `zz.*`               $listedConstructors
      quotes.reflect, whose members are not listed, may bind any       ${Pending.knownLoss(
      reflectRefuses
    )}
      quotes.reflect binds no root spelling the model reads            $reflectRoots
      a reflect import in a class body is read without looping         $reflectInClass
    The B3 delta audit's rows (audit/ad931e2)
      ModifyF.map.map and Direct.first are counted                     $modifyF
      Optic.id and its qualified twin keep 1                           $opticId
  """

  // The namespaces no fixture supplies, declaring nothing a fixture names: every fixture is
  // compiled against them and the analysis is not given them (`Compiled.withheldFile`).
  private val unsupplied = cardinality.Compiled.withheldFile(
    """|package zzz {
       |  object Unsupplied
       |  package syntax.functor { object Unsupplied }
       |  package instances.function { object Unsupplied }
       |}
       |package zz { object Unsupplied }""".stripMargin
  )

  private def counts(sources: (String, String)*): Map[String, Count] =
    cardinality.Compiled
      .analyze(cardinality.Compiled.inputs((sources :+ unsupplied)*))
      .map(e => e.name -> e.count)
      .toMap

  // `zzz.Base`, a parent no fixture supplies.
  private val base = cardinality.Compiled.withheld("package zzz\ntrait Base")

  private def refused(found: Count, reason: String): MatchResult[Count] =
    found must beLike { case Count.Unresolved(reasons) => reasons must contain(reason) }

  private val two =
    "p/Two.scala" -> "package p\ntype Two = Boolean\npackage compose:\n  type C = Boolean"

  private val q = "q/Two.scala" -> "package q\ntype Two = Boolean"
  private val pick = "def pick(x: Two): Two = x"

  // `p.Two` is another file's package member, which `import zzz.*` would outrank had `zzz` a `Two`
  // (audit/ad931e2/probes/chunk-00/prec). The stub has none, so the compiler binds `p.Two`, and its
  // binding is read (D3): `Boolean => Boolean`, 4, as without the wildcard or with a supplied `zzz`.
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
    (found("p.U.pick") === Count.Finite(4))
      .and(found("p.V.pick") === Count.Finite(4))
      .and(supplied("p.U.pick") === Count.Finite(4))
  }

  // A record's field is read where the field is written (capability records, D20): `WBox`'s file
  // imports `zzz.*`, and its `Two` is still the compiler's `p.Two`. Each getter is `Boolean =>
  // Boolean`, 4; the constructor builds one value per argument, 2.
  def recordField = {
    val found = counts(
      two,
      "p/Box.scala" -> "package p\nfinal class Box(val v: Two)",
      "p/WBox.scala" -> "package p\nimport zzz.*\nfinal class WBox(val v: Two)",
      "p/V.scala" -> "package p\nobject V { def get(b: Box): Two = b.v; def wget(b: WBox): Two = b.v }"
    )
    (found("p.V.get") === Count.Finite(4))
      .and(found("p.V.wget") === Count.Finite(4))
      .and(found("p.WBox.<init>") === Count.Finite(2))
  }

  // A projection `H#M` reads `H` as the compiler bound it: `H#M` = Boolean, 4, under `zzz.*` too
  // (probes/a7/proj).
  def projection = {
    val found = counts(
      "p/H.scala" -> "package p\ntrait H { type M = Boolean }",
      "p/U.scala" -> "package p\nimport zzz.*\nobject U { def pick(x: H#M): H#M = x }",
      "p/V.scala" -> "package p\nobject V { def pick(x: H#M): H#M = x }"
    )
    (found("p.U.pick") === Count.Finite(4)).and(found("p.V.pick") === Count.Finite(4))
  }

  // `compose` is package `p.compose` unless `zzz` declares a term `compose`, which the wildcard
  // written before `import compose.*` would bind instead (scalac: probes/a7/prec/p5.scala binds
  // `zzz.v.compose.Fst`). The stub declares none: `C` = Boolean, 4, as `_root_.p.compose.*`.
  def rebindsPrefix = {
    val found = counts(
      two,
      "p/U.scala" -> "package p\nimport zzz.*\nimport compose.*\nobject U { def pick(x: C): C = x }",
      "p/R.scala" -> "package p\nimport zzz.*\nimport _root_.p.compose.*\nobject R { def pick(x: C): C = x }"
    )
    (found("p.U.pick") === Count.Finite(4)).and(found("p.R.pick") === Count.Finite(4))
  }

  // The first name of an import's path, or of a qualified type, binds like any identifier: after
  // `import zzz.*`, `q` may be `zzz.q` (scalac: probes/a7/prec/p6.scala and probes/a7/plain bind
  // the wildcard's `dev` over the root package). The stub declares no `q`: the root package's
  // `q.Two` in each spelling, 4.
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
    (found("a.U.pick") === Count.Finite(4))
      .and(found("a.R.pick") === Count.Finite(4))
      .and(found("a.Q.pick") === Count.Finite(4))
      .and(found("a.Q.rooted") === Count.Finite(4))
  }

  // Wildcards rank alike, so an inner scope's would shadow an outer scope's (scalac:
  // probes/a7/prec/p8 binds the inner `zzz.w.Fst`); the stub has no `Two`, so `a.Two` stands: 4.
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
    (found("s.U.pick") === Count.Finite(4)).and(found("s.V.pick") === Count.Finite(4))
  }

  // `import zzz.{Boolean as _, *}` leaves `Boolean` to the root imports. `compose` in the later
  // `import compose.*` may be `zzz.compose`, and `scala` in `scala.jdk.CollectionConverters.*`
  // may be `zzz.scala`, whose wildcards could bind it (scalac: probes/review-A7/r8 binds
  // `zzz.compose.Boolean`, an `Int`; r5). Neither unsupplied namespace is assumed to (the
  // never-shadowed assumption), and the listed CollectionConverters declares no `Boolean`, so all
  // three are b, !b, true, false: 4.
  def doubtedBuiltins = {
    val hidden = "import zzz.{Boolean as _, *}"
    val found = counts(
      two,
      "p/U.scala" -> (s"package p\n$hidden\nimport compose.*\nobject U { def f(b: Boolean): Boolean = b }" +
        cardinality.Compiled.withheld("package zzz\nclass Boolean")),
      "p/C.scala" ->
        s"package p\n$hidden\nimport scala.jdk.CollectionConverters.*\nobject C { def f(b: Boolean): Boolean = b }",
      "p/R.scala" ->
        s"package p\n$hidden\nimport _root_.p.compose.*\nobject R { def f(b: Boolean): Boolean = b }"
    )
    (found("p.U.f") === Count.Finite(4))
      .and(found("p.C.f") === Count.Finite(4))
      .and(found("p.R.f") === Count.Finite(4))
  }

  // `O.type & Singleton` is `O.type` only where `Singleton` is `scala.Singleton`: a `zzz` declaring
  // `type Singleton = Nothing` would make it empty (scalac: probes/review-A7q/sc), but an unsupplied
  // namespace is assumed not to (the never-shadowed assumption): 1, as without the wildcard.
  // `quotes.reflect` does declare its own `Singleton` (probes/review-A7/r4/m.scala): refused.
  def singletonMarker = {
    val single = "def single(o: O.type): O.type & Singleton = o"
    val found = counts(
      "s/S.scala" ->
        s"""|package s
            |import scala.quoted.*
            |object O
            |object Plain { $single }
            |object W {
            |  import zzz.*
            |  $single
            |}
            |object Q {
            |  def outer(using Quotes): Unit = {
            |    import quotes.reflect.*
            |    ${single.replace("= o", "= ???")}
            |    ()
            |  }
            |}""".stripMargin
    )
    (found("s.Plain.single") === Count.Finite(1))
      .and(found("s.W.single") === Count.Finite(1))
      .and(
        refused(
          found("s.Q.outer.single"),
          "declaration not supplied: quotes.reflect.Singleton (wildcard import)"
        )
      )
  }

  // A `given` selector binds the given instances of its prefix, terms of any name: `zzz.given` may
  // bind `compose` (scalac: probes/review-A7/r11 binds a given `zzz.compose` over package
  // `p.compose`); the stub has none, so `C` = Boolean: 4, and the constructor 2. A supplied
  // `Lib.given` binds Lib's given `compose: Comp`, whose `C` = Unit: 1. `Quiet.given`, whose given
  // is anonymous, binds no `compose`: 4. A wildcard binds no given (Scala 3 reference, given
  // imports): under `import Lib.*`, 4 too.
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
    (found("p.U.g") === Count.Finite(4))
      .and(found("p.K.<init>") === Count.Finite(2))
      .and(found("p.L.g") === Count.Finite(1))
      .and(found("p.V.g") === Count.Finite(4))
      .and(found("p.W.g") === Count.Finite(4))
  }

  // A supplied wildcard binds what its namespace holds under the name, a value included (scalac:
  // r15 binds `val Lib.compose` over package `p.compose`), and an enum companion's wildcard binds
  // its case classes (r16 binds the case `E.A` over another file's `type A`). `compose.C` is then
  // `Comp`'s Boolean, 4. A case with neither parameters nor type parameters is a value, no type
  // (probes/a7fix2/spec/cases compiles `val b: B = true`, `val d: D = true` under `import E.*`):
  // `B`, `D` stay `Unit`, 1.
  private val enumCases = Seq(
    "p/Lib.scala" ->
      "package p\ntrait Comp { type C = Boolean }\nobject Lib { val compose: Comp = new Comp {} }\npackage compose:\n  type C = Unit",
    "p/E.scala" ->
      "package p\nenum E { case A(b: Boolean); case B; case D extends E }\ntype A = Unit\ntype B = Unit\ntype D = Unit",
    "p/U.scala" -> "package p\nimport Lib.*\nimport compose.*\nobject U { def pick(x: C): C = x }",
    "p/V.scala" ->
      "package p\nimport E.*\nobject V { def pick(x: A): A = x; def b(x: B): B = x; def d(x: D): D = x }",
    "p/W.scala" -> "package p\nobject W { def pick(x: A): A = x }"
  )

  def valuesAndCases = {
    val found = counts(enumCases*)
    (found("p.U.pick") === Count.Finite(4))
      .and(found("p.V.b") === Count.Finite(1))
      .and(found("p.V.d") === Count.Finite(1))
      .and(found("p.W.pick") === Count.Finite(1))
  }

  // The case `E.A`, built from one Boolean, has two values: `|E.A => E.A|` = 2^2 = 4. The TASTy
  // front end refuses the case class type (`enum case type not read`).
  def caseClassType = counts(enumCases*)("p.V.pick") === Count.Finite(4)

  // `import Outer3.this.o.Two` names a member of a value (scalac: r12 binds `o.Two`, an `Int`, over
  // the package's `Two`), as does its wildcard twin: `|Int => Int|` = 2^(2^37) each. A selector
  // naming another member binds nothing else (probes/a7fix2/spec/outer5 compiles
  // `val w: Two = true`): 4.
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
    (found("p.Outer3.pick") === intEndo)
      .and(found("p.Outer4.pick") === intEndo)
      .and(found("p.Outer5.pick") === Count.Finite(4))
  }

  // A package no file declares a member in — `zzz`, spelled only by `package zzz.inner` — holds
  // whatever a dependency declares there (scalac: r14/jar binds a `zzz.Two` from another unit).
  // Here the stub declares none, so the compiler binds `p.Two` either way: 4, the constructor 2.
  def prefixPackages = {
    val use =
      "p/U.scala" -> s"package p\nimport zzz.*\nobject U { $pick }\nfinal case class K(t: Two)"
    val open = counts(two, use, "zzz/inner/M.scala" -> "package zzz.inner\nobject Mine")
    val closed = counts(two, use, "zzz/M.scala" -> "package zzz\nobject Mine")
    (open("p.U.pick") === Count.Finite(4))
      .and(open("p.K.<init>") === Count.Finite(2))
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

  // `RC` is the compiler's `q.compose.RC` (review-A7-quality/q), and `rc.Out[S, C]` an abstract type
  // member nothing in scope produces: no implementation, 0.
  def doubtedParameter = {
    val found = counts(
      "q/RC.scala" -> "package q\npackage compose\ntrait RC[F] { type Out[S, C] }",
      "q/V.scala" ->
        "package q\nimport zzz.*\nimport compose.*\ntrait V[S] { def andThen[C, F](using rc: RC[F]): rc.Out[S, C] }"
    )
    found("q.V.andThen") === Count.Finite(0)
  }

  // Term heads: every fixture compiles with Scala 3.9.0 (probes/a7fix2/spec, with
  // a `zzz` stub declaring `trait Base` for scalac only), and its `val w` pins the binding. `C =
  // Int` inside `Lib`'s namespaces, `Boolean` in the packages of the same names: `|Int => Int| =
  // 2^(32·2^32) = 2^(2^37)` where the clause binds the head, 4 where it does not.
  private val packages =
    "p/P.scala" ->
      "package p\npackage compose:\n  type C = Boolean\npackage two:\n  type C = Boolean\npackage other:\n  type C = Boolean"

  private val intEndo = Count.Symbolic(NaturalSyntax.pow(2, BigInt(2).pow(37)))

  // `Lib` inherits an object `two`, a given `compose` and a value `other`: a wildcard binds `two`
  // and `other` (each `C` = Int), no given; a `given` selector binds `compose` (`C` = Int). The
  // stub parent `zzz.Base` declares nothing, so `Lib2.*` leaves `compose` the package: 4.
  def inheritedHeads = {
    val found = counts(
      packages,
      "p/Lib.scala" ->
        ("""|package p
           |trait Base { given compose: Ops = new Ops; val other: Ops = new Ops; object two { type C = Int } }
           |class Ops { type C = Int }
           |object Lib extends Base
           |object Lib2 extends zzz.Base""".stripMargin + base),
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
      .and(found("p.V.h") === intEndo)
      .and(found("p.T.h") === intEndo)
      .and(found("p.F.h") === Count.Finite(4))
  }

  // `export Other.*` and `export Other.compose` define `compose` (with its `Boolean = Int`); the
  // wildcard export defines no given (`gift` stays the package's, 4), which `export Other.given`
  // does, for a `given` selector (`C` = Int).
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
      .and(found("p.Z.h") === intEndo)
  }

  // `package object lib extends r.Base` passes `Two` and `compose` on to package `q.lib`; the stub
  // parent of `lib2` passes nothing, so `p.Two` stands: 4.
  def packageObjectHeads = {
    val found = counts(
      "r/Base.scala" -> "package r\ntrait Base { type Two = Int; object compose { type C = Int } }",
      "q/lib.scala" -> ("package q\npackage object lib extends r.Base\npackage object lib2 extends zzz.Base" + base),
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
      .and(found("p.V.f") === Count.Finite(4))
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
  // and `DirectPlan` 2^32. The stub `zz` declares no `Int`, so `ZPlan` is 2^32 too.
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
      .and(found("p.ZPlan.<init>") === Count.Finite(BigInt(2).pow(32)))
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
  // `Fst` is another file's match type, which `compose.*` would shadow had the stub a `compose`; it
  // has none. `Fst[X]` is stuck on its parameter, so each reads as an opaque atom: `first` picks
  // `p` or `q`, 2, as the control that imports `Fst` by name; `map` builds `(fa.modifier._1,
  // a => f(fa.modifier._2(a)))` alone, 1; the constructor, parametric in its slot, 1.
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
    (found("dev.p.data.ModifyF.map.map") === Count.Finite(1))
      .and(found("dev.p.data.ModifyF.<init>") === Count.Finite(1))
      .and(found("dev.p.data.Direct.first") === Count.Finite(2))
      .and(found("dev.p.ctl.Ctl.first") === Count.Finite(2))
  }

  // audit/ad931e2/synth/optic-id, with a `_root_` twin: `BijectionIso` is another file's class of
  // the package, which `zzz.syntax.functor.*` would shadow had the stub one, and so the head `dev`
  // of the qualified spelling (scalac: probes/a7/plain binds `zzz.syntax.functor.dev.r.optics`).
  // It has neither: every spelling is the class, (A ⇒ A) × (A ⇒ A), one inhabitant.
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
    (found("dev.r.optics.Optic.id") === Count.Finite(1))
      .and(found("dev.r.optics.Plain.id") === Count.Finite(1))
      .and(found("dev.r.optics.Rooted.id") === Count.Finite(1))
      .and(found("dev.r.optics.Iso.apply") === Count.Finite(1))
  }

}
