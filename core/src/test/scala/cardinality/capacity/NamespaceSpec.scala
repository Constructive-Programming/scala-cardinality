package cardinality.capacity

import scala.meta.*

import cardinality.analysis.StdlibModel
import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

// Names in the stored-value estimate (docs/research/stored-value-names.md): a type owns its name
// and an object is a term; objects and packages are namespaces; a qualified reference reads
// through its qualifier and nothing else; a qualifier's first name is the innermost term binding;
// imports and package clauses bind as Scala binds them, file by file; a type parameter shadows a
// sibling type; and a constructor's slot is read where the constructor is written. Every expected
// number is derived in the comment above it; a `?` names its class and reasons.
class NamespaceSpec extends Specification {

  import Rows.*

  def is = s2"""
    A type owns its name
      an open trait's companion does not stand in for the trait   $openTraitCompanion
      a sealed trait's companion does not stand in for its sum    $sealedCompanion
      a case-object child still counts once                       $caseObjectChild
      a case-object child is the module, whatever type shares its name  $caseObjectNamedLikeType
      a companion counts its own one value in a source total      $companionTotal

    Objects and packages are namespaces
      a class nested in an object resolves, in the file and across packages  $nestedMember
      nested objects resolve                                      $nestedObjects
      a recursion across an object boundary is one system         $crossObjectRecursion
      an object's import and its own types shadow the enclosing body's  $objectShadowing
      an open trait of an object is open                          $objectOpenTrait
      a generic member instantiates                               $objectGeneric
      a member is read with the reasons its value rests on        $objectReasons
      chained package clauses open the enclosing package          $packageClauses
      a package path resolves from the opened packages or the root  $packagePaths
      an inner package's subpackage shadows an outer package's object  $packageOrder
      an enum's cases are not read as a namespace                 $enumCases

    Each file of a package reads with what that file adds
      another file's explicit import is not lost across packages  $libraryImports
      another file's chained clauses resolve across packages      $libraryClauses
      a wildcard import names the package a name is read from     $wildcardImports
      an imported name is not open because another package's is   $importedOpenReason

    Class members and module members are apart
      an instance path reads the class, a module path the object  $membersByOwner
      a class member reads in the class's own body                $classMemberScope

    A qualifier's first name is the innermost term binding
      a value, a parameter or an inner object shadows an outer object  $qualifierBindings
      a value with no known type is not followed                  $untypedValue

    A qualified reference resolves through its qualifier
      a re-export is the other package's type, not itself         $qualifiedReexport
      a qualified miss is the reference as written                $qualifiedMiss
      a type is not a namespace                                   $typeNotNamespace
      the standard library may be spelled in full                 $standardSpellings
      a namespace's own `Option` is not the builtin               $namespacedBuiltinName
      a source type named like a base type is the source's        $baseTypeShadowed

    Binders shadow, and reasons follow the binder they name
      a parameter shadows a sibling type and a base type          $bindersShadow
      an instantiation reads its own definition's binders only    $instantiationBinders
      an object's type shadows an enclosing parameter             $innerTypeShadowsBinder
      only a definition's own recursion borrows its fixed point   $selfInstantiation
      a nested definition's binder stays a reason whatever its spelling  $nestedBinders
      a sealed child's own parameters are not the parent's        $childBinders

    Sealed families and lazy cycles say why they have no number
      a family the body does not sum is named                     $unsummedFamily
      a lazy cycle the solver could not complete is a reason      $lazyCycle

    A constructor slot is read where the constructor is written
      a qualified foreign name borrows nothing                    $slotQualified
      an imported name borrows no same-named source type          $slotImported
      an object's own type wins over another package's            $slotNested
      a slot never lends a size resting on an unknown name        $slotReasons
      a slot reads a type defined in several packages in its own  $slotOwnPackage
      a local class's slot reads the block around it              $slotLocal
      a sibling parameter's path reads its declared type          $slotParameterPath
      a refused slot names what the estimate stopped at           $slotRefusal
    """

  import Definition.Kind.*

  private val widget = "unresolved[Widget (imported from zz.Widget, not supplied)]"
  private val schema = "Schema (imported from zz.Schema, not supplied)"

  // `object Foo`'s value has type `Foo.type`; the type `Foo` is the open trait, which any class
  // anywhere may extend: no bound. Before, the module's `1` stood in for the trait and its users.
  def openTraitCompanion = {
    val found = rows("trait Foo\nobject Foo\nfinal case class UsesFoo(f: Foo)")
    (found(("Foo", Abstract)) === "—")
      .and(found(("Foo", Object)) === "1")
      .and(found(("UsesFoo", Class)) === "open[Foo]")
  }

  // `Light`'s cases live in its companion, where Scala 3 code keeps them: the parent sums them,
  // 1 + 1 = 2 — never the module's 1, and never the dash an open abstraction reads.
  def sealedCompanion = {
    val found = rows(
      "sealed trait Light\nobject Light { case object Red extends Light; case object Green extends Light }\nfinal case class UsesLight(l: Light)"
    )
    (found(("Light", Abstract)) === "2").and(found(("UsesLight", Class)) === "2")
  }

  // Beside the parent each `case object` is one value: |Light| = 1 + 1 = 2. The companion
  // `object Light` extends nothing and is not a case.
  def caseObjectChild = {
    val found = rows(
      "sealed trait Light\ncase object Red extends Light\ncase object Green extends Light\nobject Light\nfinal case class UsesLight(l: Light)"
    )
    (found(("Light", Abstract)) === "2").and(found(("UsesLight", Class)) === "2")
  }

  // A source holds its class's 2 values and its module's 1: 3. Before, the module read the class's
  // value from the name they share and the source summed 2 + 2.
  def companionTotal =
    Counter.source(
      dialects.Scala3("case class Spot(on: Boolean)\nobject Spot").parse[Source].get
    ) ===
      TinySize(3)

  // |Span| = 2^32 · 2^32 = 2^64, non-empty, so |List[Span]| = ω and |(Boolean, List[Span])| = 2 · ω
  // = ω, read through the object in the file and through the package path from another package.
  def nestedMember = {
    val found = rowsOf(
      "m/A.scala" -> "package m\nobject Scanner { final case class Span(start: Int, end: Int) }\ntype X1 = (Boolean, List[Scanner.Span])",
      "n/B.scala" -> "package n\ntype X2 = (Boolean, List[m.Scanner.Span])",
    )
    (found(("m.X1", Alias)) === "ω").and(found(("n.X2", Alias)) === "ω")
  }

  def nestedObjects =
    rows("object O { object P { final case class S(b: Boolean) } }\ntype X = O.P.S")(
      ("X", Alias)
    ) === "2"

  // A = 1 + |Q.B| and Q.B = 1 + |A|: the least fixed point of A = 2 + A is ω, and both rows read
  // it. Before, `Q.B` was unread from outside and the object solved against that placeholder.
  def crossObjectRecursion = {
    val found = rows(
      "final case class A(b: Option[Q.B])\nobject Q { final case class B(a: Option[A]) }"
    )
    (found(("A", Class)) === "ω").and(found(("Q.B", Class)) === "ω")
  }

  // Inside `Sh`, `Bx` is `Sh.Bx` (1 value), not the enclosing body's `Bx` (2). Inside `Imp`,
  // `Widget` is the import's — an explicit import outranks a package member another file defines
  // (scalac agrees; in the same file the two would be ambiguous) — and no source supplies `zz`:
  // never the package's `Widget` (2 values).
  def objectShadowing = {
    val shadowed = rows(
      "final case class Bx(x: Boolean)\nobject Sh { final case class Bx(x: Unit); type T = Bx }\ntype ShY = Sh.T"
    )
    val imported = rowsOf(
      "W.scala" -> "package t\nfinal case class Widget(x: Boolean)",
      "I.scala" -> "package t\nobject Imp { import zz.Widget; final case class Use(w: Widget) }\ntype ImpY = Imp.Use",
    )
    // A class body's own trait `Foo` shadows the file's class `Foo` (2) and the package's: open.
    val innerTrait =
      "final case class Foo(b: Boolean)\nfinal class C { trait Foo; final case class U(f: Foo) }"
    (shadowed(("ShY", Alias)) === "1")
      .and(imported(("t.Imp.Use", Class)) === widget)
      .and(imported(("t.ImpY", Alias)) === widget)
      .and(rows(innerTrait)(("C#U", Class)) === "open[Foo]")
      .and(rowsOf("S.scala" -> innerTrait)(("C#U", Class)) === "open[Foo]")
  }

  def objectOpenTrait =
    rows("object Op { trait Open }\nfinal case class UsesOp(o: Op.Open)")(("UsesOp", Class)) ===
      "open[Op.Open]"

  // `Gen.Pair[Boolean]` is Pair's equation with A = 2: 2 · 2 = 4.
  def objectGeneric =
    rows("object Gen { final case class Pair[A](a: A, b: A) }\ntype P = Gen.Pair[Boolean]")(
      ("P", Alias)
    ) === "4"

  // `Outer` holds an `Inner`, which holds a `Schema` nobody supplied: a reference to `Outer` is no
  // more bounded than `Inner` is. `Pair[Boolean]` binds `A`, so only `Schema` remains.
  def objectReasons = {
    val found = rows(
      """|object Taint {
         |  final case class Inner(s: Schema)
         |  final case class Outer(i: Inner)
         |  final case class Pair[A](a: A, s: Schema)
         |}
         |type X = Taint.Outer
         |type P = Taint.Pair[Boolean]
         |""".stripMargin
    )
    (found(("X", Alias)) === "unresolved[Schema]").and(found(("P", Alias)) === "unresolved[Schema]")
  }

  // `package a; package b` opens `a` and `a.b`, so `W` is `a.W` (2). A single `package a.b` opens
  // only `a.b`, where nothing defines `W`, and no package is searched by name alone — not `a`, not
  // `c` — so the name stays a reason (scalac rejects `Single` too). A source with no package clause
  // reads in the empty package, whose `V` (2) wins over `c.V` (1).
  def packageClauses = {
    val found = rowsOf(
      "W1.scala" -> "package a\nfinal case class W(x: Boolean)",
      "W2.scala" -> "package c\nfinal case class W(x: Unit)\nfinal case class V(x: Unit)",
      "U1.scala" -> "package a\npackage b\nfinal case class Chained(w: W)",
      "U2.scala" -> "package a.b\nfinal case class Single(w: W)",
      "V.scala" -> "final case class V(x: Boolean)",
      "Y.scala" -> "final case class UsesV(v: V)",
    )
    (found(("a.b.Chained", Class)) === "2")
      .and(found(("a.b.Single", Class)) === "unresolved[W]")
      .and(found(("UsesV", Class)) === "2")
  }

  // `data` is a subpackage of the opened `dev` under chained clauses; from the root, `dev.data`
  // and `_root_.dev.data` name the same package. Under a single `package dev.app2` clause, `data`
  // names nothing: the reference is its own reason.
  def packagePaths = {
    val found = rowsOf(
      "D.scala" -> "package dev.data\nfinal case class Vec(x: Boolean)\nobject Scanner { final case class Span(start: Int, end: Int) }",
      "E.scala" -> "package dev\npackage app\ntype V1 = data.Vec\ntype V2 = _root_.dev.data.Vec\ntype V3 = data.Scanner.Span",
      "F.scala" -> "package dev.app2\ntype V4 = data.Vec\ntype V5 = dev.data.Vec",
    )
    (found(("dev.app.V1", Alias)) === "2")
      .and(found(("dev.app.V2", Alias)) === "2")
      .and(found(("dev.app.V3", Alias)) === "≤2^64")
      .and(found(("dev.app2.V4", Alias)) === "unresolved[data.Vec]")
      .and(found(("dev.app2.V5", Alias)) === "2")
  }

  // A known limit: an enum's class cases are types (`Shape.Circle`), but the estimate does not
  // read an enum as a namespace; the reference stays a reason rather than borrowing a name.
  def enumCases =
    rows(
      "final case class Circle(r: Int)\nenum Shape { case Circle(r: Boolean) }\ntype C = Shape.Circle"
    )(
      ("C", Alias)
    ) === "unresolved[Shape.Circle]"

  // `p.X` is the class's member: (Array[Byte], (Array[Byte], Int, Int)) = ω · (ω · 2^32 · 2^32) =
  // ω. `Other.W` is the object's member, Unit: 1 — not the class's `W = Boolean`. Before, the
  // companion overwrote the class's members (then `X = p.X` captured itself: 0), and `Other.W`
  // read the class's 2.
  def membersByOwner = {
    val found = rows(
      """|final class Prism[A](val steps: List[A]) { type X = (Array[Byte], (Array[Byte], Int, Int)) }
         |object Prism
         |final class Holder(val p: Prism[Boolean]) { type X = p.X }
         |final class Other(val q: Boolean) { type W = Boolean; type V = Other.W }
         |object Other { type W = Unit }
         |""".stripMargin
    )
    (found(("Holder#X", Alias)) === "ω").and(found(("Other#V", Alias)) === "1")
  }

  // `b.Bytes` is `a.Bytes` = Array[Byte] = ω. Read by its last segment it named itself, and the
  // least fixed point of X = X is 0.
  def qualifiedReexport =
    rowsOf(
      "a/P.scala" -> "package a\ntype Bytes = Array[Byte]",
      "b/P.scala" -> "package b\ntype Bytes = a.Bytes",
    )(("b.Bytes", Alias)) === "ω"

  // `O` defines no `Missing`; the top-level `Missing` (2) is a different type.
  def qualifiedMiss =
    rows("type Missing = Boolean\nobject O\ntype Y = O.Missing")(("Y", Alias)) ===
      "unresolved[O.Missing]"

  // `Foo.B` with no `object Foo` is not Scala ("Expected a term, but found a type: Foo"), so it is
  // never read as the trait's member.
  def typeNotNamespace =
    rows("trait Foo[A] { type B = A }\nfinal case class D(y: Foo.B)")(("D", Class)) ===
      "unresolved[Foo.B]"

  // `scala.Int` = 2^32, `scala.Option[Boolean]` = 1 + 2 = 3, `java.lang.String` = ω, an immutable
  // `List[Boolean]` = ω; `java.util.List` is a mutable Java list the algebra does not model.
  def standardSpellings = {
    val found = rows(
      """|type I = scala.Int
         |type O = scala.Option[Boolean]
         |type S = java.lang.String
         |type L = scala.collection.immutable.List[Boolean]
         |type J = java.util.List[Boolean]
         |""".stripMargin
    )
    (found(("I", Alias)) === "≤2^32")
      .and(found(("O", Alias)) === "3")
      .and(found(("S", Alias)) === "ω")
      .and(found(("L", Alias)) === "ω")
      .and(found(("J", Alias)) === "unresolved[java.util.List]")
  }

  // `My.Option[Boolean]` is the object's class, 2 · 2 = 4, not the builtin's 1 + 2 = 3.
  def namespacedBuiltinName =
    rows("object My { final case class Option[A](a: A, b: A) }\ntype T = My.Option[Boolean]")(
      ("T", Alias)
    ) === "4"

  // `Shadow[Item]`'s `Item` is its parameter, not the sibling class (2^32), so the row depends on
  // the instantiation and `Shadow[Boolean]` is 2; the same for a parameter named `Int` and one
  // named `List` (a higher-kinded parameter, never the builtin).
  def bindersShadow = {
    val found = rows(
      """|final case class Item(n: Int)
         |final case class Shadow[Item](x: Item)
         |type S = Shadow[Boolean]
         |final case class Boxed[Int](x: Int)
         |type B = Boxed[Boolean]
         |final case class Wrapped[List[_]](xs: List[Boolean])
         |""".stripMargin
    )
    (found(("Shadow", Class)) === "instantiation[Item]")
      .and(found(("S", Alias)) === "2")
      .and(found(("Boxed", Class)) === "instantiation[Int]")
      .and(found(("B", Alias)) === "2")
      .and(found(("Wrapped", Class)) === "instantiation[List[_]]")
  }

  // In `Q`, `A` is the top-level class (1 value): Q[Boolean] = 1 · 2 = 2, and P[Int] = |Q[Boolean]|
  // = 2 — `P`'s own parameter `A` (bound to 2^32 in `P[Int]`) is not `Q`'s to read.
  def instantiationBinders = {
    val found = rows(
      """|final case class A(u: Unit)
         |final case class P[A](x: Q[Boolean])
         |final case class Q[B](y: A, z: B)
         |type T = P[Int]
         |""".stripMargin
    )
    (found(("P", Class)) === "2").and(found(("T", Alias)) === "2")
  }

  // Inside `Inner`, `Item` is the object's class (2 values), which shadows `Outer`'s parameter.
  def innerTypeShadowsBinder =
    rows(
      "class Outer[Item] { object Inner { final case class Item(n: Boolean); final case class Use(x: Item) } }"
    )(("Outer#Inner.Use", Class)) === "2"

  // `Schema.Type` under `import org.apache.avro.Schema` is not the unrelated `Type()` (1 value),
  // and the reason names the import its first name rests on — the method side's, which names the
  // path the import binds; the estimate's refusal of the same name is not repeated.
  def slotQualified =
    constructor(
      "dev.k.DirectKind.<init>",
      "K.scala" -> "package dev.k\nimport org.apache.avro.Schema\nfinal case class DirectKind(schemaType: Schema.Type)",
      "U.scala" -> "package somewhere.unrelated\nfinal case class Type()",
    ) === Count.Unresolved(
      List(
        "declaration not supplied: org.apache.avro.Schema.Type (selected through named import Schema)"
      )
    )

  // `Widget` is `zz.Widget`, whose declaration nobody supplied, not the source's Widget (2 values).
  def slotImported =
    constructor(
      "dev.k.UsesImported.<init>",
      "K.scala" -> "package dev.k\nimport zz.Widget\nfinal case class UsesImported(w: Widget)",
      "U.scala" -> "package somewhere.unrelated\nfinal case class Widget(x: Boolean)",
    ) === Count.Unresolved(List("declaration not supplied: zz.Widget (named import Widget)"))

  // `Kind` is `Holder.Kind` = 1 + 1 = 2, not another package's `Kind(a: Int)` (2^32).
  def slotNested =
    constructor(
      "dev.k.Holder.UsesKind.<init>",
      "K.scala" -> "package dev.k\nobject Holder {\n  sealed trait Kind\n  case object A extends Kind\n  case object B extends Kind\n  final case class UsesKind(k: Kind)\n}",
      "U.scala" -> "package somewhere.unrelated\nfinal case class Kind(a: Int)",
    ) === Count.Finite(2)

  // `In` holds a `Schema` nobody supplied: its solved value is a placeholder, so `Out`'s slot has no
  // size to lend (before: ω), and the row names the name the estimate stopped at.
  def slotReasons =
    constructor(
      "Out.<init>",
      "R.scala" -> "final case class In(s: Schema)\nfinal case class Out(i: In)",
    ) === Count.Unresolved(List(s"declaration not supplied: Schema (${StdlibModel.undeclared})"))

  // `PathStep` is defined in two packages; read in `a`, its own, it is the enum: Field(Boolean) +
  // Index(Unit) = 2 + 1 = 3, so Option[PathStep] = 1 + 3 = 4 — never `b`'s 2^32.
  def slotOwnPackage =
    constructor(
      "a.Uses.<init>",
      "a/P.scala" -> "package a\nenum PathStep { case Field(name: Boolean); case Index(i: Unit) }\nfinal case class Uses(steps: Option[PathStep])",
      "b/P.scala" -> "package b\nfinal case class PathStep(n: Int)",
    ) === Count.Finite(4)

  // `Out`'s `In` is the class the method's block defines (2 values), never package `m`'s `In`
  // (2^32), and `Box`'s `A` is the method's type parameter, never package `l`'s class `A`: the
  // estimate refuses that slot with the parameter as its reason, so it lends no 2^32.
  def slotLocal = {
    val l =
      "package l\nfinal case class A(n: Int)\nobject F {\n  def f[A](a: A): Int = { final case class Box(x: A); Box(a).hashCode }\n  def g(b: Boolean): Int = { final case class In(b: Boolean); final case class Out(i: In); Out(In(b)).hashCode }\n}"
    val files = List("l/L.scala" -> l, "m/M.scala" -> "package m\nfinal case class In(n: Int)")
    val tree = dialects.Scala3(l).parse[Source].get
    val box = tree.collect { case c: Defn.Class if c.name.value == "Box" => c }.head
    (constructor("l.F.g.Out.<init>", files*) === Count.Finite(2))
      .and(
        Reading.of(tree).read(box.ctor.paramClauses.head.values.head.decltpe.get) ===
          Left(List(Definition.Unbound.Parameter("A")))
      )
  }

  // In `Dep(q: Other)(val x: q.X)`, `q` is the first clause's parameter, of type `Other`, whose `X`
  // is `Int` (2^32) — not the package's object `q` (`X = Boolean`). The method side reads the
  // parameter's path too (members M1), so the constructor's two readings agree: 2^32.
  def slotParameterPath = {
    val code =
      "package u\nobject q { type X = Boolean }\nfinal class Other { type X = Int }\nfinal case class Dep(q: Other)(val x: q.X)"
    (constructor("u.Dep.<init>", "S.scala" -> code).render === "2^32")
      .and(rowsOf("S.scala" -> code)(("u.Dep", Class)) === "≤2^32")
  }

  // `Failure`'s `Failed` case holds a `Widget` nobody supplied: the estimate refuses the slot, and
  // the row says so, once, beside whatever the shape model stopped at (`Int` is a ground type). A
  // throwable slot is an open class instead (`ThrowableSlotsSpec.constructorSlot`).
  def slotRefusal =
    constructor(
      "f.FailureException.<init>",
      "F.scala" -> "package f\nenum Failure { case Missing(at: Int); case Failed(cause: Widget) }\nfinal class FailureException(val failure: Failure)",
    ) === Count.Unresolved(
      List("declaration not supplied: Widget (not declared in the sources or stdlib-model/1)")
    )

  // `Red` is both a case object extending `Light` and an alias (`p1`) or a class (`p1b`, the
  // object's companion); the sum takes the module: |Light| = |Red.type| + |Green.type| = 2.
  def caseObjectNamedLikeType = {
    val alias =
      "sealed trait Light\ncase object Red extends Light\ntype Red = Boolean\ncase object Green extends Light\nfinal case class UsesLight(l: Light)"
    val companion =
      "sealed trait Light\nfinal class Red(val b: Boolean)\ncase object Red extends Light\ncase object Green extends Light\nfinal case class UsesLight(l: Light)"
    (rows(alias)(("Light", Abstract)) === "2")
      .and(rows(alias)(("UsesLight", Class)) === "2")
      .and(rows(companion)(("Light", Abstract)) === "2")
      .and(constructor("UsesLight.<init>", "L.scala" -> companion) === Count.Finite(2))
  }

  // Under chained `package a; package b`, `x` is the subpackage `a.b.x` (inner scope, `T = Unit`),
  // which shadows package `a`'s object `x` (`T = Boolean`): Y = 1, UsesT = 1 (scalac accepts
  // `val c: Y = ()`).
  def packageOrder = {
    val files = List(
      "A.scala" -> "package a\nobject x { type T = Boolean }",
      "B.scala" -> "package a.b.x\ntype T = Unit",
      "C.scala" -> "package a\npackage b\ntype Y = x.T\nfinal case class UsesT(t: x.T)",
    )
    val found = rowsOf(files*)
    (found(("a.b.Y", Alias)) === "1")
      .and(found(("a.b.UsesT", Class)) === "1")
      .and(constructor("a.b.UsesT.<init>", files*) === Count.Finite(1))
  }

  // F1 imports `zz.Widget` (unsupplied); an explicit import outranks package `p`'s Widget from F2,
  // so `Use` holds the import's Widget: every read of it — from package `q`, from another file of
  // `p`, and the method side — is `?`, never F2's 2.
  def libraryImports = {
    val files = List(
      "F1.scala" -> "package p\nimport zz.Widget\nobject O { final case class Use(w: Widget) }",
      "F2.scala" -> "package p\nfinal case class Widget(x: Boolean)",
      "F3.scala" -> "package q\ntype Y = p.O.Use",
      "F4.scala" -> "package p\ntype Z = O.Use\nfinal case class HoldsUse(u: O.Use)",
    )
    val found = rowsOf(files*)
    (found(("q.Y", Alias)) === widget)
      .and(found(("p.Z", Alias)) === widget)
      .and(found(("p.HoldsUse", Class)) === widget)
      .and(
        constructor("p.HoldsUse.<init>", files*) ===
          Count.Unresolved(List("declaration not supplied: zz.Widget (named import Widget)"))
      )
  }

  // `a.b.Chained`'s `W` is package `a`'s (chained clauses open `a`), 2 values, read from package `c`
  // too: Y = Z = UsesZ = 2 — never `unresolved: W`.
  def libraryClauses = {
    val found = rowsOf(
      "W.scala" -> "package a\nfinal case class W(x: Boolean)",
      "C.scala" -> "package a\npackage b\nfinal case class Chained(w: W)\nobject Holder { final case class Inner(w: W) }",
      "Y.scala" -> "package c\ntype Y = a.b.Chained\ntype Z = a.b.Holder.Inner\nfinal case class UsesZ(z: a.b.Holder.Inner)",
    )
    (found(("c.Y", Alias)) === "2")
      .and(found(("c.Z", Alias)) === "2")
      .and(found(("c.UsesZ", Class)) === "2")
  }

  // `import foreign.*` names no supplied package: `Json` and `Obj` are not `x`'s (they may be
  // `foreign`'s), so both stay reasons. `import x.*` names `x`, which outranks package `p`'s own
  // `Obj` from another file (scalac binds `x.Obj`): 2 each. Two wildcard packages that both define
  // `Obj` leave it ambiguous.
  def wildcardImports = {
    val x =
      "x/A.scala" -> "package x\nfinal case class Obj(b: Boolean)\nobject Json { final case class Obj(b: Boolean) }"
    val foreign = rowsOf(
      x,
      "y/B.scala" -> "package y\nimport foreign.*\ntype J = Json.Obj\nfinal case class UseBare(j: Obj)"
    )
    val named = rowsOf(
      x,
      "p/P1.scala" -> "package p\nfinal case class Obj(u: Unit)",
      "p/P2.scala" -> "package p\nimport x.*\nfinal case class UseBare(j: Obj)\ntype J = Json.Obj",
    )
    val ambiguous = rowsOf(
      x,
      "z/Z.scala" -> "package z\nfinal case class Obj(n: Int)",
      "w/W.scala" -> "package w\nimport x.*\nimport z.*\nfinal case class Amb(j: Obj)",
    )
    (foreign(("y.J", Alias)) === "unresolved[Json.Obj (may be imported by foreign.*)]")
      .and(foreign(("y.UseBare", Class)) === "unresolved[Obj (may be imported by foreign.*)]")
      .and(named(("p.UseBare", Class)) === "2")
      .and(named(("p.J", Alias)) === "2")
      .and(
        ambiguous(
          ("w.Amb", Class)
        ) === "unresolved[Obj (ambiguous: several wildcard imports bind it)]"
      )
  }

  // `Widget` is `zz.Widget`'s, explicitly imported: package `x`'s open trait of that name says
  // nothing about it.
  def importedOpenReason =
    rowsOf(
      "x/A.scala" -> "package x\ntrait Widget",
      "y/B.scala" -> "package y\nimport zz.Widget\nfinal case class UsesWidget(w: Widget)",
    )(("y.UsesWidget", Class)) === widget

  // `p.X` reads `Prism`'s `X = Span` in Prism's body, where `Span` is `Prism.Span(b: Boolean)` —
  // 2 — never the top-level `Span(u: Unit)` the reader sees (1). Likewise `Prism3`'s `X = W` is
  // its own `W = Boolean` (2), not `Holder3`'s `W = Unit`. scalac accepts `val check: Y =
  // p.Span(true)` and `val check: Y = true`. The method side now reads `UsesY`'s slot through the
  // path `p` as Prism's own `Span` (members M1: Prism's alias read in Prism's body, 2), so the
  // readings agree; its environment (`p: Prism`, a class with a term member) stays unresolved.
  def classMemberScope = {
    val code =
      """|final case class Span(u: Unit)
         |final class Prism(val n: Int) { final case class Span(b: Boolean); type X = Span }
         |final class Holder(val p: Prism) { type Y = p.X; final case class UsesY(y: p.X) }
         |final class Prism3(val n: Int) { type W = Boolean; type X = W }
         |final class Holder3(val p: Prism3) { type W = Unit; type Y = p.X }
         |""".stripMargin
    val found = rows(code)
    (found(("Holder#Y", Alias)) === "2")
      .and(found(("Holder3#Y", Alias)) === "2")
      // The final Prism is the record of its public field (capabilities C4), so the constructor
      // reads its one parameter, p.X = p.Span: 2.
      .and(constructor("Holder.UsesY.<init>", "M.scala" -> code) === Count.Finite(2))
  }

  // The first name of `p.X` is the innermost term: `Inner`'s `val p = new Prism2` (its `X = Unit`,
  // 1) over the class parameter `p: Prism` (`X = Boolean`); `Inner2`'s object `p` (`X = Unit`) over
  // the same parameter; `H`'s `val q = new Prism2` over package `p`'s object `q` (`X = Boolean`);
  // `Holder`'s `val p = new Other` (`X = Int`, 2^32) over the file's object `p`. scalac accepts
  // `val check: X = ()` in each Unit case.
  def qualifierBindings = {
    val nested = rows(
      """|final class Prism(val n: Int) { type X = Boolean }
         |final class Prism2(val n: Int) { type X = Unit }
         |final class Outer(val p: Prism) { object Inner { val p = new Prism2(1); type X = p.X } }
         |final class Outer2(val p: Prism) { object Inner2 { object p { type X = Unit }; type X = p.X } }
         |""".stripMargin
    )
    val files = rowsOf(
      "F1.scala" -> "package p\nobject q { type X = Boolean }",
      "F2.scala" -> "package p\nfinal class Prism2(val n: Int) { type X = Unit }\nfinal class H(val n: Int) { val q = new Prism2(1); type Y = q.X }",
      "C.scala" -> "package t\nobject p { type X = Boolean }\nfinal class Other { type X = Int }\nfinal class Holder { val p = new Other; type Y = p.X }",
    )
    (nested(("Outer#Inner.X", Alias)) === "1")
      .and(nested(("Outer2#Inner2.X", Alias)) === "1")
      .and(files(("p.H#Y", Alias)) === "1")
      .and(files(("t.Holder#Y", Alias)) === "≤2^32")
  }

  // `val p = make()` has no type the estimate reads, and it shadows the object `p`: the reference
  // is its own reason, never the object's Boolean.
  def untypedValue =
    rows(
      "object p { type X = Boolean }\nfinal class Other { type X = Int }\nfinal class Holder { def make(): Other = new Other; val p = make(); type Y = p.X }"
    )(("Holder#Y", Alias)) === "unresolved[p.X]"

  // `g.Int` is the package's own class (2 values), which scalac binds over `scala.Int` even from
  // another file of the package.
  def baseTypeShadowed = {
    val files = List(
      "G.scala" -> "package g\nfinal case class Int(b: Boolean)",
      "U.scala" -> "package g\nfinal case class UseInt(i: Int)",
    )
    (rowsOf(files*)(("g.UseInt", Class)) === "2")
      .and(constructor("g.UseInt.<init>", files*) === Count.Finite(2))
  }

  // `Q[A]` and `Q2[B]` are alpha-equivalent: each holds `O.P` instantiated at its own parameter,
  // and `P` holds an unsupplied `Schema`. Neither is P's own recursion, so neither borrows P's
  // solved placeholder: both depend on their parameter and name Schema. `Chain[A]`'s reference to
  // itself inside its own equation is its recursion: Chain = 1 + Chain, least fixed point ω.
  def selfInstantiation = {
    val found = rows(
      """|import zz.Schema
         |object O { final case class P[A](a: A, s: Schema) }
         |final case class Q[A](p: O.P[A])
         |final case class Q2[B](p: O.P[B])
         |final case class R[A](a: A, s: Schema)
         |final case class T[A](r: R[A])
         |final case class Chain[A](next: Option[Chain[A]])
         |""".stripMargin
    )
    (found(("Q", Class)) === s"unresolved[A, $schema]")
      .and(found(("Q2", Class)) === s"unresolved[B, $schema]")
      .and(found(("T", Class)) === s"unresolved[A, $schema]")
      .and(found(("Chain", Class)) === "ω")
  }

  // `K`'s and `K2`'s `X = Q.Outer[Unit]` read `Outer` at `Unit`, whose `In` holds the class's own
  // parameter: `B` in `K`, `A` in `K2` — the same spelling as `Outer`'s own `A`, which the
  // instantiation binds. Renaming a binder does not change the answer.
  def nestedBinders = {
    val found = rows(
      """|class K[B] { object Q { final case class In(b: B); final case class Outer[A](i: In, x: A) }; type X = Q.Outer[Unit] }
         |class K2[A] { object Q { final case class In(a: A); final case class Outer[A](i: In, x: A) }; type X = Q.Outer[Unit] }
         |""".stripMargin
    )
    (found(("K#X", Alias)) === "instantiation[B]")
      .and(found(("K2#X", Alias)) === "instantiation[A]")
  }

  // `P` is not generic: `C[A] extends P` holds an `x: A` for every `A`, so no instantiation of `P`
  // fixes it (the values range over everything): the family is not summed, and the reason says
  // why — never `depends on A`, and never the ω a read of `O.P2` used to borrow.
  def childBinders = {
    val found = rows(
      """|sealed trait P
         |final case class C[A](x: A) extends P
         |object O { sealed trait P2; final case class C2[A](x: A) extends P2 }
         |type Q = O.P2
         |final case class UsesQ(q: O.P2)
         |""".stripMargin
    )
    def unpinned(parent: String, child: String) =
      s"unresolved[sealed family not summed: $parent (sealed subtype binds a parameter to no " +
        s"slot of its parent: $child (A, of ${parent.split('.').last}))]"
    (found(("P", Abstract)) === unpinned("P", "C"))
      .and(found(("O.P2", Abstract)) === unpinned("P2", "C2"))
      .and(found(("Q", Alias)) === unpinned("O.P2", "C2"))
      .and(found(("UsesQ", Class)) === unpinned("O.P2", "C2"))
  }

  // A sealed intermediate is summed in turn (A = |B| = |C| = 2), a parent extended with arguments
  // through its slots (`Opt[X]` = |X|: a function of the instantiation); an abstract subtype that
  // is not sealed is open to extension, so its family is not summed: named, never the dash an open
  // abstraction reads.
  def unsummedFamily = {
    val found = rows(
      "sealed trait A\nsealed trait B extends A\nfinal case class C(b: Boolean) extends B\nsealed trait Opt[X]\nfinal case class Som[X](x: X) extends Opt[X]\nsealed trait O2\ntrait Mid extends O2\ncase object K extends O2"
    )
    (found(("A", Abstract)) === "2")
      .and(found(("Opt", Abstract)) === "instantiation[X]")
      .and(
        found(("O2", Abstract)) ===
          "unresolved[sealed family not summed: O2 (an abstract subtype open to extension: Mid)]"
      )
  }

  // `S(head, tail: => O.Cell)` and `O.Cell(s: S)` form a lazy cycle whose strict `s: S` blocks the ν
  // completion: its μ value 0 counts no lazy knot, yet `lazy val c: O.Cell = O.Cell(new S(true, c))`
  // builds one (scalac accepts it). Every row on or above the cycle is `?`, in one body or across
  // an object. A completed lazy cycle keeps its number: St = νX. 2·X = ω, read through `O`.
  def lazyCycle = {
    val found = rows(
      """|final class S(val head: Boolean, tail0: => O.Cell)
         |object O { final case class Cell(s: S); final case class St(h: Boolean, t: => St) }
         |final case class UsesCell(c: O.Cell)
         |final class S2(val head: Boolean, tail0: => Cell2)
         |final case class Cell2(s: S2)
         |final case class UsesSt(s: O.St)
         |""".stripMargin
    )
    def cycle(names: String*) = names.map(n => s"lazy cycle not completed: $n").mkString(", ")
    (found(("O.Cell", Class)) === s"unresolved[${cycle("Cell", "S")}]")
      .and(found(("S", Class)) === s"unresolved[${cycle("S", "O.Cell")}]")
      .and(found(("UsesCell", Class)) === s"unresolved[${cycle("O.Cell", "S")}]")
      .and(found(("S2", Class)) === s"unresolved[${cycle("S2", "Cell2")}]")
      .and(found(("Cell2", Class)) === s"unresolved[${cycle("Cell2", "S2")}]")
      .and(found(("UsesSt", Class)) === "ω")
  }

}
