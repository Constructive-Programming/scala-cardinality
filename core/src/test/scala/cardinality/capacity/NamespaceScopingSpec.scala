package cardinality.capacity

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

// Where a name is read decides what it means (docs/research/stored-value-names.md): an import binds
// the statements after it, an export the whole template; a wildcard over a supplied namespace
// outranks another file's package member, one over a namespace no source supplies may bind
// anything; a qualifier's first name is every term binding Scala has; a match type's case body is
// read where the match type is written; and a constructor slot the estimate refuses has no number.
// Every expected number is derived in the comment above it (fixtures checked with scalac 3.9.0).
class NamespaceScopingSpec extends Specification {

  import Rows.*
  import Definition.Kind.*

  def is = s2"""
    Clauses bind where Scala binds them
      an import binds the statements after it only                $positionalImports
      a file's wildcard outranks another file's package member, in the library too  $libraryWildcards
      a wildcard names an object or a package as an identifier would  $wildcardSources
      a wildcard over an unsupplied namespace may bind any name   $unsuppliedWildcard
      an export binds its template                                 $exports
      an explicitly imported or bound constructor name is not the standard one  $modelledShadowing

    Term bindings and type bindings are Scala's
      a class member's owner binds as any type name does          $ownerShadowing
      an abstract type member shadows an outer type               $abstractMember
      a generic definition named without arguments has no value space  $typeConstructor

    Families and cycles say why they have no number
      a lazy field through an alias closes a lazy cycle           $lazyAlias
      packages are solved in the order their reads need           $packageOrder

    A refused constructor slot has no number
      a ground slot the estimate refuses is the row's reason       $groundSlot
      a slot over a type parameter refused for a declaration is `?`  $declarationSlot
    """

  private val x = "x/X.scala" -> "package x\nfinal case class Obj(b: Boolean)"

  // `UseBare` precedes `import x.*`, so its `Obj` is package `p`'s from P1 (1 value); `After`
  // follows it, and the wildcard (precedence 3) outranks P1's package member (4): `x.Obj`, 2.
  // Read from package `r` through the library, the same: R1 = 1, R2 = 2.
  def positionalImports = {
    val files = List(
      x,
      "p/P1.scala" -> "package p\nfinal case class Obj(u: Unit)",
      "p/P2.scala" -> "package p\nfinal case class UseBare(o: Obj)\nimport x.*\nfinal case class After(o: Obj)",
      "r/R.scala" -> "package r\ntype R1 = p.UseBare\ntype R2 = p.After",
    )
    val found = rowsOf(files*)
    (found(("p.UseBare", Class)) === "1")
      .and(found(("p.After", Class)) === "2")
      .and(found(("r.R1", Alias)) === "1")
      .and(found(("r.R2", Alias)) === "2")
      .and(constructor("p.UseBare.<init>", files*) === Count.Finite(1))
      .and(constructor("p.After.<init>", files*) === Count.Finite(2))
  }

  // P2 imports `q.*`: `x.T` and `Obj` are `q`'s (Unit, 1 value each), not P1's (Boolean, 2) — a
  // wildcard outranks another file's package member. Read from package `r` through the library,
  // the same 1 (before, the library solved the package's files in one map and read P1's: 2).
  def libraryWildcards = {
    val files = List(
      "q/Q.scala" -> "package q\nobject x { type T = Unit }\nfinal case class Obj(u: Unit)",
      "p/P1.scala" -> "package p\nobject x { type T = Boolean }\nfinal case class Obj(b: Boolean)",
      "p/P2.scala" -> "package p\nimport q.*\ntype Y = x.T\nfinal case class UseBare(o: Obj)",
      "r/R.scala" -> "package r\ntype RY = p.Y\ntype RU = p.UseBare\nfinal case class HoldsU(u: p.UseBare)",
    )
    val found = rowsOf(files*)
    (found(("p.Y", Alias)) === "1")
      .and(found(("p.UseBare", Class)) === "1")
      .and(found(("r.RY", Alias)) === "1")
      .and(found(("r.RU", Alias)) === "1")
      .and(found(("r.HoldsU", Class)) === "1")
      .and(constructor("r.HoldsU.<init>", files*) === Count.Finite(1))
  }

  // `import Scanner.*` names the object of another file: its `Span(b: Boolean)` (2) outranks the
  // package's `Span(u: Unit)` (1). `import x.*` inside package `p` names `p.x`, the package's own
  // object (an identifier finds it before the root package `x`): `Obj(u: Unit)`, 1.
  def wildcardSources = {
    val scanner = List(
      "pw/A.scala" -> "package pw\nfinal case class Span(u: Unit)",
      "pw/B.scala" -> "package pw\nobject Scanner { final case class Span(b: Boolean) }",
      "pw/C.scala" -> "package pw\nimport Scanner.*\nfinal case class Use(s: Span)\ntype Y = Span",
    )
    val shadowed = List(
      x,
      "p/P1.scala" -> "package p\nobject x { final case class Obj(u: Unit) }",
      "p/P2.scala" -> "package p\nimport x.*\nfinal case class Use(o: Obj)",
    )
    (rowsOf(scanner*)(("pw.Use", Class)) === "2")
      .and(rowsOf(scanner*)(("pw.Y", Alias)) === "2")
      .and(constructor("pw.Use.<init>", scanner*) === Count.Finite(2))
      .and(rowsOf(shadowed*)(("p.Use", Class)) === "1")
      .and(constructor("p.Use.<init>", shadowed*) === Count.Finite(1))
  }

  // `scala.collection.mutable.*` is not supplied, and a wildcard outranks another file's package
  // member and the root `scala.Int` alike: `StringBuilder` may be mutable's (scalac binds it so),
  // and so may `Int`. Both are refused, naming the clause. A name the file defines itself still
  // outranks the wildcard (scalac reports an outer definition and an inner import as ambiguous):
  // `Own()` holds one value.
  def unsuppliedWildcard = {
    val files = List(
      "px/A.scala" -> "package px\nfinal case class StringBuilder(b: Boolean)",
      "px/B.scala" -> "package px\nimport scala.collection.mutable.*\nfinal case class UseSB(s: StringBuilder)\nfinal case class N(i: Int)\nfinal case class Own()\nfinal case class UseOwn(o: Own)",
    )
    val found = rowsOf(files*)
    val clause = "may be imported by scala.collection.mutable.*"
    (found(("px.UseSB", Class)) === s"unresolved[StringBuilder ($clause)]")
      .and(found(("px.N", Class)) === s"unresolved[Int ($clause)]")
      .and(found(("px.UseOwn", Class)) === "1")
      .and(
        constructor("px.UseSB.<init>", files*) ===
          Count.Unresolved(List(s"unresolved type: StringBuilder ($clause)"))
      )
  }

  // `export x.Obj` makes `Obj` a member of `O`: inside `O` it is the export's, followed to
  // `x.Obj(b: Boolean)` (2), never the package's `Obj(u: Unit)` (1); an export of a name no source
  // supplies stays a reason. `export Src.*` makes `Src.Obj(b: Boolean)` a member of `O2`, which
  // outranks the package's `Obj`: 2. The method side reads the package's `Obj` (1) for `O2.U`'s
  // slot, so the constructor's readings disagree and its row is refused.
  def exports = {
    val files = List(
      x,
      "p/P.scala" -> "package p\nfinal case class Obj(u: Unit)\nobject O { export x.Obj; final case class U(o: Obj); type Y = Obj }\nobject Src { final case class Obj(b: Boolean) }\nobject O2 { export Src.*; final case class U(o: Obj) }\nobject O3 { export zz.Obj; type Y = Obj }",
    )
    val found = rowsOf(files*)
    (found(("p.O.U", Class)) === "2")
      .and(found(("p.O.Y", Alias)) === "2")
      .and(found(("p.O3.Y", Alias)) === "unresolved[Obj (exported from zz.Obj, not supplied)]")
      .and(found(("p.O2.U", Class)) === "2")
      // The method side reads `O2`'s exported `Src.Obj` too, so the two readings agree.
      .and(constructor("p.O2.U.<init>", files*) === Count.Finite(2))
  }

  // A bare `Option` or `List` the sources bind is that binding's: an explicit import of a namespace
  // no source supplies, a source open trait (any class may extend it — `Impl` alone holds
  // functions), a source sealed `Option` summed through its slots — `Some[Boolean](a, b)` (4) plus
  // `None` (1): 5. Unbound, `Option[Boolean]` is the standard one: 1 + 2 = 3.
  def modelledShadowing = {
    val imported = rows(
      "import zz.Option\nimport java.util.List\nfinal case class UsesO(o: Option[Boolean])\nfinal case class J(xs: List[Nothing])"
    )
    val source = rows(
      "trait List[A]\nfinal case class UsesL(xs: List[Boolean])\nsealed trait Option[+A]\nfinal case class Some[+A](a: A, b: A) extends Option[A]\ncase object None extends Option[Nothing]\nfinal case class UsesO(o: Option[Boolean])"
    )
    (imported(("UsesO", Class)) === "unresolved[Option (imported from zz.Option, not supplied)]")
      .and(
        imported(("J", Class)) === "unresolved[List (imported from java.util.List, not supplied)]"
      )
      .and(source(("UsesL", Class)) === "open[List]")
      .and(source(("UsesO", Class)) === "5")
      .and(rows("final case class Plain(o: Option[Boolean])")(("Plain", Class)) === "3")
  }

  // `p: Prism` reads `p.X` through whatever `Prism` binds where `p` is: `O`'s alias `Prism =
  // Other`, which has no members of its own here, or `Holder`'s type parameter `Prism <: Other` —
  // never the package's class `Prism` (`X = Unit`, 1). Both are `?`.
  def ownerShadowing = {
    val found = rows(
      "final class Prism { type X = Unit }\nclass Other { type X = Boolean }\nobject O { type Prism = Other; final class Holder(val p: Prism) { type Y = p.X } }\nfinal class Holder2[Prism <: Other](val p: Prism) { type Y = p.X }"
    )
    (found(("O.Holder#Y", Alias)) === "unresolved[p.X]")
      .and(found(("Holder2#Y", Alias)) === "unresolved[p.X]")
  }

  // `U(w: W)` inside `Holder` holds `Holder`'s abstract `W` — `this.W`, which each instance
  // chooses (`Impl` puts Ints in it): a function of that choice, as of a parameter, never the outer
  // `W(b: Boolean)` (2). An object's abstract member has one instance and stays open.
  def abstractMember = {
    val found = rows(
      "final case class W(b: Boolean)\ntrait Holder { type W; final case class U(w: W) }\nobject Impl extends Holder { type W = Int }\nobject O { type V; final case class UV(v: V) }"
    )
    (found(("Holder#U", Class)) === "instantiation[member W]")
      .and(found(("O.UV", Class)) === "open[V]")
  }

  // `O.Pair2` and `O.One` are type constructors: |Pair2[Nothing]| = 0, |Pair2[Boolean]|
  // = 4, so no one number is theirs (before: the template's placeholder ω, and 1).
  def typeConstructor = {
    val found = rows(
      "object O { final case class Pair2[A](a: A, b: A); final case class One[A](u: Unit) }\ntype Qual = O.Pair2\ntype Qual1 = O.One\nfinal case class Pair3[A](a: A, b: A)\ntype Bare = Pair3"
    )
    (found(("Qual", Alias)) === "unresolved[type constructor: O.Pair2]")
      .and(found(("Qual1", Alias)) === "unresolved[type constructor: O.One]")
  }

  // `tail: Thunk` with `type Thunk = () => Cell` is a lazy field: `Cell(s: S)` and `S` form a lazy
  // cycle whose strict `s: S` blocks the ν completion, and `lazy val c: Cell = Cell(new S(true, ()
  // => c))` builds a value the μ reading (0) omits. A completed twin through an alias keeps its
  // number: Node = νX. 2·X = ω.
  def lazyAlias = {
    val found = rows(
      "type Thunk = () => Cell\nfinal class S(val head: Boolean, val tail: Thunk)\nfinal case class Cell(s: S)\nfinal case class UsesCell(c: Cell)\ntype Next = () => Node\nfinal case class Node(b: Boolean, next: Next)"
    )
    def cycle(names: String*) = names.map(n => s"lazy cycle not completed: $n").mkString(", ")
    (found(("Cell", Class)) === s"unresolved[${cycle("Cell", "S", "Thunk")}]")
      .and(found(("S", Class)) === s"unresolved[${cycle("S", "Thunk", "Cell")}]")
      .and(found(("UsesCell", Class)) === s"unresolved[${cycle("Cell", "S", "Thunk")}]")
      .and(found(("Node", Class)) === "ω")
  }

  // A chain of seven packages, each class holding the next, ends in `C7(b: Boolean)`: 2 all the
  // way up, however long the chain (before: four rounds, so `p1.C1` named `p6.C6`). Two packages
  // whose classes hold each other are a cycle the library does not solve, and say so.
  def packageOrder = {
    val chain = (1 to 6).toList.map(i =>
      s"C$i.scala" -> s"package p$i\nfinal case class C$i(x: p${i + 1}.C${i + 1})"
    ) :+ ("C7.scala" -> "package p7\nfinal case class C7(b: Boolean)")
    val cycle = List(
      "A.scala" -> "package p\nfinal case class A(b: Option[q.B])",
      "B.scala" -> "package q\nfinal case class B(a: Option[p.A])",
    )
    (rowsOf(chain*)(("p1.C1", Class)) === "2")
      .and(constructor("p1.C1.<init>", chain*) === Count.Finite(2))
      .and(rowsOf(cycle*)(("p.A", Class)) === "unresolved[cross-package cycle not solved: p.A]")
  }

  // `U(w: W)` with `W` imported from a supplied package: the estimate follows the import to
  // `a.W(b: Boolean)`, 2, as `V(w: a.W)` reads it; with `W` imported from a namespace no source
  // supplies, the shape model would count `W` as an argument (|U|^|W|), not the values `U` holds,
  // so `?` naming the import. `U(l: L)` with `L`'s cases `R`, `G` in its companion: 2; with a
  // class open to extension beside them, not summed, `?`.
  def groundSlot = {
    val imported = List(
      "a/W.scala" -> "package a\nfinal case class W(b: Boolean)",
      "b/U.scala" -> "package b\nimport a.W\nfinal case class U(w: W)\nfinal case class V(w: a.W)",
      "c/U.scala" -> "package c\nimport zz.W\nfinal case class U(w: W)",
    )
    val family = "F.scala" ->
      "package f\nsealed trait L\nobject L { case object R extends L; case object G extends L }\nfinal case class U(l: L)"
    val open = "G.scala" ->
      "package g\nsealed trait L\nobject L { case object R extends L; class O extends L }\nfinal case class U(l: L)"
    (constructor("b.U.<init>", imported*) === Count.Finite(2))
      .and(constructor("b.V.<init>", imported*) === Count.Finite(2))
      .and(
        constructor("c.U.<init>", imported*) ===
          Count.Unresolved(List("declaration not supplied: zz.W (named import W)"))
      )
      .and(constructor("f.U.<init>", family) === Count.Finite(2))
      .and(
        constructor("g.U.<init>", open) must beLike {
          case Count.Unresolved(reasons) =>
            reasons must contain("sealed family not summed: L (a subtype open to extension: L.O)")
        }
      )
  }

  // `focus: Fam[A]` is over `A`, the shape model's to count — but the estimate refused it for a
  // family it does not sum (a class open to extension beside `Leaf`), a declaration of the sources:
  // no number may rest on it.
  def declarationSlot =
    constructor(
      "f.P.<init>",
      "F.scala" -> "package f\nsealed trait Fam[A]\nobject Fam { final case class Leaf[A](a: A) extends Fam[A]; class Open[A] extends Fam[A] }\nfinal class P[A](steps: List[Boolean], focus: Fam[A])",
    ) must beLike {
      case Count.Unresolved(reasons) =>
        reasons must contain(
          "sealed family not summed: Fam (a subtype open to extension: Fam.Open)"
        )
    }

}
