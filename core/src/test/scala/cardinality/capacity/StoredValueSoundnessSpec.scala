package cardinality.capacity

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.Specification

// Stored-value soundness (docs/research/stored-value-families.md): sealed families are tagged sums
// over their file's subtypes or a named refusal (explicit imports: `ImportClausesSpec`); a
// reference carries the reasons of what it reads; context bounds are stored evidence; a
// finite collection keeps its element's tier; an enclosing template's abstract member and a match
// type stuck on the definition's own parameters wait for the instantiation; a floating-point
// stand-in stays marked; and a row's reasons render by class. Each number is derived beside it
// (fixtures checked with scalac 3.9.0).
class StoredValueSoundnessSpec extends Specification {

  import Rows.*
  import Definition.Kind.*

  def is = s2"""
    Sealed families are tagged sums
      children in the companion, recursion included              $companionFamily
      a parameterised family instantiates through its slots       $parameterisedFamily
      sealed intermediates are summed once                        $intermediates

    A family that is not closed is refused, and says why
      a subtype through an alias, or reached twice                 $aliasesAndOverlap
      a subtype through a renamed import                           $renamedParent
      a parent name an object rebinds names another type          $rebinding
      a bottom pin is the standard library's Nothing              $shadowedBottom
      an enum case pinned to one instantiation is refused         $enumCases

    Recursion and match types the model does not count stay unknown
      recursion through a function's domain has no count          $negativeRecursion
      a constructor's slots are its declaration's, never its name $constructorSlots

    Reasons, evidence, containers, members, match types and marks
      a reference carries the reasons of what it reads            $taint
      context bounds are constructor inputs                       $contextBounds
      a finite collection keeps its element's tier                $containers
      an enclosing template's abstract member is a parameter      $abstractMember
      a floating-point stand-in survives sums and products        $lossy
      every class of reasons renders, unresolved first             $reasonClasses
    """

  // |Tree| = μX. 1 + X·X = ω (Leaf and Node sit in the companion, Scala 3's usual shape).
  // `Light` sums two case objects: 2.
  def companionFamily = {
    val found = rows(
      """|sealed trait Tree
         |object Tree {
         |  case object Leaf extends Tree
         |  final case class Node(l: Tree, r: Tree) extends Tree
         |}
         |final case class Forest(t: Tree)
         |sealed trait Light
         |object Light { case object Red extends Light; case object Green extends Light }
         |final case class UsesLight(l: Light)
         |""".stripMargin
    )
    (found(("Tree", Abstract)) === "ω")
      .and(found(("Tree.Node", Class)) === "ω")
      .and(found(("Forest", Class)) === "ω")
      .and(found(("UsesLight", Class)) === "2")
  }

  // |Opt[Boolean]| = |No| + |Yes[Boolean]| = 1 + 2 (`Nothing` pins the covariant slot bottom-up).
  // `Fl[X, Y] extends Flip[Y, X]`: Flip[Boolean, Unit] binds Y = Boolean, X = Unit: 1·2 = 2.
  // `Sum[Boolean]` through a sealed abstract class's companion: 1 + 2 = 3. The parents' own rows
  // wait for their parameters.
  def parameterisedFamily = {
    val found = rows(
      """|sealed trait Opt[+A]
         |case object No extends Opt[Nothing]
         |final case class Yes[+A](a: A) extends Opt[A]
         |final case class U(o: Opt[Boolean])
         |sealed trait Flip[A, B]
         |final case class Fl[X, Y](x: X, y: Y) extends Flip[Y, X]
         |final case class UF(f: Flip[Boolean, Unit])
         |sealed abstract class Sum[+B]
         |object Sum {
         |  case object Empty extends Sum[Nothing]
         |  final case class One[+B](b: B) extends Sum[B]
         |}
         |final case class UsesSum(s: Sum[Boolean])
         |""".stripMargin
    )
    (found(("U", Class)) === "3")
      .and(found(("UF", Class)) === "2")
      .and(found(("UsesSum", Class)) === "3")
      .and(found(("Opt", Abstract)) === "instantiation[A]")
      .and(found(("Flip", Abstract)) === "instantiation[A, B]")
  }

  // |E| = |Z| + |O| with the sealed intermediate O = μX. |S| + 1 = μX. X + 1 = ω: ω + 1.
  // |Pair[Boolean, Unit]| = |Both| + |Sub| = 2·1 + |Left0[Boolean, Unit]| = 2 + 2 = 4.
  def intermediates = {
    val found = rows(
      """|sealed trait E
         |sealed trait O extends E
         |final case class Z() extends E
         |final case class S(p: O) extends O
         |case object One extends O
         |final case class UE(e: E)
         |sealed trait Pair[A, B]
         |object Pair {
         |  final case class Both[A, B](a: A, b: B) extends Pair[A, B]
         |  sealed trait Sub[A, B] extends Pair[A, B]
         |  final case class Left0[A, B](a: A) extends Sub[A, B]
         |}
         |final case class UP(p: Pair[Boolean, Unit])
         |""".stripMargin
    )
    (found(("UE", Class)) === "ω + 1").and(found(("UP", Class)) === "4")
  }

  private def refused(code: String): String = rows(code)(("Use", Class))

  // The parameters an instantiation-dependent row waits for, in any order.
  private def notSummed(family: String, why: String) =
    s"unresolved[sealed family not summed: $family ($why)]"

        ) === notSummed("W", "a template the estimate does not read: Defn.Class")
          "a subtype open to extension: C (parent not read: Exception)"
  // `X extends WA` with `type WA = W` is a subtype the parent's name does not spell (the sum would
  // say 1, the truth is 1 + 2); so is `new WA {}`. `Both` reaches `D` through `D1` and `D2`: a
  // summand counted twice. The method side refuses the same constructors.
  def aliasesAndOverlap = {
    val alias =
      "sealed trait W\ntype WA = W\nfinal case class X(b: Boolean) extends WA\ncase object Y extends W\nfinal case class Use(w: W)"
    val anonymous =
      "sealed trait W\ntype WA = W\ncase object Y extends W\nobject M { val w: W = new WA {} }\nfinal case class Use(w: W)"
    (refused(alias) === notSummed("W", "a subtype extends it through an alias: X"))
      .and(refused(anonymous) === notSummed("W", "an anonymous instance in its file"))
      .and(
        refused(
          "sealed trait D\nsealed trait D1 extends D\nsealed trait D2 extends D\nfinal case class Both(b: Boolean) extends D1 with D2\nfinal case class Use(d: D)"
        ) === notSummed("D", "its subtype Both extends it more than once")
      )
      .and(constructor("Use.<init>", "A.scala" -> alias) must beLike {
        case Count.Unresolved(reasons) => reasons.exists(_.contains("through an alias")) must beTrue
      })
  }

  // `import x.{P => Q}` (or `P as Q`, in another package clause) lets `Z extends Q` extend `P`
  // under a name no alias spells (scalac: `List[P](A, O.Z)` compiles): |P| = 1 + 1 = 2, never the
  // 1 that omits Z — refused, and the method side's constructor with it.
  def renamedParent = {
    val renamed = "x/A.scala" ->
      "package x\nsealed trait P\ncase object A extends P\nobject O {\n  import x.{P => Q}\n  case object Z extends Q\n}\nfinal case class Use(p: P)"
    val clauses = "x2/A.scala" ->
      "package x2 {\n  sealed trait P\n  case object A extends P\n  final case class Use(p: P)\n}\npackage y2 {\n  import x2.P as Q\n  case object Z extends Q\n}"
    val reason = notSummed("P", "a subtype extends it through a renamed import: Z")
    (rowsOf(renamed)(("x.Use", Class)) === reason)
      .and(rowsOf(clauses)(("x2.Use", Class)) === reason)
      .and(constructor("x.Use.<init>", renamed) must beLike { case Count.Unresolved(_) => ok })
      .and(constructor("x2.Use.<init>", clauses) must beLike { case Count.Unresolved(_) => ok })
  }

  // An object's parent binds the parent's name inside it (scalac: `val y: P = O.Z`
  // fails, `val x: Base.P = O.Z` compiles): the top-level `P` has the one subtype `A`, so |Use| =
  // 1 (the sum said 2). `Base#P`'s subtype sits in `O`, outside its body: refused. A parent the
  // file does not declare may bind it: refused. A same-file parent without a `P` changes nothing.
  def rebinding = {
    val inherited =
      "sealed trait P\ncase object A extends P\ntrait Base { sealed trait P }\nobject O extends Base { case object Z extends P }\nfinal case class Use(p: P)"
    val found = rows(inherited)
    (found(("Use", Class)) === "1")
      .and(
        found(("Base#P", Abstract)) ===
          notSummed("P", "a template the estimate does not read: Defn.Object")
      )
      .and(constructor("Use.<init>", "A.scala" -> inherited) === Count.Finite(1))
      .and(
        refused(
          "sealed trait P\ncase object A extends P\nobject O extends zz.Base { case object Z extends P }\nfinal case class Use(p: P)"
        ) === notSummed("P", "a subtype whose clause may name another P: Z")
      )
      .and(
        refused(
          "sealed trait P\ntrait Low\nobject P extends Low { case object A extends P; case object B extends P }\nfinal case class Use(p: P)"
        ) === "2"
      )
  }

  // With `type Nothing = Boolean`, `Z extends P[Nothing]` is a `P[Boolean]` only (scalac: `val no:
  // P[Unit] = Z` fails): |Use| = |C[Unit]| = 1, not 2. The pin is read where `Z` is written.
  def shadowedBottom =
    refused(
      "type Nothing = Boolean\nsealed trait P[+A]\ncase object Z extends P[Nothing]\nfinal case class C[+A](a: A) extends P[A]\nfinal case class Use(p: P[Unit])"
    ) === notSummed("P", "a subtype pins it with Nothing, which a source binds: Z")

  // `case Y extends E[Int]` is an `E[Int]` only (scalac: `val y: P[Boolean] = E.Y` fails): |E
  // [Boolean]| = |X[Boolean]| = 2, which a sum over every case says is 3. A bottom pin is a member of every instantiation: |H[Boolean]| = 1 + 2.
  def enumCases = {
    val gadt =
      "enum E[A] {\n  case X(a: A)\n  case Y extends E[Int]\n}\nfinal case class Use(e: E[Boolean])"
    val reason =
      "unresolved[enum case not summed: E.Y (sealed subtype leaves a parameter of its parent " +
        "unpinned: Y (Int, of E))]"
    (refused(gadt) === reason)
      .and(
        refused(
          "enum H[+A] { case N extends H[Nothing]; case V(a: A) }\nfinal case class Use(h: H[Boolean])"
        ) ===
          "3"
      )
  }

  // `MC(f: M => Boolean)` from 0 reads 0 (the empty domain), yet `MC(_ => true)` is a closed value,
  // and X ≅ 2^X has no solution: no count, directly or through another
  // definition's domain (`Pred[K]`). A recursion through a function's result is counted.
  def negativeRecursion = {
    val code =
      """|sealed trait M
         |final case class MC(f: M => Boolean) extends M
         |final case class UseM(m: M)
         |sealed trait N[A]
         |final case class NC[A](f: N[A] => Boolean) extends N[A]
         |final case class UseN(n: N[Boolean])
         |final case class Pred[A](f: A => Boolean)
         |sealed trait K
         |final case class KC(p: Pred[K]) extends K
         |case object K0 extends K
         |final case class UseK(k: K)
         |final case class Res(k: Int => Res, b: Boolean)
         |""".stripMargin
    val found = rows(code)
    def negative(names: String*) =
      names
        .map(n => s"recursion through a function's domain: $n")
        .mkString("unresolved[", ", ", "]")
    (found(("UseM", Class)) === negative("M", "MC"))
      .and(found(("UseK", Class)) === negative("K", "KC"))
      .and(found(("Res", Class)) === "ω")
      .and(constructor("UseN.<init>", "A.scala" -> code) must beLike {
        case Count.Unresolved(_) => ok
      })
  }

  // A constructor's slots are what its declaration makes them, wherever its name is written: a
  // source `List[A](f: A => Boolean)` (or alias `Option[A] = A => Boolean`) in another file of the
  // package makes `P(l: List[P])` the negative P ≅ 2^P (scalac: `P(List(_ => true))` and `P(List(_
  // => false))` differ), not the 0 its name's standard reading gave; a supplied positive `Box` in
  // another file keeps T = μX. 1 + X = ω; a constructor no source supplies is not read at all.
  def constructorSlots = {
    val list = rowsOf(
      "n/A.scala" -> "package n\nfinal case class List[A](f: A => Boolean)",
      "n/B.scala" -> "package n\nfinal case class P(l: List[P])\nfinal case class UseP(p: P, b: Boolean)",
    )
    val option = rowsOf(
      "q/O.scala" -> "package q\ntype Option[A] = A => Boolean",
      "q/P.scala" -> "package q\nfinal case class P(o: Option[P])",
    )
    val box = rowsOf(
      "b/Box.scala" -> "package b\nfinal case class Box[A](a: A)",
      "b/T.scala" -> "package b\nsealed trait T\nfinal case class N(kids: Box[T]) extends T\ncase object L extends T\nfinal case class UseT(t: T)",
    )
    val foreign = rows(
      "import zz.Chunk\nsealed trait T\nfinal case class N(kids: Chunk[T]) extends T\ncase object L extends T"
    )
    (list(("n.UseP", Class)) === "unresolved[recursion through a function's domain: P]")
      .and(option(("q.P", Class)) === "unresolved[recursion through a function's domain: P]")
      .and(box(("b.UseT", Class)) === "ω")
      .and(
        foreign(("T", Abstract)) must contain(
          "recursion through a constructor the estimate does not read: T (through Chunk)"
        )
      )
  }

  }

  // Every reader of a definition resting on an unknown name rests on it too — through a generic
  // instantiation, a field, an `Either`, two hops, and a product with an empty factor (0·? is not
  // claimed as 0); a bare `Throwable` is unresolved. A reference that
  // supplies a definition's parameters owes none of them: |Gen[Boolean]| = 2; `Gen[B]` waits for
  // B; an open member keeps the reader open.
  def taint = {
    val files = List(
      "a/A.scala" -> "package a\nfinal case class Foo[A](s: Schema, a: A)\nenum Failure { case Boom(cause: Throwable); case Code(n: Int) }\nfinal case class Gen[A](a: A)\ntrait Open\nfinal case class HasOpen(o: Open)",
      "b/B.scala" -> "package b\nimport a.*\nfinal case class Bar(f: Foo[Boolean])\nfinal case class Q(s: Schema)\nfinal case class Local(q: Q)\ntype Res = Either[Failure, Boolean]\ntype Res2 = Either[Q, Boolean]\nfinal case class G1(g: Gen[Boolean])\nfinal case class G2[B](g: Gen[B])\nfinal case class UsesOpen(h: HasOpen)\nfinal case class Zero(n: Nothing, q: Q)\nfinal case class Nested(l: Local)",
    )
    val found = rowsOf(files*)
    (found(("b.Bar", Class)) === "unresolved[Schema]")
      .and(found(("b.Local", Class)) === "unresolved[Schema]")
      .and(found(("b.Nested", Class)) === "unresolved[Schema]")
      .and(found(("b.Res", Alias)) === "unresolved[Throwable]")
      .and(found(("b.Res2", Alias)) === "unresolved[Schema]")
      .and(found(("b.Zero", Class)) === "unresolved[Schema]")
      .and(found(("b.G1", Class)) === "2")
      .and(found(("b.G2", Class)) === "instantiation[B]")
      .and(found(("b.UsesOpen", Class)) === "open[Open]")
      .and(constructor("b.Local.<init>", files*) must beLike { case Count.Unresolved(_) => ok })
  }

  // `C[T: TC]` stores a `TC[T]`. |Sorted[Boolean]| = |Boolean|·|Ord[Boolean]| = 2·|Boolean =>
  // Boolean| = 2·4 = 8; |Two[Unit]| = 1·|Ord[Unit]|² = 2·2 = 4 (one field per bound). An unknown
  // type class is a reason: `TT` holds a `Traverse[T]` (before: 1).
  def contextBounds = {
    val code =
      """|final class TT[T[_]: Traverse, A, B]
         |final case class TB[F[_]: Functor, B](b: B)
         |sealed trait Ord[A]
         |object Ord { final case class ByBool[A](f: A => Boolean) extends Ord[A] }
         |final case class Sorted[A: Ord](a: A)
         |final case class UsesSorted(s: Sorted[Boolean])
         |final case class Two[A: Ord: Ord](a: A)
         |final case class UsesTwo(t: Two[Unit])
         |""".stripMargin
    val found = rows(code)
    (found(("TT", Class)) === "unresolved[Traverse]")
      .and(found(("TB", Class)) === "unresolved[B, Functor]")
      .and(found(("UsesSorted", Class)) === "8")
      .and(found(("UsesTwo", Class)) === "4")
      .and(constructor("UsesTwo.<init>", "A.scala" -> code) === Count.Finite(4))
  }

  // |C[A]| ≥ |A| (the one-element collections): over an ε₀-tier element (`String => String`,
  // `Any`) a List, Array, Set, Vector, a Map's keys or values, or a repeated parameter is ε₀; over
  // a finite or countable one ω (`Set[Boolean]` = 2² = 4); over `Nothing` 1 (`Map[Boolean,
  // Nothing]` = 1¹… the empty map). The method side keeps its own saturating reading (ω).
  def containers = {
    val code =
      """|type L = List[String => String]
         |type A = Array[Any]
         |type S = Set[String => String]
         |type LB = List[Boolean]
         |type LN = List[Nothing]
         |type SB = Set[Boolean]
         |type SS = Set[String]
         |type MK = Map[String => String, Boolean]
         |type MV = Map[Boolean, String => String]
         |type MV0 = Map[Boolean, Nothing]
         |type VV = Vector[Vector[Any]]
         |final case class Rep(xs: (String => String)*)
         |""".stripMargin
    val found = rows(code)
    def sizes(names: String*) = names.map(n => found((n, Alias)))
    (sizes("L", "A", "S", "MK", "MV", "VV") === List.fill(6)("ε₀"))
      .and(sizes("LB", "LN", "SB", "SS", "MV0") === List("ω", "1", "4", "ω", "1"))
      .and(found(("Rep", Class)) === "ε₀")
      .and(constructor("Rep.<init>", "A.scala" -> code) === Count.Bounded(EffectiveOmega))
  }

  // Inside `Opt[T]`, `M` is `this.M`, which each instance chooses: `X = (T, M)` is a function of
  // that choice and of T.
  def abstractMember =
    rows("sealed abstract class Opt[T] { type M; type X = (T, M) }")(("Opt#X", Alias)) ===
      "instantiation[T, member M]"

  // |Double| stands in as at most 2^64 (lossy): 2^64 + 2^64 rounds to 2^65, 2^64·2^32 to 2^96,
  // both bounds, both still marked.
  def lossy = {
    val found = rows(
      "enum Shape { case C(r: Double); case S(s: Double) }\nfinal case class P(a: Double, b: Int)"
    )
    (found(("Shape", Enum)) === "≤2^65 (lossy)").and(found(("P", Class)) === "≤2^96 (lossy)")
  }

  // An unknown name is not hidden behind an open abstraction, nor an open one behind a parameter:
  // every class renders, unresolved first.
  def reasonClasses = {
    val found = Counter
      .definitions(
        dialects
          .Scala3(
            "trait Open\nfinal case class Mixed(o: Open, s: Schema)\nfinal case class Gen[A](o: Open, a: A)"
          )
          .parse[Source]
          .get
      )
      .map(d => d.name -> d)
      .toMap
    (Definition.reasons(found("Mixed")) === "unresolved: Schema; open to implementations (Open)")
      .and(
        Definition.reasons(found("Gen")) ===
          "open to implementations (Open); depends on its instantiation (A)"
      )
      .and(Definition.gap(found("Mixed")) === Some(Definition.Gap.Unresolved))
      .and(Definition.gap(found("Gen")) === Some(Definition.Gap.Open))
  }

}
