package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification

/** Higher-kinded and bounded type parameters (docs/research/higher-kinded-binders.md). Every valid
  * fixture compiles with Scala 3.9.0 (`*` placeholders under `-Xkind-projector`); the refusals
  * marked invalid are inputs the compiler rejects, which the analysis must still not count.
  */
class HigherKindedBindingSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Rigid type constructors (R1)
      an application is its own value                               $same
      two values of one application are two choices                 $two
      distinct arguments are distinct types                         $diff
      argument order matters                                        $flip
      a product of applications multiplies                          $pair
      an optional application has no seed in its empty branch       $optional
      both sum branches forward their own payload                   $either
      a seeded endomorphism on an application is countable          $loop
      an unseeded endomorphism supplies nothing                     $noSeed
      an unused higher-kinded binder blocks nothing                 $unused
      binder identity is per declaration under shadowing            $shadowing
      typeclass declarations over a rigid carrier                   $typeclasses
      a nested-kind binder and its evidence are opaque              $nestedKind
      an application of an application is one more key             $nested
    Nominal argument identity (R2)
      Option and Either[Unit, _] stay distinct under F              $optionEither
      a case class and a tuple stay distinct under F                $caseTuple
      Boolean and Either[Unit, Unit] stay distinct under F          $boolEither
      equal spellings are one type                                  $sameKey
      tuple sugar is type equality                                  $tupleSugar
      function sugar is type equality                               $functionSugar
      a transparent alias is expanded                               $aliasKey
      a local alias shadows a builtin name in a key                 $shadowedKey
      an opaque argument is transparent only in its scope           $opaqueKey
      an opaque argument outside its scope is no readable key       $opaqueOutside
      an idempotent union is its one alternative                    $unionKey
      equality evidence substitutes through F                       $leibniz
      subtype evidence does not lift through an invariant F         $noLift
      an applied endpoint of equality evidence                      $appliedEq
      a long equality chain settles                                 $longChain
      an equality that keeps unfolding is refused                   $unfolding
      operators are keyed by their eta-long lambda                  $operatorKeys
      an export of a vocabulary name is not trusted                 $exportedVocabulary
      inherited substitution keeps nominal identity                 $inherited
      a constructor field of F[Unit]                                $unfold
    Constructor arguments (R3)
      a rigid binder passed to a case class                         $boxRigid
      a builtin constructor passed to a case class                  $boxOption
      an explicit type lambda is applied                            $boxLambda
      a placeholder lambda is applied                               $boxHole
      a lambda binder shadows a method binder                       $hygiene
      a lambda alias reads its names where it is declared           $closureScope
      a curried type-lambda alias is applied                        $curried
      an unapplied constructor passed but never applied             $phantom
      a higher-kinded transparent alias takes a builtin constructor $pairAlias
      a foreign callee reports only itself                          $foreignCallee
      a top-level alias's companion sees the file's opaque types    $aliasCompanion
    Context bounds (R4)
      a readable evidence joins the environment                     $evidence
      a named context bound is a named evidence                     $namedEvidence
      a higher-kinded evidence joins the environment                $hkEvidence
      a class's bound is evidence for its members                   $classEvidence
      a class's bound is a position read where it is written        $classBoundPosition
      a foreign evidence names the foreign type                     $foreignBound
      same-typed anonymous evidences are not counted                $ambiguous
      anonymous using parameters do not overwrite each other        $anonymous
      a bounded binder's evidence is not provably unrelated         $boundedEvidence
    Subtype bounds (R5, B1)
      an upper bound upcasts                                        $upper
      an upper bound does not downcast                              $noDowncast
      a lower bound upcasts the bound                               $lower
      a bound does not lift through an invariant F                  $noBoundLift
      bounds that make binders equal identify them                  $boundEquality
      Any above and Nothing below bound nothing                     $extremes
    Structural bounds (R5, B2)
      an upper bound to a sealed family is eliminated as it         $upperSealed
      two bounded values are eliminated into a structural goal      $upperPair
      an upper bound to a case class is projected                   $upperProduct
      an upper bound to Option is split                             $upperOption
      an upper bound to a ground type reads its literals            $upperGround
      an upper bound to Nothing makes the context absurd            $upperNothing
      a lower bound builds the goal as the bound                    $lowerSealed
      a lower bound survives a split of another value               $lowerSplit
      unrelated lower and upper bounds are read separately          $upLow
  """

  private def entries(code: String): Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Kinds.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def count(code: String, name: String): Count = entries(code)(name)

  private def unresolved(code: String, name: String, fragment: String) =
    count(code, name) must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains(fragment)) must beTrue
    }

  // R1 -----------------------------------------------------------------------------------------
  def same =
    count("object T { def same[F[_], A](fa: F[A]): F[A] = fa }", "T.same") === Count.Finite(1)

  def two =
    count("object T { def two[F[_], A](x: F[A], y: F[A]): F[A] = x }", "T.two") === Count.Finite(2)

  def diff =
    count("object T { def diff[F[_], A, B](fa: F[A]): F[B] = ??? }", "T.diff") === Count.Finite(0)

  def flip =
    count("object T { def flip[F[_, _], A, B](x: F[A, B]): F[B, A] = ??? }", "T.flip") ===
      Count.Finite(0)

  def pair =
    count("object T { def pair[F[_], A](x: F[A], y: F[A]): (F[A], F[A]) = (x, y) }", "T.pair") ===
      Count.Finite(4)

  def optional =
    count("object T { def opt[F[_], A](x: Option[F[A]]): F[A] = ??? }", "T.opt") === Count.Finite(0)

  def either =
    count("object T { def e[F[_], A](x: Either[F[A], F[A]]): F[A] = ??? }", "T.e") ===
      Count.Finite(1)

  def loop =
    count("object T { def loop[F[_], A](f: F[A] => F[A], x: F[A]): F[A] = x }", "T.loop") ===
      Count.Countable

  def noSeed =
    count("object T { def ns[F[_], A](f: F[A] => F[A]): F[A] = ??? }", "T.ns") === Count.Finite(0)

  // Before: `bounded or higher-kinded parameter F[_]` although F is never mentioned.
  def unused =
    count("class Holder[F[_]] { def id[A](a: A): A = a }", "Holder.id") === Count.Finite(1)

  def shadowing = {
    val counts = entries("""
      final class Shadow[F[_]] {
        def inner[F[_], A](fa: F[A]): F[A] = fa
        def outer[G[_], A](fa: F[A]): G[A] = ???
      }
    """)
    (counts("Shadow.inner") === Count.Finite(1)).and(counts("Shadow.outer") === Count.Finite(0))
  }

  def typeclasses = {
    val counts = entries("""
      trait Accessor[F[_, _]] { def get[X, A](fa: F[X, A]): A }
      trait PartialAccessor[F[_, _]] { def getOption[X, A](fa: F[X, A]): Option[A] }
      trait ReverseAccessor[F[_, _]] { def reverseGet[X, A](a: A): F[X, A] }
      trait ForgetfulFunctor[F[_, _]] { def map[X, A, B](fa: F[X, A], f: A => B): F[X, B] }
    """)
    (counts("Accessor.get") === Count.Finite(0))
      .and(counts("PartialAccessor.getOption") === Count.Finite(1))
      .and(counts("ReverseAccessor.reverseGet") === Count.Finite(0))
      .and(counts("ForgetfulFunctor.map") === Count.Finite(0))
  }

  // `G[_]: C` adds the evidence `C[G]`, an application of the rigid nested-kind C to G's operator.
  def nestedKind =
    count(
      "trait FT[F[_, _], C[_[_]]] { def traverse[X, A, B, G[_]: C](fa: F[X, A], f: A => G[B]): G[F[X, B]] }",
      "FT.traverse"
    ) === Count.Finite(0)

  def nested = {
    val counts = entries("""
      object T {
        def nested[F[_], A](x: F[F[A]]): F[F[A]] = x
        def flat[F[_], A](x: F[F[A]]): F[A] = ???
      }
    """)
    (counts("T.nested") === Count.Finite(1)).and(counts("T.flat") === Count.Finite(0))
  }

  // R2 -----------------------------------------------------------------------------------------
  def optionEither =
    count("object T { def f[F[_], A](x: F[Option[A]]): F[Either[Unit, A]] = ??? }", "T.f") ===
      Count.Finite(0)

  def caseTuple =
    count(
      "case class Pair[A](a: A, b: A)\nobject T { def f[F[_], A](x: F[Pair[A]]): F[(A, A)] = ??? }",
      "T.f"
    ) === Count.Finite(0)

  def boolEither =
    count("object T { def f[F[_]](x: F[Boolean]): F[Either[Unit, Unit]] = ??? }", "T.f") ===
      Count.Finite(0)

  def sameKey =
    count("object T { def f[F[_], A](x: F[Option[A]]): F[Option[A]] = x }", "T.f") ===
      Count.Finite(1)

  def tupleSugar =
    count("object T { def f[F[_], A](x: F[(A, A)]): F[Tuple2[A, A]] = x }", "T.f") ===
      Count.Finite(1)

  def functionSugar =
    count("object T { def f[F[_], A](x: F[A => A]): F[Function1[A, A]] = x }", "T.f") ===
      Count.Finite(1)

  def aliasKey =
    count("type Id[X] = X\nobject T { def f[F[_], A](x: F[Id[A]]): F[A] = x }", "T.f") ===
      Count.Finite(1)

  // Inside S, `Option[A]` is the local alias `Either[Unit, A]`: one type.
  def shadowedKey =
    count(
      "object S { type Option[A] = Either[Unit, A]; def f[F[_], A](x: F[Option[A]]): F[Either[Unit, A]] = x }",
      "S.f"
    ) === Count.Finite(1)

  // Inside O, `W[A]` is `A`: x itself.
  def opaqueKey =
    count(
      "object O { opaque type W[A] = A; def inside[F[_], A](x: F[W[A]]): F[A] = x }",
      "O.inside"
    ) === Count.Finite(1)

  // Outside O the opaque type is not A: its key is its own declaration's, as a class's is, and as a
  // value it is one value (rule F). O's API is no member of the environment (a qualified member of
  // another object is not, opaque or not), so nothing yields an F[A]: 0. The monomorphic and given
  // forms of the same API read alike.
  def opaqueOutside = {
    val counts = entries("""
      object O { opaque type W[A] = A; def inside[F[_], A](x: F[W[A]]): F[A] = x }
      object T { def outside[F[_], A](x: F[O.W[A]]): F[A] = O.inside(x) }
      class Ctx[F[_]] {
        object M { opaque type V = Boolean; def inside(x: F[V]): F[Boolean] = x }
        def outside(x: F[M.V]): F[Boolean] = M.inside(x)
      }
      object G { opaque type U[A] = A; given flat[F[_], A]: (F[U[A]] =:= F[A]) = ??? }
      object H { def outside[F[_], A](x: F[G.U[A]]): F[A] = G.flat[F, A](x) }
    """)
    (counts("T.outside") === Count.Finite(0))
      .and(counts("Ctx.outside") === Count.Finite(0))
      .and(counts("H.outside") === Count.Finite(0))
  }

  def unionKey =
    count("object T { def f[F[_], A](x: F[A | A]): F[A] = x }", "T.f") === Count.Finite(1)

  def leibniz =
    count("object T { def f[F[_], A, B](fa: F[A])(using A =:= B): F[B] = ??? }", "T.f") ===
      Count.Finite(1)

  def noLift =
    count("object T { def f[F[_], A, B](fa: F[A])(using A <:< B): F[B] = ??? }", "T.f") ===
      Count.Finite(0)

  def appliedEq =
    count("object T { def f[F[_], S, A](s: S)(using S =:= F[A]): F[A] = ??? }", "T.f") ===
      Count.Finite(1)

  // Each step unfolds one class's representative: 18 links settle after 18 rewrites (a fixed budget
  // of 16 refused this). `x: A1` is `F^18[A19]`, the goal too: x.
  def longChain = {
    val binders = (1 to 19).map(i => s"A$i").mkString(", ")
    val proofs = (1 to 18).map(i => s"e$i: A$i =:= F[A${i + 1}]").mkString(", ")
    count(s"object T { def f[F[_], $binders](x: A1)(using $proofs): F[A2] = x }", "T.f") ===
      Count.Finite(1)
  }

  // `A = F[A]` has no finite solution for every F: rewriting it never ends, and a class met again
  // along the chain proves it. (Exact count 1: every program is the identity cast.)
  def unfolding = {
    val counts = entries("""
      object T {
        def loop[F[_], A](a: A)(using A =:= F[A]): F[A] = ???
        def loop2[F[_], A, B](a: A)(using ab: A =:= F[B], ba: B =:= F[A]): F[B] = ???
      }
    """)
    def refused(name: String) =
      counts(name) must beLike {
        case Count.Unresolved(reasons) =>
          reasons must contain(
            "unsupported evidence relationship: an equality proof keeps unfolding"
          )
      }
    refused("T.loop").and(refused("T.loop2"))
  }

  // `G ≡ [y] =>> G[y]`, `Option ≡ [y] =>> Option[y]`, `Either ≡ [a, b] =>> Either[a, b]`; a
  // different body or argument order is a different operator.
  def operatorKeys = {
    val counts = entries("""
      object T {
        def eta[F[_[_]], G[_]](x: F[G]): F[[y] =>> G[y]] = x
        def named[F[_[_]]](x: F[Option]): F[[y] =>> Option[y]] = x
        def other[F[_[_]]](x: F[Option]): F[[y] =>> Either[Unit, y]] = ???
        def same[F[_[_, _]]](x: F[Either]): F[[a, b] =>> Either[a, b]] = x
        def swapped[F[_[_, _]]](x: F[Either]): F[[a, b] =>> Either[b, a]] = ???
      }
    """)
    (counts("T.eta") === Count.Finite(1))
      .and(counts("T.named") === Count.Finite(1))
      .and(counts("T.other") === Count.Finite(0))
      .and(counts("T.same") === Count.Finite(1))
      .and(counts("T.swapped") === Count.Finite(0))
  }

  // A parent instantiated at a binder keeps its key; one instantiated at `Option[B]` was read as a
  // shape, which has lost the nominal identity `G[Option[B]]` needs (exact count 0).
  def inherited = {
    val counts = entries("""
      trait Base[F[_], A] { val seed: F[A] }
      trait Same[G[_], B] extends Base[G, B] { def get: G[B] }
      trait Other[G[_], B] extends Base[G, Option[B]] { def get: G[B] }
    """)
    (counts("Same.get") === Count.Finite(1)).and(
      counts("Other.get") must beLike {
        case Count.Unresolved(reasons) =>
          reasons must contain("nominal identity of substituted argument not tracked: A")
      }
    )
  }

  // `Function1`, `Tuple2` and `=:=` are read as builtins wherever no source declares them, so an
  // export that puts another type behind them — an alias of `foo.Function1`, which the sources do
  // not supply — leaves the reading unknown (before: counted).
  def exportedVocabulary =
    unresolved(
      "export foo.Function1\nobject T { def f[A](g: Function1[A, A], a: A): A = a }",
      "T.f",
      "declaration not supplied: foo.Function1 (named export Function1)"
    )

  def unfold =
    count(
      "final class Unfold[T, B, F[_]](val embed: F[B] => T, val vestigialTo: () => F[Unit])",
      "Unfold.<init>"
    ) === Count.Finite(1)

  // R3 -----------------------------------------------------------------------------------------
  private val box = "case class Box[F[_], A](value: F[A])\n"

  def boxRigid =
    count(box + "object T { def f[F[_], A](fa: F[A]): Box[F, A] = Box(fa) }", "T.f") ===
      Count.Finite(1)

  def boxOption =
    count(box + "object T { def f[A](a: A): Box[Option, A] = ??? }", "T.f") === Count.Finite(2)

  def boxLambda =
    count(box + "object T { def f[A](a: A): Box[[x] =>> Either[Unit, x], A] = ??? }", "T.f") ===
      Count.Finite(2)

  def boxHole =
    count(box + "object T { def f[A](a: A): Box[Either[A, *], A] = ??? }", "T.f") ===
      Count.Finite(2)

  // The lambda's X shadows the method's: the field is `A`, one value. Capture would read `X`: 0.
  def hygiene =
    count(box + "object T { def f[X, A](a: A): Box[[X] =>> X, A] = ??? }", "T.f") ===
      Count.Finite(1)

  // WithFlag's `Flag` is the top-level Boolean, not Use's Unit: (A, Boolean) has 2 values.
  def closureScope =
    count(
      box + "type WithFlag = [X] =>> (X, Flag)\ntype Flag = Boolean\n" +
        "object Use { type Flag = Unit; def f[A](a: A): Box[WithFlag, A] = ??? }",
      "Use.f"
    ) === Count.Finite(2)

  def curried =
    count(
      """object O {
        |  opaque type K[F[_], X, A] = F[A]
        |  type KF[F[_]] = [X, A] =>> K[F, X, A]
        |  def wrap[F[_], X, A](fa: F[A]): KF[F][X, A] = fa
        |}""".stripMargin,
      "O.wrap"
    ) === Count.Finite(1)

  def phantom =
    count(
      "final case class Phantom[F[_, _]]()\nobject T { def make: Phantom[Either] = ??? }",
      "T.make"
    ) ===
      Count.Finite(1)

  def pairAlias = {
    val counts = entries("""
      type Pair2[F[_, _], A] = F[A, A]
      object T {
        def eitherPair[A](a: A): Pair2[Either, A] = Right(a)
        def tuplePair[A](a: A): Pair2[Tuple2, A] = (a, a)
      }
    """)
    (counts("T.eitherPair") === Count.Finite(2)).and(counts("T.tuplePair") === Count.Finite(1))
  }

  // Before: `opaque representation is not visible at use site: PR`, a reason about an argument
  // whose kind nobody knows. Only the callee's reason is true.
  def foreignCallee =
    count(
      "import cats.Functor\nopaque type PR[A] = Option[A]\nobject T { def f: Functor[PR] = ??? }",
      "T.f"
    ) must beLike {
      case Count.Unresolved(reasons) =>
        (reasons.exists(_.contains("declaration not supplied: cats.Functor")) must beTrue)
          .and(reasons.exists(_.contains("PR")) must beFalse)
    }

  // Scala 3.9.0 compiles `object MF`, the companion of the top-level alias `MF`, into the file's
  // package object, where the opaque `MFK` is `(X, F[A])`: `(x, fa)` and `m._1`. An object that
  // accompanies no top-level type does not see it (E007 on `NotAlias.plain`): there `MFK` is one
  // value (rule F).
  def aliasCompanion = {
    val counts = entries("""
      opaque type MFK[F[_], X, A] = (X, F[A])
      type MF[F[_]] = [X, A] =>> MFK[F, X, A]
      object MF {
        def plain[F[_], X, A](x: X, fa: F[A]): MFK[F, X, A] = (x, fa)
        def back[F[_], X, A](m: MFK[F, X, A]): X = m._1
      }
      object NotAlias { def plain[F[_], X, A](x: X, fa: F[A]): MFK[F, X, A] = ??? }
    """)
    (counts("MF.plain") === Count.Finite(1))
      .and(counts("MF.back") === Count.Finite(1))
      .and(counts("NotAlias.plain") === Count.Finite(1))
  }

  // R4 -----------------------------------------------------------------------------------------
  def evidence = {
    val counts = entries("""
      final case class Default[A](value: A)
      object T {
        def withDefault[A: Default](x: A): A = x
        def without[A](x: A): A = x
      }
    """)
    (counts("T.withDefault") === Count.Finite(2)).and(counts("T.without") === Count.Finite(1))
  }

  def namedEvidence =
    count(
      "final case class Default[A](value: A)\nobject T { def f[A: Default as d](x: A): A = x }",
      "T.f"
    ) === Count.Finite(2)

  def hkEvidence =
    count(
      "final case class Point[F[_]](zero: F[Unit])\nobject T { def zero[F[_]: Point]: F[Unit] = ??? }",
      "T.zero"
    ) === Count.Finite(1)

  // `class K[A: Default](a: A)` is `class K[A](a: A)(using Default[A])`: `a` or the evidence's value.
  def classEvidence =
    count(
      "final case class Default[A](value: A)\nclass K[A: Default](a: A) { def get: A = a }",
      "K.get"
    ) ===
      Count.Finite(2)

  // A class's bound is the `using` parameter it stands for, also as a constructor position: the
  // estimate reads it inside the class, where `Tag` is a name it knows. `O.Tag[A]` is the sum
  // T1[A]() + T2[A]() = 2 whatever `A` is, so `Bound` and `Using` hold it beside `a`: 1 · 2 = 2.
  // `R.Mark` has a subtype written through a renamed import, which the estimate refuses and the
  // shape model misses (it would read 1 · 1 = 1; scalac's `R.Mark[Int]` holds `R.Z.M2`, so the
  // truth is 2): read at the top of the file, `Mark` was no name the estimate knew and that 1 stood.
  // Both spellings read alike: `K` and `KU` hold `Default[A]` beside `a` (4), `B` and `BU` a
  // `Boolean` beside `a` (2).
  def classBoundPosition = {
    val counts = entries("""
      object O {
        sealed trait Tag[A]
        final case class T1[A]() extends Tag[A]
        final case class T2[A]() extends Tag[A]
        final class Bound[A: Tag](a: A)
        final class Using[A](a: A)(using Tag[A])
        final case class Default[A](value: A)
        final class K[A: Default](a: A)
        final class KU[A](a: A)(using Default[A])
        type Ev[A] = Boolean
        final class B[A: Ev](a: A)
        final class BU[A](a: A)(using Ev[A])
      }
      object R {
        sealed trait Mark[A]
        final case class M1[A]() extends Mark[A]
        object Z {
          import R.{Mark as Q}
          final case class M2[A]() extends Q[A]
        }
        final class Bound[A: Mark](a: A)
        final class Using[A](a: A)(using Mark[A])
      }
    """)
    def unsummed(name: String) = counts(name) must beLike {
      case Count.Unresolved(reasons) =>
        reasons must contain(
          "sealed family not summed: Mark (a subtype extends it through a renamed import: M2)"
        )
    }
    (counts("O.Bound.<init>") === Count.Finite(2))
      .and(counts("O.Using.<init>") === Count.Finite(2))
      .and(unsummed("R.Bound.<init>"))
      .and(unsummed("R.Using.<init>"))
      .and(counts("O.K.<init>") === Count.Finite(4))
      .and(counts("O.KU.<init>") === Count.Finite(4))
      .and(counts("O.B.<init>") === Count.Finite(2))
      .and(counts("O.BU.<init>") === Count.Finite(2))
  }

  def foreignBound =
    unresolved(
      "import cats.Monoid\nobject T { def f[M: Monoid](m: M): M = m }",
      "T.f",
      "declaration not supplied: cats.Monoid (named import Monoid)"
    )

  // `summon[Default[A]]` is ambiguous (3.9.0: E172); exact count 1.
  def ambiguous = {
    val counts = entries("""
      final case class Default[A](value: A)
      object T { def f[A: Default: Default](x: A): A = x }
      trait FT2[C[_[_]]] { def twice[G[_]: C: C, A](x: G[A]): G[A] }
    """)
    (counts("T.f") must beLike {
      case Count.Unresolved(reasons) =>
        reasons must contain("ambiguous anonymous evidence: Default[A]")
    }).and(counts("FT2.twice") must beLike {
      case Count.Unresolved(reasons) =>
        reasons must contain("ambiguous anonymous evidence: C[G]")
    })
  }

  def anonymous =
    count("object T { def lost[A, B](x: A)(using Option[A], B): A = x }", "T.lost") === Count
      .Finite(2)

  // With A <: B, an `A` evidence also answers a search for B: the two are not unrelated. (3.9.0
  // selects the B parameter for `summon[B]`, so the exact count is 2; the overlap is refused.)
  def boundedEvidence =
    unresolved(
      "object T { def f[A <: B, B](using A, B): B = ??? }",
      "T.f",
      "anonymous evidence selection not resolved"
    )

  // R5, B1 -------------------------------------------------------------------------------------
  def upper = {
    val counts = entries("""
      object T {
        def ub[A <: B, B](a: A): B = a
        def ub2[A <: B, B](a: A, b: B): B = b
      }
    """)
    (counts("T.ub") === Count.Finite(1)).and(counts("T.ub2") === Count.Finite(2))
  }

  def noDowncast =
    count("object T { def f[A <: B, B](b: B): A = ??? }", "T.f") === Count.Finite(0)

  def lower = count("object T { def f[A >: B, B](b: B): A = b }", "T.f") === Count.Finite(1)

  def noBoundLift =
    count("object T { def f[F[_], A >: B, B](fb: F[B]): F[A] = ??? }", "T.f") === Count.Finite(0)

  def boundEquality =
    count("object T { def f[F[_], A >: B <: B, B](fb: F[B]): F[A] = fb }", "T.f") === Count.Finite(
      1
    )

  def extremes = {
    val counts = entries("""
      object T {
        def top[A <: Any](a: A): A = a
        def bottom[A >: Nothing, B](a: A): B = ???
      }
    """)
    (counts("T.top") === Count.Finite(1)).and(counts("T.bottom") === Count.Finite(0))
  }

  // R5, B2 -------------------------------------------------------------------------------------
  private val flag =
    "sealed trait Flag\ncase object On extends Flag\ncase object Off extends Flag\n"

  // A function of the received case: |Flag|^|Flag|; "return a" is the case identity among them.
  def upperSealed =
    count(flag + "object U { def f[A <: Flag](a: A): Flag = a }", "U.f") === Count.Finite(4)

  // Both split by case: every function {On, Off}² → Flag, 2^4.
  def upperPair =
    count(flag + "object U { def f[A <: Flag](a: A, b: A): Flag = a }", "U.f") === Count.Finite(16)

  def upperProduct = {
    val counts = entries("""
      final case class P[X](x: X, y: X)
      object U {
        def field[A <: P[B], B](a: A): B = ???
        def same[A <: P[B], B](a: A): A = a
      }
    """)
    (counts("U.field") === Count.Finite(2)).and(counts("U.same") === Count.Finite(1))
  }

  // The None case has no B: no total program.
  def upperOption = {
    val counts = entries("""
      object T {
        def f[A <: Option[B], B](a: A): B = ???
        def g[A <: Option[B], B](a: A, b: B): B = b
      }
    """)
    (counts("T.f") === Count.Finite(0)).and(counts("T.g") === Count.Finite(2))
  }

  // A may be Int itself: every function Int => Int, (2^32)^(2^32) = 2^(2^37).
  def upperGround =
    count("object T { def f[A <: Int](a: A): Int = a }", "T.f").render === "2^(2^37)"

  def upperNothing =
    count("object T { def f[A <: Nothing, B](a: A): B = ??? }", "T.f") === Count.Finite(1)

  // x, or Flag's two cases upcast: 3.
  def lowerSealed =
    count(flag + "object U { def f[A >: Flag](x: A): A = x }", "U.f") === Count.Finite(3)

  // f splits into two branches, each with x, On, Off: 3^2.
  def lowerSplit =
    count(flag + "object U { def f[A >: Flag](x: A, f: Flag): A = x }", "U.f") === Count.Finite(9)

  // A and B are unrelated binders: a is split by case, and B is built as On or Off in each branch
  // (a itself, also a B through Flag, is always one of them): 2^2.
  def upLow =
    count(flag + "object U { def f[A <: Flag, B >: Flag](a: A): B = a }", "U.f") === Count.Finite(4)

}
