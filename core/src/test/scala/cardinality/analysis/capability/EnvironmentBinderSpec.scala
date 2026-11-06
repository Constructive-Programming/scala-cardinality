package cardinality.analysis.capability

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.execute.Result

// PB5: a polymorphic capability member's binders fixed by the applied values its environment
// supplies (docs/research/polymorphic-members-and-saturation.md). The fixture (valid Scala 3.9.0,
// compiled with scalac) is eo v1.1.1's ReadCompose cells and ForgetfulApplicative with the optic
// reduced to its read half (`to`), so the recursive `Optic` record (parked) does not hide the
// counts; each count is derived above its check.
class EnvironmentBinderSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Binders fixed by the applied values the environment supplies (PB5)
      a constructor's evidence fields: one get, None or getOption each    $evidence
      a composite read through the optics' results                        $composites
      a member no supplied value can be applied to builds nothing         $pure
      an instance's result supplies the next instantiation                $nested
      an open producer of the argument type keeps the refusal            $open
  """

  private val found: Map[String, Count] =
    MethodAnalysis
      .analyze(List(MethodAnalysis.Input("Env.scala", dialects.Scala3(code).parse[Source].get)))
      .map(entry => entry.name -> entry.count)
      .toMap

  private def code = """package binders
    |trait Accessor[F[_, _]] { def get[X, A](fa: F[X, A]): A }
    |trait PartialAccessor[F[_, _]] { def getOption[X, A](fa: F[X, A]): Option[A] }
    |trait ForgetfulFunctor[F[_, _]] { def map[X, A, B](fa: F[X, A], f: A => B): F[X, B] }
    |trait ForgetfulApplicative[F[_, _]] extends ForgetfulFunctor[F] {
    |  def pure[X, A](a: A): F[X, A]
    |}
    |trait Optic[S, A, F[_, _]] { type X; def to(s: S): F[X, A] }
    |final class Getter[S, A](val read: S => A)
    |final class PickFold[S, A](val pick: S => Option[A])
    |trait ReadCompose[F[_, _], G[_, _]] {
    |  type Out[_, _]
    |  def compose[S, A, C](outer: Optic[S, A, F], inner: Optic[A, C, G]): Out[S, C]
    |}
    |object ReadCompose {
    |  given totalTotal[F[_, _], G[_, _]](using AO: Accessor[F], AI: Accessor[G]): ReadCompose[F, G] with
    |    type Out[S, C] = Getter[S, C]
    |    def compose[S, A, C](outer: Optic[S, A, F], inner: Optic[A, C, G]): Getter[S, C] =
    |      Getter(s => AI.get(inner.to(AO.get(outer.to(s)))))
    |  given totalPartial[F[_, _], G[_, _]](using AO: Accessor[F], PI: PartialAccessor[G]): ReadCompose[F, G] with
    |    type Out[S, C] = PickFold[S, C]
    |    def compose[S, A, C](outer: Optic[S, A, F], inner: Optic[A, C, G]): PickFold[S, C] =
    |      PickFold(s => PI.getOption(inner.to(AO.get(outer.to(s)))))
    |  given partialTotal[F[_, _], G[_, _]](using PO: PartialAccessor[F], AI: Accessor[G]): ReadCompose[F, G] with
    |    type Out[S, C] = PickFold[S, C]
    |    def compose[S, A, C](outer: Optic[S, A, F], inner: Optic[A, C, G]): PickFold[S, C] =
    |      PickFold(s => PO.getOption(outer.to(s)).map(a => AI.get(inner.to(a))))
    |  given partialPartial[F[_, _], G[_, _]](using PO: PartialAccessor[F], PI: PartialAccessor[G]): ReadCompose[F, G] with
    |    type Out[S, C] = PickFold[S, C]
    |    def compose[S, A, C](outer: Optic[S, A, F], inner: Optic[A, C, G]): PickFold[S, C] =
    |      PickFold(s => PO.getOption(outer.to(s)).flatMap(a => PI.getOption(inner.to(a))))
    |}
    |trait Get[F[_]] { def get[A](fa: F[A]): A }
    |trait Mk[F[_, _]] { def make[X, A](a: A): F[X, A] }
    |object U {
    |  def nested[F[_], B](ffb: F[F[B]], t: Get[F]): B = t.get(t.get(ffb))
    |  def two[F[_], B](x: F[B], y: F[B], t: Get[F]): B = t.get(x)
    |  def open[F[_, _], A](fa: F[Int, A], acc: Accessor[F], mk: Mk[F]): A = acc.get(fa)
    |}
    |""".stripMargin

  private def cell(name: String) = found(s"binders.ReadCompose.$name")

  // Each constructor builds its evidence fields from its own. An Accessor[F] is a get[X, A] for
  // fresh X, A with fa: F[X, A] bound: AO.get applies only at the type of a supplied F value,
  // fa's, so AO.get(fa) is the one A (AI.get has no G value: irrelevant) — 1. A
  // PartialAccessor[G] is None or PI.getOption(fa), whose Some(a) case gives Some(a) again — 2.
  // totalTotal 1·1, totalPartial 1·2, partialTotal 2·1, partialPartial 2·2.
  def evidence =
    (cell("totalTotal.<init>") === Count.Finite(1))
      .and(cell("totalPartial.<init>") === Count.Finite(2))
      .and(cell("partialTotal.<init>") === Count.Finite(2))
      .and(cell("partialPartial.<init>") === Count.Finite(4))

  // The F and G values are the optics' results, outer.to(s): F[outer.X, A] and
  // inner.to(a): G[inner.X, C], so get and getOption apply at those instantiations only. Total
  // reads give one C: AI.get(inner.to(AO.get(outer.to(s)))) — 1. Through a partial read the
  // composite is that read or None — 2; partialPartial cases the outer read (None: None; Some(a):
  // None or the inner read), which is None or the flatMap — 2.
  def composites =
    (cell("totalTotal.compose") === Count.Finite(1))
      .and(cell("totalPartial.compose") === Count.Finite(2))
      .and(cell("partialTotal.compose") === Count.Finite(2))
      .and(cell("partialPartial.compose") === Count.Finite(2))

  // pure[X, A](a: A): F[X, A] beside map: only map builds an F value, and only from one; the
  // environment holds none, so map is applied at no type — 0.
  def pure = found("binders.ForgetfulApplicative.pure") === Count.Finite(0)

  // get at A := F[B] gives an F[B], at which get applies again: get(get(ffb)) — 1. Two supplied
  // F[B] values give two B: get(x), get(y) — 2.
  def nested =
    (found("binders.U.nested") === Count.Finite(1))
      .and(found("binders.U.two") === Count.Finite(2))

  // make builds an F[X, A] for any X its caller picks, so the instantiations of get are not the
  // supplied ones alone (get(make(get(fa))), …): refused.
  def open: Result =
    found("binders.U.open") must beLike {
      case Count.Unresolved(reasons) =>
        reasons must contain(
          "polymorphic capability binder: X (instantiations not closed: a polymorphic producer " +
            "builds an argument of binders.Accessor.get)"
        )
    }

}
