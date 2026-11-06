package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.{Card, ClosedSpaces, Natural}
import cardinality.analysis.model.MemberSyntax
import cardinality.types.Traversals

import MethodAnalysis.{Frame, Resolved}

/** The values a receiver's fields hold together: what its public constructors build (v1.md §2,
  * receiver constructors). A restriction on the class itself, or on a template around it, and a
  * class local to a block, are disregarded: such a class has no use site outside its scope, so its
  * constructors are public wherever it is used. A constructor is public where the measured
  * declaration may call it: one with no restriction, a `private[p]` one inside `p`, a `private` or
  * `protected` one inside its class, and a `private` one in its companion. Any other is ignored.
  *
  * Every secondary constructor delegates to the primary, so a public primary builds the fields'
  * whole space and the fields hold any values of their types. Otherwise the class is counted by
  * what the site may call to build one: each public secondary and each public factory
  * (`ReceiverFactories`) builds at most its parameters' space and at most the fields', the smaller
  * of the two, and the receiver's space is the largest of these — the fields' space as soon as one
  * reaches it, else the largest where every bound is known and comparable. A class nothing public
  * builds (no secondary, no factory) has no space this reading knows.
  */
private[cardinality] object ConstrainedReceivers {

  /** None where a public primary constructor builds every value of the fields' types; Some(space)
    * where only secondaries and factories build the receiver, its closed fields holding `space`
    * values together; a reason where the receiver's space is unknown.
    */
  def space(
      owner: Frame,
      from: Frame,
      resolver: Resolver,
      resolve: Type => Resolved
  ): Either[String, Option[Card]] = {
    // Whether the measured declaration may call a constructor with these modifiers.
    def callable(mods: List[Mod]): Boolean =
      !mods.exists(MemberSyntax.restricted) || resolver.reaches(mods, owner, from) ||
        !mods.exists(_.is[Mod.Protected]) && companion(owner, from)
    owner.tree match {
      case Some(d: Defn.Class) if !callable(d.ctor.mods) =>
        val path = owner.path.mkString(".")
        val secondaries = d.templ.body.stats.collect {
          case c: Ctor.Secondary if callable(c.mods) =>
            closed(
              SecondaryConstructors.arguments(c.paramClauses.toList.flatMap(_.values)),
              resolve
            )
        }
        val factories = ReceiverFactories.of(
          owner,
          from,
          resolver,
          (tpe, scope, bindings) => closed(tpe, resolver.resolve(_, scope, bindings))
        )
        if (secondaries.isEmpty && factories.isEmpty)
          Left(s"receiver has no public constructor: $path")
        else
          largest(
            closed(SecondaryConstructors.primaryArguments(d), resolve),
            secondaries ++ factories
          )
            .map(Some(_))
            .toRight(s"receiver space of its public constructors and factories not compared: $path")
      case _ => Right(None)
    }
  }

  // Whether `from` is inside the class's companion object.
  private def companion(owner: Frame, from: Frame): Boolean =
    from.chain.exists(f =>
      f.parent.map(_.id) == owner.parent.map(_.id) && f.name == owner.name &&
        f.tree.exists(_.is[Defn.Object])
    )

  // The largest of min(a constructor's or factory's space, the fields'): the fields' own as soon as
  // one reaches it, else the largest where each is known and comparable.
  private def largest(fields: Option[Card], built: List[Option[Card]]): Option[Card] =
    fields.flatMap { whole =>
      val bounded = built.map(_.flatMap(p => compare(p, whole).map(c => if (c < 0) p else whole)))
      if (bounded.contains(Some(whole))) Some(whole)
      else
        Traversals.defined(bounded).flatMap { spaces =>
          spaces.tail.foldLeft(Option(spaces.head))((acc, p) =>
            acc.flatMap(a => compare(a, p).map(c => if (c < 0) p else a))
          )
        }
    }

  private def closed(tpe: Type, resolve: Type => Resolved): Option[Card] =
    resolve(tpe).toOption
      .filter(ClosedSpaces.closed)
      .flatMap(shape => Natural.attempt(ClosedSpaces.space(shape)).toOption)

  // Magnitudes: ω above every exact count, a literal below every symbolic one (each at least
  // 2^65536), two symbolic ones only when they are the same.
  private def compare(a: Card, b: Card): Option[Int] = (a, b) match {
    case _ if a == b                                              => Some(0)
    case (Card.Omega, _)                                          => Some(1)
    case (_, Card.Omega)                                          => Some(-1)
    case (Card.Exact(Natural.Lit(x)), Card.Exact(Natural.Lit(y))) => Some(x.compare(y))
    case (Card.Exact(Natural.Lit(_)), _)                          => Some(-1)
    case (_, Card.Exact(Natural.Lit(_)))                          => Some(1)
    case _                                                        => None
  }

}
