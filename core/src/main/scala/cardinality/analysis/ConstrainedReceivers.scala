package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.{Card, ClosedSpaces, Natural}
import cardinality.analysis.model.MemberSyntax
import cardinality.types.Traversals

import MethodAnalysis.{Frame, Resolved}

/** The values a receiver's fields hold together: what its public constructors build (v1.md §2,
  * receiver constructors). A restriction on the class itself, or on a template around it, and a
  * class local to a block, are disregarded: such a class has no use site outside its scope, so its
  * constructors are public wherever it is used. A constructor with its own restriction is not
  * public and is ignored; the receiver's space is the largest any public constructor builds.
  *
  * Every secondary constructor delegates to the primary, so a public primary builds the fields'
  * whole space and the fields hold any values of their types. Otherwise a public secondary builds
  * at most its parameters' space and at most the fields', the smaller of the two; where the
  * parameters and the fields are closed, the largest of these is the receiver's space. A receiver
  * with no public constructor (only a factory builds it, `NonEmpty.of`) has no space this reading
  * knows, and neither has one whose spaces are not closed or not comparable.
  */
private[cardinality] object ConstrainedReceivers {

  /** None where the fields hold any values of their types; Some(space) where the receiver's closed
    * fields hold `space` values together; a reason where the receiver's space is unknown.
    */
  def space(owner: Frame, resolve: Type => Resolved): Either[String, Option[Card]] =
    owner.tree match {
      case Some(d: Defn.Class)
          if owner.params.nonEmpty && d.ctor.mods.exists(MemberSyntax.restricted) =>
        val path = owner.path.mkString(".")
        val public = d.templ.body.stats.collect {
          case c: Ctor.Secondary if !c.mods.exists(MemberSyntax.restricted) =>
            closed(
              SecondaryConstructors.arguments(c.paramClauses.toList.flatMap(_.values)),
              resolve
            )
        }
        if (public.isEmpty) Left(s"receiver has no public constructor: $path")
        else
          largest(closed(SecondaryConstructors.primaryArguments(d), resolve), public)
            .toRight(s"receiver space of its public secondary constructors not compared: $path")
      case _ => Right(None)
    }

  // The largest of min(a secondary's space, the fields'), None when it is the fields' own.
  private def largest(fields: Option[Card], built: List[Option[Card]]): Option[Option[Card]] =
    for {
      whole <- fields
      spaces <- Traversals.defined(built)
      bounded <- Traversals.defined(
        spaces.map(p => compare(p, whole).map(c => if (c < 0) p else whole))
      )
      top <- bounded.tail.foldLeft(Option(bounded.head))((acc, p) =>
        acc.flatMap(a => compare(a, p).map(c => if (c < 0) p else a))
      )
    } yield Option.unless(top == whole)(top)

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
