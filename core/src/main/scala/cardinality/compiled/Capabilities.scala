package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.{Binder, Shape}
import cardinality.analysis.inhabitation.PolymorphicMembers
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.*

/** A capability's own type parameters, read for a member a record declares (`Shapes.member`) and
  * for one it inherits (`Receivers`) alike.
  */
private[compiled] object Capabilities {
  import Unread.Uncovered

  // A method capability: one function from its parameters to its result, which `result` and
  // `inputs` read with its own type parameters bound, as the scalameta front end reads them
  // (`PolymorphicMembers.close`); a one-valued result supplies nothing, whatever its inputs are.
  def method(method: TermSymbol, own: List[Symbol], env: Shapes.Env)(using
      Context
  )(
      result: Shapes.Env => Either[Unread, Shape],
      inputs: Shapes.Env => Either[Unread, List[Shape]]
  ): Either[Unread, Option[Shape]] = for {
    typed <- polymorphic(method, own)
    inside = env.copy(binders = env.binders ++ typed.map((p, shape, _) => p -> shape))
    output <- result(inside)
    read <- if (output == Shape.Product(Nil)) Right(Nil) else inputs(inside)
    closed <- PolymorphicMembers
      .close(method.displayFullName, typed.map(_._3), Shape.Function(read, output))
      .left
      .map(Uncovered(_))
  } yield Option.when(output != Shape.Product(Nil))(closed)

  // A capability's own type parameters, each under the identity `PolymorphicMembers.close` looks
  // for: the member's own atom, or a rigid constructor for an `F[_]`, with the bounds it declares.
  private def polymorphic(method: TermSymbol, parameters: List[Symbol])(using Context) =
    Traversals.sequence(parameters.map { symbol =>
      val parameter = symbol.asInstanceOf[TypeParamSymbol]
      val (name, bounds) = (parameter.name.toString, parameter.declaredBounds)
      // The old front end reads any constructor here as rigid (`MemberSyntax`): a bound or a
      // variance it does not refuse is not read yet.
      val read = Shapes.binder(method, parameter).left.map {
        case Unread.Refused(_) =>
          Uncovered(s"a type parameter of another kind than an invariant F[_]: $name")
        case uncovered => uncovered
      }
      read.map { read =>
        // A rigid constructor's bounds are a type lambda's, which `Shapes.binder` checked.
        val proper = !read.isInstanceOf[Shape.Constructor]
        val declared = PolymorphicMembers.Declared(
          name,
          lower = proper && !bounds.low.isSameType(defn.NothingType),
          upper = proper && !bounds.high.isSameType(defn.AnyType),
          view = false
        )
        val id = Binder(method.displayFullName, name).id
        (symbol, if (proper) Shape.Atom(id) else Shape.Constructor(id), declared)
      }
    })

}
