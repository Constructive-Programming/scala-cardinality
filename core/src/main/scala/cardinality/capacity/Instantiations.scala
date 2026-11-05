package cardinality.capacity

import scala.meta.*

import cardinality.types.*

// `C[ā]` read as C's equation with its parameters bound to what the arguments are worth — `Pair
// [Boolean]` is 4, not an unknown constructor. Two recursive readings stay careful: inside C's own
// equation, `C` applied to C's own parameters borrows the fixed point being found (how a
// parameterised recursion keeps its μ/ν reading) — the same spelling read anywhere else is an
// instantiation like any other — and one re-entered while its own name is substituting has no such
// fixed point: a reason that says so.
private[capacity] object Instantiations {

  /** One reference `callee[args]` read in `scope`: the definition `named` it resolved to, solved
    * under `name` in `context`.
    */
  final case class Instance(
      scope: Scope,
      callee: Type,
      args: List[Type],
      name: TypeName,
      context: Scope,
      named: Named
  ) {

    def written: Map[TypeName, Type] = named.params.zip(args).toMap

    // The definition's own scope, guarding its re-entry.
    def entered: Scope = Scope.activeLens.modify(_ + name)(context)
  }

  // What an instantiation binds its names to: their sizes, the ones standing for a parameter the
  // reader has not fixed (which the reader has already named as its reason), and what each was
  // written as, with the scope that reads it.
  final private case class Frame(
      sizes: Map[TypeName, Size],
      stand: Set[TypeName],
      held: Map[TypeName, Argument]
  ) {

    def ++(other: Frame): Frame =
      Frame(sizes ++ other.sizes, stand ++ other.stand, held ++ other.held)

    def in(scope: Scope): Scope = scope.instantiating(sizes, stand, held)
  }

  /** One instantiation, read: a re-entry — a recursion through instantiations, its own included —
    * is a reason, a match type reduces on the arguments' syntax, and everything else substitutes
    * the arguments into the equation.
    */
  def read(i: Instance): Size =
    if (Scope.activeLens.get(i.context)(i.name)) {
      // Re-entered (`Ev[A]` inside `Od[A]` inside `Ev[A]`): no fixed point is solved for it, and
      // the name itself is not unknown.
      i.scope.note(
        Definition.Unbound.Refusal(s"recursive instantiation not solved: ${Spelling.of(i.callee)}")
      )
      EffectiveOmega
    } else i.named.matchType.fold(substituted(i))(reduced(i, _))

  // The instantiation's frame is the only one its equation reads: the definition's names resolve
  // where it was written, not in whatever instantiation the reference sits in.
  private def substituted(i: Instance): Size =
    i.named.equation(frame(i.scope, i.written).in(i.entered))

  // A match type reduces on the arguments' *syntax*. They, and the parts of them a case pattern
  // binds, are read where the reference is; the case body where the match type is written, with
  // those bound. A scrutinee that is a parameter applied to a name an enclosing instantiation bound
  // (`Fst[A]` in a family's subtype read for `Affine[(T, M0), B]`) reduces on what that
  // instantiation was written with, and the parts a pattern binds read where it was.
  private def reduced(i: Instance, matchType: Type): Size = {
    val written = i.written
    val inspected = scrutinee(matchType, written)
    val through = inspected.flatMap(p => argumentOf(i.scope, written(p)).filterNot(_.in eq i.scope))
    val arguments = through.fold(written)(a => written ++ inspected.map(_ -> a.tpe))
    val parts = through.fold(i.scope)(_.in)
    MatchTypes.matched(matchType, arguments) match {
      case Some((body, bindings)) =>
        val own = bindings.filter((n, _) => written.contains(n)).keySet
        val bound = frame(i.scope, written.filter((p, _) => own(p))) ++
          frame(parts, bindings -- written.keys)
        Counter.typeIn(i.named.read(bound.in(i.entered)))(body)
      case None =>
        through.fold(stuck(i.scope, matchType, i.args))(a => stuck(a.in, matchType, List(a.tpe)))
    }
  }

  private def frame(in: Scope, bindings: Map[TypeName, Type]): Frame =
    Frame(
      bindings.map((param, arg) => param -> Counter.typeIn(in)(arg)),
      placeholders(in, bindings),
      bindings.map((param, arg) => param -> argument(in, arg))
    )

  // The parameter a match type inspects, when its scrutinee is one of them as written.
  private def scrutinee(matchType: Type, written: Map[TypeName, Type]): Option[TypeName] =
    matchType match {
      case Type.Match.After_4_9_9(Type.Name(n), _) => Some(TypeName.of(n)).filter(written.contains)
      case _                                       => None
    }

  // What an argument stands for as written: a name an enclosing instantiation bound is what that
  // instantiation was written with, anything else itself, read here.
  private def argumentOf(scope: Scope, arg: Type): Option[Argument] = arg match {
    case Type.Name(n) => scope.argument(TypeName.of(n))
    case _            => None
  }

  private def argument(scope: Scope, arg: Type): Argument =
    argumentOf(scope, arg).getOrElse(Argument(arg, scope))

  // The parameters an instantiation binds to a parameter its reader has not fixed.
  private def placeholders(scope: Scope, bindings: Map[TypeName, Type]): Set[TypeName] =
    bindings.collect {
      case (param, Type.Name(arg)) if scope.unfixed(TypeName.of(arg)) => param
    }.toSet

  /** A match type that does not reduce. Its scrutinee still carries its own reasons. Stuck only on
    * parameters the reading has not fixed — the definition's own, or the template's a reference
    * reads — it is a function of the instantiation like any other use of them: each instantiation
    * either reduces it or is stuck itself, so the parameters are the reasons (the definitions the
    * scrutinee names, `Fst[T]`, are the same for every instantiation). Stuck on anything else (a
    * case it cannot pass over, a parameter bound to a size whose syntax is gone) it is a reason.
    */
  def stuck(scope: Scope, matchType: Type, scrutinees: List[Type]): Size = {
    scrutinees.foreach(Counter.typeIn(scope))
    val parameters = scrutinees
      .flatMap(TypeSubstitution.typeNames)
      .map(TypeName.of)
      .filter(n => scope.frame(n).isDefined || scope.unboundBinder(n))
    if (parameters.isEmpty || !parameters.forall(scope.unfixed)) scope.note(matchType)
    EffectiveOmega
  }

}
