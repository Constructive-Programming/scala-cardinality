package cardinality.types

import scala.meta.*

// Match types, read on a known scrutinee: the reduction behind `Fst[(Boolean, Boolean)]`. See
// `Counter.applied`.

private[cardinality] object MatchTypes {

  type Pair = (Type, Type)

  // A named tuple is no tuple here: a match on one is stuck (`NamedTuples`).
  private def expandPair(pair: Pair): List[Pair] = pair match {
    case (PositionalTuple(xs), PositionalTuple(ys)) if xs.size == ys.size => xs.zip(ys)
    case _                                                                => Nil
  }

  // The bindings a case pattern makes against a scrutinee, or None when it does not match. A
  // pattern variable is written the way Scala 3 spells one — a lowercase name — so `case x =>` binds
  // whatever it is given; a tuple of them binds element by element. Any other pattern is not read.
  private val bindings: Pair => Option[Map[TypeName, Type]] =
    TypeSubstitution.hylo[Pair, Option[Map[TypeName, Type]]](
      expandPair,
      (pair, kids) =>
        pair match {
          case (Type.Name(name), scrutinee) if name.headOption.exists(_.isLower) =>
            Some(Map(TypeName.of(name) -> scrutinee))
          case (PositionalTuple(elements), PositionalTuple(parts)) if elements.size == parts.size =>
            Traversals.defined(kids).map(_.flatten.toMap)
          case _ => None
        }
    )

  val determinateHylo: Pair => Boolean =
    TypeSubstitution.hylo[Pair, Boolean](
      expandPair,
      (pair, kids) =>
        pair match {
          case (PositionalTuple(xs), PositionalTuple(ys)) =>
            xs.size != ys.size || kids.forall(identity)
          case (_: Type.Tuple, _) => false
          case _                  => true
        }
    )

  val projectionPattern: Type => Boolean =
    TypeSubstitution.hylo[Type, Boolean](
      {
        case PositionalTuple(parts) => parts
        case _                      => Nil
      },
      (tpe, kids) =>
        tpe match {
          case Type.Name(name)    => name.headOption.exists(_.isLower)
          case PositionalTuple(_) => kids.forall(identity)
          case _                  => false
        }
    )

  // A match type read on a known scrutinee: every case pattern is matched against the scrutinee
  // with the definition's parameters already replaced by the arguments, and the first case that
  // matches gives its body with the pattern's binders replaced by the parts of the scrutinee they
  // stood for. The patterns are those a projection earns — a tuple of binders, a bare binder, a
  // wildcard — and never a spelling, whose names the method side does not read here.
  private[cardinality] def reduced(matchType: Type, arguments: Map[TypeName, Type]): Option[Type] =
    TypeSubstitution.replace(matchType, arguments) match {
      case Type.Match.After_4_9_9(scrutinee, block) =>
        block.cases.collectFirst(Function.unlift(caseOf(_, scrutinee)))
      case _ => None
    }

  /** The case a match type takes on known arguments, unsubstituted, with what each of its names
    * stands for: the definition's parameters their arguments, and the pattern's binders the parts
    * of the scrutinee they matched — so a reader can read the arguments where they are written and
    * the body where the match type is. Only the first case is tried: one that does not match leaves
    * the match type stuck.
    */
  private[cardinality] def matched(
      matchType: Type,
      arguments: Map[TypeName, Type]
  ): Option[(Type, Map[TypeName, Type])] =
    matchType match {
      case Type.Match.After_4_9_9(scrutinee, block) =>
        val known = TypeSubstitution.replace(scrutinee, arguments)
        block.cases.headOption.flatMap { c =>
          bindings((TypeSubstitution.replace(c.pat, arguments), known))
            .map(bound => c.body -> (arguments ++ bound))
        }
      case _ => None
    }

  private def caseOf(caseType: TypeCase, scrutinee: Type): Option[Type] =
    bindings((caseType.pat, scrutinee)).map(TypeSubstitution.replace(caseType.body, _))

}
