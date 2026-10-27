package cardinality.types

import scala.meta.*

// Match types, read on a known scrutinee: the reduction behind `Fst[(Boolean, Boolean)]`. See
// `Counter.applied`.

private[cardinality] object MatchTypes {

  // Trees are parsed as Scala 3: printing one under any other dialect reprints it under Scala 2
  // rules, which cannot spell Scala 3's modifiers (`inline def` throws while printing).
  private given scala3: Dialect = dialects.Scala3

  type Pair = (Type, Type)

  private def expandPair(pair: Pair): List[Pair] = pair match {
    case (Type.Tuple(xs), Type.Tuple(ys)) if xs.size == ys.size => xs.zip(ys)
    case _                                                      => Nil
  }

  // The bindings a case pattern makes against a scrutinee, or None when it does not match. A
  // pattern variable is written the way Scala 3 spells one — a lowercase name — so `case Int =>`
  // matches a `Int` scrutinee by spelling and `case x =>` binds whatever it is given.
  private val bindingsHylo: Pair => Option[Map[TypeName, Type]] =
    TypeSubstitution.hylo[Pair, Option[Map[TypeName, Type]]](
      expandPair,
      (pair, kids) =>
        pair match {
          case (_: Type.Wildcard, _)                                             => Some(Map.empty)
          case (Type.Name(name), scrutinee) if name.headOption.exists(_.isLower) =>
            Some(Map(TypeName.of(name) -> scrutinee))
          case (Type.Tuple(elements), Type.Tuple(parts)) if elements.size == parts.size =>
            cardinality.analysis.inhabitation.Inhabitation.Shape
              .sequenceOptions(kids)
              .map(_.flatten.toMap)
          case (_: Type.Tuple, _)                                     => None
          case (other, scrutinee) if other.syntax == scrutinee.syntax => Some(Map.empty)
          case _                                                      => None
        }
    )

  val determinateHylo: Pair => Boolean =
    TypeSubstitution.hylo[Pair, Boolean](
      expandPair,
      (pair, kids) =>
        pair match {
          case (Type.Tuple(xs), Type.Tuple(ys)) =>
            xs.size != ys.size || kids.forall(identity)
          case (_: Type.Tuple, _) => false
          case _                  => true
        }
    )

  val projectionPattern: Type => Boolean =
    TypeSubstitution.hylo[Type, Boolean](
      {
        case Type.Tuple(parts) => parts
        case _                 => Nil
      },
      (tpe, kids) =>
        tpe match {
          case _: Type.Wildcard => true
          case Type.Name(name)  => name.headOption.exists(_.isLower)
          case _: Type.Tuple    => kids.forall(identity)
          case _                => false
        }
    )

  // A match type read on a known scrutinee: every case pattern is matched against the scrutinee with
  // the definition's parameters already replaced by the arguments, and the first case that matches
  // gives its body with the pattern's binders replaced by the parts of the scrutinee they stood
  // for. The patterns a case can have are the ones a reducer earns: a tuple of binders, a bare
  // binder, a wildcard, and anything else by spelling.
  private[cardinality] def reduced(matchType: Type, arguments: Map[TypeName, Type]): Option[Type] =
    TypeSubstitution.replace(matchType, arguments) match {
      case Type.Match.After_4_9_9(scrutinee, block) =>
        block.cases.collectFirst(Function.unlift(caseOf(_, scrutinee)))
      case other => Some(other)
    }

  private def caseOf(caseType: TypeCase, scrutinee: Type): Option[Type] =
    bindingsHylo((caseType.pat, scrutinee)).map(TypeSubstitution.replace(caseType.body, _))

}
