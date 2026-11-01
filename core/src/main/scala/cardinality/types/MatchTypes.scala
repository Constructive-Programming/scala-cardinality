package cardinality.types

import scala.annotation.tailrec
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
  // matches an `Int` scrutinee by spelling and `case x =>` binds whatever it is given. A spelling
  // matches only where each of its names means the same declaration as the pattern reads it and as
  // the scrutinee does (`same`): `Int` written where a source binds `type Int = String` is not the
  // pattern's `Int`.
  private def bindings(same: String => Boolean): Pair => Option[Map[TypeName, Type]] =
    TypeSubstitution.hylo[Pair, Option[Map[TypeName, Type]]](
      expandPair,
      (pair, kids) =>
        pair match {
          case (_: (Type.Wildcard | Type.PatWildcard), _)                        => Some(Map.empty)
          case (Type.Name(name), scrutinee) if name.headOption.exists(_.isLower) =>
            Some(Map(TypeName.of(name) -> scrutinee))
          case (Type.Tuple(elements), Type.Tuple(parts)) if elements.size == parts.size =>
            Traversals.defined(kids).map(_.flatten.toMap)
          case (_: Type.Tuple, _) => None
          case (other, scrutinee)
              if other.syntax == scrutinee.syntax && TypeName.words(other).forall(same) =>
            Some(Map.empty)
          case _ => None
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

  // A match type read on a known scrutinee: every case pattern is matched against the scrutinee
  // with the definition's parameters already replaced by the arguments, and the first case that
  // matches gives its body with the pattern's binders replaced by the parts of the scrutinee they
  // stood for. The patterns are those a projection earns — a tuple of binders, a bare binder, a
  // wildcard — and never a spelling, whose names the method side does not read here.
  private[cardinality] def reduced(matchType: Type, arguments: Map[TypeName, Type]): Option[Type] =
    TypeSubstitution.replace(matchType, arguments) match {
      case Type.Match.After_4_9_9(scrutinee, block) =>
        block.cases.collectFirst(Function.unlift(caseOf(_, scrutinee)))
      case other => Some(other)
    }

  /** The case a match type takes on known arguments, unsubstituted, with what each of its names
    * stands for: the definition's parameters their arguments, and the pattern's binders the parts
    * of the scrutinee they matched — so a reader can read the arguments where they are written and
    * the body where the match type is. A case that does not match is passed over only when its
    * pattern provably shares no value with the scrutinee (`disjoint`); otherwise the match type is
    * stuck there, as Scala's is. `meaning` says whether a name means one declaration wherever the
    * match reads it, and whether that is the standard library's type.
    */
  private[cardinality] def matched(
      matchType: Type,
      arguments: Map[TypeName, Type],
      meaning: Meaning
  ): Option[(Type, Map[TypeName, Type])] =
    matchType match {
      case Type.Match.After_4_9_9(scrutinee, block) =>
        val known = TypeSubstitution.replace(scrutinee, arguments)
        @tailrec def first(cases: List[TypeCase]): Option[(Type, Map[TypeName, Type])] =
          cases match {
            case c :: rest =>
              val pattern = TypeSubstitution.replace(c.pat, arguments)
              bindings(meaning.same)((pattern, known)) match {
                case Some(bound) => Some(c.body -> (arguments ++ bound))
                case None if disjoint(pattern, known, meaning.standard) => first(rest)
                case None                                               => None
              }
            case Nil => None
          }
        first(block.cases)
      case other => Some(other -> arguments)
    }

  // The standard library's final classes a match type can name: no value belongs to two of them,
  // nor to one of them and a tuple.
  private val finals: Set[String] =
    Set("Int", "Long", "Short", "Byte", "Char", "Float", "Double", "Boolean", "Unit", "String")

  // Whether no value is both the pattern's and the scrutinee's: tuples of different arities or
  // with a disjoint element pair, different literals, and otherwise two different classes of which
  // one is final — a tuple, a literal's class or a final standard type — beside anything but a
  // trait it extends (none of them extends a function type).
  private def disjoint(pattern: Type, scrutinee: Type, standard: String => Boolean): Boolean = {
    def kind(tpe: Type): Option[(String, Boolean)] = tpe match {
      case Type.Tuple(args)                         => Some(s"Tuple${args.size}" -> true)
      case Type.Name(n) if finals(n) && standard(n) => Some(n -> true)
      case _: Type.FunctionType                     => Some("Function" -> false)
      case _: Lit.Int                               => Some("Int" -> true)
      case _: Lit.Long                              => Some("Long" -> true)
      case _: Lit.Boolean                           => Some("Boolean" -> true)
      case _: Lit.Char                              => Some("Char" -> true)
      case _: Lit.Double                            => Some("Double" -> true)
      case _: Lit.Float                             => Some("Float" -> true)
      case _: Lit.String                            => Some("String" -> true)
      case _: Lit.Unit                              => Some("Unit" -> true)
      case _                                        => None
    }
    (pattern, scrutinee) match {
      case (Type.Tuple(ps), Type.Tuple(ss)) =>
        ps.size != ss.size || ps.zip(ss).exists((p, s) => disjoint(p, s, standard))
      case (l: Lit, r: Lit) => l.structure != r.structure
      case _                =>
        kind(pattern).zip(kind(scrutinee)).exists {
          case ((a, af), (b, bf)) => a != b && (af || bf)
        }
    }
  }

  private def caseOf(caseType: TypeCase, scrutinee: Type): Option[Type] =
    bindings(_ => false)((caseType.pat, scrutinee)).map(TypeSubstitution.replace(caseType.body, _))

  /** What a match reading knows of a name: whether it means one declaration wherever the match
    * reads it (`same`), and whether that is the standard library's type (`standard`).
    */
  final case class Meaning(same: String => Boolean, standard: String => Boolean)

}
