package cardinality

import scala.meta.*

// Match types, read on a known scrutinee: the reduction behind `Fst[(Boolean, Boolean)]`. See
// `Counter.applied`.

private[cardinality] object MatchTypes {

  // Trees are parsed as Scala 3: printing one under any other dialect reprints it under Scala 2
  // rules, which cannot spell Scala 3's modifiers (`inline def` throws while printing).
  private given scala3: Dialect = dialects.Scala3

  // A match type read on a known scrutinee: every case pattern is matched against the scrutinee with
  // the definition's parameters already replaced by the arguments, and the first case that matches
  // gives its body with the pattern's binders replaced by the parts of the scrutinee they stood
  // for. The patterns a case can have are the ones a reducer earns: a tuple of binders, a bare
  // binder, a wildcard, and anything else by spelling.
  private[cardinality] def reduced(matchType: Type, arguments: Map[TypeName, Type]): Option[Type] =
    replace(matchType, arguments) match {
      case Type.Match(scrutinee, cases) =>
        cases.collectFirst(Function.unlift(caseOf(_, scrutinee)))
      case other => Some(other)
    }

  private def caseOf(caseType: TypeCase, scrutinee: Type): Option[Type] =
    bindings(caseType.pat, scrutinee).map(replace(caseType.body, _))

  // The bindings a case pattern makes against a scrutinee, or None when it does not match. A
  // pattern variable is written the way Scala 3 spells one — a lowercase name — so `case Int =>`
  // matches a `Int` scrutinee by spelling and `case x =>` binds whatever it is given.
  private def bindings(pattern: Type, scrutinee: Type): Option[Map[TypeName, Type]] =
    pattern match {
      case _: Type.Wildcard                                     => Some(Map.empty)
      case Type.Name(name) if name.headOption.exists(_.isLower) =>
        Some(Map(TypeName.of(name) -> scrutinee))
      case Type.Tuple(elements) =>
        scrutinee match {
          case Type.Tuple(parts) if parts.size == elements.size =>
            elements
              .zip(parts)
              .foldLeft(Option(Map.empty[TypeName, Type])) {
                case (bound, (element, part)) =>
                  for {
                    known <- bound
                    elementBindings <- bindings(element, part)
                  } yield known ++ elementBindings
              }
          case _ => None
        }
      // A name, literal or applied pattern the calculator does not read binds nothing and matches
      // when its spelling is the scrutinee's.
      case other if other.syntax == scrutinee.syntax => Some(Map.empty)
      case _                                         => None
    }

  // A type with the definition's parameters replaced by the argument types, syntax kept as it is.
  private[cardinality] def replace(tpe: Type, arguments: Map[TypeName, Type]): Type =
    TypeSubstitution.replace(tpe, arguments)

}
