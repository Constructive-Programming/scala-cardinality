package cardinality.capacity

import scala.annotation.tailrec
import scala.meta.*

import cardinality.types.{Trees, TypeName, TypeSubstitution}

// A recursion has a count in the model only through positive positions (docs/type-arithmetic.md §8
// covers covariant constructions only). Through a function's domain the numeric equation is no
// count: `case class P(f: P => Boolean)` iterated from 0 reads 0 by the empty-domain rule, yet `P(_
// => true)` is a closed value — and X ≅ 2^X has no solution at all (Cantor). So a cycle of a
// system's definitions that passes through a function's domain, a partial function's keys or a
// match type has no count, and neither has one through the argument of a constructor whose use of
// it the estimate does not read (a foreign one, a type parameter applied, a source definition of a
// standard name): every definition on it is blocked with that cause.
private[capacity] object Positivity {

  /** Why a position is not positive. */
  enum Cause {

    /** Inside a function's domain, a partial function's keys, a match type or a form the reading
      * does not follow.
      */
    case Domain

    /** An argument of a constructor whose slots the estimate does not read. */
    case Unread(constructor: String)
  }

  /** Which slots of a generic definition hold their arguments positively (`None`), or why not. */
  type Slots = List[Option[Cause]]

  /** A system's verdict: the definitions on a cycle through a non-positive position, with its
    * cause, and the slots of each generic definition.
    */
  final case class Verdict(blocked: Map[TypeName, Cause], slots: Map[TypeName, Slots])

  /** The definitions of a system on a cycle through a non-positive position. `params` are the type
    * parameters of each definition the system solves; `reaches` what each reaches through the
    * system's dependency graph; `outside` which slots of a constructor the system does not define
    * are positive, read where `owner` is written (None when no reading knows).
    */
  def of(
      shapes: Shapes,
      params: Map[TypeName, List[TypeName]],
      reaches: Map[TypeName, Set[TypeName]],
      outside: (TypeName, String, Int) => Option[Slots]
  ): Verdict = {
    val reader = Reader(shapes, params, outside)
    val positive = reader.slots
    val edges = params.keySet.toList.map(name => name -> reader.nonPositive(name, positive))
    def onCycle(from: TypeName, to: TypeName) =
      to == from || reaches.getOrElse(to, Set.empty)(from)
    val blocked = edges.flatMap { (from, into) =>
      into.collectFirst { case (to, cause) if onCycle(from, to) => cause }.toList.flatMap { cause =>
        (from :: reaches.getOrElse(from, Set.empty).filter(onCycle(from, _)).toList).map(_ -> cause)
      }
    }
    Verdict(blocked.groupMapReduce(_._1)(_._2)((a, b) => if (a == Cause.Domain) a else b), positive)
  }

  // One system read for positivity.
  final private case class Reader(
      shapes: Shapes,
      params: Map[TypeName, List[TypeName]],
      outside: (TypeName, String, Int) => Option[Slots]
  ) {

    // The types a definition's value is made of: an alias's body, a class's or an enum's stored
    // fields and evidence, and what a member of a value it stores makes a field.
    private def types(name: TypeName): List[Type] =
      shapes.aliases.get(name).flatten.toList ++ shapes.arms.getOrElse(name, Nil).flatten ++
        shapes.reads.getOrElse(name, Nil)

    /** The names a definition mentions in a non-positive position, each with its cause. */
    def nonPositive(
        name: TypeName,
        positive: Map[TypeName, List[Option[Cause]]]
    ): Map[TypeName, Cause] =
      types(name).flatMap(occurrences(name, _, positive, params(name).toSet)).toMap

    // Which slots of each generic definition are positive (None) or why not, as the greatest fixed
    // point: a slot is positive while its parameter occurs in no non-positive position of the
    // definition, and a family's slots are positive while every parameter of every subtype is. A
    // slot's first cause is kept, so the iteration only ever turns slots non-positive.
    def slots: Map[TypeName, List[Option[Cause]]] = {
      @tailrec def refine(
          current: Map[TypeName, List[Option[Cause]]]
      ): Map[TypeName, List[Option[Cause]]] = {
        val next = current.map { (name, causes) =>
          val negative = nonPositive(name, current)
          val child = shapes.parents.getOrElse(name, Nil).flatMap(current.get).flatten.flatten
          name -> params(name).zip(causes).map { (p, cause) =>
            cause.orElse(negative.get(p)).orElse(child.headOption)
          }
        }
        if (next == current) current else refine(next)
      }
      refine(params.filter(_._2.nonEmpty).map((n, ps) => n -> List.fill(ps.size)(None)))
    }

    // The names a type mentions in a non-positive position: inside a function's domain, a partial
    // function's keys, a match type or another form the reading does not follow, or an argument a
    // constructor does not hold as data. A definition's own type parameter applied (`F[X]`)
    // depends on the instantiation, which reads it again.
    private def occurrences(
        owner: TypeName,
        tpe: Type,
        positive: Map[TypeName, List[Option[Cause]]],
        binders: Set[TypeName]
    ): List[(TypeName, Cause)] = {
      def all(t: Type, cause: Cause) =
        TypeSubstitution.typeNames(t).map(n => TypeName.of(n) -> cause)
      def go(t: Type): List[(TypeName, Cause)] = t match {
        case _: (Type.Name | Lit | Type.Singleton) => Nil
        case Trees.Held(parts)                     => parts.flatMap(go)
        case f: Type.FunctionType                  =>
          f.paramClause.values.flatMap(all(_, Cause.Domain)) ++ go(f.res)
        case Type.Apply.After_4_6_0(callee, Type.ArgClause(args)) =>
          val slots: List[Option[Cause]] = callee match {
            case Type.Name(n) if positive.contains(TypeName.of(n)) => positive(TypeName.of(n))
            case Type.Name(n) if binders(TypeName.of(n))           => args.map(_ => None)
            case Type.Name(n) => outside(owner, n, args.size).getOrElse(Nil)
            case other        => args.map(_ => Some(unread(other)))
          }
          args.zipWithIndex.flatMap { (arg, i) =>
            slots.lift(i).getOrElse(Some(unread(callee))).fold(go(arg))(all(arg, _))
          }
        case other => all(other, Cause.Domain)
      }
      go(tpe)
    }

    private def unread(callee: Type): Cause = callee match {
      case Type.Name(n) => Cause.Unread(n)
      case other        => Cause.Unread(other.syntax)
    }

  }

}
