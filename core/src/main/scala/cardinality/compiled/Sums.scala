package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation.Shape
import cardinality.types.Traversals
import tastyquery.Contexts.{defn, Context}
import tastyquery.Modifiers.{Variance, Visibility}
import tastyquery.Symbols.*
import tastyquery.Trees.{ClassDef, DefDef, TypeTree, ValDef}
import tastyquery.Types.*

/** Enums and sealed families as the method side reads them (v1.md §2): the sum of their cases, each
  * the product of its fields. An object or a value case is one value; a class case, a case class or
  * a class nothing extends is its one parameter list, each field public; a sealed intermediate is
  * its own cases.
  *
  * A sum with one missing branch is a wrong sum, so a case the model does not read refuses the
  * whole: an anonymous instance, a given object, an inner or local subtype, an open subtype, an
  * enum class under a sealed family, a class something extends, a class with several parameter
  * clauses, a parameter that is no member.
  *
  * What the reading does not cover yet stays `Uncovered`: a construction that always throws, binds
  * a pattern or inherits an abstract member of the empty type (slice S6); an enum's value case
  * under a sealed family; a restricted field or constructor (rule B); a case class something
  * extends; a plain class with no parameter (`class L` and `class L()` are one tree); an enum case
  * with several parameter clauses; a subtype reached twice; an abstract member of the empty type
  * that takes parameters; a sum whose cases the classpath does not list; a subtype without a tree;
  * and a subtype that instantiates its parent otherwise than `Sums.carried` reads.
  */
final private[compiled] class Sums(world: World, shapes: Shapes)(using Context) {
  import Shapes.Env
  import Unread.{Refused, Uncovered}

  // The cases of a sum: each subtype that is one, with the shapes of its fields.
  private type Cases = Either[Unread, List[(Symbol, List[Shape])]]

  /** The sum an enum or a sealed class or trait is, its type parameters standing for `arguments`.
    */
  def of(tree: ClassDef, arguments: List[Shape], env: Env): Either[Unread, Shape] = {
    val level = new Level(tree, arguments, env)
    (if (tree.symbol.isEnum) level.enumCases else level.familyCases).flatMap { cases =>
      val subtypes = cases.map(_._1)
      // A subtype under two sealed levels is one case, and would be summed once per level.
      subtypes.diff(subtypes.distinct).headOption match {
        case Some(twice) =>
          Left(Uncovered(s"a sealed subtype reached twice: ${twice.displayFullName}"))
        case None => Right(Shape.Sum(cases.map((_, fields) => Shape.Product(fields))))
      }
    }
  }

  // One level of a sum: the enum or sealed template, what its type parameters stand for, and where
  // it is read from.
  final private class Level(tree: ClassDef, arguments: List[Shape], env: Env) {
    private val symbol = tree.symbol

    def enumCases: Cases = {
      val cases = symbol.sealedChildren
      // A value case is built with the companion that holds it, and one with its own `extends`
      // clause is an instance of an anonymous class its definition declares.
      val constructed = tree :: cases.flatMap {
        case cls: ClassSymbol  => cls.tree.toList
        case value: TermSymbol =>
          value.owner.tree.toList.collect { case holder: ClassDef => holder } ++
            value.tree.toList.flatMap(World.classes)
      }
      for {
        // An enum has a case: none means the classpath lacks the annotation that lists them.
        _ <- Either.cond(cases.nonEmpty, (), unlisted)
        _ <- Traversals.sequence(constructed.distinct.map(construction))
        read <- Traversals.sequence(cases.map(c => caseFields(c).map(c -> _)))
      } yield read
    }

    // A sealed family's cases: those its file declares, each read whole or the family refused.
    def familyCases: Cases =
      construction(tree).flatMap(_ => uninhabited(tree)).flatMap { isUninhabited =>
        val children = symbol.sealedChildren
        if (isUninhabited) Right(Nil)
        // An anonymous instance registers the family as its own child.
        else if (children.contains(symbol))
          Left(Refused(s"sealed family has anonymous instances in its file: ${symbol.name}"))
        // A family without subtypes has no value; one something extends has them listed.
        else if (children.isEmpty && world.extended(symbol)) Left(unlisted)
        else Traversals.sequence(children.map(subtype)).map(_.flatten)
      }

    private def unlisted: Unread =
      Uncovered(s"a sum whose cases are not listed: ${symbol.displayFullName}")

    // The cases one subtype adds to the family's sum.
    private def subtype(child: ClassSymbol | TermSymbol): Cases = child match {
      // A given object; one with parameters is a class, which the class's own checks stop.
      case value: TermSymbol if value.isGivenOrUsing =>
        Left(Refused(s"sealed family has a given instance outside the fragment: ${symbol.name}"))
      // An enum's value case under a family of its own is the enum's construction to read first.
      case value: TermSymbol if value.isEnumCase =>
        Left(Uncovered(s"an enum case under a sealed family: ${value.displayFullName}"))
      // An object and its class have one owner.
      case _ if !World.inObjects(child) =>
        Left(Refused(s"sealed subtype is an inner or local class: ${child.name}"))
      case value: TermSymbol =>
        value.moduleClass
          .fold(Right(()))(Shapes.tree(_).flatMap(construction))
          .flatMap(_ => caseFields(value))
          .map(read => List(value -> read))
      case cls: ClassSymbol =>
        Shapes.tree(cls).flatMap { below =>
          // An enum has no reading under a sealed family, and an open trait or abstract class has
          // subtypes elsewhere.
          if (cls.isEnum) Left(Refused(s"sealed subtype outside the closed fragment: ${cls.name}"))
          else if (Shapes.isFamily(cls))
            instantiated(cls).flatMap(new Level(below, _, env).familyCases)
          else if (cls.isAbstractClass)
            Left(Refused(s"sealed family has an open abstract subtype: ${cls.name}"))
          else construction(below).flatMap(_ => caseFields(cls)).map(read => List(cls -> read))
        }
    }

    // The fields of one case: an object or a value case has none, a class its one parameter list.
    private def caseFields(child: ClassSymbol | TermSymbol): Either[Unread, List[Shape]] =
      child match {
        case value: TermSymbol =>
          // It has no type parameter, so what it stands for is no field either.
          instantiated(value)
        case cls: ClassSymbol =>
          Shapes.tree(cls).flatMap { below =>
            val constructor = below.rhs.constr
            val name = cls.displayFullName
            val accessors = below.rhs.body.collect {
              case v: ValDef if v.symbol.isParamAccessor => v.symbol.visibility
            }
            val extended = world.extended(cls)
            if (extended && !cls.isCaseClass)
              Left(Refused(s"sealed subtype outside the closed fragment: ${cls.name}"))
            else if (extended) Left(Uncovered(s"a sealed case class that is extended: $name"))
            // An enum's case is read by the enum's rule, which has no such refusal.
            else if (constructor.paramLists.count(_.isLeft) != 1 && cls.isEnum)
              Left(Uncovered(s"an enum case with several parameter clauses: $name"))
            else if (constructor.paramLists.count(_.isLeft) != 1)
              Left(Refused(s"sealed subtype shape unreadable: ${cls.name}"))
            // `class L` and `class L()` are one tree, and only the second is an empty product.
            else if (!cls.isCaseClass && Shapes.parameters(constructor).isEmpty)
              Left(Uncovered(s"a sealed subtype without parameters: $name"))
            else if (!constructor.symbol.isPublic)
              Left(Uncovered(s"a restricted constructor: $name"))
            // The compiler gives a plain class's parameter without `val` an accessor private to
            // the instance. It is no member: values that differ only there may be one value.
            else if (accessors.contains(Visibility.PrivateThis))
              Left(Refused(s"sealed subtype hides a parameter: ${cls.name}"))
            else if (accessors.exists(_ != Visibility.Public))
              Left(Uncovered(s"a restricted field: $name"))
            else instantiated(cls).flatMap(shapes.fields(below, _, env))
          }
      }

    // What a subtype's own type parameters stand for, where this level is applied to
    // `arguments`.
    private def instantiated(child: ClassSymbol | TermSymbol): Either[Unread, List[Shape]] =
      Sums
        .carried(child, symbol, arguments)
        .toRight(
          Uncovered(
            "a sealed subtype that does not pass its own parameters to its parent: " +
              child.displayFullName
          )
        )

  }

  // The abstract members of the empty type a template declares, each by its parameters.
  private def absurdMembers(template: ClassDef): List[List[ValDef]] = template.rhs.body.collect {
    case v: ValDef if v.symbol.isAbstractMember && nothing(v.tpt)       => Nil
    case d: DefDef if d.symbol.isAbstractMember && nothing(d.resultTpt) => Shapes.parameters(d)
  }

  private def nothing(tpt: TypeTree): Boolean = tpt.toType.isSameType(defn.NothingType)

  // Whether a level declares an abstract member of the empty type: every case inherits it and no
  // pure, total body implements `Nothing`, so the level has no value. One that takes parameters is
  // implemented where a parameter's type is empty, which is not read here.
  private def uninhabited(level: ClassDef): Either[Unread, Boolean] = {
    val members = absurdMembers(level)
    Either.cond(
      members.forall(_.isEmpty),
      members.nonEmpty,
      Uncovered(
        s"an abstract member of the empty type with parameters: ${level.symbol.displayFullName}"
      )
    )
  }

  // Construction (register D26) is restated for classes with slice S6 (decision Q5). Until then a
  // template is read only where building an instance evaluates nothing that always throws and no
  // pattern definition, in itself and in every template of the module it extends, and where none
  // of those it extends declares an abstract member of the empty type (its own is its level's to
  // read).
  private def construction(tree: ClassDef): Either[Unread, Unit] = {
    val templates = tree.symbol.linearization.filter(world.ofModule).flatMap(_.tree)
    Either.cond(
      !templates.exists(t => Bodies.destructures(t) || Bodies.alwaysThrows(t)) &&
        templates.drop(1).forall(absurdMembers(_).isEmpty),
      (),
      Uncovered(
        "a construction that throws, binds a pattern or inherits a member of the empty type: " +
          tree.symbol.displayFullName
      )
    )
  }

}

private[compiled] object Sums {

  /** How a subtype stands for its parent applied to `arguments`, by its clause: what each of its
    * own type parameters stands for, the argument of the parent's slot that carries it.
    * `C[Y] extends P[a]` holds the values of `C[t]` in `P[t]`, for every `t`, where each of the
    * subtype's parameters is unbounded and passed to exactly one slot, of its own variance, and
    * every other slot is pinned bottom-up (`Nothing` into a covariant one, `Any` into a
    * contravariant one). A slot pinned to another type, a parameter no slot or two slots carry, a
    * bound and another variance each need more than this reads: none.
    */
  def carried[A](child: ClassSymbol | TermSymbol, parent: ClassSymbol, arguments: List[A])(using
      Context
  ): Option[List[A]] = {
    // The subtype's clause and its own type parameters: a class's parents; for an object or a
    // value case, which has none, the object's parents, those of the anonymous class a value case
    // with its own `extends` clause is an instance of, or the value case's declared type.
    val (parents, parameters) = child match {
      case cls: ClassSymbol  => cls.parents -> cls.typeParams
      case value: TermSymbol =>
        value.moduleClass
          .orElse(value.tree.toList.flatMap(World.classes).map(_.symbol).headOption)
          .fold(List[TypeOrMethodic](value.declaredType))(_.parents) -> Nil
    }
    val passed = parents.collectFirst {
      case tpe: Type if World.classNamed(tpe).contains(parent) =>
        tpe.dealias match {
          case applied: AppliedType => applied.args
          case _                    => Nil
        }
    }
    val filled = passed.toList.flatten.zip(parent.typeParams).zipWithIndex
    // Each parameter with the slots that carry it.
    val bound = parameters.map { parameter =>
      parameter -> filled.collect {
        case ((argument, slot), index) if carries(argument, parameter) => slot -> index
      }
    }
    val fits = bound.forall {
      case (parameter, List((slot, _))) =>
        slot.declaredVariance == parameter.declaredVariance &&
        Shapes.unbounded(parameter.declaredBounds)
      case _ => false
    }
    val settled = filled.forall {
      case ((argument, slot), _) =>
        pinned(argument, slot) || parameters.exists(carries(argument, _))
    }
    Option.when(passed.exists(_.sizeIs == parent.typeParams.size) && settled && fits)(
      bound.map((_, carriers) => arguments(carriers.head._2))
    )
  }

  // A slot every instantiation admits: `Nothing` in a covariant one, `Any` in a contravariant one.
  private def pinned(argument: TypeOrWildcard, slot: ClassTypeParamSymbol)(using
      Context
  ): Boolean =
    argument match {
      case tpe: Type =>
        (slot.declaredVariance == Variance.Covariant && tpe.isSameType(defn.NothingType)) ||
        (slot.declaredVariance == Variance.Contravariant && tpe.isSameType(defn.AnyType))
      case _ => false
    }

  // Whether a clause's argument is the subtype's own type parameter.
  private def carries(argument: TypeOrWildcard, parameter: ClassTypeParamSymbol)(using
      Context
  ): Boolean =
    argument match {
      case ref: TypeRef => ref.optSymbol.contains(parameter)
      case _            => false
    }

}
