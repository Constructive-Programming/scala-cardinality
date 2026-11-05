package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.inhabitation.{Card, ClosedSpaces, Inhabitation, Natural}
import cardinality.analysis.model.*
import cardinality.analysis.{MethodAnalysis, Resolver}
import cardinality.types.{TypeApplications, TypeSubstitution, Universe}
import dev.constructive.eo.optics.Plated

import Inhabitation.Shape
import MethodAnalysis.TypeEntry
import TypeSubstitution.{platedType, Meet, Union}

/** `Any`, `AnyRef` (`Object`), `Matchable` and `AnyVal` on the method side: the terminal type.
  *
  * The contract excludes every operation Scala offers on a value of a top type — universal equality
  * and hashing, `toString`, `getClass`, type tests and casts, `eq`, `synchronized` — so no context
  * tells two such values apart: the type is `∃X. X`, which parametricity makes `1`
  * (docs/research/top-types-unions.md). It is always inhabited, by `()`. Four places read it
  * differently on purpose: a `transparent inline` result, whose call site sees the expansion's type
  * (`TransparentResults`); an `inline` method whose body may match on an argument's static type
  * (`InlineBodies`); a constructor position, whose stored-value estimate keeps `Any` as everything
  * (ε₀), so the position is read through this model before the estimate is asked; and a goal with
  * exactly one value, which has one implementation in every environment.
  */
private[cardinality] object TopTypes {

  /** The top types, by the class their spellings denote. */
  enum Kind(val label: String) {
    case Universal extends Kind("Any")
    case References extends Kind("AnyRef")
    case Matchables extends Kind("Matchable")
    case Values extends Kind("AnyVal")
  }

  // The spellings a builtin lookup reaches, by the type they denote.
  private val spellings: Map[String, Kind] = Map(
    "Any" -> Kind.Universal,
    "AnyRef" -> Kind.References,
    "Object" -> Kind.References,
    "java.lang.Object" -> Kind.References,
    "Matchable" -> Kind.Matchables,
    "AnyVal" -> Kind.Values
  )

  /** The bare names an import or an export could put another meaning behind. */
  val names: Set[String] = spellings.keySet.filterNot(_.contains('.'))

  /** A top type before its position is known: one value, which `polarized` keeps in a negative
    * position (a parametric body observes nothing of it) and raises to ε₀ in a positive one (v1.md
    * §2). Its own ground, so a key of a finite map or set tells it from `Unit`.
    */
  val terminal: Shape = Shape.Ground("Any", Card.one)

  // A top among the keys or values of a finite map or set, read as the function the container
  // tabulates: that function is no arrow of the program, so its contents take the container's own
  // place, as a sequence's elements do.
  private val Keyed = "Any (key)"

  /** Whether a ground is a top type's, as written or as a key. */
  def top(name: String): Boolean = name == "Any" || name == Keyed

  /** A key or value shape of a finite map or set, its tops marked as contents (see `Keyed`). A
    * function inside it is an arrow of its own: its tops are read by their place in it.
    */
  def keyed(key: Shape): Shape = key match {
    case Shape.Ground("Any", size) => Shape.Ground(Keyed, size)
    case _: Shape.Function         => key
    case other                     => Shape.rebuild(other, Shape.children(other))(keyed)
  }

  /** A shape with its tops read by position (v1.md §2): each arrow on its own, innermost first, a
    * top in its input is one value and a top in its output ε₀; nothing flips through nesting. A top
    * under no arrow takes `positive`, the place the shape itself has (a method's parameter or a
    * constructor slot is an input, its result an output); a product, sum, sequence, existential or
    * polymorphic body passes its place on; a rigid application's arguments are keys, not values.
    */
  def polarized(shape: Shape, positive: Boolean): Shape = placed(shape, positive, positive)

  // `enclosing` is the place of the nearest arrow around the shape, which a key takes.
  private def placed(shape: Shape, positive: Boolean, enclosing: Boolean): Shape = shape match {
    case Shape.Ground("Any", _)             => Shape.Ground("Any", tier(positive))
    case Shape.Ground(Keyed, _)             => Shape.Ground(Keyed, tier(enclosing))
    case Shape.Function(parameters, result) =>
      Shape.Function(
        parameters.map(placed(_, false, positive)),
        placed(result, true, positive)
      )
    case _: (Shape.Applied | Shape.Evidence | Shape.Singleton) => shape
    case other => Shape.rebuild(other, Shape.children(other))(placed(_, positive, enclosing))
  }

  private def tier(positive: Boolean): Card = if (positive) Card.Epsilon0 else Card.one

  /** Whether a shape holds a top read at ε₀. */
  def unbounded(shape: Shape): Boolean =
    Shape.exists(shape) {
      case Shape.Ground(name, Card.Epsilon0) => top(name)
      case _                                 => false
    }

  /** The shape with every ε₀ top read as `Unit`, one value: what a body that observes nothing of it
    * may do with it.
    */
  def observed(shape: Shape): Shape = shape match {
    case Shape.Ground(name, Card.Epsilon0) if top(name)        => Shape.Product(Nil)
    case _: (Shape.Applied | Shape.Evidence | Shape.Singleton) => shape
    case other => Shape.rebuild(other, Shape.children(other))(observed)
  }

  /** The top type a builtin spelling denotes. */
  def named(name: String): Option[Kind] = spellings.get(name.stripPrefix("scala."))

  /** The top type `tpe` denotes where it is read: a top spelling no binder, declaration, import or
    * inherited member gives another meaning.
    */
  def denoted(tpe: Type, context: ResolutionContext, resolver: Resolver): Option[Kind] =
    builtinNamed(tpe, context, resolver)(Builtin(named, terminal))

  /** Whether `tpe` is `Any` itself where it is read: the unit of `&` and the absorbing alternative
    * of `|`.
    */
  def isAny(tpe: Type, context: ResolutionContext, resolver: Resolver): Boolean =
    denoted(tpe, context, resolver).contains(Kind.Universal)

  /** Whether `tpe` is the builtin `Null` where it is read. */
  def isNull(tpe: Type, context: ResolutionContext, resolver: Resolver): Boolean =
    denotes(NullType)(tpe, context, resolver)

  /** Whether `tpe` is the builtin `Nothing` where it is read. */
  def isNothing(tpe: Type, context: ResolutionContext, resolver: Resolver): Boolean =
    denotes(NothingType)(tpe, context, resolver)

  private def denotes(empty: Builtin[Unit])(
      tpe: Type,
      context: ResolutionContext,
      resolver: Resolver
  ): Boolean = builtinNamed(tpe, context, resolver)(empty).nonEmpty

  // A builtin's spellings, and the shape a read that reaches it gives.
  final private case class Builtin[+A](spelled: String => Option[A], reading: Shape)

  // The empty builtins, each spelled one way.
  private def empty(spelling: String): Builtin[Unit] =
    Builtin(name => Option.when(name == spelling)(()), Shape.Sum(Nil))

  private val NullType = empty("Null")

  private val NothingType = empty("Nothing")

  private def builtinNamed[A](tpe: Type, context: ResolutionContext, resolver: Resolver)(
      builtin: Builtin[A]
  ): Option[A] =
    spelling(tpe)
      .filterNot(context.variables.contains)
      .flatMap(name =>
        builtin
          .spelled(name.stripPrefix("scala."))
          .filter(_ =>
            resolver.declaration(tpe, name, context) == Found.Absent &&
              resolver.resolve(tpe, context) == Right(builtin.reading)
          )
      )

  private def spelling(tpe: Type): Option[String] =
    Option.when(tpe.is[Type.Name] || tpe.is[Type.Select])(TypeApplications.name(tpe))

  /** A goal with exactly one value has exactly one implementation, whatever the environment holds:
    * an empty environment admits the unique absurd answer, any other only that value.
    */
  def terminalGoal(shape: Shape): Boolean = shape match {
    case Shape.Singleton(_, _)                             => true
    case Shape.Product(fields)                             => fields.forall(terminalGoal)
    case Shape.Function(_, result) if terminalGoal(result) => true
    case other                                             =>
      ClosedSpaces.closed(other) && Natural.attempt(ClosedSpaces.space(other)).exists(_.isOne)
  }

  /** Why a constructor position is the method side's alone to read, if it is: a top type or `Null`
    * behind a source alias, which the rewrite below does not reach, or a top type inside a
    * constructor that may merge the values the rewrite identifies.
    */
  def unestimable(tpe: Type, context: ResolutionContext, resolver: Resolver): Option[String] =
    if (aliased(tpe, context, resolver)) Some("a top type or Null through an alias")
    else
      merging(tpe, context, resolver).map(head =>
        s"a top type inside $head, which may tell its elements apart"
      )

  /** A constructor position's type in the implementation model, for the stored-value estimate: a
    * top type is the one value `()`, `Null` is empty and drops out of a union. A union or an
    * intersection left over is the shape model's to read — the estimate's sum is only a bound — and
    * so is a slot where `Nothing`, the name the rewrite writes the empty type with, means a binder
    * or a declaration of the sources.
    */
  def implementationSlot(
      tpe: Type,
      context: ResolutionContext,
      resolver: Resolver
  ): Option[Type] = {
    val rewritten = new Slot(context, resolver).rewrite(tpe)
    val written = Plated.universe(rewritten)
    // Rebuilding a tree copies a child that already has a parent, so the written `Nothing` is
    // recognised by its spelling, not by identity.
    Option.unless(
      written.exists(connective) ||
        (written.exists(spellsNothing) && !isNothing(Type.Name("Nothing"), context, resolver))
    )(rewritten)
  }

  private def connective(tpe: Type): Boolean = tpe match {
    case Union(_, _) | Meet(_, _) => true
    case _                        => false
  }

  private def spellsNothing(tpe: Type): Boolean = tpe match {
    case Type.Name("Nothing") => true
    case _                    => false
  }

  // Whether a top type or `Null` sits behind a source alias `tpe` names: the rewrite reads
  // spellings, so the estimate would still see the alias's `Any` as everything.
  private def aliased(tpe: Type, scope: ResolutionContext, resolver: Resolver): Boolean =
    namedParts(tpe).exists(
      behind(_, scope, resolver, (t, at) => top(resolver)(t, at) || isNull(t, at, resolver))
    )

  private def top(resolver: Resolver)(tpe: Type, context: ResolutionContext): Boolean =
    denoted(tpe, context, resolver).nonEmpty

  private def namedParts(tpe: Type): List[Type] = Universe.select(tpe) {
    case named @ (_: Type.Name | _: Type.Select) => Some(named)
    case _                                       => None
  }

  // Whether `sought` holds of `named`'s alias body, read in the alias's own scope.
  private def behind(
      named: Type,
      scope: ResolutionContext,
      resolver: Resolver,
      sought: (Type, ResolutionContext) => Boolean
  ): Boolean = {
    val name = TypeApplications.name(named)
    Option
      .unless(scope.variables.contains(name))(resolver.declaration(named, name, scope))
      .collect {
        case Found.Entry(entry @ TypeEntry(_, alias: Defn.Type, _, _)) => entry -> alias
      }
      .exists((entry, alias) =>
        scope
          .enterAlias(entry, resolver.typeParameters(entry.owner), AliasReading.Slot)
          .exists(reaches(alias.body, _, resolver, sought))
      )
  }

  // Whether `sought` holds of a part of `tpe`, or of a source alias it names.
  private def reaches(
      tpe: Type,
      scope: ResolutionContext,
      resolver: Resolver,
      sought: (Type, ResolutionContext) => Boolean
  ): Boolean =
    Universe.exists(tpe)(sought(_, scope)) ||
      namedParts(tpe).exists(behind(_, scope, resolver, sought))

  // The library constructors that hold their arguments by position alone, at the argument indexes
  // they do: a sequence's elements, an option's value, a map's values. Reading a top as `()` there
  // identifies values none of their operations keeps apart. A `Set` or a `Map`'s keys merge what
  // equality calls equal, so `Set(1, "a")` and `Set(1)` differ in size though no operation tests a
  // top value; any constructor not listed may do the same.
  private val positional: Map[String, Set[Int]] = Map(
    "Option" -> Set(0),
    "Either" -> Set(0, 1),
    "List" -> Set(0),
    "Vector" -> Set(0),
    "Seq" -> Set(0),
    "IndexedSeq" -> Set(0),
    "Array" -> Set(0),
    "LazyList" -> Set(0),
    "Stream" -> Set(0),
    "PartialFunction" -> Set(0, 1),
    "Map" -> Set(1)
  )

  // The first constructor around a top type that may merge its arguments, as spelled.
  private def merging(tpe: Type, context: ResolutionContext, resolver: Resolver): Option[String] =
    tpe match {
      case a: Type.Apply => applied(a.tpe, a.argClause.values, context, resolver)
      case i: Type.ApplyInfix if !connective(i) =>
        applied(i.op, List(i.lhs, i.rhs), context, resolver)
      case other =>
        TypeSubstitution.localPlate
          .childrenVec(other)
          .toList
          .iterator
          .flatMap(merging(_, context, resolver))
          .nextOption()
    }

  private def applied(
      head: Type,
      args: List[Type],
      context: ResolutionContext,
      resolver: Resolver
  ): Option[String] = {
    val name = TypeApplications.name(head)
    val kept = Option
      .when(
        !context.variables.contains(name) &&
          resolver.declaration(head, name, context) == Found.Absent
      )(name.stripPrefix("scala."))
      .flatMap(positional.get)
      .getOrElse(Set.empty)
    args.zipWithIndex.iterator
      .flatMap { (arg, index) =>
        if (kept(index)) merging(arg, context, resolver)
        else Option.when(Universe.exists(arg)(denoted(_, context, resolver).nonEmpty))(name)
      }
      .nextOption()
  }

  final private class Slot(context: ResolutionContext, resolver: Resolver) {

    // The empty type, as the estimate reads it: one tree per rewrite, so a union can tell the
    // operand it wrote from a `Nothing` the source spells.
    private val empty: Type = Type.Name("Nothing")

    // Tops become `()`, `Null` the empty type, and a union drops its empty alternatives.
    val rewrite: Type => Type = TypeSubstitution.localCata[Type] { (node, kids, rebuild) =>
      node match {
        case top if denoted(top, context, resolver).nonEmpty => Lit.Unit()
        case read if isNull(read, context, resolver)         => empty
        // The operands are the first and last children in either spelling; a rewritten operand is
        // still the tree returned for it, not yet a copy under a new parent.
        case Union(_, _) if kids.head eq empty => kids.last
        case Union(_, _) if kids.last eq empty => kids.head
        case _                                 => kept(node, kids, rebuild)
      }
    }

    // A type with nothing rewritten is handed over as written, positions included.
    private def kept(node: Type, kids: List[Type], rebuild: List[Type] => Type): Type =
      if (kids.corresponds(TypeSubstitution.localPlate.childrenVec(node).toList)(_ eq _)) node
      else rebuild(kids)

  }

}
