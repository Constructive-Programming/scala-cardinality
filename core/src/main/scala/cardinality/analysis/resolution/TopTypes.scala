package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.analysis.inhabitation.{Card, ClosedSpaces, Inhabitation, Natural}
import cardinality.analysis.model.*
import cardinality.types.{TypeApplications, TypeSubstitution, Universe}
import dev.constructive.eo.optics.Plated

import Inhabitation.Shape
import MethodAnalysis.TypeEntry
import TypeSubstitution.{platedType, Meet, Union}

/** `Any`, `AnyRef` (`Object`), `Matchable` and `AnyVal` on the method side, read by their place in
  * their own arrow (Rule P, docs/research/top-types-unions.md).
  *
  * The contract excludes every operation Scala offers on a value of a top type — universal equality
  * and hashing, `toString`, `getClass`, type tests and casts, `eq`, `synchronized` — so the code
  * receiving one observes nothing of it: in an arrow's input it is `1`, always inhabited, by `()`.
  * In an arrow's output it is ω: the code that builds it may return any value. Two places read it
  * differently on purpose: a `transparent inline` result, whose call site sees the expansion's type
  * (`TransparentResults`); and a constructor position, whose stored-value estimate keeps `Any` as
  * every value (ω), so the position is read through this model before the estimate is asked.
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

  /** A top type before its position is known: one value, which `polarized` keeps in an input (the
    * code receiving it observes nothing of it) and raises to ω in an output (v1.md §2). Its own
    * ground, so a key of a finite map or set tells it from `Unit`.
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
    * top in its input is one value and a top in its output ω; nothing flips through nesting. A top
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
    case other => values(other)(placed(_, positive, enclosing))
  }

  // A rigid application, evidence or singleton keeps its arguments, which are keys, not values;
  // any other shape has its children read by `f`.
  private def values(shape: Shape)(f: Shape => Shape): Shape = shape match {
    case _: (Shape.Applied | Shape.Evidence | Shape.Singleton) => shape
    case other => Shape.rebuild(other, Shape.children(other))(f)
  }

  private def tier(positive: Boolean): Card = if (positive) Card.Omega else Card.one

  /** Whether a shape holds a top type, in any place. */
  def holds(shape: Shape): Boolean = Shape.exists(shape) {
    case Shape.Ground(name, _) => top(name)
    case _                     => false
  }

  /** Whether a shape holds a top read in an output, at ω. */
  def unbounded(shape: Shape): Boolean =
    Shape.exists(shape) {
      case Shape.Ground(name, Card.Omega) => top(name)
      case _                              => false
    }

  /** The shape with every top read in an output as `Unit`, one value: what a body that observes
    * nothing of it may do with it.
    */
  def observed(shape: Shape): Shape = shape match {
    case Shape.Ground(name, Card.Omega) if top(name) => Shape.Product(Nil)
    case other                                       => values(other)(observed)
  }

  /** The top type a builtin spelling denotes. */
  def named(name: String): Option[Kind] = spellings.get(name.stripPrefix("scala."))

  /** The top type `written` denotes where it is read: a top spelling no binder, declaration, import
    * or inherited member gives another meaning.
    */
  def denoted(written: WrittenType): Option[Kind] =
    builtinNamed(written)(Builtin(named, terminal))

  /** Whether `written` is `Any` itself where it is read: the unit of `&` and the absorbing
    * alternative of `|`.
    */
  def isAny(written: WrittenType): Boolean = denoted(written).contains(Kind.Universal)

  /** Whether a top absorbs a union and is the unit of `&`: `Any`, and `AnyRef` (`Object`) and
    * `Matchable`, which the model reads exactly as `Any` (v1.md §2). `AnyVal` does not.
    */
  def absorbing(kind: Kind): Boolean = kind != Kind.Values

  /** Whether `written` is an absorbing top where it is read (`absorbing`). */
  def absorbs(written: WrittenType): Boolean = denoted(written).exists(absorbing)

  /** Whether `written` is the builtin `Null` where it is read. */
  def isNull(written: WrittenType): Boolean = denotes(NullType)(written)

  /** Whether `written` is the builtin `Nothing` where it is read. */
  def isNothing(written: WrittenType): Boolean = denotes(NothingType)(written)

  private def denotes(empty: Builtin[Unit])(written: WrittenType): Boolean =
    builtinNamed(written)(empty).nonEmpty

  // A builtin's spellings, and the shape a read that reaches it gives.
  final private case class Builtin[+A](spelled: String => Option[A], reading: Shape)

  // The empty builtins, each spelled one way.
  private def empty(spelling: String): Builtin[Unit] =
    Builtin(name => Option.when(name == spelling)(()), Shape.Sum(Nil))

  private val NullType = empty("Null")

  private val NothingType = empty("Nothing")

  private def builtinNamed[A](written: WrittenType)(builtin: Builtin[A]): Option[A] = {
    val WrittenType(tpe, context, resolver) = written
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
  }

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

  /** Whether a constructor position is the method side's alone to read: a top type or `Null` behind
    * a source alias, which the rewrite below does not reach, or a top type inside a constructor
    * whose arguments' places the rewrite does not know. A constructor is a method: the estimate is
    * never handed a top (v1.md §2).
    */
  def unestimable(written: WrittenType): Boolean = aliased(written) || merging(written).nonEmpty

  /** A constructor position's type in the implementation model, for the stored-value estimate: a
    * top type is the one value `()` — its reading in an input; a top in a function's output, ω, is
    * the method side's own (`ConstructorPositions`) — `Null` is empty and drops out of a union. A
    * union or an intersection left over is the shape model's to read — the estimate's sum is only a
    * bound — and so is a slot where `Nothing`, the name the rewrite writes the empty type with,
    * means a binder or a declaration of the sources.
    */
  def implementationSlot(written: WrittenType): Option[Type] = {
    val rewritten = new Slot(written).rewrite(written.tpe)
    val parts = Plated.universe(rewritten)
    // Rebuilding a tree copies a child that already has a parent, so the written `Nothing` is
    // recognised by its spelling, not by identity.
    Option.unless(
      parts.exists(connective) ||
        (parts.exists(spellsNothing) && !isNothing(written.at(Type.Name("Nothing"))))
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

  // Whether a top type or `Null` sits behind a source alias the type names: the rewrite reads
  // spellings, so the estimate would still see the alias's `Any` as everything.
  private def aliased(written: WrittenType): Boolean =
    namedParts(written.tpe).exists(part =>
      behind(written.at(part), at => denoted(at).nonEmpty || isNull(at))
    )

  private def namedParts(tpe: Type): List[Type] = Universe.select(tpe) {
    case named @ (_: Type.Name | _: Type.Select) => Some(named)
    case _                                       => None
  }

  // Whether `sought` holds of `named`'s alias body, read in the alias's own scope.
  private def behind(named: WrittenType, sought: WrittenType => Boolean): Boolean = {
    val WrittenType(tpe, scope, resolver) = named
    val name = TypeApplications.name(tpe)
    Option
      .unless(scope.variables.contains(name))(resolver.declaration(tpe, name, scope))
      .collect {
        case Found.Entry(entry @ TypeEntry(_, alias: Defn.Type, _, _)) => entry -> alias
      }
      .exists((entry, alias) =>
        scope
          .enterAlias(entry, resolver.typeParameters(entry.owner), AliasReading.Slot)
          .exists(inAlias => reaches(WrittenType(alias.body, inAlias, resolver), sought))
      )
  }

  // Whether `sought` holds of a part of the type, or of a source alias it names.
  private def reaches(written: WrittenType, sought: WrittenType => Boolean): Boolean =
    Universe.exists(written.tpe)(part => sought(written.at(part))) ||
      namedParts(written.tpe).exists(part => behind(written.at(part), sought))

  // The library constructors whose arguments take the constructor's own place, at the argument
  // indexes they do: a sequence's elements, an option's value, a set's elements, a map's keys and
  // values (v1.md §2: a set's size in an input is not known, so `Set[Any]` there is `Set[Unit]`'s
  // 2). Any other constructor — a source class, a function type spelled as a class — may hold its
  // argument in an output, where a top is ω, not `()`.
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
    "Set" -> Set(0),
    "Map" -> Set(0, 1)
  )

  // The first constructor around a top type that does not pass its place on, as spelled.
  private def merging(written: WrittenType): Option[String] =
    written.tpe match {
      case a: Type.Apply                        => applied(written.at(a.tpe), a.argClause.values)
      case i: Type.ApplyInfix if !connective(i) => applied(written.at(i.op), List(i.lhs, i.rhs))
      case other                                =>
        TypeSubstitution.localPlate
          .childrenVec(other)
          .toList
          .iterator
          .flatMap(part => merging(written.at(part)))
          .nextOption()
    }

  private def applied(head: WrittenType, args: List[Type]): Option[String] = {
    val WrittenType(callee, context, resolver) = head
    val name = TypeApplications.name(callee)
    val kept = Option
      .when(
        !context.variables.contains(name) &&
          resolver.declaration(callee, name, context) == Found.Absent
      )(name.stripPrefix("scala."))
      .flatMap(positional.get)
      .getOrElse(Set.empty)
    args.zipWithIndex.iterator
      .flatMap { (arg, index) =>
        if (kept(index)) merging(head.at(arg))
        else Option.when(Universe.exists(arg)(part => denoted(head.at(part)).nonEmpty))(name)
      }
      .nextOption()
  }

  final private class Slot(written: WrittenType) {

    // The empty type, as the estimate reads it: one tree per rewrite, so a union can tell the
    // operand it wrote from a `Nothing` the source spells.
    private val empty: Type = Type.Name("Nothing")

    // Tops become `()`, `Null` the empty type, and a union drops its empty alternatives.
    val rewrite: Type => Type = TypeSubstitution.localCata[Type] { (node, kids, rebuild) =>
      node match {
        case top if denoted(written.at(top)).nonEmpty => Lit.Unit()
        case read if isNull(written.at(read))         => empty
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
