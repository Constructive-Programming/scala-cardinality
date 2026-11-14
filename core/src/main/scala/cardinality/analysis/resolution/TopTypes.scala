package cardinality.analysis.resolution

import cardinality.analysis.inhabitation.{Card, ClosedSpaces, Inhabitation}

import Inhabitation.Shape

/** `Any`, `AnyRef` (`Object`), `Matchable` and `AnyVal` on the method side, read by their place in
  * their own arrow (Rule P, docs/research/top-types-unions.md).
  *
  * The contract excludes every operation Scala offers on a value of a top type — universal equality
  * and hashing, `toString`, `getClass`, type tests and casts, `eq`, `synchronized` — so the code
  * receiving one observes nothing of it: in an arrow's input it is `1`, always inhabited, by `()`.
  * In an arrow's output it is ω: the code that builds it may return any value. A constructor
  * position reads it differently on purpose: its stored-value estimate keeps `Any` as every value
  * (ω), so the position is read through this model before the estimate is asked.
  */
private[cardinality] object TopTypes {

  // The top types' classes, by their fully qualified names (`AnyRef` is `java.lang.Object`).
  private val classes = Set("scala.Any", "java.lang.Object", "scala.Matchable", "scala.AnyVal")

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

  /** Whether a fully qualified name is a top type's. */
  def named(name: String): Boolean = classes(name)

  /** A goal with exactly one value has exactly one implementation, whatever the environment holds:
    * an empty environment admits the unique absurd answer, any other only that value.
    */
  def terminalGoal(shape: Shape): Boolean = shape match {
    case Shape.Singleton(_, _)                             => true
    case Shape.Product(fields)                             => fields.forall(terminalGoal)
    case Shape.Function(_, result) if terminalGoal(result) => true
    case other                                             =>
      ClosedSpaces.count(other).exists(_.isOne)
  }

}
