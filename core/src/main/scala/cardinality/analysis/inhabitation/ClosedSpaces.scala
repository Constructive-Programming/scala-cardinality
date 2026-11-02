package cardinality.analysis.inhabitation

import cardinality.analysis.resolution.LiteralTypes
import cardinality.capacity.{EffectiveOmega, FinitePart, NothingSize, Size, UnitSize}

import Inhabitation.Shape
import Shape.*

/** Closed shapes hold no atom (a rigid application is one), evidence, existential or singleton
  * other than a literal type's. A closed value only yields closed values, each a literal the empty
  * environment builds, so eliminating one is a uniform split: one branch without the value, raised
  * to its value space. A literal type is one point, like `Unit` (its constant is a literal,
  * `1 => B` carries what `Unit => B` does), and a union of literals the sum of its constants.
  * Products multiply, sums add, a function space is its codomain raised to its domain, a sequence
  * is ω (1 if its element is empty); an infinite operand saturates at 0, 1 or ω (register D2).
  */
private[cardinality] object ClosedSpaces {

  def closed(shape: Shape): Boolean =
    !Shape.exists(shape) {
      case Singleton(id, None) if LiteralTypes.literal(id)       => false
      case _: Singleton | _: Evidence | _: Existential | _: Poly => true
      case other                                                 => atomic(other)
    }

  /** The method-side value space of a closed shape. Callers test `closed` first: an open shape has
    * no value space, only a choice of supplied values.
    */
  def space(shape: Shape): Card = exact.get(shape)

  /** The size algebra's reading of a closed shape — the tier the stored-value estimate gives the
    * same type when it can read it: `ω^ω` is `ε₀` there, while the solver's finite programs
    * saturate it at ω. Used only where the estimate could not read the position itself.
    */
  def estimate(shape: Shape): Size = tiers.get(shape)

  /** Atoms beside ground values: neither a value space nor a choice of supplied values. */
  def mixed(shape: Shape): Boolean =
    !closed(shape) && Shape.exists(shape)(_.isInstanceOf[Ground]) &&
      Shape.exists(shape)(atomic)

  /** No atom, but a singleton, evidence or existential beside a node with a choice of values: a
    * value space this algebra does not read, whose functions differ from its values.
    */
  def valued(shape: Shape): Boolean =
    !closed(shape) && !Shape.exists(shape)(atomic) && Shape.exists(shape) {
      case _: Ground | _: Sum | _: Function | _: Repeated => true
      case _                                              => false
    }

  private def atomic(shape: Shape): Boolean = shape match {
    case _: Atom | _: Applied | _: Constructor => true
    case _                                     => false
  }

  // The two readings share one structure — products multiply, sums add, a function space is its
  // codomain raised to its domain, a sequence is ω unless its element is empty — and differ only in
  // carrier and in how a ground leaf enters it; the constructor reading compares them, so they must
  // not drift apart.
  private trait Algebra[C] {
    def zero: C
    def one: C
    def omega: C
    def ground(size: Card): C
    def plus(a: C, b: C): C
    def times(a: C, b: C): C
    def power(base: C, exponent: C): C
    def empty(c: C): Boolean
  }

  private def reading[C](algebra: Algebra[C]) = Shape.cata[C] { (node, kids) =>
    import algebra.*
    def product(factors: Iterable[C]) = factors.foldLeft(one)(times)
    node match {
      case Ground(_, size)                                 => ground(size)
      case Singleton(id, None) if LiteralTypes.literal(id) => one
      case Product(_)                                      => product(kids)
      case Sum(_)                                          => kids.foldLeft(zero)(plus)
      case Function(_, _)                                  => power(kids.last, product(kids.init))
      case Repeated(_)                                     => if (empty(kids.head)) one else omega
      case open                                            => unclosed(open)
    }
  }

  private val exact = reading(new Algebra[Card] {
    def zero = Card.zero
    def one = Card.one
    def omega = Card.Omega
    def ground(size: Card) = size
    def plus(a: Card, b: Card) = a + b
    def times(a: Card, b: Card) = a * b
    def power(base: Card, exponent: Card) = base ^ exponent
    def empty(c: Card) = c.isZero
  })

  private val tiers = reading(new Algebra[Size] {
    def zero = NothingSize
    def one = UnitSize
    def omega = EffectiveOmega
    // A ground size is a word-sized literal or ω.
    def ground(size: Card) = size match {
      case Card.Exact(Natural.Lit(value)) => Size(0, 0, FinitePart.exact(value))
      case _                              => EffectiveOmega
    }
    def plus(a: Size, b: Size) = a + b
    def times(a: Size, b: Size) = a * b
    def power(base: Size, exponent: Size) = base.pow(exponent)
    def empty(c: Size) = c.isZero
  })

  // Invariant guard: `closed` rules these nodes out before either reading is asked for.
  private def unclosed(node: Shape): Nothing =
    throw new IllegalArgumentException(s"a closed space was asked of an open shape: $node")

}
