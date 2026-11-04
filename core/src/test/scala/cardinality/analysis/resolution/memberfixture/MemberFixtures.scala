package cardinality.analysis.resolution.memberfixture

// Declarations the type-member regressions pin against the compiler (`typeChecks`): Scala 3.9.0
// reads these as the counting rules assume.

trait R { type X }

type Fst[T] = T match { case (f, s) => f }
type Snd[T] = T match { case (f, s) => s }
type First[T] = T match { case (a, b) => a }
type K[T] = T match { case _ => Option[T] }

// An inherited alias beats a package-level one inside the template body (members M0).
type Shadowed = Unit

trait HasShadowed { type Shadowed = Boolean }

final class Reads extends HasShadowed { def g(): Shadowed = true }

// A final class may leave an abstract member undefined: nothing can then instantiate it.
final class Never extends R

// `case _` reduces on an abstract scrutinee, and two spellings of one projection are one type.
def irrefutable[A](x: A, y: A): K[A] = if (x == y) Some(x) else Some(y)

def canonical[T](x: First[T]): Fst[T] = x

// Tuple arguments reach the fields of a family through its parameters.
def concrete[A](x: Fst[(A, A)], y: Snd[(A, A)]): (A, A) = (x, y)

// A refinement of a final class, of an alias it already fixes, and a subclass of a value class or
// of a private constructor are rejected: no value or subclass gives their members another type.
final class FinalX { type X = Int }

trait OpenAlias { type X = Int }

class Value(val n: Int) extends AnyVal { type X }

class Private private () { type X }

// Inside a refinement `this` is the refined value; a `final abstract` class, a member with two
// bounds and a template below a private constructor or inside a private scope admit no instance
// or no outside subclass; a narrowed field is the receiver's; evidence rewrites a stuck type.
trait TY { type X; type Y }

final abstract class FinalAbstract { type X }

trait Bounded { type X >: Int <: String }

class Closed private ()

object Closed { def make: Closed = new Closed() }

trait BelowClosed extends Closed { type X }

object Hidden { private class P { type X }; def make: Any = new P }

trait U { type Y }

abstract class WithVal { val p: U; type X = p.Y }
