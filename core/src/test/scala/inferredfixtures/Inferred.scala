package inferredfixtures

// Members written without a result type, each in its own object so nothing else is in reach.
// CompiledClassesSpec reads their compiled types back from this file's TASTy.

object Flags {
  val flag = !true
}

object Pairs {
  def pair = (true, false)
}

object Lists {
  val markers = List("a").map(s => s + "b")
}

object Literal {
  final val k = 3
}

object Transparent {
  transparent inline def t = true
}

object Locals {

  def outer: Boolean = {
    def pick = (true, false)
    pick._1
  }

}

trait Flag { def f: Boolean }

object Anonymous {
  val flag: Flag = new Flag { def f = !true }
}

object Patterns {
  val (left, right) = (true, (false, true))
}

object Lambdas {
  val negate: Boolean => Boolean = x => !x
  val both = List((true, false)).map((a, b) => a && b)
  val applied = List((b: Boolean) => !b).map(f => f(true))
}

object FunctionPatterns {
  val (negate, unit) = ((b: Boolean) => !b, ())
}
