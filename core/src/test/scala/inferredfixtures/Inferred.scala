package inferredfixtures

// Members written without a result type, each in its own object so nothing else is in reach.
// CompiledTypesSpec reads their compiled types back from this file's TASTy.

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
