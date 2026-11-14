package cardinality.compiled

import scala.collection.mutable

import tastyquery.Symbols.Symbol

/** Least fixed points read on demand over trees (acyclic) and members that may name each other,
  * where an answer joins those it reads (a union, a disjunction): a member asked while it is being
  * read answers `bottom` there. An answer that depended on that (`cut`) is kept only while the
  * outermost read lasts, where every answer is read once and the outermost one is exact; it is
  * remembered after only where the outermost answer is `bottom` (so is every answer it read).
  * `settled` answers, and answers that depended on no member being read, are always remembered.
  */
final private[compiled] class Fixed[A](bottom: A, settled: A => Boolean) {
  private val known = new java.util.IdentityHashMap[AnyRef, A]
  private val pending = new java.util.IdentityHashMap[AnyRef, (A, Set[Symbol])]
  private val reading = mutable.Set.empty[Symbol]
  private var cut = Set.empty[Symbol]
  private var depth = 0

  def apply(key: AnyRef)(read: => A): A = {
    val symbol = Option(key).collect { case m: Symbol => m }
    if (known.containsKey(key)) known.get(key)
    else if (pending.containsKey(key)) {
      val (found, after) = pending.get(key)
      cut ++= after
      found
    } else if (symbol.exists(!reading.add(_))) {
      cut ++= symbol
      bottom
    } else first(key, symbol)(read)
  }

  // A key read for the first time, `symbol` now being read.
  private def first(key: AnyRef, symbol: Option[Symbol])(read: => A): A = {
    val outer = cut
    cut = Set.empty
    depth += 1
    try {
      val found = read
      cut --= symbol
      if (settled(found) || cut.isEmpty) known.put(key, found): Unit
      else pending.put(key, (found, cut)): Unit
      if (depth == 1 && found == bottom) pending.forEach((k, v) => known.put(k, v._1): Unit)
      found
    } finally {
      depth -= 1
      reading --= symbol
      cut = outer ++ cut -- symbol
      if (depth == 0) {
        pending.clear()
        cut = Set.empty
      }
    }
  }

}
