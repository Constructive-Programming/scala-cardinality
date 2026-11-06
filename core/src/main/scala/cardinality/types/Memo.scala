package cardinality.types

import scala.collection.mutable

/** A function's results by argument, each computed on its first use. */
final private[cardinality] class Memo[K, V] {
  private val results = mutable.Map.empty[K, V]

  def apply(key: K)(compute: => V): V = results.getOrElseUpdate(key, compute)
}
