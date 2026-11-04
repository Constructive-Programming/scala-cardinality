package cardinality.analysis

import scala.meta.XtensionCollectionLikeUI

import kyo.*

/** The rows of a set of target sources, with an environment beside them (dependency stubs) that is
  * read but never measured, across workers.
  *
  * Each worker builds an index of its own over every input and measures a share of the target
  * files: an index's caches, cycle guards and name counters are never shared, and the rows do not
  * depend on which targets an index measured before (the runner checks input order cannot move a
  * result). The rows come back in the order one index gives: file by file in path order, each
  * file's rows in its index's order.
  *
  * With an environment, each target gets `quota` logical steps (`AnalysisWork`); a target that
  * exhausts them refuses with the budget named. Without one the measurement is unbudgeted, as
  * `MethodAnalysis.analyze` is.
  */
private[cardinality] object Measuring {

  /** Ten times the most any eo v1.0.0 target spends with the sources alone (29,366 steps). */
  val TargetWork = 300000L

  def rows(
      targets: List[MethodAnalysis.Input],
      environment: List[MethodAnalysis.Input] = Nil,
      workers: Int = defaultWorkers,
      quota: Long = TargetWork,
      only: String => Boolean = _ => true
  ): List[MethodAnalysis.Entry] = {
    val groups = shares(targets.filter(t => only(t.path)), workers)
    val measured =
      if (groups.size <= 1) groups.map(measure(targets, environment, _, quota))
      else {
        import AllowUnsafe.embrace.danger
        val all = Async.foreach(groups, groups.size)(group =>
          Sync.defer(measure(targets, environment, group, quota))
        )
        KyoApp.Unsafe.runAndBlock(Duration.Infinity)(all).getOrThrow.toList
      }
    val byFile = measured.flatten.groupBy(_.path)
    byFile.keys.toList.sorted.flatMap(byFile)
  }

  /** Workers by default: four, whatever the machine. A budget-exhausted row names the step it
    * stopped at, which depends on what its index measured before; a fixed partition keeps every row
    * the same from run to run and machine to machine (fewer processors are only slower).
    */
  val defaultWorkers: Int = 4

  private def measure(
      targets: List[MethodAnalysis.Input],
      environment: List[MethodAnalysis.Input],
      files: Set[String],
      quota: Long
  ): List[MethodAnalysis.Entry] =
    if (environment.isEmpty) {
      val index = new MethodAnalysis.Index(targets, MethodAnalysis.Limits())
      index.inFiles(files).map(index.measure)
    } else {
      val limits = MethodAnalysis.Limits()
      val work = new AnalysisWork(Long.MaxValue, limits.maxTypeDepth, counting = false)
      val index = new MethodAnalysis.Index(targets ++ environment, limits, Some(work))
      index.selected(files, Set.empty).map { target =>
        work.target(quota)
        index.measure(target)
      }
    }

  // The target files in shares of about equal weight: heaviest first, each to the lightest share.
  private def shares(targets: List[MethodAnalysis.Input], workers: Int): List[Set[String]] = {
    val weighed = targets.map(t => t.path -> weight(t)).sortBy((path, w) => (-w, path))
    val loads = Array.fill(math.max(1, math.min(workers, targets.size)))(0L)
    val assigned = Array.fill(loads.length)(Set.empty[String])
    weighed.foreach { (path, w) =>
      val lightest = loads.indices.minBy(i => (loads(i), i))
      loads(lightest) += w
      assigned(lightest) += path
    }
    assigned.toList.filter(_.nonEmpty)
  }

  private def weight(input: MethodAnalysis.Input): Long =
    input.tree.collect { case _: (scala.meta.Defn | scala.meta.Decl) => 1L }.sum + 1

}
