package cardinality.reporting

/** What `Report.of` reads its rows from (docs/plans/compiled-own-types.md, phase 4): loading a
  * program, resolving its names and building each target's environment and goal sit behind it.
  * `Report.query` still calls the scalameta pipeline's own steps.
  */
private[cardinality] trait FrontEnd {
  def rows(module: FrontEnd.Module): FrontEnd.Rows
}

private[cardinality] object FrontEnd {

  /** Each source's path and text, and what the build supplies beside them. */
  final case class Module(sources: List[(String, String)], environment: Report.Environment)

  /** One answer per source, in the order given: its rows, or why it could not be read. `warnings`
    * lists what of the environment could not be read.
    */
  final case class Rows(
      sources: List[Either[Report.Error, Report.Source]],
      warnings: List[String]
  )

}
