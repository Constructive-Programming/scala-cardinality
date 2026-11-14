package cardinality.reporting

/** What `Report.of` hands the front end (`TastyFrontEnd`), and what it answers. */
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
