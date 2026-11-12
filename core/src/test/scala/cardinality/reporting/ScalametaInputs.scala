package cardinality.reporting

import cardinality.analysis.MethodAnalysis

/** What the scalameta front end reads a module as, for a check that asks its parts directly: the
  * sources parsed with the compiler's types written in, and the dependency stubs.
  */
private[cardinality] object ScalametaInputs {

  def of(module: FrontEnd.Module): (List[MethodAnalysis.Input], List[MethodAnalysis.Input]) = {
    val parsed = module.sources.map(ScalametaFrontEnd.parse).collect { case Right(input) => input }
    val (inputs, _) = ScalametaFrontEnd.ascribed(parsed, module.environment.classes)
    (inputs, ScalametaFrontEnd.dependencies(module.environment, module.sources.map(_._2)).inputs)
  }

}
