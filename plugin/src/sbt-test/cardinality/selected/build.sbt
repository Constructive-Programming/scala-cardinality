scalaVersion := "3.8.4"

cardinalityQuerySupport := Seq(baseDirectory.value / "support")
cardinalityReportFile := baseDirectory.value / "report.txt"

// One row, example.get's: neither the other target nor the support is a row.
def checkScope(text: String): Unit = {
  assert(text.contains("1 signature: 1 finite"))
  assert(text.contains("example.get"))
  assert(!text.contains("example.other"))
  assert(!text.contains("example.supportOnly"))
  assert(!text.contains("example.Box.<init>"))
}

lazy val checkSelected = taskKey[Unit]("Check selected report scope and provenance")
checkSelected := {
  val text = IO.read(cardinalityReportFile.value)
  checkScope(text)
  assert(text.contains("persistent cache disabled"))
  assert(text.contains("work:"))
}

// The TASTy front end filters the module's report: no request key, no work accounting.
lazy val checkSelectedTasty = taskKey[Unit]("Check the selected report's scope under TASTy")
checkSelectedTasty := checkScope(IO.read(cardinalityReportFile.value))

lazy val checkBudget = taskKey[Unit]("Check deterministic fuel exhaustion")
checkBudget := {
  val text = IO.read(cardinalityReportFile.value)
  assert(text.contains("budget exhausted"))
  assert(text.contains("1 unresolved"))
}
