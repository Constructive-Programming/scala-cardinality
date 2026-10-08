scalaVersion := "3.8.4"

cardinalityQuerySupport := Seq(baseDirectory.value / "support")
cardinalityReportFile := baseDirectory.value / "report.txt"

lazy val checkSelected = taskKey[Unit]("Check selected report scope and provenance")
checkSelected := {
  val text = IO.read(cardinalityReportFile.value)
  assert(text.contains("1 signature: 1 finite"))
  assert(text.contains("example.get"))
  assert(!text.contains("example.other"))
  assert(!text.contains("example.supportOnly"))
  assert(!text.contains("example.Box.<init>"))
  assert(text.contains("persistent cache disabled"))
  assert(text.contains("work:"))
}

lazy val checkBudget = taskKey[Unit]("Check deterministic fuel exhaustion")
checkBudget := {
  val text = IO.read(cardinalityReportFile.value)
  assert(text.contains("budget exhausted"))
  assert(text.contains("1 unresolved"))
}
