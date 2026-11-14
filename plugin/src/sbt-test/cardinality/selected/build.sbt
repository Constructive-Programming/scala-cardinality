scalaVersion := "3.8.4"

Compile / unmanagedSourceDirectories += baseDirectory.value / "support"
cardinalityReportFile := baseDirectory.value / "report.txt"

// One row, example.get's: neither the other target nor the support is a row.
lazy val checkSelected = taskKey[Unit]("Check the selected report's scope")
checkSelected := {
  val text = IO.read(cardinalityReportFile.value)
  assert(text.contains("1 signature: 1 finite"))
  assert(text.contains("example.get"))
  assert(!text.contains("example.other"))
  assert(!text.contains("example.supportOnly"))
  assert(!text.contains("example.Box.<init>"))
}
