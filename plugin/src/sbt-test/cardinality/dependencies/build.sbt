scalaVersion := "3.8.4"

libraryDependencies += "org.typelevel" %% "cats-kernel" % "2.13.0"

cardinalityReportFile := baseDirectory.value / "report.txt"

// The checks read a file the reports rewrite, so none may be served from the task cache.
lazy val checkResolved = taskKey[Unit]("Check a row resolves through the compiled dependency")
checkResolved := Def.uncached {
  val text = IO.read(cardinalityReportFile.value)
  assert(text.contains("27  example.pick"), text)
  assert(!text.contains("tasty/"), text)
}

lazy val checkUnsupplied = taskKey[Unit]("Check the sources-only report leaves the name unsupplied")
checkUnsupplied := Def.uncached {
  val text = IO.read(cardinalityReportFile.value)
  assert(text.contains("declaration not supplied: cats.kernel.Comparison"), text)
}
