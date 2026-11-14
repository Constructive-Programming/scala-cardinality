scalaVersion := "3.8.4"

libraryDependencies += "org.typelevel" %% "cats-kernel" % "2.13.0"

cardinalityReportFile := baseDirectory.value / "report.txt"

// The checks read a file the reports rewrite, so none may be served from the task cache.
lazy val checkResolved = taskKey[Unit]("Check a row resolves through the compiled dependency")
checkResolved := Def.uncached {
  val text = IO.read(cardinalityReportFile.value)
  assert(text.contains("27  example.pick"), text)
}

lazy val checkUnsupplied = taskKey[Unit]("Check a row over a class the classpath lacks is refused")
checkUnsupplied := Def.uncached {
  val text = IO.read(cardinalityReportFile.value)
  assert(text.contains("?  example.pick"), text)
  assert(text.contains("a member the classpath does not hold: cats"), text)
}
