scalaVersion := "3.3.6"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"

// The report artifact lands next to the test project, where the script can mirror it.
cardinalityReportFile := baseDirectory.value / "report.txt"
