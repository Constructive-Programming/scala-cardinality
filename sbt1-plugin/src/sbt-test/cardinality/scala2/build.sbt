scalaVersion := "2.12.20"

cardinalityReportFile := baseDirectory.value / "report.txt"

lazy val check = taskKey[Unit]("The report counts the definition")
check := assert(IO.read(cardinalityReportFile.value).contains("6  P.<init>"))
