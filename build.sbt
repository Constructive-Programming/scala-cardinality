name := "scala-cardinality"
version := "0.1.0-SNAPSHOT"
scalaVersion := "3.9.0"

libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % "4.17.4",
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.specs2" %% "specs2-core" % "4.23.0" % Test,
  "org.specs2" %% "specs2-cats" % "4.23.0" % Test
)
