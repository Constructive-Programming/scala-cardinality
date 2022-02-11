ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "scala-cardinality",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "4.4.34",
      "org.typelevel" %% "cats-core" % "2.7.0"
    ),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "4.13.2",
      "org.specs2" %% "specs2-cats" % "4.13.2"
    ).map(_ % Test)
  )
