import java.io.FileOutputStream
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipOutputStream}

scalaVersion := "3.8.4"

// The report artifact lands next to the test project, where the script can mirror it.
cardinalityReportFile := baseDirectory.value / "report.txt"

// Stand-in for a published library: the sources jar a dependency ships. Built here so that the
// test needs neither network access nor a binary fixture checked in.
val librarySources = taskKey[File]("Package the fixture library's sources into a sources jar.")
librarySources := Def.uncached {
  val root = file("vendor/library-sources").toPath
  val out = baseDirectory.value / "library-sources.jar"
  Files.createDirectories(out.getParentFile.toPath)
  val zip = new ZipOutputStream(new FileOutputStream(out))
  val sources = Files.walk(root)
  try
    sources
      .filter(path => path.toString.endsWith(".scala"))
      .sorted()
      .forEach { path =>
        zip.putNextEntry(new ZipEntry(root.relativize(path).toString))
        zip.write(Files.readAllBytes(path))
        zip.closeEntry()
      }
  finally
    sources.close()
    zip.close()
  out
}

// The test project itself, which does not aggregate the library's: a report runs once.
lazy val root = project.in(file("."))

// Stand-in for the library's binary jar, compiled here from the same sources: the TASTy front end
// reads it beside the sources jar.
lazy val lib = project.settings(
  scalaVersion := "3.8.4",
  Compile / unmanagedSourceDirectories := Seq((ThisBuild / baseDirectory).value / "vendor" / "library-sources")
)

val libraryBinary = taskKey[File]("Package the fixture library's classes into a binary jar.")
libraryBinary := Def.uncached {
  val out = baseDirectory.value / "library.jar"
  IO.copyFile(fileConverter.value.toPath((lib / Compile / packageBin).value).toFile, out)
  out
}

// The rows the TASTy front end reads, as the sources jar places them. The sealed family `Optic` is
// not checked: its row is not read from compiled classes yet.
val checkTasty = taskKey[Unit]("Check the rows read from the library's binary jar.")
checkTasty := {
  // Runs of spaces as one: the table's padding depends on the row not checked.
  val text = IO.read(cardinalityReportFile.value).replaceAll(" +", " ")
  assert(text.contains("4 optics.Composed.<init> Composed[A](left: A, right: A) optics/Optic.scala:5"))
  assert(text.contains("? optics.Composed[A] class optics/Optic.scala:5 depends on its instantiation (A)"))
  assert(text.contains("1 optics.Id object optics/Optic.scala:4"))
}
