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
