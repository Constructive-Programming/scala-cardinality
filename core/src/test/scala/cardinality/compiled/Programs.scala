package cardinality.compiled

import scala.jdk.CollectionConverters.*

import cardinality.Compiled
import cardinality.tasty.CompiledClasses
import java.nio.file.{FileSystems, Files, Path}
import tastyquery.Contexts.*
import tastyquery.Symbols.ClassSymbol

/** Small programs for the specs of the compiled front end's parts: a source compiled by scalac in
  * this JVM, and its classes loaded with the dependencies a spec chooses to read.
  */
private[cardinality] object Programs {

  /** The Scala library's jars, which a build always passes. */
  val libraries: List[Path] = sys.props
    .getOrElse("cardinality.test.classpath", sys.props("java.class.path"))
    .split(java.io.File.pathSeparator)
    .toList
    .map(Path.of(_))
    .filter { entry =>
      val name = entry.getFileName.toString
      name.startsWith("scala-library") || name.startsWith("scala3-library")
    }

  /** One source compiled against `classpath` and the Scala library: its class directory. */
  def compiled(code: String, classpath: List[Path] = Nil): Path = {
    val root = Compiled.temporary("program")
    val file = Files.writeString(root.resolve("Example.scala"), code)
    val classes = Files.createDirectories(root.resolve("classes"))
    val reporter = new dotty.tools.dotc.reporting.StoreReporter()
    val arguments = List(
      "-d",
      classes.toString,
      "-classpath",
      (classpath ++ libraries).mkString(java.io.File.pathSeparator),
      file.toString
    )
    if (dotty.tools.dotc.Main.process(arguments.toArray, reporter).hasErrors)
      sys.error(s"fixture does not compile: ${reporter.allErrors.map(_.message).mkString("; ")}")
    classes
  }

  /** A module's classes loaded: its top-level classes, why any did not load, and what
    * `CompiledClasses.dependencies` is asked with.
    */
  final class Loaded(
      val context: Context,
      val own: List[ClassSymbol],
      val unloaded: List[String],
      val classes: CompiledClasses
  ) {

    /** The classes of the dependency classpath, and why any did not load. */
    def dependencies: (List[ClassSymbol], List[String]) =
      CompiledClasses.dependencies(classes)(using context)

  }

  /** `code` compiled against `classpath`, and loaded with `read` (by default the same) and the
    * Scala library as its dependencies.
    */
  def loaded(code: String, classpath: List[Path] = Nil, read: Option[List[Path]] = None): Loaded = {
    val classes =
      CompiledClasses(List(compiled(code, classpath)), read.getOrElse(classpath) ++ libraries)
    val (context, own, unloaded) = CompiledClasses.loaded(classes)
    new Loaded(context, own.distinct, unloaded, classes)
  }

  /** The jar or class directory a class was loaded from. */
  def jarOf(c: Class[?]): Path =
    Path.of(c.getProtectionDomain.getCodeSource.getLocation.toURI)

  /** The classes of one package of the test classes, as a class directory of its own: a dependency
    * that holds nothing else.
    */
  def packaged(name: String): Path = {
    val all = jarOf(classOf[tastyfixtures.Endo[?]])
    val out = Compiled.temporary("dependency")
    val zipped = Option.unless(Files.isDirectory(all))(FileSystems.newFileSystem(all))
    try {
      val from = zipped.fold(all)(_.getPath("/")).resolve(name)
      val files = Files.walk(from)
      try
        files.iterator.asScala.filter(Files.isRegularFile(_)).foreach { file =>
          val to = out.resolve(name).resolve(from.relativize(file).toString)
          Files.createDirectories(to.getParent)
          Files.copy(file, to)
        }
      finally files.close()
    } finally zipped.foreach(_.close())
    out
  }

}
