package cardinality.tasty

import scala.meta.Parsed
import scala.util.control.NonFatal

import cardinality.analysis.MethodAnalysis
import java.net.URI
import java.nio.file.{FileSystems, Files, Path}
import tastyquery.Classpaths.ClasspathEntry
import tastyquery.Contexts.*
import tastyquery.Exceptions.UnknownClasspathEntry
import tastyquery.SourceLanguage
import tastyquery.Symbols.*
import tastyquery.jdk.ClasspathLoaders

/** Dependency declarations read from compiled TASTy (tasty-query), handed to the analyzer as
  * signature stubs: one parsed source per original source file of each rendered jar, under
  * `tasty/<jar>/<source path>`. The stubs are environment, never review targets.
  *
  * Scala 3 (TASTy) and Java (`.class`) classes outside the stdlib model's namespaces are rendered;
  * Scala 2 classes stay unsupplied, so a reference to them keeps its `declaration not supplied`
  * reason. Every concrete body is the unresolved `Spell.Body`: the stubs supply identities and
  * signatures, not implementations.
  */
object TastyStubs {

  /** The prefix every stub input's path carries. */
  val Prefix = "tasty/"

  final case class Result(inputs: List[MethodAnalysis.Input], errors: List[String])

  /** The JDK's own classes, which the compiled signatures refer to (`java.lang.Object`, …). */
  private def jdk: Path =
    FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules", "java.base")

  /** Renders the jars `classpath` lists that `rendered` selects (their classes in the `packages`
    * selected); the whole classpath (the selected jars' own dependencies and the Scala library)
    * resolves their signatures.
    */
  def of(
      classpath: List[Path],
      rendered: Path => Boolean,
      packages: String => Boolean = _ => true
  ): Result = parse(render(classpath, rendered, packages))

  /** Stub sources as text, by path: what `parse` reads and `StubFile` stores. */
  final case class Rendered(stubs: List[(String, String)], errors: List[String] = Nil)

  /** Renders the jars `selected` picks as `of` does, without parsing. */
  def render(classpath: List[Path], selected: Path => Boolean): Rendered =
    render(classpath, selected, _ => true)

  private def render(
      classpath: List[Path],
      selected: Path => Boolean,
      packages: String => Boolean
  ): Rendered = {
    // One read per jar: a jar with no classes has no entry, so entries cannot be zipped back.
    val perJar = classpath.map(jar => jar -> ClasspathLoaders.read(List(readable(jar))))
    given Context = Context.initialize(perJar.flatMap(_._2) ++ ClasspathLoaders.read(List(jdk)))
    val chosen = perJar.filter((jar, _) => selected(jar)).flatMap((jar, es) => es.map(jar -> _))
    val declarations = StubDeclarations(javaClassfiles(chosen.map(_._2)))
    val stubs = chosen.flatMap { (jar, entry) =>
      // An entry with no packages (a resources-only jar) has no symbols to look up.
      val symbols =
        try ctx.findSymbolsByClasspathEntry(entry).toList
        catch { case _: UnknownClasspathEntry => Nil }
      val classes = symbols.collect {
        case c: ClassSymbol
            if c.sourceLanguage != SourceLanguage.Scala2 &&
              (Spell.packageObject(c) || !Spell.objectName(c).contains("$")) &&
              !stdlibOwned(packageOf(c)) && packages(packageOf(c)) =>
          c
      }
      classes
        .groupBy(c => (packageOf(c), sourceOf(c)))
        .toList
        .sortBy(_._1)
        .map {
          case ((pkg, source), cs) =>
            val body = cs.sortBy(_.name.toString).flatMap { c =>
              try declarations.topLevel(c)
              catch { case NonFatal(e) => List(s"// not rendered: ${c.name}: ${e.getMessage}") }
            }
            val header = if (pkg.isEmpty) "" else s"package $pkg\n\n"
            (s"$Prefix${jar.getFileName}/$source", header + body.mkString("\n\n") + "\n")
        }
    }
    Rendered(stubs)
  }

  /** A Java class's `.class` file facts, read from the rendered entries (a Java class's nested
    * classes are files of the same package, `Outer$Inner`).
    */
  private def javaClassfiles(entries: List[ClasspathEntry]): ClassSymbol => JavaClassfile = {
    val packages = entries
      .flatMap(e =>
        try e.listAllPackages()
        catch { case NonFatal(_) => Nil }
      )
      .groupBy(_.dotSeparatedName)
    def binaryName(c: ClassSymbol): String = c.owner match {
      case o: ClassSymbol => binaryName(o) + "$" + Spell.objectName(c)
      case _              => Spell.objectName(c)
    }
    def pkg(s: Symbol): String = s match {
      case p: PackageSymbol => p.fullName.path.mkString(".")
      case other            => pkg(other.owner)
    }
    c =>
      packages
        .getOrElse(pkg(c), Nil)
        .iterator
        .flatMap(_.getClassDataByBinaryName(binaryName(c)))
        .find(_.hasClassFile)
        .fold(JavaClassfile(Nil, sealedClass = true))(d =>
          JavaClassfile.read(IArray.genericWrapArray(d.readClassFileBytes()).toArray)
        )
  }

  /** The rendered stubs parsed: a stub that does not parse is left out, with an error naming it. */
  def parse(rendered: Rendered): Result = {
    val parsed = rendered.stubs.map((path, text) => (path, text, Spell.source(text)))
    Result(
      parsed.collect { case (path, _, Parsed.Success(tree)) => MethodAnalysis.Input(path, tree) },
      parsed.collect {
        case (path, text, e: Parsed.Error) =>
          val line = text.linesIterator.drop(e.pos.startLine).nextOption().getOrElse("")
          s"$path:${e.pos.startLine + 1}: ${e.message}: $line"
      } ++ rendered.errors
    )
  }

  /** tasty-query reads a directory or a `.jar`; sbt 2 hands out content-addressed jars with no
    * suffix, which are read through a `.jar`-named link.
    */
  private def readable(entry: Path): Path =
    if (Files.isDirectory(entry) || entry.getFileName.toString.endsWith(".jar")) entry
    else {
      val dir = Files.createTempDirectory("tasty-stubs")
      dir.toFile.deleteOnExit()
      val link = dir.resolve(entry.getFileName.toString + ".jar")
      try Files.createSymbolicLink(link, entry.toAbsolutePath)
      catch { case NonFatal(_) => Files.copy(entry, link) }
      link.toFile.deleteOnExit()
      link
    }

  /** Namespaces the stdlib model answers: a stub there (`scala.collection.compat`, the Scala
    * library's own TASTy) would make the namespace partly supplied and doubt what the model reads.
    */
  private def stdlibOwned(pkg: String): Boolean =
    List("scala", "java", "javax").exists(root => pkg == root || pkg.startsWith(root + "."))

  private def packageOf(c: ClassSymbol): String =
    c.owner match {
      case p: PackageSymbol if !p.isRootPackage =>
        p.fullName.path.map(n => Spell.term(n.toString)).mkString(".")
      case _ => ""
    }

  /** The source file the class was compiled from, relative, or a path made from its name. */
  private def sourceOf(c: ClassSymbol): String = {
    val recorded =
      try c.tree.map(_.pos.sourceFile.path).getOrElse("")
      catch { case NonFatal(_) => "" }
    if (recorded.nonEmpty) recorded.stripPrefix("/")
    else
      packageOf(c).replace('.', '/') + "/" + Spell.objectName(c).stripSuffix("$package") + ".scala"
  }

}
