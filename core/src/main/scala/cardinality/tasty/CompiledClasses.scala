package cardinality.tasty

import scala.reflect.NameTransformer
import scala.util.control.NonFatal

import java.net.URI
import java.nio.file.{FileSystems, Files, Path}
import java.util.concurrent.ConcurrentHashMap
import tastyquery.Classpaths.{ClassData, ClasspathEntry, PackageData}
import tastyquery.Contexts.*
import tastyquery.Names.{termName, ObjectClassTypeName, PackageFullName}
import tastyquery.Symbols.*
import tastyquery.jdk.ClasspathLoaders

/** The module's compiled output, and the classpath its types are read against. */
final case class CompiledClasses(output: Seq[Path], classpath: Seq[Path] = Nil)

/** The module's compiled classes and its dependencies' (docs/plans/compiled-own-types.md), read
  * with tasty-query: Scala 3 (TASTy), Scala 2 (pickle) and Java (`.class`) classes, the running
  * JDK's among them. The TASTy front end reads its symbols and trees from the context `loaded`
  * makes.
  */
object CompiledClasses {

  // The running JDK's classes, read and indexed once: each read starts from them, and a class a
  // later entry declares again is not seen (`firstLoaded`).
  private lazy val jdk: (List[ClasspathEntry], Set[(String, String)]) = {
    val entries =
      firstLoaded(jdkModules.flatMap(module => ClasspathLoaders.read(List(module))))
    val names = entries.flatMap(e =>
      e.listAllPackages()
        .flatMap(p => p.listAllClassDatas().map(p.dotSeparatedName -> _.binaryName))
    )
    (entries, names.toSet)
  }

  /** The module's own top-level classes, in a context over the JDK, its output and its classpath,
    * with why each class that does not load fails.
    */
  private[cardinality] def loaded(
      classes: CompiledClasses
  ): (Context, List[ClassSymbol], List[String]) = {
    val (jdkEntries, jdkNames) = jdk
    val output = classes.output.toList.map(readable)
    val own = output.flatMap(path => ClasspathLoaders.read(List(path)))
    val rest = classes.classpath.toList.distinct.map(readable)
    val later =
      firstLoaded(
        own ++ rest.flatMap(path => ClasspathLoaders.read(List(path))),
        jdkNames
      )
    val entries = jdkEntries ++ later
    given context: Context = Context.initialize(entries)
    val (found, failed) = classesOf(later.take(own.size))
    (context, found, failed)
  }

  /** The top-level classes of the module's classpath, in the context `loaded` made for it, with why
    * each class that does not load fails: its dependencies' declarations, which with its own are
    * the whole program. A class the JDK declares too is the JDK's, and is not listed. Each class is
    * read as it is listed, so this is asked only where a reading needs every one of them.
    */
  private[cardinality] def dependencies(
      classes: CompiledClasses
  )(using Context): (List[ClassSymbol], List[String]) =
    classesOf(firstLoaded(classpath(classes), jdk._2))

  /** What a Java class's class file says and its symbol does not, for the classes of the JDK and of
    * the module's classpath: the names of its enum constants, in order, and whether it is sealed. A
    * class file that is not found or read is sealed with no constants.
    */
  private[cardinality] def javaFacts(
      classes: CompiledClasses
  ): ClassSymbol => (List[String], Boolean) = {
    val read = javaClassfiles(jdk._1 ++ classpath(classes))
    c => {
      val file = read(c)
      (file.enumConstants, file.sealedClass)
    }
  }

  /** A class no symbol stands for, from its class file: its binary name (`a/b/Outer$1`), the binary
    * names of the classes it extends, and the names of the methods an instance declares.
    */
  final case class ClassFile(binary: String, parents: List[String], methods: List[String])

  /** The local and anonymous classes of the classes without TASTy (`Lone$1`, `Foo$$anon$1`) in
    * `paths`, the module's output or its classpath. A class with TASTy holds its own in its tree.
    */
  private[cardinality] def unnamed(paths: Seq[Path]): List[ClassFile] =
    for {
      entry <- paths.toList.distinct
        .map(readable)
        .flatMap(path => ClasspathLoaders.read(List(path)))
      pkg <- entry.listAllPackages()
      all = pkg.listAllClassDatas()
      withTasty = all.filter(_.hasTastyFile).map(_.binaryName).toSet
      data <- all
      name = data.binaryName
      if data.hasClassFile && !withTasty.exists(top => name.startsWith(top + "$")) &&
        name.split('$').drop(1).exists(part => part.headOption.exists(_.isDigit) || part == "anon")
    } yield {
      val file = JavaClassfile.read(IArray.genericWrapArray(data.readClassFileBytes()).toArray)
      val prefix =
        if (pkg.dotSeparatedName.isEmpty) "" else pkg.dotSeparatedName.replace('.', '/') + "/"
      ClassFile(prefix + name, file.parents, file.methods)
    }

  private def classpath(classes: CompiledClasses): List[ClasspathEntry] =
    classes.classpath.toList.distinct
      .map(readable)
      .flatMap(path => ClasspathLoaders.read(List(path)))

  // The top-level classes some entries list, and why each class that does not load fails.
  private def classesOf(
      entries: List[ClasspathEntry]
  )(using Context): (List[ClassSymbol], List[String]) = {
    val read = entries.map(entry => symbolsOf(entry))
    (read.flatMap(_._1).collect { case c: ClassSymbol => c }, read.flatMap(_._2))
  }

  /** The JDK's own classes, which the compiled signatures refer to (`java.lang.Object`, …): every
    * module of the running JDK, `java.base` first.
    */
  private def jdkModules: List[Path] = {
    val modules = FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules")
    val all = Files.list(modules).toArray.toList.collect { case p: Path => p }
    all.sortBy(p => (p.getFileName.toString != Base, p.getFileName.toString))
  }

  private val Base = "java.base"

  /** The classpath as the JVM loads it: each class file (a package and binary name) is read from
    * the first entry that has one, the JDK's before any jar's (parent-first delegation), and a
    * later copy is not seen. A shaded copy is left out too: its TASTy still declares the original
    * package (a jar's `shaded/fastparse/` declaring `fastparse`), a name the JVM never loads from
    * it. tasty-query refuses a class declared twice, so either one would otherwise lose a whole
    * jar.
    */
  private def firstLoaded(
      entries: List[ClasspathEntry],
      loaded: Set[(String, String)] = Set.empty
  ): List[ClasspathEntry] = {
    val seen = scala.collection.mutable.Set.empty[(String, String)] ++= loaded
    entries.map { entry =>
      val packages = entry.listAllPackages().flatMap { p =>
        val kept = p
          .listAllClassDatas()
          .filter(c =>
            !relocated(p.dotSeparatedName, c) && seen.add(p.dotSeparatedName -> c.binaryName)
          )
        Option.when(kept.nonEmpty)(new PackageData {
          val dotSeparatedName: String = p.dotSeparatedName
          private lazy val byName = kept.map(c => c.binaryName -> c).toMap
          def listAllClassDatas(): List[ClassData] = kept
          def getClassDataByBinaryName(binaryName: String): Option[ClassData] =
            byName.get(binaryName)
          override def toString: String = p.toString
        })
      }
      new ClasspathEntry {
        def listAllPackages(): List[PackageData] = packages
        override def toString: String = entry.toString
      }
    }
  }

  /** An entry's top-level symbols, and why each class that does not load fails. */
  private def symbolsOf(entry: ClasspathEntry)(using Context): (List[Symbol], List[String]) = {
    val found = entry.listAllPackages().flatMap { p =>
      val pkg = ctx.findPackageFromRoot(
        if (p.dotSeparatedName.isEmpty) PackageFullName.emptyPackageName
        else PackageFullName(p.dotSeparatedName.split('.').toList.map(termName(_)))
      )
      p.listAllClassDatas().map { c =>
        val name = termName(NameTransformer.decode(c.binaryName))
        try
          Right(List(name, name.toTypeName, name.withObjectSuffix.toTypeName).flatMap(pkg.getDecl))
        catch { case NonFatal(e) => Left(s"${c.binaryName}: ${e.getMessage}") }
      }
    }
    (found.collect { case Right(s) => s }.flatten, found.collect { case Left(e) => e })
  }

  private def relocated(pkg: String, c: ClassData): Boolean =
    c.hasTastyFile &&
      !TastyNames.declares(IArray.genericWrapArray(c.readTastyFileBytes()).toArray, pkg)

  /** A Java class's `.class` file facts, read from the entries (a Java class's nested classes are
    * files of the same package, `Outer$Inner`).
    */
  private def javaClassfiles(
      entries: List[ClasspathEntry]
  ): ClassSymbol => JavaClassfile = {
    val packages = entries
      .flatMap(e =>
        try e.listAllPackages()
        catch { case NonFatal(_) => Nil }
      )
      .groupBy(_.dotSeparatedName)
    def binaryName(c: ClassSymbol): String = c.owner match {
      case o: ClassSymbol => binaryName(o) + "$" + objectName(c)
      case _              => objectName(c)
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

  /** tasty-query reads a directory or a `.jar`; sbt 2 hands out content-addressed jars with no
    * suffix, which are read through a `.jar`-named link, made once per jar (a content-addressed
    * path keeps its content) and again only if the link is gone.
    */
  private[tasty] def readable(entry: Path): Path =
    if (Files.isDirectory(entry) || entry.getFileName.toString.endsWith(".jar")) entry
    else
      links.compute(
        entry.toAbsolutePath.normalize,
        (_, made) => if (made != null && Files.exists(made)) made else link(entry.toAbsolutePath)
      )

  private val links = new ConcurrentHashMap[Path, Path]

  private def link(entry: Path): Path = {
    val dir = Files.createTempDirectory("tasty-classes")
    dir.toFile.deleteOnExit()
    val link = dir.resolve(entry.getFileName.toString + ".jar")
    try Files.createSymbolicLink(link, entry)
    catch { case NonFatal(_) => Files.copy(entry, link) }
    link.toFile.deleteOnExit()
    link
  }

  private def objectName(c: ClassSymbol): String = c.name match {
    case n: ObjectClassTypeName => n.underlying.toString
    case n                      => n.toString
  }

}
