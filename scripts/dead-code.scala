//> using scala 3.9.0
//> using dep org.scalameta:semanticdb-shared_2.13:4.17.4

// Production symbols of `core` that nothing references, read from the build's SemanticDB.
//
// Usage: scripts/dead-code.sh, which compiles first and then runs this script from the repository
// root (`scala-cli run scripts/dead-code.scala`).
//
// Three lists, each a candidate to read and not a verdict:
//   - never referenced: no occurrence in `core`, the sbt plugin, the Mill plugin or their tests;
//   - referenced only by tests: code the tests keep alive;
//   - case/enum never built or named in production: a case nothing constructs or matches by name.
// Known false positives: a member reached only through an override or by reflection, a given found
// by implicit search (SemanticDB records no occurrence for a synthetic reference), the entry points
// the plugins call by name, and what only `bench` calls (it is not scanned). Members of case
// classes and enums are skipped in the first two lists: the compiler makes most of them.

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.meta.internal.semanticdb.*

@main def deadCode(): Unit =
  // sbt 2 writes each module's SemanticDB to `target/out/jvm/scala-<version>/<module>/meta`. A
  // directory that is not there (a module renamed, another Scala version, nothing compiled) stops
  // the scan: read as empty, it would list every symbol that module references.
  // `core` is on tasty-query's Scala (`coreScalaVersion` in build.sbt), the plugins on the build's.
  def documents(dir: String, scala: String = "3.9.0"): List[TextDocument] =
    val root = Path.of(s"target/out/jvm/scala-$scala").resolve(dir)
    if !Files.isDirectory(root) then sys.error(s"no SemanticDB at $root: run scripts/dead-code.sh")
    Files.walk(root).iterator.asScala.filter(_.toString.endsWith(".semanticdb")).toList
      .flatMap(file => TextDocuments.parseFrom(Files.readAllBytes(file)).documents)
  def references(of: List[TextDocument]): Set[String] =
    of.flatMap(_.occurrences).filter(_.role == SymbolOccurrence.Role.REFERENCE).map(_.symbol).toSet

  val core = documents("scala-cardinality/meta", "3.10.1")
  val production =
    references(core ++ documents("sbt-cardinality/meta") ++ documents("mill-cardinality_mill1/meta"))
  val tests =
    references(documents("scala-cardinality/test-meta", "3.10.1") ++ documents("mill-cardinality_mill1/test-meta"))
  val infos = core.flatMap(d => d.symbols.map(info => info.symbol -> (info, d.uri))).toMap

  // Members the compiler writes or the language calls by name: never listed.
  val implied = Set("<init>", "apply", "unapply", "copy", "toString", "equals", "hashCode",
    "productElement", "productArity", "canEqual", "fromProduct", "ordinal", "values", "valueOf", "main")
  def name(symbol: String): String =
    symbol.stripSuffix(".").stripSuffix("#").replaceAll("\\(\\+?\\d*\\)\\.?$", "").split("[#./]").last
  def owner(symbol: String): String =
    symbol.stripSuffix(".").stripSuffix("#").replaceAll("\\([^)]*\\)$", "").split("(?<=[#./])").init.mkString
  def has(info: SymbolInformation, property: SymbolInformation.Property): Boolean =
    (info.properties & property.value) != 0
  def caseLike(symbol: String): Boolean = infos.get(symbol).exists((info, _) =>
    has(info, SymbolInformation.Property.CASE) || has(info, SymbolInformation.Property.ENUM))

  // A definition is a candidate when it is a method, or a field, object, class, trait or type
  // outside any method (a symbol with `(` is a method's parameter or local), overrides nothing and
  // has no production reference.
  val candidates = core.flatMap { d =>
    d.occurrences
      .filter(o => o.role == SymbolOccurrence.Role.DEFINITION && !o.symbol.startsWith("local"))
      .map(o => (o.symbol, d.uri, o.range.fold(0)(_.startLine + 1)))
  }.distinct.filter { (symbol, _, _) =>
    infos.get(symbol).exists { (info, _) =>
      val kind = info.kind
      val member = kind.isMethod ||
        ((kind.isField || kind.isObject || kind.isClass || kind.isTrait || kind.isType) && !symbol.contains("("))
      member && info.overriddenSymbols.isEmpty && !implied(name(symbol))
    } && !production(symbol) && !symbol.endsWith(")") && !caseLike(symbol) && !caseLike(owner(symbol))
  }
  val (testOnly, unused) = candidates.partition((symbol, _, _) => tests(symbol))
  def show(title: String, found: List[(String, String, Int)]): Unit =
    println(s"== $title (${found.size})")
    found.sortBy(f => (f._2, f._3)).foreach((symbol, uri, line) =>
      println(f"$uri%-70s:$line%-5d ${infos(symbol)._1.kind}%-8s $symbol"))
  show("never referenced", unused)
  show("referenced only by tests", testOnly)

  // A case class, case object or enum case that production code never constructs or names.
  val cases = infos.keys.toList.filter(symbol =>
    caseLike(symbol) && !symbol.startsWith("local") &&
      (infos(symbol)._1.kind.isClass || infos(symbol)._1.kind.isObject))
  def built(symbol: String): Boolean =
    val companion = symbol.stripSuffix("#") + "."
    val cls = symbol.stripSuffix(".") + "#"
    List(symbol, companion, cls, companion + "apply().", cls + "`<init>`().").exists(production)
  val unbuilt = cases.filterNot(built).sorted
  println(s"== case/enum never built or named in production (${unbuilt.size})")
  unbuilt.foreach(symbol => println(f"${infos(symbol)._2}%-70s $symbol"))
