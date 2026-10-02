package cardinality

import scala.meta.*

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}
import org.specs2.Specification

// The definitions a source introduces and the report over them: what a reader sees, and what the
// walk has to keep straight to make the numbers mean anything — nested definitions, enum cases
// counted once, aliases that name a size without holding one.
class ReportSpec extends Specification {

  def is = s2"""
    Definitions
      packages qualify the names they hold      $qualifiedNames
      kinds carry their own sizes               $kinds
      nested definitions are their own rows     $nested
      enum cases are counted by their enum      $enumCases
      values inside a body add nothing          $bodyValues
      unresolved names are the reason           $unresolved
      a definition resolves inside its body     $enclosing
      a forward reference resolves through the body  $forward
      entry points size one node at a time      $entryPoints
      package objects qualify their members     $packageObjects
      enum bodies contribute their cases        $enumBodies
      a companion object keeps its own value    $companions
      a source counts its top-level values       $sourceTotals
      signatures carry their parameters         $signatures
      an applied user type substitutes arguments  $appliedType
      a parameterised recursion keeps its fixed point  $parameterisedRecursion
      a match type reduces on a known scrutinee  $matchType
      a match type that does not reduce stays unread  $matchTypeStuck
      a higher-kinded parameter names its arity  $higherKinded
      the top and the bottom of the lattice   $latticeEnds

    Report
      reads a directory of sources              $directory
      reads a library's sources jar             $jar
      reads a single file                       $singleFile
      reports a path it cannot read             $missing
      reports a source it cannot parse          $unparsable
      renders the numbers it found              $renders
      orders rows by cardinality                $orders
      renders an empty report                   $empty

    Counting the branch gained since the report was cut
      a solved recursion reports its fixed point  $recursiveRows
      a lazy hole counts ω, and is no question    $lazyRow
      the ε₀ tier arrives through modelled types  $tierRow
      a function space still names its blocker    $blockedRow
      an open abstraction has no bound            $openAbstraction
      a type constructor is a shape, not a gap    $constructorRow
      a named member resolves                     $memberByName
      an instance-qualified member resolves       $memberByValue
      an abstract member is open                  $memberAbstract
      a member the sources do not give reads as written  $memberUnknown
      a refinement is read as the type it refines $refinementRow
      an open abstraction reads as one in a report  $openRow

    Reading a library's sources as one set
      a type from another file resolves          $crossSource
      an argument from another file substitutes  $crossSourceArgument
      a qualified name resolves                  $crossSourceQualified
      the file's own package wins over another's $crossSourcePackage
      a name two packages define stays unresolved  $crossSourceAmbiguous
      an imported name is left to the import     $crossSourceImported
      a sealed hierarchy sums across files       $crossSourceSealed
      an opaque value is one value outside       $crossSourceOpaque
    """

  private def definitions(code: String): List[Definition] =
    Counter.definitions(dialects.Scala3(code).parse[Source].get)

  private def definition(code: String, name: String): Definition =
    definitions(code)
      .find(_.name == name)
      .getOrElse(throw new AssertionError(s"no definition $name"))

  // What a row's reasons render as, for the tests that read them as names.
  private def reasons(code: String, name: String): List[String] =
    definition(code, name).unbound.map(_.render)

  private def temporary(files: (String, String)*): Path = {
    val root = Files.createTempDirectory("cardinality")
    files.foreach {
      case (name, content) =>
        val file = root.resolve(name)
        Files.createDirectories(file.getParent)
        Files.writeString(file, content)
    }
    root
  }

  def qualifiedNames = {
    val found = definitions("package a\npackage b\nobject O { case class C(x: Boolean) }")
    found.map(_.name) === List("a.b.O", "a.b.O.C")
  }

  def kinds = {
    val found = definitions(
      """|sealed trait Light
         |case object Red extends Light
         |case class Dot(on: Boolean) extends Light
         |enum Color { case Red, Green, Blue }
         |object Module
         |type Flag = Boolean
         |opaque type Id = Byte
         |val top: Boolean = true
         |""".stripMargin,
    )
    found.map(d => (d.name, d.kind, d.size)) === List(
      // A sealed parent sums the cases the body defines, so its row carries the sum.
      ("Light", Definition.Kind.Abstract, Some(TinySize(3))),
      ("Red", Definition.Kind.Object, Some(UnitSize)),
      ("Dot", Definition.Kind.Class, Some(BooleanSize)),
      ("Color", Definition.Kind.Enum, Some(TinySize(3))),
      ("Module", Definition.Kind.Object, Some(UnitSize)),
      ("Flag", Definition.Kind.Alias, Some(BooleanSize)),
      // An opaque row is read where its representation is visible.
      ("Id", Definition.Kind.Opaque, Some(ByteSize)),
      ("top", Definition.Kind.Value, Some(UnitSize)),
    )
  }

  def nested = {
    val found = definitions(
      "sealed trait Light {\n  case object Red extends Light\n  case object Green extends Light\n}",
    )
    (found.map(_.name) === List("Light", "Light.Red", "Light.Green"))
      .and(found.map(_.size) === List(None, Some(UnitSize), Some(UnitSize)))
  }

  def enumCases = definitions("enum Color { case Red, Green, Blue }").map(_.name) === List("Color")

  def bodyValues =
    definitions("class Derived(a: Boolean) { val b: Boolean = !a }").map(_.name) === List(
      "Derived"
    )

  def unresolved = {
    (reasons("case class Holder(a: String, b: Boolean)", "Holder") === List("String"))
      .and(reasons("case class Generic[A](a: A)", "Generic") === List("A"))
      .and(
        reasons("case class Wrapped(a: Option[String])", "Wrapped") === List("String")
      )
      .and(
        // A modelled collection is no longer a reason: since the solver counts unbounded
        // lengths, `List[Boolean]` is ω exactly, and an unresolved element type reports itself.
        reasons("case class Counted(a: List[Boolean])", "Counted") === Nil
      )
      .and(
        reasons("case class Elements(a: List[String])", "Elements") === List(
          "String"
        )
      )
      .and(
        reasons("type Fst[T] = T match { case (a, b) => a }", "Fst") === List(
          "a match type"
        )
      )
  }

  def enclosing = {
    val found = definition("class Outer(a: Boolean) { case class Inner(o: Outer) }", "Outer.Inner")
    found.size === Some(BooleanSize)
  }

  def companions = {
    val found = definitions(
      """|sealed trait Light
         |case class Spot(on: Boolean) extends Light
         |object Spot
         |case class Uses(s: Spot)
         |""".stripMargin,
    )
    // A companion object shares its name with its class but not its value space: the type claims
    // the name — `Uses` reads `Spot` as the class — while the module is one value of its own.
    (found.map(d => (d.name, d.kind, d.size)) === List(
      ("Light", Definition.Kind.Abstract, Some(BooleanSize)),
      ("Spot", Definition.Kind.Class, Some(BooleanSize)),
      ("Spot", Definition.Kind.Object, Some(UnitSize)),
      ("Uses", Definition.Kind.Class, Some(BooleanSize)),
    ))
      .and(found.flatMap(_.unbound.map(_.render)) === Nil)
  }

  def entryPoints = {
    val source = dialects.Scala3("case class Pair(a: Boolean, b: Boolean)").parse[Source].get
    val pair = source.stats.collectFirst { case definition: Defn.Class => definition }.get
    val packageClause = dialects.Scala3("package p { case class X(a: Boolean) }").parse[Stat].get
    val packageObject =
      dialects.Scala3("package object p { case class X(a: Boolean) }").parse[Stat].get
    (Counter.stat(pair) === TinySize(4))
      .and(Counter.defn(pair) === TinySize(4))
      .and(Counter.ctor(pair.ctor) === TinySize(4))
      .and(Counter.param(pair.ctor.paramClauses.head.values.head) === BooleanSize)
      .and(Counter.stat(packageClause) === BooleanSize)
      .and(Counter.stat(packageObject) === BooleanSize)
  }

  def packageObjects =
    definitions("package object p { case class X(a: Boolean) }").map(_.name) === List("p.X")

  def sourceTotals = {
    val source = dialects
      .Scala3("val flag: Boolean = true\ncase class Pair(a: Boolean, b: Boolean)")
      .parse[Source]
      .get
    (Counter.source(source) === TinySize(5))
      .and(
        Counter.source(dialects.Scala3("val flag: Boolean = true").parse[Source].get) === UnitSize
      )
  }

  def signatures = {
    val found = definitions("case class Pair[A, B](a: A, b: B)")
    found.map(Definition.signature) === List("Pair[A, B]")
  }

  def enumBodies = {
    val found = definitions("enum E[A] { case One(a: A)\n  def flag: Boolean = true }")
    found.map(d => (d.name, d.params, d.size)) === List(("E", List("A"), Some(EffectiveOmega)))
  }

  def forward = {
    // Since the branch point the solver replaced the walk's read-as-you-go names: a body's
    // definitions are solved together, so a forward reference resolves instead of falling back to
    // ω. Both classes read two values, and no name is left unresolved.
    val code = "case class A(b: B)\ncase class B(b: Boolean)"
    (definition(code, "A").size === Some(BooleanSize))
      .and(reasons(code, "A") === Nil)
      .and(definition(code, "B").size === Some(BooleanSize))
  }

  def directory = {
    val root = temporary(
      "Holder.scala" -> "package p\ncase class Holder(a: String)",
      "Light.scala" -> "package p\nenum Light { case Red, Green }",
    )
    val report = Report.of(List(root))
    (report.sources.map(_.path) === List(
      root.resolve("Holder.scala").toString,
      root.resolve("Light.scala").toString
    ))
      .and(report.definitions.map(_.name) === List("p.Holder", "p.Light"))
      .and(report.errors === Nil)
  }

  def jar = {
    val archive = Files.createTempFile("cardinality", ".jar")
    val out = new ZipOutputStream(Files.newOutputStream(archive))
    try {
      out.putNextEntry(new ZipEntry("README.md"))
      out.write("not a source".getBytes(UTF_8))
      out.closeEntry()
      out.putNextEntry(new ZipEntry("p/Light.scala"))
      out.write("package p\nenum Light { case Red, Green }".getBytes(UTF_8))
      out.closeEntry()
    } finally out.close()
    val report = Report.of(List(archive))
    (report.sources.map(_.path) === List("p/Light.scala"))
      .and(report.definitions.map(_.name) === List("p.Light"))
      .and(report.errors === Nil)
  }

  def singleFile = {
    val root = temporary("Light.scala" -> "enum Light { case Red, Green }")
    Report.of(List(root.resolve("Light.scala"))).definitions.map(_.name) === List("Light")
  }

  def missing = {
    val path = Files.createTempDirectory("cardinality").resolve("missing.scala")
    val report = Report.of(List(path))
    (report.sources === Nil).and(report.errors.map(_.path) === List(path.toString))
  }

  def unparsable = {
    val root = temporary(
      "Broken.scala" -> "case class Broken(a: Boolean",
      "Fine.scala" -> "case class Fine(a: Boolean)",
    )
    val report = Report.of(List(root))
    (report.definitions.map(_.name) === List("Fine"))
      .and(report.errors.map(_.path) === List(root.resolve("Broken.scala").toString))
      .and(report.errors.map(_.message).mkString must contain("line"))
      .and(report.render must contain(s"could not read ${root.resolve("Broken.scala")}"))
  }

  def orders = {
    val root = temporary(
      "Types.scala" ->
        """|case class Huge(f: String => String)
           |case class Many(xs: List[Boolean])
           |case class Partial[A, B](a: A, b: Boolean)
           |case class Lossy(d: Double)
           |case class Big(n: Int)
           |type Zero = Nothing
           |""".stripMargin,
    )
    val rendered = Report.of(List(root)).render
    val table = rendered.linesIterator.toList
      .dropWhile(_ != "Stored-value estimates (constructor inputs; `?` = unresolved)")
      .drop(2)
    // What the calculator could not bound leads the table — `Huge` (the unresolved `String`) and
    // `Partial` (the unresolved `A`) — then the bounded sizes from the largest down: the countable
    // `Many`, the lossy `Lossy`, the capacity `Big`, and the empty `Zero` last. A size whose tier
    // is known is not a question: `Many` is ω exactly.
    (table(0) must contain("Huge"))
      .and(table(1) must contain("Partial"))
      .and(table(2) must contain("Many"))
      .and(table(3) must contain("Lossy"))
      .and(table(4) must contain("Big"))
      .and(table(5) must contain("Zero"))
      .and(table(0) must contain("?"))
      .and(table(1) must contain("?"))
      .and(table(2) must contain("ω"))
      .and(rendered must contain("1 with no values"))
  }

  def empty = {
    val rendered = Report.of(Nil).render
    rendered === """scala-cardinality — 0 sources, 0 definitions
                  |  stored-value estimates: constructor inputs only; finite capacities are upper bounds
                  |  ?: no single number here — the reason after the row says what would give one""".stripMargin
  }

  def renders = {
    val root = temporary(
      "Types.scala" ->
        """|package p
           |sealed trait Shape
           |case class Dot(on: Boolean) extends Shape
           |case class Box[A](a: A) extends Shape
           |object Shape
           |type Flag = Boolean
           |""".stripMargin,
    )
    val rendered = Report.of(List(root)).render
    val lines = rendered.linesIterator.toList
    val estimate = lines
      .dropWhile(_ != "Stored-value estimates (constructor inputs; `?` = unresolved)")
      .drop(2)
    (lines.head === "Generic method / constructor implementation cardinalities")
      .and(rendered must contain("scala-cardinality — 1 source, 5 definitions"))
      .and(rendered must contain("stored-value estimates: constructor inputs only"))
      .and(rendered must contain("1  p.Box.<init>  Box[A](a: A)  Types.scala:4  [constructor]"))
      .and(rendered must contain("?  p.Shape"))
      .and(rendered must contain("  abstract  Types.scala:2"))
      .and(rendered must contain("depends on its instantiation (A)"))
      .and(estimate.last must contain("1  p.Shape   object"))
  }

  def appliedType = {
    val found = definitions(
      """|case class Pair[A](a: A, b: A)
         |enum Opt[A] { case None; case Some(a: A) }
         |type TwoBools = Pair[Boolean]
         |case class Use(p: Pair[Boolean], o: Opt[Boolean], t: TwoBools)
         |""".stripMargin,
    )
    // A reference to a generic definition supplies its arguments: `Pair[Boolean]` is `Pair`'s
    // equation read with `A` bound to 2, and an alias' body substitutes the same way. The
    // definition's own row keeps its parameters free — `Pair` as a template is `ω`, with `A` as
    // the reason — while a use site multiplies what the instantiation is worth.
    // A row whose reasons are its own parameters says so instead of listing names, and the
    // summary counts it apart from what the calculator could not read.
    (found.map(d => (d.name, d.size, d.unbound.map(_.render))) === List(
      ("Pair", Some(EffectiveOmega), List("A")),
      ("Opt", Some(EffectiveOmega + UnitSize), List("A")),
      ("TwoBools", Some(TinySize(4)), Nil),
      ("Use", Some(TinySize(48)), Nil),
    ))
      .and(found.map(Definition.instantiationDependent) === List(true, true, false, false))
  }

  def parameterisedRecursion = {
    val found = definitions(
      "case class Node[A](value: A, next: Option[Node[A]])",
    )
    // `Node[A]` inside `Node`'s own equation is the definition's own parameters: it borrows the
    // fixed point the solver gives the name, so the parameterised recursion keeps its ω reading
    // and reports the parameter, not the name.
    found.map(d => (d.name, d.size, d.unbound.map(_.render))) === List(
      ("Node", Some(EffectiveOmega), List("A"))
    )
  }

  private def rowsOf(root: Path): List[(String, Option[Size], List[String])] =
    Report.of(List(root)).definitions.map(d => (d.name, d.size, d.unbound.map(_.render)))

  def crossSource = {
    val root = temporary(
      "Box.scala" -> "package p\ncase class Box(a: Boolean)",
      "Use.scala" -> "package p\ncase class Use(b: Box)",
    )
    // A report reads the supplied sources as one set: `Box` is in a sibling file, and `Use` is
    // worth what a reference to it is worth.
    rowsOf(root) === List(
      ("p.Box", Some(BooleanSize), Nil),
      ("p.Use", Some(BooleanSize), Nil),
    )
  }

  def crossSourceArgument = {
    val root = temporary(
      "Pair.scala" -> "package p\ncase class Pair[A](a: A, b: A)",
      "Use.scala" -> "package p\ncase class Use(p: Pair[Boolean])",
    )
    rowsOf(root) === List(
      ("p.Pair", Some(EffectiveOmega), List("A")),
      ("p.Use", Some(TinySize(4)), Nil),
    )
  }

  def crossSourceQualified = {
    val root = temporary(
      "Box.scala" -> "package p\ncase class Box(a: Boolean)",
      "Use.scala" -> "package p\ncase class Use(b: p.Box)",
    )
    rowsOf(root) === List(
      ("p.Box", Some(BooleanSize), Nil),
      ("p.Use", Some(BooleanSize), Nil),
    )
  }

  def crossSourcePackage = {
    val root = temporary(
      "a/Widget.scala" -> "package a\ncase class Widget(x: Boolean)",
      "b/Widget.scala" -> "package b\ncase class Widget(x: Boolean)",
      "a/Use.scala" -> "package a\ncase class Use(w: Widget)",
    )
    // A package member needs no import: inside `a`, `Widget` is `a.Widget`, whatever `b` defines.
    rowsOf(root).filter(_._1 == "a.Use") === List(("a.Use", Some(BooleanSize), Nil))
  }

  def crossSourceAmbiguous = {
    val root = temporary(
      "a/Widget.scala" -> "package a\ncase class Widget(x: Boolean)",
      "b/Widget.scala" -> "package b\ncase class Widget(x: Boolean)",
      "c/Use.scala" -> "package c\ncase class Use(w: Widget)",
    )
    // From a third package, `Widget` could only be an import of either: the report says so rather
    // than picking one.
    rowsOf(root).filter(_._1 == "c.Use") === List(("c.Use", Some(EffectiveOmega), List("Widget")))
  }

  def crossSourceImported = {
    val root = temporary(
      "a/Widget.scala" -> "package a\ncase class Widget(x: Boolean)",
      "c/Use.scala" -> "package c\nimport a.Widget\ncase class Use(w: Widget)",
    )
    // The import decides what the name means and the calculator does not follow imports, so the
    // name stays a reason even though the source set defines it.
    rowsOf(root).filter(_._1 == "c.Use") === List(("c.Use", Some(EffectiveOmega), List("Widget")))
  }

  def crossSourceSealed = {
    val root = temporary(
      "Nat.scala" -> "package p\nsealed trait Nat",
      "Cases.scala" -> "package p\ncase object Zero extends Nat\ncase class Succ(n: Nat) extends Nat",
      "Use.scala" -> "package p\ncase class Use(n: Nat)",
    )
    // The package's equations are solved as one system, so the sealed parent sums the cases a
    // sibling file defines, and the recursion is read through it. Rows follow the sources, so
    // `Cases.scala` comes before `Nat.scala`.
    (rowsOf(root).map(_._1) === List("p.Zero", "p.Succ", "p.Nat", "p.Use"))
      .and(
        rowsOf(root) === List(
          ("p.Zero", Some(UnitSize), Nil),
          ("p.Succ", Some(EffectiveOmega), Nil),
          ("p.Nat", Some(EffectiveOmega), Nil),
          ("p.Use", Some(EffectiveOmega), Nil),
        )
      )
  }

  def crossSourceOpaque = {
    val root = temporary(
      "Id.scala" -> "package p\nopaque type Id = Byte",
      "User.scala" -> "package p\ncase class User(id: Id)",
    )
    // An opaque row reads its representation (`Id` is 2^8), and a reference from outside the
    // defining scope is one opaque value — the two readings the contract keeps apart.
    (rowsOf(root).filter(_._1 == "p.User") === List(("p.User", Some(UnitSize), Nil)))
      .and(rowsOf(root).filter(_._1 == "p.Id") === List(("p.Id", Some(ByteSize), Nil)))
  }

  def matchType = {
    val found = definitions(
      """|type Fst[T] = T match { case (f, s) => f }
         |type Snd[T] = T match { case (f, s) => s }
         |type Pick[T] = T match
         |  case Int => Boolean
         |  case x => x
         |type Two = Fst[(Boolean, Boolean)]
         |type Second = Snd[(Int, Boolean)]
         |type ANumber = Pick[Int]
         |type Other = Pick[String => String]
         |""".stripMargin,
    )
    // A match type reduces on the argument's syntax: `Fst[(Boolean, Boolean)]` is `Boolean` because
    // the case pattern is that tuple and its body is the first binder. A concrete pattern matches
    // by spelling and falls through to the binder case when it does not.
    found.map(d => (d.name, d.size, d.unbound.map(_.render))) === List(
      ("Fst", Some(EffectiveOmega), List("a match type")),
      ("Snd", Some(EffectiveOmega), List("a match type")),
      ("Pick", Some(EffectiveOmega), List("a match type")),
      ("Two", Some(BooleanSize), Nil),
      ("Second", Some(BooleanSize), Nil),
      ("ANumber", Some(BooleanSize), Nil),
      ("Other", Some(EffectiveEpsilon0), List("String")),
    )
  }

  def matchTypeStuck = {
    val found = definitions(
      """|type Fst[T] = T match { case (f, s) => f }
         |type Stuck = Fst[String]
         |""".stripMargin,
    )
    // No case matches a `String` scrutinee: the match type stays unreduced, which a report names
    // rather than guessing what it might be worth — and the scrutinee keeps its own reason too.
    found.find(_.name == "Stuck").map(d => (d.size, d.unbound.map(_.render))) ===
      Some((Some(EffectiveOmega), List("String", "a match type")))
  }

  def openAbstraction = {
    val found = definitions(
      """|trait Open
         |sealed trait Closed
         |case object Only extends Closed
         |case class Uses(o: Open, c: Closed)
         |""".stripMargin,
    )
    // An unsealed trait is a capability someone else implements: a reference to it has no bound,
    // so the row carries the reason rather than an unknown name. A sealed parent is the other
    // case — its children are the sum, so `Closed` is worth 1 through `Only`.
    (found.map(d => (d.name, d.size, d.unbound.map(_.render))) === List(
      ("Open", None, Nil),
      ("Closed", Some(UnitSize), Nil),
      ("Only", Some(UnitSize), Nil),
      ("Uses", Some(EffectiveOmega), List("Open")),
    ))
      .and(found.find(_.name == "Uses").exists(Definition.open))
  }

  def constructorRow = {
    val found = definitions("type Forget[F[_]] = [X, A] =>> F[(X, A)]")
    // The alias names a function on types: no value space of its own, so the row is a shape rather
    // than a question, and applying the constructor is where a size would come from.
    found.map(d => (d.name, d.kind, d.size, d.unbound)) === List(
      ("Forget", Definition.Kind.Constructor, None, Nil)
    )
  }

  def memberByName = {
    val found = definitions(
      """|object Outer { type B = Boolean }
         |trait Foo[A] { type B = A }
         |case class C(x: Outer.B, y: Foo.B)
         |""".stripMargin,
    )
    // A qualified reference is a member the sources declare: the owner's parameters are replaced by
    // what the reference supplied, so `Foo[Boolean].B` is `Boolean`.
    found.map(d => (d.name, d.size, d.unbound.map(_.render))) === List(
      ("Outer", Some(UnitSize), Nil),
      ("Outer.B", Some(BooleanSize), Nil),
      ("Foo", None, Nil),
      ("Foo.B", Some(EffectiveOmega), List("A")),
      ("C", Some(EffectiveOmega), List("A")),
    )
  }

  def memberByValue = {
    val found = definitions(
      """|trait Foo[A] { type B = A }
         |class D(x: Foo[Boolean]) { type Y = x.B }
         |""".stripMargin,
    )
    // An instance-qualified member reads the value's declared type, which is what makes `x.B` a
    // member of `Foo[Boolean]`.
    found.map(d => (d.name, d.size, d.unbound.map(_.render))) === List(
      ("Foo", None, Nil),
      ("Foo.B", Some(EffectiveOmega), List("A")),
      ("D", Some(EffectiveOmega), List("Foo")),
      ("D.Y", Some(BooleanSize), Nil),
    )
  }

  def memberAbstract = {
    val found = definitions(
      """|trait O { type Z }
         |class C(x: O) { type Y = x.Z }
         |""".stripMargin,
    )
    // A member a body declares abstract is supplied by whoever implements it: unbounded, which the
    // row says, rather than an unknown name.
    (found.map(d => (d.name, d.size, d.unbound.map(_.render))) === List(
      ("O", None, Nil),
      ("C", Some(EffectiveOmega), List("O")),
      ("C.Y", Some(EffectiveOmega), List("x.Z")),
    ))
      .and(found.find(_.name == "C.Y").exists(Definition.open))
  }

  def memberUnknown = {
    val found = definitions(
      """|class C(x: NotSupplied) { type Y = x.Z }
         |""".stripMargin,
    )
    // The sources do not say what `x` is, so the reference is the reason, as it was written.
    found.find(_.name == "C.Y").map(d => (d.size, d.unbound.map(_.render))) ===
      Some((Some(EffectiveOmega), List("x.Z")))
  }

  def refinementRow = {
    val found = definitions(
      """|trait Open
         |case class Value(a: Boolean)
         |case class Use(o: Open { type X = Boolean }, v: Value { type X = Boolean })
         |""".stripMargin,
    )
    // A refinement's base type is what a reference means: `Value { type X = Boolean }` is worth 2,
    // and refining an open trait is still open — a fact about the sources, not a missing rule.
    (found.map(d => (d.name, d.size, d.unbound.map(_.render))) === List(
      ("Open", None, Nil),
      ("Value", Some(BooleanSize), Nil),
      ("Use", Some(EffectiveOmega), List("Open")),
    ))
      .and(found.find(_.name == "Use").exists(Definition.open))
  }

  def openRow = {
    val root = temporary("Types.scala" -> "package p\ntrait Open\ncase class Uses(o: Open)")
    val rendered = Report.of(List(root)).render
    (rendered must contain("open to implementations (Open)"))
      .and(rendered must contain("1 unbounded by an open abstraction"))
  }

  def higherKinded = {
    val found = definitions(
      """|class Box[F[_]](f: F[Boolean])
         |class Pairish[F[_, _]](f: F[Int, Int])
         |case class Mixed[A, F[_]](value: A, wrapped: F[A])
         |""".stripMargin,
    )
    // A parameter that takes parameters of its own is a value space only the instantiation decides,
    // and the row names its arity the way eo's triage list does: `F[_]`, `F[_, _]`.
    (found.map(d => (d.name, d.unbound.map(_.render))) === List(
      ("Box", List("F[_]")),
      ("Pairish", List("F[_, _]")),
      ("Mixed", List("A", "F[_]")),
    ))
      .and(found.forall(Definition.instantiationDependent))
  }

  def latticeEnds = {
    val found = definitions(
      """|case class MaybeNull(x: Null)
         |case class Anything(x: Any)
         |case class Elements(xs: Array[Any])
         |case class Nullable(xs: Array[Int] | Null)
         |""".stripMargin,
    )
    // `Null` has the one value `null`; `Any` is the top of the lattice, and nothing the analysis can
    // place is above the ε₀ tier, so it sits there — which makes an `Array[Any]` the countable
    // space its length makes it, rather than an unknown name. eo's `PSVec.Slice` is that shape.
    found.map(d => (d.name, d.size, d.unbound.map(_.render))) === List(
      ("MaybeNull", Some(UnitSize), Nil),
      ("Anything", Some(EffectiveEpsilon0), Nil),
      ("Elements", Some(EffectiveOmega), Nil),
      ("Nullable", Some(EffectiveOmega + UnitSize), Nil),
    )
  }

  def recursiveRows = {
    val found = definitions(
      "sealed trait Nat\ncase object Zero extends Nat\ncase class Succ(n: Nat) extends Nat",
    )
    // The body's equations are solved together, so `Succ` is worth what a reference to it is worth:
    // `Nat` is the sum of its cases and the family is countably infinite. The row carries the same
    // number as the reference, and nothing about it is unresolved.
    (found.map(d => (d.name, d.kind, d.size)) === List(
      // A sealed parent's sum is the value a reference to it reads.
      ("Nat", Definition.Kind.Abstract, Some(EffectiveOmega)),
      ("Zero", Definition.Kind.Object, Some(UnitSize)),
      ("Succ", Definition.Kind.Class, Some(EffectiveOmega)),
    ))
      .and(found.flatMap(_.unbound.map(_.render)) === Nil)
  }

  def lazyRow = {
    val root = temporary(
      "Timeline.scala" ->
        "package p\ncase class Timeline(head: Boolean, tail: => Timeline)",
    )
    val rendered = Report.of(List(root)).render
    val table = rendered.linesIterator.toList
      .dropWhile(_ != "Stored-value estimates (constructor inputs; `?` = unresolved)")
      .drop(2)
    // A hole the constructor never demands counts its infinite values as well, so `ω` is an
    // answer: the row shows the size, and the summary does not count it as unresolved.
    (definition("case class Timeline(head: Boolean, tail: => Timeline)", "Timeline").size ===
      Some(EffectiveOmega))
      .and(table(0) must contain("ω  p.Timeline"))
      .and(rendered must contain("1 with more than one value"))
  }

  def tierRow = {
    val grid = definition("case class Grid(cells: LazyList[LazyList[Unit]])", "Grid")
    // ω^ω lands on the analysis' ε₀ tier, and every step of that route is a type the calculator
    // models: the row is a size, not a question.
    (grid.size === Some(EffectiveEpsilon0)).and(grid.unbound.map(_.render) === Nil)
  }

  def blockedRow = {
    val reducer = definition("case class Reducer(run: String => String)", "Reducer")
    (reducer.size === Some(EffectiveEpsilon0))
      .and(reducer.unbound.map(_.render) === List("String"))
  }

}
