package cardinality.analysis

import scala.jdk.CollectionConverters.*

import cardinality.Compiled
import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.capacity.Definition
import cardinality.reporting.Report
import java.nio.file.{FileSystems, Files, Path}
import org.specs2.Specification
import org.specs2.execute.Result

/** A module's dependencies are part of the program (v1.md §2): what a row counts through a
  * declaration its module only has as compiled classes. Each fixture is a module compiled against
  * its dependency and read with it as the dependency classpath (`Compiled.reportOver`), never as a
  * source of the module: a Scala 3 dependency (`tastyfixtures`, from its TASTy), a Java one
  * (`javafixtures`, from its class files), a Scala 2 one (two of scalameta's 2.13 jars, from their
  * pickles), a JDK module, and dependencies a fixture writes itself (`Compiled.dependency`).
  *
  * Where the module can be read without the dependency, its rows refuse or read otherwise, and the
  * examples say so: the numbers below are the dependency's.
  */
class DependencyRowsSpec extends Specification {

  def is = s2"""
    A Scala 3 dependency
      counts as its source does, or refuses                           $parity
      supplies what the module alone cannot name                      $supplied
      is no row of the module                                         $noRows
      a compiled inline body is not read: its caller refuses          $inlineBody
      a stored value through it is its source's                       $stored
    A Java dependency
      an enum is its constants, a static field of its type is none    $javaEnum
      an interface is the record of its abstract methods              $javaInterface
      a class's state and a sealed interface's subclasses are not read $javaHidden
      nothing of it is read without it                                $javaAlone
      `Object` is `Any`; a wildcard is read, a raw type is not        $javaSignatures
    A Scala 2 dependency
      a class of it has a count where the module alone refuses        $scala2
      a sealed class with a child the pickle does not declare refuses $scala2Sealed
    The dependency classpath
      a class two dependencies declare is the first one's             $firstWins
      a dependency's template is the one of its qualified name        $qualified
      a JDK module other than java.base is read where a source names it $jdkModule
  """

  private def jarOf(c: Class[?]): Path =
    Path.of(c.getProtectionDomain.getCodeSource.getLocation.toURI)

  // The classes of one package of the test classes, as a class directory of its own: a dependency
  // that holds nothing else.
  private def packaged(name: String): Path = {
    val all = jarOf(classOf[tastyfixtures.Endo[?]])
    val out = Files.createTempDirectory("dependency")
    sys.addShutdownHook(Compiled.deleted(out))
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

  private def counts(report: Report): Map[String, Count] =
    report.methods.map(entry => entry.name -> entry.count).toMap

  private def refused(count: Count, reason: String): Result =
    count must beLike { case Count.Unresolved(reasons) => reasons.mkString must contain(reason) }

  // Refused, and for another reason than the module read without the dependency is: a refusal that
  // is the dependency's, whatever the front end words it as.
  private def refusedThrough(read: Count, alone: Count): Result =
    (read must beAnInstanceOf[Count.Unresolved])
      .and(alone must beAnInstanceOf[Count.Unresolved])
      .and(read !== alone)

  private def each(found: Map[String, Count], expected: Count)(names: String*): Result =
    names.map(name => found(name) === expected).reduce(_ and _)

  // tastyfixtures/Fixtures.scala, the Scala 3 dependency's source.
  private lazy val fixtureSource: String = {
    val relative = Path.of("core/src/test/scala/tastyfixtures/Fixtures.scala")
    val file = Iterator
      .iterate(Path.of("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(relative))
      .find(Files.exists(_))
      .get
    Files.readString(file)
  }

  private val usesScala3 = "t/T.scala" ->
    """package t
      |import tastyfixtures.*
      |def endo(e: Endo[Boolean], b: Boolean): Boolean = ???
      |def pair(p: Pair): Boolean = ???
      |def mkPair(b: Boolean): Pair = ???
      |def light(l: Light): Boolean = ???
      |def suit(s: Suit): Boolean = ???
      |def flag(f: Flag): Boolean = ???
      |def show(b: Boolean)(using s: Show[Boolean]): Boolean = ???
      |def built(b: Built): Int = ???
      |def fed(f: Fed): Int = ???
      |inline def picked[A](x: A, y: A): A = Inlines.pick(x, y)
      |final case class Holds(p: Pair, l: Light, s: Suit)
      |""".stripMargin

  private lazy val throughClasses =
    Compiled.reportOver(List(packaged("tastyfixtures")), usesScala3)

  private lazy val throughSource = Compiled.report(usesScala3, "f/F.scala" -> fixtureSource)
  private lazy val alone = Compiled.report(usesScala3)

  // Pair is two Booleans, 4 values: 2^4 = 16 predicates, and 4^2 = 16 ways to build one from b.
  // Light is Red, Green or Blink(on), 4 values: 16. Suit has two cases: 2^2 = 4. Flag is opaque
  // outside its scope, one value (rule F): 2. Endo[Boolean] and Show[Boolean] are each the record
  // of one Boolean => Boolean: a body observes b and its two results, 2^(2^3) = 2^8. `Built()` has
  // one value, its inherited seed being the 0 its constructor passes: an Int from one value, 2^32.
  // Each is what the dependency's own source counts. Fed's seed is its parameter, which only its
  // constructor's body says: through the classes fed is the source's count or a refusal, never
  // Built's 2^32.
  def parity = {
    val (classes, source) = (counts(throughClasses), counts(throughSource))
    val names =
      List("endo", "pair", "mkPair", "light", "suit", "flag", "show", "built", "fed", "picked")
        .map("t." + _)
    names
      .map { name =>
        (classes(name) must beLike {
          case _: Count.Unresolved => ok
          case counted             => counted === source(name)
        }).setMessage(s"$name: ${classes(name)} through classes, ${source(name)} through source")
      }
      .reduce(_ and _)
      .and(each(classes, Count.Finite(16))("t.pair", "t.mkPair", "t.light"))
      .and(classes("t.suit") === Count.Finite(4))
      .and(classes("t.flag") === Count.Finite(2))
      .and(each(classes, Count.Finite(256))("t.endo", "t.show"))
      .and(classes("t.built") === Count.Finite(BigInt(2).pow(32)))
      .and(classes("t.fed") !== classes("t.built"))
  }

  // The module compiles against the dependency either way; read without it the names are not
  // declared anywhere the analysis reads.
  def supplied =
    List("endo", "pair", "mkPair", "light", "suit", "flag", "show", "built", "fed")
      .map(name => refused(counts(alone)(s"t.$name"), "declaration not supplied"))
      .reduce(_ and _)

  // A dependency is environment: the rows are the module's own, as read without it.
  def noRows =
    (throughClasses.methods.map(_.name).sorted === alone.methods.map(_.name).sorted)
      .and(throughClasses.definitions.map(_.name) === List("t.Holds"))
      .and(throughSource.definitions.map(_.name) must contain("tastyfixtures.Pair"))

  // `picked` expands `Inlines.pick`, an inline method whose body the classes hold and this reading
  // does not read: refused, naming `pick`, never the 2 its source counts. Without the dependency it
  // refuses too, for `Inlines`, which it then cannot name.
  def inlineBody =
    refused(counts(throughClasses)("t.picked"), "pick")
      .and(refusedThrough(counts(throughClasses)("t.picked"), counts(alone)("t.picked")))
      .and(counts(throughSource)("t.picked") === Count.Finite(2))

  // Holds is Pair × Light × Suit: 4 · 4 · 2 = 32 stored values, through the classes as through the
  // source.
  def stored = {
    def holds(report: Report) = report.definitions.collectFirst {
      case Definition("t.Holds", Definition.Kind.Class, _, size, unbound, _) =>
        (size.map(_.render), unbound)
    }
    (holds(alone) must beSome(beLike[(Option[String], List[Definition.Unbound])] {
      case (_, unbound) => unbound must not(beEmpty)
    }))
      .and(holds(throughClasses) === Some((Some("32"), Nil)))
      .and(holds(throughSource) === holds(throughClasses))
  }

  private val usesJava = "j/J.scala" ->
    """package j
      |import javafixtures.*
      |def coin(c: Coin): Boolean = ???
      |def pickCoin(b: Boolean): Coin = ???
      |def op(o: Op): Boolean = ???
      |def size(s: Box.Size): Boolean = ???
      |def toggle(t: Toggle): Boolean = ???
      |def mkToggle: Toggle = ???
      |def point(p: Box.Point): Boolean = ???
      |def box(b: Box[Boolean]): Boolean = ???
      |def shape(s: Shape): Boolean = ???
      |def choice(c: Choice): Boolean = ???
      |def mkChoice: Choice = ???
      |def mkAny: Sig.AnyFn = ???
      |def useAny(f: Sig.AnyFn): Boolean = ???
      |def useWild(w: Sig.Wild[Boolean]): Boolean = ???
      |def useRaw(r: Sig.Raw): Boolean = ???
      |""".stripMargin

  private lazy val throughJava = counts(
    Compiled.reportOver(List(packaged("javafixtures")), usesJava)
  )

  private lazy val aloneJava = counts(Compiled.report(usesJava))

  // Coin is HEADS and TAILS: 2^2 = 4 predicates and 4 ways to pick one from b. Its static field
  // DEFAULT, of its own type, is a member of the companion and no third constant (8 otherwise). Op's
  // constants have bodies and are still two; Box.Size is a nested enum of two.
  def javaEnum =
    each(throughJava, Count.Finite(4))("j.coin", "j.pickCoin", "j.op", "j.size")

  // Toggle has no state: a value is the record of its abstract `flip`, a Boolean => Boolean, 4
  // behaviors, and 2^4 = 16 predicates on one. Its default `same` nothing overrides, so it is
  // concrete code over flip (the closed world); its static `identity` and its constant LIMIT are
  // the companion's, no members of the record (LIMIT in it would make 4 · 2^32).
  def javaInterface =
    (throughJava("j.mkToggle") === Count.Finite(4))
      .and(throughJava("j.toggle") === Count.Finite(16))

  // A Java class's constructor sets state the class files do not tie to its parameters (Box and its
  // subclass Shape), a final class's field is set by a constructor body that is not read (Point),
  // and a sealed interface's permitted classes are not read (Choice): each refuses rather than
  // count what it does not see, and for another reason than a class that is not supplied.
  def javaHidden =
    List("j.point", "j.box", "j.shape", "j.choice", "j.mkChoice")
      .map(name => refusedThrough(throughJava(name), aloneJava(name)))
      .reduce(_ and _)
      .and(refused(throughJava("j.box"), "hidden constructor state"))
      .and(refused(throughJava("j.shape"), "hidden constructor state"))

  // Every row of the Java module refuses without the dependency, for the class it cannot name: none
  // of the counts above is the module's own.
  def javaAlone =
    (aloneJava.keySet === throughJava.keySet)
      .and(aloneJava.values.map(refused(_, "declaration not supplied")).reduce(_ and _))

  // AnyFn's one method is `Object apply(Object)`, and Java's Object is Any: an Any => Any, whose
  // input is one value and whose output is ω (v1.md §2, a top type by its place): ω ways to build
  // one. A body given one holds an output top and has two results to choose: ω. Read as a class
  // `Object` the model does not have, both would refuse. Wild's `List<? extends A>` is read as a
  // bounded wildcard, which the reading then refuses by name; Raw's `List` has no argument to read,
  // and refuses for the signature, not for a class it cannot name.
  def javaSignatures =
    each(throughJava, Count.Countable)("j.mkAny", "j.useAny")
      .and(refused(throughJava("j.useWild"), "wildcard"))
      .and(refusedThrough(throughJava("j.useRaw"), aloneJava("j.useRaw")))

  // scalameta's `io_2.13` and `common2_2.13`, compiled by Scala 2.
  private val scala2Jars = List(
    jarOf(classOf[scala.meta.io.AbsolutePath]),
    jarOf(classOf[scala.meta.internal.SingletonReference[?]])
  )

  private val usesScala2 = "s/S.scala" ->
    """package s
      |import scala.meta.io.*
      |import scala.meta.internal.io.*
      |def path(a: AbsolutePath): Boolean = ???
      |def classpath(c: Classpath): Boolean = ???
      |def drop(c: Classpath): Unit = ???
      |def out(o: OutputStreamIO[Boolean]): Boolean = ???
      |def mkOut: OutputStreamIO[Boolean] = ???
      |""".stripMargin

  private lazy val throughScala2 = counts(Compiled.reportOver(scala2Jars, usesScala2))
  private lazy val aloneScala2 = counts(Compiled.report(usesScala2))

  // `OutputStreamIO[Boolean]` is the record of its one abstract `write`, whose result is Unit: one
  // value, so one way to build it and 2^1 predicates on it. Dropping a Classpath is one body, `()`,
  // with or without its declaration: an unsupplied wildcard rebinds no `Unit` (register D37).
  def scala2 = {
    val unsupplied = aloneScala2
    (throughScala2("s.mkOut") === Count.Finite(1))
      .and(throughScala2("s.out") === Count.Finite(2))
      .and(throughScala2("s.drop") === Count.Finite(1))
      .and(unsupplied("s.drop") === Count.Finite(1))
      .and(refused(unsupplied("s.mkOut"), "declaration not supplied"))
      .and(refused(unsupplied("s.out"), "declaration not supplied"))
  }

  // `AbsolutePath` is a sealed case class whose one child is anonymous (`new AbsolutePath(p) {}`),
  // which the pickle records as a local child no declaration stands for: read as a sealed family
  // it would hold no value at all, and path would be vacuous, 1. It is refused, and so is Classpath,
  // a case class of a list of them: each for another reason than a class that is not supplied.
  def scala2Sealed =
    List("s.path", "s.classpath")
      .map(name => refusedThrough(throughScala2(name), aloneScala2(name)))
      .reduce(_ and _)

  // Both units declare `dup.Coin`. The compiler and the report take the first on the classpath, as
  // the JVM loads it: with `two` first Coin has two cases, 2^2 = 4 predicates. With `three` first it
  // has three, 2^3, and the module may name `Edge`, which only that unit declares: three ways to
  // give a Coin.
  def firstWins = {
    val two = Compiled.dependency("two", "package dup\nenum Coin { case Heads, Tails }")
    val three = Compiled.dependency("three", "package dup\nenum Coin { case Heads, Tails, Edge }")
    val use = "package u\ndef f(c: dup.Coin): Boolean = ???"
    val first = Compiled.reportOver(Nil, two, three, "u/U.scala" -> use)
    val other = counts(
      Compiled.reportOver(
        Nil,
        three,
        two,
        "u/U.scala" -> (use + "\ndef edge: dup.Coin = dup.Coin.Edge")
      )
    )
    (counts(first)("u.f") === Count.Finite(4))
      .and(other("u.f") === Count.Finite(8))
      .and(other("u.edge") === Count.Finite(3))
      // The units are no sources of the module: its one row is its own.
      .and(first.methods.map(_.name) === List("u.f"))
      .and(first.definitions must beEmpty)
  }

  // A dependency names the parent the compiler resolved. q.D extends q.S (or q.T), not p's
  // template of that simple name, so D's parent outside the sources, `Iterator`, is not below p.T:
  // nothing may override p.T's m, which is final, t carries nothing, and target ranges over the
  // functions of b, 4. Where q.D does extend p.S, that parent is below p.T and may override m:
  // refused. A source spelling `S` or `T` may mean either template, so with q.D in the module's
  // sources it is refused too.
  def qualified = {
    val use = "u.scala" -> "object U { def target(t: p.T)(b: Boolean): Boolean = ??? }"
    def target(files: (String, String)*) =
      counts(Compiled.reportOver(Nil, (files :+ use)*))("U.target")
    val outside = "member may be overridden through a parent outside the sources"
    val p =
      Compiled.dependency("p", "package p\ntrait T { def m: Boolean = true }\ntrait S extends T")
    val q = "package q\ntrait S\ntrait D extends S with Iterator[Int]"
    val owner = "package q\ntrait T\ntrait D extends T with Iterator[Int]"
    val below = "package q\ntrait D extends p.S with Iterator[Int]"
    (target(p, Compiled.dependency("q", q)) === Count.Finite(4))
      .and(target(p, Compiled.dependency("q", owner)) === Count.Finite(4))
      .and(refused(target(p, Compiled.dependency("q", below)), outside))
      .and(refused(target(p, "q.scala" -> q), outside))
      .and(refused(target(p, "q.scala" -> owner), outside))
  }

  // `java.sql.ClientInfoStatus` is a Java enum of four constants: 2^4 = 16 predicates. `java.awt`'s
  // `List` (java.desktop) is its rooted declaration, a class whose state is not read, and no
  // `declaration not supplied`. Read with no dependency classpath at all, which only the scalameta
  // front end can be, neither module is read.
  def jdkModule = {
    val code =
      "j/J.scala" -> ("package j\nimport java.sql.*\ndef status(s: ClientInfoStatus): Boolean = ???\n" +
        "object A { import java.awt.*; def list(l: List): Boolean = ??? }\n")
    val (read, unread) = (counts(Compiled.reportOver(Nil, code)), counts(Compiled.report(code)))
    (read("j.status") === Count.Finite(16))
      .and(refused(read("j.A.list"), "hidden constructor state: java.awt.List"))
      .and(refused(unread("j.status"), "declaration not supplied"))
      .and(refused(unread("j.A.list"), "declaration not supplied: java.awt.List"))
  }

}
