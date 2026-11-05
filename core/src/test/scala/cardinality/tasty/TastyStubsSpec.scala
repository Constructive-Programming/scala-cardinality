package cardinality.tasty

import scala.meta.*

import cardinality.analysis.MethodAnalysis
import cardinality.analysis.inhabitation.Inhabitation
import cardinality.capacity.{Counter, Library}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import org.specs2.mutable.Specification

/** Compiled declarations read back as stubs (tastyfixtures/Fixtures.scala, from its TASTy).
  *
  * The differential check: a target read through the stubs counts what it counts through the
  * fixtures' own source, or refuses - a stub never supplies a different number. And the stubs carry
  * information: without them the same targets refuse as unsupplied.
  */
class TastyStubsSpec extends Specification {

  private def jarOf(c: Class[?]): Path =
    Path.of(c.getProtectionDomain.getCodeSource.getLocation.toURI)

  private val fixtures = jarOf(classOf[tastyfixtures.Endo[?]])

  private lazy val stubs = TastyStubs.of(
    List(jarOf(classOf[Option[?]]), jarOf(classOf[scala.deriving.Mirror]), fixtures).distinct,
    _ == fixtures,
    _ == "tastyfixtures"
  )

  private lazy val fixtureSource: Source = {
    val relative = Path.of("core/src/test/scala/tastyfixtures/Fixtures.scala")
    val file = Iterator
      .iterate(Path.of("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(relative))
      .find(Files.exists(_))
      .get
    dialects.Scala3(Files.readString(file, UTF_8)).parse[Source].get
  }

  private val targets =
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
      |def ident(b: Boolean): Boolean = Concrete.identity(b)
      |inline def picked[A](x: A, y: A): A = Inlines.pick(x, y)
      |object U { val k: Built = ???; val p: Pair = ???; val l: Light = ??? }
      |""".stripMargin

  private val target =
    MethodAnalysis.Input("t/T.scala", dialects.Scala3(targets).parse[Source].get)

  private def methods(environment: List[MethodAnalysis.Input]): Map[String, Inhabitation.Count] =
    MethodAnalysis
      .analyze(target :: environment)
      .filter(_.path == target.path)
      .map(e => e.name -> e.count)
      .toMap

  private def values(environment: List[Source]): Map[String, String] = {
    val library = Library.of(target.tree :: environment)
    Counter.definitions(target.tree, library).map(d => d.name -> d.toString).toMap
  }

  private lazy val throughStubs = methods(stubs.inputs)
  private lazy val throughSource = methods(List(MethodAnalysis.Input("f/F.scala", fixtureSource)))
  private lazy val unsupplied = methods(Nil)

  private def refused(c: Inhabitation.Count): Boolean =
    c.isInstanceOf[Inhabitation.Count.Unresolved]

  "every fixture class renders to a stub that parses" in {
    (stubs.errors must beEmpty).and(
      stubs.inputs.map(_.path) must contain(
        s"tasty/${fixtures.getFileName}/core/src/test/scala/tastyfixtures/Fixtures.scala"
      )
    )
  }

  "a stub never counts differently from the source it was compiled from" in {
    val differing = throughStubs.collect {
      case (name, count) if !refused(count) && throughSource.get(name).exists(_ != count) =>
        s"$name: stubs $count, source ${throughSource(name)}"
    }
    (throughStubs.keySet must_== throughSource.keySet).and(differing must beEmpty)
  }

  "the stubs supply what the unsupplied environment cannot" in {
    List("t.pair", "t.mkPair", "t.light", "t.suit")
      .map { name =>
        (refused(unsupplied(name)) must beTrue).and(throughStubs(name) must_== throughSource(name))
      }
      .reduce(_ and _)
  }

  "a call into a compiled inline body refuses: the body is not read" in {
    throughStubs("t.picked") must beLike {
      case u: Inhabitation.Count.Unresolved =>
        u.reasons.mkString must contain("compiledBody")
    }
  }

  "stored values read through the stubs agree with the source, or refuse" in {
    val stubbed = values(stubs.inputs.map(_.tree))
    val sourced = values(List(fixtureSource))
    List("t.U.k", "t.U.p", "t.U.l")
      .map { name =>
        val s = stubbed.getOrElse(name, "")
        (s.contains("None") || s == sourced.getOrElse(name, "")) must beTrue.setMessage(
          s"$name: stubs $s, source ${sourced.getOrElse(name, "")}"
        )
      }
      .reduce(_ and _)
  }

  // Java declarations (javafixtures/*.java, from their .class files).

  private lazy val javaStubs = TastyStubs.of(
    List(jarOf(classOf[Option[?]]), jarOf(classOf[scala.deriving.Mirror]), fixtures).distinct,
    _ == fixtures,
    _ == "javafixtures"
  )

  private val javaTargets =
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
      |""".stripMargin

  private val javaTarget =
    MethodAnalysis.Input("j/J.scala", dialects.Scala3(javaTargets).parse[Source].get)

  private def javaMethods(environment: List[MethodAnalysis.Input]) =
    MethodAnalysis
      .analyze(javaTarget :: environment)
      .filter(_.path == javaTarget.path)
      .map(e => e.name -> e.count)
      .toMap

  private lazy val throughJava = javaMethods(javaStubs.inputs)

  private def javaStub(name: String): String =
    javaStubs.inputs.find(_.path.endsWith(s"javafixtures/$name.scala")).fold("")(_.tree.syntax)

  "every Java fixture class renders to a stub that parses" in {
    (javaStubs.errors must beEmpty).and(
      javaStubs.inputs.map(_.path.split('/').last).sorted must_==
        List("Box.scala", "Choice.scala", "Coin.scala", "Op.scala", "Shape.scala", "Toggle.scala")
    )
  }

  // A Java enum is exactly its constants: 2 values, so 2^2 maps to or from Boolean. A static field
  // of the enum's type (`Coin.DEFAULT`) is no constant, and constant bodies (`Op`) add none.
  "a Java enum is the closed set of its constants" in {
    List("j.coin", "j.pickCoin", "j.op", "j.size")
      .map(name => throughJava(name) must_== Inhabitation.Count.Finite(4))
      .reduce(_ and _)
      .and(refused(javaMethods(Nil)("j.coin")) must beTrue)
  }

  // An interface has no state: its value is the record of its abstract `flip` and its overridable
  // default `same`, each Boolean => Boolean (4 behaviors), so 16 values, and 2^16 maps to Boolean.
  "a Java interface is the record of its overridable methods" in {
    (throughJava("j.mkToggle") must_== Inhabitation.Count.Finite(16))
      .and(throughJava("j.toggle") must_== Inhabitation.Count.Finite(65536))
  }

  // A class's constructors set state no stub ties to their parameters; a sealed interface's
  // permitted subclasses are not read. Each refuses rather than counting what it cannot see.
  "a Java class's state and a sealed interface's subclasses are not read" in {
    List("j.point", "j.box", "j.shape", "j.choice", "j.mkChoice")
      .map(name =>
        refused(throughJava(name)) must beTrue.setMessage(s"$name: ${throughJava(name)}")
      )
      .reduce(_ and _)
      .and(throughJava("j.box").toString must contain("hidden constructor state"))
  }

  "Java signatures: statics in the companion, Object as Any, wildcards spelled, raw types unread" in {
    val box = javaStub("Box")
    (box must contain("open class Box[A](compiledStateNotRead: _root_.SignatureNotRead)"))
      .and(box must contain("def any(x$0: scala.Any): scala.Any"))
      .and(box must contain("def wild(): java.util.List[? <: A]"))
      .and(box must contain("def raw: _root_.SignatureNotRead"))
      .and(box must contain("def of[B](x$0: B): javafixtures.Box[B]"))
      .and(javaStub("Toggle") must contain("final val LIMIT: scala.Int"))
      .and(javaStub("Coin") must not(contain("case DEFAULT")))
      .and(
        javaStub("Shape") must contain("extends Box[java.lang.Boolean](_root_.compiledBodyNotRead)")
      )
  }

  "a jar tasty-query cannot load renders nothing and says why, instead of failing the read" in {
    // `Box.class` alone: its `InnerClasses` attribute names `Box$Point`, which the jar lacks (as a
    // shaded jar can), so loading the jar's symbols throws.
    val broken = Files.createTempDirectory("broken").resolve("broken.jar")
    val out = new java.util.jar.JarOutputStream(Files.newOutputStream(broken))
    out.putNextEntry(new java.util.jar.JarEntry("javafixtures/Box.class"))
    out.write(getClass.getResourceAsStream("/javafixtures/Box.class").readAllBytes())
    out.close()
    val rendered = TastyStubs.render(
      List(jarOf(classOf[Option[?]]), jarOf(classOf[scala.deriving.Mirror]), broken).distinct,
      _ == broken
    )
    (rendered.stubs.map(_._1).filter(_.contains("broken.jar")) must beEmpty)
      .and(rendered.errors.mkString must contain("broken.jar"))
  }

  "a class two jars declare is read from the first, as the JVM loads it" in {
    val library = List(jarOf(classOf[Option[?]]), jarOf(classOf[scala.deriving.Mirror])).distinct
    val directory = Files.createTempDirectory("shadowed")
    val List(first, second) = List("first.jar", "second.jar").map { name =>
      Files.copy(fixtures, directory.resolve(name))
    }: @unchecked
    val alone = TastyStubs.render(library :+ first, _ == first)
    val both = TastyStubs.render(library ++ List(first, second), Set(first, second))
    (both.errors must beEmpty)
      .and(both.stubs must_== alone.stubs)
      .and(
        alone.stubs.map(_._1).filterNot(_.startsWith("tasty/java.base/")) must
          contain(contain("first.jar/")).forall
      )
  }

  "a TASTy file declares its own package, not the directory a shaded jar moved it to" in {
    val option = classOf[Option[?]].getResourceAsStream("/scala/Option.tasty").readAllBytes()
    (TastyNames.declares(option, "scala") must beTrue)
      .and(TastyNames.declares(option, "scala.meta.shaded.internal.scala") must beFalse)
  }
}
