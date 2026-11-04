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
}
