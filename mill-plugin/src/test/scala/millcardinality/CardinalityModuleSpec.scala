package millcardinality

import cardinality.reporting.Report
import mainargs.Leftover
import mill.*
import mill.api.Discover
import mill.testkit.{TestRootModule, UnitTester}
import org.specs2.mutable.Specification

class CardinalityModuleSpec extends Specification {

  object build extends TestRootModule {

    object foo extends CardinalityModule {
      def scalaVersion = "3.9.0"
    }

    // `foo`'s sources, read by the TASTy front end.
    object bar extends CardinalityModule {
      def scalaVersion = "3.9.0"
      override def sources = foo.sources
      override def cardinalityFrontEnd = Report.Front.Tasty
    }

    lazy val millDiscover = Discover[this.type]
  }

  // A root of its own, in a directory of its own: testers of one root copy the fixture into the
  // same directory, and concurrent examples would collide there.
  object seeding extends TestRootModule {

    object foo extends CardinalityModule {
      def scalaVersion = "3.9.0"
      override def cardinalitySkillFile = skillFile
    }

    lazy val millDiscover = Discover[this.type]
  }

  private lazy val skillFile = os.temp.dir() / "SKILL.md"

  private val fixture = os.Path(sys.props("user.dir")) / "src" / "test" / "resources" / "fixture"

  "cardinalityReport" should {
    "write the module's report: Boolean × Option[Boolean] = 6; the compiler's (Boolean, Boolean) = 4, with either front end" in {
      UnitTester(build, fixture).scoped { eval =>
        def text(report: Command[PathRef]) = {
          val Right(result) = eval(report): @unchecked
          os.read(result.value.path)
        }
        def counted(text: String) =
          (text must contain("6  P.<init>")).and(text must contain("4  Q.pair"))
        // Only the TASTy front end refuses a source without compiled classes.
        val uncompiled = Leftover((fixture / "foo" / "src" / "P.scala").toString)
        counted(text(build.foo.cardinalityReport()))
          .and(counted(text(build.bar.cardinalityReport())))
          .and(eval(build.foo.cardinalityReportOf(uncompiled)) must beRight)
          .and(eval(build.bar.cardinalityReportOf(uncompiled)) must beLeft)
      }
    }
  }

  "cardinalitySeedSkill" should {
    "write the skill with Mill's commands, then refuse a file a user took over" in {
      UnitTester(seeding, fixture).scoped { eval =>
        val Right(result) = eval(seeding.foo.cardinalitySeedSkill()): @unchecked
        val seeded = os.read(result.value.path)
        os.write.over(skillFile, "my own skill")
        (seeded must contain("./mill __.cardinalityReport"))
          .and(eval(seeding.foo.cardinalitySeedSkill()) must beLeft)
          .and(os.read(skillFile) === "my own skill")
      }
    }
  }

}
