package millcardinality

import mill.*
import mill.api.Discover
import mill.testkit.{TestRootModule, UnitTester}
import org.specs2.mutable.Specification

class CardinalityModuleSpec extends Specification {

  object build extends TestRootModule {

    object foo extends CardinalityModule {
      def scalaVersion = "3.9.0"
      override def cardinalitySkillFile = skillFile
    }

    lazy val millDiscover = Discover[this.type]
  }

  private lazy val skillFile = os.temp.dir() / "SKILL.md"

  private val fixture = os.Path(sys.props("user.dir")) / "src" / "test" / "resources" / "fixture"

  "cardinalityReport" should {
    "write the module's report: a Boolean and an Option[Boolean] make 2 * 3 = 6" in {
      UnitTester(build, fixture).scoped { eval =>
        val Right(result) = eval(build.foo.cardinalityReport()): @unchecked
        os.read(result.value.path) must contain("6  P.<init>")
      }
    }

    "read a result the source leaves to the compiler: (Boolean, Boolean) makes 4" in {
      UnitTester(build, fixture).scoped { eval =>
        val Right(result) = eval(build.foo.cardinalityReport()): @unchecked
        os.read(result.value.path) must contain("4  Q.pair")
      }
    }
  }

  "cardinalitySeedSkill" should {
    "write the skill with Mill's commands, then refuse a file a user took over" in {
      UnitTester(build, fixture).scoped { eval =>
        val Right(result) = eval(build.foo.cardinalitySeedSkill()): @unchecked
        val seeded = os.read(result.value.path)
        os.write.over(skillFile, "my own skill")
        (seeded must contain("./mill __.cardinalityReport"))
          .and(eval(build.foo.cardinalitySeedSkill()) must beLeft)
          .and(os.read(skillFile) === "my own skill")
      }
    }
  }

}
