package millcardinality

import mill.*
import mill.api.Discover
import mill.testkit.{TestRootModule, UnitTester}
import org.specs2.mutable.Specification

class CardinalityModuleSpec extends Specification {

  object build extends TestRootModule {

    object foo extends CardinalityModule {
      def scalaVersion = "3.9.0"
    }

    lazy val millDiscover = Discover[this.type]
  }

  private val fixture = os.Path(sys.props("user.dir")) / "src" / "test" / "resources" / "fixture"

  "cardinalityReport" should {
    "write the module's report: a Boolean and an Option[Boolean] make 2 * 3 = 6" in {
      UnitTester(build, fixture).scoped { eval =>
        val Right(result) = eval(build.foo.cardinalityReport()): @unchecked
        os.read(result.value.path) must contain("6  P.<init>")
      }
    }
  }

}
