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

    object old extends CardinalityModule {
      def scalaVersion = "2.13.16"
      override def moduleDir = foo.moduleDir
    }

    lazy val millDiscover = Discover[this.type]
  }

  private val fixture = os.Path(sys.props("user.dir")) / "src" / "test" / "resources" / "fixture"

  // One testkit session for both modules: a session sets up `build`'s directory and two cannot
  // share it.
  "cardinalityReport" should {
    "write a module's report (a Boolean and an Option[Boolean] make 2 * 3 = 6), on the calculator's " +
      "Scala or another: its jars are resolved apart from the module" in {
        UnitTester(build, fixture).scoped { eval =>
          val Right(same) = eval(build.foo.cardinalityReport()): @unchecked
          val Right(other) = eval(build.old.cardinalityReport()): @unchecked
          (os.read(same.value.path) must contain("6  P.<init>"))
            .and(os.read(other.value.path) must contain("6  P.<init>"))
        }
      }
  }

}
