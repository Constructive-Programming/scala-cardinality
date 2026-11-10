package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation.Count
import org.specs2.mutable.Specification

/** A measurement does not depend on the caller's stack: a goal chain that grows until the state
  * budget stops it (PolymorphicMemberSpec's `cyc`) refuses with that budget even when asked from a
  * thread with a 256 KiB stack, where the solver's recursion alone would overflow.
  */
class DeepStackSpec extends Specification {

  private val source = """package bounds
    |trait Seesaw[F[_]] { def wrap[A](a: A): F[A]; def unwrap[A](fa: F[A]): A }
    |object U { def cyc[F[_], B](w: Seesaw[F], b: B): F[B] = w.wrap(b) }
    |""".stripMargin

  private def onSmallStack[A](body: => A): A = {
    @volatile var outcome: Option[Either[Throwable, A]] = None
    val thread = new Thread(
      null,
      () =>
        outcome = Some(try Right(body)
        catch { case e: Throwable => Left(e) }),
      "small",
      256L << 10
    )
    thread.start()
    thread.join()
    outcome.get.fold(throw _, identity)
  }

  "a growing goal chain asked from a small stack" should {
    "refuse with the state budget, not overflow" in {
      val found = onSmallStack(
        MethodAnalysis
          .analyze(
            List(MethodAnalysis.Input("Cyc.scala", dialects.Scala3(source).parse[Source].get))
          )
          .find(_.name == "bounds.U.cyc")
          .map(_.count)
      )
      found must beSome(beLike[Count] {
        case Count.Unresolved(reasons) =>
          reasons.exists(_.contains("Inhabitation state budget exceeded")) must beTrue
      })
    }
  }

}
