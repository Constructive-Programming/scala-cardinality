package cardinality

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}
import org.specs2.execute.Details
import org.specs2.reporter.Notifier

/** One line for each example a run executes, appended to the file the system property
  * `cardinality.examples` names: the specification, the example (under the blocks around it, and
  * numbered where a specification repeats a description), `pass`, `fail`, `pending` or `skipped`,
  * and the first line of what failed or is pending. `scripts/spec-ledger.sh` runs the suite with
  * this as specs2's `notifier` and reads the files: the build's JUnit reports drop the examples of
  * some specifications.
  */
class ExampleLog extends Notifier {

  private val file = Path.of(sys.props("cardinality.examples"))
  private var specification = ""
  private var blocks = List.empty[String]
  private var seen = Map.empty[String, Int]

  private def line(example: String, outcome: String, message: String): Unit = {
    // A description is laid out by its specification's text; a message keeps its own spaces.
    val described = (example :: blocks).reverse.mkString(" ").replaceAll("\\s+", " ").trim
    val count = seen.getOrElse(described, 0) + 1
    seen = seen.updated(described, count)
    val name = if (count == 1) described else s"$described #$count"
    val first = message.linesIterator.nextOption().getOrElse("").replace('\t', ' ')
    val row = List(specification, name, outcome, first)
    classOf[ExampleLog].synchronized {
      val _ = Files.writeString(
        file,
        row.mkString("", "\t", "\n"),
        UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    }
  }

  def specStart(title: String, location: String): Unit = {
    specification = title
    blocks = Nil
    seen = Map.empty
  }

  def specEnd(title: String, location: String): Unit = ()
  def contextStart(text: String, location: String): Unit = blocks = text :: blocks
  def contextEnd(text: String, location: String): Unit = blocks = blocks.drop(1)
  def text(text: String, location: String): Unit = ()
  def exampleStarted(name: String, location: String): Unit = ()
  def exampleSuccess(name: String, duration: Long): Unit = line(name, "pass", "")

  def exampleFailure(
      name: String,
      message: String,
      location: String,
      f: Throwable,
      details: Details,
      duration: Long
  ): Unit = line(name, "fail", message)

  def exampleError(
      name: String,
      message: String,
      location: String,
      f: Throwable,
      duration: Long
  ): Unit = line(name, "fail", message)

  def exampleSkipped(name: String, message: String, location: String, duration: Long): Unit =
    line(name, "skipped", message)

  def examplePending(name: String, message: String, location: String, duration: Long): Unit =
    line(name, "pending", message)

  def stepStarted(location: String): Unit = ()
  def stepSuccess(duration: Long): Unit = ()

  // A step that fails stops its specification: said as an example of it, so the run shows it.
  def stepError(message: String, location: String, f: Throwable, duration: Long): Unit =
    line("(step)", "fail", message)

}
