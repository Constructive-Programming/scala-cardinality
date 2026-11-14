package cardinality.tasty

import scala.annotation.unused
import scala.meta.*

import cardinality.analysis.MethodAnalysis
import java.nio.file.Path

/** Native stand-in for the JVM `TastyStubs` (spike). tasty-query has no Scala Native build and a
  * native binary holds no JDK, so nothing is rendered here: a classpath is reported as not read,
  * and the JDK packages a source names are not supplied. A stub file rendered on the JVM is still
  * read and parsed.
  */
object TastyStubs {

  val Prefix = "tasty/"

  final case class Result(inputs: List[MethodAnalysis.Input], errors: List[String])

  final case class Rendered(stubs: List[(String, String)], errors: List[String] = Nil)

  private val unread = "dependency classpath not read: the native build has no TASTy reader"

  def of(
      classpath: List[Path],
      @unused rendered: Path => Boolean,
      @unused packages: String => Boolean = _ => true,
      @unused sources: Iterable[String] = Nil
  ): Result = Result(Nil, if (classpath.isEmpty) Nil else List(unread))

  def render(classpath: List[Path], @unused selected: Path => Boolean): Rendered =
    Rendered(Nil, if (classpath.isEmpty) Nil else List(unread))

  def jdkFor(
      @unused sources: Iterable[String],
      @unused present: List[MethodAnalysis.Input]
  ): Result =
    Result(Nil, Nil)

  def parse(rendered: Rendered): Result = {
    val parsed = rendered.stubs.map((path, text) =>
      (path, text, summon[parsers.Parse[Source]].apply(Input.String(text), dialects.Scala3))
    )
    Result(
      parsed.collect { case (path, _, Parsed.Success(tree)) => MethodAnalysis.Input(path, tree) },
      parsed.collect {
        case (path, text, e: Parsed.Error) =>
          val line = text.linesIterator.drop(e.pos.startLine).nextOption().getOrElse("")
          s"$path:${e.pos.startLine + 1}: ${e.message}: $line"
      } ++ rendered.errors
    )
  }

}
