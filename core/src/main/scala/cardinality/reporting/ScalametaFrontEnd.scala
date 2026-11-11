package cardinality.reporting

import scala.meta.{Source as ScalaSource, *}

import cardinality.analysis.{Measuring, MethodAnalysis}
import cardinality.capacity.{Counter, Library}
import cardinality.tasty.{CompiledTypes, StubFile, TastyStubs}
import java.nio.file.Path

/** The front end that reads the sources' syntax (scalameta): each source is parsed, the compiler's
  * types are written into it where the build supplies its classes (`CompiledTypes`), and the
  * dependencies join as signature stubs (`TastyStubs`).
  */
private[cardinality] object ScalametaFrontEnd extends FrontEnd {

  def rows(module: FrontEnd.Module): FrontEnd.Rows = {
    import module.{environment, sources}
    val parsed = sources.map(parse)
    val (inputs, unread) =
      ascribed(parsed.collect { case Right(input) => input }, environment.classes)
    val stubs = dependencies(environment, sources.map(_._2))
    // One library for the whole run: every source is read against the others, so a reference can
    // leave its file.
    val library = Library.of((inputs ++ stubs.inputs).map(_.tree))
    val methods =
      Measuring.rows(inputs, stubs.inputs, library = Some(library)).groupBy(_.path)
    val measured = inputs.iterator.map(input =>
      Report.Source(
        input.path,
        Counter.definitions(input.tree, library),
        methods.getOrElse(input.path, Nil)
      )
    )
    // One measured source per parsed one, in order: the ascription keeps every input in place.
    FrontEnd.Rows(
      parsed.map(_.map(_ => measured.next())),
      Report.unsupplied(stubs.errors) ++ unread
    )
  }

  /** The inputs with the compiler's types written in, and what of `classes` could not be read. */
  private[reporting] def ascribed(
      inputs: List[MethodAnalysis.Input],
      classes: Option[CompiledTypes.Classes]
  ): (List[MethodAnalysis.Input], List[String]) =
    classes.fold((inputs, Nil))(CompiledTypes.ascribe(inputs, _))

  /** The dependency stubs `classpath` renders. Every jar and the JDK are rendered, except what the
    * stdlib model reads, so the Scala library alone yields only the JDK's. A classpath that cannot
    * be read is a warning: its names stay unsupplied, which only refuses more. A stub file is
    * rendered without the sources: the JDK packages outside `java.base` they name render beside it.
    */
  private[reporting] def dependencies(
      environment: Report.Environment,
      sources: List[String]
  ): TastyStubs.Result =
    environment.stubs.fold(rendered(environment.classpath, sources)) { file =>
      val read = TastyStubs.parse(StubFile.read(file))
      val named = TastyStubs.jdkFor(sources, read.inputs)
      TastyStubs.Result(read.inputs ++ named.inputs, read.errors ++ named.errors)
    }

  private def rendered(classpath: Seq[Path], sources: List[String]): TastyStubs.Result =
    if (classpath.isEmpty) TastyStubs.Result(Nil, Nil)
    else
      try TastyStubs.of(classpath.toList.distinct, _ => true, sources = sources)
      catch {
        case scala.util.control.NonFatal(error) =>
          TastyStubs.Result(Nil, List(s"dependency classpath not read: $error"))
      }

  /** One source parsed: what it defines, or what stopped the parser. */
  private[reporting] def parse(
      source: (String, String)
  ): Either[Report.Error, MethodAnalysis.Input] = {
    val (path, text) = source
    dialects.Scala3(text).parse[ScalaSource].toEither match {
      case Right(tree) => Right(MethodAnalysis.Input(path, tree))
      case Left(error) =>
        Left(Report.Error(path, s"${error.message} (line ${error.pos.startLine + 1})"))
    }
  }

}
