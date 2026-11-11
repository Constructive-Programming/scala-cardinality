package cardinality.compiled

import scala.util.control.NonFatal

import cardinality.analysis.MethodAnalysis.Entry
import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.capacity.Definition
import cardinality.reporting.{FrontEnd, Report}
import cardinality.tasty.CompiledTypes
import tastyquery.Contexts.*
import tastyquery.Exceptions.MemberNotFoundException
import tastyquery.Symbols.ClassSymbol
import tastyquery.Trees.ClassDef

/** The front end that reads the compiler's symbols and typed trees (TASTy, through tasty-query),
  * and no syntax: docs/plans/compiled-own-types.md, "Front end: design and status".
  *
  * It answers every row the classes hold. A row no slice reads yet is `?` with `Uncovered` and what
  * it needs, never measured another way: this front end calls nothing of the scalameta one.
  */
private[cardinality] object TastyFrontEnd extends FrontEnd {

  /** How a row says that the compiled front end does not read it yet. */
  val Uncovered = "not read from compiled classes yet"

  def rows(module: FrontEnd.Module): FrontEnd.Rows =
    module.environment.classes match {
      case None          => refused(module.sources, "no compiled classes supplied")
      case Some(classes) =>
        loaded(classes) match {
          case Left(why)                       => refused(module.sources, why)
          case Right((context, own, unloaded)) =>
            FrontEnd.Rows(
              read(module.sources, own)(using context),
              unloaded.map(why => s"compiled class not read: $why")
            )
        }
    }

  private def loaded(classes: CompiledTypes.Classes) =
    try Right(CompiledTypes.loaded(classes))
    catch { case NonFatal(error) => Left(s"compiled classes not read: $error") }

  private def refused(sources: List[(String, String)], why: String): FrontEnd.Rows =
    FrontEnd.Rows(sources.map((path, _) => Left(Report.Error(path, why))), Nil)

  private def read(sources: List[(String, String)], own: List[ClassSymbol])(using
      Context
  ): List[Either[Report.Error, Report.Source]] = {
    // A class file is listed under its class's and its object's name: each class once.
    val trees = own.distinct.flatMap(_.tree).groupBy(_.pos.sourceFile.path)
    val world = new World(own.toSet)
    val measure = rowsOf(new Readings(world), new Stored(world))
    // Each compiled path belongs to the source that shares most of it (`Exp.scala` against
    // `lib/Exp.scala`), and a source is read from the one compiled path that belongs to it. Two
    // sources that share as much of a compiled path are not told apart: neither is read from it.
    val owners = trees.keys.toList.groupBy { compiled =>
      val named = sources.map((path, _) => path -> shared(path, compiled)).filter(_._2 > 0)
      named.filter(_._2 == named.map(_._2).max) match {
        case List((path, _)) => Some(path)
        case _               => None
      }
    }
    sources.map { (path, text) =>
      def error(why: String) = Left(Report.Error(path, why))
      owners.getOrElse(Some(path), Nil) match {
        case List(compiled) =>
          // A tree this reading cannot walk costs its source, said, not the report.
          try measure(new Source(path, text), trees(compiled))
          catch { case NonFatal(failure) => error(s"compiled classes not read: $failure") }
        case Nil if owners.getOrElse(None, Nil).exists(shared(path, _) > 0) =>
          error("compiled classes not told apart from another source's")
        // A source that defines nothing has no classes and no rows; one that defines something
        // and has no classes was not compiled.
        case Nil if Defining.findFirstIn(text).isEmpty => Right(Report.Source(path, Nil, Nil))
        case Nil     => error("no compiled classes for this source")
        case several =>
          error(s"compiled from more than one source: ${several.sorted.mkString(", ")}")
      }
    }
  }

  // A keyword that starts a definition; in a comment or a string it counts too, which only keeps
  // the error.
  private val Defining =
    """\b(class|object|trait|enum|def|val|var|type|given|extension|export)\b""".r

  // How much of their paths a source and a compiled path share: the length of the one that ends
  // the other, none when neither does.
  private def shared(path: String, compiled: String): Int = {
    def normal(p: String) = p.replace('\\', '/').stripPrefix("/")
    val (a, b) = (normal(path), normal(compiled))
    if (a == b || a.endsWith("/" + b) || b.endsWith("/" + a)) math.min(a.length, b.length) else 0
  }

  // A type that names a member the classpath does not hold (a class of a jar the module was
  // compiled against and the report is not given) is not read; any other failure is the reading's
  // own and is not hidden as a row to come.
  private def reading[A](attempt: => Either[String, A]): Either[String, A] =
    try attempt
    catch {
      case missing: MemberNotFoundException =>
        Left(s"a member the classpath does not hold: ${missing.name}")
    }

  private def rowsOf(readings: Readings, stored: Stored)(source: Source, trees: List[ClassDef])(
      using Context
  ): Either[Report.Error, Report.Source] = {
    val (methods, definitions, stale) = Targets.of(source, trees)
    if (stale.nonEmpty)
      Left(
        Report.Error(
          source.path,
          s"compiled classes do not match the source: ${stale.mkString(", ")}"
        )
      )
    else
      Right(
        Report.Source(
          source.path,
          definitions.map { defined =>
            val row = reading(stored.row(defined.kind, defined.tree))
            Definition(
              defined.name,
              defined.kind,
              defined.params,
              row.toOption.flatMap(_.size),
              row.fold(why => List(Definition.Unbound.Refusal(s"$Uncovered: $why")), _.unbound),
              defined.tree.pos.startLine + 1
            )
          },
          methods.map { method =>
            Entry(
              source.path,
              method.name,
              source.signature(method.shown),
              method.shown.pos.startLine + 1,
              method.kind,
              reading(readings.method(method.measured))
                .fold(why => Count.Unresolved(List(s"$Uncovered: $why")), identity),
              Nil
            )
          }
        )
      )
  }

}
