package cardinality.compiled

import scala.util.control.NonFatal

import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.capacity.Definition
import cardinality.reporting.Report
import cardinality.reporting.Report.Entry
import cardinality.tasty.CompiledClasses
import tastyquery.Contexts.*
import tastyquery.Exceptions.MemberNotFoundException
import tastyquery.Symbols.ClassSymbol
import tastyquery.Trees.ClassDef

/** The front end that reads the compiler's symbols and typed trees (TASTy, through tasty-query),
  * and no syntax: docs/plans/compiled-own-types.md, "Front end: design and status".
  *
  * It answers every row the classes hold. A row no slice reads yet is `?` with `Uncovered` and what
  * it needs, never measured another way.
  */
private[cardinality] object TastyFrontEnd {

  /** How a row says that the compiled front end does not read it yet. */
  val Uncovered = "not read from compiled classes yet"

  /** Why a source that defines something and has no compiled classes is not read. */
  val Uncompiled = "no compiled classes for this source"

  /** One answer per source, in the order given: its rows, or why it could not be read. `warnings`
    * lists what of the compiled classes could not be read.
    */
  final case class Rows(sources: List[Either[Report.Error, Report.Source]], warnings: List[String])

  /** The rows of each source (its path and text), read from `classes`. */
  def rows(sources: List[(String, String)], classes: Option[CompiledClasses]): Rows =
    classes match {
      case None          => refused(sources, "no compiled classes supplied")
      case Some(classes) =>
        loaded(classes) match {
          case Left(why)                       => refused(sources, why)
          case Right((context, own, unloaded)) =>
            given Context = context
            // The classpath's classes are listed only where a reading needs them (an inline body
            // selecting a member of one, `Inlines`; a question about a class outside the module,
            // `ClosedProgram`): a class that does not load then is a warning of the report.
            var missed = List.empty[String]
            lazy val dependencies = {
              val (found, failed) = CompiledClasses.dependencies(classes)
              // A class tasty-query refuses to enter twice (a second jar of the standard library
              // declaring `scala.caps` again) is the class the context already holds: not lost.
              missed = failed.filterNot(_.contains("trying to add a second entry"))
              found
            }
            // The closed world asks for the same listing, and for the classpath's classes no
            // symbol stands for, only where a question is about a class outside the module.
            val closed = new ClosedProgram(
              own.distinct,
              unloaded,
              (dependencies, missed),
              CompiledClasses.unnamed(classes.output),
              CompiledClasses.unnamed(classes.classpath)
            )
            val answers =
              read(
                sources,
                own,
                new Inlines(own.toSet, dependencies),
                closed,
                // A class file is read only for a class without TASTy that a type names.
                {
                  lazy val facts = CompiledClasses.javaFacts(classes)
                  new DependencyClasses(c => facts(c))
                }
              )
            Rows(
              answers,
              (unloaded ++ missed).map(why => s"compiled class not read: $why")
            )
        }
    }

  private def loaded(classes: CompiledClasses) =
    try Right(CompiledClasses.loaded(classes))
    catch { case NonFatal(error) => Left(s"compiled classes not read: $error") }

  private def refused(sources: List[(String, String)], why: String): Rows =
    Rows(sources.map((path, _) => Left(Report.Error(path, why))), Nil)

  private def read(
      sources: List[(String, String)],
      own: List[ClassSymbol],
      inlines: Inlines,
      closed: ClosedProgram,
      dependencyClasses: DependencyClasses
  )(using Context): List[Either[Report.Error, Report.Source]] = {
    // A class file is listed under its class's and its object's name: each class once.
    val trees = own.distinct.flatMap(_.tree).groupBy(_.pos.sourceFile.path)
    val world =
      new World(own.toSet, closed, dependencyClasses)
    val stored = new Stored(world)
    val measure = rowsOf(new Readings(world, stored, inlines), stored)
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
        case Nil                                       => error(Uncompiled)
        case several                                   =>
          error(s"compiled from more than one source: ${several.sorted.mkString(", ")}")
      }
    }
  }

  // A keyword that starts a definition; in a comment or a string it counts too, which only keeps
  // the error.
  private val Defining =
    """\b(class|object|trait|enum|def|val|var|type|given|extension|export)\b""".r

  // How much of their paths a source and a compiled path share: the length of the one that ends
  // the other, none when neither does; one more for the path itself, which comes before a source
  // it merely ends (`Exp.scala` before `lib/Exp.scala`).
  private def shared(path: String, compiled: String): Int = {
    def normal(p: String) = p.replace('\\', '/').stripPrefix("/")
    val (a, b) = (normal(path), normal(compiled))
    if (a == b) a.length + 1
    else if (a.endsWith("/" + b) || b.endsWith("/" + a)) math.min(a.length, b.length)
    else 0
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
              row
                .fold(why => List(Definition.Unbound.Refusal(s"$Uncovered: $why")), _.unbound)
                .map(spelled),
              defined.tree.pos.startLine + 1
            )
          },
          methods.map { method =>
            val read = reading(readings.method(method.measured))
            Entry(
              source.path,
              method.name,
              source.signature(method.shown),
              method.shown.pos.startLine + 1,
              method.kind,
              spelled(read.fold(why => Count.Unresolved(List(s"$Uncovered: $why")), _._1)),
              read.fold(_ => Nil, _._2)
            )
          }
        )
      )
  }

  // A reason names what it quotes as the source would (`Source.spelled`).
  private def spelled(count: Count): Count = count match {
    case Count.Unresolved(reasons) => Count.Unresolved(reasons.map(Source.spelled))
    case other                     => other
  }

  private def spelled(unbound: Definition.Unbound): Definition.Unbound = {
    import Definition.Unbound.*
    unbound match {
      case m: Member     => m.copy(name = Source.spelled(m.name))
      case Open(name)    => Open(Source.spelled(name))
      case Unknown(name) => Unknown(Source.spelled(name))
      case Syntax(what)  => Syntax(Source.spelled(what))
      case Refusal(what) => Refusal(Source.spelled(what))
      case other         => other
    }
  }

}
