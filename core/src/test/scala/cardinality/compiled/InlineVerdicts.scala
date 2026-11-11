package cardinality.compiled

import scala.jdk.CollectionConverters.*

import cardinality.Compiled
import cardinality.analysis.inhabitation.Inhabitation.Count
import cardinality.reporting.{FrontEnd, Report, ScalametaFrontEnd}
import cardinality.tasty.CompiledTypes
import java.nio.file.{Files, Path}
import tastyquery.Contexts.*
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.Symbols.{ClassSymbol, TermSymbol}
import tastyquery.Traversers.TreeTraverser
import tastyquery.Trees.*

/** The gate of `Inlines` while no row shows it: for every inline method of a module, and every
  * method written inside one, the verdict of the scalameta front end against the verdict of
  * `Inlines`.
  *
  *   - The old verdict is read off the scalameta front end's row for the method
  *     (`ScalametaFrontEnd`, `analysis.expansion`): it refuses the method where a reason of the row
  *     starts with `inline method` or `definition inside an inline method`, or names the
  *     transparent result.
  *   - The new verdict is `Inlines.refusal` for the method's symbol, over the module's compiled
  *     classes (tasty-query). Nothing of it reads a row.
  *
  * A method is `same` where both count or both refuse; `counts` where only the old one refuses
  * (expected for what the compiler resolved and the old reading could not: each is listed under the
  * old reason); `worse` where only the new one refuses, which fails the run; `masked` where the old
  * row counts 1, which the scalameta front end answers for a one-valued result before it reads the
  * body, and the new verdict refuses (listed, to be read by hand); `other` where the old row
  * refuses for another reason and does not show its verdict (listed where the new one refuses);
  * `unpaired` where no old row is at the method's source, line and name (an export's forwarder, a
  * local given).
  *
  * `InlineVerdicts fixtures <cache>` reads the compiled test fixtures, `InlineVerdicts corpus
  * <root>` a frozen corpus (`Differential`'s inputs); `--list` lists every method that counts where
  * the old front end refuses, not one of each reason. Exit 1: a `worse` method, or an input a front
  * end could not read.
  */
object InlineVerdicts {

  enum Verdict { case Same, Counts, Worse, Masked, Other, Unpaired }

  /** One method: where it is, what each front end says of it (`None`: it counts), the old row's
    * other reasons, and whether the old row counts 1.
    */
  final case class Method(
      source: String,
      line: Int,
      name: String,
      old: Option[Option[String]],
      next: Option[String],
      otherwise: List[String],
      one: Boolean
  ) {

    def verdict: Verdict = (old, next) match {
      case (None, _)                             => Verdict.Unpaired
      case (Some(None), _) if otherwise.nonEmpty => Verdict.Other
      case (Some(None), None)                    => Verdict.Same
      case (Some(Some(_)), Some(_))              => Verdict.Same
      case (Some(Some(_)), None)                 => Verdict.Counts
      case (Some(None), Some(_)) if one          => Verdict.Masked
      case (Some(None), Some(_))                 => Verdict.Worse
    }

    def render: String =
      s"$source:$line $name\n    old: ${old.fold("no row")(_.getOrElse("counts"))}" +
        s"\n    new: ${next.getOrElse("counts")}" +
        (if (otherwise.isEmpty) "" else s"\n    old row also: ${otherwise.mkString("; ")}")

  }

  // Whether a reason of an old row is the inline verdict.
  private def inline(reason: String): Boolean =
    reason.startsWith(Inlines.Prefix) || reason.startsWith(Inlines.Inside) ||
      reason.contains("transparent inline")

  /** The methods of one input, compared. */
  def compare(input: Differential.Input): Either[String, List[Method]] = {
    val old = Differential.answer(ScalametaFrontEnd, input.old)
    val unread = old.sources.collect { case Left(error) => s"${error.path}: ${error.message}" }
    if (unread.nonEmpty) Left(s"scalameta front end: ${unread.mkString("; ")}")
    else
      try {
        val classes = input.next.environment.classes.get
        val (context, own, unloaded) = CompiledTypes.loaded(classes)
        given Context = context
        val (dependencies, missed) = CompiledTypes.dependencies(classes)
        // A class that does not load may hold what a verdict turns on.
        if (unloaded.nonEmpty || missed.nonEmpty)
          sys.error(s"classes not loaded: ${(unloaded ++ missed).take(3).mkString("; ")}")
        Right(
          read(old, new Inlines(own.toSet, dependencies), own.distinct)
        )
      } catch {
        case scala.util.control.NonFatal(error) => Left(s"compiled classes: $error")
      }
  }

  private def read(old: FrontEnd.Rows, inlines: Inlines, own: List[ClassSymbol]): List[Method] = {
    val rows = old.sources.collect { case Right(source) => source }
    val found = List.newBuilder[Method]
    own.flatMap(_.tree).foreach { top =>
      new TreeTraverser {
        override def traverse(tree: Tree): Unit = {
          tree match {
            // An inline method, or a method written inside one (no closure, no default).
            case d: DefDef if !d.pos.isUnknown && !d.name.toString.contains("$") && expanded(d) =>
              val (path, line, name) = (d.pos.sourceFile.path, d.pos.startLine + 1, d.name.toString)
              val here = rows
                .filter { source =>
                  Path.of(path).endsWith(source.path) || Path.of(source.path).endsWith(path)
                }
                .flatMap(_.methods)
                .filter(_.line == line)
              // The old row at the method's line named as it is; for a given the compiler named,
              // the one row of the line.
              val row = here
                .find(_.name.split('.').last == name)
                .orElse(Some(here).collect {
                  case List(only) => only
                })
              val reasons = row.toList.flatMap(_.count match {
                case Count.Unresolved(all) => all
                case _                     => Nil
              })
              found += Method(
                path,
                line,
                name,
                row.map(_ => reasons.find(inline)),
                inlines.refusal(d.symbol),
                reasons.filterNot(inline),
                row.exists(_.count == Count.Finite(1))
              )
            case _ => ()
          }
          super.traverse(tree)
        }
      }.traverse(top)
    }
    found.result()
  }

  private def expanded(d: DefDef): Boolean =
    d.symbol.kind == TermSymbolKind.Def && !World.isConstructor(d.symbol) &&
      World.owners(d.symbol).exists {
        case around: TermSymbol => around.isInline && around.kind == TermSymbolKind.Def
        case _                  => false
      }

  // Why the new reading is expected to count where an old reason refuses: the decision the reason
  // falls under, by its words.
  private def expected(reason: String): String =
    if (List("may expand", "may answer", "is not resolved)").exists(reason.contains))
      "D2, a conversion or extension the body does not apply"
    else if (
      List("implicit search", "resolves a given", "context function").exists(reason.contains)
    )
      "D4, a given the compiler resolved"
    else "a name or type the scalameta reading does not resolve"

  // What an old reason is about, without the names it quotes: the head of its first clause.
  private def kind(reason: String): String =
    reason
      .stripPrefix(Inlines.Prefix)
      .stripPrefix(":")
      .trim
      .split("[,:(]")
      .head
      .split(' ')
      .take(4)
      .mkString(" ")

  // A fixture with its dependency units (`cardinality.Compiled.dependency`) as its test reads it:
  // their classes on the classpath, and their sources no source of the module.
  private def overUnits(cache: Path, libraries: List[Path])(
      input: Differential.Input
  ): Differential.Input = {
    val kept = cache.resolve(input.label).resolve(Compiled.Dependency)
    if (!Files.isDirectory(kept)) input
    else {
      val listed = Files.list(kept)
      val units =
        try listed.iterator().asScala.toList.sortBy(_.toString)
        finally listed.close()
      def over(module: FrontEnd.Module) = FrontEnd.Module(
        module.sources.filterNot(_._1.startsWith(Compiled.Dependency)),
        Report.Environment(
          units ++ libraries,
          classes = module.environment.classes.map(c => c.copy(classpath = units ++ c.classpath))
        )
      )
      Differential.Input(input.label, over(input.old), over(input.next))
    }
  }

  def main(args: Array[String]): Unit = {
    val libraries = sys
      .props("java.class.path")
      .split(java.io.File.pathSeparator)
      .toList
      .map(Path.of(_))
      .filter { p =>
        val name = p.getFileName.toString
        name.startsWith("scala-library") || name.startsWith("scala3-library")
      }
    val inputs = args.toList match {
      case "corpus" :: root :: _    => List(Differential.corpus(Path.of(root)))
      case "fixtures" :: cache :: _ =>
        Differential.fixtures(Path.of(cache), libraries).map(overUnits(Path.of(cache), libraries))
      case _ => sys.error("usage: InlineVerdicts (corpus <root> | fixtures <cache>) [--list]")
    }
    // A module without the word has no inline method.
    val withInline = inputs.filter(_.old.sources.exists(_._2.contains("inline")))
    val compared = withInline.map(input => input.label -> compare(input))
    val failed = compared.collect { case (label, Left(why)) => s"$label $why" }
    val methods = compared.collect { case (label, Right(found)) => found.map(label -> _) }.flatten
    val by = methods.groupBy(_._2.verdict)
    def of(verdict: Verdict) = by.getOrElse(verdict, Nil)
    println("old: ScalametaFrontEnd's rows (analysis.expansion); new: compiled.Inlines")
    println(
      s"${inputs.size} inputs, ${withInline.size} with `inline`, ${methods.size} methods: " +
        Verdict.values.map(v => s"${v.toString.toLowerCase} ${of(v).size}").mkString(", ")
    )
    println(
      s"  same: ${of(Verdict.Same).count(_._2.next.isEmpty)} count on both sides, " +
        s"${of(Verdict.Same).count(_._2.next.nonEmpty)} refuse on both"
    )
    println(
      s"  other: ${of(Verdict.Other).count(_._2.next.isEmpty)} count on the new side, " +
        s"${of(Verdict.Other).count(_._2.next.nonEmpty)} refuse (listed)"
    )
    println("counts where the old front end refuses:")
    of(Verdict.Counts)
      .groupBy((_, method) => expected(method.old.flatten.getOrElse("")))
      .toList
      .sortBy(_._1)
      .foreach { (decision, under) =>
        println(s"  ${under.size}  $decision")
        under
          .groupBy((_, method) => kind(method.old.flatten.getOrElse("")))
          .toList
          .sortBy(-_._2.size)
          .foreach { (reason, listed) =>
            println(s"    ${listed.size}  $reason")
            val shown = if (args.contains("--list")) listed else listed.take(1)
            shown.foreach((label, method) => println(s"      $label${method.render}"))
          }
      }
    List(Verdict.Worse, Verdict.Masked, Verdict.Other, Verdict.Unpaired).foreach { verdict =>
      // An `other` method the new side counts shows nothing to read by hand.
      val listed = of(verdict).filter((_, m) => verdict != Verdict.Other || m.next.nonEmpty)
      if (listed.nonEmpty) {
        println(s"${verdict.toString.toLowerCase}:")
        listed.foreach((label, method) => println(s"  $label${method.render}"))
      }
    }
    failed.foreach(why => println(s"not read: $why"))
    if (of(Verdict.Worse).nonEmpty || failed.nonEmpty) sys.exit(1)
  }

}
