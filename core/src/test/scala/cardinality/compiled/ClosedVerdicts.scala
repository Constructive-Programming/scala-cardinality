package cardinality.compiled

import scala.meta.{Decl, Defn, Init, Tree as SourceTree, XtensionCollectionLikeUI}

import cardinality.analysis.ClosedWorld
import cardinality.analysis.model.MemberSyntax
import cardinality.compiled.ClosedProgram.Overriding
import cardinality.reporting.ScalametaInputs
import cardinality.tasty.CompiledTypes
import cardinality.types.TypeApplications
import java.nio.file.Path
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Trees.*

/** The gate of `ClosedProgram` while no row shows it: for every class and trait of a module and
  * every member one defines, the answer of the scalameta front end's closed world against the
  * answer of `ClosedProgram`.
  *
  *   - The old answer is `analysis.ClosedWorld` asked by name, over the inputs the scalameta front
  *     end reads (its sources with the compiled types written in, and the dependency stubs):
  *     `extended` for a class, `overriding` for a member. These decide the `final`s it writes.
  *   - The new answer is `ClosedProgram` asked by symbol, over the module's compiled classes and
  *     its dependency classpath: `subclasses` for a class, `overriding` for a member.
  *
  * A class is paired by its source, the line of its name and its name; a member by those of its
  * class and its own. An answer is `same`; `finer` where the old one is open and the new one
  * closed, or the old one does not know and the new one does, each filed under a fact of the old
  * reading's inputs that explains it (another template or alias of the class's simple name, another
  * template that defines the member's name, a parent with no declaration) or under `unexplained`;
  * `opener` where the new one finds a subclass or an overrider the old one does not, each to be
  * read by hand; `coarser` where the new one does not know and the old one does; `unpaired` where
  * only one side has the class or member at that place (the old side lists the members of the
  * classes and traits its sources write, the new one every class of the trees).
  *
  * `ClosedVerdicts fixtures <cache>` reads the compiled test fixtures, `ClosedVerdicts corpus
  * <root>` a frozen corpus; `--list` lists every answer that is not the same. For one input it
  * prints what the index costs: the module's part alone, then with the dependencies. Exit 1: a
  * `coarser` or an `unexplained` answer, or an input that could not be read.
  */
object ClosedVerdicts {

  /** Where a class or member is: its source, the line of its name, its class and its own name
    * (empty for the class itself).
    */
  final case class Key(source: String, line: Int, owner: String, member: String) {
    def render: String = s"$source:${line + 1} $owner${if (member.isEmpty) "" else s".$member"}"
  }

  /** What the scalameta reading cannot tell apart: how many templates and aliases of its inputs
    * (sources and stubs) have each simple name, how many templates define each member or type name,
    * and the simple names its parent clauses spell.
    */
  final case class Names(
      templates: Map[String, Int],
      members: Map[String, Int],
      parents: Set[String]
  )

  // The old side: each class or trait and each member it defines, with the closed world's answer.
  private def old(
      input: Differential.Input
  ): (Map[(Int, String, String), List[(Key, Overriding)]], Names) = {
    val (sources, stubs) = ScalametaInputs.of(input.old)
    val world = new ClosedWorld(sources ++ stubs)
    val templates = (sources ++ stubs).flatMap(_.tree.collect {
      case template if MemberSyntax.templateName(template).nonEmpty =>
        (
          MemberSyntax.templateName(template).get,
          body(template).flatMap(stat => MemberSyntax.defined(stat) ++ typeName(stat)).distinct
        )
      case alias: Defn.Type => (alias.name.value, Nil)
    })
    val names = Names(
      templates.groupMapReduce(_._1)(_ => 1)(_ + _),
      templates.flatMap(_._2).groupMapReduce(identity)(_ => 1)(_ + _),
      (sources ++ stubs)
        .flatMap(_.tree.collect { case init: Init => TypeApplications.parentName(init) })
        .toSet
    )
    val answers = sources.flatMap { source =>
      source.tree.collect {
        case template @ (_: Defn.Class | _: Defn.Trait) =>
          val name = MemberSyntax.templateName(template).get
          val owner = world.owner(template, name)
          // The old answer, in the new one's words.
          def asked(member: String) = Overriding.valueOf(world.overriding(owner, member).toString)
          val members = body(template).flatMap { stat =>
            MemberSyntax.defined(stat).map { member =>
              Key(source.path, nameLine(stat), name, member) -> asked(member)
            } ++ Option(stat).collect {
              case d: Defn.Type =>
                Key(source.path, d.name.pos.startLine, name, d.name.value) ->
                  asked(d.name.value)
            }
          }
          val extended = if (world.extended(owner)) Overriding.Supplied else Overriding.No
          (Key(source.path, nameLine(template), name, "") -> extended) :: members
      }.flatten
    }
    (answers.groupBy((key, _) => (key.line, key.owner, key.member)), names)
  }

  private def typeName(stat: SourceTree): Option[String] = stat match {
    case d: Defn.Type => Some(d.name.value)
    case d: Decl.Type => Some(d.name.value)
    case _            => None
  }

  private def body(template: SourceTree): List[SourceTree] = template match {
    case c: Defn.Class  => c.templ.body.stats
    case t: Defn.Trait  => t.templ.body.stats
    case o: Defn.Object => o.templ.body.stats
    case e: Defn.Enum   => e.templ.body.stats
    case _              => Nil
  }

  private def nameLine(tree: SourceTree): Int = tree match {
    case c: Defn.Class => c.name.pos.startLine
    case t: Defn.Trait => t.name.pos.startLine
    case d: Defn.Def   => d.name.pos.startLine
    case v: Defn.Val   => v.pats.head.pos.startLine
    case v: Defn.Var   => v.pats.head.pos.startLine
    case other         => other.pos.startLine
  }

  /** One answer compared, and for a finer one what explains it. */
  final case class Compared(key: Key, old: Overriding, next: Overriding, why: String)

  /** The answers of one input, the keys only one side has, and what the index took to answer: the
    * module's questions, then the first question that reads the dependencies.
    */
  final case class Outcome(compared: List[Compared], unpaired: List[String], cost: String)

  def compare(input: Differential.Input): Outcome = {
    val (before, names) = old(input)
    val classes = input.next.environment.classes.get
    val (context, own, unloaded) = CompiledTypes.loaded(classes)
    given Context = context
    val closed = new ClosedProgram(
      own.distinct,
      unloaded,
      CompiledTypes.dependencies(classes),
      CompiledTypes.unnamed(classes, ofModule = true),
      CompiledTypes.unnamed(classes, ofModule = false)
    )
    val all = own.distinct.flatMap(_.tree).flatMap(World.classes)
    val started = System.nanoTime()
    val after = all.flatMap { tree =>
      val c = tree.symbol
      if (c.isModuleClass || tree.pos.isUnknown || tree.pos.isSynthetic) Nil
      else {
        val (source, name) = (tree.pos.sourceFile.path, c.name.toString)
        val below = closed.subclasses(c)
        val extended =
          if (below.any) Overriding.Supplied
          else if (below.complete) Overriding.No
          else Overriding.Unknown
        val members = tree.rhs.body
          .collect {
            case member: ValOrDefDef => member -> (member.symbol: TermOrTypeSymbol)
            case member: TypeMember  => member -> (member.symbol: TermOrTypeSymbol)
          }
          .collect {
            case (member, symbol)
                if !member.pos.isUnknown && !member.pos.isSynthetic &&
                  !World.isConstructor(symbol) && !paramAccessor(symbol) &&
                  !symbol.name.toString.contains("$") =>
              Key(source, member.pos.pointLine, name, symbol.name.toString) ->
                closed.overriding(c, symbol)
          }
        (Key(source, tree.pos.pointLine, name, "") -> extended) :: members
      }
    }
    val module = (System.nanoTime() - started) / 1000000
    val again = System.nanoTime()
    val probed = closed.subclasses(ctx.findStaticClass("scala.Function1")).found.size
    val outside = (System.nanoTime() - again) / 1000000
    // The old side's path is the one the test or the corpus gives, the new one's the one the
    // classes record: a key pairs where one path ends with the other.
    val paired = after.flatMap { (key, next) =>
      before
        .getOrElse((key.line, key.owner, key.member), Nil)
        .collectFirst {
          case (other, old) if tails(other.source, key.source) =>
            (other, Compared(key, old, next, explained(names, key, old, next)))
        }
    }
    val (olds, news) = (paired.map(_._1).toSet, paired.map(_._2.key).toSet)
    Outcome(
      paired.map(_._2),
      before.values.flatten
        .map(_._1)
        .filterNot(olds)
        .map(k => s"only the scalameta front end: ${k.render}")
        .toList ++
        after.map(_._1).filterNot(news).map(k => s"only the compiled classes: ${k.render}"),
      s"${all.size} classes of the module walked and answered in $module ms; then the " +
        s"dependencies listed and indexed in $outside ms ($probed classes extend Function1)"
    )
  }

  private def tails(left: String, right: String): Boolean =
    Path.of(left).endsWith(right) || Path.of(right).endsWith(left)

  // Why an answer that is not the same differs. A finer answer is filed under a fact of the
  // scalameta reading's inputs, which it matches by simple name and cannot tell apart; one with
  // no such fact is unexplained.
  private def explained(names: Names, key: Key, old: Overriding, next: Overriding): String =
    if (old == next) "same"
    else if (next == Overriding.Unknown) "coarser"
    else if (next == Overriding.Supplied)
      "opener: a class of the program extends or overrides it, which the scalameta reading misses"
    else if (old == Overriding.Unknown)
      "finer: a parent the scalameta reading has no declaration of"
    else if (names.templates.getOrElse(key.owner, 0) > 1)
      "finer: another template or alias has the class's simple name"
    else if (key.member.nonEmpty && names.members.getOrElse(key.member, 0) > 1)
      "finer: another template defines the member's name"
    else if (key.member.isEmpty && names.parents(key.owner))
      "finer: a parent clause spells the simple name and names another class"
    else "finer, unexplained"

  // A constructor parameter's field: the scalameta closed world is not asked about one.
  private def paramAccessor(symbol: Symbol): Boolean = symbol match {
    case term: TermSymbol => term.isParamAccessor
    case _                => false
  }

  def main(args: Array[String]): Unit = {
    val inputs = args.toList match {
      case "corpus" :: root :: _    => List(Differential.corpus(Path.of(root)))
      case "fixtures" :: cache :: _ => Differential.fixtures(Path.of(cache), Programs.libraries)
      case _ => sys.error("usage: ClosedVerdicts (corpus <root> | fixtures <cache>) [--list]")
    }
    val outcomes = inputs.map { input =>
      input.label ->
        (try Right(compare(input))
        catch { case scala.util.control.NonFatal(error) => Left(error.toString) })
    }
    val failed = outcomes.collect { case (label, Left(why)) => s"$label $why" }
    val read = outcomes.collect { case (label, Right(outcome)) => label -> outcome }
    val compared = read.flatMap((label, outcome) => outcome.compared.map(label -> _))
    val (classes, members) = compared.partition(_._2.key.member.isEmpty)
    println("old: analysis.ClosedWorld by name; new: compiled.ClosedProgram by symbol")
    List("classes" -> classes, "members" -> members).foreach { (what, of) =>
      println(s"$what: ${of.size} paired")
      of.groupBy(_._2.why).toList.sortBy(_._1).foreach { (why, under) =>
        println(s"  ${under.size}  $why")
        val shown =
          if (why == "same") Nil else if (args.contains("--list")) under else under.take(3)
        shown.foreach { (label, c) =>
          println(s"      $label${c.key.render}: old ${c.old}, new ${c.next}")
        }
      }
    }
    val unpaired = read.flatMap((label, outcome) => outcome.unpaired.map(label + _))
    println(s"unpaired: ${unpaired.size}")
    (if (args.contains("--list")) unpaired else unpaired.take(5)).foreach(u => println(s"  $u"))
    if (read.size == 1) println(s"cost: ${read.head._2.cost}")
    failed.foreach(why => println(s"not read: $why"))
    val blocking =
      compared.count(c => c._2.why == "coarser" || c._2.why.endsWith("unexplained"))
    if (blocking > 0 || failed.nonEmpty) sys.exit(1)
  }

}
