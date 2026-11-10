package cardinality.tasty

import scala.meta.{Defn, Mod, Pat, Pkg, Source, Template, Tree}
import scala.util.control.NonFatal

import cardinality.analysis.MethodAnalysis
import java.nio.file.Path
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Trees.{ClassDef, ValOrDefDef}
import tastyquery.Types.*
import tastyquery.jdk.ClasspathLoaders

/** The analyzed sources' own compiled types (docs/plans/compiled-own-types.md): a member written
  * without a result type takes the one the compiler wrote into the module's TASTy, spelled from
  * `_root_` and written into the source before anything reads it, as `ClosedWorld` writes `final`.
  *
  * The compiler's type is the answer, not a guess, so the syntax-only inference (`InferredResults`)
  * no longer has to refuse it. A member keeps its source untouched where the compiled one is not
  * found at the same name and span (a stale or foreign class), where its type has no spelling here,
  * and where it is `transparent inline`, whose callers see the expansion's type instead (Rule X).
  * Phase 1 covers template and top-level members, not locals.
  */
object CompiledTypes {

  /** The module's compiled output, and the classpath its types are read against. */
  final case class Classes(output: Seq[Path], classpath: Seq[Path] = Nil)

  /** A compiled member found in a source: its name, where its definition ends, and its result. */
  final private case class Compiled(name: String, end: Int, result: Option[String])

  /** The inputs with the compiled result types written in, and what could not be read. */
  def ascribe(
      inputs: List[MethodAnalysis.Input],
      classes: Classes
  ): (List[MethodAnalysis.Input], List[String]) =
    if (classes.output.isEmpty) (inputs, Nil)
    else
      try {
        val compiled = read(classes)
        (inputs.map(input => written(input, matching(input.path, compiled))), Nil)
      } catch {
        case NonFatal(error) => (inputs, List(s"compiled types not read: $error"))
      }

  // Every compiled member, by source file and the offset of its name.
  private def read(classes: Classes): Map[String, Map[Int, Compiled]] = {
    val jdk = TastyStubs.jdk.flatMap(module => ClasspathLoaders.read(List(module)))
    val output = classes.output.toList.map(TastyStubs.readable)
    val own = output.flatMap(path => ClasspathLoaders.read(List(path)))
    val rest = classes.classpath.toList.distinct.map(TastyStubs.readable)
    val entries =
      TastyStubs.firstLoaded(jdk ++ own ++ rest.flatMap(path => ClasspathLoaders.read(List(path))))
    given Context = Context.initialize(entries)
    val ownEntries = entries.slice(jdk.size, jdk.size + own.size)
    ownEntries
      .flatMap(entry => TastyStubs.symbolsOf(entry, _ => true)._1)
      .collect { case c: ClassSymbol => c }
      .flatMap(c => c.tree.toList.flatMap(members))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toMap)
      .toMap
  }

  // A class's template members, nested classes' included: (source, (name offset, member)).
  private def members(c: ClassDef)(using Context): List[(String, (Int, Compiled))] =
    c.rhs.body.flatMap {
      case member: ValOrDefDef if !member.pos.isUnknown && !member.pos.isSynthetic =>
        val at = member.pos.pointOffset
        List(
          member.pos.sourceFile.path ->
            (at -> Compiled(member.name.toString, member.pos.endOffset, result(member)))
        )
      case nested: ClassDef => members(nested)
      case _                => Nil
    }

  // The member's result type spelled for its source; none where it has no spelling here.
  private def result(member: ValOrDefDef)(using Context): Option[String] = {
    val symbol = member.symbol
    def enclosing(s: Symbol): List[ClassSymbol] = s match {
      case c: ClassSymbol   => c :: enclosing(c.owner)
      case _: PackageSymbol => Nil
      case other            => enclosing(other.owner)
    }
    def last(t: TypeOrMethodic): Type = t match {
      case m: MethodType => last(m.resultType)
      case p: PolyType   => last(p.resultType)
      case t: Type       => t
    }
    if (symbol.isTransparentInline) None
    else
      try {
        val spelled = StubTypes(enclosing(symbol.owner), rooted = true)(last(symbol.declaredType))
        Option.when(!spelled.contains('\n') && Spell.parses(s"type T = $spelled"))(spelled)
      } catch { case NonFatal(_) => None }
  }

  // The compiled members of the one compiled source whose path `path` names; none when no single
  // one does.
  private def matching(
      path: String,
      compiled: Map[String, Map[Int, Compiled]]
  ): Map[Int, Compiled] = {
    def normal(p: String) = p.replace('\\', '/').stripPrefix("/")
    val candidates = compiled.filter { (source, _) =>
      val (a, b) = (normal(path), normal(source))
      a == b || a.endsWith("/" + b) || b.endsWith("/" + a)
    }
    if (candidates.size == 1) candidates.head._2 else Map.empty
  }

  // The input with a result type written after each member that has none and a compiled match.
  private def written(
      input: MethodAnalysis.Input,
      compiled: Map[Int, Compiled]
  ): MethodAnalysis.Input = {
    val insertions = untyped(input.tree).flatMap { (name, at, end, mods, after) =>
      compiled.get(at).collect {
        case Compiled(`name`, `end`, Some(tpe)) if !mods.exists(_.isInstanceOf[Mod.Transparent]) =>
          after -> tpe
      }
    }
    if (insertions.isEmpty) input
    else {
      val text = input.tree.pos.input.text
      val ascribed = insertions.sortBy(-_._1).foldLeft(text) {
        case (text, (after, tpe)) =>
          val c = text.charAt(after - 1)
          val gap = if (c.isLetterOrDigit || "_`)]".contains(c)) "" else " "
          text.substring(0, after) + gap + ": " + tpe + text.substring(after)
      }
      Spell.source(ascribed).toOption.fold(input)(MethodAnalysis.Input(input.path, _))
    }
  }

  // Members written without a result type at template or top level: their name, the name's
  // offset, where the definition ends, their modifiers, and where a type would go.
  private def untyped(tree: Tree): List[(String, Int, Int, List[Mod], Int)] = {
    val here = tree match {
      case d: Defn.Def if d.decltpe.isEmpty =>
        val after = d.paramClauseGroups.lastOption.fold(d.name.pos.end)(_.pos.end)
        List((d.name.value, d.name.pos.start, d.pos.end, d.mods, after))
      case v: Defn.Val if v.decltpe.isEmpty => single(v, v.pats, v.mods)
      case v: Defn.Var if v.decltpe.isEmpty => single(v, v.pats, v.mods)
      case _                                => Nil
    }
    val inside = tree match {
      case _: (Source | Pkg | Pkg.Body | Template | Template.Body | Defn.Class | Defn.Trait |
            Defn.Object | Defn.Enum | Pkg.Object | Defn.Given) =>
        tree.children.flatMap(untyped)
      case _ => Nil
    }
    here ++ inside
  }

  private def single(defn: Tree, pats: List[Pat], mods: List[Mod]) = pats match {
    case List(Pat.Var(name)) => List((name.value, name.pos.start, defn.pos.end, mods, name.pos.end))
    case _                   => Nil
  }

}
