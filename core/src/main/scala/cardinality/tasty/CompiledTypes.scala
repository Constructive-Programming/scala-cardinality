package cardinality.tasty

import scala.meta.{Defn, Mod, Pat, Term, Tree, XtensionCollectionLikeUI}
import scala.util.control.NonFatal

import cardinality.analysis.MethodAnalysis
import java.nio.file.Path
import tastyquery.Classpaths.ClasspathEntry
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

  // The running JDK's classes, read and indexed once: each read starts from them, and a class a
  // later entry declares again is not seen (`TastyStubs.firstLoaded`).
  private lazy val jdk: (List[ClasspathEntry], Set[(String, String)]) = {
    val entries =
      TastyStubs.firstLoaded(TastyStubs.jdk.flatMap(module => ClasspathLoaders.read(List(module))))
    val names = entries.flatMap(e =>
      e.listAllPackages()
        .flatMap(p => p.listAllClassDatas().map(p.dotSeparatedName -> _.binaryName))
    )
    (entries, names.toSet)
  }

  /** The module's own top-level classes, in a context over the JDK, its output and its classpath,
    * with why each class that does not load fails.
    */
  private[cardinality] def loaded(classes: Classes): (Context, List[ClassSymbol], List[String]) = {
    val (jdkEntries, jdkNames) = jdk
    val output = classes.output.toList.map(TastyStubs.readable)
    val own = output.flatMap(path => ClasspathLoaders.read(List(path)))
    val rest = classes.classpath.toList.distinct.map(TastyStubs.readable)
    val later =
      TastyStubs.firstLoaded(
        own ++ rest.flatMap(path => ClasspathLoaders.read(List(path))),
        jdkNames
      )
    val entries = jdkEntries ++ later
    given context: Context = Context.initialize(entries)
    val read = later.take(own.size).map(entry => TastyStubs.symbolsOf(entry, _ => true))
    (context, read.flatMap(_._1).collect { case c: ClassSymbol => c }, read.flatMap(_._2))
  }

  // Every compiled member, by source file and the offset of its name.
  private def read(classes: Classes): Map[String, Map[Int, List[Compiled]]] = {
    val (context, own, _) = loaded(classes)
    given Context = context
    own
      .flatMap(c => c.tree.toList.flatMap(members))
      .groupMap(_._1)(_._2)
      .view
      // A `var`'s getter and setter share their name's offset: every member there is kept.
      .mapValues(_.groupMap(_._1)(_._2))
      .toMap
  }

  // Every definition a class's tree holds, bodies included (locals, anonymous classes' members,
  // pattern names, lambda parameters): (source, (name offset, member)).
  private def members(c: ClassDef)(using Context): List[(String, (Int, Compiled))] = {
    val found = List.newBuilder[(String, (Int, Compiled))]
    new tastyquery.Traversers.TreeTraverser {
      override def traverse(tree: tastyquery.Trees.Tree): Unit = {
        tree match {
          case member: ValOrDefDef if !member.pos.isUnknown && !member.pos.isSynthetic =>
            val at = member.pos.pointOffset
            found += member.pos.sourceFile.path ->
              (at -> Compiled(member.name.toString, member.pos.endOffset, result(member)))
          case _ =>
        }
        super.traverse(tree)
      }
    }.traverse(c)
    found.result()
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
        // A compiler-made name (`$anon`, `x$1`) names nothing a source can write.
        Option.when(!spelled.exists("\n$".contains(_)) && Spell.parses(s"type T = $spelled"))(
          spelled
        )
      } catch { case NonFatal(_) => None }
  }

  // The compiled members of the one compiled source whose path `path` names; none when no single
  // one does.
  private def matching(
      path: String,
      compiled: Map[String, Map[Int, List[Compiled]]]
  ): Map[Int, List[Compiled]] = {
    def normal(p: String) = p.replace('\\', '/').stripPrefix("/")
    val candidates = compiled.filter { (source, _) =>
      val (a, b) = (normal(path), normal(source))
      a == b || a.endsWith("/" + b) || b.endsWith("/" + a)
    }
    if (candidates.size == 1) candidates.head._2 else Map.empty
  }

  /** Where a compiled type is written: after a member's name and parameters (`: T`), around a name
    * in a pattern or an unparenthesized lambda parameter (`(x: T)`), or after a parameter already
    * in parentheses.
    */
  private enum Site {
    case After(at: Int)
    case Around(start: Int, end: Int)
  }

  /** A definition written without a type: its name, the name's offset, where it ends, and where the
    * type goes.
    */
  final private case class Untyped(name: String, at: Int, end: Int, site: Site)

  // The input with a type written at each untyped definition that has a compiled match.
  private def written(
      input: MethodAnalysis.Input,
      compiled: Map[Int, List[Compiled]]
  ): MethodAnalysis.Input = {
    val edits = untyped(input.tree).flatMap { u =>
      compiled
        .getOrElse(u.at, Nil)
        .flatMap {
          case Compiled(name, end, Some(tpe)) if name == u.name && end == u.end =>
            u.site match {
              case Site.After(at)          => List(at -> (": " + tpe))
              case Site.Around(start, end) => List(end -> (": " + tpe + ")"), start -> "(")
            }
          case _ => Nil
        }
        .distinct
    }
    if (edits.isEmpty) input
    else {
      val text = input.tree.pos.input.text
      val ascribed = edits.sortBy(-_._1).foldLeft(text) {
        case (text, (at, word)) =>
          val c = text.charAt(at - 1)
          val gap =
            if (!word.startsWith(":") || c.isLetterOrDigit || "_`)]".contains(c)) "" else " "
          text.substring(0, at) + gap + word + text.substring(at)
      }
      Spell.source(ascribed).toOption.fold(input)(MethodAnalysis.Input(input.path, _))
    }
  }

  // Every definition written without a type: members and locals (not `transparent inline`, whose
  // callers see the expansion), the names of a pattern definition, and named lambda parameters.
  private def untyped(tree: Tree): List[Untyped] = {
    val here = tree match {
      case d: Defn.Def if d.decltpe.isEmpty && !transparent(d.mods) =>
        val after = d.paramClauseGroups.lastOption.fold(d.name.pos.end)(_.pos.end)
        List(Untyped(d.name.value, d.name.pos.start, d.pos.end, Site.After(after)))
      case v: Defn.Val if v.decltpe.isEmpty => values(v, v.pats)
      case v: Defn.Var if v.decltpe.isEmpty => values(v, v.pats)
      case f: Term.FunctionTerm             =>
        val parenthesized = f.paramClause.pos.text.startsWith("(")
        f.paramClause.values.collect {
          case Term.Param(_, name: Term.Name, None, None) =>
            val (start, end) = (name.pos.start, name.pos.end)
            Untyped(
              name.value,
              start,
              end,
              if (parenthesized) Site.After(end) else Site.Around(start, end)
            )
        }
      case _ => Nil
    }
    here ++ tree.children.flatMap(untyped)
  }

  private def transparent(mods: List[Mod]) = mods.exists(_.isInstanceOf[Mod.Transparent])

  // A single name is typed after it, to its definition's end; each name of a pattern is typed in
  // place, to its own end.
  private def values(defn: Tree, pats: List[Pat]): List[Untyped] = pats match {
    case List(Pat.Var(name)) =>
      List(Untyped(name.value, name.pos.start, defn.pos.end, Site.After(name.pos.end)))
    case _ =>
      pats.flatMap(_.collect {
        case v @ Pat.Var(name) if !v.parent.exists(_.isInstanceOf[Pat.Typed]) =>
          Untyped(
            name.value,
            name.pos.start,
            name.pos.end,
            Site.Around(name.pos.start, name.pos.end)
          )
      })
  }

}
