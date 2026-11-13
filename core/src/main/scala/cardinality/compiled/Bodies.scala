package cardinality.compiled

import tastyquery.Contexts.*
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.Trees.*

/** What the counting model reads of a template's construction, over typed trees (register D26):
  * whether its constructor always throws. A pattern definition is an ordinary binding, and one that
  * may not match is a possible throw, which the model ignores.
  */
private[compiled] object Bodies {

  /** Whether building an instance of a template always throws, by what the template itself
    * evaluates: the arguments it passes to its parents' constructors, each strict value's
    * initializer and each statement of its body, but the statement of `excluded` (the measured
    * member, whose initializer is the choice being counted).
    */
  def alwaysThrows(template: ClassDef, excluded: Option[Tree] = None)(using Context): Boolean = {
    val statement = excluded.toList.flatMap(statementOf(_, template))
    (arguments(template) ++ evaluated(template, statement)).exists(throws)
  }

  // The values of the source statement a member is written in: itself, the other names of a
  // multi-name definition (`val a, b = …`, all starting where it does) and the holder the compiler
  // makes for a pattern definition, which its value selects from (`val (a, b) = …`); only in the
  // template the member is written in (an ancestor's statement at the same offset is another one).
  private def statementOf(member: Tree, template: ClassDef)(using Context): List[Tree] =
    member match {
      case v: ValDef if !v.pos.isUnknown && v.symbol.owner == template.symbol =>
        val body = template.rhs.body
        val holder = v.rhs.toList.collect { case Select(id: Ident, _) => World.named(id) }.flatten
        member :: body.collect {
          case w: ValDef if !w.pos.isUnknown && w.pos.startOffset == v.pos.startOffset => w
          case w: ValDef if holder.contains(w.symbol) && w.symbol.isSynthetic          => w
        }
      case _ => List(member)
    }

  private def evaluated(template: ClassDef, excluded: List[Tree]): List[TermTree] =
    template.rhs.body.filterNot(stat => excluded.exists(_ eq stat)).flatMap {
      case v: ValDef if v.symbol.kind != TermSymbolKind.LazyVal => v.rhs.toList
      case term: TermTree                                       => List(term)
      case _                                                    => Nil
    }

  // The arguments a template passes to its parents' constructors: named ones out of order are
  // values of a block around the call, one in order keeps its name, and repeated ones are a
  // sequence literal. A by-name argument is taken as evaluated.
  private def arguments(template: ClassDef): List[TermTree] =
    template.rhs.parents
      .flatMap {
        case call: Apply               => passed(call)
        case Block(stats, call: Apply) =>
          stats.collect { case v: ValDef => v.rhs }.flatten ++ passed(call)
        case _ => Nil
      }
      .flatMap(each)

  private def passed(call: Apply): List[TermTree] = call.fun match {
    case inner: Apply => passed(inner) ++ call.args
    case _            => call.args
  }

  private def each(argument: TermTree): List[TermTree] = argument match {
    case NamedArg(_, value)             => each(value)
    case Typed(sequence: SeqLiteral, _) => each(sequence)
    case SeqLiteral(elems, _)           => elems.flatMap(each)
    case plain                          => List(plain)
  }

  // Whether evaluating a term always throws: `throw`, the library's `???` and `sys.error`, one of
  // them ascribed, a block whose result throws, a conditional or match every branch of which
  // throws or whose scrutinee does (`val _ = ???`, a pattern definition's), and a parameter's
  // default whose body throws (`defaulted`; a by-name parameter's too, as a by-name argument is
  // taken as evaluated).
  private def throws(term: TermTree)(using Context): Boolean = term match {
    case _ if defaulted(term)             => true
    case _: Throw                         => true
    case ref: TermReferenceTree           => ref.symbol.displayFullName == "scala.Predef$.???"
    case Apply(fun: TermReferenceTree, _) =>
      fun.symbol.displayFullName == "scala.sys.package$.error"
    case Typed(inner, _)        => throws(inner)
    case Block(_, result)       => throws(result)
    case If(_, yes, no)         => throws(yes) && throws(no)
    case Match(selector, cases) =>
      throws(selector) || cases.nonEmpty && cases.forall(c => throws(c.body))
    case _ => false
  }

  // Whether a term reads a parameter's default (as the argument a parent's clause leaves out) whose
  // body always throws: the default is evaluated where it is passed (the old `Builds.omitted`).
  private def defaulted(term: TermTree)(using Context): Boolean = term match {
    case Apply(fun, _)     => defaulted(fun)
    case TypeApply(fun, _) => defaulted(fun)
    // The written name first: not every reference's symbol resolves (a refinement's member).
    case ref: TermReferenceTree if written(ref).contains("$default$") =>
      ref.symbol.tree.exists {
        case getter: DefDef => getter.rhs.exists(throws)
        case _              => false
      }
    case _ => false
  }

  private def written(ref: TermReferenceTree): String = ref match {
    case id: Ident      => id.name.toString
    case select: Select => select.name.toString
  }

}
