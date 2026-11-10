package cardinality.analysis.model

import scala.meta.*

import cardinality.analysis.SourceIndex
import cardinality.types.Trees.hasLazy

/** Terms whose evaluation always throws (register D26): no value and no implementation, typed
  * `Nothing` by Scala — `throw e`; the root imports' `???` or `sys.error(…)`, both declared
  * `Nothing`; a block whose result throws; a conditional or match every branch of which throws. A
  * simple name is the root import's only where nothing nearer binds it: `root` answers for the
  * scopes around the term, and the blocks, cases and imports inside it are read here.
  */
private[cardinality] object Throws {

  /** Whether evaluating `term` always throws, `root(name)` telling whether the simple `name` is the
    * root imports' where the term is written.
    */
  def only(term: Term, root: String => Boolean): Boolean = term match {
    case _: Term.Throw                         => true
    case Term.Name("???")                      => root("???")
    case Term.Select(qualifier, Term.Name(op)) => op == "???" && path(qualifier, Predef, root)
    case apply: Term.Apply                     => error(apply.fun, root)
    case Term.Block(stats :+ (last: Term))     => only(last, hidden(stats, root))
    case Term.If.After_4_4_0(_, yes, no, _)    => only(yes, root) && only(no, root)
    case Term.Match.After_4_9_9(_, cases, _)   =>
      cases.cases.nonEmpty && cases.cases.forall { c =>
        val names = MemberSyntax.patternNames(c.pat).toSet
        only(c.body, name => !names(name) && root(name))
      }
    case _ => false
  }

  /** The parents' arguments a primary constructor evaluates, where no `this` exists yet. */
  def arguments(parents: List[Init]): List[Term] = parents.flatMap(_.argClauses).flatMap(_.values)

  /** The terms a primary constructor evaluates in its body: each strict `val` or `var` initializer
    * and each statement, but `excluded` (the measured declaration, the choice being counted).
    */
  def evaluated(stats: List[Stat], excluded: Option[Tree]): List[Term] =
    stats.filterNot(stat => excluded.exists(_ eq stat)).collect {
      case v: Defn.Val if !v.mods.hasLazy => v.rhs
      case v: Defn.Var                    => v.body
      case term: Term                     => term
    }

  // `sys.error`, through the `sys` package the root imports bind or its full path.
  private def error(fun: Term, root: String => Boolean): Boolean = fun match {
    case Term.Select(qualifier, Term.Name("error")) => path(qualifier, Sys, root)
    case _                                          => false
  }

  private val Predef = List("scala", "Predef")
  private val Sys = List("scala", "sys")

  // A path to a root package member: its full path, `_root_`-anchored or not, or its last segment
  // where the root imports bind it (`Predef`, `sys`).
  private def path(term: Term, full: List[String], root: String => Boolean): Boolean =
    SourceIndex.importPath(term) match {
      case Some(List(single))             => single == full.last && root(single)
      case Some(SourceIndex.Root :: rest) => rest == full
      case Some(written)                  => written == full && root(full.head)
      case None                           => false
    }

  // The names a block's statements define or may import hide the root imports' ones after them; a
  // wildcard import may bind any.
  private def hidden(stats: List[Stat], root: String => Boolean): String => Boolean = {
    val defined = ScopeTerms.defined(stats)
    val imported = ScopeTerms.imported(stats)
    name => !defined(name) && imported.exists(names => !names(name)) && root(name)
  }

}
