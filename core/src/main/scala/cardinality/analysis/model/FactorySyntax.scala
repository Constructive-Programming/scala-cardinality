package cardinality.analysis.model

import scala.meta.*

/** The syntax rule B's factories are read by (v1.md §2, register D42), shared by the method side
  * (`ReceiverFactories`) and the stored-value side (`capacity.Factories`).
  *
  * A definition constructs a class where its result — through blocks, local `val`s, `if` and
  * `match` branches and ascriptions — is a direct construction of it (`builds`), or a call of a
  * method whose result constructs it, followed down at most `Depth` levels (`callees` names the
  * methods a call may run). A class's observable fields are the constructor parameters the site
  * reads: a field it may select, or one a member it may call reads, directly or through members it
  * may not call.
  */
private[cardinality] object FactorySyntax {

  /** How many calls a factory is read through to the construction it delegates to. */
  val Depth = 2

  /** A method's or a single `val`'s body and declared result. */
  def definition(stat: Stat): Option[(Term, Option[Type])] = stat match {
    case d: Defn.Def                     => Some(d.body -> d.decltpe)
    case v: Defn.Val if v.pats.size == 1 => Some(v.rhs -> v.decltpe)
    case _                               => None
  }

  /** Whether `body`'s result, read in `context`, constructs the class, through at most `depth`
    * delegating calls; `callees` gives the bodies a call may run, each with its own context.
    */
  def constructs[C](
      body: Term,
      context: C,
      builds: (Term, C) => Boolean,
      callees: (Term, C) => List[(Term, C)],
      depth: Int = Depth
  ): Boolean =
    results(body, Map.empty).exists { result =>
      builds(result, context) || depth > 0 && (result match {
        case _: Term.Apply =>
          callees(result, context).exists((b, c) => constructs(b, c, builds, callees, depth - 1))
        case _ => false
      })
    }

  /** Whether a body is a compiled dependency stub's, which the sources do not supply: its factory
    * builds what the dependency builds, which no reading here knows.
    */
  def compiled(body: Term): Boolean = body match {
    case Term.Name("compiledBodyNotRead")                 => true
    case Term.Select(_, Term.Name("compiledBodyNotRead")) => true
    case _                                                => false
  }

  /** A call's callee: its qualifier, if any, and the method's name. */
  def callee(term: Term): Option[(Option[Term], String)] = term match {
    case Term.Apply.After_4_6_0(fun, _)             => callee(fun)
    case Term.ApplyType.After_4_6_0(fun, _)         => callee(fun)
    case Term.Select(qualifier, Term.Name("apply")) => callee(qualifier)
    case Term.Name(name)                            => Some(None -> name)
    case Term.Select(qualifier, Term.Name(name))    => Some(Some(qualifier) -> name)
    case _                                          => None
  }

  /** The class's constructor parameters the site observes. */
  def observed(
      c: Defn.Class,
      open: Stat => Boolean,
      selectable: Term.Param => Boolean
  ): List[Term.Param] = {
    val names = read(c.templ.body.stats, open)
    c.ctor.paramClauses.toList.flatMap(_.values).filter(p => selectable(p) || names(p.name.value))
  }

  // The names the members the site may call read, through the members it may not call.
  private def read(stats: List[Stat], open: Stat => Boolean): Set[String] = {
    val members = stats.flatMap(stat => MemberSyntax.defined(stat).map(_ -> stat))
    @annotation.tailrec
    def loop(names: Set[String], seen: Set[Stat]): Set[String] = {
      val next = members.collect { case (n, stat) if names(n) && !seen(stat) => stat }
      if (next.isEmpty) names
      else loop(names ++ next.flatMap(mentioned), seen ++ next)
    }
    val start = stats.filter(s => open(s) && MemberSyntax.defined(s).nonEmpty)
    loop(start.flatMap(mentioned).toSet, start.toSet)
  }

  private def mentioned(stat: Stat): List[String] = stat.collect { case Term.Name(n) => n }

  // The terms a body may evaluate to: its result through blocks, local vals and branches.
  private def results(term: Term, locals: Map[String, Term]): List[Term] = term match {
    case Term.Block(stats :+ (last: Term)) =>
      val vals = stats.collect { case Defn.Val(_, List(Pat.Var(n)), _, rhs) => n.value -> rhs }
      results(last, locals -- stats.flatMap(MemberSyntax.defined) ++ vals)
    case Term.If.After_4_4_0(_, yes, no, _)  => results(yes, locals) ++ results(no, locals)
    case Term.Match.After_4_9_9(_, cases, _) => cases.cases.flatMap(c => results(c.body, locals))
    case Term.Ascribe(inner, _)              => results(inner, locals)
    case Term.Name(n) if locals.contains(n)  => results(locals(n), locals - n)
    case other                               => List(other)
  }

}
