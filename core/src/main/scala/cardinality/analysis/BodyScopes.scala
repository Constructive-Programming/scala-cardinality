package cardinality.analysis

import scala.meta.*

import cardinality.types.{Spelling, TypeApplications}

import MethodAnalysis.Frame

/** The scopes a definition's body opens below its own block, each with what it binds, so a
  * definition nested in one is indexed with the environment it sees.
  *
  * Definitions sit in the blocks, anonymous classes, function literals, case clauses and
  * for-comprehensions of a body at any depth. A scope that holds one becomes a frame, named by its
  * kind and its ordinal under its parent (`<new#1>`, `<case#2>`), so two `loop`s of two case arms
  * are two rows. Its environment is its typing context, as for a definition of the top block: the
  * enclosing parameters and captures, each ranging over its type — the signature lambda lifting
  * would give the definition. Control flow is no part of it: a branch, a guard or an argument the
  * callee may never evaluate leaves the context as it is, as callers leave a method's parameters.
  * An anonymous class is a template no subclass extends: its members are those of a final class. A
  * function literal's parameters are parameters, supplied by whoever applies it, as a method's are.
  *
  * A pattern changes the context in two ways the reading does not follow, so each is a reason a row
  * nested under it names. A binder may be the scrutinee itself (`case b: T`) or a part of a value
  * the scope already has, and two aliases of one value are one choice (the contract's §2); a given
  * binder (`case given A`) is one more contextual value. And where a type parameter is in scope, a
  * constructor, typed or stable-identifier pattern may add a type equation (`case TInt()` of a
  * `Ty[A]` makes `A` Int in its arm): every count may change, 0 and 1 included.
  */
final private[cardinality] class BodyScopes(
    holders: java.util.Set[Tree],
    open: (String, Frame, BodyScopes.Opening) => Frame,
    index: (String, Frame) => Unit
) {
  import BodyScopes.*

  /** A definition's right-hand side: its top block is the body frame, as it always was, and any
    * other expression is walked in the definition's own frame.
    */
  def body(input: String, frame: Frame, term: Tree): Unit = term match {
    case block: Term.Block =>
      index(
        input,
        frame.copy(
          id = frame.id + ":body",
          parent = Some(frame),
          stats = block.stats,
          params = Nil,
          typeParams = Nil,
          tree = Some(block)
        )
      )
    case other => walk(input, other, frame)
  }

  /** Every scope under `term` that holds a definition, opened under `frame` and indexed. */
  def walk(input: String, term: Tree, frame: Frame): Unit =
    if (holders.contains(term)) term match {
      case n: Term.NewAnonymous => anonymous(input, frame, n)
      case b: Term.Block        => scope(input, frame, Opening(b, "block", b.stats), Nil)
      case f: Term.FunctionTerm => lambda(input, frame, f)
      case f: Term.PolyFunction =>
        within(input, frame, Opening(f, "lambda", Nil, tparams = f.tparamClause.values), f.body)
      case c: Case          => within(input, frame, arm(c, frame), c.body, c.cond.toList)
      case f: Term.For      => comprehension(input, frame, f, f.enumsBlock.enums, f.body)
      case f: Term.ForYield => comprehension(input, frame, f, f.enumsBlock.enums, f.body)
      case other            => other.children.foreach(walk(input, _, frame))
    }

  // The constructor arguments are evaluated outside the instance.
  private def anonymous(input: String, frame: Frame, n: Term.NewAnonymous): Unit = {
    n.templ.inits.flatMap(_.argClauses).foreach(walk(input, _, frame))
    val opening =
      Opening(n, "new", n.templ.body.stats, local = false, parents = n.templ.inits)
    index(input, open(input, frame, opening))
  }

  // A context-function literal's parameters are contextual, named or not; another's anonymous ones
  // are no value its body can name.
  private def lambda(input: String, frame: Frame, f: Term.FunctionTerm): Unit = {
    val params = f.paramClause.values.filter(p => f.is[Term.ContextFunction] || named(p))
    within(input, frame, Opening(f, "lambda", Nil, params = params), f.body)
  }

  // A scope whose body is an expression: a block body's statements are the scope's own.
  private def within(
      input: String,
      frame: Frame,
      opening: Opening,
      body: Term,
      others: List[Tree] = Nil
  ): Unit = body match {
    case block: Term.Block => scope(input, frame, opening.copy(stats = block.stats), others)
    case _                 => scope(input, frame, opening, body :: others)
  }

  // The scope's statements are indexed in it, and the expressions it evaluates walked in it.
  private def scope(input: String, frame: Frame, opening: Opening, terms: List[Tree]): Unit = {
    val nested = open(input, frame, opening)
    index(input, nested)
    terms.foreach(walk(input, _, nested))
  }

  // Every pattern of a comprehension is in scope from its enumerator on; reading them all in one
  // scope only adds reasons, never a value.
  private def comprehension(
      input: String,
      frame: Frame,
      node: Term,
      enums: List[Enumerator],
      body: Term
  ): Unit = {
    val patterns = enums.collect {
      case g: Enumerator.Generator     => g.pat
      case g: Enumerator.CaseGenerator => g.pat
      case v: Enumerator.Val           => v.pat
    }
    val reasons = patterns.flatMap(refusals("for-comprehension", _, Spelling.of(_), frame))
    scope(input, frame, Opening(node, "for", Nil, refusals = reasons), enums :+ body)
  }

  private def arm(c: Case, frame: Frame): Opening = {
    val reasons = refusals("case clause", c.pat, pat => s"case ${Spelling.of(pat)}", frame)
    Opening(c, "case", Nil, refusals = reasons)
  }

  private def named(p: Term.Param): Boolean =
    !p.name.is[Name.Anonymous] && p.name.value.nonEmpty && p.name.value != "_"

}

private[cardinality] object BodyScopes {

  /** One scope a body opens: the tree, its kind's label, its statements, binders and parameters,
    * whether its statements are positional (a block's are; a template's are not), the parents of an
    * anonymous class, and the reasons its patterns give.
    */
  final case class Opening(
      node: Tree,
      kind: String,
      stats: List[Stat],
      local: Boolean = true,
      parents: List[Init] = Nil,
      tparams: List[Type.Param] = Nil,
      params: List[Term.Param] = Nil,
      refusals: List[String] = Nil
  )

  /** Every definition and every tree with one below it — the scopes a walk must open — across the
    * inputs, by identity: trees with equal syntax at two places are two scopes. An anonymous class
    * is one itself, its members values or definitions.
    */
  def holders(sources: List[Source]): java.util.Set[Tree] = {
    val found = new java.util.IdentityHashMap[Tree, Unit]()
    // Every ancestor of a tree already found is found: the walk up stops at the first one.
    @scala.annotation.tailrec
    def up(tree: Tree): Unit =
      if (!found.containsKey(tree)) {
        found.put(tree, ())
        tree.parent match {
          case Some(parent) => up(parent)
          case None         => ()
        }
      }
    sources.foreach(_.traverse { case definition if definitionNode(definition) => up(definition) })
    found.keySet
  }

  private def definitionNode(tree: Tree): Boolean = tree match {
    case _: (Defn.Def | Defn.GivenAlias | Defn.Given | Defn.ExtensionGroup) => true
    case _: (Defn.Class | Defn.Trait | Defn.Object | Defn.Enum)             => true
    case _: Term.NewAnonymous                                               => true
    case _                                                                  => false
  }

  // What one pattern does to the context that the reading leaves out: its binders, named and given,
  // and — where a type parameter of an enclosing frame is in scope — the type equations it may add.
  private def refusals(
      where: String,
      pat: Pat,
      spelled: Pat => String,
      frame: Frame
  ): List[String] = {
    val binders = TypeApplications.boundNames(List(pat)) ++ givenBinders(pat)
    val refines = Option.when(refining(pat) && frame.chain.exists(_.typeParams.nonEmpty))(
      s"pattern of an enclosing $where may refine a type: ${spelled(pat)}"
    )
    binders.map(name => s"binder of an enclosing $where not read: $name") ++ refines
  }

  private def givenBinders(pat: Pat): List[String] =
    pat.collect { case g: Pat.Given => s"given ${Spelling.of(g.tpe)}" }

  // Scala 3 constrains a type parameter in scope by matching a constructor, typed or stable
  // identifier pattern (an object's), whose type may extend the scrutinee's with arguments of its
  // own; a binder, a wildcard or a literal adds nothing (3.9.0 fixtures `$S/probes/b5-fix2/refine`).
  private def refining(pat: Pat): Boolean = pat match {
    case _: (Pat.Wildcard | Pat.SeqWildcard | Pat.Var | Pat.Repeated | Lit) => false
    case b: Pat.Bind                                                        => refining(b.rhs)
    case a: Pat.Alternative => refining(a.lhs) || refining(a.rhs)
    case t: Pat.Tuple       => t.args.exists(refining)
    case _                  => true
  }

}
