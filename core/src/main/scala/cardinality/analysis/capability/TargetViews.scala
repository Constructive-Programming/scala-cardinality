package cardinality.analysis.capability

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.model.{MemberSyntax, ScopeTerms, UniversalMembers}

/** Which concrete members of a receiver are views of the measured declaration (capabilities C6).
  *
  * The model counts recursion-free implementations: no evaluation of one re-enters the measured
  * declaration, in any admissible receiver. A receiver that overrides nothing is admissible, so a
  * member whose default body evaluates the declaration — directly, or through another member's
  * default on the same receiver — re-enters it whenever it is evaluated: such a member is a
  * *definite* view, and no implementation uses it. Every use of a producer in a long normal form is
  * fully applied (its curried result lambdas applied too), and every surrounding producer is a
  * capability that may evaluate what it is given, so "evaluated whenever the body is" is the test:
  * the head of the body's spine, the receiver of a selection, a by-value argument of a callee whose
  * declaration is read, an operand of `==`, a block statement, a tuple field, an `if` condition, a
  * match scrutinee, the body of a lambda the member returns as a function. A name evaluates a
  * member only where it applies every value clause of the one member its arguments select
  * (overloads resolve by arity first): an unapplied method is η-expanded, not run.
  *
  * Each of those positions is evaluated only if the compiler inserts nothing that defers it: a
  * context-function parameter wraps its argument in a lambda, an implicit conversion or extension
  * taking its argument by name runs without it, and an overload of `==` may take its operand by
  * name. So a selection's receiver counts only when the selected name is a member of its declared
  * template or of `Any`, or no extension or conversion with a by-name receiver may be visible; an
  * argument or a typed position only when no by-name conversion may be; an `==` operand only when
  * no by-name `==` may be selected (`Scope`). Any other mention — a branch, a lambda passed on or
  * converted to a SAM, an argument of an unread callee or of a right-associative operator, an
  * η-expansion, a nested template's body, `super`, an overload the arity does not settle, `this`
  * escaping — makes it a *possible* view: re-entry depends on what the member is given, which this
  * reading does not split, so it carries a reason.
  */
private[cardinality] object TargetViews {

  enum View {
    case Free, Possible, Definite
  }

  /** How a measurement reads possible views (`ViewSandwich`): refused, left out (as a definite view
    * is) or bound as free capabilities.
    */
  enum ViewMode {
    case Refuse, Exclude, Include

    def effective(view: View): View = (this, view) match {
      case (Exclude, View.Possible) => View.Definite
      case (Include, View.Possible) => View.Free
      case _                        => view
    }

  }

  val Reentry = "overridable member may re-enter the measured declaration"

  /** The value clauses a mention applies (`using` clauses are filled implicitly) and the argument
    * count of the first.
    */
  final case class Applied(clauses: Int, first: Int)

  object Applied {
    val bare: Applied = Applied(0, 0)
  }

  /** The slots a mention may denote, and whether it certainly runs the one it denotes. */
  final case class Resolution(slots: Set[Int], evaluated: Boolean)

  /** What the names one body spells mean on the receiver. */
  trait Scope {

    /** A mention of a member through `this`, applied as given; `None` when no member has the name.
      */
    def own(name: String, applied: Applied): Option[Resolution]

    /** A selection chain from this receiver (`Inner.target`): the measured declaration reached
      * through its static path, if the chain spells it.
      */
    def selected(path: List[String], name: String, applied: Applied): Option[Resolution]

    /** Every slot a name may denote: members, nested templates, the measured declaration. */
    def named(name: String): Set[Int]

    /** A mention of a definition local to the body, applied as given: the measured declaration's
      * slot when the definition is the measured one, none otherwise.
      */
    def local(definition: Tree, applied: Applied): Option[Resolution]

    /** The slots building a class may run (`Constructs`, spelled at offset `at`): the constructors,
      * strict initializers and statements of the levels it extends, and any member its own code may
      * call.
      */
    def constructed(constructs: Constructs, at: Int): Set[Int]

    /** Whether a literal has the declared result type as it is, so no conversion is searched. */

    /** The enclosing template a bare name not of this receiver is a member of. */
    def enclosing(name: String): Option[String]

    /** Whether a member of an enclosing template (`Outer.this.n`; the empty name for `Outer.this`
      * itself) may reach the measured declaration.
      */
    def outer(template: String, name: String): Boolean

    /** For each argument list of a call (each flagged when it is an explicit `using` list), whether
      * its arguments are evaluated before the call; unknown is `false`. `typed` tells whether the
      * call passes explicit type arguments.
      */
    def byValue(head: Term, contextual: List[Boolean], typed: Boolean): List[Boolean]

    /** Whether selecting `name` (applied as given) on a qualifier at offset `at` evaluates the
      * qualifier: the name is a member of the qualifier's declared template — `member` names the
      * receiver member the qualifier is, if it is one — or of `Any`, or nothing visible there could
      * defer it.
      */
    def evaluatesQualifier(member: Option[String], name: String, applied: Applied, at: Int): Boolean

    /** Whether the compiler may wrap an expression with an expected type in an implicit conversion
      * that takes it by name.
      */
    def converts: Boolean

    /** Whether an operand of the universal operator `op` may be taken by name: some overload or
      * extension of it with a by-name parameter may be selected.
      */
    def deferredOperand(op: String): Boolean
  }

  /** How a body names its own receiver: its self alias and its template's name (`T.this`). */
  final case class Self(aliases: Set[String], template: Set[String])

  /** What one body reads: slots certainly and possibly evaluated, whether it may reach the
    * declaration through an enclosing template, whether `this` escapes, and whether it may search
    * implicits (any body but a literal).
    */
  final case class Reads(
      definite: Set[Int],
      possible: Set[Int],
      outer: Boolean,
      escapes: Boolean,
      searches: Boolean
  )

  /** One member's body: its tree, the names its parameters bind, its declared result type (`None`
    * when inferred), and whether it can search no implicit (`settled`). A secondary constructor's
    * tree is its block, and a template's the statements its primary constructor runs.
    */
  final case class Body(
      tree: Tree,
      parameters: Set[String],
      result: Option[Type],
      settled: Boolean = false
  )

  /** What a member's body (a nested template's, read loosely) reads of its receiver. */
  def reads(body: Body, self: Self, scope: Scope): Reads = {
    val walk = new Walk(scope, self)
    val bound = Bound(body.parameters, Map.empty)
    body.tree match {
      case term: Term         => walk.result(term, bound, body.result)
      case block: Ctor.Block  => walk.statements(block.stats, bound)
      case template: Template => walk.statements(template.body.stats.filter(_.is[Term]), bound)
      case other              => walk.loose(other, bound)
    }
    val literal = body.tree match {
      case _: Lit => body.result.isEmpty
      case _      => false
    }
    walk.reads.copy(searches = !literal && !body.settled)
  }

  /** Each node's view of the target slot, by position: a node is a concrete member or a nested
    * template, keyed by its slot. A step through a slot is definite only when every node of it is.
    */
  def classify(target: Int, nodes: List[(Int, Reads)]): List[View] = {
    val bySlot = nodes.indices.groupBy(nodes(_)._1)
    def at(views: Vector[View])(slot: Int): Seq[View] =
      bySlot.getOrElse(slot, Nil).map(views(_))
    @scala.annotation.tailrec
    def settle(views: Vector[View]): Vector[View] = {
      val next = nodes.zipWithIndex.map {
        case ((_, read), index) =>
          val found = view(read, target, at(views))
          if (found.ordinal > views(index).ordinal) found else views(index)
      }.toVector
      if (next == views) views else settle(next)
    }
    settle(Vector.fill(nodes.size)(View.Free)).toList
  }

  private def view(read: Reads, target: Int, views: Int => Seq[View]): View = {
    def definite(slot: Int): Boolean =
      slot == target || {
        val found = views(slot); found.nonEmpty && found.forall(_ == View.Definite)
      }
    val touched = read.definite ++ read.possible
    if (read.definite.exists(definite)) View.Definite
    else if (
      read.possible(target) || touched.exists(views(_).exists(_ != View.Free)) || read.outer ||
      read.escapes
    ) View.Possible
    else View.Free
  }

  // One walk over a body. A name bound by a parameter, a lambda or a local definition is no member;
  // any construct this walk does not read is entered loosely, so it can only add possible mentions.
  final private class Walk(scope: Scope, self: Self) {
    private val definite = mutable.Set.empty[Int]
    private val possible = mutable.Set.empty[Int]
    private var outer = false
    private var escapes = false
    // What the imports of enclosing blocks bring before the term: names that may denote an
    // extension from outside the sources, or, after a wildcard or given import, any extension or
    // conversion (the receiver's channels, `Scope.converts`, cover every other import).
    private var imports = Imports(open = false, Set.empty)

    def reads: Reads = Reads(definite.toSet, possible.toSet, outer, escapes, searches = true)

    private def record(resolution: Resolution, strict: Boolean): Unit =
      if (strict && resolution.evaluated) definite ++= resolution.slots
      else possible ++= resolution.slots

    private def reach(template: String, name: String): Unit =
      outer = outer || scope.outer(template, name)

    // The body as the member's result, `expected` its declared type: a lambda returned as a
    // function (or with an inferred type) is applied; one converted to a SAM or another type may
    // only be observed through other members. A block's last expression is the result.
    def result(term: Term, bound: Bound, expected: Option[Type]): Unit = term match {
      case Term.Function.After_4_6_0(params, body) =>
        val inner = bound ++ params.values.map(_.name.value)
        functionResult(expected) match {
          case Some(next) => result(body, inner, next)
          case None       => evaluate(body, strict = false, inner)
        }
      case Term.Block(stats) if stats.nonEmpty =>
        importing {
          val inner = block(stats.init, strict = true, bound)
          stats.last match {
            case last: Term => result(last, inner, expected)
            case other      => loose(other, inner)
          }
        }
      case other => evaluate(other, strict = typed(strict = true), bound)
    }

    /** Statements run in order, as a constructor runs its body's. */
    def statements(stats: List[Stat], bound: Bound): Unit =
      importing { block(stats, strict = true, bound); () }

    // A block's imports bind only inside it.
    private def importing(body: => Unit): Unit = {
      val enclosing = imports
      body
      imports = enclosing
    }

    private def evaluate(term: Tree, strict: Boolean, bound: Bound): Unit = term match {
      case name: Term.Name                    => named(name.value, Applied.bare, strict, bound)
      case _: Term.This                       => escapes = true
      case select: Term.Select                => selection(select, Applied.bare, strict, bound)
      case Term.ApplyType.After_4_6_0(fun, _) => evaluate(fun, strict, bound)
      case apply: Term.Apply                  => application(apply, strict, bound)
      case Term.ApplyInfix.After_4_6_0(lhs, op, _, args) =>
        operator(Infix(lhs, op.value, args.values), strict, bound)
      case Term.ApplyUnary(op, arg) =>
        evaluate(arg, strict && qualifier(arg, s"unary_${op.value}", Applied.bare, bound), bound)
      case Term.Tuple(args)                      => args.foreach(evaluate(_, typed(strict), bound))
      case Term.Ascribe(expr, _)                 => evaluate(expr, typed(strict), bound)
      case Term.If.After_4_4_0(cond, yes, no, _) =>
        evaluate(cond, typed(strict), bound)
        evaluate(yes, strict = false, bound)
        evaluate(no, strict = false, bound)
      case Term.Match.After_4_9_9(scrutinee, cases, _) =>
        evaluate(scrutinee, strict, bound)
        cases.cases.foreach(c => loose(c, bound ++ MemberSyntax.patternNames(c.pat)))
      case Term.Block(stats)                       => importing { block(stats, strict, bound); () }
      case Term.Function.After_4_6_0(params, body) =>
        evaluate(body, strict = false, bound ++ params.values.map(_.name.value))
      case _: Lit => ()
      case other  => loose(other, bound)
    }

    // A bare name: a member of this receiver, of an enclosing template, or a nested template.
    private def named(name: String, applied: Applied, strict: Boolean, bound: Bound): Unit =
      if (bound(name))
        bound.definitions.get(name).flatMap(scope.local(_, applied)).foreach(record(_, strict))
      else if (self.aliases(name)) escapes = true
      else
        scope.own(name, applied) match {
          case Some(resolution) => record(resolution, strict)
          case None             => elsewhere(name)
        }

    // A name no member of this receiver has: a member of an enclosing template, a nested template,
    // or the measured declaration seen from an enclosing receiver.
    private def elsewhere(name: String): Unit = {
      scope.enclosing(name).foreach(reach(_, name))
      possible ++= scope.named(name)
    }

    // `this` or the template's self alias; any `T.this` escapes.
    private def receiver(term: Term, bound: Bound): Boolean =
      term match {
        case Term.This(_: Name.Anonymous)                             => true
        case Term.Name(alias) if self.aliases(alias) && !bound(alias) => true
        case _                                                        => false
      }

    // A position with an expected type: evaluated only if no by-name conversion may wrap it.
    private def typed(strict: Boolean): Boolean = strict && !scope.converts && !imports.open

    // A selection: a member through `this`, a parent's definition through `super`, a member of an
    // enclosing template, the declaration through its static path, or a member of another value
    // (whose receiver is evaluated unless an extension or conversion may take it by name).
    private def selection(
        select: Term.Select,
        applied: Applied,
        strict: Boolean,
        bound: Bound
    ): Unit = {
      val name = select.name.value
      (select.qual, receiver(select.qual, bound)) match {
        case (Term.Super(owner, _), _) => parent(owner, name, applied)
        case (_, true)                 => scope.own(name, applied).foreach(record(_, strict))
        case (qual, false)             =>
          chain(qual, bound).flatMap(scope.selected(_, name, applied)) match {
            case Some(resolution) => record(resolution, strict)
            case None => evaluate(qual, strict && qualifier(qual, name, applied, bound), bound)
          }
      }
    }

    // `super.m` runs a parent's definition of m: in a trait, whichever definition the instance's
    // linearization puts next, so any definition of m's slot may run. `Outer.super.m` is an
    // enclosing template's.
    private def parent(owner: Name, name: String, applied: Applied): Unit =
      if (owner.is[Name.Anonymous] || self.template(owner.value))
        possible ++= scope.own(name, applied).toList.flatMap(_.slots)
      else reach(owner.value, name)

    private def qualifier(qual: Term, name: String, applied: Applied, bound: Bound): Boolean =
      !imports.open && !imports.names(name) &&
        scope.evaluatesQualifier(member(qual, bound), name, applied, qual.pos.start)

    // The receiver member a qualifier names, if it is one: a bare name or one selected on `this`.
    private def member(qual: Term, bound: Bound): Option[String] =
      chain(qual, bound).collect { case List(name) => name }

    // The names of a selection chain rooted at a bare name or at `this`.
    private def chain(term: Term, bound: Bound): Option[List[String]] = term match {
      case Term.Name(name) if !bound(name) && !self.aliases(name) => Some(List(name))
      case Term.Select(qualifier, Term.Name(name))                =>
        if (receiver(qualifier, bound)) Some(List(name))
        else chain(qualifier, bound).map(_ :+ name)
      case _ => None
    }

    // A call: its head is evaluated (a member applied as its clauses say); its arguments are, where
    // the callee's declaration says they are by value. A head naming a local binding or a member of
    // one has no declaration this reading looks up.
    private def application(apply: Term.Apply, strict: Boolean, bound: Bound): Unit = {
      val (head, clauses, typeArguments) = spine(apply, Nil, typeArguments = false)
      val contextual = clauses.map(_.mod.exists(_.is[Mod.Using]))
      val values = clauses.zip(contextual).collect { case (clause, false) => clause }
      val applied = Applied(values.size, values.headOption.fold(0)(_.values.size))
      head match {
        case Term.Name(name)     => named(name, applied, strict, bound)
        case select: Term.Select => selection(select, applied, strict, bound)
        case other               => evaluate(other, strict, bound)
      }
      possible ++= creation(head, bound)
      val known = head match {
        case Term.Name(name)                      => !bound(name)
        case Term.Select(Term.Name(qualifier), _) => !bound(qualifier) && !self.aliases(qualifier)
        case _                                    => false
      }
      val byValue =
        if (known) scope.byValue(head, contextual, typeArguments)
        else List.fill(contextual.size)(false)
      clauses.zip(byValue).foreach { (clause, eager) =>
        clause.values.map(argument).foreach(evaluate(_, typed(strict && eager), bound))
      }
    }

    // A creator application builds a class as `new` does: `C(…)`, `p.C(…)` or `C.apply(…)` — a case
    // class's companion `apply` or any class's constructor proxy, where the name is a supplied
    // class's — and `copy(…)` builds the case class it is a member of: this receiver's own on
    // `this`, one this reading does not follow on any other value.
    private def creation(head: Term, bound: Bound): Set[Int] = {
      def named(tpe: Type, name: String) =
        Option.when(!bound(name) && !self.aliases(name) && scope.own(name, Applied.bare).isEmpty)(
          Constructs.Named(tpe, name, imports.open || imports.names(name), creator = true)
        )
      (head match {
        case Term.Name("copy") if !bound("copy")              => Some(Constructs.Unfollowed)
        case Term.Select(_, Term.Name("copy"))                => Some(Constructs.Unfollowed)
        case Term.Select(Term.Name(name), Term.Name("apply")) => named(Type.Name(name), name)
        case Term.Name(name)                                  => named(Type.Name(name), name)
        case Term.Select(path: Term.Ref, Term.Name(name)) if chain(path, bound).nonEmpty =>
          named(Type.Select(path, Type.Name(name)), root(path))
        case _ => None
      }).fold(Set.empty[Int])(scope.constructed(_, head.pos.start))
    }

    // A named argument `p = e` is `e` in the clause it fills (every parameter of an eager clause is
    // by value, wherever it sits); `p` names a parameter, not a value in scope.
    private def argument(arg: Term): Term = arg match {
      case Term.Assign(_: Term.Name, rhs) => rhs
      case other                          => other
    }

    // A call's head, its argument lists, and whether it passes explicit type arguments.
    @scala.annotation.tailrec
    private def spine(
        function: Term,
        clauses: List[Term.ArgClause],
        typeArguments: Boolean
    ): (Term, List[Term.ArgClause], Boolean) =
      function match {
        case Term.Apply.After_4_6_0(inner, args)  => spine(inner, args :: clauses, typeArguments)
        case Term.ApplyType.After_4_6_0(inner, _) => spine(inner, clauses, typeArguments = true)
        case head                                 => (head, clauses, typeArguments)
      }

    // `l op r` calls `op` on `l` with `r`: `l` is evaluated as a selection's receiver is, and `r`
    // before the call only for universal equality whose by-name overloads cannot be selected. A
    // right-associative `l op: r` calls it on `r`, and `l` may be by name.
    private def operator(infix: Infix, strict: Boolean, bound: Bound): Unit = {
      val Infix(lhs, op, rhs) = infix
      val rightAssociative = op.endsWith(":")
      val (callee, operands) = if (rightAssociative) (rhs, List(lhs)) else (List(lhs), rhs)
      val applied = Applied(1, operands.size)
      callee.foreach { term =>
        if (receiver(term, bound)) scope.own(op, applied).foreach(record(_, strict))
        else evaluate(term, strict && qualifier(term, op, applied, bound), bound)
      }
      val eager = !rightAssociative && universal(op) && !scope.deferredOperand(op)
      operands.foreach(evaluate(_, strict && eager, bound))
    }

    // Statements in order; a lazy value or a method body is not evaluated by the block. Every local
    // definition binds its name in the whole block: Scala rejects a reference that would reach past
    // a local value to an outer one, so no name the block defines means a member anywhere in it.
    private def block(stats: List[Stat], strict: Boolean, bound: Bound): Bound = {
      val inner = bound.defining(stats.flatMap(stat => local(stat).map(_ -> stat)))
      stats.foreach {
        case v: Defn.Val =>
          val eager = strict && !v.mods.exists(_.is[Mod.Lazy])
          evaluate(v.rhs, if (v.decltpe.isEmpty) eager else typed(eager), inner)
        // The measured declaration's own body is the choice being counted, no mention of it.
        case d: Defn.Def if scope.local(d, Applied.bare).isEmpty =>
          evaluate(
            d.body,
            strict = false,
            inner ++ MemberSyntax.parameterNames(d.paramClauseGroups)
          )
        case _: Defn.Def => ()
        case term: Term  => evaluate(term, strict, inner)
        case i: Import   =>
          imports = ScopeTerms
            .imported(List(i))
            .fold(imports.copy(open = true))(names => imports.copy(names = imports.names ++ names))
          loose(i, inner)
        case other => loose(other, inner)
      }
      inner
    }

    private def local(stat: Stat): List[String] =
      MemberSyntax.templateName(stat).fold(MemberSyntax.defined(stat))(List(_))

    // Anything this walk does not read: every name in it a possible mention — the measured local
    // definition's included, inside a match arm, a `try`, an interpolation or a loop as anywhere —
    // and nested templates too (`new H()` names H as a type). A local class is read where the block
    // defines it, its parents' construction included.
    def loose(tree: Tree, bound: Bound): Unit =
      tree.traverse {
        case Term.Name(name) if bound(name) => possible ++= local(name, bound)
        case Term.Name(name)                =>
          if (self.aliases(name)) escapes = true
          else {
            scope.own(name, Applied.bare).foreach(r => possible ++= r.slots)
            elsewhere(name)
          }
        case Type.Name(name) => possible ++= scope.named(name)
        case _: Term.This    => escapes = true
        case init: Init      =>
          spelled(init.tpe) match {
            case Some(Left(name)) if bound(name) => ()
            case first                           =>
              val imported = imports.open || first.exists(f => imports.names(f.merge))
              possible ++= scope.constructed(Constructs.of(init.tpe, imported), init.pos.start)
          }
        case apply: Term.Apply => possible ++= creation(spine(apply, Nil, false)._1, bound)
      }

    // How an initializer's class is spelled: a plain name (Left), or the first name of a path.
    private def spelled(tpe: Type): Option[Either[String, String]] = tpe match {
      case Type.Name(name)                   => Some(Left(name))
      case Type.Select(path, _)              => Some(Right(root(path)))
      case Type.Apply.After_4_6_0(callee, _) => spelled(callee)
      case _                                 => None
    }

    @scala.annotation.tailrec
    private def root(path: Term.Ref): String = path match {
      case Term.Select(qualifier: Term.Ref, _) => root(qualifier)
      case other                               => other.syntax
    }

    // The target's slot when a bound name is the measured local definition.
    private def local(name: String, bound: Bound): Set[Int] =
      bound.definitions.get(name).flatMap(scope.local(_, Applied.bare)).fold(Set.empty)(_.slots)

  }

  /** The names a body binds, a local definition's with its tree; a name bound again drops it. */
  final case class Bound(names: Set[String], definitions: Map[String, Tree]) {

    def apply(name: String): Boolean = names(name)

    def ++(more: Iterable[String]): Bound = Bound(names ++ more, definitions -- more)

    def defining(locals: List[(String, Tree)]): Bound =
      Bound(names ++ locals.map(_._1), definitions ++ locals)

  }

  // The names block imports bring into scope, or `open` when one may bring any name.
  final private case class Imports(open: Boolean, names: Set[String])

  // An infix application: its left operand, operator and right operands.
  final private case class Infix(lhs: Term, op: String, rhs: List[Term])

  // Operators every value has (`Any`'s and `AnyRef`'s equality), whose operand they evaluate.
  private val universal: Set[String] = Set("==", "!=", "eq", "ne")

  /** The class a construction builds, as the walk spells it. */
  enum Constructs {

    /** A class named by `tpe`, whose simple name is `name`: `imported` when a block import around
      * it may bind the name, `creator` for a creator application, which builds nothing unless the
      * name is a supplied class's (it may be a method's or an object's `apply`).
      */
    case Named(tpe: Type, name: String, imported: Boolean, creator: Boolean)

    /** A class this reading does not follow. */
    case Unfollowed
  }

  object Constructs {

    /** The class an initializer's type names. */
    def of(tpe: Type, imported: Boolean): Constructs = tpe match {
      case Type.Name(name)                   => Named(tpe, name, imported, creator = false)
      case Type.Select(_, Type.Name(name))   => Named(tpe, name, imported, creator = false)
      case Type.Apply.After_4_6_0(callee, _) => of(callee, imported)
      case _                                 => Unfollowed
    }

  }

  /** `Any`'s members: every value has them, so selecting one evaluates its receiver. */
  val anyMembers: Set[String] = UniversalMembers.any

  /** A body returning a parameter declared with exactly its (non-contextual) result type: it
    * conforms where it is read, so no conversion is inserted and nothing is searched.
    */
  def returnsParameter(d: Defn.Def): Boolean = (d.body, d.decltpe) match {
    case (Term.Name(name), Some(result)) if !result.is[Type.ContextFunction] =>
      MemberSyntax
        .parameters(d.paramClauseGroups)
        .exists(p => p.name.value == name && p.decltpe.exists(_.structure == result.structure))
    case _ => false
  }

  /** The type a lambda's body has when the lambda is returned at a function type `expected`; none
    * for any other type, at which the body is read as not evaluated.
    */
  def functionResult(expected: Option[Type]): Option[Option[Type]] = expected.collect {
    case f: Type.Function => Some(f.res)
  }

}
