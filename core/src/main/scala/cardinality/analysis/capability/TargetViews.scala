package cardinality.analysis.capability

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.model.{MemberSyntax, ScopeTerms}

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

  /** One member's body: its tree, the names its parameters bind, and its declared result type
    * (`None` when inferred).
    */
  final case class Body(tree: Tree, parameters: Set[String], result: Option[Type])

  /** What a member's body (a template's, read loosely) reads of its receiver. */
  def reads(body: Body, self: Self, scope: Scope): Reads = {
    val walk = new Walk(scope, self)
    body.tree match {
      case term: Term => walk.result(term, body.parameters, body.result)
      case other      => walk.loose(other, body.parameters)
    }
    walk.reads.copy(searches = !body.tree.is[Lit])
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
    def result(term: Term, bound: Set[String], expected: Option[Type]): Unit = term match {
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

    // A block's imports bind only inside it.
    private def importing(body: => Unit): Unit = {
      val enclosing = imports
      body
      imports = enclosing
    }

    private def evaluate(term: Tree, strict: Boolean, bound: Set[String]): Unit = term match {
      case name: Term.Name                    => named(name.value, Applied.bare, strict, bound)
      case here: Term.This                    => thisRef(here, bound)
      case select: Term.Select                => selection(select, Applied.bare, strict, bound)
      case Term.ApplyType.After_4_6_0(fun, _) => evaluate(fun, strict, bound)
      case apply: Term.Apply                  => application(apply, strict, bound)
      case Term.ApplyInfix.After_4_6_0(lhs, op, _, args) =>
        operator(Infix(lhs, op.value, args.values), strict, bound)
      case Term.ApplyUnary(op, arg) =>
        evaluate(arg, strict && qualifier(arg, s"unary_${op.value}", Applied.bare, bound), bound)
      case Term.Tuple(args)                      => args.foreach(evaluate(_, typed(strict), bound))
      case Term.Ascribe(expr, _)                 => evaluate(expr, typed(strict), bound)
      case Term.Annotate(expr, _)                => evaluate(expr, strict, bound)
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
    private def named(name: String, applied: Applied, strict: Boolean, bound: Set[String]): Unit =
      if (bound(name)) ()
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

    private def thisRef(here: Term.This, bound: Set[String]): Unit =
      receiver(here, bound) match {
        case Some(Left(template)) => reach(template, "")
        case _                    => escapes = true
      }

    // `this`, the template's own `T.this` or its self alias (Right), or an enclosing template's
    // `Outer.this` (Left).
    private def receiver(term: Term, bound: Set[String]): Option[Either[String, Unit]] =
      term match {
        case Term.This(_: Name.Anonymous)                             => Some(Right(()))
        case Term.This(name) if self.template(name.value)             => Some(Right(()))
        case Term.This(name)                                          => Some(Left(name.value))
        case Term.Name(alias) if self.aliases(alias) && !bound(alias) => Some(Right(()))
        case _                                                        => None
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
        bound: Set[String]
    ): Unit = {
      val name = select.name.value
      (select.qual, receiver(select.qual, bound)) match {
        case (Term.Super(owner, _), _) => parent(owner, name, applied)
        case (_, Some(Right(_)))       => scope.own(name, applied).foreach(record(_, strict))
        case (_, Some(Left(template))) => reach(template, name)
        case (qual, None)              =>
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

    private def qualifier(qual: Term, name: String, applied: Applied, bound: Set[String]): Boolean =
      !imports.open && !imports.names(name) &&
        scope.evaluatesQualifier(member(qual, bound), name, applied, qual.pos.start)

    // The receiver member a qualifier names, if it is one: a bare name or one selected on `this`.
    private def member(qual: Term, bound: Set[String]): Option[String] =
      chain(qual, bound).collect { case List(name) => name }

    // The names of a selection chain rooted at a bare name or at `this`.
    private def chain(term: Term, bound: Set[String]): Option[List[String]] = term match {
      case Term.Name(name) if !bound(name) && !self.aliases(name) => Some(List(name))
      case Term.Select(qualifier, Term.Name(name))                =>
        if (receiver(qualifier, bound).contains(Right(()))) Some(List(name))
        else chain(qualifier, bound).map(_ :+ name)
      case _ => None
    }

    // A call: its head is evaluated (a member applied as its clauses say); its arguments are, where
    // the callee's declaration says they are by value. A head naming a local binding or a member of
    // one has no declaration this reading looks up.
    private def application(apply: Term.Apply, strict: Boolean, bound: Set[String]): Unit = {
      val (head, clauses, typeArguments) = spine(apply, Nil, typeArguments = false)
      val contextual = clauses.map(_.mod.exists(_.is[Mod.Using]))
      val values = clauses.zip(contextual).collect { case (clause, false) => clause }
      val applied = Applied(values.size, values.headOption.fold(0)(_.values.size))
      head match {
        case Term.Name(name)     => named(name, applied, strict, bound)
        case select: Term.Select => selection(select, applied, strict, bound)
        case other               => evaluate(other, strict, bound)
      }
      val known = head match {
        case Term.Name(name)                      => !bound(name)
        case Term.Select(Term.Name(qualifier), _) => !bound(qualifier) && !self.aliases(qualifier)
        case _                                    => false
      }
      val byValue =
        if (known) scope.byValue(head, contextual, typeArguments)
        else List.fill(contextual.size)(false)
      clauses.zip(byValue).foreach { (clause, eager) =>
        clause.values.foreach(evaluate(_, typed(strict && eager), bound))
      }
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
    private def operator(infix: Infix, strict: Boolean, bound: Set[String]): Unit = {
      val Infix(lhs, op, rhs) = infix
      val rightAssociative = op.endsWith(":")
      val (callee, operands) = if (rightAssociative) (rhs, List(lhs)) else (List(lhs), rhs)
      val applied = Applied(1, operands.size)
      callee.foreach { term =>
        receiver(term, bound) match {
          case Some(Right(_)) => scope.own(op, applied).foreach(record(_, strict))
          case _ => evaluate(term, strict && qualifier(term, op, applied, bound), bound)
        }
      }
      val eager = !rightAssociative && universal(op) && !scope.deferredOperand(op)
      operands.foreach(evaluate(_, strict && eager, bound))
    }

    // Statements in order; a lazy value or a method body is not evaluated by the block. Every local
    // definition binds its name in the whole block: Scala rejects a reference that would reach past
    // a local value to an outer one, so no name the block defines means a member anywhere in it.
    private def block(stats: List[Stat], strict: Boolean, bound: Set[String]): Set[String] = {
      val scope = bound ++ stats.flatMap(local)
      stats.foreach {
        case v: Defn.Val =>
          val eager = strict && !v.mods.exists(_.is[Mod.Lazy])
          evaluate(v.rhs, if (v.decltpe.isEmpty) eager else typed(eager), scope)
        case d: Defn.Def =>
          evaluate(
            d.body,
            strict = false,
            scope ++ MemberSyntax.parameterNames(d.paramClauseGroups)
          )
        case term: Term => evaluate(term, strict, scope)
        case i: Import  =>
          imports = ScopeTerms
            .imported(List(i))
            .fold(imports.copy(open = true))(names => imports.copy(names = imports.names ++ names))
          loose(i, scope)
        case other => loose(other, scope)
      }
      scope
    }

    private def local(stat: Stat): List[String] =
      MemberSyntax.templateName(stat).fold(MemberSyntax.defined(stat))(List(_))

    // Anything this walk does not read: every name in it a possible mention, nested templates
    // included (`new H()` names H as a type).
    def loose(tree: Tree, bound: Set[String]): Unit =
      tree.traverse {
        case Term.Name(name) if !bound(name) =>
          if (self.aliases(name)) escapes = true
          else {
            scope.own(name, Applied.bare).foreach(r => possible ++= r.slots)
            elsewhere(name)
          }
        case Type.Name(name) => possible ++= scope.named(name)
        case here: Term.This => thisRef(here, bound)
      }

  }

  // The names block imports bring into scope, or `open` when one may bring any name.
  final private case class Imports(open: Boolean, names: Set[String])

  // An infix application: its left operand, operator and right operands.
  final private case class Infix(lhs: Term, op: String, rhs: List[Term])

  // Operators every value has (`Any`'s and `AnyRef`'s equality), whose operand they evaluate.
  private val universal: Set[String] = Set("==", "!=", "eq", "ne")

  /** `Any`'s members: every value has them, so selecting one evaluates its receiver. */
  val anyMembers: Set[String] = Set(
    "==",
    "!=",
    "equals",
    "hashCode",
    "toString",
    "##",
    "getClass",
    "isInstanceOf",
    "asInstanceOf"
  )

  /** The type a lambda's body has when the lambda is returned at `expected`: `Some(None)` for an
    * inferred type, `None` when the lambda is converted to a type that is no function — a SAM or
    * another record, observable without applying it.
    */
  def functionResult(expected: Option[Type]): Option[Option[Type]] = expected match {
    case None                          => Some(None)
    case Some(f: Type.Function)        => Some(Some(f.res))
    case Some(f: Type.ContextFunction) => Some(Some(f.res))
    case Some(_)                       => None
  }

}
