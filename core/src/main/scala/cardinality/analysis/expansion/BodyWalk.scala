package cardinality.analysis.expansion

import scala.collection.mutable
import scala.meta.*

import cardinality.analysis.model.MemberSyntax
import cardinality.analysis.{MethodAnalysis, Resolver, SourceIndex}
import cardinality.types.Trees.{hasCase, hasInline, ClassDefn, TemplateDefn}
import cardinality.types.{Spelling, TypeApplications, TypeSubstitution}

import MethodAnalysis.Frame

/** A name a body binds itself: a value with its declared type or its right-hand side (`via` spells
  * a lambda's parameter, which an inline call may bind to its argument), a local method or given
  * alias read like a supplied one, a local object or given instance read through its frame, or a
  * local class, trait or enum, whose name as a term is its companion.
  */
private[expansion] enum Local {
  case Typed(tpe: Option[Type], rhs: Option[Term], scope: Frame, via: Option[String] = None)
  case Method(definition: Definition)
  case Instance(frame: Frame)
  case Class(tree: ClassDefn)
}

/** Where a walk stands: the frame names are read from (a definition nested in the body opens its
  * own), the names the body bound so far, the type variables a type test may not name.
  */
final private[expansion] case class Scope(
    frame: Frame,
    locals: Map[String, Local],
    variables: Set[String]
) {
  def bind(names: Map[String, Local]): Scope = copy(locals = locals ++ names)
  def typed(binders: Iterable[String]): Scope = copy(variables = variables ++ binders)
}

/** Reads an inline method's body for what would make one expansion differ from another (register
  * D27): `inline if` and `inline match`, quotes and splices, `scala.compiletime` and
  * `scala.quoted`, a call of an inline method that does (`inlined` judges it), a given the body
  * leaves to implicit search, a member an expansion selects on a narrower class than the body's
  * (`Receiver.Open`), and a runtime class an expansion instantiates. Every name is resolved; one
  * that is not resolved refuses. Each check answers the first thing it finds, as a phrase (`uses
  * inline match`).
  */
final private[expansion] class BodyWalk(
    terms: BodyTerms,
    inlined: Definition => Option[String],
    fallback: (RestatedExtensions.Applied, Frame, Option[Meaning]) => Option[String],
    resolver: Resolver
) {

  private val typesOf = new BodyTypes(terms, resolver, pathMeaning)

  private val applyMember = RestatedExtensions.Applied("apply", None)

  /** The first reason a definition's body, or a default of its parameters, makes its expansions
    * differ, if any.
    */
  def body(term: Term, frame: Frame, defaults: List[Term]): Option[String] = {
    val scope = Scope(frame, Map.empty, frame.chain.flatMap(_.typeParams).map(_.name.value).toSet)
    all(defaults)(walk(_, scope)).orElse(walk(term, scope))
  }

  // The meanings each walk under way resolved, innermost first: what its body references.
  private val heard = mutable.Stack.empty[mutable.ListBuffer[Meaning]]

  private def noted(meaning: Meaning): Meaning = {
    heard.headOption.foreach(_ += meaning)
    meaning
  }

  /** `body`, with the meanings of the names and paths the walk resolved on the way. */
  def read(term: Term, frame: Frame, defaults: List[Term]): (Option[String], List[Meaning]) = {
    heard.push(mutable.ListBuffer.empty)
    try {
      val found = body(term, frame, defaults)
      (found, heard.head.toList)
    } finally heard.pop(): Unit
  }

  /** Whether a type spells a context function type, or names an alias of one. */
  def contextual(tpe: Type): Boolean = typesOf.contextual(tpe)

  private def all[A](items: Iterable[A])(check: A => Option[String]): Option[String] =
    items.iterator.flatMap(check).nextOption()

  private def walk(tree: Tree, scope: Scope): Option[String] = tree match {
    case term: Term => form(term).orElse(expression(term, scope))
    case other      => children(other, scope)
  }

  // A form an expansion decides at its call site, or one this reading does not follow.
  private def form(term: Term): Option[String] = term match {
    case t: Term.If if t.mods.hasInline    => Some("uses inline if")
    case t: Term.Match if t.mods.hasInline => Some("uses inline match")
    case _: (Term.QuotedMacroExpr | Term.QuotedMacroType | Term.SplicedMacroExpr) =>
      Some("uses a quote or a splice")
    case _: (Term.For | Term.ForYield) =>
      Some("uses a for expression, whose calls are not resolved")
    case _: Term.Xml             => Some("uses an XML literal")
    case _: Term.ContextFunction =>
      Some("writes a context function literal, which implicit search applies where it is read")
    case t: Term.Super => Some(s"selects through ${Spelling.of(t)}, not resolved")
    case _             => None
  }

  private def expression(term: Term, scope: Scope): Option[String] = term match {
    case t: Term.Interpolate                                             => interpolation(t, scope)
    case _: Lit | _: Term.This | _: Term.Placeholder | _: Term.EndMarker => None
    // A thrown value is no result (D26): what an expansion throws changes nothing counted.
    case _: Term.Throw        => None
    case t: Term.Block        => block(t.stats, scope)
    case t: Term.Function     => lambda(t.paramClause.values, t.body, scope)
    case t: Term.PolyFunction => walk(t.body, scope.typed(t.tparamClause.values.map(_.name.value)))
    case t: Term.Match        => walk(t.expr, scope).orElse(cases(t.casesBlock.cases, scope))
    case t: Term.PartialFunction => cases(t.cases, scope)
    case t: Term.Try             =>
      walk(t.expr, scope)
        .orElse(cases(t.catchClause.toList.flatMap(_.cases), scope))
        .orElse(t.finallyp.flatMap(walk(_, scope)))
    case t: Term.Ascribe      => walk(t.expr, scope).orElse(types(t.tpe, scope))
    case t: Term.New          => created(t.init, scope)
    case t: Term.NewAnonymous => anonymous(t, scope)
    case other                => application(other, scope).getOrElse(children(other, scope))
  }

  private def children(tree: Tree, scope: Scope): Option[String] = all(tree.children) {
    case term: Term => walk(term, scope)
    case tpe: Type  => types(tpe, scope)
    case other      => children(other, scope)
  }

  private def types(tpe: Type, scope: Scope): Option[String] = typesOf.types(tpe, scope)

  private def tested(tpe: Type, scope: Scope): Option[String] =
    typesOf.reified(tpe, scope, "tests the type")

  // An application, a selection or a name: the call it makes, then its arguments.
  private def application(term: Term, scope: Scope): Option[Option[String]] = term match {
    case _: (Term.Apply | Term.ApplyType | Term.Name | Term.Select | Term.ApplyInfix |
          Term.ApplyUnary | Term.SelectPostfix) =>
      val found = Chain.of(term)
      Some(
        head(found, scope)
          .orElse(all(found.targs)(types(_, scope)))
          .orElse(all(found.args)(argument(_, scope)))
      )
    case _ => None
  }

  // A named argument `p = e` evaluates `e`; `p` names a parameter, not a value in scope.
  private def argument(arg: Term, scope: Scope): Option[String] = arg match {
    case Term.Assign(_: Term.Name, rhs) => walk(rhs, scope)
    case other                          => walk(other, scope)
  }

  private def head(found: Chain, scope: Scope): Option[String] = found.head match {
    case Term.Select(q, Term.Name("isInstanceOf")) =>
      qualifier(q, scope).orElse(all(found.targs)(tested(_, scope)))
    case s: Term.Select =>
      qualifier(s.qual, scope).orElse {
        val meaning = selection(s, scope)
        val call = RestatedExtensions.Call(
          () => receiver(s.qual, scope),
          meaning,
          found.args.size,
          found.clauses
        )
        selected(
          meaning,
          RestatedExtensions.Applied(s.name.value, Some(call)),
          found.clauses,
          scope,
          pathMeaning(s.qual, scope)
        )
      }
    case n: Term.Name =>
      val named = meaning(n, scope)
      check(named, found.clauses, scope).orElse(reifies(named, found.targs, scope))
    case operator: (Term.ApplyInfix | Term.ApplyUnary | Term.SelectPostfix) =>
      operatorCall(Operator(operator), found.clauses, scope)
    case other =>
      walk(other, scope).orElse(
        Option
          .when(found.clauses.nonEmpty)(
            check(applyOf(receiver(other, scope)), found.clauses, scope)
          )
          .flatten
      )
  }

  // `classOf[Array[A]]` is the class of the array an expansion's type argument makes (fixture
  // review-A6 r2 c2), a type test by another name.
  private def reifies(meaning: Meaning, targs: List[Type], scope: Scope): Option[String] =
    meaning match {
      case Meaning.Call(name, _, _) if LibraryInlines.reifying(name) =>
        all(targs)(typesOf.reified(_, scope, s"reifies with $name the class of"))
      case _ => None
    }

  // The operands are evaluated, then the operator is selected on its receiver; an infix operator
  // takes its right operand as one clause of arguments.
  private def operatorCall(op: Operator, clauses: List[Boolean], scope: Scope): Option[String] =
    all(op.operands)(walk(_, scope))
      .orElse(all(op.targs)(types(_, scope)))
      .orElse(
        op.receiver.fold(Some(s"applies ${op.name} to no operand")) { operand =>
          selected(
            terms.selectOn(receiver(operand, scope), op.name),
            RestatedExtensions.Applied(op.name, None),
            if (op.infix) false :: clauses else clauses,
            scope,
            pathMeaning(operand, scope)
          )
        }
      )

  // A member selected with arguments is applied only if it accepts them; otherwise Scala tries an
  // extension or a conversion of the name, which the sources may declare (`fallback`), a
  // conversion of the qualifier's static type (`qualifier`, the path it denotes, when it is one).
  private def selected(
      meaning: Meaning,
      applied: RestatedExtensions.Applied,
      clauses: List[Boolean],
      scope: Scope,
      qualifier: Option[Meaning] = None
  ): Option[String] =
    check(meaning, clauses, scope).orElse(
      Option.when(clauses.nonEmpty)(applied).flatMap(fallback(_, scope.frame, qualifier))
    )

  // A qualifier is evaluated, so its own calls are checked; a path is only resolved.
  private def qualifier(qual: Term, scope: Scope): Option[String] = qual match {
    case _: (Term.Name | Term.This) => None
    case s: Term.Select             => qualifier(s.qual, scope)
    case other                      => walk(other, scope)
  }

  /** Whether calling what `meaning` denotes, with the given clauses, is one function of its values.
    * A clause the callee does not take applies its result's `apply` (`further`).
    */
  private def check(meaning: Meaning, clauses: List[Boolean], scope: Scope): Option[String] =
    noted(meaning) match {
      case value @ Meaning.Value(Some(tpe), _, _) if contextual(tpe) =>
        contextValue(value, tpe, clauses, scope)
      case Meaning.Methods(definitions)   => all(definitions)(definition(_, clauses, scope))
      case Meaning.Call(_, result, arity) => further(result, clauses.drop(arity), scope)
      case Meaning.Value(_, _, _) | Meaning.Instance(_) if clauses.nonEmpty =>
        selected(applyOf(value(meaning)), applyMember, clauses, scope, Some(meaning))
      case Meaning.Module(_) if clauses.nonEmpty =>
        selected(guarded(terms.select(meaning, "apply")), applyMember, clauses, scope)
      case Meaning.Library(module) if clauses.nonEmpty =>
        if (LibraryInlines.module(module).exists(_("apply")))
          check(terms.select(meaning, "apply"), clauses, scope)
        else Some(s"applies scala.$module, which the library table gives no apply")
      case Meaning.Companion(c: Defn.Class) if clauses.nonEmpty =>
        val declared = contextualClauses(c)
        leftToSearch(s"${c.name.value}.apply", declared, clauses).orElse(
          further(Receiver.Unread(s"a new ${c.name.value}"), Clauses.rest(declared, clauses), scope)
        )
      case Meaning.Package(path, _) if clauses.nonEmpty =>
        Some(s"applies the package ${path.mkString(".")}")
      case other => other.refusal
    }

  // Clauses left once a callee has taken its own apply its result's `apply`: `Obj.g(x)` with `def
  // g: Fn` is `Obj.g.apply(x)`, and Fn's inline apply is expanded at each call site (fixture
  // review-A6 r2 m1).
  private def further(result: Receiver, clauses: List[Boolean], scope: Scope): Option[String] =
    Option.when(clauses.nonEmpty)(selected(applyOf(result), applyMember, clauses, scope)).flatten

  private def applyOf(receiver: Receiver): Meaning = guarded(terms.selectOn(receiver, "apply"))

  // An `apply` reached by applying a value. One taking no clause would apply its own result's in
  // turn, which this reading does not follow (it is also how a chain of them would never end).
  private def guarded(apply: Meaning): Meaning = apply match {
    case Meaning.Methods(definitions) if definitions.forall(contextualClauses(_).isEmpty) =>
      Meaning.Refused("applies an apply member that takes no argument clause, not followed")
    case other => other
  }

  // A value of a context function type is applied, wherever it is read without an explicit `using`
  // clause, to a given implicit search chooses at the definition — an inline given among them is
  // expanded at each call site (fixture review-A6 h4).
  private def contextValue(
      value: Meaning.Value,
      tpe: Type,
      clauses: List[Boolean],
      scope: Scope
  ): Option[String] =
    if (clauses.headOption.contains(true) && typesOf.topContext(tpe))
      selected(applyOf(terms.receiverOf(value)), applyMember, clauses, scope)
    else
      Some(
        s"reads ${value.phrase}, of the context function type ${Spelling.of(tpe)}, which implicit " +
          "search applies where it is read"
      )

  /** The receiver a meaning evaluates to when it is read as a value. */
  private def value(meaning: Meaning): Receiver = meaning match {
    case v: Meaning.Value             => terms.receiverOf(v)
    case Meaning.Instance(receiver)   => receiver
    case call: Meaning.Call           => terms.unapplied(call)
    case Meaning.Methods(definitions) =>
      // A method that takes a clause, read unapplied, is the function eta-expansion makes.
      if (definitions.forall(d => Clauses.remaining(contextualClauses(d), Nil).nonEmpty))
        declaredResult(definitions)
      else Receiver.Unread(s"the function ${definitions.headOption.fold("")(_.name)}")
    case Meaning.Module(frame)   => Receiver.Template(frame)
    case Meaning.Library(module) =>
      LibraryInlines
        .moduleClass(module)
        .fold(Receiver.Unread(s"scala.$module"))(Receiver.Library(_))
    case other => Receiver.Unread(other.phrase)
  }

  // A supplied method's declared result, when every overload declares the same one, read with the
  // method's own type parameters as variables.
  private def declaredResult(definitions: List[Definition]): Receiver =
    definitions.map(d => MemberSyntax.result(d.tree).map(_.syntax)).distinct match {
      case List(Some(_)) =>
        val first = definitions.head
        terms.receiver(
          MemberSyntax.result(first.tree),
          first.frame.parent.getOrElse(first.frame),
          first.frame.typeParams.map(_.name.value).toSet
        )
      case _ => Receiver.Unread(s"the result of ${Definition.named(definitions)}")
    }

  // A supplied method a call selects: inline ones must pass themselves; none may take or return a
  // context function, or leave a contextual clause to the compiler's search, whose given this
  // reading does not check; clauses beyond its own apply its result.
  private def definition(callee: Definition, clauses: List[Boolean], scope: Scope): Option[String] =
    Option
      .when(MemberSyntax.inline(callee.tree))(callee)
      .flatMap(inlined)
      .map(inner => s"calls inline method ${callee.name}, which $inner")
      .orElse(contextSignature(callee))
      .orElse(leftToSearch(callee.name, contextualClauses(callee), clauses))
      .orElse(
        further(
          declaredResult(List(callee)),
          Clauses.rest(contextualClauses(callee), clauses),
          scope
        )
      )

  // An extension method's clauses follow its group's receiver clauses.
  private def calleeClauses(callee: Definition): List[Term.ParamClause] =
    group(callee).toList.flatMap(_.paramClauseGroup.toList).flatMap(_.paramClauses) ++
      MemberSyntax.groups(callee.tree).flatMap(_.paramClauses)

  private def group(callee: Definition): Option[Defn.ExtensionGroup] =
    callee.frame.parent.flatMap(_.tree).collect { case e: Defn.ExtensionGroup => e }

  // Each clause flagged contextual; a context bound adds a clause after them.
  private def contextualClauses(callee: Definition): List[Boolean] =
    calleeClauses(callee).map(MemberSyntax.contextual) ++
      Option.when(
        (callee.frame :: callee.frame.parent.filter(_ => group(callee).nonEmpty).toList)
          .exists(_.typeParams.exists(_.bounds.context.nonEmpty))
      )(true)

  private def contextualClauses(c: Defn.Class): List[Boolean] =
    c.ctor.paramClauses.toList.map(MemberSyntax.contextual)

  // A context function in a callee's parameters or result: a value read through it may be applied
  // by implicit search (and a lambda argument's parameter may be one).
  private def contextSignature(callee: Definition): Option[String] =
    (calleeClauses(callee).flatMap(_.values).flatMap(_.decltpe) ++ MemberSyntax.result(callee.tree))
      .find(contextual)
      .map(tpe =>
        s"calls ${callee.name}, whose signature has the context function type ${Spelling.of(tpe)}, " +
          "which implicit search may apply"
      )

  private def leftToSearch(
      label: String,
      declared: List[Boolean],
      written: List[Boolean]
  ): Option[String] =
    Option.when(Clauses.searched(declared, written))(
      s"leaves a given of $label to implicit search, whose choice is not checked"
    )

  private def meaning(name: Term.Name, scope: Scope): Meaning =
    scope.locals.get(name.value) match {
      case Some(Local.Typed(Some(tpe), _, declared, via)) =>
        if (polyBound(tpe, declared, scope)) Meaning.Instance(Receiver.Variable)
        else Meaning.Value(Some(tpe), declared, via)
      // A local's type is fixed where the body is typed (fixture a6-fix1 d1: `val y = b; y.f(x)`
      // is not re-selected in an expansion), so it keeps no argument's class.
      case Some(Local.Typed(None, rhs, _, _)) =>
        Meaning.Instance(
          rhs.fold(Receiver.Unread(s"the type of ${name.value}"))(r => fixed(receiver(r, scope)))
        )
      case Some(Local.Method(definition)) => Meaning.Methods(List(definition))
      case Some(Local.Instance(frame))    => Meaning.Module(frame)
      case Some(Local.Class(tree))        => companion(tree)
      case None                           => terms.named(name.value, scope.frame, name.pos.start)
    }

  // A type a polymorphic function literal binds is no type its frame declares: a value of it has
  // no class this reading may look up by name.
  private def polyBound(tpe: Type, declared: Frame, scope: Scope): Boolean =
    TypeSubstitution
      .typeNames(tpe)
      .exists(name => scope.variables(name) && !declared.bindsTypeParam(name))

  private def companion(tree: ClassDefn): Meaning = tree match {
    case c: Defn.Class if c.mods.hasCase => Meaning.Companion(c)
    case e: Defn.Enum                    => Meaning.Companion(e)
    case other                           =>
      Meaning.Refused(
        s"reads the local ${MemberSyntax.templateName(other).getOrElse("type")}, " +
          "which has no companion this reading reads"
      )
  }

  private def fixed(receiver: Receiver): Receiver = receiver match {
    case Receiver.Open(inner, _) => inner
    case other                   => other
  }

  private def selection(s: Term.Select, scope: Scope): Meaning =
    pathMeaning(s.qual, scope) match {
      case Some(prefix) => terms.select(prefix, s.name.value)
      case None         => terms.selectOn(receiver(s.qual, scope), s.name.value)
    }

  // A qualifier that is a name or a path of names: what it denotes, package or value.
  private def pathMeaning(qual: Term, scope: Scope): Option[Meaning] = (qual match {
    case Term.Name(SourceIndex.Root) => Some(Meaning.Package(Nil, supplied = true))
    case n: Term.Name                => Some(meaning(n, scope))
    case t: Term.This                => Some(Meaning.Instance(enclosing(scope, t.qual)))
    case s: Term.Select => pathMeaning(s.qual, scope).map(terms.select(_, s.name.value))
    case _              => None
  }).map(noted)

  // `this` is the innermost template's, `C.this` the template named C around the body (an open
  // one may be a subclass at the call site, fixture review-A6 r2 o1).
  private def enclosing(scope: Scope, qual: Name): Receiver = {
    val templates = scope.frame.chain.filter(_.isTemplate)
    qual match {
      case _: Name.Anonymous =>
        templates.headOption.fold(Receiver.Unread("this outside a template"))(terms.thisOf(_))
      case named =>
        templates
          .find(_.path.lastOption.contains(named.value))
          .fold(Receiver.Unread(s"${named.value}.this, which names no template around the body"))(
            terms.thisOf(_, s"${named.value}.this")
          )
    }
  }

  /** The receiver an expression evaluates to, as far as its syntax tells. */
  private def receiver(term: Term, scope: Scope): Receiver = term match {
    case lit: Lit                                 => literal(lit)
    case _: (Term.Name | Term.Select | Term.This) =>
      pathMeaning(term, scope).fold(Receiver.Unread(excerpt(term)))(value)
    case a: (Term.Apply | Term.ApplyType) =>
      val found = Chain.of(a)
      resultOf(callee(found.head, scope), found.clauses, viaApply = false)
    case t: Term.Tuple        => Receiver.Library(s"Tuple${t.args.size}")
    case n: Term.New          => terms.receiver(Some(n.init.tpe), scope.frame, scope.variables)
    case n: Term.NewAnonymous => Receiver.Template(SourceIndex.anonymousFrame(scope.frame, n))
    case a: Term.Ascribe      => terms.receiver(Some(a.tpe), scope.frame, scope.variables)
    case f: Term.Function     => Receiver.Library(s"Function${f.paramClause.values.size}")
    case operator: (Term.ApplyInfix | Term.ApplyUnary | Term.SelectPostfix) =>
      val op = Operator(operator)
      op.receiver.fold(Receiver.Unread(excerpt(term))) { operand =>
        resultOf(
          terms.selectOn(receiver(operand, scope), op.name),
          if (op.infix) List(false) else Nil,
          viaApply = false
        )
      }
    case b: Term.Block =>
      b.stats.lastOption
        .collect { case last: Term => last }
        .fold(Receiver.Unread("a block"))(receiver(_, scope.bind(locals(b.stats, scope))))
    case other => Receiver.Unread(excerpt(other))
  }

  // What an application's head denotes.
  private def callee(head: Term, scope: Scope): Meaning = head match {
    case s: Term.Select             => selection(s, scope)
    case _: (Term.Name | Term.This) =>
      pathMeaning(head, scope).getOrElse(Meaning.Instance(Receiver.Unread(excerpt(head))))
    case other => Meaning.Instance(receiver(other, scope))
  }

  // What applying a callee to the written clauses returns: a method's declared result, a function
  // value's result, the result of an `apply` member, a library member's result — each clause the
  // callee does not take applying its result's `apply` in turn.
  private def resultOf(meaning: Meaning, clauses: List[Boolean], viaApply: Boolean): Receiver =
    meaning match {
      case _ if clauses.isEmpty         => value(meaning)
      case Meaning.Methods(definitions) =>
        definitions.map(d => Clauses.remaining(contextualClauses(d), clauses)).distinct match {
          case List(Some(more)) if !(viaApply && more == clauses) =>
            applied(declaredResult(definitions), more)
          case _ => Receiver.Unread(s"the result of ${meaning.phrase}")
        }
      case call: Meaning.Call =>
        if (clauses.size < call.arity) Receiver.Unread(call.phrase)
        else applied(call.result, clauses.drop(call.arity))
      case Meaning.Value(Some(f: Type.Function), declared, _) =>
        applied(terms.receiver(Some(f.res), declared), clauses.tail)
      case Meaning.Library(module) if LibraryInlines.module(module).exists(_("apply")) =>
        resultOf(terms.select(meaning, "apply"), clauses, viaApply = true)
      case _: Meaning.Module =>
        resultOf(terms.select(meaning, "apply"), clauses, viaApply = true)
      case _: (Meaning.Value | Meaning.Instance) =>
        resultOf(terms.selectOn(value(meaning), "apply"), clauses, viaApply = true)
      case other => Receiver.Unread(s"the result of ${other.phrase}")
    }

  private def applied(result: Receiver, clauses: List[Boolean]): Receiver =
    if (clauses.isEmpty) result
    else resultOf(terms.selectOn(result, "apply"), clauses, viaApply = true)

  private def literal(lit: Lit): Receiver = lit match {
    case _: Lit.Int     => Receiver.Library("Int")
    case _: Lit.Long    => Receiver.Library("Long")
    case _: Lit.Boolean => Receiver.Library("Boolean")
    case _: Lit.Char    => Receiver.Library("Char")
    case _: Lit.String  => Receiver.Library("String")
    case _: Lit.Double  => Receiver.Library("Double")
    case _: Lit.Float   => Receiver.Library("Float")
    case _              => Receiver.Unread(Spelling.of(lit))
  }

  /** A tree as a reason quotes it: one line, its first words when it is long. */
  private def excerpt(tree: Tree): String = {
    val spelled = Spelling.of(tree)
    if (spelled.length <= BodyWalk.Excerpt) spelled
    else spelled.take(BodyWalk.Excerpt).reverse.dropWhile(!_.isWhitespace).reverse.trim + " …"
  }

  // ---- definitions inside the body

  private def block(stats: List[Stat], scope: Scope): Option[String] = {
    val inner = scope.bind(locals(stats, scope))
    all(stats)(statement(_, inner))
  }

  // A companion object read by name is the object, wherever its class is written.
  private def locals(stats: List[Stat], scope: Scope): Map[String, Local] =
    stats
      .flatMap(local(_, scope))
      .sortBy {
        case (_, _: Local.Class) => 0
        case _                   => 1
      }
      .toMap

  private def local(stat: Stat, scope: Scope): List[(String, Local)] = stat match {
    case Valued(pats, tpe, rhs)          => values(pats, tpe, rhs, scope)
    case d: (Defn.Def | Defn.GivenAlias) =>
      MemberSyntax
        .names(d)
        .filter(_.nonEmpty)
        .map(_ -> Local.Method(Definition(d, SourceIndex.methodFrame(d, scope.frame))))
    case o: Defn.Object =>
      List(o.name.value -> Local.Instance(SourceIndex.templateFrame(scope.frame, o)))
    case g: Defn.Given if g.name.value.nonEmpty =>
      List(g.name.value -> Local.Instance(SourceIndex.givenFrame(scope.frame, g)))
    case d: ClassDefn => List(d.name.value -> Local.Class(d))
    case _            => Nil
  }

  private def values(
      pats: List[Pat],
      tpe: Option[Type],
      rhs: Option[Term],
      scope: Scope
  ): List[(String, Local)] =
    pats match {
      case List(Pat.Var(name)) => List(name.value -> Local.Typed(tpe, rhs, scope.frame))
      case _ => TypeApplications.boundNames(pats).map(_ -> Local.Typed(None, None, scope.frame))
    }

  private def statement(stat: Stat, scope: Scope): Option[String] = stat match {
    case term: Term                  => walk(term, scope)
    case Valued(pats, declared, rhs) =>
      declared
        .flatMap(types(_, scope))
        .orElse(rhs.flatMap(walk(_, scope)))
        .orElse(destructured(pats, scope))
    case d: Defn.Def => method(SourceIndex.methodFrame(d, scope.frame), d.decltpe, d.body, scope)
    case g: Defn.GivenAlias =>
      method(SourceIndex.methodFrame(g, scope.frame), Some(g.decltpe), g.body, scope)
    case g: Defn.Given   => template(SourceIndex.givenFrame(scope.frame, g), scope, created)
    case d: TemplateDefn =>
      hiding(d, scope).orElse(template(SourceIndex.templateFrame(scope.frame, d), scope, inherits))
    case i: Import    => Some(s"imports inside the body: ${Spelling.of(i)} (not followed)")
    case d: Defn.Type => hiding(d, scope).orElse(types(d.body, scope))
    case _            => children(stat, scope)
  }

  // A local definition read where it is written: its clauses' types and defaults (a default is
  // evaluated at each call, fixture review-A6 r2 l5), its declared result, its body — under its
  // own frame, so its type parameters are its own.
  private def method(
      frame: Frame,
      result: Option[Type],
      body: Term,
      scope: Scope
  ): Option[String] = {
    val inner = within(frame, scope)
    clauses(inner).orElse(result.flatMap(types(_, inner))).orElse(walk(body, inner))
  }

  // A local template: its parameters, its parents (read around it, its parameters in scope), its
  // body under its own frame, where `this` and its inherited members are its own (fixture
  // review-A6 r2 a1).
  private def template(
      frame: Frame,
      scope: Scope,
      parent: (Init, Scope) => Option[String]
  ): Option[String] = {
    val inner = within(frame, scope)
    clauses(inner)
      .orElse(all(frame.parents)(parent(_, binding(frame, scope))))
      .orElse(block(frame.stats, inner))
  }

  private def anonymous(n: Term.NewAnonymous, scope: Scope): Option[String] =
    template(SourceIndex.anonymousFrame(scope.frame, n), scope, created)

  private def within(frame: Frame, scope: Scope): Scope = binding(frame, scope).copy(frame = frame)

  // A definition's binders and parameters in scope, as its parent clauses read them.
  private def binding(frame: Frame, scope: Scope): Scope =
    scope.typed(frame.typeParams.map(_.name.value)).bind(parameters(frame.params, frame))

  private def clauses(scope: Scope): Option[String] =
    all(scope.frame.params)(p =>
      p.decltpe.flatMap(types(_, scope)).orElse(p.default.flatMap(walk(_, scope)))
    )

  // A local type named like one the reading resolves outside the body would be read as that one.
  private def hiding(d: Member, scope: Scope): Option[String] =
    d.name match {
      case name: Type.Name =>
        Option.when(typesOf.denoted(name, scope.frame).exists(_.fold(_.tree ne d, _ => true)))(
          s"declares the local type ${name.value}, which hides a type of that name this reading " +
            "resolves outside the body"
        )
      case _ => None
    }

  // `val (x, _) = …` matches a pattern: its extractors and type tests are the body's.
  private def destructured(pats: List[Pat], scope: Scope): Option[String] =
    pats match {
      case List(_: Pat.Var) => None
      case _                => patterns(pats, scope)
    }

  private def parameters(
      params: List[Term.Param],
      frame: Frame,
      open: Boolean = false
  ): Map[String, Local] =
    params.map { p =>
      val name = p.name.value
      name -> Local.Typed(p.decltpe, None, frame, Option.when(open)(name))
    }.toMap

  // A lambda's parameters: an inline call that receives the lambda applies it to its own
  // arguments, which keep their classes (fixture a6-fix1 d1, `viaLambda`).
  private def lambda(params: List[Term.Param], body: Term, scope: Scope): Option[String] =
    all(params.flatMap(_.decltpe))(types(_, scope))
      .orElse(walk(body, scope.bind(parameters(params, scope.frame, open = true))))

  private def cases(cs: List[Case], scope: Scope): Option[String] = all(cs) { c =>
    val inner = scope.bind(
      TypeApplications.boundNames(List(c.pat)).map(_ -> Local.Typed(None, None, scope.frame)).toMap
    )
    pattern(c.pat, scope).orElse(c.cond.flatMap(walk(_, inner))).orElse(walk(c.body, inner))
  }

  private def arguments(init: Init, scope: Scope): Option[String] =
    all(init.argClauses.flatMap(_.values))(argument(_, scope))

  // `new C(…)`: the class's constructor is no inline method, but it may take a contextual clause.
  private def created(init: Init, scope: Scope): Option[String] = parentClause(init, scope)(Some(_))

  // A named class's parent clause: its arguments run when an instance is made, a supplied class's
  // contextual clause among them; another parent is read where its members are.
  private def inherits(init: Init, scope: Scope): Option[String] =
    parentClause(init, scope) {
      case parent: Receiver.Template => Some(parent)
      case _                         => None
    }

  private def parentClause(init: Init, scope: Scope)(
      built: Receiver => Option[Receiver]
  ): Option[String] =
    types(init.tpe, scope)
      .orElse(arguments(init, scope))
      .orElse(
        built(terms.receiver(Some(init.tpe), scope.frame, scope.variables))
          .flatMap(construction(_, init))
      )

  private def construction(built: Receiver, init: Init): Option[String] = built match {
    case Receiver.Template(frame) =>
      frame.tree
        .collect { case c: Defn.Class => c }
        .flatMap(c =>
          leftToSearch(
            s"new ${c.name.value}",
            contextualClauses(c),
            init.argClauses.map(_.mod.exists(_.is[Mod.Using])).toList
          )
        )
    case Receiver.Library(key) if LibraryInlines.members(key).nonEmpty => None
    case Receiver.Library(key)   => Some(s"creates a $key, which the library table does not list")
    case Receiver.Variable       => Some("creates an instance of an abstract type")
    case Receiver.Unread(why)    => Some(s"creates an instance of a class not read: $why")
    case Receiver.Open(inner, _) => construction(inner, init)
  }

  private def interpolation(t: Term.Interpolate, scope: Scope): Option[String] =
    Option
      .unless(Set("s", "raw", "f")(t.prefix.value))(
        s"uses the interpolator ${t.prefix.value}, an extension not resolved"
      )
      .orElse(all(t.args)(walk(_, scope)))

  // ---- patterns

  private def pattern(pat: Pat, scope: Scope): Option[String] = pat match {
    case _: (Pat.Var | Pat.Wildcard | Pat.SeqWildcard | Lit) => None
    case p: Pat.Bind         => pattern(p.lhs, scope).orElse(pattern(p.rhs, scope))
    case p: Pat.Typed        => pattern(p.lhs, scope).orElse(tested(p.rhs, scope))
    case p: Pat.Given        => tested(p.tpe, scope)
    case p: Pat.Extract      => extractor(p.fun, scope).orElse(patterns(p.argClause.values, scope))
    case p: Pat.ExtractInfix =>
      extractor(p.op, scope).orElse(patterns(p.lhs :: p.argClause.values, scope))
    case p: Pat.Tuple       => patterns(p.args, scope)
    case p: Pat.Alternative => pattern(p.lhs, scope).orElse(pattern(p.rhs, scope))
    case _: Pat.Repeated    => None
    case stable: Term       => pathMeaning(stable, scope).flatMap(_.refusal)
    case other              => Some(s"matches a pattern not read: ${excerpt(other)}")
  }

  private def patterns(pats: List[Pat], scope: Scope): Option[String] = all(pats)(pattern(_, scope))

  private def extractor(fun: Term, scope: Scope): Option[String] =
    pathMeaning(fun, scope) match {
      case Some(Meaning.Library(_) | Meaning.Companion(_) | _: Meaning.Call) => None
      case Some(found @ Meaning.Module(_))                                   =>
        check(terms.select(found, "unapply"), List(false), scope)
      case Some(other) if other.refusal.nonEmpty => other.refusal
      case _ => Some(s"matches with ${excerpt(fun)}, an extractor not resolved")
    }

}

private[expansion] object BodyWalk {

  /** How much of a tree a reason quotes. */
  val Excerpt = 60

}
