package cardinality.analysis.expansion

import scala.meta.*

import cardinality.analysis.MethodAnalysis.Frame
import cardinality.analysis.model.MemberSyntax
import cardinality.types.TypeApplications

/** A value a body binds itself: its declared type, or its right-hand side. */
final private[expansion] case class Local(tpe: Option[Type], rhs: Option[Term])

/** Where a walk stands: the frame names are read from, the names the body bound so far, the type
  * variables of the frames around it.
  */
final private[expansion] case class Scope(
    frame: Frame,
    locals: Map[String, Local],
    variables: Set[String]
) {
  def bind(names: Map[String, Local]): Scope = copy(locals = locals ++ names)
}

/** Reads an inline method's body for what would make one expansion differ from another (register
  * D27): `inline match`, quotes and splices, `scala.compiletime` and `scala.quoted`, a call of an
  * inline method that does (`inlined` judges it), a given the body leaves to implicit search, and a
  * member an expansion selects on a narrower class than the body's (`Receiver.Open`). Every name is
  * resolved; one that is not resolved refuses, and so does a form this reading does not follow.
  * Each check answers the first thing it finds, as a phrase (`uses inline match`).
  */
final private[expansion] class BodyWalk(
    terms: BodyTerms,
    inlined: Definition => Option[String],
    fallback: (String, Frame) => Option[String]
) {

  private val typesOf = new BodyTypes(terms, pathMeaning)

  /** The first reason a definition's body, or a default of its parameters, makes its expansions
    * differ, if any.
    */
  def body(term: Term, frame: Frame, defaults: List[Term]): Option[String] = {
    val scope = Scope(frame, Map.empty, frame.chain.flatMap(_.typeParams).map(_.name.value).toSet)
    all(defaults)(walk(_, scope)).orElse(walk(term, scope))
  }

  private def all[A](items: Iterable[A])(check: A => Option[String]): Option[String] =
    items.iterator.flatMap(check).nextOption()

  private def walk(term: Term, scope: Scope): Option[String] =
    form(term).orElse(expression(term, scope))

  // A form an expansion decides at its call site.
  private def form(term: Term): Option[String] = term match {
    case t: Term.Match if inlineMod(t.mods) => Some("uses inline match")
    case _: (Term.QuotedMacroExpr | Term.QuotedMacroType | Term.SplicedMacroExpr) =>
      Some("uses a quote or a splice")
    case _ => None
  }

  private def expression(term: Term, scope: Scope): Option[String] = term match {
    case t: Term.Block    => block(t.stats, scope)
    case t: Term.Function => walk(t.body, scope.bind(parameters(t.paramClause.values)))
    case t: Term.Match    =>
      walk(t.expr, scope).orElse(Some("matches on cases this reading does not follow"))
    case t: Term.New => created(t.init, scope)
    case other       => application(other, scope).getOrElse(children(other, scope))
  }

  private def inlineMod(mods: List[Mod]): Boolean = mods.exists(_.is[Mod.Inline])

  private def children(tree: Tree, scope: Scope): Option[String] = all(tree.children) {
    case term: Term => walk(term, scope)
    case _          => Some("reads a part of an expression this reading does not follow")
  }

  private def types(tpe: Type, scope: Scope): Option[String] = typesOf.types(tpe, scope)

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
    case s: Term.Select =>
      qualifier(s.qual, scope).orElse(
        selected(selection(s, scope), s.name.value, found.clauses, scope)
      )
    case n: Term.Name       => check(meaning(n, scope), found.clauses, scope)
    case i: Term.ApplyInfix =>
      all(i.lhs :: i.argClause.values)(walk(_, scope))
        .orElse(Some(s"applies ${i.op.value}, which this reading does not resolve"))
    case _ => Some("applies an expression this reading does not resolve")
  }

  // A member selected with arguments is applied only if it accepts them; otherwise Scala tries an
  // extension or a conversion of the name, which the sources may declare (`fallback`).
  private def selected(
      meaning: Meaning,
      name: String,
      clauses: List[Boolean],
      scope: Scope
  ): Option[String] =
    check(meaning, clauses, scope).orElse(
      Option.when(clauses.nonEmpty)(name).flatMap(fallback(_, scope.frame))
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
    meaning match {
      case Meaning.Methods(definitions)   => all(definitions)(definition(_, clauses, scope))
      case Meaning.Call(_, result, arity) => further(result, clauses.drop(arity), scope)
      case Meaning.Value(_, _, _) | Meaning.Instance(_) if clauses.nonEmpty =>
        selected(applyOf(value(meaning)), "apply", clauses, scope)
      case Meaning.Module(_) if clauses.nonEmpty =>
        selected(terms.select(meaning, "apply"), "apply", clauses, scope)
      case other => other.refusal
    }

  // Clauses left once a callee has taken its own apply its result's `apply`: `Obj.g(x)` with `def
  // g: Fn` is `Obj.g.apply(x)`, and Fn's inline apply is expanded at each call site (fixture
  // review-A6 r2 m1).
  private def further(result: Receiver, clauses: List[Boolean], scope: Scope): Option[String] =
    Option.when(clauses.nonEmpty)(selected(applyOf(result), "apply", clauses, scope)).flatten

  private def applyOf(receiver: Receiver): Meaning = terms.selectOn(receiver, "apply")

  /** The receiver a meaning evaluates to when it is read as a value. */
  private def value(meaning: Meaning): Receiver = meaning match {
    case v: Meaning.Value => terms.receiverOf(v)
    case _                => Receiver.Unread("a value this reading does not type")
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
      case _ => Receiver.Unread("the result of overloads")
    }

  // A supplied method a call selects: inline ones must pass themselves; none may leave a contextual
  // clause to the compiler's search, whose given this reading does not check; clauses beyond its
  // own apply its result.
  private def definition(callee: Definition, clauses: List[Boolean], scope: Scope): Option[String] =
    Option
      .when(MemberSyntax.inline(callee.tree))(callee)
      .flatMap(inlined)
      .map(inner => s"calls inline method ${callee.name}, which $inner")
      .orElse(leftToSearch(callee.name, contextualClauses(callee), clauses))
      .orElse(
        further(
          declaredResult(List(callee)),
          Clauses.rest(contextualClauses(callee), clauses),
          scope
        )
      )

  // Each clause flagged contextual; a context bound adds a clause after them.
  private def contextualClauses(callee: Definition): List[Boolean] =
    MemberSyntax.groups(callee.tree).flatMap(_.paramClauses).map(MemberSyntax.contextual) ++
      Option.when(callee.frame.typeParams.exists(_.bounds.context.nonEmpty))(true)

  private def leftToSearch(
      label: String,
      declared: List[Boolean],
      written: List[Boolean]
  ): Option[String] =
    Option.when(Clauses.searched(declared, written))(
      s"leaves a given of $label to implicit search, whose choice is not checked"
    )

  // A local's type is fixed where the body is typed (fixture a6-fix1 d1: `val y = b; y.f(x)` is
  // not re-selected in an expansion), so it keeps no argument's class.
  private def meaning(name: Term.Name, scope: Scope): Meaning =
    scope.locals.get(name.value) match {
      case Some(Local(None, rhs)) =>
        Meaning.Instance(
          rhs.fold(Receiver.Unread(s"the type of ${name.value}"))(receiver(_, scope))
        )
      case Some(_) => Meaning.Refused(s"reads the typed local ${name.value}, not followed")
      case None    => terms.named(name.value, scope.frame, name.pos.start)
    }

  private def selection(s: Term.Select, scope: Scope): Meaning =
    pathMeaning(s.qual, scope)
      .map(terms.select(_, s.name.value))
      .getOrElse(Meaning.Refused("selects on an expression this reading does not type"))

  // A qualifier that is a name or a path of names: what it denotes, package or value.
  private def pathMeaning(qual: Term, scope: Scope): Option[Meaning] = qual match {
    case n: Term.Name   => Some(meaning(n, scope))
    case s: Term.Select => pathMeaning(s.qual, scope).map(terms.select(_, s.name.value))
    case _              => None
  }

  /** The receiver an application evaluates to: a method's declared result. */
  private def receiver(term: Term, scope: Scope): Receiver = term match {
    case a: (Term.Apply | Term.ApplyType) =>
      val found = Chain.of(a)
      found.head match {
        case n: Term.Name => resultOf(meaning(n, scope), found.clauses)
        case _            => Receiver.Unread("an application this reading does not type")
      }
    case _ => Receiver.Unread("an expression this reading does not type")
  }

  // What applying a method to the written clauses returns: its declared result.
  private def resultOf(meaning: Meaning, clauses: List[Boolean]): Receiver =
    meaning match {
      case Meaning.Methods(definitions) =>
        definitions.map(d => Clauses.remaining(contextualClauses(d), clauses)).distinct match {
          case List(Some(Nil)) => declaredResult(definitions)
          case _               => Receiver.Unread("the result of a partial application")
        }
      case _ => Receiver.Unread("the result of a call this reading does not type")
    }

  // ---- definitions inside the body

  private def block(stats: List[Stat], scope: Scope): Option[String] = {
    val inner = scope.bind(stats.flatMap(local).toMap)
    all(stats)(statement(_, inner))
  }

  private def local(stat: Stat): List[(String, Local)] = stat match {
    case Valued(List(Pat.Var(name)), tpe, rhs) => List(name.value -> Local(tpe, rhs))
    case Valued(pats, _, _) => TypeApplications.boundNames(pats).map(_ -> Local(None, None))
    case _                  => Nil
  }

  private def statement(stat: Stat, scope: Scope): Option[String] = stat match {
    case term: Term          => walk(term, scope)
    case Valued(pats, _, rhs) => rhs.flatMap(walk(_, scope)).orElse(destructured(pats))
    case _ => Some("defines inside the body what this reading does not follow")
  }

  // `val (x, _) = …` matches a pattern: its extractors and type tests are the body's.
  private def destructured(pats: List[Pat]): Option[String] =
    pats match {
      case List(_: Pat.Var) => None
      case _                => patterns(pats)
    }

  // A lambda's parameters: values whose type this reading does not follow.
  private def parameters(params: List[Term.Param]): Map[String, Local] =
    params.map(p => p.name.value -> Local(p.decltpe, None)).toMap

  // `new C(…)`: its type and its arguments; the instance itself is not followed.
  private def created(init: Init, scope: Scope): Option[String] =
    types(init.tpe, scope)
      .orElse(all(init.argClauses.flatMap(_.values))(argument(_, scope)))
      .orElse(Some(s"creates an instance of ${init.tpe.syntax}, not followed"))

  // ---- patterns

  private def pattern(pat: Pat): Option[String] = pat match {
    case _: (Pat.Var | Pat.Wildcard | Pat.SeqWildcard | Lit) => None
    case p: Pat.Tuple                                        => patterns(p.args)
    case _ => Some("matches a pattern this reading does not follow")
  }

  private def patterns(pats: List[Pat]): Option[String] = all(pats)(pattern)

}
