package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.model.{ResolutionContext, ScopeTerms}
import cardinality.analysis.{MethodAnalysis, Resolver, SourceIndex, StdlibModel}
import cardinality.types.{Spelling, TypeName, TypeSubstitution}

import MethodAnalysis.{Frame, Resolved, Target}

/** The result type Scala gives a method written without one, where the definition's syntax alone
  * fixes it; otherwise the form that would need the compiler's type inference.
  *
  * A member that may override is read through the declaration it overrides (`OverriddenResults`).
  * Any other definition takes the type of its right-hand side, which syntax fixes for a literal
  * (its class: `true` is a `Boolean`, never the name `Boolean` in scope), an ascription to a type
  * that is no singleton, literal or union, written or behind an alias (Scala widens a singleton:
  * `(1: One)` with `type One = 1` is an `Int`), `new C[T]` or `new C` of a class with no type
  * parameters, an explicitly applied constructor `C[T](…)` of a source class no other term of its
  * name answers, and a call `f[T](…)` or `f(…)` of the one source method the name means, with a
  * declared result (`TermBindings`), widened as Scala widens an inferred one. Matching argument
  * types against parameters is inference and is not done: such a definition stays unresolved, its
  * reason naming the form.
  */
private[cardinality] object InferredResults {

  /** An inferred result: the type, and the context it is read in. */
  final case class Inferred(tpe: Type, context: ResolutionContext)

  val Unresolved = "inferred result type not resolved"

  /** A method target's result type and the context to read it in: the declared one where it has
    * one, read where it is written; otherwise the inferred one, given the inherited levels its
    * template reads. A `transparent inline` method's call sites see its right-hand side's own type,
    * which no reading here gives.
    */
  def declared(
      target: Target,
      steps: List[InheritedScopes.Step],
      variables: Map[String, Resolved],
      resolver: Resolver
  ): Either[String, Inferred] =
    (target.result, target.tree) match {
      case (Some(tpe), _) =>
        Right(Inferred(tpe, ResolutionContext(target.frame, variables, Set.empty)))
      case _ if TransparentResults.transparent(target) =>
        Left(s"$Unresolved: transparent inline method")
      case (None, d: Defn.Def) =>
        new OverriddenResults(target, d, variables, resolver)
          .result(steps)
          .getOrElse(new Reading(target, d.name.value, variables, resolver, steps).body(d.body))
      case _ => Left(Unresolved)
    }

  private val Singleton = "singleton"

  /** A literal's class, by a root-anchored path no declaration in scope can rebind: `true` and the
    * literal type `true` are a `Boolean`, never the name `Boolean` in scope.
    */
  def literalClass(literal: Lit): Option[Type] =
    (literal match {
      case _: Lit.Boolean => Some(List("scala", "Boolean"))
      case _: Lit.Int     => Some(List("scala", "Int"))
      case _: Lit.Long    => Some(List("scala", "Long"))
      case _: Lit.Double  => Some(List("scala", "Double"))
      case _: Lit.Float   => Some(List("scala", "Float"))
      case _: Lit.Char    => Some(List("scala", "Char"))
      case _: Lit.String  => Some(List("java", "lang", "String"))
      case _: Lit.Unit    => Some(List("scala", "Unit"))
      case _              => None
    }).map(rooted)

  /** A stdlib class by its root-anchored path, which no declaration in scope can rebind. */
  def rooted(path: List[String]): Type = {
    val prefix = path.init.foldLeft(Term.Name(SourceIndex.Root): Term.Ref)((qual, segment) =>
      Term.Select(qual, Term.Name(segment))
    )
    Type.Select(prefix, Type.Name(path.last))
  }

  /** The type binders of a definition's clauses, in order. */
  def binders(groups: List[Member.ParamClauseGroup]): List[String] =
    groups.flatMap(_.tparamClause.values.map(_.name.value))

  /** Whether a result names one of the definition's term parameters (a dependent result). */
  def dependent(result: Type, groups: List[Member.ParamClauseGroup]): Boolean =
    TypeName.words(result).exists(FinalImplementations.parameterNames(groups))

  /** A method's name, clauses and declared result, as a definition or a declaration writes them. */
  final case class Signature(
      name: String,
      groups: List[Member.ParamClauseGroup],
      result: Option[Type]
  )

  def signature(stat: Stat): Option[Signature] = stat match {
    case d: Defn.Def => Some(Signature(d.name.value, d.paramClauseGroups, d.decltpe))
    case d: Decl.Def => Some(Signature(d.name.value, d.paramClauseGroups, Some(d.decltpe)))
    case _           => None
  }

  /** An application as written: the callee's name, its explicit type arguments, how many explicit
    * argument clauses it applies, and where.
    */
  final private case class Call(callee: String, targs: List[Type], clauses: Int, at: Tree)

  /** A source method a call may mean, with its clauses and result. */
  final private case class Callee(
      scope: Frame,
      groups: List[Member.ParamClauseGroup],
      result: Option[Type]
  )

  final private class Reading(
      target: Target,
      name: String,
      variables: Map[String, Resolved],
      resolver: Resolver,
      steps: List[InheritedScopes.Step]
  ) {

    private def here(at: Tree): ResolutionContext =
      ResolutionContext(target.frame, variables, Set.empty, at = Some(target.input -> at.pos.start))

    private val terms =
      new TermBindings(InheritedScopes.levels(steps, _), resolver)

    /** The right-hand side's type, where syntax fixes it. */
    def body(rhs: Term): Either[String, Inferred] = rhs match {
      case Term.Block(List(only: Term))                     => body(only)
      case literal: Lit                                     => this.literal(literal)
      case Term.Ascribe(_, tpe)                             => ascribed(tpe)
      case Term.New(init)                                   => created(init)
      case _: Term.Apply | _: Term.Name | _: Term.ApplyType =>
        val (callee, clauses) = ExplicitClauses.of(rhs)
        applied(callee, clauses)
      case other => Left(s"$Unresolved: ${form(other)}")
    }

    private def literal(literal: Lit): Either[String, Inferred] =
      literalClass(literal)
        .map(Inferred(_, here(literal)))
        .toRight(s"$Unresolved: literal ${Spelling.of(literal)}")

    private def ascribed(tpe: Type): Either[String, Inferred] =
      widened(tpe, here(tpe), Set.empty)
        .map(kind => s"$Unresolved: $kind ascription")
        .toLeft(Inferred(tpe, here(tpe)))

    // What the compiler widens in a result it infers: a singleton or literal type, alone, in a union
    // (`1 | 2` becomes `Int`) or behind source aliases; a union of other types it keeps, and this
    // reading leaves alone as an ascription (3.9.0 fixtures).
    private def widened(tpe: Type, context: ResolutionContext, seen: Set[String]): Option[String] =
      tpe match {
        case _: Type.Singleton | _: Lit          => Some(Singleton)
        case TypeSubstitution.Union(left, right) =>
          List(left, right)
            .flatMap(widened(_, context, seen))
            .find(_ == Singleton)
            .orElse(Some("union"))
        case _ =>
          declared(tpe, context).flatMap { entry =>
            (entry.tree, TypeIdentities.key(entry)) match {
              case (alias: Defn.Type, key) if alias.tparamClause.values.isEmpty && !seen(key) =>
                val scope = ResolutionContext(entry.owner, Map.empty, Set.empty)
                widened(alias.body, scope, seen + key)
              case _ => None
            }
          }
      }

    // The source declaration a type written as a name means where it is read.
    private def declared(tpe: Type, context: ResolutionContext): Option[MethodAnalysis.TypeEntry] =
      Found.named(resolver, tpe, tpe.syntax, context).collect { case Found.Entry(entry) => entry }

    // `new C[T](…)` is a `C[T]`, `new C(…)` a `C` when the class takes no type parameters. A class
    // outside the sources is read as written, so its reading counts it or names what is absent;
    // only one the stdlib model gives type parameters needs them.
    private def created(init: Init): Either[String, Inferred] = init.tpe match {
      case applied: Type.Apply                     => Right(Inferred(applied, here(applied)))
      case named @ (_: Type.Name | _: Type.Select) =>
        val needing = s"$Unresolved: instance creation needing type arguments"
        Found.named(resolver, named, named.syntax, here(named)) match {
          case Some(Found.Entry(entry)) =>
            entry.tree match {
              case c: Defn.Class if c.tparamClause.values.isEmpty =>
                Right(Inferred(named, here(named)))
              case _: Defn.Class => Left(needing)
              case _             => Left(s"$Unresolved: instance creation of ${Spelling.of(named)}")
            }
          case Some(Found.Absent)
              if StdlibModel.entry(named.syntax).exists(_.parameters.nonEmpty) =>
            Left(needing)
          case _ => Right(Inferred(named, here(named)))
        }
      case _ => Left(s"$Unresolved: instance creation")
    }

    // An application's callee: a source class's constructor with explicit type arguments, or the
    // one source method a name means, with a declared result and every explicit clause applied.
    private def applied(callee: Term, clauses: Int): Either[String, Inferred] = {
      val (function, targs) = callee match {
        case t: Term.ApplyType => (t.fun, t.targClause.values)
        case other             => (other, Nil)
      }
      function match {
        case Term.Name(name) => call(Call(name, targs, clauses, callee))
        case other           => Left(s"$Unresolved: application of a ${form(other)}")
      }
    }

    private def call(call: Call): Either[String, Inferred] =
      constructor(call).getOrElse(method(call))

    private def constructor(call: Call): Option[Either[String, Inferred]] =
      declared(Type.Name(call.callee), here(call.at)).flatMap { entry =>
        entry.tree match {
          case c: Defn.Class if call.clauses > 0 && c.ctor.paramClauses.size == call.clauses =>
            Some(
              if (c.tparamClause.values.size != call.targs.size)
                Left(s"$Unresolved: creator application of ${call.callee} needing type arguments")
              else if (factory(entry) || shadowed(call.callee, entry.owner))
                Left(s"$Unresolved: application of ${call.callee}, which may name another term")
              else Right(Inferred(created(call), here(call.at)))
            )
          case _ => None
        }
      }

    private def created(call: Call): Type =
      if (call.targs.isEmpty) Type.Name(call.callee)
      else Type.Apply(Type.Name(call.callee), Type.ArgClause(call.targs))

    // A companion that declares or may inherit an `apply` makes `C[T](…)` a call of it.
    private def factory(entry: MethodAnalysis.TypeEntry): Boolean =
      resolver.modules.filter(_.path == entry.owner.path :+ entry.name).exists { module =>
        module.parents.nonEmpty || ScopeTerms.defined(module.stats)("apply")
      }

    // A term of the callee's name bound nearer than the class's declaring scope, or beside the
    // class there, is what the application calls.
    private def shadowed(callee: String, owner: Frame): Boolean = {
      val (nearer, rest) = target.frame.chain.span(frame => !sameScope(frame, owner))
      nearer.exists(terms.scope(callee, _).nonEmpty) ||
      rest.headOption.exists(terms.beside(callee, _, owner))
    }

    private def sameScope(frame: Frame, owner: Frame): Boolean =
      frame.id == owner.id ||
        (resolver.isPackage(frame) && resolver.isPackage(owner) && frame.path == owner.path)

    // The one method a simple name means: the nearest scope's definitions of it, where no import,
    // export or inherited member may bind it instead.
    private def method(call: Call): Either[String, Inferred] =
      if (call.callee == name) Left(s"$Unresolved: recursive call")
      else
        terms.nearest(call.callee, target.frame.chain) match {
          case TermBinding.Parameter       => Left(s"$Unresolved: application of a parameter")
          case TermBinding.Bound(why)      => Left(s"$Unresolved: call of ${call.callee}, $why")
          case TermBinding.Defined(scopes) => declaredIn(scopes, call)
          case TermBinding.Unbound         =>
            Left(s"$Unresolved: call of ${call.callee}, not a method an enclosing scope declares")
        }

    private def declaredIn(scopes: List[Frame], call: Call): Either[String, Inferred] = {
      val callees = scopes.flatMap(scope =>
        scope.stats.flatMap(stat =>
          (signature(stat), stat) match {
            case (Some(method), _) if method.name == call.callee =>
              List(Right(Callee(scope, method.groups, method.result)))
            case (_, e: Defn.ExtensionGroup)
                if ScopeTerms.extensionMethods(e).exists(_.name.value == call.callee) =>
              List(Left(s"$Unresolved: call of ${call.callee}, an extension method"))
            case _ => Nil
          }
        )
      )
      callees match {
        case List(Right(Callee(scope, groups, Some(result)))) =>
          called(scope, groups, result, call)
        case List(Right(_)) =>
          Left(s"$Unresolved: call of ${call.callee}, whose result is inferred")
        case _ =>
          Left(
            callees
              .collectFirst { case Left(reason) => reason }
              .getOrElse(s"$Unresolved: call of ${call.callee}, not one declared method")
          )
      }
    }

    private def called(
        owner: Frame,
        groups: List[Member.ParamClauseGroup],
        result: Type,
        call: Call
    ): Either[String, Inferred] = {
      val names = binders(groups)
      val explicit =
        groups.flatMap(_.paramClauses).count(clause => !ExplicitClauses.contextual(clause))
      if (names.size != call.targs.size)
        Left(s"$Unresolved: call of ${call.callee} needing type-argument inference")
      else if (explicit != call.clauses)
        Left(s"$Unresolved: partial application of ${call.callee}")
      else if (dependent(result, groups))
        Left(s"$Unresolved: dependent result of ${call.callee}")
      else {
        val arguments = names.zip(call.targs).map((b, t) => b -> resolver.resolve(t, here(t)))
        val context = ResolutionContext(
          owner,
          resolver.typeParameters(owner) ++ arguments,
          Set.empty,
          Some(target.frame)
        )
        widened(result, context, Set.empty) match {
          case Some(Singleton) => singleton(result, context, call)
          case _               => Right(Inferred(result, context))
        }
      }
    }

    // A call's singleton result as the compiler widens it: a literal type to its class, an object's
    // singleton to itself (its one value); any other — a value's, `this.type`, an alias of one, a
    // union holding one — is widened to a type this reading does not compute.
    private def singleton(
        result: Type,
        context: ResolutionContext,
        call: Call
    ): Either[String, Inferred] =
      result match {
        case lit: Lit => literalClass(lit).map(Inferred(_, context)).toRight(singletonResult(call))
        case _: Type.Singleton =>
          resolver.resolve(result, context) match {
            case Right(Inhabitation.Shape.Singleton(_, None)) => Right(Inferred(result, context))
            case _                                            => Left(singletonResult(call))
          }
        case _ => Left(singletonResult(call))
      }

    private def singletonResult(call: Call): String =
      s"$Unresolved: singleton result of a call of ${call.callee}"

    private def form(term: Term): String = term match {
      case _: Term.Block        => "block"
      case _: Term.If           => "conditional"
      case _: Term.Match        => "match"
      case _: Term.Function     => "lambda"
      case _: Term.Tuple        => "tuple"
      case _: Term.NewAnonymous => "anonymous class"
      case _: Term.Select       => "selection"
      case _                    => "expression"
    }

  }

}

/** An application's callee and how many explicit argument clauses it applies: `f[T](a)(b)` is
  * `f[T]` with two; a `using` clause is contextual and not counted.
  */
private object ExplicitClauses {

  def of(term: Term): (Term, Int) = term match {
    case a: Term.Apply if a.argClause.mod.exists(_.is[Mod.Using]) => of(a.fun)
    case a: Term.Apply                                            =>
      val (callee, clauses) = of(a.fun)
      (callee, clauses + 1)
    case other => (other, 0)
  }

  def contextual(clause: Term.ParamClause): Boolean =
    clause.mod.exists(m => m.is[Mod.Using] || m.is[Mod.Implicit])

}
