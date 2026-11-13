package cardinality.compiled

import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.{ClassSymbol, Symbol, TermSymbol}
import tastyquery.Trees.{ClassDef, DefDef, Import, Tree, ValDef, ValOrDefDef}
import tastyquery.Types.{AppliedType, Type, TypeRef}

/** The anonymous contextual parameters implicit search finds for a definition (`Readings`): a
  * method's, a constructor's, a receiver's, each told apart from the candidates it competes with.
  */
final private[compiled] class Evidence(world: World)(using Context) {
  import Unread.Uncovered

  // An anonymous contextual parameter (of `evidence`) is found by implicit search alone. A body is
  // counted, not searched, and what one declares tells nothing of the others; what encloses its
  // method is shallower and loses (`ContextualScope` of the old front end). So its competitors are
  // the contextual parameters at its method or deeper (`competitors`) and the binders the measured
  // definition's context-function result puts around every body (`wrapping`, None where they are
  // not read), each of which must be told apart from it (`apart`).
  def searched(
      evidence: List[ValDef],
      competitors: List[ValDef],
      wrapping: Option[List[Type]]
  ): Either[Unread, Unit] = {
    val anonymous = evidence.filter(Readings.anonymous)
    if (anonymous.isEmpty) Right(())
    else
      wrapping
        .toRight(Uncovered("an anonymous contextual parameter beside a context-function result"))
        .flatMap(apart(anonymous, competitors, _))
  }

  def functionResult(result: Type) = World.classNamed(result).exists(Readings.function)

  // The binders a context-function result puts around the measured definition's body, clause after
  // clause: the parameters of the function literals at the head of its body, which the compiler
  // made from the result as it reads it; for a declaration without a body, the result's own
  // clauses, None where a context function is named but not read as one.
  def binders(tree: ValOrDefDef, result: Type): Option[List[Type]] = {
    def literals(body: Tree): List[Type] = body match {
      case Targets.Scope.Lambda(made) if Shapes.parameters(made).forall(_.symbol.isGivenOrUsing) =>
        Shapes.parameters(made).map(_.tpt.toType) ++ made.rhs.toList.flatMap(literals)
      case _ => Nil
    }
    def clauses(tpe: Type): Option[List[Type]] = tpe.dealias match {
      case applied: AppliedType if functionResult(applied) =>
        val types = applied.args.collect { case t: Type => t }
        Option
          .when(types.sizeIs == applied.args.size)(types)
          .flatMap(ts => clauses(ts.last).map(ts.init ++ _))
      case other => Option.unless(functionResult(other))(Nil)
    }
    val body = tree match {
      case d: DefDef => d.rhs
      case v: ValDef => v.rhs
    }
    body.map(literals).filter(_.nonEmpty).orElse(clauses(result))
  }

  // Whether search tells each anonymous parameter apart from every other contextual candidate it
  // competes with: one of its very type at its own site makes it ambiguous (refused); each other
  // one must be of a different class, read by its declaration or final (the old reading knows only
  // those classes' bases) and not `Null`, neither class extending the other; or of its class over
  // distinct rigid parameters (`unrelated`); or one is an unbounded rigid type parameter and the
  // other a distinct one or a class type neither top nor bottom (only those relate to every such
  // parameter in a body; the old `ContextualEvidence.unrelated` over atoms). A context function
  // also answers searches for its result: not told apart. A binder is declared by no site, so it
  // is never ambiguous. Otherwise it is not read yet. A refusal of the model is reported first, as
  // the old front end reports every one.
  private def apart(
      evidence: List[ValDef],
      parameters: List[ValDef],
      binders: List[Type]
  ): Either[Unread, Unit] = {
    def known(c: ClassSymbol) =
      c != defn.NullClass && (world.readByDeclaration(c) || Shapes.isFinal(c))
    def nominal(c: ClassSymbol) = !Evidence.Unordered(c.displayFullName) && !Readings.function(c)
    def told(mine: Type, theirs: Type) =
      (World.classNamed(mine), World.classNamed(theirs)) match {
        case (Some(a), Some(b)) if a != b =>
          known(a) && known(b) && !a.isSubClass(b) && !b.isSubClass(a)
        case (Some(a), Some(_)) => nominal(a) && Evidence.unrelated(mine, theirs)
        case (Some(c), None)    => nominal(c) && Readings.rigid(theirs).nonEmpty
        case (None, Some(c))    => nominal(c) && Readings.rigid(mine).nonEmpty
        case (None, None)       => Readings.rigid(mine).zip(Readings.rigid(theirs)).exists(_ != _)
      }
    val contextual = parameters.filter(p => Readings.contextual(p.symbol))
    val verdicts = for {
      p <- evidence
      mine = Evidence.seen(p)
      (theirs, site) <-
        contextual.filter(_ ne p).map(q => Evidence.seen(q) -> (q.symbol.owner == p.symbol.owner))
          ++ binders.map(Some(_) -> false)
    } yield (mine, theirs) match {
      case (Some(m), Some(t)) if site && m.isSameType(t) =>
        Left(Unread.Refused(s"ambiguous anonymous evidence: ${m.showBasic}"))
      case (Some(m), Some(t)) if told(m, t) => Right(())
      case _                                =>
        Left(Uncovered(s"an anonymous contextual parameter search may not tell apart: ${p.name}"))
    }
    verdicts
      .collectFirst { case refused @ Left(_: Unread.Refused) => refused }
      .orElse(verdicts.find(_.isLeft))
      .getOrElse(Right(()))
  }

  // Whether a class's body leaves its constructor's contextual parameters alone in a search from
  // inside it, as a member reads its receiver's (a constructor's own row searches no body):
  // written without parents or a self type, importing nothing and declaring nothing contextual but
  // its parameters (the old `ContextualScope` reads the template's candidates).
  private def quiet(owner: ClassSymbol): Boolean =
    owner.tree.exists { cls =>
      Shapes.parentless(cls) && World.ownSelf(owner) && cls.rhs.body.forall {
        case _: Import           => false
        case member: ValOrDefDef =>
          member.symbol.isParamAccessor || !Readings.contextual(member.symbol)
        case _ => true
      }
    }

  // The receivers whose anonymous contextual parameters (a context bound's evidence) search from
  // the measured definition finds: each such parameter competes with every contextual parameter at
  // its class or deeper, those of the classes inside it and the definition's own (`deeper`), as the
  // old `ContextualScope` places them (a shallower one loses), and every template from its class
  // down to the definition, objects included, is `quiet`. Any other receiver is left to
  // `Readings.plainInputs`, which leaves the row uncovered.
  def receiverSearch(
      around: List[ClassDef],
      measured: Symbol,
      deeper: List[ValDef]
  ): Either[Unread, Set[Symbol]] = {
    def inside(c: ClassSymbol) =
      World.owners(measured).collect { case t: ClassSymbol => t }.takeWhile(_ != c) :+ c
    val found = around.tails.collect {
      case c :: below
          if Shapes.parameters(c.rhs.constr).exists(Readings.anonymous) &&
            inside(c.symbol).forall(quiet) =>
        val parameters = Shapes.parameters(c.rhs.constr)
        val competitors =
          parameters ++ below.flatMap(k => Shapes.parameters(k.rhs.constr)) ++ deeper
        apart(parameters.filter(Readings.anonymous), competitors, Nil).map(_ => c.symbol)
    }.toList
    Traversals.sequence(found).map(_.toSet)
  }

}

private object Evidence {

  // The classes every unbounded type parameter may relate to: the tops, and `Null` and `Nothing`
  // below them.
  private val Unordered = Set(
    "scala.Any",
    "java.lang.Object",
    "scala.Matchable",
    "scala.Null",
    "scala.Nothing",
    "scala.Singleton"
  )

  // A parameter's type as its class's members see it: a primary constructor's parameter names the
  // constructor's copies of the class's type parameters, its field the class's own; none without
  // a field. A secondary constructor's parameters have no fields, and are compared among
  // themselves.
  private def seen(parameter: ValDef)(using Context): Option[Type] = {
    val owner = parameter.symbol.owner
    if (!Readings.primary(owner)) Some(parameter.tpt.toType)
    else
      owner.owner.asClass
        .getDecl(parameter.name)
        .collect { case field: TermSymbol if field.isParamAccessor => field.declaredType }
        .collect { case tpe: Type => tpe }
  }

  /** Whether two applications of one class are never subtypes of each other in a body: their
    * arguments are unbounded type parameters held rigid there (a method's, or the class's own
    * through `this`), distinct at some place, whatever the variance (`[A: Show, B: Show]`; the old
    * `ContextualEvidence.unrelated`, distinct atoms).
    */
  private def unrelated(left: Type, right: Type)(using Context): Boolean = {
    def parameters(tpe: Type) = tpe.dealias match {
      case applied: AppliedType =>
        val rigid = applied.args.collect { case ref: TypeRef => Shapes.rigidParameter(ref) }.flatten
        Option.when(rigid.sizeIs == applied.args.size)(rigid)
      case _ => None
    }
    parameters(left).zip(parameters(right)).exists(_.lazyZip(_).exists(_ != _))
  }

}
