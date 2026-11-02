package cardinality.analysis.resolution

import scala.meta.*

import cardinality.analysis.*
import cardinality.analysis.model.*
import dev.constructive.eo.generics.lens

import MethodAnalysis.{Frame, Target}
import OverrideIdentity.{known, lexical, localsLens, Bindings, Key, MethodMember, Side, Spelled}

private[cardinality] object OverrideIdentity {

  /** A type's identity as override identity compares it, and whether it rests on a declaration
    * whose equalities the keying does not see (a name outside the sources, an opaque type).
    */
  final case class Spelled(spelling: String, tentative: Boolean)

  type Key = Option[Spelled]

  /** The key of a spelling that rests on nothing tentative. */
  def known(spelling: String): Key = Some(Spelled(spelling, tentative = false))

  /** A method member a template declares or defines. */
  type MethodMember = Decl.Def | Defn.Def

  // Immediate method/alias formals take precedence; enclosing binders compete with declarations
  // in lexical order. Scoped identities retain receiver substitutions across shadowing.
  final case class Bindings(
      locals: Map[String, Key],
      scoped: Map[(String, String), Key],
      expansionDepth: Int = 0
  )

  // One side of an override comparison: the scope its signature is read in (with `this` the
  // receiver) and the identities of its binders.
  final case class Side(context: ResolutionContext, bindings: Bindings)

  val localsLens = lens[Bindings](_.locals)
  val depthLens = lens[Bindings](_.expansionDepth)
  val aliasFrame = lens[Bindings](_.locals, _.expansionDepth)

  /** Whether an inherited declaration of a name is the measured value-like definition — a member
    * value, or a given alias — seen from its template: a value implements a parameterless member of
    * its name (SLS §5.1.4), so the two are one slot when neither takes a clause, and different
    * clause shapes are another signature. Matching a given's parameter types is left unknown.
    */
  def value(
      target: Target,
      name: String,
      groups: List[Member.ParamClauseGroup],
      step: InheritedScopes.Step
  ): Option[Boolean] =
    valueClauses(target.tree) match {
      case Some(_) if GivenNames.mayBe(target.tree, name) => None
      case Some(_) if !memberName(target).contains(name)  => Some(false)
      case Some(_) if !memberOf(target, step)             => None
      case Some(own) if own.isEmpty && groups.isEmpty     => Some(true)
      case Some(own) => Option.when(MemberSyntax.shape(own) != MemberSyntax.shape(groups))(false)
      case None      => Some(false)
    }

  /** The names of an inherited value or given declaration the measured definition may be itself,
    * and the reasons naming those whose identity is not known (kept among the names: neither
    * reading is safe).
    */
  def own(
      target: Target,
      step: InheritedScopes.Step,
      declaration: Stat
  ): (Set[String], List[String]) = {
    val answers = FinalImplementations
      .names(declaration)
      .map(name => name -> value(target, name, MemberSyntax.groups(declaration), step))
    (
      answers.collect { case (name, answer) if !answer.contains(false) => name }.toSet,
      answers.collect { case (name, None) => s"inherited override identity not resolved: $name" }
    )
  }

  /** The name the measured definition is a member under: an anonymous given's is the one Scala
    * synthesizes from its type (`given A = a` is `given_A`), any other its frame's.
    */
  def memberName(target: Target): Option[String] =
    MemberSyntax.anonymousGiven(target.tree).orElse(target.frame.name)

  /** Whether the measured definition is a member of the step's receiver: one nested elsewhere is
    * not known to be the receiver's member.
    */
  def memberOf(target: Target, step: InheritedScopes.Step): Boolean =
    target.frame.parent.exists(_.id == step.root.id)

  // A value-like definition's clauses: none for a value, a given's own.
  private def valueClauses(tree: Tree): Option[List[Member.ParamClauseGroup]] = tree match {
    case _: (Defn.Val | Defn.Var | Decl.Val | Decl.Var) => Some(Nil)
    case g: (Defn.GivenAlias | Decl.Given)              => Some(g.paramClauseGroups)
    case _                                              => None
  }

  /** A frame's class binders, each its own identity unless replaced, beside the replacements other
    * levels' binders already carry: an alias read at another level keys its binders as composed.
    */
  def lexical(frame: Frame, replacements: Map[(String, String), Key] = Map.empty): Bindings = {
    val own = frame.chain
      .flatMap(scope =>
        scope.typeParams
          .map(p => (scope.id -> p.name.value) -> known(s"binder:${scope.id}:${p.name.value}"))
      )
      .toMap
    Bindings(Map.empty, own ++ replacements)
  }

}

/** Override identity is nominal type identity, not equality of inhabitant representations.
  * Unsupported signatures deliberately leave an obligation instead of guessing an override.
  *
  * Two members are one slot when their names and parameter signatures match as seen from the
  * receiver (capabilities C9): each side's class binders are keyed through every `extends` clause
  * between its level and the receiver, so a grandparent's `A` is the argument the chain gives it.
  * Result types are not compared — SLS §5.1.3 matches members by parameter types; a matching member
  * whose result does not conform is a type error, not an overload, and the analyzed sources
  * typecheck — so equal parameters are an override, a covariant result included.
  */
final private[cardinality] class OverrideIdentity(resolver: Resolver) {
  private val keys = new OverrideKeys(resolver)

  /** Whether an inherited member is the measured declaration, seen from the target's template. */
  def matches(
      target: Target,
      declaration: MethodMember,
      step: InheritedScopes.Step,
      steps: List[InheritedScopes.Step]
  ): Option[Boolean] =
    target.tree match {
      case method: (Defn.Def | Decl.Def) if method.name.value == declaration.name.value =>
        if (!OverrideIdentity.memberOf(target, step)) None
        else {
          val current = Side(
            ResolutionContext(target.frame, resolver.typeParameters(target.frame), Set.empty),
            lexical(target.frame)
          )
          side(declaration, step.declared, step.root, steps)
            .flatMap(inherited => compare(declaration, inherited, method, current))
        }
      case _ =>
        OverrideIdentity.value(target, declaration.name.value, declaration.paramClauseGroups, step)
    }

  /** Whether two members of one receiver's linearization are one slot — an implementation and the
    * declaration it implements — each read at its own level.
    */
  def sameMember(
      left: (MethodMember, Frame),
      right: (MethodMember, Frame),
      root: Frame,
      steps: List[InheritedScopes.Step]
  ): Option[Boolean] =
    if (left._1.name.value != right._1.name.value) Some(false)
    // A member with another number of binders or parameters per clause is another signature.
    else if (arity(left._1) != arity(right._1)) Some(false)
    else
      for {
        leftSide <- side(left._1, left._2, root, steps)
        rightSide <- side(right._1, right._2, root, steps)
        matched <- compare(left._1, leftSide, right._1, rightSide)
      } yield matched

  private def arity(member: MethodMember): List[Int] = MemberSyntax.shape(member.paramClauseGroups)

  // A member read at its level, with `this` the receiver: its type members are the receiver's
  // (members M2), so both signatures key them alike. A private member is no inherited slot.
  private def side(
      member: MethodMember,
      level: Frame,
      root: Frame,
      steps: List[InheritedScopes.Step]
  ): Option[Side] =
    Option.unless(member.mods.exists(_.is[Mod.Private]))(()).flatMap { _ =>
      bindingsAt(level, root, steps, 0).map { bindings =>
        val scope = ResolutionContext(level, resolver.typeParameters(level), Set.empty)
        Side(
          if (level.id == root.id) scope
          else scope.as(MemberPath.self(root, resolver.typeParameters(root))),
          bindings
        )
      }
    }

  // A level's class binders as the receiver sees them: the receiver's own, or the keys of the
  // arguments the clause naming the level gives it, read under that clause's level — composed
  // along the walk, so every level is keyed in the receiver's binders.
  private def bindingsAt(
      level: Frame,
      root: Frame,
      steps: List[InheritedScopes.Step],
      depth: Int
  ): Option[Bindings] =
    if (level.id == root.id) Some(lexical(root))
    else if (depth > steps.size) None
    else
      InheritedScopes.at(steps, root, level).flatMap { step =>
        bindingsAt(step.from, root, steps, depth + 1).flatMap { from =>
          val caller = ResolutionContext(step.from, resolver.typeParameters(step.from), Set.empty)
          val side = Side(caller, from)
          keys.key(step.parent.tpe, side).map { _ =>
            val arguments = ResolutionOptics.typeArguments.getOption(step.parent.tpe).getOrElse(Nil)
            val replacements = step.declared.typeParams
              .map(p => (step.declared.id, p.name.value))
              .zip(arguments.map(keys.key(_, side)))
              .toMap
            lexical(step.declared, from.scoped ++ replacements)
          }
        }
      }

  private def compare(left: MethodMember, leftSide: Side, right: MethodMember, rightSide: Side) =
    for {
      leftSignature <- signature(left.paramClauseGroups, MemberSyntax.result(left), leftSide)
      rightSignature <- signature(
        right.paramClauseGroups,
        MemberSyntax.result(right),
        rightSide
      )
      leftConvention <- conventions(left.paramClauseGroups)
      rightConvention <- conventions(right.paramClauseGroups)
      matched <- sameSignature(leftSignature, rightSignature, leftConvention == rightConvention)
    } yield matched

  // Equal parameters are one slot whatever the results; different ones are an overload — unless a
  // key rests on a declaration whose equalities this keying cannot see.
  private def sameSignature(left: Spelled, right: Spelled, sameConvention: Boolean) =
    if (left.spelling != right.spelling) Option.unless(left.tentative || right.tentative)(false)
    else Option.when(sameConvention)(true)

  // The parameter signature: method binders keyed by position, every clause's types; a result
  // that depends on a term parameter, or binders past the first clause group, stay unknown.
  private def signature(
      groups: List[Member.ParamClauseGroup],
      result: Option[Type],
      side: Side
  ): Key = {
    val parameters = groups.flatMap(_.tparamClause.values)
    val method = parameters.zipWithIndex.map((p, index) => p.name.value -> known(s"method:$index"))
    val bound = side.copy(bindings = localsLens.replace(method.toMap)(side.bindings))
    if (groups.drop(1).exists(_.tparamClause.values.nonEmpty) || dependent(groups, result)) None
    else keys.combine(groups.map(groupKey(_, bound)))
  }

  private def dependent(groups: List[Member.ParamClauseGroup], result: Option[Type]): Boolean = {
    val terms = MemberSyntax.parameterNames(groups)
    val trees: List[Tree] = result.toList ++ groups
    trees.exists(_.collect {
      case selected: Type.Select => selected.qual.syntax.takeWhile(_ != '.')
    }.exists(terms))
  }

  private def groupKey(group: Member.ParamClauseGroup, side: Side): Key = {
    val types = group.tparamClause.values.map(parameterKey(_, side))
    val clauses = group.paramClauses.map { clause =>
      val params = clause.values.map { p =>
        for {
          // Scalameta repeats the clause's contextual modifier on each parameter.
          _ <- plainModifiers(
            p.mods.filterNot(mod => clause.mod.exists(_.structure == mod.structure))
          )
          tpe <- p.decltpe.flatMap(keys.key(_, side))
        } yield tpe
      }
      for {
        _ <- clauseKind(clause.mod)
        types <- keys.combine(params)
      } yield types
    }
    keys.combine(List(keys.combine(types), keys.combine(clauses)))
  }

  // `implicit` and `using` are two spellings of a contextual clause. A different calling
  // convention is not by itself proof of a separate inherited slot, so compare leaves it unknown.
  private def conventions(groups: List[Member.ParamClauseGroup]): Key =
    keys.combine(groups.flatMap(_.paramClauses).map(clause => clauseKind(clause.mod)))

  private def clauseKind(mod: Option[Mod]): Key = mod match {
    case None                                 => known("ordinary")
    case Some(_: Mod.Using | _: Mod.Implicit) => known("contextual")
    case _                                    => None
  }

  private def parameterKey(parameter: Type.Param, side: Side): Key =
    // A rigid higher-kinded binder is keyed by its kind beside its bounds: `F[_]` and `F[_, _]`
    // are different signatures.
    if (cardinality.types.Kinds.refusal(parameter).nonEmpty) None
    else {
      val bounds = parameter.bounds
      val bounded = List(bounds.lo.toList, bounds.hi.toList, bounds.context, bounds.view)
        .map(types => keys.combine(types.map(keys.key(_, side))))
      val kind = known(s"kind:${cardinality.types.Kinds.of(parameter).render}")
      plainModifiers(parameter.mods).flatMap(_ => keys.combine(kind :: bounded))
    }

  // An annotation does not distinguish an overload's parameter type or calling convention.
  // Other parameter modifiers need semantic normalization before we can compare.
  private def plainModifiers(mods: List[Mod]): Option[Unit] =
    Option.when(mods.forall(_.is[Mod.Annot]))(())

}
