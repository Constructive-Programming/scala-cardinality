package cardinality.capacity

import scala.meta.*

import cardinality.types.*

object Counter {

  // Trees are parsed as Scala 3: printing one under any other dialect reprints it under Scala 2
  // rules, which cannot spell Scala 3's modifiers (`inline def` throws while printing).
  private given scala3: Dialect = dialects.Scala3

  /** What every definition a source introduces holds together: the sum over its concrete classes,
    * enums, modules and top-level values, nested definitions included; the supplied sources are in
    * scope too, the way `definitions` reads them.
    */
  def source(s: Source, library: Library = Library.empty): Size =
    Walk.body(s.stats, Scope.empty.withLibrary(library))

  /** Every definition a source introduces, in source order, each with the cardinality of its type
    * and the names that stopped the calculator from bounding it — the number and the reason.
    * `library` lets a sibling file's type resolve instead of counting as unknown; `Library.empty`
    * reads the source on its own.
    */
  def definitions(s: Source, library: Library = Library.empty): List[Definition] =
    Walk.of(s.stats, Scope.empty.withLibrary(library), Nil, top = true).definitions

  /** What the definitions of a source declare: every typed member contributes the size of its
    * declared type, added rather than multiplied, so two `String => String` methods and a `String`
    * field in one object report `2ε₀ + ω`. See `signatureIn` for what counts as a member.
    */
  def sourceSignature: Source => Size = s => signatureBody(s.stats, Scope.empty)

  def stat: Stat => Size = statIn(Scope.empty)

  /** A single definition's member signature, summed as in `sourceSignature`. */
  def defnSignature: Defn => Size = signatureIn(Scope.empty)

  def defn: Defn => Size = defnIn(Scope.empty)

  def ctor: Ctor.Primary => Size = ctorIn(Scope.empty)

  def param: Term.Param => Size = paramIn(Scope.empty)

  def `type`: Type => Size = typeIn(Scope.empty)

  private[cardinality] def equations(stats: List[Stat]): List[(TypeName, Named)] = {
    val modules = stats.flatMap {
      case d: Defn.Object => Some(nameOf(d) -> Named(Nil, (_: Scope) => UnitSize))
      case _              => None
    }
    val types = stats.flatMap {
      case d: Defn.Type if isOpaqueAlias(d) =>
        Some(nameOf(d) -> Named(parameterNames(d), (_: Scope) => UnitSize))
      case d: Defn.Type =>
        // A match type's body is read on the *argument's* syntax when the alias is applied (see
        // `instantiation`), so the body travels with the definition.
        val matchType = d.body match {
          case m: Type.Match => Some(m)
          case _             => None
        }
        Some(
          nameOf(d) -> Named(
            parameterNames(d),
            sc => typeIn(sc)(d.body),
            matchType
          )
        )
      case d: Defn.Enum => Some(nameOf(d) -> Named(parameterNames(d), sc => enumSize(sc)(d)))
      case d: Defn.Class if isConcreteClass(d) =>
        Some(nameOf(d) -> Named(parameterNames(d), sc => ctorIn(sc)(d.ctor)))
      case _ => None
    }
    val defined = modules ++ types
    defined ++ sealedSums(stats, defined.map(_._1).toSet).toList.map {
      case (parent, children) =>
        // A sealed parent's equation is the sum of its children, and it keeps their names: a
        // report reads the parent's value from it, and the children's reasons are the parent's.
        parent -> Named(
          Nil,
          (sc: Scope) =>
            children.foldLeft(NothingSize: Size)((acc, child) =>
              acc + sc.getOrElse(child, NothingSize)
            ),
          children = children
        )
    }
  }

  // The type parameters a definition declares, in the order its arguments are supplied in.
  private[cardinality] def parameters(d: Defn): List[Type.Param] = d match {
    case c: Defn.Class => c.tparamClause.values
    case c: Defn.Trait => c.tparamClause.values
    case c: Defn.Enum  => c.tparamClause.values
    case c: Defn.Type  => c.tparamClause.values
    case _             => Nil
  }

  // The parameters as binders: a parameter that takes parameters of its own (`F[_]`) is the
  // higher-kinded kind, and every applied `F[A]` then depends on the instantiation too.
  private[cardinality] def binders(d: Defn): List[Binder] =
    parameters(d).map(param =>
      Binder(TypeName.of(param.name.value), param.tparamClause.values.size)
    )

  private[cardinality] def parameterNames(d: Defn): List[TypeName] =
    parameters(d).map(p => TypeName.of(p.name.value))

  // The name a definition is known by, as the maps below key it.
  private[cardinality] def nameOf(d: Defn): TypeName = Walk.named(d)

  // The modifier tests the case arms repeat: abstract classes contribute no inhabitants of their
  // own, opaque aliases a fixed single one, and `sealed` marks the pass-through parents.
  private[cardinality] def isConcreteClass(d: Defn.Class): Boolean = !isAbstractClass(d)

  private[cardinality] def isAbstractClass(d: Defn.Class): Boolean =
    d.mods.exists(_.is[Mod.Abstract])

  private[cardinality] def isSealed(d: Defn.Class | Defn.Trait): Boolean =
    d.mods.exists(_.is[Mod.Sealed])

  private[cardinality] def isOpaqueAlias(d: Defn.Type): Boolean = d.mods.exists(_.is[Mod.Opaque])

  // The names a body gives an equation of their own: aliases, enums, objects and concrete
  // classes. Traits and abstract classes are pass-through parents, handled by `sealedSums`.
  private[cardinality] def definedNames(stats: List[Stat]): Set[TypeName] =
    stats.collect {
      case d: Defn.Type                        => nameOf(d)
      case d: Defn.Enum                        => nameOf(d)
      case d: Defn.Object                      => nameOf(d)
      case d: Defn.Class if isConcreteClass(d) => nameOf(d)
    }.toSet

  // A potential subtype: its name and the parents its `extends` clauses list.
  private def childEntry(d: Defn): (TypeName, List[String]) = nameOf(d) -> initsOf(d)

  private def initsOf(d: Defn): List[String] =
    d.children.collect { case t: Template => t.inits }.flatten.map(Solver.initParent)

  // A sealed trait or abstract class stands for the sum of the body's subtypes that extend it,
  // so a recursive reference through the parent (`Succ(n: Nat)`) resolves. The sum is complete
  // only when every local subtype is concrete; otherwise the parent stays unknown, keeping the
  // `EffectiveOmega` fallback. Cross-file sealed hierarchies are future work.
  private[cardinality] def sealedSums(
      stats: List[Stat],
      defined: Set[TypeName]
  ): Map[TypeName, List[TypeName]] = {
    val sealedNames: Set[TypeName] = stats.collect {
      case d: Defn.Trait if isSealed(d)                       => nameOf(d)
      case d: Defn.Class if isSealed(d) && isAbstractClass(d) => nameOf(d)
    }.toSet
    val subtypes = stats.collect {
      case d: (Defn.Class | Defn.Object | Defn.Enum) => childEntry(d)
    }
    sealedNames.iterator
      .map(parent =>
        parent -> subtypes.collect {
          case (child, parents) if parents.contains(parent.value) => child
        }
      )
      .filter { case (_, children) => children.nonEmpty && children.forall(defined.contains) }
      .toMap
  }

  // A statement's contribution to a source total. A definition the solver gave an equation
  // contributes that solved value rather than a fresh evaluation: twice would count the base
  // summand twice (`Q = 1 + Q` contributing `ω + 2` where a reference to `Q` gives `ω + 1`).
  private def statIn(scope: Scope): Stat => Size = {
    case p: Pkg                              => Walk.body(p.body.stats, scope)
    case p: Pkg.Object                       => Walk.body(p.templ.body.stats, scope)
    case d: Defn.Class if isConcreteClass(d) =>
      scope.getOrElse(nameOf(d), ctorIn(scope)(d.ctor))
    case d: Defn.Enum   => scope.getOrElse(nameOf(d), enumSize(scope)(d))
    case d: Defn.Object => scope.getOrElse(nameOf(d), UnitSize)
    // scalameta's `Stat` is not sealed and hides `Stat.Quasi` as `private[meta]`, so an
    // exhaustive match is impossible. Every other statement — declarations, imports and
    // exports, bare terms, aliases — defines no values of its own.
    case d: Defn => defnIn(scope)(d)
    case _       => NothingSize
  }

  private[cardinality] def defnIn(scope: Scope): Defn => Size = {
    // Abstract classes contribute no inhabitants of their own; only their concrete
    // subclasses do.
    case c: Defn.Class if c.mods.exists(_.is[Mod.Abstract]) => NothingSize
    case c: Defn.Class                                      => ctorIn(scope)(c.ctor)
    // An enum's cardinality is the sum over its cases; the enum's own constructor
    // arguments are shared state, not extra inhabitants.
    case e: Defn.Enum => enumSize(scope)(e)
    // A module (including a `case object`) is a single instance.
    case _: Defn.Object            => UnitSize
    case _: Defn.Val | _: Defn.Var => UnitSize
    // scalameta's `Defn` is not sealed and hides `Defn.Quasi` as `private[meta]`, so an
    // exhaustive match is impossible. Every other member — type members, methods, givens,
    // enum cases — defines no values of its own.
    case _ => NothingSize
  }

  private def enumSize(scope: Scope): Defn.Enum => Size =
    _.templ.body.stats.foldLeft(NothingSize: Size) {
      case (acc, c: Defn.EnumCase)         => acc + ctorIn(scope)(c.ctor)
      case (acc, r: Defn.RepeatedEnumCase) => r.cases.foldLeft(acc)((a, _) => a + UnitSize)
      case (acc, _)                        => acc
    }

  private def ctorIn(scope: Scope): Ctor.Primary => Size =
    _.paramClauses.flatMap(_.values).foldLeft(UnitSize: Size)(_ * paramIn(scope)(_))

  private def paramIn(scope: Scope): Term.Param => Size =
    _.decltpe.fold(EffectiveOmega: Size)(typeIn(scope))

  // The member signature of a body: a static inventory of what a definition declares, summed
  // rather than multiplied — every typed member contributes the size of its declared type
  // (`String => String` is ε₀, `String` is ω, `Boolean` is 2) and nested definitions contribute
  // their members once; the container itself adds nothing. The surface is the accessor surface:
  // case and enum-case parameters, a plain class's `val`/`var` parameters, and the body's values,
  // variables and methods — a parameter without an accessor builds a class, it does not expose it.
  private def signatureBody(stats: List[Stat], scope: Scope): Size = {
    val entries = equations(stats)
    val solved = Solver.solve(entries, stats, scope.withDefinitions(entries))
    stats.foldLeft(NothingSize: Size)((acc, st) => acc + signatureIn(solved)(st))
  }

  private def signatureIn(scope: Scope): Stat => Size = {
    case p: Pkg        => signatureBody(p.body.stats, scope)
    case d: Defn.Class =>
      ctorSignature(scope)(caseParams = d.mods.exists(_.is[Mod.Case]), d.ctor) +
        signatureBody(d.templ.body.stats, scope)
    case d: Defn.Trait  => signatureBody(d.templ.body.stats, scope)
    case d: Defn.Object => signatureBody(d.templ.body.stats, scope)
    // An enum's own parameters reach every case, so they are members too.
    case d: Defn.Enum =>
      ctorSignature(scope)(caseParams = true, d.ctor) + signatureBody(d.templ.body.stats, scope)
    case d: Defn.EnumCase => ctorSignature(scope)(caseParams = true, d.ctor)
    // A value, variable or field contributes the size of its declared type; several patterns
    // (`val a, b: Int`) declare one member each, so their contributions add.
    case d: Defn.Val => declared(scope)(d.pats.size, d.decltpe)
    case d: Defn.Var => declared(scope)(d.pats.size, d.decltpe)
    case d: Decl.Val => declared(scope)(d.pats.size, Some(d.decltpe))
    case d: Decl.Var => declared(scope)(d.pats.size, Some(d.decltpe))
    // A method contributes its function space: the codomain raised to the product of its
    // parameters, with the same arrow and empty-domain rules `typeIn` uses.
    case d: Defn.Def            => methodSignature(scope)(d.paramClauses, d.decltpe)
    case d: Decl.Def            => methodSignature(scope)(d.paramClauses, Some(d.decltpe))
    case d: Defn.ExtensionGroup => extensionSignature(scope)(d)
    case d: Defn.GivenAlias     => givenSignature(scope)(d, Some(d.decltpe))
    case d: Decl.GivenLike      => givenSignature(scope)(d, Some(d.decltpe))
    case d: Defn.Given          =>
      val result = d.templ.inits.map(_.tpe).reduceOption { (left, right) =>
        Type.ApplyInfix(left, Type.Name("&"), right)
      }
      givenSignature(scope)(d, result)
    // Everything else — type members, secondary constructors — is not
    // a declared value or method of the definition.
    case _ => NothingSize
  }

  // A given contributes the instance it provides, not its implementation's members. Parameterized
  // givens are factories: their complete parameter domain raises the declared instance space.
  private def givenSignature(scope: Scope)(givenDef: Stat.GivenLike, result: Option[Type]): Size =
    methodSignature(scope)(givenDef.paramClauseGroups.flatMap(_.paramClauses), result)

  // Each extension is a method on the receiver, not a field of it. The group's receiver and
  // context clauses belong to every method's domain; the group itself contributes nothing.
  private def extensionSignature(scope: Scope)(group: Defn.ExtensionGroup): Size = {
    val clauses = group.paramClauseGroup.toList.flatMap(_.paramClauses)
    val stats = group.body match {
      case block: Term.Block => block.stats
      case stat              => List(stat)
    }
    stats.foldLeft(NothingSize: Size) {
      case (acc, d: Defn.Def) =>
        acc + methodSignature(scope)(clauses ++ d.paramClauses, d.decltpe)
      case (acc, d: Decl.Def) =>
        acc + methodSignature(scope)(clauses ++ d.paramClauses, Some(d.decltpe))
      case (acc, _) => acc
    }
  }

  private def ctorSignature(scope: Scope)(caseParams: Boolean, ctor: Ctor.Primary): Size = {
    val members = ctor.paramClauses.flatMap(_.values).filter(isAccessor(caseParams, _))
    members.foldLeft(NothingSize: Size)(_ + paramIn(scope)(_))
  }

  // A parameter is a signature member when the definition exposes it: every parameter of a case
  // class or an enum case, and the val/var parameters of a plain class.
  private def isAccessor(caseParams: Boolean, param: Term.Param): Boolean =
    caseParams || param.mods.exists(isAccessorMod)

  private def isAccessorMod(mod: Mod): Boolean = mod.is[Mod.ValParam] || mod.is[Mod.VarParam]

  private def declared(scope: Scope)(patterns: Int, decltpe: Option[Type]): Size =
    (0 until patterns).foldLeft(NothingSize: Size) { (acc, _) =>
      acc + decltpe.fold(EffectiveOmega: Size)(typeIn(scope))
    }

  private def methodSignature(scope: Scope)(
      paramClauses: Seq[Term.ParamClause],
      decltpe: Option[Type]
  ): Size = {
    val domain = paramClauses
      .flatMap(_.values)
      .foldLeft(UnitSize: Size)((acc, param) => acc * paramIn(scope)(param))
    arrow(decltpe.fold(EffectiveOmega: Size)(typeIn(scope)), domain)
  }

  private[cardinality] def typeIn(scope: Scope): Type => Size = {
    // A type parameter stands for whatever the instantiation supplied — an innermost binder wins
    // over a builtin of the same name, as it does in Scala.
    case scope.Parameter(size) => size

    case BaseTypes(size) => size

    // A qualified reference is a *member* the sources may declare (`Outer.B`, `x.B` over a value's
    // declared type); an unresolved whole reference is its own reason, read as written (`af.Z`).
    case select @ Type.Select(qualifier, Type.Name(member)) =>
      val key = TypeName.of(member)
      memberSize(scope, qualifier, key)
        .orElse(plainSize(scope, key))
        .getOrElse {
          scope.note(select)
          EffectiveOmega
        }

    // Literal types (`true`, `42`, `'a'`) and singleton types (`None.type`) have exactly
    // one inhabitant: the value itself.
    case _: Lit            => UnitSize
    case _: Type.Singleton => UnitSize

    // A by-name parameter has the cardinality of its underlying type.
    case Type.ByName(tpe) => typeIn(scope)(tpe)

    // Products: tuples (and named tuples) multiply the sizes of their fields.
    case Type.Tuple(elems) =>
      elems.foldLeft(UnitSize: Size)((acc, e) => acc * typeIn(scope)(tupleElem(e)))
    case Type.ApplyInfix(l, Type.Name("*:"), r) => typeIn(scope)(l) * typeIn(scope)(r)

    // Sums: unions add the sizes of their branches, unless both branches describe the
    // exact same type, in which case they fully overlap and count once.
    case Type.ApplyInfix(l, Type.Name("|"), r) =>
      if (l.structure == r.structure) typeIn(scope)(l) else typeIn(scope)(l) + typeIn(scope)(r)

    // Intersection: the overlap of two types is their meet, which is exact when (as here)
    // one is a subtype of the other.
    case Type.ApplyInfix(l, Type.Name("&"), r) => typeIn(scope)(l).min(typeIn(scope)(r))

    // Exponentials: a function type's cardinality is codomain ^ domain; multi-argument
    // and curried functions multiply/nest the same way. Context functions behave as functions.
    case Type.Function.After_4_6_0(clause, result) =>
      functionSize(scope)(clause.values, result)
    case Type.ContextFunction.After_4_6_0(clause, result) =>
      functionSize(scope)(clause.values, result)

    // Constructors the algebra adds: Option/Either, Set, Map/PartialFunction, and named instantiations.
    case Type.Apply.After_4_6_0(callee, Type.ArgClause(args)) => applied(scope)(callee, args)

    // The file's own names first, then the library's; any other name is unbounded, and recorded.
    case t: Type.Name =>
      scope.resolve(TypeName.of(t.value)).getOrElse { scope.note(t); EffectiveOmega }

    // A refinement is read as the type it refines: the members the refinement binds are not what
    // the value space is made of, and the base type is what the reference means.
    case refinement: Type.Refine =>
      refinement.tpe match {
        case Some(inner) => typeIn(scope)(inner)
        case None        =>
          scope.note("a refinement")
          EffectiveOmega
      }

    // `Type` is not sealed (`Type.And`, `Type.Or`, `Type.Method`, `Type.ImplicitFunction`,
    // `Type.Quasi` are `private[meta]`): no exhaustive match. Each remaining form — a cross-file
    // name, refinement, existential, capture type — is unbounded or unmodelled, and recorded.
    // ponytail: resolve sealed hierarchies and type parameters across files when needed
    case t =>
      scope.note(t)
      EffectiveOmega
  }

  // `A.B`: the size of the member a qualified reference names, when the sources supply the owner's
  // type. A member a body declares abstract is supplied by whoever implements the owner, so the
  // reference is unbounded rather than unknown.
  private def memberSize(scope: Scope, qualifier: Term, member: TypeName): Option[Size] =
    owner(scope, qualifier).flatMap {
      case (name, arguments) =>
        scope.membersOf(name).flatMap { members =>
          if (members.abstractMembers(member)) {
            scope.noteOpen(s"${qualifier.syntax}.$member")
            Some(EffectiveOmega)
          } else
            members.declared.get(member).map { body =>
              val bindings = members.params.zip(arguments).toMap
              typeIn(scope)(MatchTypes.replace(body, bindings))
            }
        }
    }

  // The type a qualifier denotes, with the arguments it was applied to: a type the sources name
  // (`Foo`, `Foo[A]`), or a value whose declared type they give (`x` in `x.B`).
  private def owner(scope: Scope, qualifier: Term): Option[(TypeName, List[Type])] =
    qualifier match {
      case Term.Name(name) =>
        val key = TypeName.of(name)
        scope.declaredType(key).flatMap(ownerOf).orElse(Some(key -> Nil))
      case Term.ApplyType.After_4_6_0(Term.Name(name), Type.ArgClause(arguments)) =>
        Some(TypeName.of(name) -> arguments)
      case _ => None
    }

  private def ownerOf(tpe: Type): Option[(TypeName, List[Type])] = tpe match {
    case Type.Name(name)                                      => Some(TypeName.of(name) -> Nil)
    case Type.Apply.After_4_6_0(callee, Type.ArgClause(args)) => nameOf(callee).map(_ -> args)
    case _                                                    => None
  }

  // The name on its own, without recording a reason: what a builtin, a binder or a definition the
  // sources give is worth.
  private def plainSize(scope: Scope, name: TypeName): Option[Size] =
    scope.frame(name).orElse(BaseTypes.get(name.value)).orElse(scope.resolve(name))

  // `codomain ^ domain`, except that an empty domain gives 0 rather than the set-theoretic 1,
  // `0^0` included. This is a constructivist approach: Scala is eager, a call evaluates its
  // argument first, and no argument of an uninhabited type can be constructed, so a function
  // from one can never run. Only function types take this rule; `Set` and `Map` over
  // `Nothing` still hold their one empty value.
  private def arrow(codomain: Size, domain: Size): Size =
    if (domain == NothingSize) NothingSize else codomain.pow(domain)

  // `C[args]`: an instantiation of a named definition reads that definition's equation with its
  // parameters bound to what the arguments are worth — `Pair[Boolean]` is 4, not an unknown
  // constructor. Builtins (`Option`, `Either`, `Set`, collections) are read below.
  private def applied(scope: Scope)(callee: Type, args: List[Type]): Size =
    instantiation(scope, callee, args).getOrElse(builtin(scope)(callee, args))

  // The size of `C[args]` when `C` names a definition with that many parameters; None otherwise.
  // Two recursive readings stay careful: an instantiation exactly the definition's own parameters
  // borrows the fixed point the solver solved under the name (how a parameterised recursion
  // keeps its μ/ν reading), and one re-entered while its own name is substituting has no such
  // fixed point, so it keeps the old fallback: ω, with the name as the reason.
  private def instantiation(scope: Scope, callee: Type, args: List[Type]): Option[Size] = {
    val found = for
      name <- nameOf(callee)
      resolved <- scope
        .definition(name)
        .map((scope, _))
        .orElse(scope.libraryDefinition(name))
      if resolved._2.params.size == args.size
    yield (name, resolved._1, resolved._2)
    found.map(resolved => instantiated(scope, callee, args, resolved))
  }

  // One instantiation of a definition, read: an instantiation that is exactly the definition's own
  // parameters borrows the solved fixed point, a repeat inside its own substitution keeps the old
  // fallback, a match type reduces on the argument's syntax, and everything else substitutes the
  // arguments into the equation.
  private def instantiated(
      scope: Scope,
      callee: Type,
      args: List[Type],
      resolved: (TypeName, Scope, Named)
  ): Size = {
    val (name, context, named) = resolved
    val self = named.params.zip(args).forall((param, arg) => Solver.bareName(arg) == param.value)
    if (self && context.size(name).isDefined) context(name)
    else if (context.isSubstituting(name)) {
      scope.note(callee)
      EffectiveOmega
    } else
      named.matchType match {
        // An alias whose body is a match type reduces on the argument's *syntax*, which no size
        // can carry; a match that does not reduce keeps its template reading, named by the report.
        case Some(matchType) =>
          MatchTypes.reduced(matchType, named.params.zip(args).toMap) match {
            case Some(argument) => typeIn(scope)(argument)
            case None           =>
              // Stuck: the arguments still carry their own reasons — a parameter over which
              // the match type stays inert is one of them — and the match type is another.
              args.foreach(typeIn(scope))
              scope.note(matchType)
              EffectiveOmega
          }
        case None =>
          val frame =
            named.params.zip(args).map((param, arg) => param -> typeIn(scope)(arg)).toMap
          named.equation(context.substituting(name).instantiated(frame))
      }
  }

  // The simple name a type constructor is spelled with: `Pair`, or `data.Pair`'s last segment — a
  // qualified name resolves by its own name, as it does for a bare reference.
  private[cardinality] def nameOf(tpe: Type): Option[TypeName] = tpe match {
    case Type.Name(name)              => Some(TypeName.of(name))
    case Type.Select(_, Type.Name(n)) => Some(TypeName.of(n))
    case _                            => None
  }

  // The constructors the algebra models itself: `Option`/`Either` (sums), `Set` (powerset),
  // `Map`/`PartialFunction` (into an Option of the codomain), and the collections whose unbounded
  // length is their infinity; `Size.pow` keeps a finite base countable over an infinite exponent.
  private def builtin(scope: Scope): (Type, List[Type]) => Size = {
    case (Type.Name("Option"), List(t))    => UnitSize + typeIn(scope)(t)
    case (Type.Name("Either"), List(l, r)) => typeIn(scope)(l) + typeIn(scope)(r)
    case (Type.Name("Set"), List(t))       => BooleanSize.pow(typeIn(scope)(t))
    case (Type.Name("Map"), List(k, v))    => (typeIn(scope)(v) + UnitSize).pow(typeIn(scope)(k))
    case (Type.Name("PartialFunction"), List(a, b)) =>
      (typeIn(scope)(b) + UnitSize).pow(typeIn(scope)(a))
    // A linear collection over an empty element type holds just the empty collection; over any
    // other it is countably infinite — a number the algebra knows, so no reason is recorded.
    case (Type.Name("List" | "Vector" | "Seq" | "IndexedSeq" | "Array"), List(t)) =>
      if (typeIn(scope)(t) == NothingSize) UnitSize else EffectiveOmega
    // `LazyList`/`Stream` are the greatest fixed point `νX. 1 + A*X` (§8): all the finite ones
    // (ℵ₀ over a nonempty finitely-countable alphabet) plus the infinite streams (the alphabet
    // raised to ℵ₀), collapsed to the highest tier; sums around it keep coefficients, and an
    // empty alphabet has the single empty list.
    case (Type.Name("LazyList" | "Stream"), List(t)) =>
      val elem = typeIn(scope)(t)
      if (elem == NothingSize) UnitSize
      else Solver.completeCoinduction(EffectiveOmega, elem.pow(EffectiveOmega))
    // A type constructor the calculator does not model is recorded by name, so that a report can
    // say what it could not bound.
    case (callee, _) =>
      scope.note(callee)
      EffectiveOmega
  }

  // A function's domain is the product of its parameter types; `Unit` (a single empty
  // tuple) when it takes none.
  private def domain(scope: Scope)(params: List[Type]): Size =
    params.foldLeft(UnitSize: Size)((acc, p) => acc * typeIn(scope)(p))

  // The function-space rule shared by plain and context function types in `typeIn`.
  private def functionSize(scope: Scope)(params: List[Type], result: Type): Size =
    arrow(typeIn(scope)(result), domain(scope)(params))

  // A named-tuple element (`a: Boolean`) carries its type inside a TypedParam; the rest already are.
  private def tupleElem: Type => Type = {
    case Type.TypedParam.After_4_7_8(_, tpe, _) => tpe
    case other                                  => other
  }

}
