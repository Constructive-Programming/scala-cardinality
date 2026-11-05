package cardinality.capacity

import scala.meta.*

import cardinality.types.*

object Counter {

  // Trees are parsed and reprinted under Scala 3 dialect rules.
  private given scala3: Dialect = dialects.Scala3

  /** What every definition a source introduces holds together: the sum over its concrete classes,
    * enums, modules and top-level values, nested definitions and supplied library sources included.
    */
  def source(s: Source, library: Library = Library.empty): Size =
    Walk.body(s.stats, Scope.libraryLens.replace(library)(Scope.empty))

  /** Every definition a source introduces, in source order, with the cardinality of its type and
    * any reasons that stopped the calculator from bounding it.
    */
  def definitions(s: Source, library: Library = Library.empty): List[Definition] =
    Walk.of(s.stats, Scope.libraryLens.replace(library)(Scope.empty), Nil, top = true).definitions

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

  // A type owns its name; a module is a term. `object Foo`'s value has type `Foo.type`, so no
  // equation is made for it: a type reference `Foo` means the trait, class, enum or alias, and a
  // companion of an unsealed trait does not stand in for the trait. A `case object` child counts
  // once in its sealed parent's sum — the statement that defines it says it is a module, whatever
  // type shares its name. Each equation reads with its own type parameters bound, so a parameter
  // shadows a sibling type of the same name whether the solver, an instantiation or a row reads it.
  private[cardinality] def equations(stats: List[Stat]): List[(TypeName, Named)] = {
    val types = stats.flatMap {
      case d: Defn.Type =>
        // A match type's body travels with the definition: it is read when the alias is applied.
        // An opaque alias's representation is its value where it is transparent; a reference
        // anywhere else is refused (`hidden`).
        val matchType = d.body match {
          case m: Type.Match => Some(m)
          case _             => None
        }
        val equation = bound(d)(sc => typeIn(sc)(d.body))
        val opaque = Option.when(isOpaqueAlias(d))(d)
        Some(
          Walk.named(d) -> Named(
            parameterNames(d),
            equation,
            matchType,
            read = signature(d),
            opaque = opaque
          )
        )
      case d: Defn.Enum =>
        Some(Walk.named(d) -> built(d, enumSize(_)(d)))
      case d: Defn.Class if isConcreteClass(d) =>
        Some(Walk.named(d) -> built(d, stored(_)(d.tparamClause.values, d.ctor)))
      case _ => None
    }
    types
  }

  // A class or enum: what it stores, read with its own binders — and, while a value row reads it,
  // why an instance is not known to be built, since what it stores is no evidence that one is
  // (`Builds`).
  private def built(d: Defn, stores: Scope => Size): Named = {
    val equation = bound(d)(stores)
    Named(
      parameterNames(d),
      sc => {
        if (sc.readingValue) Builds.unread(d, sc).foreach(why => sc.note(Overcount.Unread(why)))
        equation(sc)
      }
    )
  }

  // An equation read with the definition's own binders innermost, and its constructor's parameters
  // in scope for a later parameter's path (`Dep(q: Other)(x: q.X)`).
  private def bound(d: Defn)(equation: Scope => Size): Scope => Size =
    sc => equation(signature(d)(sc))

  /** A scope with `d`'s binders innermost and its constructor's parameters in scope. */
  private[cardinality] def signature(d: Defn)(scope: Scope): Scope =
    Scope.termsLens
      .modify(_ ++ Walk.parameters(d))(Scope.bindersLens.modify(binders(d) ++ _)(scope))

  // The type parameters a definition declares, in the order its arguments are supplied in.
  private[cardinality] def parameters(d: Defn): List[Type.Param] = d match {
    case c: (Defn.Class | Defn.Trait | Defn.Enum | Defn.Type) => c.tparamClause.values
    case m: Defn.Def => m.paramClauseGroups.flatMap(_.tparamClause.values)
    case _           => Nil
  }

  // The parameters as binders: a parameter that takes parameters of its own (`F[_]`) is the
  // higher-kinded kind, and every applied `F[A]` then depends on the instantiation too.
  private[cardinality] def binders(d: Defn): List[Binder] = binders(parameters(d))

  private[cardinality] def binders(params: List[Type.Param]): List[Binder] =
    params.map(param => Binder(TypeName.of(param.name.value), param.tparamClause.values.size))

  private[cardinality] def parameterNames(d: Defn): List[TypeName] =
    parameters(d).map(p => TypeName.of(p.name.value))

  // The modifier tests the case arms repeat: abstract classes contribute no inhabitants of their
  // own, opaque aliases hide their representation outside their scope, and `sealed` marks the
  // pass-through parents.
  private[cardinality] def isConcreteClass(d: Defn.Class): Boolean = !isAbstractClass(d)

  private[cardinality] def isAbstractClass(d: Defn.Class): Boolean =
    d.mods.exists(_.is[Mod.Abstract])

  private[cardinality] def isSealed(d: Defn.Class | Defn.Trait): Boolean =
    d.mods.exists(_.is[Mod.Sealed])

  private[cardinality] def isOpaqueAlias(d: Defn.Type): Boolean = d.mods.exists(_.is[Mod.Opaque])

  // A statement's contribution to a source total. A definition the solver gave an equation
  // contributes that solved value rather than a fresh evaluation: twice would count the base
  // summand twice (`Q = 1 + Q` contributing `ω + 2` where a reference to `Q` gives `ω + 1`).
  private def statIn(scope: Scope): Stat => Size = {
    case p: Pkg                              => Walk.body(p.body.stats, scope)
    case p: Pkg.Object                       => Walk.body(p.templ.body.stats, scope)
    case d: Defn.Class if isConcreteClass(d) =>
      scope.getOrElse(Walk.named(d), stored(scope)(d.tparamClause.values, d.ctor))
    case d: Defn.Enum   => scope.getOrElse(Walk.named(d), enumSize(scope)(d))
    case _: Defn.Object => UnitSize
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
    case c: Defn.Class => stored(scope)(c.tparamClause.values, c.ctor)
    // An enum's cardinality is the sum over its cases; the enum's own constructor
    // arguments are shared state, not extra inhabitants.
    case e: Defn.Enum => enumSize(scope)(e)
    // A module (including a `case object`) is a single instance.
    case _: Defn.Object => UnitSize
    case v: Defn.Val    => ValueRows.value(scope)(v.decltpe)
    case v: Defn.Var    => ValueRows.value(scope)(v.decltpe)
    // scalameta's `Defn` is not sealed and hides `Defn.Quasi` as `private[meta]`, so an
    // exhaustive match is impossible. Every other member — type members, methods, givens,
    // enum cases — defines no values of its own.
    case _ => NothingSize
  }

  private def enumSize(scope: Scope): Defn.Enum => Size = e =>
    e.templ.body.stats.foldLeft(NothingSize: Size) {
      case (acc, c: Defn.EnumCase)         => acc + enumCase(scope, e, c)
      case (acc, r: Defn.RepeatedEnumCase) => r.cases.foldLeft(acc)((a, _) => a + UnitSize)
      case (acc, _)                        => acc
    }

  // A case with its own `extends` is a member of the instantiations it admits, as a family's
  // subtype is: `case Y extends E[Int]` (a GADT index), or a source's `Nothing`, is no summand.
  private def enumCase(scope: Scope, e: Defn.Enum, c: Defn.EnumCase): Size = {
    val enumName = e.name.value
    val why =
      if (c.inits.isEmpty) None
      else
        SealedSlots
          .enumCase(c, e, SealedFamilies.denoted(c))
          .fold(Some(_), SealedFamilies.unstandard(scope, enumName, _).map(why => s"it $why"))
    why.fold(stored(EnumCompanions.caseScope(scope, e))(c.tparamClause.values, c.ctor)) { reason =>
      scope.note(
        Definition.Unbound.Refusal(s"enum case not summed: $enumName.${c.name.value} ($reason)")
      )
      EffectiveOmega
    }
  }

  private def ctorIn(scope: Scope): Ctor.Primary => Size =
    _.paramClauses.flatMap(_.values).foldLeft(UnitSize: Size)(_ * paramIn(scope)(_))

  // What a constructor stores: its parameters, and the evidence each context bound of the class's
  // type parameters adds — Scala 3 desugars `C[T: TC]` to `C[T](using TC[T])`, a field like any
  // other, so `final class TT[T[_]: Traverse]` holds a `Traverse[T]`.
  private def stored(scope: Scope)(tparams: List[Type.Param], ctor: Ctor.Primary): Size =
    evidence(tparams).foldLeft(ctorIn(scope)(ctor))((acc, tpe) => acc * typeIn(scope)(tpe))

  /** The evidence types a definition's context bounds add to its constructor: `TC[T]` per bound. */
  private[cardinality] def evidence(tparams: List[Type.Param]): List[Type] =
    tparams.flatMap { p =>
      p.bounds.context.map { bound =>
        // A named bound (`T: Ord as o`) names the evidence; its type is the bound.
        val tc = bound match {
          case named: Type.BoundsAlias => named.bounds
          case plain                   => plain
        }
        Type.Apply(tc, Type.ArgClause(List(Type.Name(p.name.value))))
      }
    }

  private def paramIn(scope: Scope): Term.Param => Size =
    _.decltpe.fold(EffectiveOmega: Size)(typeIn(scope))

  // The member signature of a body: a static inventory of what a definition declares, summed
  // rather than multiplied — every typed member contributes the size of its declared type
  // (`String => String` is ε₀, `String` is ω, `Boolean` is 2) and nested definitions contribute
  // their members once; the container itself adds nothing. The surface is the accessor surface:
  // case and enum-case parameters, a plain class's `val`/`var` parameters, and the body's values,
  // variables and methods — a parameter without an accessor builds a class, it does not expose it.
  private def signatureBody(stats: List[Stat], scope: Scope): Size = {
    val families = SealedFamilies.of(List(stats), Nil)
    val entries = equations(stats) ++ SealedFamilies.equations(families)
    val solved =
      Solver.solve(
        entries,
        Shapes.of(stats).copy(parents = SealedFamilies.edges(families)),
        Scope.definitionsLens.modify(entries.toMap ++ _)(scope)
      )
    stats.foldLeft(NothingSize: Size)((acc, st) => acc + signatureIn(solved)(st))
  }

  private def signatureIn(scope: Scope): Stat => Size = {
    case p: Pkg        => signatureBody(p.body.stats, scope)
    case d: Defn.Class =>
      ctorSignature(scope)(caseParams = d.mods.exists(_.is[Mod.Case]), d.ctor) +
        signatureBody(d.templ.body.stats, scope)
    case d: (Defn.Trait | Defn.Object) => signatureBody(d.templ.body.stats, scope)
    // An enum's own parameters reach every case, so they are members too.
    case d: Defn.Enum =>
      ctorSignature(scope)(caseParams = true, d.ctor) + signatureBody(d.templ.body.stats, scope)
    case d: Defn.EnumCase => ctorSignature(scope)(caseParams = true, d.ctor)
    // A value, variable or field contributes the size of its declared type; several patterns
    // (`val a, b: Int`) declare one member each, so their contributions add.
    case d: (Defn.Val | Defn.Var) => declared(scope)(d.pats.size, d.decltpe)
    case d: (Decl.Val | Decl.Var) => declared(scope)(d.pats.size, Some(d.decltpe))
    // A method contributes its function space: the codomain raised to the product of its
    // parameters, with the same arrow and empty-domain rules `typeIn` uses.
    case d: Defn.Def            => methodSignature(scope)(d.paramClauses, d.decltpe)
    case d: Decl.Def            => methodSignature(scope)(d.paramClauses, Some(d.decltpe))
    case d: Defn.ExtensionGroup => extensionSignature(scope)(d)
    case d: (Defn.GivenAlias | Decl.GivenLike) => givenSignature(scope)(d, Some(d.decltpe))
    case d: Defn.Given                         =>
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

    // A parameter no instantiation bound is the parameter, whatever a sibling type or a base type
    // of the same name would be worth: Scala's scoping puts the binder innermost.
    case scope.Free(binder) => unbound(scope, binder)

    // A base type's name is the standard library's only where no source binds it: a root import
    // is the outermost binding, so even another file's `Int` in the same package wins over it.
    case name @ BaseTypes(size) if !scope.binds(TypeName.of(name.syntax)) => size

    // A throwable of the root imports is an open class: any subclass's values are its own.
    case name: Type.Name
        if BaseTypes.throwables.contains(name.value) && !scope.binds(TypeName.of(name.value)) =>
      scope.noteOpen(name.value)
      EffectiveOmega

    case select: Type.Select => qualified(scope, select)

    // Literal types (`true`, `42`, `'a'`) and singleton types (`None.type`) have exactly
    // one inhabitant: the value itself — a singleton's, if its path denotes one (`ValueRows`).
    case _: Lit                 => UnitSize
    case single: Type.Singleton =>
      scope.note(Overcount.Unread(s"the singleton type ${single.syntax}"))
      UnitSize

    case Type.ByName(tpe) => typeIn(scope)(tpe)

    // A repeated parameter holds the sequence of its arguments — the linear collections' rule.
    case Type.Repeated(tpe) => sequence(scope, typeIn(scope)(tpe))

    // Products: tuples multiply the sizes of their elements; `EmptyTuple` is the unit.
    // A named-tuple element carries its type inside a `Type.TypedParam`.
    case Type.Tuple(args) =>
      args.foldLeft(UnitSize: Size)((acc, arg) => acc * typeIn(scope)(tupleElem(arg)))

    case Type.ApplyInfix(l, Type.Name("*:"), r) => typeIn(scope)(l) * typeIn(scope)(r)

    // Sums: unions add the sizes of their branches, unless both branches describe the
    // exact same type, in which case they fully overlap and count once.
    case Type.ApplyInfix(l, Type.Name("|"), r) =>
      if (l.structure == r.structure) typeIn(scope)(l) else typeIn(scope)(l) + typeIn(scope)(r)

    // Intersection: the overlap of two types is their meet, which is exact when (as here)
    // one is a subtype of the other.
    case Type.ApplyInfix(l, Type.Name("&"), r) =>
      scope.note(Overcount.Narrowing)
      typeIn(scope)(l).min(typeIn(scope)(r))

    // A match type written in place reduces only through an alias applied to known arguments.
    case m: Type.Match => Instantiations.stuck(scope, m, List(m.tpe))

    // Exponentials: a function type's cardinality is codomain ^ domain; multi-argument
    // and curried functions multiply/nest the same way. Context functions behave as functions.
    case f: Type.FunctionType => functionSize(scope)(f.paramClause.values, f.res)

    // Named instantiations, and the constructors the algebra models (`builtin`).
    case Type.Apply.After_4_6_0(callee, Type.ArgClause(args)) => applied(scope)(callee, args)

    // The name as it binds (`Scope.binding`); any other name is unbounded, and recorded. A generic
    // definition named without arguments is a type constructor, with no value space of its own.
    case t: Type.Name =>
      val name = TypeName.of(t.value)
      if (generic(scope, t)) constructor(scope, t.value)
      else
        hidden(t, Namespaces.lookup(scope, t)((in, key, _) => in.definition(key)))
          .getOrElse(scope.resolve(name).getOrElse { scope.note(t); EffectiveOmega })

    // A refinement is read as the type it refines: the members the refinement binds are not what
    // the value space is made of, and the base type is what the reference means.
    case refinement: Type.Refine =>
      scope.note(Overcount.Narrowing)
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

  // `q.T` reads through its qualifier and nothing else: a value's known type gives a class member
  // (`x.T`), an object or a package what its body defines, `scala.Int` the base type it spells.
  // Anything else is its own reason, with the clause its first name rests on, if any.
  private def qualified(scope: Scope, select: Type.Select): Size =
    located(scope, select) match {
      case Some(Namespaces.Located.Member(key, in))      => keyed(scope, select, key, in)
      case Some(Namespaces.Located.Instance(tpe, in))    => classMember(scope, select, tpe, in)
      case Some(Namespaces.Located.Unread(Some(clause))) =>
        unbound(scope, Definition.Unbound.Imported(TypeSubstitution.described(select), clause))
      case Some(Namespaces.Located.Unread(None))        => unbound(scope, select)
      case Some(Namespaces.Located.Pending(reason))     => unbound(scope, reason)
      case None if Namespaces.standardThrowable(select) =>
        scope.noteOpen(select.syntax)
        EffectiveOmega
      case None => standard(select).getOrElse(unbound(scope, select))
    }

  private def located(scope: Scope, select: Type.Select): Option[Namespaces.Located] =
    Namespaces.locate(scope, select.qual, TypeName.of(select.name.value))

  // `scala.Int`, `scala.Option`, `java.lang.String`, `java.lang.Object`: the base types the
  // standard library spells in full, when no source namespace has that path.
  private def standard(select: Type.Select): Option[Size] =
    if (Namespaces.standardQualifier(select.qual)) BaseTypes.get(select.name.value)
    else if (Namespaces.javaLang(select)) BaseTypes.get(select.name.value)
    else None

  // A member a namespace solved, read with the reasons its value rests on: a qualified reference
  // to a definition that holds an unknown name is no more bounded than the definition is.
  private def keyed(scope: Scope, select: Type.Select, key: TypeName, in: Scope): Size =
    hidden(select, in.definition(key)).getOrElse(solvedMember(scope, select, key, in))

  private def solvedMember(scope: Scope, select: Type.Select, key: TypeName, in: Scope): Size =
    in.size(key) match {
      case Some(_) if in.definition(key).exists(_.params.nonEmpty) =>
        constructor(scope, select.syntax)
      case Some(size) =>
        in.blocking(key).foreach(scope.noteBlocked(key, _, in.definition(key)))
        in.definition(key).foreach(named => scope.taintOf(named, in.home(key, named)))
        size
      case None => missing(scope, select, key, in)
    }

  // A key a namespace does not solve: an open abstraction of it, a declaration it does not read
  // (an enum's class case), a sealed family it does not sum, or the reference as written.
  private def missing(scope: Scope, select: Type.Select, key: TypeName, in: Scope): Size = {
    if (in.isOpen(key)) scope.noteOpen(select.syntax)
    else
      (in.refused(key), in.unsummed(key)) match {
        case (Some(reason), _) => scope.note(reason)
        case (_, Some(why))    => scope.noteUnsummed(TypeName.of(select.syntax), why)
        case _                 => scope.note(select)
      }
    EffectiveOmega
  }

  // `x.T` with `x` a value of type `C[ā]`: class C's member T, read in C's own body, with C's
  // parameters bound to `ā` as read where `x` is bound; an abstract member is supplied by whoever
  // implements C. `C` binds as any type name does: a type parameter, or an alias or type with no
  // members of its own, stops the read — never a same-named class elsewhere.
  private def classMember(scope: Scope, select: Type.Select, declared: Type, in: Scope): Size = {
    val member = TypeName.of(select.name.value)
    val bound = if (in eq scope) scope else scope.borrow(in)
    ownerMembers(bound, declared) match {
      case Some((members, _)) if members.abstractMembers(member) =>
        scope.noteOpen(select.syntax)
        EffectiveOmega
      case Some((members, arguments)) if members.declared.contains(member) =>
        // A member that reads itself through an instance of its own class is a cycle Scala
        // rejects; it is a reason here rather than a loop.
        val guard = TypeName.of(s"${members.owner.value}#${member.value}")
        if (Scope.activeLens.get(scope)(guard)) unbound(scope, select)
        else {
          val frame = members.params.zip(arguments).map((p, arg) => p -> typeIn(bound)(arg)).toMap
          val inside = Scope.activeLens.modify(_ + guard)(scope.borrow(members.inside))
          typeIn(inside.instantiating(frame))(members.declared(member))
        }
      case _ => unbound(scope, select)
    }
  }

  // The members of the class a declared type names, with the arguments it applies.
  private def ownerMembers(scope: Scope, tpe: Type): Option[(Members, List[Type])] =
    TypeApplications.applied.getOption(tpe) match {
      case Some(app) =>
        ownerMembers(scope, app.tpe)
          .map((members, _) => members -> TypeApplications.applyArguments.get(app))
      case None => Namespaces.lookup(scope, tpe)((in, key, _) => in.membersAt(key)).map(_ -> Nil)
    }

  // `codomain ^ domain`, except that an empty domain gives 0 rather than the set-theoretic 1,
  // `0^0` included. This is a constructivist approach: Scala is eager, a call evaluates its
  // argument first, and no argument of an uninhabited type can be constructed, so a function
  // from one can never run. Only function types take this rule; `Set` and `Map` over
  // `Nothing` still hold their one empty value.
  private def arrow(codomain: Size, domain: Size): Size =
    if (domain == NothingSize) NothingSize else codomain.pow(domain)

  // `C[args]`: an instantiation of a named definition reads that definition's equation with its
  // parameters bound to what the arguments are worth — `Pair[Boolean]` is 4, not an unknown
  // constructor. Builtins (`Option`, `Either`, `Set`, collections) are read below; a type parameter
  // applied (`F[A]`) is the parameter's, whatever builtin shares its name.
  private def applied(scope: Scope)(callee: Type, args: List[Type]): Size = callee match {
    case scope.Free(binder) => unbound(scope, binder)
    case _                  =>
      instantiation(scope, callee, args).getOrElse {
        // A bare name the sources bind is that binding's, not the standard constructor's.
        val bound = Counter.nameOf(callee).exists(scope.binds) && callee.isInstanceOf[Type.Name]
        if (bound) unbound(scope, callee) else builtin(scope)(callee, args)
      }
  }

  // A type the calculator could not bound, recorded with its reason.
  private def unbound(scope: Scope, tpe: Type): Size = {
    scope.note(tpe)
    EffectiveOmega
  }

  private def unbound(scope: Scope, reason: Definition.Unbound): Size = {
    scope.note(reason)
    EffectiveOmega
  }

  private def generic(scope: Scope, tpe: Type): Boolean =
    Namespaces.lookup(scope, tpe)((in, key, _) => in.definition(key)).exists(_.params.nonEmpty)

  // A generic definition named without arguments.
  private def constructor(scope: Scope, written: String): Size =
    unbound(scope, Definition.Unbound.Syntax(s"type constructor: $written"))

  // The size of `C[args]` when `C` names a definition with that many parameters; None otherwise
  // (`Instantiations` reads it).
  private def instantiation(scope: Scope, callee: Type, args: List[Type]): Option[Size] = {
    // A definition reads where it was solved — never in a body nested inside that shadows a name it
    // reads — recording into the reference's notes, with the instantiations it is inside of.
    val found = Namespaces.lookup(scope, callee) { (in, key, qualified) =>
      in.definition(key).map { named =>
        val home = in.home(key, named)
        (key, if (qualified || !(home eq scope)) scope.borrow(home) else scope, named)
      }
    }
    found
      .filter(_._3.params.size == args.size)
      .map((name, context, named) =>
        hidden(callee, Some(named)).getOrElse(
          Instantiations.read(Instantiations.Instance(scope, callee, args, name, context, named))
        )
      )
  }

  // A reference to an opaque alias written where its representation is not visible: one value
  // (v1.md §2, opaque types).
  private def hidden(reference: Type, named: Option[Named]): Option[Size] =
    named.flatMap(_.opaque).filterNot(OpaqueScopes.transparent(_, reference)).map(_ => UnitSize)

  // The simple name a type constructor is spelled with: `Pair`, or `data.Pair`'s last segment. Only
  // the solver's syntactic classification reads it (an `extends` clause, an `Option` hole); a
  // reference resolves through its qualifier (`qualified`).
  private[cardinality] def nameOf(tpe: Type): Option[TypeName] = tpe match {
    case Type.Name(name)              => Some(TypeName.of(name))
    case Type.Select(_, Type.Name(n)) => Some(TypeName.of(n))
    case _                            => None
  }

  // The constructors the algebra models itself: `Option`/`Either` (sums), `Set` (powerset),
  // `Map`/`PartialFunction` (into an Option of the codomain), and the collections whose unbounded
  // length is their infinity; `Size.pow` keeps a finite base countable over an infinite exponent.
  private def builtin(scope: Scope): (Type, List[Type]) => Size = {
    case (Modelled("Option"), List(t))    => UnitSize + typeIn(scope)(t)
    case (Modelled("Either"), List(l, r)) => typeIn(scope)(l) + typeIn(scope)(r)
    case (Modelled("Set"), List(t))       =>
      val element = typeIn(scope)(t)
      finite(scope, element, BooleanSize.pow(element))
    case (Modelled("Map"), List(k, v)) =>
      val key = typeIn(scope)(k)
      finite(scope, key, (typeIn(scope)(v) + UnitSize).pow(key))
    case (Modelled("PartialFunction"), List(k, v)) =>
      (typeIn(scope)(v) + UnitSize).pow(typeIn(scope)(k))
    case (Modelled("List" | "Vector" | "Seq" | "IndexedSeq" | "Array"), List(t)) =>
      sequence(scope, typeIn(scope)(t))
    // `LazyList`/`Stream` are the greatest fixed point `νX. 1 + A*X` (§8): all the finite ones
    // (ℵ₀ over a nonempty finitely-countable alphabet) plus the infinite streams (the alphabet
    // raised to ℵ₀), collapsed to the highest tier; sums around it keep coefficients, and an
    // empty alphabet has the single empty list.
    case (Modelled("LazyList" | "Stream"), List(t)) =>
      val elem = typeIn(scope)(t)
      if (elem == NothingSize) UnitSize
      else Solver.completeCoinduction(EffectiveOmega, elem.pow(EffectiveOmega))
    // A type constructor the calculator does not model is recorded by name, so that a report can
    // say what it could not bound.
    case (select: Type.Select, _) =>
      located(scope, select).fold(unbound(scope, select)) {
        case Namespaces.Located.Member(key, in)      => missing(scope, select, key, in)
        case Namespaces.Located.Unread(Some(clause)) =>
          unbound(scope, Definition.Unbound.Imported(TypeSubstitution.described(select), clause))
        case _ => unbound(scope, select)
      }
    case (callee, _) => unbound(scope, callee)
  }

  // A linear collection over an empty element type holds just the empty one; over any other it is
  // `Σₙ aⁿ` — at least ω and, by its `n = 1` term, the element's tier: a number, no reason.
  private def sequence(scope: Scope, element: Size): Size =
    if (element == NothingSize) UnitSize else finite(scope, element, EffectiveOmega)

  // A finite collection holds each one-element collection, so it is never below its element's
  // tier (ε₀ over `String => String`, however `2^a` or `ω` rounds); the empty one alone stays 1.
  // The method side's own model, and the reading lent to it, saturate at ω.
  private def finite(scope: Scope, element: Size, collection: Size): Size =
    if (scope.saturating || element.epsilon == 0 || !collection.larger(UnitSize)) collection
    else Size.order.max(collection, EffectiveEpsilon0)

  // A constructor the algebra models, spelled bare (`Option`) or in full under the standard
  // library's qualifiers (`scala.Option`, `scala.collection.immutable.List`): its simple name.
  private object Modelled {

    def unapply(tpe: Type): Option[String] = tpe match {
      case Type.Name(name) if StandardConstructors.modelled(name) => Some(name)
      case Type.Select(q, Type.Name(name))
          if Namespaces.standardQualifier(q) && StandardConstructors.modelled(name) =>
        Some(name)
      case _ => None
    }

  }

  // A function's domain: the product of its parameter types (`Unit` for none).
  private[cardinality] def domain(scope: Scope)(params: List[Type]): Size =
    params.foldLeft(UnitSize: Size)((acc, p) => acc * typeIn(scope)(p))

  // The function-space rule shared by plain and context function types in `typeIn`.
  private def functionSize(scope: Scope)(params: List[Type], result: Type): Size = {
    val codomain = typeIn(scope)(result)
    val from = domain(scope)(params)
    if (from.isZero) scope.note(Overcount.EmptyDomain)
    arrow(codomain, from)
  }

  // A named-tuple element (`a: Boolean`) carries its type inside a TypedParam.
  private[capacity] def tupleElem: Type => Type = {
    case Type.TypedParam.After_4_7_8(_, tpe, _) => tpe
    case other                                  => other
  }

}
