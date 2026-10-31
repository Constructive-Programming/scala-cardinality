package cardinality.capacity

import scala.annotation.tailrec
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
      case d: Defn.Type if isOpaqueAlias(d) =>
        Some(nameOf(d) -> Named(parameterNames(d), (_: Scope) => UnitSize))
      case d: Defn.Type =>
        // A match type's body travels with the definition: it is read when the alias is applied.
        val matchType = d.body match {
          case m: Type.Match => Some(m)
          case _             => None
        }
        val equation = bound(d)(sc => typeIn(sc)(d.body))
        Some(nameOf(d) -> Named(parameterNames(d), equation, matchType, read = signature(d)))
      case d: Defn.Enum => Some(nameOf(d) -> Named(parameterNames(d), bound(d)(enumSize(_)(d))))
      case d: Defn.Class if isConcreteClass(d) =>
        Some(nameOf(d) -> Named(parameterNames(d), bound(d)(ctorIn(_)(d.ctor))))
      case _ => None
    }
    types ++ sealedSums(stats).toList.map {
      case (parent, children) =>
        // A sealed parent's equation is the sum of its children, and it keeps them: a report
        // reads the parent's value from it, and the children's reasons are the parent's.
        parent -> Named(
          Nil,
          (sc: Scope) =>
            children.foldLeft(NothingSize: Size)((acc, child) =>
              acc + (if (child.module) UnitSize else sc.getOrElse(child.name, NothingSize))
            ),
          children = children
        )
    }
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

  // A sealed trait or abstract class stands for the sum of the body's subtypes that extend it,
  // so a recursive reference through the parent (`Succ(n: Nat)`) resolves. The sum is complete
  // only when every subtype the file writes is one of the body's concrete classes, enums or
  // modules; a subtype anywhere else — in a companion, nested in another definition, an abstract
  // one, an anonymous instance, an enum case — leaves a family the body does not sum. A parent
  // extended with arguments (`extends P[A]`) is not matched (`Solver.initParent`), so a summed
  // parent's subtypes pass it none.
  private[cardinality] def sealedSums(stats: List[Stat]): Map[TypeName, List[Child]] = {
    val sealedNames: Set[TypeName] = stats.collect {
      case d: Defn.Trait if isSealed(d)                       => nameOf(d)
      case d: Defn.Class if isSealed(d) && isAbstractClass(d) => nameOf(d)
    }.toSet
    val subtypes = stats.collect { case d: (Defn.Class | Defn.Object | Defn.Enum) => d }
    val written = if (sealedNames.isEmpty) Map.empty else extenders(stats)
    sealedNames.iterator
      .map(parent => parent -> subtypes.filter(extendsParent(_, parent)))
      .collect {
        case (parent, children)
            if children.nonEmpty && children.forall(concrete) &&
              written.getOrElse(parent.value, Nil).forall(o => children.exists(_ eq o)) =>
          parent -> children.map(d => Child(nameOf(d), d.is[Defn.Object], parameterNames(d)))
      }
      .toMap
  }

  // Every template and enum case of the body's file, by the simple name of each parent it extends.
  private def extenders(stats: List[Stat]): Map[String, List[Tree]] = {
    @tailrec def top(tree: Tree): Tree = tree.parent match {
      case Some(parent) => top(parent)
      case None         => tree
    }
    val roots = stats.headOption.map(top).toList
    roots
      .flatMap(_.collect {
        case t: Template => t.inits.map(init => Solver.initParent(init) -> t.parent.getOrElse(t))
        case c: Defn.EnumCase => c.inits.map(init => Solver.initParent(init) -> c)
      })
      .flatten
      .groupMap(_._1)(_._2)
  }

  private def extendsParent(d: Defn, parent: TypeName): Boolean =
    d.children
      .collect { case t: Template => t.inits }
      .flatten
      .map(Solver.initParent)
      .contains(parent.value)

  private def concrete(d: Defn): Boolean = d match {
    case c: Defn.Class => isConcreteClass(c)
    case _             => true
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
    val solved =
      Solver.solve(
        entries,
        Shapes.of(stats),
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

    case select: Type.Select => qualified(scope, select)

    // Literal types (`true`, `42`, `'a'`) and singleton types (`None.type`) have exactly
    // one inhabitant: the value itself.
    case _: (Lit | Type.Singleton) => UnitSize

    case Type.ByName(tpe) => typeIn(scope)(tpe)

    // A repeated parameter holds the sequence of its arguments: the empty one alone over an empty
    // element type, countably many over any other — the linear collections' rule.
    case Type.Repeated(tpe) => if (typeIn(scope)(tpe) == NothingSize) UnitSize else EffectiveOmega

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
    case Type.ApplyInfix(l, Type.Name("&"), r) => typeIn(scope)(l).min(typeIn(scope)(r))

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
      else scope.resolve(name).getOrElse { scope.note(t); EffectiveOmega }

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

  // `q.T` reads through its qualifier and nothing else: a value's known type gives a class member
  // (`x.T`), an object or a package what its body defines, `scala.Int` the base type it spells.
  // Anything else is its own reason, with the clause its first name rests on, if any.
  private def qualified(scope: Scope, select: Type.Select): Size =
    located(scope, select) match {
      case Some(Namespaces.Located.Member(key, in))      => keyed(scope, select, key, in)
      case Some(Namespaces.Located.Instance(tpe, in))    => classMember(scope, select, tpe, in)
      case Some(Namespaces.Located.Unread(Some(clause))) =>
        unbound(scope, Definition.Unbound.Imported(TypeSubstitution.described(select), clause))
      case Some(Namespaces.Located.Unread(None))    => unbound(scope, select)
      case Some(Namespaces.Located.Pending(reason)) => unbound(scope, reason)
      case None => standard(select).getOrElse(unbound(scope, select))
    }

  private def located(scope: Scope, select: Type.Select): Option[Namespaces.Located] =
    Namespaces.locate(scope, select.qual, TypeName.of(select.name.value))

  // `scala.Int`, `scala.Option`, `java.lang.String`: the base types the standard library spells in
  // full, when no source namespace has that path.
  private def standard(select: Type.Select): Option[Size] =
    if (Namespaces.standardQualifier(select.qual)) BaseTypes.get(select.name.value)
    else if (Namespaces.javaString(select)) BaseTypes.get("String")
    else None

  // A member a namespace solved, read with the reasons its value rests on: a qualified reference
  // to a definition that holds an unknown name is no more bounded than the definition is.
  private def keyed(scope: Scope, select: Type.Select, key: TypeName, in: Scope): Size =
    in.size(key) match {
      case Some(_) if in.definition(key).exists(_.params.nonEmpty) =>
        constructor(scope, select.syntax)
      case Some(size) =>
        if (in.isBlocked(key)) scope.noteLazyCycle(key)
        in.definition(key).foreach(scope.taintOf(_, in))
        size
      case None => missing(scope, select, key, in)
    }

  // A key a namespace does not solve: an open abstraction of it, a sealed family it does not sum,
  // or the reference as written.
  private def missing(scope: Scope, select: Type.Select, key: TypeName, in: Scope): Size = {
    if (in.isOpen(key)) scope.noteOpen(select.syntax)
    else if (in.isUnsummed(key)) scope.noteUnsummed(TypeName.of(select.syntax))
    else scope.note(select)
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

  // The size of `C[args]` when `C` names a definition with that many parameters; None otherwise.
  // Two recursive readings stay careful: inside C's own equation, `C` applied to C's own
  // parameters borrows the fixed point the solver solved under the name (how a parameterised
  // recursion keeps its μ/ν reading) — the same spelling read anywhere else is an instantiation
  // like any other — and one re-entered while its own name is substituting has no such fixed
  // point, so it keeps the old fallback: ω, with the name as the reason.
  private def instantiation(scope: Scope, callee: Type, args: List[Type]): Option[Size] = {
    // A qualified reference reads with the reasons its member rests on; a bare one reads the
    // definition in place, or a namespace's while a reference's reasons are collected.
    val found = Namespaces.lookup(scope, callee) { (in, key, qualified) =>
      val context =
        if (qualified || !(in eq scope) && scope.collecting) scope.borrow(in)
        else in
      in.definition(key).map((key, context, _))
    }
    found
      .filter(_._3.params.size == args.size)
      .map(resolved => instantiated(scope, callee, args, resolved))
  }

  // One instantiation of a definition, read: the definition's recursion on its own parameters
  // borrows the solved fixed point, a repeat inside its own substitution keeps the old fallback, a
  // match type reduces on the argument's syntax, and everything else substitutes the arguments
  // into the equation.
  private def instantiated(
      scope: Scope,
      callee: Type,
      args: List[Type],
      resolved: (TypeName, Scope, Named)
  ): Size = {
    val (name, context, named) = resolved
    val self = scope.readingItself(named) && named.params.zip(args).forall {
      case (param, Type.Name(arg)) => arg == param.value
      case _                       => false
    }
    if (self && context.size(name).isDefined) context(name)
    else if (Scope.activeLens.get(context)(name)) {
      scope.note(callee)
      EffectiveOmega
    } else
      named.matchType match {
        // A match type reduces on the arguments' *syntax*. They, and the parts of them a case
        // pattern binds, are read where the reference is; the case body where the match type is
        // written, with those bound — a recursion through the body is a reason.
        case Some(matchType) =>
          MatchTypes.matched(matchType, named.params.zip(args).toMap) match {
            case Some((body, bindings)) =>
              val frame = bindings.map((binder, part) => binder -> typeIn(scope)(part))
              typeIn(named.read(Scope.activeLens.modify(_ + name)(context).instantiating(frame)))(
                body
              )
            case None =>
              // Stuck: the arguments still carry their own reasons — a parameter over which
              // the match type stays inert is one of them — and the match type is another.
              args.foreach(typeIn(scope))
              scope.note(matchType)
              EffectiveOmega
          }
        case None =>
          // The instantiation's frame is the only one its equation reads: the definition's names
          // resolve where it was written, not in whatever instantiation the reference sits in.
          val frame =
            named.params.zip(args).map((param, arg) => param -> typeIn(scope)(arg)).toMap
          named.equation(Scope.activeLens.modify(_ + name)(context).instantiating(frame))
      }
  }

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
    case (Modelled("Option"), List(t))                     => UnitSize + typeIn(scope)(t)
    case (Modelled("Either"), List(l, r))                  => typeIn(scope)(l) + typeIn(scope)(r)
    case (Modelled("Set"), List(t))                        => BooleanSize.pow(typeIn(scope)(t))
    case (Modelled("Map" | "PartialFunction"), List(k, v)) =>
      (typeIn(scope)(v) + UnitSize).pow(typeIn(scope)(k))
    // A linear collection over an empty element type holds just the empty collection; over any
    // other it is countably infinite — a number the algebra knows, so no reason is recorded.
    case (Modelled("List" | "Vector" | "Seq" | "IndexedSeq" | "Array"), List(t)) =>
      if (typeIn(scope)(t) == NothingSize) UnitSize else EffectiveOmega
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
        case Namespaces.Located.Member(key, in) => missing(scope, select, key, in)
        case _                                  => unbound(scope, select)
      }
    case (callee, _) => unbound(scope, callee)
  }

  // A constructor the algebra models, spelled bare (`Option`) or in full under the standard
  // library's qualifiers (`scala.Option`, `scala.collection.immutable.List`): its simple name.
  private object Modelled {

    private val names: Set[String] = Set(
      "Option",
      "Either",
      "Set",
      "Map",
      "PartialFunction",
      "List",
      "Vector",
      "Seq",
      "IndexedSeq",
      "Array",
      "LazyList",
      "Stream",
    )

    def unapply(tpe: Type): Option[String] = tpe match {
      case Type.Name(name) if names(name) => Some(name)
      case Type.Select(q, Type.Name(name)) if Namespaces.standardQualifier(q) && names(name) =>
        Some(name)
      case _ => None
    }

  }

  // A function's domain is the product of its parameter types; `Unit` (a single empty
  // tuple) when it takes none.
  private[cardinality] def domain(scope: Scope)(params: List[Type]): Size =
    params.foldLeft(UnitSize: Size)((acc, p) => acc * typeIn(scope)(p))

  // The function-space rule shared by plain and context function types in `typeIn`.
  private def functionSize(scope: Scope)(params: List[Type], result: Type): Size =
    arrow(typeIn(scope)(result), domain(scope)(params))

  // A named-tuple element (`a: Boolean`) carries its type inside a TypedParam.
  private def tupleElem: Type => Type = {
    case Type.TypedParam.After_4_7_8(_, tpe, _) => tpe
    case other                                  => other
  }

}
