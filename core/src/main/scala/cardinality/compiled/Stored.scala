package cardinality.compiled

import cardinality.capacity.Definition.{Kind, Unbound}
import cardinality.capacity.{
  BaseTypes,
  Blocked,
  EffectiveOmega,
  Fixpoint,
  NothingSize,
  Size,
  Store,
  UnitSize
}
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Trees.{
  ClassDef,
  OpaqueTypeAliasDefinitionTree,
  PolyTypeDefinitionTree,
  TermTree,
  Tree,
  TypeAliasDefinitionTree,
  TypeBoundsTree,
  TypeDefinitionTree,
  TypeMember,
  TypeTree,
  ValDef
}
import tastyquery.Types.*

/** What a definition stores, the size of its row (slice S2; v1.md §2, docs/type-arithmetic.md): a
  * size and, where it is no number, the reasons (a type parameter, an open class); neither for an
  * abstraction, which has no value of its own.
  *
  *   - An object is one value; a concrete class is the product of what its constructor's parameters
  *     store, whatever it extends; an enum is the sum of its cases and a sealed family of its
  *     subtypes; an alias is what its right-hand side stores; a top-level value is one value where
  *     its declared type is known to have one.
  *   - A base type is its table's size; tuples multiply, unions add, an intersection is its smaller
  *     side, a function is codomain ^ domain; the standard collections follow `standard`; a literal
  *     or singleton type is one value, a refinement the type it refines.
  *   - A type is read where it is written: a class's fields at the class, the body of an alias of
  *     an object or a package at the alias, an alias's arguments where they are. So an opaque type
  *     is its representation inside its scope and one value outside it (rule F), wherever the
  *     reading started, as the old estimate reads it. (The method side follows aliases first and
  *     decides at the use site: `Shapes`.)
  *   - A reference to a concrete class, an enum or a sealed family of the module, or of a
  *     dependency with TASTy outside the standard library and the JDK, is what it stores with its
  *     arguments bound; one to an open class, an unbound type parameter, an abstract type member, a
  *     class named without its arguments, an enum case's class or `AnyVal` is a reason.
  *   - A definition that reaches itself is a system of equations, one per class of the module it
  *     names, solved by the old estimate's fixed point (`solved`, `Fixpoint`).
  *
  * What it does not read yet is a `Left`, naming what it met. The main ones: a sealed family with a
  * subtype it does not read whole (`family`), a recursion through a function's domain, one a lazy
  * field leaves incomplete, one whose widening is not settled, a class a system reads with ever new
  * type arguments or through a type argument, a restricted constructor (rule B), an enum case whose
  * clause does not pass its own parameters on, a class the algebra does not model or of a
  * dependency without TASTy, an opaque type of the standard library or of a class named inside it,
  * an alias of a class beside an opaque type's scope, a wildcard argument, a refinement of no
  * written type, a function type refined by its `apply`, any other form of type, and a value of a
  * class of a dependency or of an enum with parameters and a value case.
  */
final private[compiled] class Stored(world: World)(using Context) {
  import Stored.{Env, Held, Instance, Row, Values, given}

  /** The row of a definition. */
  def row(kind: Kind, tree: Tree): Either[String, Row] = (kind, tree) match {
    case (Kind.Object, _) => Right(Row(Some(UnitSize), Nil))
    // A constructor alias names a function on types, and an open trait or abstract class has no
    // value of its own: a dash, no size and no reason.
    case (Kind.Constructor, _)                         => Right(Row(None, Nil))
    case (_, c: ClassDef) if c.symbol.isEnum           => own(c)(enumeration(c, _))
    case (_, c: ClassDef) if Shapes.isFamily(c.symbol) =>
      strayed(c.symbol).map(Right(_)).getOrElse(own(c)(familyOnce(c, _)))
    case (_, c: ClassDef) if c.symbol.isAbstractClass => Right(Row(None, Nil))
    case (_, c: ClassDef)                             => own(c)(constructed(c, _))
    case (_, t: TypeMember)                           => alias(t)
    case (_, v: ValDef)                               => topLevel(v)
    case _                                            => Left("a definition")
  }

  // A definition read for its own row: each of its type parameters is a reason the row has no one
  // size, and a reference to itself is solved as a system (`solved`), whose value it is.
  private def rowEnv(tree: ClassDef): Env = {
    val c = tree.symbol
    Env(c.typeParams.map(p => (p: Symbol) -> Stored.parameter(p)).toMap, c, Set(c))
  }

  private def own(tree: ClassDef)(reading: Env => Either[String, Held]): Either[String, Row] = {
    val env = rowEnv(tree)
    solved(env, Some(Instance(tree.symbol, tree.symbol.typeParams.map(env.binders))))(reading)
      .map(_.row)
  }

  // What a class's constructor stores: the product of its parameters, the evidence of a context
  // bound among them, each read at the class. The constructor names its own copies of the class's
  // type parameters.
  private def constructed(tree: ClassDef, env: Env): Either[String, Held] =
    if (!built(tree)) each.tupled(fields(tree, env)).map(Algebra.product)
    else if (world.extended(tree.symbol))
      Left(s"a restricted constructor: ${tree.symbol.displayFullName}")
    else restricted(tree, env)

  // Whether a class holds what its public builders build (rule B): its constructor is restricted
  // and it has no type parameter. A class with type parameters keeps its fields' product, as the
  // old estimate (`capacity.Factories`) reads it.
  private def built(tree: ClassDef): Boolean =
    !tree.rhs.constr.symbol.isPublic && tree.symbol.typeParams.isEmpty

  // Rule B, for a class nothing supplied extends (a subclass's constructor builds it too): it
  // holds what its public secondary constructors and factories build (`Factories.builders`), each
  // at most min(its parameters, the fields a public member or field shows or code outside the
  // class may read; none: one value), the class the largest of these. "Public" is what a site
  // outside every template may call: a row has no measuring site.
  private def restricted(tree: ClassDef, env: Env): Either[String, Held] = {
    val name = tree.symbol.displayFullName
    val site = defn.RootPackage
    // Each parameter read where it is written: a factory's at the factory.
    def stored(parameters: List[ValDef]) =
      Traversals
        .sequence(parameters.map(p => of(p.tpt.toType, env.copy(site = p.symbol.owner))))
        .map(Algebra.product)
    val builders = factories.builders(tree, site)
    for {
      _ <- Either.cond(builders.nonEmpty, (), s"a restricted constructor: $name")
      _ <- Either.cond(
        !factories.toParent(tree),
        (),
        s"a restricted constructor passing a parameter to a parent: $name"
      )
      fields <- stored(factories.observed(tree, site, outside = true))
      bounds <- Traversals.sequence(builders.map {
        case Some(parameters) => stored(parameters).map(held => Some(held.map(_.min(fields.size))))
        case None             => Right(None)
      })
      largest <-
        if (bounds.exists(_.exists(_.size == fields.size))) Right(fields)
        else if (bounds.contains(None))
          Left(s"a restricted constructor with a builder whose space is not known: $name")
        else Right(bounds.flatten.maxBy(_.size)(using Size.order.toOrdering))
    } yield largest.joined(fields :: bounds.flatten)
  }

  private lazy val factories = new Factories(world)

  // A class's constructor's parameters, and where they are read.
  private def fields(tree: ClassDef, env: Env): (List[Type], Env) =
    (Shapes.parameters(tree.rhs.constr).map(_.tpt.toType), atClass(tree, env))

  // Where a class's constructor is written: at the class, with the constructor's own copies of the
  // class's type parameters bound, and written, as the class's are.
  private def atClass(tree: ClassDef, env: Env): Env = {
    val copies = Shapes.typeParameters(tree.rhs.constr)
    env.copy(
      binders = env.binders ++ Shapes.copied(copies, tree.symbol, env.binders),
      written = env.written ++ Shapes.copied(copies, tree.symbol, env.written),
      site = tree.symbol
    )
  }

  /** What one position of a class's constructor stores, as the method side's value space of it is
    * compared with (slice S3): the position's written type is read as an input, so a top type in it
    * is one value, and with the method side's saturation, so a collection is not lifted to its
    * element's tier. An alias is transparent to that; a class of the module the type names is read
    * as it stores.
    */
  def position(tree: ClassDef, tpe: Type): Either[String, Row] =
    solved(atClass(tree, rowEnv(tree)).copy(input = true))(of(tpe, _)).map(_.row)

  // An enum is the sum of its cases: a value case is one value, a class case what it stores, each
  // where its clause passes the enum's parameters on (`Sums.carried`: `case Y extends E[Int]` is a
  // member of one instantiation only). The enum's own constructor arguments are shared state, not
  // extra values.
  // A case whose construction always throws, the enum's own or that of a template building it, is
  // never built and adds nothing (register D26), as `Sums` reads it.
  private def enumeration(tree: ClassDef, env: Env): Either[String, Held] = {
    lazy val noValue = (tree :: Sums.singletons(tree.symbol)).exists(t => world.unbuilt(t.symbol))
    cases(tree, env)
      .flatMap(found =>
        Traversals.sequence(found.map {
          case None => Right(Held(if (noValue) NothingSize else UnitSize))
          case Some((below, _)) if world.unbuilt(below.symbol) => Right(Held(NothingSize))
          case Some((below, inside))                           => constructed(below, inside)
        })
      )
      .map(Algebra.sum)
  }

  // An enum's cases: none for a value case, a class case's tree with its parameters bound to what
  // its clause passes on.
  private def cases(tree: ClassDef, env: Env): Either[String, List[Option[(ClassDef, Env)]]] = {
    val enumClass = tree.symbol
    val carried = passed(enumClass, env, "an enum case")
    enumClass.sealedChildren match {
      // An enum has a case: none means the classpath lacks the annotation that lists them.
      case Nil   => Left(s"an enum whose cases are not listed: ${enumClass.displayFullName}")
      case cases =>
        Traversals.sequence(cases.map {
          case single: TermSymbol => carried(single).map(_ => None)
          case cls: ClassSymbol   =>
            for {
              below <- World.tree(cls)
              bound <- carried(cls)
            } yield Some(below -> env.copy(binders = cls.typeParams.zip(bound).toMap))
        })
    }
  }

  // The arguments a case of `parent` binds its own parameters to (`Sums.carried`).
  private def passed(parent: ClassSymbol, env: Env, what: String)(
      child: ClassSymbol | TermSymbol
  ): Either[String, List[Held]] =
    Sums
      .carried(child, parent, parent.typeParams.map(env.binders))
      .toRight(s"$what that does not pass its own parameters on: ${child.displayFullName}")

  // A sealed family is the sum of its subtypes: an object is one value, a concrete class what it
  // stores, a sealed level below its own sum, each where its clause passes the family's parameters
  // on. A subtype whose construction always throws is never built and adds nothing (register
  // D26), and so is every subtype of a family whose own does. Every other form is a `Left` below.
  // (`Sums` reads the same families for the method side, with its own refusals.)
  private def family(tree: ClassDef, env: Env): Either[String, Held] = {
    val parent = tree.symbol
    val name = parent.displayFullName
    val children = ownCases(parent)
    val carried = passed(parent, env, "a subtype")
    def subtype(child: ClassSymbol | TermSymbol): Either[String, Held] = child match {
      case value: TermSymbol if value.isGivenOrUsing =>
        Left(s"a family with a given instance: $name")
      case value: TermSymbol if value.isEnumCase =>
        Left(s"an enum case under a sealed family: ${value.displayFullName}")
      case _ if !World.inObjects(child) => Left(s"a family with an inner or local subtype: $name")
      case value: TermSymbol            =>
        for {
          unbuilt <- value.moduleClass.fold(Right(false))(c =>
            World.tree(c).map(_ => world.unbuilt(c))
          )
          _ <- carried(value)
        } yield Held(if (unbuilt) NothingSize else UnitSize)
      case cls: ClassSymbol =>
        for {
          below <- World.tree(cls)
          bound <- carried(cls)
          inside = env.copy(
            binders = cls.typeParams.zip(bound).toMap,
            visiting = env.visiting + cls,
            written = Sums
              .carried(cls, parent, parent.typeParams.map(env.written.get))
              .toList
              .flatMap(cls.typeParams.zip(_).collect { case (p, Some(w)) => p -> w })
              .toMap
          )
          held <-
            if (world.unbuilt(cls)) Right(Held(NothingSize))
            else if (env.system.isEmpty && env.visiting(cls)) Left(Stored.Reaches(cls))
            else if (cls.isEnum) Left(s"an enum under a sealed family: ${cls.displayFullName}")
            else if (Shapes.isFamily(cls))
              env.system.fold(family(below, inside))(variable(cls, bound))
            else if (cls.isAbstractClass)
              Left(s"a family with an open abstract subtype: ${cls.displayFullName}")
            else if (world.extended(cls))
              Left(s"a family with a subtype something extends: ${cls.displayFullName}")
            // A subtype nothing extends is final in the closed world, written so or not. One
            // not written final whose parent is outside the module is left, for parity alone:
            // the old estimate refuses it (it did not read that parent) and counts its `final`
            // twin, and what such a parent declares is the index's to say.
            else if (
              !Shapes.isFinal(cls) && World.ancestors(cls).exists(base => !world.ofModule(base))
            )
              Left(
                s"a family with a subtype not written final whose parent is outside the module: ${cls.displayFullName}"
              )
            else env.system.fold(constructed(below, inside))(variable(cls, bound))
        } yield held
    }
    // A family a class declares is one per instance of that class. An anonymous instance lists
    // the family itself; none listed for a family something extends means the classpath lacks the
    // annotation that lists them.
    if (!World.inObjects(parent)) Left(s"a family nested in a class or a body: $name")
    else if (parent.sealedChildren.contains(parent))
      Left(s"a family with an anonymous instance: $name")
    else if (children.isEmpty && world.extended(parent))
      Left(s"a family whose subtypes are not listed: $name")
    else if (world.unbuilt(parent)) Right(Held(NothingSize))
    else Traversals.sequence(children.map(subtype)).map(Algebra.sum)
  }

  // The row of a family that an enum's value case extends directly is the old estimate's: no size
  // and its refusal, whatever the other cases hold. (A reference to such a family, and a family
  // that has one under a sealed level, is a `Left` of `family`.)
  private def strayed(parent: ClassSymbol): Option[Row] =
    ownCases(parent).collectFirst {
      case stray: TermSymbol if stray.isEnumCase =>
        val why =
          s"sealed family not summed: ${parent.name} (an enum case extends it: ${stray.name})"
        Row(None, List(Unbound.Refusal(why)))
    }

  // A family's subtypes as its own level counts them: a case under one of its sealed levels is
  // that level's (a sealed trait that extends a sealed class makes its cases the class's too).
  private def ownCases(parent: ClassSymbol): List[ClassSymbol | TermSymbol] = {
    val children = parent.sealedChildren
    val levels = children.collect {
      case c: ClassSymbol if c != parent && Shapes.isFamily(c) => c
    }
    children.filterNot { child =>
      val cls = child match {
        case c: ClassSymbol => Some(c)
        case v: TermSymbol  => v.moduleClass
      }
      // One whose clause names this level itself beside the sublevel extends it twice.
      cls.exists(c =>
        levels.exists(level => level != c && c.isSubClass(level)) &&
          !c.tree.exists(_.rhs.parents.exists(clauseNames(_, parent)))
      )
    }
  }

  // Whether a parent of a template is one somebody wrote (the compiler's own have no extent) that
  // names `parent`.
  private def clauseNames(clause: Tree, parent: ClassSymbol): Boolean = {
    val named = clause match {
      case tree: TypeTree => Some(tree.toType)
      case term: TermTree => Some(term.tpe).collect { case tpe: Type => tpe }
      case _              => None
    }
    !clause.pos.isZeroExtent && named.flatMap(World.classNamed).contains(parent)
  }

  // The cases a family counts, through its sealed levels.
  private def leaves(parent: ClassSymbol): List[Symbol] = ownCases(parent).flatMap {
    case cls: ClassSymbol if cls != parent && Shapes.isFamily(cls) => leaves(cls)
    case leaf                                                      => List(leaf)
  }

  // A family read from its top, left where a subtype is under two of its levels: it is one case,
  // and would be counted once per level.
  private def familyOnce(tree: ClassDef, env: Env): Either[String, Held] = {
    val reached = leaves(tree.symbol)
    reached.diff(reached.distinct).headOption match {
      case Some(twice) => Left(s"a subtype reached twice: ${twice.displayFullName}")
      case None        => family(tree, env)
    }
  }

  // An alias row is what its right-hand side stores, read where the alias is written: an opaque
  // one shows its representation. The parameters of `type F[X] = …` are the row's reasons.
  private def alias(tree: TypeMember): Either[String, Row] =
    body(tree.rhs)
      .flatMap((_, aliased) => solved(Env(Map.empty, tree.symbol))(of(aliased, _)))
      .map(_.row)

  // The type an alias's definition names, and the parameters it is written under.
  private def body(definition: TypeDefinitionTree): Either[String, (List[Symbol], Type)] =
    definition match {
      case PolyTypeDefinitionTree(binders, inner) =>
        body(inner).map((more, aliased) => (binders.map(_.symbol) ++ more, aliased))
      case TypeAliasDefinitionTree(alias)          => Right(Nil -> alias.toType)
      case OpaqueTypeAliasDefinitionTree(_, alias) => Right(Nil -> alias.toType)
      case tree: TypeBoundsTree if PathTypes.exported(tree.toTypeBounds) =>
        Right(Nil -> tree.toTypeBounds.high)
      case _ => Left("an abstract type member")
    }

  // A top-level value holds one value where its declared type has one (register D30): the size is
  // 1, and the row is undecided where the type has reasons, is empty, has a size that does not say
  // a value exists, or names a class of the module whose construction always throws (`undecided`, at
  // every class the reading meets). The initializer is never read.
  private def topLevel(tree: ValDef): Either[String, Row] =
    solved(Env(Map.empty, tree.symbol, isValue = true))(of(tree.tpt.toType, _)).map { held =>
      val reasons =
        if (held.reasons.nonEmpty) held.reasons
        else if (held.size.isZero || held.undecided)
          List(Unbound.Refusal(s"value not decided: ${tree.tpt.toType.showBasic}"))
        else Nil
      Row(Some(UnitSize), reasons.distinct)
    }

  // ---- types --------------------------------------------------------------------------------

  // What a type stores, read in `env`. An alias is followed where it is named (`named`), not
  // before: its body is read at the alias.
  private def of(tpe: Type, env: Env): Either[String, Held] = tpe match {
    case by: ByNameType           => of(by.resultType, env)
    case annotated: AnnotatedType => of(annotated.typ, env)
    case repeated: RepeatedType => of(repeated.elemType, env).map(_.map(Algebra.linear(env.input)))
    case _: NothingType         => Right(Held(NothingSize))
    // A literal type has one value. A singleton is read as one too, which does not say that its
    // path denotes a value.
    case _: ConstantType         => Right(Held(UnitSize))
    case _: (TermRef | ThisType) => Right(Held(UnitSize, undecided = true))
    // A union adds its sides, once where both are one type; an intersection is no more than its
    // smaller side.
    case or: OrType =>
      if (or.first.isSameType(or.second)) of(or.first, env)
      else both(or.first, or.second, env)(_ + _)
    case and: AndType => both(and.first, and.second, env)(_.min(_)).map(_.copy(undecided = true))
    case refined: RefinedType        => refinement(refined, env)
    case World.Named(ref, arguments) => named(ref, arguments, env)
    // A type lambda applied is its body at the arguments, as the compiler reduces it.
    case applied: AppliedType if applied.tycon.isInstanceOf[TypeLambda] => of(applied.dealias, env)
    case matched: MatchType                                             => matchType(matched, env)
    case other => Left(s"a type: ${other.showBasic}")
  }

  // A match type is what the compiler reduces it to, where the method side trusts the reduction
  // (`PathTypes.trusted`); one stuck on a type parameter that stands for what the
  // instance chooses is what the instance chooses, as the parameter is.
  private def matchType(matched: MatchType, env: Env): Either[String, Held] =
    (PathTypes.trusted(matched), matched.reduced, matched.scrutinee.dealias) match {
      case (Some(reduced), _, _)   => of(reduced, env)
      case (_, None, ref: TypeRef) =>
        ref.optSymbol match {
          // A class's parameter is the argument the class was named with, outside a system (whose
          // classes are keyed by their arguments' sizes alone), read with both sites' binders and
          // classes being read; one symbol bound at both, otherwise, is refused.
          case Some(binder) if env.system.isEmpty && env.written.contains(binder) =>
            val (argument, outer) = env.written(binder)
            if (outer.binders.exists((s, held) => env.binders.get(s).exists(_ != held)))
              Left(s"a match type over a parameter bound again: ${matched.showBasic}")
            else
              matchType(
                MatchType(matched.bound, argument, matched.cases),
                env.copy(binders = outer.binders ++ env.binders, written = outer.written)
              )
          case Some(binder) if env.binders.contains(binder) =>
            val held = env.binders(binder)
            Either.cond(
              held.reasons.nonEmpty && held.reasons.forall(_.isInstanceOf[Unbound.Parameter]),
              held,
              s"a type: ${matched.showBasic}"
            )
          case Some(free: (ClassTypeParamSymbol | LocalTypeParamSymbol)) =>
            Right(Stored.parameter(free))
          case _ => Left(s"a type: ${matched.showBasic}")
        }
      case _ => Left(s"a type: ${matched.showBasic}")
    }

  // A refinement is no more than the type it refines. One written without a type refines `Object`
  // in the tree, as one written `Object { … }` does: the two are not told apart, and the first has
  // no size. A function type refined by its `apply` is a dependent or polymorphic one, whose
  // result the parent does not hold.
  private def refinement(refined: RefinedType, env: Env): Either[String, Held] = refined match {
    case _ if refined.parent.isSameType(defn.ObjectType) =>
      Left("a refinement of no written type")
    case method: TermRefinement if method.refinedName.toString == "apply" =>
      Left(s"a function type refined by its apply: ${refined.showBasic}")
    case _ => of(refined.parent, env).map(_.copy(undecided = true))
  }

  private def both(left: Type, right: Type, env: Env)(f: (Size, Size) => Size) =
    for {
      l <- of(left, env)
      r <- of(right, env)
    } yield l.zip(r)(f)

  private def each(types: List[Type], env: Env): Either[String, List[Held]] =
    Traversals.sequence(types.map(of(_, env)))

  private def named(
      ref: TypeRef,
      arguments: List[TypeOrWildcard],
      env: Env
  ): Either[String, Held] = {
    def applied(constructor: Type) =
      World.proper(arguments).flatMap(types => of(constructor.appliedTo(types), env))
    ref.optSymbol match {
      // A type parameter, bare or applied (`F[A]`): what it is bound to, whatever its arguments.
      case Some(binder) if env.binders.contains(binder) => Right(env.binders(binder))
      // A type parameter nothing bound, wherever it is declared.
      case Some(free: (ClassTypeParamSymbol | LocalTypeParamSymbol)) =>
        Right(Stored.parameter(free))
      // A generic class named without its arguments is a type constructor.
      case Some(c: ClassSymbol) if c.typeParams.sizeIs != arguments.size =>
        Right(Held(EffectiveOmega, List(Unbound.Syntax(s"type constructor: ${c.name}"))))
      case Some(c: ClassSymbol) =>
        World.proper(arguments).flatMap(types => each(types, env).map(types -> _)).flatMap {
          (types, sizes) =>
            world.library(c) match {
              case Some(name)                         => Algebra.standard(name, sizes, env.input)
              case None if world.readByDeclaration(c) =>
                declared(c, sizes, env.copy(written = c.typeParams.zip(types.map(_ -> env)).toMap))
              case None => Left(s"a class of a dependency: ${c.displayFullName}")
            }
        }
      // The empty tuple is a base type, though the library spells it as an alias of a singleton.
      case Some(member: TypeMemberSymbol) if Stored.EmptyTuple.matches(member.displayFullName) =>
        BaseTypes.get("EmptyTuple").map(Held(_)).toRight(s"a type: ${ref.showBasic}")
      case Some(member: TypeMemberSymbol) =>
        (member.typeDef, ref.optAliasedType, ref.bounds) match {
          // A named tuple, `NamedTuple[names, values]`, stores its values' tuple, as the method
          // side reads it (`StdlibModel`).
          case (_: TypeMemberDefinition.OpaqueTypeAlias, _, _)
              if member.displayFullName == "scala.NamedTuple$.NamedTuple" && arguments.sizeIs == 2 =>
            World.proper(arguments).flatMap(types => of(types.last, env))
          case (_: TypeMemberDefinition.OpaqueTypeAlias, _, _) =>
            opaque(member, env)(written(member, arguments, env))
          // A match type alias is applied to its arguments, where the compiler reduces it.
          case (_, Some(body), _) if Stored.matches(body) => applied(body)
          // An alias of the module in an object or a package is read where it is written, its
          // arguments where they are.
          case (_, Some(_), _) if world.ofModule(member) && World.inObjects(member) =>
            written(member, arguments, env)
          // An alias a class declares is read as its prefix sees it, at the use site, which
          // mixes what the alias names with what the prefix does: not where either side is inside
          // an opaque type's scope, where the two sites may differ.
          case (_, Some(_), _) if world.ofModule(member) && (scoped(member) || scoped(env.site)) =>
            Left(s"an alias of a class beside an opaque type's scope: ${member.displayFullName}")
          case (_, Some(body), _) => applied(body)
          // An export's alias is a member bounded by one type on both sides: that type.
          case (_, _, bounds) if PathTypes.exported(bounds) => applied(bounds.high)
          // An abstract member nothing fixed is what each instance chooses.
          case _ => Right(Held(EffectiveOmega, List(Unbound.Member(member.name.toString))))
        }
      // A member a refinement declares (`R#X` for `R = { type X = Unit }`) has no symbol: the type
      // its bounds pin, if any (`PathTypes.pinned`).
      case None => PathTypes.pinned(ref).fold(Left(s"a type: ${ref.showBasic}"))(applied)
      case _    => Left(s"a type: ${ref.showBasic}")
    }
  }

  // An alias of the module, opaque or not, read where it is written (its body at the alias, rule
  // F decided there), with its parameters bound to the arguments as they read where they are
  // written. A curried alias applied to its first parameters only is not read.
  private def written(
      member: TypeMemberSymbol,
      arguments: List[TypeOrWildcard],
      env: Env
  ): Either[String, Held] =
    for {
      definition <- member.tree.toRight(s"an alias without TASTy: ${member.displayFullName}")
      read <- body(definition.rhs)
      (parameters, aliased) = read
      _ <- Either.cond(
        parameters.sizeIs == arguments.size,
        (),
        s"an alias applied to another number of arguments: ${member.displayFullName}"
      )
      sizes <- World.proper(arguments).flatMap(each(_, env))
      held <- of(aliased, env.copy(binders = parameters.zip(sizes).toMap, site = member))
    } yield held

  // Whether a symbol is inside the scope of some opaque type: a template around it declares one.
  private def scoped(symbol: Symbol): Boolean =
    World.owners(symbol).collect { case c: ClassSymbol => c.declarations }.flatten.exists {
      case alias: TypeMemberSymbol => alias.isOpaqueTypeAlias
      case _                       => false
    }

  // Rule F: an opaque alias of the module is one value where its representation is hidden, and its
  // representation (`inside`) where the type naming it is written inside its scope. One a class
  // declares, named inside that class, is seen through an instance, which is not read.
  private def opaque(alias: TypeMemberSymbol, env: Env)(
      inside: => Either[String, Held]
  ): Either[String, Held] =
    if (World.standard(alias))
      Left(s"an opaque type of the standard library: ${alias.displayFullName}")
    else if (!World.owners(env.site).contains(alias.owner)) Right(Held(UnitSize))
    else
      alias.owner match {
        case owner: ClassSymbol if owner.isModuleClass => inside
        case _ => Left(s"an opaque type of a class: ${alias.displayFullName}")
      }

  // A class of the module: a concrete one or an enum is what it stores with its type parameters
  // bound to `arguments`; an open one is a reason.
  private def declared(c: ClassSymbol, arguments: List[Held], env: Env): Either[String, Held] = {
    val name = c.name.toString
    if (env.system.isEmpty && env.visiting(c)) Left(Stored.Reaches(c))
    else if (env.isValue && !world.ofModule(c))
      Left(s"a value of a class of a dependency: ${c.displayFullName}")
    else if (c.isEnum && c.isCaseClass)
      Right(Held(EffectiveOmega, List(Unbound.Refusal(s"enum case type not read: $name"))))
    // Any subtype anywhere may add values to an open trait or abstract class.
    else if (c.isAbstractClass && !c.isEnum && !Shapes.isFamily(c))
      Right(Held(EffectiveOmega, List(Unbound.Open(name))))
    else
      for {
        // Inside a system a concrete class named over arguments that read its values is read in
        // place at them (no variable: its key would hold sizes the fixed point changes), where no
        // field is by-name or a function, nor any type argument holds one (which the ν completion
        // reads of a variable alone); one reached again so is refused (`variable`).
        held <- env.system.fold(expand(c, arguments, env))(values =>
          if (
            arguments.exists(_.refs.nonEmpty) && !env.visiting(c) && World.strict(c) &&
            !c.typeParams.flatMap(env.written.get).exists((tpe, _) => World.deferred(tpe))
          )
            expand(c, arguments, env)
          else variable(c, arguments)(values)
        )
        thrown <- if (env.isValue) undecided(c) else Right(false)
      } yield held.copy(undecided = held.undecided || thrown)
  }

  // While a value row reads a class of the module, what it stores is no evidence that an instance
  // is ever built (register D30, the old `Builds`): one whose construction, an ancestor's of the
  // module or, for an enum, a class case's always throws leaves the row undecided. An enum with
  // parameters and a value case is not read: its clause's arguments are not read here.
  private def undecided(c: ClassSymbol): Either[String, Boolean] = {
    val cases = if (c.isEnum) c.sealedChildren else Nil
    if (cases.exists(_.isTerm) && c.tree.exists(t => Shapes.parameters(t.rhs.constr).nonEmpty))
      Left(s"a value of an enum with parameters and a value case: ${c.displayFullName}")
    else Right((c :: cases.collect { case cls: ClassSymbol => cls }).exists(world.unbuilt(_)))
  }

  // What a class of the module stores with its type parameters bound to `arguments`, as it is
  // declared, whatever position named it.
  private def expand(c: ClassSymbol, arguments: List[Held], env: Env): Either[String, Held] =
    World.tree(c).flatMap { tree =>
      val inside = env.copy(
        binders = c.typeParams.zip(arguments).toMap,
        visiting = env.visiting + c,
        input = false
      )
      if (c.isEnum) enumeration(tree, inside)
      else if (Shapes.isFamily(c)) familyOnce(tree, inside)
      else constructed(tree, inside)
    }

  // ---- recursion ----------------------------------------------------------------------------

  // A reading that reaches a class it is inside of is read again as a system of equations, one per
  // class of the module it names (`variable`), and solved by the old estimate's fixed point
  // (`Fixpoint`; docs/type-arithmetic.md §8): least, widened to its tier where a cycle keeps
  // growing, and completed coinductively through the fields a constructor does not demand. A cycle
  // through a function's domain has no count, nor has one a lazy field leaves incomplete, one whose
  // widened value its own equation does not reach again (an intersection's bound, a factor of 0,
  // as the old reading refuses an instantiation it does not settle), or a class read with ever new
  // type arguments. The row's reasons are those of every class read; a definition's own row
  // (`self`) is its value in the system.
  private def solved(env: Env, self: Option[Instance] = None)(
      reading: Env => Either[String, Held]
  ): Either[String, Held] =
    reading(env) match {
      case Left(Stored.Reaches()) => system(env, self, reading)
      case read                   => read
    }

  private def system(
      env: Env,
      self: Option[Instance],
      reading: Env => Either[String, Held]
  ): Either[String, Held] = {
    def at(values: Values) = env.copy(system = Some(values))
    for {
      root <- reading(at(Values()))
      found <- Stored.ordered(root.refs).foldLeft(Right(Map.empty): Found) { (acc, k) =>
        acc.flatMap(discovered(env.isValue)(k, _))
      }
      keys = Stored.ordered(found.keys)
      reaches = Fixpoint.reaches(found.view.mapValues(_.refs).toMap)
      _ <- keys
        .collectFirst {
          case k if found(k).domains.exists(to => to == k || reaches(to)(k)) =>
            s"recursion through a function's domain: ${k.cls.displayFullName}"
        }
        .toLeft(())
      nu <- lazily(found)
      eqs = keys.map(k =>
        k -> ((values: Values) => settled(equation(env.isValue)(values)(k), NothingSize)(_.size))
      )
      values = Fixpoint.solve(eqs, Set.empty, Values(), nu, reaches)
      _ <- keys
        .collectFirst {
          case k if values.blocked.contains(k) => values.blocked(k).about(k.cls.displayFullName)
        }
        .toLeft(())
      // Every equation read again over the solved values: what it reads and its reasons.
      all <- Traversals.sequence(keys.map(k => equation(env.isValue)(values)(k).map(k -> _)))
      _ <- all
        .collectFirst {
          case (k, held) if values.value(k).larger(held.size) =>
            s"a recursion whose widening is not settled: ${k.cls.displayFullName}"
        }
        .toLeft(())
      read <- reading(at(values))
    } yield read
      .copy(size = self.filter(found.contains).fold(read.size)(values.value))
      .joined(all.map(_._2))
  }

  private type Found = Either[String, Map[Instance, Held]]

  // A class of the module inside a system: its value in `values`, a variable of the equation.
  private def variable(c: ClassSymbol, arguments: List[Held])(
      values: Values
  ): Either[String, Held] =
    if (arguments.exists(_.refs.nonEmpty))
      Left(s"a class a system reads through a type argument: ${c.displayFullName}")
    else {
      val key = Instance(c, arguments)
      Right(Held(values.value(key), refs = Set(key)))
    }

  // A class's equation, read for a value row (`isValue`) or a definition's.
  private def equation(isValue: Boolean)(values: Values)(k: Instance): Either[String, Held] =
    expand(k.cls, k.arguments, Env(Map.empty, k.cls, isValue = isValue, system = Some(values)))

  // A reading inside a system after `discovered` has read every equation over empty values: what a
  // reading may refuse does not depend on the values, and `system` reads every equation again
  // over the solved ones, so a refusal cannot first appear here.
  private def settled[A, B](read: Either[String, A], none: B)(f: A => B): B =
    read.fold(_ => none, f)

  // The classes a system reads from `k` on, each with its equation read over empty values. A class
  // read with ever new type arguments (`N[A](n: Option[N[(A, A)]])`) would not end: one read with
  // more argument lists than `Stored.Instantiations` is refused, whatever order meets them.
  private def discovered(isValue: Boolean)(k: Instance, found: Map[Instance, Held]): Found =
    if (found.contains(k)) Right(found)
    else if (found.keys.count(_.cls == k.cls) >= Stored.Instantiations)
      Left(s"a class a system reads with ever new type arguments: ${k.cls.displayFullName}")
    else
      equation(isValue)(Values())(k).flatMap { held =>
        Stored.ordered(held.refs).foldLeft(Right(found.updated(k, held)): Found) { (acc, next) =>
          acc.flatMap(discovered(isValue)(next, _))
        }
      }

  // What the ν completion reads of a system (`Fixpoint.Lazy`): each concrete class's and enum's
  // constructors, a family's subtypes, and the classes a by-name or function field names.
  private def lazily(found: Map[Instance, Held]): Either[String, Fixpoint.Lazy[Instance, Values]] =
    Traversals.sequence(found.keys.toList.map(k => constructors(k).map(k -> _))).map { owned =>
      val arms = owned.collect { case (k, Some(arms)) => k -> arms }.toMap
      Fixpoint.Lazy(
        arms = arms.map((k, fields) => k -> fields.map(arm(found.keySet))),
        parents = found.collect { case (k, held) if !arms.contains(k) => k -> held.refs.toList },
        lazily = arms.map((k, fields) =>
          k -> fields.flatMap((types, env) => types.flatMap(lazyNames(env))).toSet
        )
      )
    }

  // A class's constructors as the ν completion reads them: an enum's cases (a value case has no
  // field) or a concrete class's own; a family has none, nor a class its builders build.
  private def constructors(k: Instance): Either[String, Option[List[(List[Type], Env)]]] =
    World.tree(k.cls).flatMap { tree =>
      val inside = Env(k.cls.typeParams.zip(k.arguments).toMap, k.cls)
      if (k.cls.isEnum)
        cases(tree, inside).map(found => Some(found.map(_.fold(Nil -> inside)(fields.tupled))))
      // A family, or a class its builders build (`restricted`), is read through what it names.
      else if (Shapes.isFamily(k.cls) || built(tree)) Right(None)
      else Right(Some(List(fields(tree, inside))))
    }

  private def arm(keys: Set[Instance])(
      types: List[Type],
      env: Env
  ): Fixpoint.Arm[Instance, Values] = {
    val classified = types.map(tpe => tpe -> continuation(tpe, env, keys))
    val rest = classified.collect { case (tpe, None) => tpe }
    Fixpoint.Arm(classified.flatMap(_._2), sized(rest, env), rest.flatMap(names(_, env)).toSet)
  }

  // A field that continues a cycle where its constructor does not demand it, as the old estimate
  // classifies one (`Solver.continuation`): `=> X`, `=> Option[X]` and `() => X`
  // deterministically, a strict `Option[X]`, and `D => X` (not a context function) with a domain
  // that names no class of the system, where `X` is a class of the system named without arguments.
  private def continuation(
      tpe: Type,
      env: Env,
      keys: Set[Instance]
  ): Option[Fixpoint.Continuation[Instance, Values]] = {
    import Fixpoint.Continuation.Kind
    def instance(t: Type) = t match {
      case World.Named(ref, Nil) =>
        ref.optSymbol.collect { case c: ClassSymbol => Instance(c, Nil) }.filter(keys)
      case _ => None
    }
    def target(t: Type) = t match {
      case World.Applied("scala.Option", List(inner)) => instance(inner)
      case other                                      => instance(other)
    }
    def edge(kind: Kind, domain: List[Type])(to: Instance) =
      Fixpoint.Continuation(to, kind, sized(List(tpe), env), sized(domain, env))
    tpe.dealias match {
      case by: ByNameType => target(by.resultType).map(edge(Kind.Deterministic, Nil))
      case World.Applied(name, domain :+ result)
          if name.matches("scala\\.Function\\d+") && domain.forall(names(_, env).isEmpty) =>
        target(result).map(
          edge(if (domain.isEmpty) Kind.Deterministic else Kind.FunctionField, domain)
        )
      case World.Applied("scala.Option", List(inner)) =>
        instance(inner).map(edge(Kind.OptionBranch, Nil))
      case _ => None
    }
  }

  // The classes of a system a field names lazily, as the old estimate's `lazyTargets`: under a
  // by-name type or as a function's result, a context function's included.
  private def lazyNames(env: Env)(tpe: Type): Set[Instance] = tpe.dealias match {
    case by: ByNameType => names(by.resultType, env)
    case applied @ World.Applied(_, _ :+ result) if World.functional(applied) => names(result, env)
    case _                                                                    => Set.empty
  }

  private def sized(types: List[Type], env: Env)(values: Values): Size =
    settled(each(types, env.copy(system = Some(values))), NothingSize)(Algebra.product(_).size)

  private def names(tpe: Type, env: Env): Set[Instance] =
    settled(of(tpe, env.copy(system = Some(Values()))), Set.empty[Instance])(_.refs)

}

private[compiled] object Stored {

  /** A definition's row: its size, if it has one, and the reasons it is not a number. */
  final case class Row(size: Option[Size], unbound: List[Unbound], undecided: Boolean = false)

  // What a type stores: a size, the reasons it is no number, whether the size does not say that a
  // value exists (an empty domain, an intersection, a refinement, a singleton), and inside a system
  // the classes it reads, and those of them it reads in a function's domain.
  final private[compiled] case class Held(
      size: Size,
      reasons: List[Unbound] = Nil,
      undecided: Boolean = false,
      refs: Set[Instance] = Set.empty,
      domains: Set[Instance] = Set.empty
  ) {
    def map(f: Size => Size): Held = copy(size = f(size))

    def zip(other: Held)(f: (Size, Size) => Size): Held =
      Held(
        f(size, other.size),
        reasons ++ other.reasons,
        undecided || other.undecided,
        refs ++ other.refs,
        domains ++ other.domains
      )

    // The same, read in a function's domain.
    def domain: Held = copy(domains = refs)

    // Its size, with the reasons and flags of `others` too.
    def joined(others: List[Held]): Held = others.foldLeft(this)(_.zip(_)((kept, _) => kept))

    def row: Row = Row(Some(size), reasons.distinct, undecided)
  }

  // What a type is read in: what the type parameters in scope store, where the type is written
  // (rule F), the classes being read (one reached again makes the reading a system), whether a
  // top-level value is being read, or a constructor position as an input, and inside a system the
  // values its classes hold so far.
  final private case class Env(
      binders: Map[Symbol, Held],
      site: Symbol,
      visiting: Set[ClassSymbol] = Set.empty,
      isValue: Boolean = false,
      input: Boolean = false,
      system: Option[Values] = None,
      written: Map[Symbol, (Type, Env)] = Map.empty
  )

  // A class of the module with its type parameters bound: a definition of a system.
  final private[compiled] case class Instance(cls: ClassSymbol, arguments: List[Held])

  // A system's values so far, and the classes the fixed point left blocked.
  final private case class Values(
      sizes: Map[Instance, Size] = Map.empty,
      blocked: Map[Instance, Blocked] = Map.empty
  )

  private given Store[Instance, Values] with {

    extension (values: Values) {
      def value(name: Instance): Size = values.sizes.getOrElse(name, NothingSize)

      def withValues(more: Iterable[(Instance, Size)]): Values =
        values.copy(sizes = values.sizes ++ more)

      def withBlocked(more: Iterable[(Instance, Blocked)]): Values =
        values.copy(blocked = values.blocked ++ more)

    }

  }

  // A reading that reaches a class it is inside of, which `solved` reads again as a system: no
  // other refusal may start with these words.
  private object Reaches {
    private val words = "a definition that reaches itself: "
    def apply(c: ClassSymbol): String = words + c.displayFullName
    def unapply(why: String): Boolean = why.startsWith(words)
  }

  // How many argument lists a system reads one class with: past them it would not end.
  private val Instantiations = 8

  // A system's classes in an order that does not depend on the run.
  private def ordered(keys: Iterable[Instance]): List[Instance] =
    keys.toList.sortBy(k => (k.cls.displayFullName, k.arguments.toString))

  // Whether an alias's body, under its parameters, is a match type.
  private def matches(body: Type): Boolean = body match {
    case lambda: TypeLambda => matches(lambda.resultType)
    case _: MatchType       => true
    case _                  => false
  }

  // A type parameter no instantiation bound: the size is a function of it.
  private def parameter(symbol: ClassTypeParamSymbol | LocalTypeParamSymbol): Held = {
    val name = symbol.name.toString
    val reason = symbol.declaredBounds.high match {
      case lambda: TypeLambda => Unbound.HigherKinded(name, lambda.paramNames.size)
      case _                  => Unbound.Parameter(name)
    }
    Held(EffectiveOmega, List(reason))
  }

  private val EmptyTuple = """scala\.Tuple\$package\$?\.EmptyTuple""".r

}
