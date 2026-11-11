package cardinality.compiled

import cardinality.capacity.Definition.{Kind, Unbound}
import cardinality.capacity.{
  BaseTypes,
  BooleanSize,
  EffectiveEpsilon0,
  EffectiveOmega,
  NothingSize,
  Size,
  UnitSize
}
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Trees.{
  ClassDef,
  OpaqueTypeAliasDefinitionTree,
  PolyTypeDefinitionTree,
  Tree,
  TypeAliasDefinitionTree,
  TypeDefinitionTree,
  TypeMember,
  ValDef
}
import tastyquery.Types.*

/** What a definition stores, the size of its row (slice S2; v1.md §2, docs/type-arithmetic.md): a
  * size and, where it is no number, the reasons (a type parameter, an open class); neither for an
  * abstraction, which has no value of its own.
  *
  *   - An object is one value; a concrete class is the product of what its constructor's parameters
  *     store, whatever it extends; an enum is the sum of its cases; an alias is what its right-hand
  *     side stores; a top-level value is one value where its declared type is known to have one.
  *   - A base type is its table's size; tuples multiply, unions add, an intersection is its smaller
  *     side, a function is codomain ^ domain; the standard collections follow `standard`; a literal
  *     or singleton type is one value, a refinement the type it refines.
  *   - A type is read where it is written: a class's fields at the class, the body of an alias of
  *     an object or a package at the alias, an alias's arguments where they are. So an opaque type
  *     is its representation inside its scope and one value outside it (rule F), wherever the
  *     reading started, as the old estimate reads it. (The method side follows aliases first and
  *     decides at the use site: `Shapes`.)
  *   - A reference to a concrete class or an enum of the module is what that definition stores with
  *     its arguments bound; one to an open class, an unbound type parameter, an abstract type
  *     member, a class named without its arguments, an enum case's class or `AnyVal` is a reason.
  *
  * What it does not read yet is a `Left`, naming what it met. The main ones: a sealed family, a
  * definition that reaches itself (the solver's fixed point), a restricted constructor (rule B), an
  * enum case whose clause does not pass its own parameters on, a class the algebra does not model
  * or of a dependency, an opaque type of a dependency or of a class named inside it, an alias of a
  * class beside an opaque type's scope, a wildcard argument, a refinement of no written type, a
  * function type refined by its `apply`, any other form of type, and what a value of a class of the
  * module needs (its construction).
  */
final private[compiled] class Stored(world: World)(using Context) {
  import Stored.{Env, Held, Row}

  /** The row of a definition. */
  def row(kind: Kind, tree: Tree): Either[String, Row] = (kind, tree) match {
    case (Kind.Object, _) => Right(Row(Some(UnitSize), Nil))
    // A constructor alias names a function on types, and an open trait or abstract class has no
    // value of its own: a dash, no size and no reason.
    case (Kind.Constructor, _)                         => Right(Row(None, Nil))
    case (_, c: ClassDef) if c.symbol.isEnum           => enumeration(c, rowEnv(c)).map(_.row)
    case (_, c: ClassDef) if Shapes.isFamily(c.symbol) => sum(c, rowEnv(c)).map(_.row)
    case (_, c: ClassDef) if c.symbol.isAbstractClass  => Right(Row(None, Nil))
    case (_, c: ClassDef)                              => constructed(c, rowEnv(c)).map(_.row)
    case (_, t: TypeMember)                            => alias(t)
    case (_, v: ValDef)                                => topLevel(v)
    case _                                             => Left("a definition")
  }

  // A definition read for its own row: each of its type parameters is a reason the row has no one
  // size, and a reference to itself is not solved yet.
  private def rowEnv(tree: ClassDef): Env = {
    val c = tree.symbol
    Env(c.typeParams.map(p => (p: Symbol) -> Stored.parameter(p)).toMap, c, Set(c))
  }

  // What a class's constructor stores: the product of its parameters, the evidence of a context
  // bound among them, each read at the class. The constructor names its own copies of the class's
  // type parameters.
  private def constructed(tree: ClassDef, env: Env): Either[String, Held] = {
    val constructor = tree.rhs.constr
    if (!constructor.symbol.isPublic)
      Left(s"a restricted constructor: ${tree.symbol.displayFullName}")
    else
      each(Shapes.parameters(constructor).map(_.tpt.toType), atClass(tree, env))
        .map(Stored.product)
  }

  // Where a class's constructor is written: at the class, with the constructor's own copies of the
  // class's type parameters bound as the class's are.
  private def atClass(tree: ClassDef, env: Env): Env = {
    val copies = Shapes
      .typeParameters(tree.rhs.constr)
      .zip(tree.symbol.typeParams)
      .map((copy, parameter) => copy -> env.binders(parameter))
    env.copy(binders = env.binders ++ copies, site = tree.symbol)
  }

  /** What one position of a class's constructor stores, as the method side's value space of it is
    * compared with (slice S3): the position's written type is read as an input, so a top type in it
    * is one value, and with the method side's saturation, so a collection is not lifted to its
    * element's tier. An alias is transparent to that; a class of the module the type names is read
    * as it stores.
    */
  def position(tree: ClassDef, tpe: Type): Either[String, Row] =
    of(tpe, atClass(tree, rowEnv(tree)).copy(input = true)).map(_.row)

  // An enum is the sum of its cases: a value case is one value, a class case what it stores, each
  // where its clause passes the enum's parameters on (`Sums.carried`: `case Y extends E[Int]` is a
  // member of one instantiation only). The enum's own constructor arguments are shared state, not
  // extra values.
  private def enumeration(tree: ClassDef, env: Env): Either[String, Held] = {
    val enumClass = tree.symbol
    def carried(child: ClassSymbol | TermSymbol) =
      Sums
        .carried(child, enumClass, enumClass.typeParams.map(env.binders))
        .toRight(s"an enum case that does not pass its own parameters on: ${child.displayFullName}")
    enumClass.sealedChildren match {
      // An enum has a case: none means the classpath lacks the annotation that lists them.
      case Nil   => Left(s"an enum whose cases are not listed: ${enumClass.displayFullName}")
      case cases =>
        Traversals
          .sequence(cases.map {
            case single: TermSymbol => carried(single).map(_ => Held(UnitSize))
            case cls: ClassSymbol   =>
              for {
                below <- World.tree(cls)
                bound <- carried(cls)
                held <- constructed(below, env.copy(binders = cls.typeParams.zip(bound).toMap))
              } yield held
          })
          .map(Stored.sum)
    }
  }

  // A sealed family is the sum of its subtypes: an object is one value, a concrete class what it
  // stores, a sealed level below its own sum, each where its clause passes the family's parameters
  // on (`Sums.carried`). A subtype the file does not name whole leaves the sum unread: an
  // anonymous, given, inner or local one, an open one, one something extends, one under two
  // levels; so does a construction that throws (slice S6). The old estimate refuses a family an
  // enum's case extends, and so does this.
  private def family(tree: ClassDef, env: Env): Either[String, Held] = {
    val parent = tree.symbol
    val name = parent.displayFullName
    val children = parent.sealedChildren
    def carried(child: ClassSymbol | TermSymbol) =
      Sums
        .carried(child, parent, parent.typeParams.map(env.binders))
        .toRight(s"a subtype that does not pass its own parameters on: ${child.displayFullName}")
    def built(template: ClassDef) =
      Either.cond(
        !world.unbuilt(template),
        (),
        s"a construction that throws or binds a pattern: ${template.symbol.displayFullName}"
      )
    def subtype(child: ClassSymbol | TermSymbol): Either[String, Held] = child match {
      case value: TermSymbol if value.isGivenOrUsing =>
        Left(s"a family with a given instance: $name")
      case value: TermSymbol if value.isEnumCase =>
        val why =
          s"sealed family not summed: ${parent.name} (an enum case extends it: ${value.name})"
        Right(Held(EffectiveOmega, List(Unbound.Refusal(why))))
      case _ if !World.inObjects(child) => Left(s"a family with an inner or local subtype: $name")
      case value: TermSymbol            =>
        for {
          _ <- value.moduleClass.fold(Right(()))(World.tree(_).flatMap(built))
          _ <- carried(value)
        } yield Held(UnitSize)
      case cls: ClassSymbol =>
        for {
          below <- World.tree(cls)
          _ <- built(below)
          bound <- carried(cls)
          inside = env.copy(
            binders = cls.typeParams.zip(bound).toMap,
            visiting = env.visiting + cls
          )
          held <-
            if (env.visiting(cls)) Left(s"a definition that reaches itself: ${cls.displayFullName}")
            else if (cls.isEnum) Left(s"an enum under a sealed family: ${cls.displayFullName}")
            else if (Shapes.isFamily(cls)) family(below, inside)
            else if (cls.isAbstractClass || world.extended(cls))
              Left(s"a family with a subtype open to extension: ${cls.displayFullName}")
            else constructed(below, inside)
        } yield held
    }
    // An anonymous instance lists the family itself; none listed for a family something extends
    // means the classpath lacks the annotation that lists them.
    if (children.contains(parent)) Left(s"a family with an anonymous instance: $name")
    else if (children.isEmpty && world.extended(parent))
      Left(s"a family whose subtypes are not listed: $name")
    else built(tree).flatMap(_ => Traversals.sequence(children.map(subtype))).map(Stored.sum)
  }

  // The cases a family counts, through its sealed levels: one under two levels would be counted
  // twice.
  private def leaves(parent: ClassSymbol): List[Symbol] = parent.sealedChildren.flatMap {
    case cls: ClassSymbol if cls != parent && Shapes.isFamily(cls) => leaves(cls)
    case leaf                                                      => List(leaf)
  }

  // A family read from its top.
  private def sum(tree: ClassDef, env: Env): Either[String, Held] = {
    val reached = leaves(tree.symbol)
    reached.diff(reached.distinct).headOption match {
      case Some(twice) => Left(s"a subtype reached twice: ${twice.displayFullName}")
      case None        => family(tree, env)
    }
  }

  // An alias row is what its right-hand side stores, read where the alias is written: an opaque
  // one shows its representation. The parameters of `type F[X] = …` are the row's reasons.
  private def alias(tree: TypeMember): Either[String, Row] =
    body(tree.rhs).flatMap((_, aliased) => of(aliased, Env(Map.empty, tree.symbol))).map(_.row)

  // The type an alias's definition names, and the parameters it is written under.
  private def body(definition: TypeDefinitionTree): Either[String, (List[Symbol], Type)] =
    definition match {
      case PolyTypeDefinitionTree(binders, inner) =>
        body(inner).map((more, aliased) => (binders.map(_.symbol) ++ more, aliased))
      case TypeAliasDefinitionTree(alias)          => Right(Nil -> alias.toType)
      case OpaqueTypeAliasDefinitionTree(_, alias) => Right(Nil -> alias.toType)
      case _                                       => Left("an abstract type member")
    }

  // A top-level value holds one value where its declared type has one (register D30): the size is
  // 1, and the row is undecided where the type has reasons, is empty, or has a size that does not
  // say a value exists. The initializer is never read.
  private def topLevel(tree: ValDef): Either[String, Row] =
    of(tree.tpt.toType, Env(Map.empty, tree.symbol, isValue = true)).map { held =>
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
    case repeated: RepeatedType   => of(repeated.elemType, env).map(_.map(Stored.linear(env.input)))
    case _: NothingType           => Right(Held(NothingSize))
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
    case other                       => Left(s"a type: ${other.showBasic}")
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
        World.proper(arguments).flatMap(each(_, env)).flatMap { sizes =>
          world.library(c) match {
            case Some(name)                => standard(name, sizes, env.input)
            case None if world.ofModule(c) => declared(c, sizes, env)
            case None                      => Left(s"a class of a dependency: ${c.displayFullName}")
          }
        }
      // The empty tuple is a base type, though the library spells it as an alias of a singleton.
      case Some(member: TypeMemberSymbol) if Stored.EmptyTuple.matches(member.displayFullName) =>
        BaseTypes.get("EmptyTuple").map(Held(_)).toRight(s"a type: ${ref.showBasic}")
      case Some(member: TypeMemberSymbol) =>
        (member.typeDef, ref.optAliasedType, ref.bounds) match {
          case (_: TypeMemberDefinition.OpaqueTypeAlias, _, _) =>
            opaque(member, env)(written(member, arguments, env))
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
          case (_, _, bounds) if bounds.low.isSameType(bounds.high) => applied(bounds.high)
          // An abstract member nothing fixed is what each instance chooses.
          case _ => Right(Held(EffectiveOmega, List(Unbound.Member(member.name.toString))))
        }
      case _ => Left(s"a type: ${ref.showBasic}")
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
    if (!world.ofModule(alias)) Left(s"an opaque type of a dependency: ${alias.displayFullName}")
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
    if (env.visiting(c)) Left(s"a definition that reaches itself: ${c.displayFullName}")
    // Whether a value of the class exists is its construction's to say (slice S6).
    else if (env.isValue) Left(s"a value of a class of the module: ${c.displayFullName}")
    else if (c.isEnum && c.isCaseClass)
      Right(Held(EffectiveOmega, List(Unbound.Refusal(s"enum case type not read: $name"))))
    // Any subtype anywhere may add values to an open trait or abstract class.
    else if (c.isAbstractClass && !c.isEnum && !Shapes.isFamily(c))
      Right(Held(EffectiveOmega, List(Unbound.Open(name))))
    else
      World.tree(c).flatMap { tree =>
        // A declaration is read as it stores, whatever position named it.
        val inside = env.copy(
          binders = c.typeParams.zip(arguments).toMap,
          visiting = env.visiting + c,
          input = false
        )
        if (c.isEnum) enumeration(tree, inside)
        else if (Shapes.isFamily(c)) sum(tree, inside)
        else constructed(tree, inside)
      }
  }

  // The standard library's types the algebra models, by the class the compiler bound.
  private def standard(name: String, sizes: List[Held], input: Boolean): Either[String, Held] =
    (name, sizes) match {
      // As the method side reads an input: a top type is one value, and `Null` is empty, since
      // the method side counts no `null` (v1.md §2).
      case (_, Nil) if input && Stored.Tops(name) => Right(Held(UnitSize))
      case ("scala.Null", Nil) if input           => Right(Held(NothingSize))
      case (Stored.Based(size), Nil)              => Right(Held(size))
      // A throwable of the root imports is an open class: any subclass's values are its own.
      case (Stored.Throwables(simple), Nil) =>
        Right(Held(EffectiveOmega, List(Unbound.Open(simple))))
      case ("scala.AnyVal", Nil) => Right(Held(EffectiveOmega, List(Unbound.Unknown("AnyVal"))))
      case _ if name.matches("""scala\.(Tuple\d+|\*:)""") => Right(Stored.product(sizes))
      case (_, parameters :+ result) if name.matches("""scala\.(Context)?Function\d+""") =>
        Right(Stored.arrow(result, Stored.product(parameters)))
      case ("scala.Option", List(element))                   => Right(element.map(UnitSize + _))
      case ("scala.util.Either", List(left, right))          => Right(left.zip(right)(_ + _))
      case ("scala.collection.immutable.Set", List(element)) =>
        Right(element.map(e => Stored.lifted(input)(e, BooleanSize.pow(e))))
      case ("scala.collection.immutable.Map", List(key, value)) =>
        Right(key.zip(value)((k, v) => Stored.lifted(input)(k, (v + UnitSize).pow(k))))
      case ("scala.PartialFunction", List(key, value)) =>
        Right(key.zip(value)((k, v) => (v + UnitSize).pow(k)))
      case (_, List(element)) if Stored.Sequences(name) =>
        Right(element.map(Stored.linear(input)))
      // The finite lists and the infinite streams together, collapsed to the highest tier.
      case (_, List(element)) if Stored.Lazy(name) =>
        Right(element.map { e =>
          if (e == NothingSize) UnitSize else (EffectiveOmega + e.pow(EffectiveOmega)).widen
        })
      case _ => Left(s"a class the algebra does not model: $name")
    }

}

private[compiled] object Stored {

  /** A definition's row: its size, if it has one, and the reasons it is not a number. */
  final case class Row(size: Option[Size], unbound: List[Unbound])

  // What a type stores: a size, the reasons it is no number, and whether the size does not say
  // that a value exists (an empty domain, an intersection, a refinement, a singleton).
  final private case class Held(
      size: Size,
      reasons: List[Unbound] = Nil,
      undecided: Boolean = false
  ) {
    def map(f: Size => Size): Held = copy(size = f(size))

    def zip(other: Held)(f: (Size, Size) => Size): Held =
      Held(f(size, other.size), reasons ++ other.reasons, undecided || other.undecided)

    def row: Row = Row(Some(size), reasons.distinct)
  }

  // What a type is read in: what the type parameters in scope store, where the type is written
  // (rule F), the classes being read (a cycle is not solved yet), and whether a top-level value
  // is being read, or a constructor position as an input.
  final private case class Env(
      binders: Map[Symbol, Held],
      site: Symbol,
      visiting: Set[ClassSymbol] = Set.empty,
      isValue: Boolean = false,
      input: Boolean = false
  )

  // A type parameter no instantiation bound: the size is a function of it.
  private def parameter(symbol: ClassTypeParamSymbol | LocalTypeParamSymbol): Held = {
    val name = symbol.name.toString
    val reason = symbol.declaredBounds.high match {
      case lambda: TypeLambda => Unbound.HigherKinded(name, lambda.paramNames.size)
      case _                  => Unbound.Parameter(name)
    }
    Held(EffectiveOmega, List(reason))
  }

  private def product(sizes: List[Held]): Held = sizes.foldLeft(Held(UnitSize))(_.zip(_)(_ * _))
  private def sum(sizes: List[Held]): Held = sizes.foldLeft(Held(NothingSize))(_.zip(_)(_ + _))

  // A function type is codomain ^ domain; from an empty domain the algebra says 0, though the
  // absurd function is a value.
  private def arrow(codomain: Held, domain: Held): Held =
    if (domain.size == NothingSize)
      domain.zip(codomain)((_, _) => NothingSize).copy(undecided = true)
    else codomain.zip(domain)(_.pow(_))

  // A linear collection over an empty element type holds the empty one; over any other it is at
  // least ω and, by its one-element term, its element's tier.
  private def linear(saturating: Boolean)(element: Size): Size =
    if (element == NothingSize) UnitSize else lifted(saturating)(element, EffectiveOmega)

  // A finite collection holds each one-element collection, so it is never below its element's
  // tier; the method side saturates at ω, and so does a reading compared with it.
  private def lifted(saturating: Boolean)(element: Size, collection: Size): Size =
    if (!saturating && collection.larger(UnitSize) && element.epsilon > 0)
      Size.order.max(collection, EffectiveEpsilon0)
    else collection

  // The base types of `BaseTypes` that are classes, by fully qualified name.
  private val Based: Map[String, Size] =
    (List("Unit", "Boolean", "Byte", "Short", "Char", "Int", "Long", "Float", "Double", "Any")
      .++(List("Matchable", "Null"))
      .map(name => s"scala.$name" -> name) ++ List(
      "java.lang.String" -> "String",
      "java.lang.Object" -> "Object",
      "scala.math.BigInt" -> "BigInt",
      "scala.math.BigDecimal" -> "BigDecimal"
    )).flatMap((qualified, name) => BaseTypes.get(name).map(qualified -> _)).toMap

  private val Tops = Set("scala.Any", "scala.AnyVal", "java.lang.Object", "scala.Matchable")

  private val EmptyTuple = """scala\.Tuple\$package\$?\.EmptyTuple""".r

  // The throwables the root imports bind, by fully qualified name: the name a row gives each.
  private val Throwables: Map[String, String] =
    BaseTypes.throwables.map((simple, throwable) => throwable.fqn -> simple)

  private val Sequences: Set[String] =
    Set("List", "Vector", "Seq", "IndexedSeq").map(
      "scala.collection.immutable." + _
    ) + "scala.Array"

  private val Lazy: Set[String] = Set("LazyList", "Stream").map("scala.collection.immutable." + _)

}
