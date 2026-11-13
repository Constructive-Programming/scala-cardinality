package cardinality.compiled

import cardinality.types.ListedNamespaces
import tastyquery.Contexts.*
import tastyquery.Modifiers.Visibility
import tastyquery.Names.nme
import tastyquery.Symbols.*
import tastyquery.Traversers.TreeTraverser
import tastyquery.Trees.*
import tastyquery.Types.*

/** The classes of the module (the top-level ones it compiles, and what is inside them) and which
  * classes outside it are read by their declarations. What extends a class and what overrides a
  * member is the closed world's to say (`ClosedProgram`), and what a dependency's class without
  * TASTy is, `DependencyClasses`'.
  */
final private[compiled] class World(
    own: Set[ClassSymbol],
    closed: ClosedProgram,
    dependencies: DependencyClasses
)(using Context) {

  /** The module's top-level classes. */
  def module: Set[ClassSymbol] = own

  /** Whether a symbol is the module's: one of its top-level classes, or inside one. */
  def ofModule(symbol: Symbol): Boolean = World.owners(symbol).exists {
    case c: ClassSymbol => own(c)
    case _              => false
  }

  /** The fully qualified name of a top-level class of the standard library or the JDK
    * (`World.standard`) that the module does not compile itself: the only names the standard model
    * and the base types are asked.
    */
  def library(c: ClassSymbol): Option[String] =
    Option.when(c.owner.isPackage && !own(c) && World.standard(c))(c.displayFullName)

  /** Whether a template is never built (register D26): it, or a template of the module it extends,
    * evaluates something that always throws. The measured member's own initializer, `site`'s, is
    * the choice being counted, and is not read.
    */
  def unbuilt(c: ClassSymbol, site: Option[Symbol] = None): Boolean =
    templates(c).exists(Bodies.alwaysThrows(_, site.flatMap(_.tree)))

  /** The templates of the module a class's construction runs: its own and its ancestors'. */
  def templates(c: ClassSymbol): List[ClassDef] = c.linearization.filter(ofModule).flatMap(_.tree)

  /** Whether a class of the program extends `c`, or the classes that do are not all known. */
  def extended(c: ClassSymbol): Boolean = {
    val below = closed.subclasses(c)
    below.any || !below.complete
  }

  /** Whether no instance of `c` is ever built: there is at least one concrete class among `c` and
    * the classes of the program that extend it, and each is never built (D26), so a member `c`
    * declares never runs. Not where those classes are not all known, nor where there is none (a
    * trait nothing extends stays one a new body may implement). The measured member's own
    * initializer, `site`'s, is the choice being counted, and is not read.
    */
  def idle(c: ClassSymbol, site: Symbol): Boolean =
    idleness.getOrElseUpdate(
      (c, site),
      below(c).exists { ks =>
        val concrete = (c :: ks).filter(k => !k.isTrait && !k.isAbstractClass)
        concrete.nonEmpty && concrete.forall(unbuilt(_, Some(site)))
      }
    )

  private val idleness = scala.collection.mutable.Map.empty[(ClassSymbol, Symbol), Boolean]

  /** Whether a class of the program that extends `c` is never built, its own initializers read. */
  def unbuiltBelow(c: ClassSymbol): Boolean =
    unbuiltBelowOf.getOrElseUpdate(c, below(c).exists(_.exists(unbuilt(_))))

  private val unbuiltBelowOf = scala.collection.mutable.Map.empty[ClassSymbol, Boolean]

  /** The classes of the program that extend `c`, where they are all known and read. */
  def below(c: ClassSymbol): Option[List[ClassSymbol]] = {
    val below = closed.subclasses(c)
    Option.when(below.complete && below.unnamed.isEmpty)(below.found)
  }

  /** Whether a class is read by its declaration: one of the module, or of a dependency with TASTy,
    * whose declarations are the program's and whose bodies are not read. The standard library and
    * the JDK are read by the standard model or not at all.
    */
  def readByDeclaration(c: ClassSymbol): Boolean =
    ofModule(c) || (c.tree.nonEmpty && !World.standard(c))

  /** Whether a type of `symbol` is read by its shape: the module's, or a dependency's outside the
    * standard library and the JDK (a class by its declaration or its symbols, an opaque type as one
    * value).
    */
  def shaped(symbol: Symbol): Boolean = ofModule(symbol) || !World.standard(symbol)

  /** What a dependency's class without TASTy is (`DependencyClasses`). */
  def withoutTasty(c: ClassSymbol): Either[String, DependencyClasses.Declared] =
    dependencies.of(c)

  /** What may override `member` in the instances of `c`'s subclasses (`ClosedProgram`). */
  def overriders(c: ClassSymbol, member: TermSymbol): List[Option[TermSymbol]] =
    closed.overriders(c, member)

  /** Whether no instance of `c` is of a subclass (the closed world's final). */
  def finalClass(c: ClassSymbol): Boolean = closed.finalClass(c)

  /** Whether an instance may be of both classes (`ClosedProgram.mixes`). */
  def mixes(left: ClassSymbol, right: ClassSymbol): Boolean = closed.mixes(left, right)

  /** How a class extending `owner` may override `member`, one `owner` declares or inherits. */
  def overriding(owner: ClassSymbol, member: TermOrTypeSymbol): ClosedProgram.Overriding =
    closed.overriding(owner, member)

}

private[compiled] object World {

  /** Whether `this` is of the class's own type: its self type, if any, is a supertype of the class
    * (an untyped alias `self =>` records the class's own). A self type that cannot be compared is
    * not the class's own.
    */
  def ownSelf(c: ClassSymbol)(using Context): Boolean =
    c.givenSelfType.forall { self =>
      scala.util.Try(c.appliedRefInsideThis.isSubType(self)).getOrElse(false)
    }

  /** Whether a class is one whose members every value has (`Any`, `Matchable`, `AnyVal` and the
    * compiler's parents of a case class), which call nothing back.
    */
  def universal(owner: Symbol): Boolean = Universal(owner.displayFullName)

  /** Whether a symbol is declared in the standard library or the JDK's `java` packages, which the
    * standard model reads: the `scala` package and the subpackages the standard library declares (a
    * library's own namespace beside them, such as scalameta's, is a dependency like any other, as
    * the stubs have it). Another JDK namespace (`javax`) is read like a Java dependency.
    */
  def standard(symbol: Symbol): Boolean =
    owners(symbol).collectFirst {
      case p: PackageSymbol => p.displayFullName.split('.').toList
    } match {
      case Some("java" :: _)                => true
      case Some("scala" :: Nil)             => true
      case Some("scala" :: subpackage :: _) => ListedNamespaces.scalaPackages.contains(subpackage)
      case _                                => false
    }

  /** A symbol and everything around it, innermost first. */
  def owners(symbol: Symbol): List[Symbol] =
    if (symbol == null) Nil else symbol :: owners(symbol.owner)

  /** Whether a class is nested only in objects and packages: one nested in a class or a body is a
    * class per enclosing instance or evaluation.
    */
  def inObjects(symbol: Symbol): Boolean = owners(symbol.owner).forall {
    case c: ClassSymbol => c.isModuleClass
    case other          => other.isPackage
  }

  /** The parents that declare no member a count reads (v1.md §2), the compiler's own for a case
    * class among them.
    */
  val Memberless: Set[String] =
    Set("java.lang.Object", "scala.Product", "scala.Equals", "java.io.Serializable")

  private val Universal = Memberless ++ Set("scala.Any", "scala.Matchable", "scala.AnyVal")

  /** The classes a class inherits members from: `Any`, `Object`, `AnyVal` and the compiler's own
    * parents of a case class declare none a count reads.
    */
  def ancestors(c: ClassSymbol)(using Context): List[ClassSymbol] = {
    c.linearization.drop(1).filterNot(universal)
  }

  /** Whether a symbol is declared under the package `root`, or under a package inside it. */
  def under(symbol: Symbol, root: String): Boolean = owners(symbol).exists { around =>
    around.isPackage &&
    (around.displayFullName == root || around.displayFullName.startsWith(root + "."))
  }

  /** Whether the site may call a definition, as rule B reads a constructor's or a factory's
    * modifiers: an unrestricted one; a `private[p]` one inside `p`; a `private` or `protected` one
    * inside its class, and a `private` one in its companion.
    */
  def callable(symbol: TermOrTypeSymbol, site: Symbol)(using Context): Boolean = {
    val around = owners(site).toSet
    def companion = symbol.owner match {
      case k: ClassSymbol => k.companionClass.exists(around)
      case _              => false
    }
    symbol.visibility match {
      case Visibility.Public                 => true
      case Visibility.Private                => around(symbol.owner) || companion
      case Visibility.ScopedPrivate(scope)   => around(scope)
      case Visibility.ScopedProtected(scope) => around(scope) || around(symbol.owner)
      case _                                 => around(symbol.owner)
    }
  }

  /** Whether the site may select a member on a value it is given: callable there, and not selected
    * on `this` alone: a `protected[this]` member, or a term private to the instance. The compiler
    * marks a `private` member it finds read on `this` alone private to the instance too, which a
    * new body may still select on another value, and TASTy does not tell it from an explicit
    * `private[this]` one: neither is taken as selectable, so a value observed through one is not
    * read (a capability record refuses it) rather than counted with a field it may not see.
    */
  def selectable(symbol: TermOrTypeSymbol, site: Symbol)(using Context): Boolean =
    callable(symbol, site) && (symbol.visibility match {
      case Visibility.ProtectedThis => false
      case Visibility.PrivateThis   => !symbol.isTerm
      case _                        => true
    })

  /** Whether a member restates one every value has (`Any`'s `toString`, `hashCode`,
    * `equals(that: Any)`, and those of the compiler's parents of a case class: `canEqual`,
    * `productArity`, …), which the model excludes. An overload of such a name counts.
    */
  def restates(member: TermSymbol)(using Context): Boolean =
    member.allOverriddenSymbols.exists(overridden => universal(overridden.owner))

  /** A method's or a `val`'s body, where the tree holds one. */
  def rhs(definition: ValOrDefDef): Option[TermTree] = definition match {
    case d: DefDef => d.rhs
    case v: ValDef => v.rhs
  }

  /** A method's or a `val`'s declared result. */
  def declared(definition: ValOrDefDef): Type = definition match {
    case d: DefDef => d.resultTpt.toType
    case v: ValDef => v.tpt.toType
  }

  /** The term a reference names; none where it does not resolve. */
  def named(tree: Tree)(using Context): Option[TermSymbol] = tree match {
    case reference: TermReferenceTree =>
      scala.util.Try(reference.symbol).toOption.collect { case t: TermSymbol => t }
    case _ => None
  }

  /** Whether a type is a function's (`FunctionN`, `ContextFunctionN`). */
  def functional(tpe: TypeOrMethodic)(using Context): Boolean = tpe match {
    case t: Type =>
      classNamed(t).exists(_.displayFullName.matches("scala\\.(Context)?Function\\d+"))
    case _ => false
  }

  // A class applied to type arguments, by its full name.
  object Applied {

    def unapply(tpe: Type)(using Context): Option[(String, List[Type])] = tpe match {
      case Named(ref, arguments) =>
        for {
          c <- ref.optSymbol.collect { case c: ClassSymbol => c }
          types <- proper(arguments).toOption
        } yield c.displayFullName -> types
      case _ => None
    }

  }

  /** A concrete class, no enum or family, none of whose constructor's fields holds a by-name or
    * function type at any depth.
    */
  def strict(c: ClassSymbol)(using Context): Boolean =
    !c.isEnum && !Shapes.isFamily(c) &&
      c.tree.exists(t => !Shapes.parameters(t.rhs.constr).exists(p => deferred(p.tpt.toType)))

  /** Whether a type is by-name or holds a function type at any depth (a context function's too); an
    * opaque alias, whose representation may hold one, is taken to.
    */
  def deferred(tpe: Type)(using Context): Boolean = tpe.dealias match {
    case _: ByNameType                                 => true
    case Named(ref, _) if ref.optSymbol.exists(opaque) => true
    case applied @ Applied(_, parts) => functional(applied) || parts.exists(deferred)
    case _                           => false
  }

  /** Whether a symbol is an opaque type alias. */
  def opaque(symbol: Symbol): Boolean = symbol match {
    case alias: TypeMemberSymbol => alias.isOpaqueTypeAlias
    case _                       => false
  }

  /** Whether a method is a constructor, primary or secondary. */
  def isConstructor(symbol: Symbol): Boolean = symbol.name == nme.Constructor

  /** Every class a tree declares, itself included, at any depth. */
  def classes(tree: Tree): List[ClassDef] = {
    val found = List.newBuilder[ClassDef]
    new TreeTraverser {
      override def traverse(tree: Tree): Unit = {
        found ++= Option(tree).collect { case c: ClassDef => c }
        super.traverse(tree)
      }
    }.traverse(tree)
    found.result()
  }

  /** A class's tree; a class the module compiled from Java has none. */
  def tree(c: ClassSymbol): Either[String, ClassDef] =
    c.tree.toRight(s"a class without TASTy: ${c.displayFullName}")

  /** Type arguments that are types: a wildcard is not read. */
  def proper(arguments: List[TypeOrWildcard]): Either[String, List[Type]] =
    arguments.foldRight[Either[String, List[Type]]](Right(Nil)) {
      case (tpe: Type, rest) => rest.map(tpe :: _)
      case _                 => Left("a wildcard type argument")
    }

  /** The first symbol a type as it is written names that passes `test`: through its prefix (a
    * path's values included, and the class of a `this` or `super`), its arguments and a wildcard's
    * bounds, an annotation, a refinement of a type or a value and its parent (a recursive one's
    * too), a union or an intersection, a type lambda's body and bounds, a match type's parts; an
    * alias is not followed.
    */
  def mentioned(tpe: TypeOrWildcard, test: Symbol => Boolean)(using Context): Option[Symbol] = {
    def first(types: TypeOrWildcard*) = types.iterator.flatMap(mentioned(_, test)).nextOption()
    tpe match {
      case ref: TypeRef =>
        ref.optSymbol
          .filter(test)
          .orElse(ref.prefix match {
            case prefix: Type => mentioned(prefix, test)
            case _            => None
          })
      case path: TermRef =>
        path.optSymbol
          .filter(test)
          .orElse(path.prefix match {
            case prefix: Type => mentioned(prefix, test)
            case _            => None
          })
      case self: ThisType            => Some(self.cls).filter(test)
      case sup: SuperType            => first(sup.thistpe)
      case applied: AppliedType      => first(applied.tycon :: applied.args*)
      case wildcard: WildcardTypeArg => first(wildcard.bounds.low, wildcard.bounds.high)
      case annotated: AnnotatedType  => first(annotated.typ)
      // A refined method's signature is not walked: its parent and a value's type are.
      case refined: TermRefinement =>
        first(refined.parent :: List(refined.refinedType).collect { case value: Type => value }*)
      case refined: TypeRefinement =>
        first(refined.parent, refined.refinedBounds.low, refined.refinedBounds.high)
      case rec: RecType           => first(rec.parent)
      case by: ByNameType         => first(by.resultType)
      case repeated: RepeatedType => first(repeated.elemType)
      case and: AndType           => first(and.first, and.second)
      case or: OrType             => first(or.first, or.second)
      case lambda: TypeLambda     =>
        first(lambda.resultType :: lambda.paramTypeBounds.flatMap(b => List(b.low, b.high))*)
      case matched: MatchType =>
        first(
          matched.bound :: matched.scrutinee :: matched.cases.flatMap(c =>
            c.pattern :: c.result :: c.paramTypeBounds.flatMap(b => List(b.low, b.high))
          )*
        )
      case _ => None
    }
  }

  /** Whether a type as it is written names one of `symbols` (`mentioned`). */
  def mentions(tpe: TypeOrWildcard, symbols: Set[Symbol])(using Context): Boolean =
    mentioned(tpe, symbols).nonEmpty

  /** A named type as it is applied: `C` with no argument, or `C[args]`. */
  object Named {

    def unapply(tpe: Type): Option[(TypeRef, List[TypeOrWildcard])] = tpe match {
      case ref: TypeRef         => Some(ref -> Nil)
      case applied: AppliedType =>
        applied.tycon match {
          case ref: TypeRef => Some(ref -> applied.args)
          case _            => None
        }
      case _ => None
    }

  }

  /** The class a type names, its aliases followed and its arguments left aside. */
  def classNamed(tpe: Type)(using Context): Option[ClassSymbol] = tpe.dealias match {
    case ref: TypeRef         => ref.optSymbol.collect { case c: ClassSymbol => c }
    case applied: AppliedType => classNamed(applied.tycon)
    case refined: RefinedType => classNamed(refined.parent)
    case _                    => None
  }

}
