package cardinality.compiled

import tastyquery.Contexts.*
import tastyquery.Names.nme
import tastyquery.Symbols.*
import tastyquery.Traversers.TreeTraverser
import tastyquery.Trees.*
import tastyquery.Types.*

/** The classes of the module: the top-level ones it compiles, and what is inside them. No class
  * outside the module extends one of them (the closed world).
  */
final private[compiled] class World(own: Set[ClassSymbol])(using Context) {

  /** Whether a symbol is the module's: one of its top-level classes, or inside one. */
  def ofModule(symbol: Symbol): Boolean = World.owners(symbol).exists {
    case c: ClassSymbol => own(c)
    case _              => false
  }

  /** The fully qualified name of a top-level class under `scala` or `java` that the module does not
    * compile itself: the only names the standard model and the base types are asked.
    */
  def library(c: ClassSymbol): Option[String] =
    Option
      .when(c.owner.isPackage && !own(c))(c.displayFullName)
      .filter(name => name.startsWith("scala.") || name.startsWith("java."))

  /** Whether building an instance of a template is not read yet (register D26 for classes, slice
    * S6): it, or a template of the module it extends, evaluates something that always throws or
    * holds a pattern definition.
    */
  def unbuilt(tree: ClassDef): Boolean =
    tree.symbol.linearization
      .filter(ofModule)
      .flatMap(_.tree)
      .exists(template => Bodies.destructures(template) || Bodies.alwaysThrows(template))

  /** The classes some class of the module extends directly; its nested, local and anonymous classes
    * are found in the trees.
    */
  lazy val extended: Set[ClassSymbol] =
    own.toList.flatMap(_.tree).flatMap(World.classes).flatMap(_.symbol.parentClasses).toSet

}

private[compiled] object World {

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
    * path's values included), its arguments and a wildcard's bounds, an annotation, a refinement of
    * a type or a value and its parent, a union or an intersection; an alias is not followed.
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
      case applied: AppliedType      => first(applied.tycon :: applied.args*)
      case wildcard: WildcardTypeArg => first(wildcard.bounds.low, wildcard.bounds.high)
      case annotated: AnnotatedType  => first(annotated.typ)
      // A refined method's signature is not walked: its parent and a value's type are.
      case refined: TermRefinement =>
        first(refined.parent :: List(refined.refinedType).collect { case value: Type => value }*)
      case refined: TypeRefinement =>
        first(refined.parent, refined.refinedBounds.low, refined.refinedBounds.high)
      case by: ByNameType         => first(by.resultType)
      case repeated: RepeatedType => first(repeated.elemType)
      case and: AndType           => first(and.first, and.second)
      case or: OrType             => first(or.first, or.second)
      case _                      => None
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
    case _                    => None
  }

}
