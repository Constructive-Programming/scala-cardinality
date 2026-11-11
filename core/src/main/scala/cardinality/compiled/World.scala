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

  /** The class a type names, its aliases followed and its arguments left aside. */
  def classNamed(tpe: Type)(using Context): Option[ClassSymbol] = tpe.dealias match {
    case ref: TypeRef         => ref.optSymbol.collect { case c: ClassSymbol => c }
    case applied: AppliedType => classNamed(applied.tycon)
    case _                    => None
  }

}
