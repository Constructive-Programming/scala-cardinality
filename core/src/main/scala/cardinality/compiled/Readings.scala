package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.inhabitation.Inhabitation.{Binding, Count, Shape}
import cardinality.analysis.resolution.TopTypes
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

/** What the compiled front end measures so far, read from symbols and types
  * (docs/plans/compiled-own-types.md, "Front end: design and status", lists the slices): a `Left`
  * names what a row needs that no slice reads yet, and is never a count. A type the counting model
  * refuses is the row's `?` with the model's reason (`Unread.Refused`).
  *
  * Method rows: a method or value whose owners are packages and objects without parents, so its
  * environment is its own parameters (a concrete member around it computes from what the
  * environment already holds, and an object has no abstract one), over the types `Shapes` reads and
  * the method's own unbounded type parameters.
  *
  * Definition rows are measured by `Stored`.
  */
final private[compiled] class Readings(world: World)(using Context) {
  import Unread.Uncovered

  private val shapes = new Shapes(world)

  // ---- method rows --------------------------------------------------------------------------

  /** The count of a method row. */
  def method(tree: ValOrDefDef): Either[String, Count] =
    for {
      _ <- later(tree.symbol)
      _ <- static(tree.symbol.owner)
      _ <- plain(tree.symbol.owner)
      count <-
        // No call of a member of an object that is never built returns: one implementation.
        if (!built(tree.symbol.owner, tree)) Right(Count.Finite(1))
        else
          counted(tree).left.flatMap {
            case Unread.Refused(reason) => Right(Count.Unresolved(List(reason)))
            case Uncovered(what)        => Left(what)
          }
    } yield count

  // A goal with one value has one implementation whatever the environment holds (v1.md §2), so
  // the result is read first; else the count is the solver's over the parameters.
  private def counted(tree: ValOrDefDef): Either[Unread, Count] = {
    val (binders, parameters, declared) = tree match {
      case v: ValDef => (Nil, Nil, v.tpt)
      case d: DefDef =>
        (d.paramLists.flatMap(_.toOption.toList.flatten), Shapes.parameters(d), d.resultTpt)
    }
    val self = tree.symbol.displayFullName
    // The measured method's type parameters, each an atom of its own, and the method as the site
    // rule F reads an opaque type from.
    val env = Shapes.Env(
      binders.map(binder => (binder.symbol: Symbol) -> Shape.Atom(s"$self[${binder.name}]")).toMap,
      tree.symbol
    )
    for {
      _ <- Traversals.sequence(binders.map(unbounded))
      result <- shapes.of(declared.toType, env)
      count <-
        if (TopTypes.terminalGoal(TopTypes.polarized(result, true))) Right(Count.Finite(1))
        else
          for {
            _ <- Traversals.sequence(parameters.map(p => later(p.symbol))).left.map(Uncovered(_))
            inputs <- Traversals.sequence(parameters.map(p => shapes.of(p.tpt.toType, env)))
          } yield Inhabitation.count(
            parameters.lazyZip(inputs).map { (p, read) =>
              Binding(s"$self.${p.name}", read, Some(p.name.toString))
            },
            result,
            maxDepth = Readings.Depth
          )
    } yield count
  }

  // What a member's or a parameter's flags, or a constructor's name, leave to slices not read yet.
  private def later(symbol: TermSymbol): Either[String, Unit] =
    if (symbol.isInline) Left("an inline definition")
    else if (symbol.isGivenOrUsing || symbol.isImplicit) Left("a contextual definition")
    else if (symbol.isExtensionMethod) Left("an extension method")
    else if (World.isConstructor(symbol)) Left("a constructor")
    else Right(())

  // Every owner up to the package is an object that inherits nothing.
  private def static(owner: Symbol): Either[String, Unit] = owner match {
    case _: PackageSymbol                  => Right(())
    case c: ClassSymbol if c.isModuleClass =>
      // An object's self type is its own singleton.
      if (c.parents.forall(memberless)) static(c.owner)
      else Left(s"an enclosing object with parents: ${c.name}")
    case c: ClassSymbol => Left(s"an enclosing class: ${c.name}")
    case other          => Left(s"an enclosing definition: ${other.name}")
  }

  // Which values of a pattern or multi-name definition the constructor evaluates, and which is the
  // measured choice, is the body reading's to say (slice S6).
  private def plain(owner: Symbol): Either[String, Unit] = owner.tree match {
    case Some(c: ClassDef) if Bodies.destructures(c) =>
      Left("an enclosing object with a pattern or multi-name value definition")
    case _ => Right(())
  }

  // Whether the object a member runs in is ever built (register D26): a constructor that always
  // throws leaves no instance.
  private def built(owner: Symbol, measured: Tree): Boolean =
    owner.tree.forall {
      case c: ClassDef => !Bodies.alwaysThrows(c, Some(measured))
      case _           => true
    }

  // A parent that declares no member a count reads (v1.md §2).
  private def memberless(parent: Type): Boolean =
    World.classNamed(parent).exists(c => Readings.Memberless(c.displayFullName))

  // A proper type parameter any type instantiates.
  private def unbounded(binder: TypeParam): Either[Unread, Unit] = {
    if (Shapes.unbounded(binder.symbol.declaredBounds)) Right(())
    else Left(Uncovered(s"a bounded or higher-kinded type parameter: ${binder.name}"))
  }

}

private[compiled] object Readings {

  /** The solver's depth budget, the same for every row; its state budget is its own default. */
  val Depth = 64

  // The parents that declare no member a count reads (v1.md §2), the compiler's own for a case
  // class among them.
  private val Memberless =
    Set("java.lang.Object", "scala.Product", "scala.Equals", "java.io.Serializable")

}
