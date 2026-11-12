package cardinality.compiled

import cardinality.analysis.inhabitation.Inhabitation
import cardinality.analysis.inhabitation.Inhabitation.{Binding, Count, Shape}
import cardinality.analysis.resolution.TopTypes
import cardinality.types.Traversals
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Trees.*

/** What the compiled front end measures so far, read from symbols and types
  * (docs/plans/compiled-own-types.md, "Front end: design and status", lists the slices): a `Left`
  * names what a row needs that no slice reads yet, and is never a count. A type the counting model
  * refuses is the row's `?` with the model's reason (`Unread.Refused`).
  *
  * Method rows: a method or value whose environment is its own parameters and what the classes
  * around it hold (`Receivers`; a concrete member around it computes from what the environment
  * already holds, and an object holds nothing), over the types `Shapes` reads and the method's own
  * unbounded type parameters.
  *
  * An inline method counts as a method unless an expansion of it is decided at a call site, which
  * `Inlines` says: its refusal is the row's reason, the model's own.
  *
  * A class's row is its primary constructor's, read by `Constructors` where the class is declared.
  * Definition rows are measured by `Stored`.
  */
final private[compiled] class Readings(world: World, stored: Stored, inlines: Inlines)(using
    Context
) {
  import Unread.Uncovered

  private val shapes = new Shapes(world)
  private val constructors = new Constructors(shapes, stored)
  private val receivers = new Receivers(world, shapes)

  // ---- method rows --------------------------------------------------------------------------

  /** The count of a method row, and its captures: the names of what the classes around it supply. A
    * class's row measures its primary constructor.
    */
  def method(tree: ValOrDefDef): Either[String, (Count, List[String])] = {
    val primary = tree.symbol.owner match {
      case c: ClassSymbol => c.tree.exists(_.rhs.constr.symbol == tree.symbol)
      case _              => false
    }
    // A constructor runs where its class is declared: the instance it builds does not exist yet.
    val owner = if (primary) tree.symbol.owner.owner else tree.symbol.owner
    for {
      _ <- if (primary) Right(()) else Readings.later(tree.symbol)
      // A constructor row is read where every owner is an object; a member reads its receivers.
      around <- if (primary) static(owner).map(_ => Nil) else receivers.around(owner)
      _ <- plain(owner)
      count <-
        // A call of an inline method is its expansion, which builds no object: the old front
        // end counts one in an object that is never built like any other.
        if (inlines.inlineMethod(tree.symbol) || built(owner, tree))
          (tree match {
            case constructor: DefDef if primary => constructors.count(constructor).map(_ -> Nil)
            case _ => counted(tree, around).map(_ -> captures(tree, around))
          }).left.flatMap {
            // A row the model refuses lists no captures, as the old front end's does not.
            case Unread.Refused(reason) => Right(Count.Unresolved(List(reason)) -> Nil)
            case Uncovered(what)        => Left(what)
          }
        else if (primary) Left("a constructor in an object that is never built")
        // No call of a member of an object that is never built returns: one implementation.
        else Right(Count.Finite(1) -> Nil)
    } yield count
  }

  // A `transparent inline` result is refused first (rule X: callers see the expansion's type).
  // A goal with one value has one implementation whatever the environment holds (v1.md §2), so
  // the result is read next, before what an inline body does, as the old front end reads it;
  // else the count is the solver's over the parameters and what the receivers hold.
  private def counted(
      tree: ValOrDefDef,
      around: List[ClassDef]
  ): Either[Unread, Count] = {
    val (binders, parameters, declared) = signature(tree)
    val self = tree.symbol.displayFullName
    val env = environment(tree, around)
    for {
      _ <- inlines.narrowed(tree.symbol).map(Unread.Refused(_)).toLeft(())
      _ <- Traversals.sequence(binders.map(Readings.unbounded))
      result <- shapes.of(declared.toType, env)
      count <-
        if (TopTypes.terminalGoal(TopTypes.polarized(result, true))) Right(Count.Finite(1))
        else
          for {
            _ <- inlines.refusal(tree.symbol).map(Unread.Refused(_)).toLeft(())
            _ <- Readings.plainInputs(parameters)
            held <- receivers.bindings(around, tree.symbol, env)
            inputs <- shapes.inputs(parameters, env)
          } yield Inhabitation.count(
            held ++ parameters.lazyZip(inputs).map { (p, read) =>
              Binding(s"$self.${p.name}", read, Some(p.name.toString))
            },
            result,
            maxDepth = Readings.Depth
          )
    } yield count
  }

  // A member's type parameters, value parameters and declared result.
  private def signature(tree: ValOrDefDef) = tree match {
    case v: ValDef => (Nil, Nil, v.tpt)
    case d: DefDef =>
      (d.paramLists.flatMap(_.toOption.toList.flatten), Shapes.parameters(d), d.resultTpt)
  }

  // The measured method's type parameters, each an atom of its own, beside its receivers', and the
  // method as the site rule F reads an opaque type from.
  private def environment(tree: ValOrDefDef, around: List[ClassDef]): Shapes.Env = {
    val self = tree.symbol.displayFullName
    Shapes.Env(
      receivers.atoms(around) ++ signature(tree)._1.map { binder =>
        (binder.symbol: Symbol) -> Shape.Atom(s"$self[${binder.name}]")
      },
      tree.symbol
    )
  }

  // A row's captures, as the report lists them: the names its receivers supply, and for an
  // extension method the parameters of its `extension` clause, written before the method.
  private def captures(tree: ValOrDefDef, around: List[ClassDef]): List[String] = {
    val group = tree match {
      case method: DefDef if method.symbol.isExtensionMethod && !method.pos.isUnknown =>
        Shapes
          .parameters(method)
          .filter(p => !p.pos.isUnknown && p.pos.startOffset < method.pos.startOffset)
          .map(_.name.toString)
      case _ => Nil
    }
    (receivers.names(around, tree.symbol) ++ group).distinct.sorted
  }

  // Every owner up to the package is an object that inherits nothing.
  private def static(owner: Symbol): Either[String, Unit] = owner match {
    case _: PackageSymbol                  => Right(())
    case c: ClassSymbol if c.isModuleClass =>
      // An object's self type is its own singleton.
      if (World.ancestors(c).isEmpty) static(c.owner)
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

}

private[compiled] object Readings {

  /** A proper type parameter any type instantiates. */
  def unbounded(binder: TypeParam)(using Context): Either[Unread, Unit] =
    if (Shapes.unbounded(binder.symbol.declaredBounds)) Right(())
    else Left(Unread.Uncovered(s"a bounded or higher-kinded type parameter: ${binder.name}"))

  /** Parameters the reading takes as inputs. A contextual one with a name is a value like any other
    * (slice S5): only implicit search tells it apart, and a body is counted, not searched. One
    * without a name (`using T`, a context bound's evidence) is found by search alone, among
    * whatever else is in scope: not read yet.
    */
  def plainInputs(parameters: List[ValDef]): Either[Unread, Unit] =
    parameters
      // The compiler names it `x$N` or `evidence$N`; a constructor's is not flagged synthetic.
      .find(p => contextual(p.symbol) && p.name.toString.contains("$"))
      .map(p => Unread.Uncovered(s"an anonymous contextual parameter: ${p.name}"))
      .toLeft(())

  def contextual(symbol: TermSymbol): Boolean = symbol.isGivenOrUsing || symbol.isImplicit

  /** What a member's kind leaves to slices not read yet. A given alias, an implicit definition and
    * an extension method count as the methods they are (S5); a given instance is a class.
    */
  def later(symbol: TermSymbol)(using Context): Either[String, Unit] =
    if (World.isConstructor(symbol)) Left("a secondary constructor")
    else if (contextual(symbol) && instance(symbol)) Left("a given instance")
    else Right(())

  // A given instance's value or method: it builds the class the compiler names like it.
  private def instance(symbol: TermSymbol)(using Context): Boolean =
    symbol.isModuleVal || symbol.tree.exists {
      case member: ValOrDefDef =>
        val declared = member match {
          case method: DefDef => method.resultTpt
          case value: ValDef  => value.tpt
        }
        World
          .classNamed(declared.toType)
          .exists { c =>
            // Made with the given, at its own place (`Targets.instances`): a class written apart
            // that the given shares its name with is an alias's type.
            c.owner == symbol.owner && c.name.toString == symbol.name.toString &&
            c.tree.forall { made =>
              made.pos.isUnknown || member.pos.isUnknown ||
              made.pos.startOffset == member.pos.startOffset
            }
          }
      case _ => false
    }

  /** The solver's depth budget, the same for every row; its state budget is its own default. */
  val Depth = 64

}
