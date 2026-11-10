package cardinality.analysis.model

import scala.meta.{Defn, Type}

import cardinality.analysis.*
import cardinality.types.TypeApplications
import dev.constructive.eo.generics.lens

import MethodAnalysis.{Frame, Resolved, TypeEntry}

/** The point a name is read from: its source and the offset in it. */
final private[cardinality] case class Anchor(source: String, offset: Int)

/** Lexical lookup and binder scope travel together; the observing use site survives rebinding. */
final private[cardinality] case class ResolutionContext(
    frame: Frame,
    variables: Map[String, Resolved],
    visiting: Set[String],
    observer: Option[Frame] = None,
    // The position the name is read from, as (source, offset): the reference decides which
    // import clauses of the enclosing blocks have already bound their names. `resolve` sets it
    // from the type it is given, so every read carries the position of its own reference.
    at: Option[Anchor] = None,
    // The stable path `this` denotes while a parent's declaration or a member alias is read as a
    // member of another receiver (members M2): its type members are that receiver's.
    self: Option[MemberPath] = None,
    // Where each binder was bound by an application, the argument as written and the scope it was
    // written in: a projection reads the argument again rather than its frozen shape (members MT1').
    arguments: Map[String, (Type, ResolutionContext)] = Map.empty,
    // The value whose refinement is being read: inside `p: T { … }`, `this` and the members the
    // refinement or T declares are p's.
    refinedThis: Option[MemberPath] = None,
    // The reference, as (source, offset), of a value's declared type when the value is built before
    // the measured declaration's receiver, so it is never that receiver (capabilities C6). Only a
    // read at that very reference keeps it (`referencing`): a part of the type is another value.
    preceding: Option[Anchor] = None
) {

  def useSite: Frame = observer.getOrElse(frame)

  /** This context read at `tpe`'s own reference. */
  def referencing(tpe: Type): ResolutionContext = {
    val here = referenceAt(tpe)
    copy(at = here, preceding = preceding.filter(p => tpe.pos.start >= 0 && here.contains(p)))
  }

  private def referenceAt(tpe: Type): Option[Anchor] =
    val file = frame.input
    if (tpe.pos.start >= 0) Some(Anchor(file, tpe.pos.start))
    else
      at match
        // Reconstructed inside the caller's own file: the caller's reference decides.
        case Some(Anchor(callerFile, _)) if callerFile == file => at
        // Reconstructed under another file's scope — a transparent alias's body: the scope's
        // own declaration position is the lower bound, so clauses written before it bind.
        case _ => frameAnchor(file)

  // Root frames carry the path alone; a nested frame's id is `input:offset[...]`, the
  // declaration point the scope was opened at.
  private def frameAnchor(file: String): Option[Anchor] =
    frame.id.drop(file.length + 1).takeWhile(_.isDigit) match
      case digits if digits.nonEmpty => Some(Anchor(file, digits.toInt))
      case _                         => None

  def read(tpe: Type, resolver: Resolver): Resolved =
    resolver.resolve(tpe, this)

  def bind(entries: Iterable[(String, Resolved)]): ResolutionContext =
    ResolutionOptics.variablesLens
      .modify(_ ++ entries)(this)
      .copy(arguments = arguments -- entries.map(_._1))

  /** This context with the arguments, as written, that bound the given binders. */
  def written(bound: Iterable[(String, (Type, ResolutionContext))]): ResolutionContext =
    copy(arguments = arguments ++ bound)

  /** This context reading `this` as the given path. */
  def as(path: MemberPath): ResolutionContext = copy(self = Some(path))

  /** This context inside a refinement of the given path. */
  def refining(path: MemberPath): ResolutionContext = copy(refinedThis = Some(path))

  def enter(key: String): ResolutionContext =
    ResolutionOptics.visitingLens.modify(_ + key)(this)

  /** Alias declarations supply lexical names, not their caller's representation permissions. */
  def inScope(owner: Frame, bindings: Map[String, Resolved]): ResolutionContext =
    copy(
      frame = owner,
      variables = bindings,
      observer = Some(useSite),
      arguments = Map.empty,
      refinedThis = None
    )

  /** An alias body's own scope: its declaring frame's binders, and its parameters as the given
    * readings (an argument it is applied to, or why none is known). A reading of an alias has no
    * scope while the same reading is already inside that alias: the alias is recursive.
    */
  def enterAlias(
      entry: TypeEntry,
      binders: Map[String, Resolved],
      reading: AliasReading,
      arguments: List[Resolved] = Nil
  ): Option[ResolutionContext] = {
    val key = s"$reading:${entry.owner.id}:${entry.name}"
    val parameters = entry.tree match {
      case alias: Defn.Type => alias.tparamClause.values
      case _                => Nil
    }
    Option.unless(visiting(key))(
      inScope(entry.owner, binders)
        .bind(parameters.zipWithIndex.map { (param, index) =>
          param.name.value -> arguments.lift(index).getOrElse(Left("alias parameter"))
        })
        .enter(key)
    )
  }

}

/** The readings that enter alias bodies, each tracking its own recursion. */
private[cardinality] enum AliasReading {
  case Slot, Union, UnionBody, Runtime, Transparent
}

private[cardinality] object ResolutionOptics {

  val variablesLens = lens[ResolutionContext](_.variables)
  val visitingLens = lens[ResolutionContext](_.visiting)
  val entryTreeLens = lens[TypeEntry](_.tree)
  val entryTypeDefinition = entryTreeLens.andThen(TypeApplications.typeDefinition)
  val typeArguments = TypeApplications.arguments

}
