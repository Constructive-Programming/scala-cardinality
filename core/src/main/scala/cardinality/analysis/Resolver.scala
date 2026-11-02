package cardinality.analysis

import scala.meta.*

import cardinality.analysis.capability.Insertions
import cardinality.analysis.model.{ResolutionContext, TypeOperators}
import cardinality.analysis.resolution.{Found, InheritedTypeMembers, TypeMembers}
import cardinality.capacity.Reading

import MethodAnalysis.{Frame, Limits, Resolved, Target, TypeEntry}

/** The narrow view of the index the measurement needs, so the two stay apart without a second copy
  * of the resolution rules.
  */
private[cardinality] trait Resolver {

  /** The estimate's reading of a source, so a constructor position answers as its definition. */
  def reading(path: String): Reading

  def lookup(name: String, frame: Frame): List[TypeEntry]

  /** The declaration a simple or dotted name means at the context's reference, imports included. */
  def resolveName(name: String, context: ResolutionContext): Found

  /** Whether a simple name, read at `tpe`'s position, means a member its template inherits. */
  def inheritsName(tpe: Type, name: String, context: ResolutionContext): Boolean

  /** The supplied declaration an `extends` clause names (None outside the source set). */
  def parent(init: Init, from: Frame): Either[String, Option[(TypeEntry, Map[String, Resolved])]]

  /** Whether a parent clause names a type whose members this reading does not have. */
  def unsuppliedParent(init: Init, from: Frame): Boolean

  /** A parent clause outside the source set: its path, how its name was bound, and whether the
    * stdlib model's declaration of that path is the one it names.
    */
  def outsideParent(init: Init, from: Frame): Option[InheritedTypeMembers.Outside]

  /** Whether every instance of a template is of a supplied declaration no subclass extends. */
  def fixedTemplate(frame: Frame): Boolean

  /** The builtin or stdlib-model key of a name no supplied declaration binds, or why it is none. */
  def builtin(written: String, context: ResolutionContext): Either[String, String]

  /** A template's supplied base classes by frame, itself included, and whether they are all. */
  def baseClasses(frame: Frame): (Set[String], Boolean)

  /** What a type name or a path selection means when it denotes a type member (members M1). */
  def typeMember(tpe: Type, context: ResolutionContext): Option[Either[String, TypeMembers.Meaning]]

  /** A callee's member meaning where it is applied to no arguments (an applied one has none). */
  def unappliedMember(
      callee: Type,
      args: List[Type],
      context: ResolutionContext
  ): Option[Either[String, TypeMembers.Meaning]] =
    if (args.isEmpty) typeMember(callee, context) else None

  /** The type a stable input is read as: a satisfiable type-only refinement is its base. */
  def stableInput(tpe: Type, context: ResolutionContext): Either[String, Type]

  /** Whether a type declares or inherits a type member: a type that is no supplied template may. */
  def declaresMember(owner: Type, name: String, context: ResolutionContext): Boolean

  /** Whether a sealed template's family has a subtype a file outside its own may extend. */
  def openFamily(template: Frame): Boolean

  /** The declaration a type name means at `tpe`'s position, an inherited member first. */
  def declaration(tpe: Type, name: String, context: ResolutionContext): Found

  /** A channel through which declarations the sources do not supply may bind a builtin name. */
  def unreadChannel(context: ResolutionContext): Option[String]

  /** The source declarations that may defer an expression the compiler inserts them around. */
  def insertions: Insertions

  /** The channel through which another declaration may bind a name the stdlib model reads, where
    * the name is read, if one is open there.
    */
  def rootShadow(name: String, context: ResolutionContext): Option[String]

  /** Why a `transparent inline` call site may observe more than the declared result `tpe`. */
  def narrowedResult(tpe: Type, context: ResolutionContext): Option[String]

  def resolve(
      tpe: Type,
      frame: Frame,
      variables: Map[String, Resolved],
      visiting: Set[String] = Set.empty
  ): Resolved

  def resolve(tpe: Type, context: ResolutionContext): Resolved

  def typeParameters(frame: Frame): Map[String, Resolved]

  /** The operators higher-kinded names are bound to. */
  def operators: TypeOperators

  /** Every proper binder that carries a subtype bound, by its atom, with its declaring frame. */
  def boundedBinders: Map[String, (Frame, Type.Param)]

  def accessible(tree: Tree, owner: Frame, from: Frame): Boolean

  def modules: List[Frame]

  def kindOf(target: Target): String

  def limits: Limits

  def inspect(operation: String): Unit
  def expanding[A](operation: String)(read: => A): A
  def solverDepth: Int
  def peers(path: List[String]): List[Frame]

  /** The frames one scope's declarations live in: every file's frame of a package (package members
    * are indexed across files), any other frame alone.
    */
  def scopes(frame: Frame): List[Frame] = {
    val shared = peers(frame.path)
    if (shared.exists(_.id == frame.id)) shared else List(frame)
  }

  def isPackage(frame: Frame): Boolean
}
