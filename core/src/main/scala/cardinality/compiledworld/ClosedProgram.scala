package cardinality.compiled

import scala.collection.mutable
import scala.util.control.NonFatal

import tastyquery.Contexts.*
import tastyquery.Modifiers.OpenLevel
import tastyquery.SourceLanguage
import tastyquery.Symbols.*

/** The closed world over symbols (AGENTS.md, "The modelling assumption: a closed world"): the
  * module's classes and its dependencies' are the whole program, so a class or a member is open
  * only where one of them extends or overrides it. Every answer is about exact symbols.
  *
  * The program is read in two parts, each only when a question needs it:
  *
  *   - the module's classes (`own`), with every class their trees declare: nested, local and
  *     anonymous ones;
  *   - the classes of the dependency classpath (`dependencies`, the Scala library among them), with
  *     the classes their trees declare.
  *
  * A dependency is compiled before the module, so no class of it extends a class of the module: a
  * question about a class of the module reads the module alone.
  *
  * What is not listed is not known to be absent, and an answer says so (`complete` is false,
  * `Overriding.Unknown`, `finalClass` is false, `mixes` is true):
  *
  *   - a class that does not load (`unloaded` for the module, the second half of `dependencies` for
  *     the classpath) may extend anything its part of the program may;
  *   - a parent that is not read may extend any class outside the module, and may override any
  *     member of the class that mixes it in;
  *   - the JDK is not listed, so a class of it is closed only where it is written `final`;
  *   - a class without TASTy (Java, Scala 2) has local and anonymous subclasses no symbol stands
  *     for, so it too is closed only where it is written `final`, and the library's and the
  *     dependencies' Scala 3 classes are taken not to be extended by such a class.
  */
final private[compiled] class ClosedProgram(
    own: List[ClassSymbol],
    unloaded: List[String],
    dependencies: => (List[ClassSymbol], List[String])
)(using Context) {
  import ClosedProgram.*

  // One part of the program: each class it declares under the classes it extends, and whether
  // every parent of every class was read.
  final private class Part(tops: List[ClassSymbol]) {

    val (below: Map[ClassSymbol, List[ClassSymbol]], parentsRead: Boolean) = {
      val read = tops.flatMap(declared).distinct.map { c =>
        val (line, whole) = linearized(c)
        (line.drop(1).map(_ -> c), whole)
      }
      (read.flatMap(_._1).groupMap(_._1)(_._2), read.forall(_._2))
    }

  }

  private val world = new World(own.toSet)
  private lazy val module = new Part(own)
  private lazy val listing = dependencies
  private lazy val listed: Set[Symbol] = listing._1.toSet
  private lazy val outside = new Part(listing._1.filterNot(world.ofModule))

  private val extenders = mutable.Map.empty[ClassSymbol, Subclasses]

  /** The classes of the program that extend `c`, at any depth, and whether they are all of them. */
  def subclasses(c: ClassSymbol): Subclasses = extenders.get(c).getOrElse {
    val found =
      if (c.openLevel == OpenLevel.Final || c.isModuleClass) Subclasses(Nil, complete = true)
      else sealedBelow(c).getOrElse(listedBelow(c))
    extenders.update(c, found)
    found
  }

  // The compiler lists a Scala 3 sealed class's children, wherever they are: where each is a
  // class or an object, they and what extends them are all. A child that is the class itself
  // stands for a local or anonymous one, and an enum's value case is an anonymous class: those
  // are found by listing.
  private def sealedBelow(c: ClassSymbol): Option[Subclasses] =
    if (c.openLevel != OpenLevel.Sealed || c.sourceLanguage != SourceLanguage.Scala3) None
    else {
      val children = c.sealedChildren.map {
        case child: ClassSymbol if child != c => Some(child)
        case child: TermSymbol                => child.moduleClass
        case _                                => None
      }
      Option.when(children.forall(_.nonEmpty)) {
        val under = children.flatten.map(subclasses)
        Subclasses((children.flatten ++ under.flatMap(_.found)).distinct, under.forall(_.complete))
      }
    }

  private def listedBelow(c: ClassSymbol): Subclasses =
    // A parent the module does not read is no class of the module, and leads to none. A class
    // the module compiles without TASTy (from Java) has local and anonymous classes no symbol
    // stands for, which may extend any class of the module.
    if (world.ofModule(c))
      Subclasses(module.below.getOrElse(c, Nil), unloaded.isEmpty && own.forall(_.tree.nonEmpty))
    else
      Subclasses(
        (module.below.getOrElse(c, Nil) ++ outside.below.getOrElse(c, Nil)).distinct,
        unloaded.isEmpty && module.parentsRead && listing._2.isEmpty && outside.parentsRead &&
          c.tree.nonEmpty && World.owners(c).exists(listed)
      )

  /** Whether no instance of `c` is of a subclass: it is written `final`, or it is an object, or it
    * is a concrete class (not written `open`, `sealed` or `abstract`, and no trait) that no class
    * of the program extends. A constructor's visibility does not open it: only its scope may extend
    * it, and that scope is supplied.
    */
  def finalClass(c: ClassSymbol): Boolean =
    c.openLevel == OpenLevel.Final || c.isModuleClass ||
      (!c.isTrait && !c.isAbstractClass && c.openLevel == OpenLevel.Closed &&
        subclasses(c) == Subclasses(Nil, complete = true))

  private val overriders = mutable.Map.empty[(ClassSymbol, TermOrTypeSymbol), Overriding]

  /** How a class extending `owner` may override its member `member` (one it declares or inherits).
    * The question is asked from each subclass's side: the classes its linearization has before
    * `owner` are what the member resolves through there, a subclass of `owner` or a parent mixed in
    * beside it. One of them declaring a matching member overrides it; a mixed-in parent's abstract
    * declaration does not, since the member stays `owner`'s there. A parent that is not read, mixed
    * in beside `owner`, may override it.
    */
  def overriding(owner: ClassSymbol, member: TermOrTypeSymbol): Overriding =
    overriders.getOrElseUpdate(
      (owner, member),
      if (member.isPrivate || member.isFinalMember || World.isConstructor(member)) Overriding.No
      else {
        val below = subclasses(owner)
        val each = below.found.map { subclass =>
          val (line, whole) = linearized(subclass)
          // A line that is not whole is in no sure order: everything of it that `owner` does not
          // extend may come before `owner`.
          val between =
            if (whole) line.takeWhile(_ != owner)
            else line.filterNot(linearized(owner)._1.toSet)
          val declared =
            try
              between.exists { k =>
                member.matchingSymbol(k, subclass).exists { found =>
                  found != member && !found.isPrivate &&
                  (!found.isAbstractMember || k.isSubClass(owner))
                }
              }
            // A signature that is not read: a declaration of the name between the two is taken
            // for an overrider, as the scalameta reading takes one by name.
            catch {
              case NonFatal(_) =>
                between.exists(_.declarations.exists(d => d.name == member.name && !d.isPrivate))
            }
          if (declared) Overriding.Supplied
          else if (whole || !unreadBeside(subclass, owner)) Overriding.No
          else Overriding.Unknown
        }
        val rest = if (below.complete) Overriding.No else Overriding.Unknown
        (rest :: each).maxBy(_.ordinal)
      }
    )

  /** Whether an instance may be of both classes: one extends the other, or a class of the program
    * extends both, or the classes that extend one of them are not all known.
    */
  def mixes(left: ClassSymbol, right: ClassSymbol): Boolean = {
    val ((leftLine, leftWhole), (rightLine, rightWhole)) = (linearized(left), linearized(right))
    !leftWhole || !rightWhole || leftLine.contains(right) || rightLine.contains(left) || {
      val (lefts, rights) = (subclasses(left), subclasses(right))
      !lefts.complete || !rights.complete || lefts.found.exists(rights.found.toSet)
    }
  }

}

private[compiled] object ClosedProgram {

  /** The classes of the program that extend a class, and whether they are all of them. */
  final case class Subclasses(found: List[ClassSymbol], complete: Boolean)

  /** How a member may be overridden: not at all, or only by a class that is not read, or by a class
    * of the program.
    */
  enum Overriding {
    case No, Unknown, Supplied
  }

  // A class's linearization and whether it is whole: where a parent is not read, the class, the
  // parents that are and what they extend.
  private def linearized(c: ClassSymbol)(using Context): (List[ClassSymbol], Boolean) =
    try (c.linearization, true)
    catch {
      case NonFatal(_) =>
        val parents =
          try c.parents
          catch { case NonFatal(_) => Nil }
        val above = parents.flatMap { parent =>
          try World.classNamed(parent).toList.flatMap(linearized(_)._1)
          catch { case NonFatal(_) => Nil }
        }
        ((c :: above).distinct, false)
    }

  // Whether a parent that is not read is among what `c` extends beside `owner`: not above it.
  private def unreadBeside(c: ClassSymbol, owner: ClassSymbol)(using Context): Boolean =
    c != owner && {
      val parents =
        try Some(c.parents.map(World.classNamed))
        catch { case NonFatal(_) => None }
      parents.forall(_.exists(_.forall(unreadBeside(_, owner))))
    }

  // A class and every class its tree declares: nested, local and anonymous ones. A class without
  // TASTy is itself and its member classes.
  private def declared(c: ClassSymbol)(using Context): List[ClassSymbol] =
    c.tree match {
      case Some(tree) => World.classes(tree).map(_.symbol)
      case None       =>
        val members =
          try c.declarations.collect { case member: ClassSymbol => member }
          catch { case NonFatal(_) => Nil }
        c :: members.flatMap(declared)
    }

}
