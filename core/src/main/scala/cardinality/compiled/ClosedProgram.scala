package cardinality.compiled

import scala.collection.mutable
import scala.util.control.NonFatal

import cardinality.tasty.CompiledTypes
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
  *   - a class without TASTy (Java, Scala 2) has local and anonymous classes no symbol stands for
  *     (`Lone$1`). `ownFiles` and `classpathFiles` list them from their class files
  *     (`CompiledTypes.unnamed`), for the module's output and its classpath: the classes each
  *     extends, by binary name, and the methods it declares. One of them under a class is a
  *     subclass of it that overrides the members it declares, by name. Where a parent of one does
  *     not resolve, it may extend anything.
  */
final private[compiled] class ClosedProgram(
    own: List[ClassSymbol],
    unloaded: List[String],
    dependencies: => (List[ClassSymbol], List[String]),
    ownFiles: => List[CompiledTypes.ClassFile],
    classpathFiles: => List[CompiledTypes.ClassFile]
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

  private val ownTops = own.toSet

  private def ofModule(symbol: Symbol): Boolean = World.owners(symbol).exists {
    case c: ClassSymbol => ownTops(c)
    case _              => false
  }

  private lazy val module = new Part(own)
  private lazy val listing = dependencies
  private lazy val listed: Set[Symbol] = listing._1.toSet
  private lazy val outside = new Part(listing._1.filterNot(ofModule))

  // The classes no symbol stands for in a part of the program, and whether every parent of each
  // resolved. One that extends another has that one's parents and declares its methods too.
  private def indexed(files: List[CompiledTypes.ClassFile]): (List[Unnamed], Boolean) = {
    val byName = files.map(file => file.binary -> file).toMap
    def resolved(binary: String, seen: Set[String]): List[(Option[ClassSymbol], List[String])] =
      byName.get(binary) match {
        // A class file that does not read has no parents: every other class has a super class.
        case Some(file) if file.parents.isEmpty => List((None, Nil))
        case Some(file) if !seen(binary)        =>
          file.parents.flatMap(resolved(_, seen + binary)).map((k, m) => (k, m ++ file.methods))
        case Some(_) => List((None, Nil))
        case None    => List((named(binary), Nil))
      }
    val read = files.map { file =>
      val parents = resolved(file.binary, Set.empty)
      val methods = file.methods ++ parents.flatMap(_._2)
      (
        Unnamed(file.binary, parents.flatMap(_._1).distinct, methods.toSet),
        parents.forall(_._1.nonEmpty)
      )
    }
    (read.map(_._1), read.forall(_._2))
  }

  private lazy val (moduleUnnamed: List[Unnamed], moduleUnnamedRead: Boolean) = indexed(ownFiles)

  private lazy val (outsideUnnamed: List[Unnamed], outsideUnnamedRead: Boolean) =
    indexed(classpathFiles)

  // The classes no symbol stands for, among `among`, that extend `c`.
  private def unnamedBelow(c: ClassSymbol, among: List[Unnamed]): List[Unnamed] =
    among.filter(_.parents.exists(linearized(_)._1.contains(c)))

  private val extenders = mutable.Map.empty[ClassSymbol, Subclasses]

  /** The classes of the program that extend `c`, at any depth, and whether they are all of them. */
  def subclasses(c: ClassSymbol): Subclasses = extenders.get(c).getOrElse {
    val found =
      if (c.openLevel == OpenLevel.Final || c.isModuleClass) Subclasses.Empty
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
        Subclasses(
          (children.flatten ++ under.flatMap(_.found)).distinct,
          under.forall(_.complete),
          under.flatMap(_.unnamed).distinct
        )
      }
    }

  private def listedBelow(c: ClassSymbol): Subclasses =
    // A parent the module does not read is no class of the module, and leads to none.
    if (ofModule(c))
      Subclasses(
        module.below.getOrElse(c, Nil),
        unloaded.isEmpty && moduleUnnamedRead,
        unnamedBelow(c, moduleUnnamed)
      )
    else
      Subclasses(
        (module.below.getOrElse(c, Nil) ++ outside.below.getOrElse(c, Nil)).distinct,
        unloaded.isEmpty && module.parentsRead && listing._2.isEmpty && outside.parentsRead &&
          moduleUnnamedRead && outsideUnnamedRead && World.owners(c).exists(listed),
        unnamedBelow(c, moduleUnnamed ++ outsideUnnamed)
      )

  /** Whether no instance of `c` is of a subclass: it is written `final`, or it is an object, or it
    * is a concrete class (not written `open`, `sealed` or `abstract`, and no trait) that no class
    * of the program extends. A constructor's visibility does not open it: only its scope may extend
    * it, and that scope is supplied.
    */
  def finalClass(c: ClassSymbol): Boolean =
    c.openLevel == OpenLevel.Final || c.isModuleClass ||
      (!c.isTrait && !c.isAbstractClass && c.openLevel == OpenLevel.Closed &&
        subclasses(c) == Subclasses.Empty)

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
        val ownerLine = linearized(owner)._1.toSet
        val each = below.found.map { subclass =>
          val (line, whole) = linearized(subclass)
          // A line that is not whole is in no sure order: everything of it that `owner` does not
          // extend may come before `owner`.
          // What may override a member is before its declaring class and outside what `owner`
          // extends (`owner` resolves the member to itself there).
          val between =
            if (whole) line.takeWhile(_ != member.owner).filterNot(ownerLine)
            else line.filterNot(ownerLine)
          val declared =
            try
              between.exists { k =>
                member.matchingSymbol(k, subclass).exists { found =>
                  found != member && !found.isPrivate &&
                  (!found.isAbstractMember || k.isSubClass(member.owner.asClass))
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
        // A class no symbol stands for overrides what it declares, by name, and so does what it
        // mixes in beside `owner`.
        val nameless = below.unnamed.map { subclass =>
          val between = subclass.parents.flatMap(linearized(_)._1).filterNot(ownerLine)
          val name = member.name.toString
          // A class file spells a method by its encoded name (`$plus` for `+`).
          if (
            subclass.methods(scala.reflect.NameTransformer.encode(name)) ||
            between.exists(_.declarations.exists(d => d.name.toString == name && !d.isPrivate))
          ) Overriding.Supplied
          else Overriding.No
        }
        val rest = if (below.complete) Overriding.No else Overriding.Unknown
        (rest :: each ::: nameless).maxBy(_.ordinal)
      }
    )

  /** What may override `member` in the instances of `owner`'s subclasses: the declarations of its
    * name in every class a subclass's linearization holds outside `owner`'s (the subclass itself,
    * what it mixes in beside `owner`), and `None` for a subclass no symbol stands for, whose body
    * is not read.
    */
  def overriders(owner: ClassSymbol, member: TermOrTypeSymbol): List[Option[TermSymbol]] =
    overriderLists.getOrElseUpdate((owner, member), overridersOf(owner, member))

  private val overriderLists =
    mutable.Map.empty[(ClassSymbol, TermOrTypeSymbol), List[Option[TermSymbol]]]

  private def overridersOf(owner: ClassSymbol, member: TermOrTypeSymbol) = {
    val below = subclasses(owner)
    val ownerLine = linearized(owner)._1.toSet
    val name = member.name.toString
    below.found
      .flatMap(s => linearized(s)._1)
      .filterNot(ownerLine)
      .distinct
      .flatMap(_.declarations.collect {
        case d: TermSymbol if d.name.toString == name => Some(d)
      }) ++ below.unnamed.collect {
      case u
          if u.methods(scala.reflect.NameTransformer.encode(name)) ||
            u.parents.flatMap(linearized(_)._1).filterNot(ownerLine).exists {
              _.declarations.exists(_.name.toString == name)
            } =>
        None
    }
  }

  /** Whether an instance may be of both classes: one extends the other, or a class of the program
    * extends both, or the classes that extend each of them are not all known (or not all read).
    */
  def mixes(left: ClassSymbol, right: ClassSymbol): Boolean = {
    val ((leftLine, leftWhole), (rightLine, rightWhole)) = (linearized(left), linearized(right))
    // A class of the program that extends both is among the subclasses of either: those of one
    // that are all known settle it.
    def reaches(c: ClassSymbol, other: ClassSymbol) = {
      val (line, whole) = linearized(c)
      !whole || line.contains(other)
    }
    def within(below: Subclasses, other: ClassSymbol) =
      below.found.exists(reaches(_, other)) ||
        below.unnamed.exists(_.parents.exists(reaches(_, other)))
    !leftWhole || !rightWhole || leftLine.contains(right) || rightLine.contains(left) || {
      val (lefts, rights) = (subclasses(left), subclasses(right))
      if (lefts.complete) within(lefts, right)
      else if (rights.complete) within(rights, left)
      else true
    }
  }

}

private[compiled] object ClosedProgram {

  /** The classes of the program that extend a class: the ones a symbol stands for, whether they and
    * the `unnamed` ones are all of them, and the ones no symbol stands for.
    */
  final case class Subclasses(
      found: List[ClassSymbol],
      complete: Boolean,
      unnamed: List[Unnamed] = Nil
  ) {

    /** Whether a class of the program extends it. */
    def any: Boolean = found.nonEmpty || unnamed.nonEmpty

  }

  object Subclasses {

    /** No subclass, for sure. */
    val Empty: Subclasses = Subclasses(Nil, complete = true)

  }

  /** A local or anonymous class of a class without TASTy: its binary name, the classes it extends
    * that resolve, and the names of the methods an instance of it declares.
    */
  final case class Unnamed(name: String, parents: List[ClassSymbol], methods: Set[String])

  // The class a binary name stands for (`a/b/Outer$Inner`): a top-level class, then its member
  // classes, an object's among them.
  private def named(binary: String)(using Context): Option[ClassSymbol] = {
    val at = binary.lastIndexOf('/')
    val parts = binary.substring(at + 1).split('$').toList.filter(_.nonEmpty)
    val pkg = binary.take(at.max(0)).replace('/', '.')
    try
      parts match {
        case Nil         => None
        case top :: rest =>
          rest.foldLeft(Option(ctx.findStaticClass(if (pkg.isEmpty) top else s"$pkg.$top"))) {
            (outer, name) =>
              outer.flatMap { k =>
                (k :: k.companionClass.toList)
                  .flatMap(_.declarations)
                  .collectFirst {
                    case member: ClassSymbol if member.name.toString == name => member
                  }
              }
          }
      }
    catch { case NonFatal(_) => None }
  }

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
