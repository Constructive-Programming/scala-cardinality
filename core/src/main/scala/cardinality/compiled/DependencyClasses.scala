package cardinality.compiled

import scala.util.control.NonFatal

import tastyquery.Contexts.*
import tastyquery.Modifiers.{OpenLevel, TermSymbolKind}
import tastyquery.SourceLanguage
import tastyquery.Symbols.*

/** What a class of a dependency without TASTy is, read from its symbols: a Java class from its
  * class file's signatures, a Scala 2 class from its pickle. (A Scala 3 dependency's class has a
  * tree and is read as a class of the module is.) Nothing here reads a body, and nothing is
  * guessed: a dependency's concrete member is code that is not supplied, never a throw (a
  * capability only where a class of the program overrides it, `Receivers.dependency`), and what the
  * signatures do not say refuses.
  *
  * `classFile` answers what a Java class's class file holds and its symbol does not: the names of
  * its enum constants, and whether it is sealed (`CompiledClasses.javaFacts`).
  */
final private[compiled] class DependencyClasses(
    classFile: ClassSymbol => (List[String], Boolean)
)(using Context) {
  import DependencyClasses.*

  /** What the values of `c` are, or why its symbols do not say. */
  def of(c: ClassSymbol): Either[String, Declared] =
    try
      if (c.tree.nonEmpty) Left(s"a class with TASTy, read from its tree: ${c.displayFullName}")
      else if (c.sourceLanguage == SourceLanguage.Java) javaClass(c)
      else scala2Class(c)
    catch { case NonFatal(error) => Left(s"class not read: ${c.displayFullName} ($error)") }

  // A Java enum is its constants (a static field of its type is none); an interface is the record
  // of its abstract methods, unless it is sealed, whose permitted classes are its values; a class
  // keeps state its constructors set, which no signature ties to their parameters.
  private def javaClass(c: ClassSymbol): Either[String, Declared] = {
    val (constants, sealedClass) = classFile(c)
    if (c.isEnum)
      c.companionClass.toRight(s"a Java enum without its statics: ${c.displayFullName}").flatMap {
        statics =>
          val found = constants.flatMap(name =>
            statics.declarations.collectFirst {
              case constant: TermSymbol if constant.name.toString == name => constant
            }
          )
          Either.cond(
            constants.nonEmpty && found.sizeIs == constants.size,
            Declared.Sum(found),
            s"a Java enum whose constants are not read: ${c.displayFullName}"
          )
      }
    else if (sealedClass) Left(s"$SealedJava: ${c.displayFullName}")
    else if (c.isTrait) Right(record(c))
    else Left(s"$HiddenState: ${c.displayFullName}")
  }

  // A sealed trait or abstract class is its children, each a class or an object a symbol stands
  // for (the pickle records a local or anonymous child as the class itself: read as fewer
  // children it would be a smaller sum); a sealed class with instances of its own is not only
  // them. A case class is the fields of its primary constructor. A trait, or an abstract class
  // no constructor gives state, is the record of its abstract members.
  private def scala2Class(c: ClassSymbol): Either[String, Declared] =
    if (c.openLevel == OpenLevel.Sealed) {
      val children = c.sealedChildren
      if (!c.isTrait && !c.isAbstractClass) Left(s"$OwnInstances: ${c.displayFullName}")
      else
        Either.cond(
          children.nonEmpty && children.forall(child => child != c && named(child)),
          Declared.Sum(children),
          s"$UnnamedChild: ${c.displayFullName}"
        )
    } else if (c.isCaseClass && !c.isAbstractClass)
      primary(c).map(constructor => Declared.Fields(constructor, fields(constructor)))
    else if (c.isTrait || (c.isAbstractClass && constructors(c).forall(fields(_).isEmpty)))
      Right(record(c))
    else Left(s"$HiddenState: ${c.displayFullName}")

  private def named(child: Symbol): Boolean =
    !child.name.toString.contains("$") && World.owners(child.owner).forall {
      case _: ClassSymbol | _: PackageSymbol => true
      case _                                 => false
    }

  private def constructors(c: ClassSymbol): List[TermSymbol] =
    c.declarations.collect { case t: TermSymbol if World.isConstructor(t) => t }

  private def fields(constructor: TermSymbol): List[TermSymbol] =
    constructor.paramSymss.flatMap(_.left.toSeq.flatten)

  // A class without a tree has its constructors in no order: the primary is the one whose
  // parameters name every field a parameter sets, and when several do, none is chosen. It builds
  // the class's values only where it is public.
  private def primary(c: ClassSymbol): Either[String, TermSymbol] = {
    val accessors = c.declarations.collect { case t: TermSymbol if t.isParamAccessor => t.name }
    val all = constructors(c)
    val candidates =
      if (all.sizeIs <= 1) all
      else all.filter(k => accessors.toSet.subsetOf(fields(k).map(_.name).toSet))
    candidates match {
      case List(only) if only.isPublic => Right(only)
      case List(_) => Left(s"a class without a public primary constructor: ${c.displayFullName}")
      case _       => Left(s"no primary constructor among ${all.size}: ${c.displayFullName}")
    }
  }

  // The members a value of `c` answers, each as `c` resolves it: the abstract ones are what an
  // implementation chooses, the concrete ones code that is not supplied. A Java interface's statics
  // and constants are its companion's, and what `Object` declares is every value's.
  private def record(c: ClassSymbol): Declared.Record = {
    val line = c.linearization
    val members = World
      .ancestors(c)
      .prepended(c)
      .flatMap(_.declarations)
      .collect {
        case member: TermSymbol
            if !member.isPrivate && !World.isConstructor(member) &&
              member.kind != TermSymbolKind.Module &&
              member.matchingSymbol(defn.ObjectClass, c).isEmpty =>
          val matching = line.flatMap(k => member.matchingSymbol(k, c)).collect {
            case found: TermSymbol => found
          }
          // In Scala a concrete member always wins; a Java interface may declare an inherited
          // default method abstract again.
          matching
            .find(!_.isAbstractMember)
            .filter(concrete =>
              c.sourceLanguage != SourceLanguage.Java || matching.headOption.forall { first =>
                !first.isAbstractMember ||
                !first.owner.asClass.isSubClass(concrete.owner.asClass)
              }
            )
            .orElse(matching.headOption)
      }
      .flatten
      .distinct
    val (abstracts, concrete) = members.partition(_.isAbstractMember)
    Declared.Record(abstracts, concrete)
  }

}

private[compiled] object DependencyClasses {

  /** How the refusals start. */
  val HiddenState = "open class with hidden constructor state"
  val SealedJava = "a sealed Java class's permitted classes are not read"
  val UnnamedChild = "a sealed class with a child no declaration stands for"
  val OwnInstances = "a sealed class with instances of its own"

  /** What the values of a class are. */
  enum Declared {

    /** One of its cases: a Java enum's constants, a sealed class's children (classes and objects).
      */
    case Sum(cases: List[TermOrTypeSymbol])

    /** What its public primary constructor is given: its parameters, each a field. */
    case Fields(constructor: TermSymbol, fields: List[TermSymbol])

    /** No state: the choice of its abstract members. Its concrete members are code that is not
      * supplied, which a count may call and may not read (and which varies where a class of the
      * program overrides it).
      */
    case Record(abstracts: List[TermSymbol], concrete: List[TermSymbol])
  }

}
