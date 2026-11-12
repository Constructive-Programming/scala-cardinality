package cardinality.compiled

import cardinality.compiled.DependencyClasses.Declared
import cardinality.tasty.CompiledTypes
import org.specs2.Specification
import tastyquery.Contexts.*

/** A dependency's classes without TASTy, read from their symbols (`DependencyClasses`): the Java
  * classes of `javafixtures` and two of scalameta's Scala 2 jars, the dependencies
  * `DependencyRowsSpec` pins on rows. Each answer is derived from the class's source.
  */
class DependencyClassesSpec extends Specification {

  def is = s2"""
    A Java class
      an enum is its constants, a static field of its type is none     $javaEnum
      an interface is the record of its abstract methods               $javaInterface
      a class's state and a sealed interface's subclasses are not read $javaHidden
    A Scala 2 class
      a trait is the record of its abstract members                    $scala2Record
      a case class is the fields of its primary constructor            $scala2Fields
      a sealed class with a child the pickle does not declare refuses  $scala2Sealed
    A class with TASTy is its tree's to read                           $withTasty
  """

  private val scala2Jars = List(
    Programs.jarOf(classOf[scala.meta.io.AbsolutePath]),
    Programs.jarOf(classOf[scala.meta.internal.SingletonReference[?]])
  )

  private lazy val loaded =
    Programs.loaded("class Mine", Programs.packaged("javafixtures") :: scala2Jars)

  private def of(name: String): Either[String, Declared] = {
    given Context = loaded.context
    new DependencyClasses(CompiledTypes.javaFacts(loaded.classes)).of(ctx.findStaticClass(name))
  }

  private def names(symbols: List[tastyquery.Symbols.Symbol]): List[String] =
    symbols.map(_.name.toString)

  // `Coin` declares HEADS and TAILS, and a static `DEFAULT` of its own type, which is a field
  // and no third constant. `Op`'s constants have bodies and are still two; `Box.Size` is nested.
  def javaEnum =
    (of("javafixtures.Coin") must beRight(beLike[Declared] {
      case Declared.Sum(cases) => names(cases) must_== List("HEADS", "TAILS")
    }))
      .and(of("javafixtures.Op") must beRight(beLike[Declared] {
        case Declared.Sum(cases) => cases must haveSize(2)
      }))
      .and(of("javafixtures.Box.Size") must beRight(beLike[Declared] {
        case Declared.Sum(cases) => names(cases) must_== List("SMALL", "LARGE")
      }))

  // `Toggle` declares an abstract `flip`, a default `same`, a static `identity` and a constant
  // `LIMIT`: a value is the choice of `flip`; `same` is code beside it; the statics are the
  // companion's. `Sig.AnyFn` has the one method `apply`.
  def javaInterface =
    (of("javafixtures.Toggle") must beRight(beLike[Declared] {
      case Declared.Record(abstracts, concrete) =>
        (names(abstracts) must_== List("flip")).and(names(concrete) must_== List("same"))
    })).and(of("javafixtures.Sig.AnyFn") must beRight(beLike[Declared] {
      case Declared.Record(abstracts, Nil) => names(abstracts) must_== List("apply")
    }))

  // `Box` and `Box.Point` set fields in constructor bodies, which no signature ties to the
  // parameters; `Shape` extends `Box`. `Choice` is sealed: its values are those of the classes it
  // permits, which the symbols do not list.
  def javaHidden =
    (List("Box", "Box.Point", "Shape").map(n => of(s"javafixtures.$n")) must contain(
      beLeft(startWith(DependencyClasses.HiddenState))
    ).forall).and(of("javafixtures.Choice") must beLeft(startWith(DependencyClasses.SealedJava)))

  // `OutputStreamIO[T]` declares the one abstract method `write`.
  def scala2Record =
    of("scala.meta.internal.io.OutputStreamIO") must beRight(beLike[Declared] {
      case Declared.Record(abstracts, _) => names(abstracts) must_== List("write")
    })

  // `Classpath` is `final case class Classpath(entries: List[AbsolutePath])`.
  def scala2Fields =
    of("scala.meta.io.Classpath") must beRight(beLike[Declared] {
      case Declared.Fields(_, fields) => names(fields) must_== List("entries")
    })

  // `AbsolutePath` is a sealed case class whose one child is anonymous (`new AbsolutePath(p)
  // {}`): read as the sum of its declared children it would have no value.
  def scala2Sealed =
    of("scala.meta.io.AbsolutePath") must beLeft(startWith(DependencyClasses.UnnamedChild))

  // `Option` is compiled by Scala 3: its tree says what it is.
  def withTasty = of("scala.Option") must beLeft(startWith("a class with TASTy"))

}
