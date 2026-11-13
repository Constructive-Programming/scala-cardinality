package cardinality.compiled

import cardinality.compiled.ClosedProgram.{Overriding, Subclasses}
import java.nio.file.Path
import org.specs2.Specification
import tastyquery.Contexts.*
import tastyquery.Symbols.*

/** The closed world over symbols (`ClosedProgram`; AGENTS.md, "The modelling assumption: a closed
  * world"). Each fixture is compiled by scalac, and each answer is derived in the comment beside it
  * from the classes the program holds.
  */
class ClosedProgramSpec extends Specification {

  def is = s2"""
    A class is final
      when it is concrete and no class of the program extends it       $concrete
      never by a name another class shares                             $sameName
      not when an anonymous, a local or a given instance extends it    $unnamed
      a sealed class by its children, an anonymous one included        $sealedClasses
    A member is open
      when a subclass declares it, by signature and not by name        $overridden
      when a parent mixed in beside its class brings it to a subclass  $mixedIn
      not by a private, a final or a mixed-in abstract declaration     $notOverridden
    The dependencies are part of the program
      a class of one may extend or override another's                  $dependencies
      a question about the module's own class does not read them       $lazily
      a class that does not load leaves no answer about them sure      $unloaded
      a parent that is not read hides no class of the module           $unreadParent
      a class without TASTy is extended by what its class files name  $java
    The Scala library and the JDK
      the library is listed like a dependency, a sealed class by its children $library
      the JDK is not listed: only what is written closes a class of it $jdk
  """

  // A program's index, and its classes by name: a qualified name is a class of the classpath, any
  // other the one class of the module with that simple name, wherever it is declared.
  final private class Program(loaded: Programs.Loaded, dependencies: Boolean = true) {
    given context: Context = loaded.context

    val closed = new ClosedProgram(
      loaded.own,
      loaded.unloaded,
      if (dependencies) loaded.dependencies else sys.error("the dependencies are not read"),
      cardinality.tasty.CompiledTypes.unnamed(loaded.classes, ofModule = true),
      if (dependencies) cardinality.tasty.CompiledTypes.unnamed(loaded.classes, ofModule = false)
      else sys.error("the dependencies are not read")
    )

    def cls(name: String): ClassSymbol =
      if (name.contains('.')) ctx.findStaticClass(name)
      else
        loaded.own.flatMap(_.tree).flatMap(World.classes).map(_.symbol).distinct.filter {
          _.name.toString == name
        } match {
          case List(only) => only
          case found      => sys.error(s"$name names ${found.size} classes of the module")
        }

    // The one member of the name a class declares, or with `parameter` the overload whose only
    // parameter is of that class.
    def member(owner: String, name: String, parameter: String = ""): TermOrTypeSymbol =
      cls(owner).declarations.filter(_.name.toString == name).filter {
        case method: TermSymbol if parameter.nonEmpty =>
          method.signature.toString.takeWhile(_ != ')').contains(parameter)
        case _ => true
      } match {
        case List(only) => only
        case found      => sys.error(s"$owner.$name names ${found.size} members")
      }

    def subclasses(name: String): (Set[String], Boolean) = {
      val Subclasses(found, complete, _) = closed.subclasses(cls(name))
      (found.map(_.name.toString).toSet, complete)
    }

    def isFinal(names: String*): List[Boolean] = names.toList.map(n => closed.finalClass(cls(n)))

    def overriding(owner: String, in: String, name: String, parameter: String = ""): Overriding =
      closed.overriding(cls(owner), member(in, name, parameter))

  }

  private def program(code: String, classpath: List[Path] = Nil, read: Option[List[Path]] = None) =
    new Program(Programs.loaded(code, classpath, read))

  // `Leaf` is concrete and nothing extends it: final. `Base` is extended by `Leaf`. A concrete
  // class written `open` or `sealed` that nothing extends is final too (AGENTS.md, the closed
  // world: the supplied sources are the whole program). A trait and an abstract class are not
  // closed by having no subclass: an abstract one has no instance of its own. An object
  // is one instance. `Hidden` has a private constructor and nothing extends it: only its scope
  // could, and that scope is here.
  def concrete = {
    val p = program("""class Base
      |class Leaf extends Base
      |trait Shape
      |abstract class Partial
      |open class Wide
      |sealed class Kept
      |object Single
      |final class Written
      |class Hidden private (val n: Int)
      |""".stripMargin)
    (p.isFinal("Leaf", "Single$", "Written", "Hidden", "Wide", "Kept") must_== List.fill(6)(true))
      .and(p.isFinal("Base", "Shape", "Partial") must_== List.fill(3)(false))
      .and(p.subclasses("Base") must_== (Set("Leaf"), true))
  }

  // Two classes are both named `Base`. `Sub` extends `one.Base`, so that one is open and its `f`,
  // which `Sub` overrides, is too; `two.Base` has no subclass, and its `f` no overrider.
  def sameName = {
    val p = program("""package one { class Base { def f(x: Int): Int = x } }
      |package two { class Base { def f(x: Int): Int = x } }
      |package three { class Sub extends one.Base { override def f(x: Int): Int = 0 } }
      |""".stripMargin)
    given Context = p.context
    val (one, two) = (ctx.findStaticClass("one.Base"), ctx.findStaticClass("two.Base"))
    def f(owner: ClassSymbol) = owner.declarations.find(_.name.toString == "f").get
    (p.closed.finalClass(one) must beFalse)
      .and(p.closed.finalClass(two) must beTrue)
      .and(p.closed.overriding(one, f(one)) must_== Overriding.Supplied)
      .and(p.closed.overriding(two, f(two)) must_== Overriding.No)
  }

  // An anonymous class, a class local to a method and a given instance each extend the class
  // they name, and are classes of the program like any other.
  def unnamed = {
    val p = program("""class Anon { def f: Int = 0 }
      |class Local
      |trait Given
      |class Free
      |object Uses {
      |  val a: Anon = new Anon { override def f: Int = 1 }
      |  def m: Local = { class Inner extends Local; new Inner }
      |  given g: Given with {}
      |}
      |""".stripMargin)
    (p.isFinal("Anon", "Local", "Free") must_== List(false, false, true))
      .and(p.subclasses("Local")._1 must_== Set("Inner"))
      .and(p.subclasses("Given")._1 must haveSize(1))
      .and(p.overriding("Anon", "Anon", "f") must_== Overriding.Supplied)
  }

  // `Kind`'s children are `One` and `Two`, and `Two` is open: `Deep` extends it. `Odd` has an
  // anonymous instance, which overrides `f`: it is found, though the compiler's list of children
  // names no class for it.
  def sealedClasses = {
    val p = program("""sealed trait Kind
      |final class One extends Kind
      |class Two extends Kind
      |class Deep extends Two
      |sealed trait Odd { def f: Int = 0 }
      |object Uses { val odd: Odd = new Odd { override def f: Int = 1 } }
      |""".stripMargin)
    (p.subclasses("Kind") must_== (Set("One", "Two", "Deep"), true))
      .and(p.overriding("Odd", "Odd", "f") must_== Overriding.Supplied)
  }

  // `Sub` declares `f(x: Int)` and `X`: those members of `Base` are open. It declares no
  // `f(s: String)` and no `g`: the other overload and `g` stay closed, though the name `f` is
  // declared below.
  def overridden = {
    val p = program("""class Base {
      |  type X <: Any
      |  def f(x: Int): Int = x
      |  def f(s: String): Int = 0
      |  def g: Int = 0
      |}
      |class Sub extends Base { type X = Int; override def f(x: Int): Int = 1 }
      |""".stripMargin)
    (p.overriding("Base", "Base", "f", "Int") must_== Overriding.Supplied)
      .and(p.overriding("Base", "Base", "f", "String") must_== Overriding.No)
      .and(p.overriding("Base", "Base", "g") must_== Overriding.No)
      .and(p.overriding("Base", "Base", "X") must_== Overriding.Supplied)
  }

  // `Mixed` is an `Owner` and declares nothing, but its `m` is `Side.m`: `Side` comes before
  // `Owner` in its linearization (Mixed, Side, Owner, Root). So the `m` an `Owner` inherits is
  // open, though `Side` does not extend `Owner` and no subclass of `Owner` declares `m`. Asked of
  // `Root`, `m` is open through `Side` itself.
  def mixedIn = {
    val p = program("""trait Root { def m: Int = 0 }
      |class Owner extends Root
      |trait Side extends Root { override def m: Int = 1 }
      |class Mixed extends Owner with Side
      |""".stripMargin)
    (p.overriding("Owner", "Root", "m") must_== Overriding.Supplied)
      .and(p.overriding("Root", "Root", "m") must_== Overriding.Supplied)
  }

  // A private member is not inherited and a final one cannot be overridden: `Sub`'s own `p` is
  // another member. `Both` mixes in `Wants`, whose `m` is abstract: the `m` of a `Both` is still
  // `Holds.m`. `Again` extends `Holds` and declares `m` abstract: there it is to be implemented.
  def notOverridden = {
    val p = program("""class Base { private def p: Int = 0; final def q: Int = 0 }
      |class Sub extends Base { def p: Int = 1 }
      |class Holds { def m: Int = 1 }
      |trait Wants { def m: Int }
      |class Both extends Holds with Wants
      |class Kept { def m: Int = 1 }
      |abstract class Again extends Kept { override def m: Int }
      |""".stripMargin)
    (p.overriding("Base", "Base", "p") must_== Overriding.No)
      .and(p.overriding("Base", "Base", "q") must_== Overriding.No)
      .and(p.overriding("Holds", "Holds", "m") must_== Overriding.No)
      .and(p.overriding("Kept", "Kept", "m") must_== Overriding.Supplied)
  }

  private val dependencyCode = """package dep
    |class Base { def f(x: Int): Int = x; def g: Int = 0 }
    |class Sub extends Base { override def f(x: Int): Int = 0 }
    |class Plain { def h: Int = 0 }
    |""".stripMargin

  // `dep.Sub` extends `dep.Base` and overrides `f` in the dependency, and `Mine` extends
  // `dep.Plain` in the module: the three are one program.
  def dependencies = {
    val dependency = Programs.compiled(dependencyCode)
    val p = program("class Mine extends dep.Plain { override def h: Int = 1 }", List(dependency))
    (p.subclasses("dep.Base") must_== (Set("Sub"), true))
      .and(p.overriding("dep.Base", "dep.Base", "f") must_== Overriding.Supplied)
      .and(p.overriding("dep.Base", "dep.Base", "g") must_== Overriding.No)
      .and(p.subclasses("dep.Plain") must_== (Set("Mine"), true))
      .and(p.overriding("dep.Plain", "dep.Plain", "h") must_== Overriding.Supplied)
      .and(p.isFinal("dep.Sub", "dep.Base", "Mine") must_== List(true, false, true))
  }

  // A dependency is compiled before the module, so none of its classes extends `Mine` or
  // `Other`: asking of them does not list the dependencies, which here would fail.
  def lazily = {
    val dependency = Programs.compiled(dependencyCode)
    val p = new Program(
      Programs.loaded("class Mine extends dep.Plain; class Other", List(dependency)),
      dependencies = false
    )
    (p.isFinal("Mine", "Other") must_== List(true, true))
      .and(
        p.closed.subclasses(p.cls("dep.Plain")) must
          throwA[RuntimeException]("the dependencies are not read")
      )
  }

  // `late.Late` extends `gone.Gone`, which the classes read do not hold: its parents are not
  // read, and it may extend `dep.Base` for all that is known. So `dep.Base` and `dep.Plain` are
  // not known to be final and their members not known to be closed; what is found stays found.
  // The module's own `Mine` is not a dependency's to extend: it stays final.
  def unloaded = {
    val dependency = Programs.compiled(dependencyCode)
    val gone = Programs.compiled("package gone\nclass Gone")
    val late = Programs.compiled("package late\nclass Late extends gone.Gone", List(gone))
    val p = program("class Mine", List(dependency, late))
    (p.subclasses("dep.Base") must_== (Set("Sub"), false))
      .and(p.isFinal("dep.Plain", "dep.Sub", "Mine") must_== List(false, false, true))
      .and(p.overriding("dep.Base", "dep.Base", "g") must_== Overriding.Unknown)
      .and(p.overriding("dep.Base", "dep.Base", "f") must_== Overriding.Supplied)
  }

  // A class without TASTy may be extended by a local or an anonymous class no symbol stands
  // for, which its class file names: `Lone.off` returns `Lone$1`, which extends `Lone` and
  // declares `on`. So `Lone` is not final and its `on` is overridden. Nothing extends `Solo`,
  // named or not: it is final and its `on` closed, as a Scala class's would be. `Box.Point` is
  // written final, `Shape` extends `Box`, `Choice.Yes` and `Choice.No` implement `Choice`, and
  // nothing overrides the default method `Toggle.same` (no class file of the fixtures extends
  // `Toggle`: `Lone` and `Solo` are the checks a lost class file would fail). `Stray$1` and
  // `Stray$1Local` extend `javamissing.Gone`, which the package is not read with: a class outside
  // the program, which extends none of its classes, so every other answer stays sure (`Lone`'s
  // subclasses are all known). `Stray$1Local` implements `Dial`, and `Gone`, not read, may
  // override `Dial.same` there: not known. `Gen$1` extends `Gen$mcI$sp`, named as a Scala 2 specialization, which no symbol stands
  // for: it extends `Gen` and may override all `Gen` has, `off` too, which it does not declare.
  // `Wid$Impl$1` overrides `f` of `Wid$Impl`, a top-level class whose name holds a `$`: its parent
  // is on the classpath, so never outside the program (it read as absent `Wid`: `f` was final).
  def java = {
    val p = program("class Mine", List(Programs.packaged("javafixtures")))
    val names = List("Lone", "Solo", "Box.Point", "Box", "Choice").map("javafixtures." + _)
    def unnamed(name: String) = p.closed.subclasses(p.cls(name)).unnamed.map(_.name)
    (p.isFinal(names*) must_== List(false, true, true, false, false))
      .and(p.subclasses("javafixtures.Lone") must_== (Set(), true))
      .and(unnamed("javafixtures.Lone") must_== List("javafixtures/Lone$1"))
      .and(p.overriding("javafixtures.Lone", "javafixtures.Lone", "on") must_== Overriding.Supplied)
      .and(p.overriding("javafixtures.Solo", "javafixtures.Solo", "on") must_== Overriding.No)
      .and(p.overriding("javafixtures.Toggle", "javafixtures.Toggle", "same") must_== Overriding.No)
      .and(p.subclasses("javafixtures.Box") must_== (Set("Shape"), true))
      .and(p.subclasses("javafixtures.Choice") must_== (Set("Yes", "No"), true))
      .and(unnamed("javafixtures.Gen") must_== List("javafixtures/Gen$1"))
      .and(p.overriding("javafixtures.Gen", "javafixtures.Gen", "off") must_== Overriding.Supplied)
      .and(unnamed("javafixtures.Dial") must_== List("javafixtures/Stray$1Local"))
      .and(
        p.overriding("javafixtures.Dial", "javafixtures.Dial", "same") must_== Overriding.Unknown
      )
      .and(
        p.overriding(
          "javafixtures.Wid$Impl",
          "javafixtures.Wid$Impl",
          "f"
        ) must_== Overriding.Supplied
      )
  }

  // `Mid` extends `gone.Gone`, which the classes read do not hold. `Leaf` and `Leaf2` are still
  // found under `Mid`, so `Mid` is open. `Last` mixes `Over` in after `Late`, whose own line is
  // not whole: `Over.r` overrides `Late.r` there. `Mixed` mixes in `gone.Mix`, which may override
  // the `m` it inherits from `Owner`: that is not known. `Under` reaches `gone.Gone` only through
  // `Above`, where it is above `Above` and overrides nothing of it. `Free` has nothing to do with
  // any of them, and no dependency extends a class of the module: it stays final.
  def unreadParent = {
    val gone = Programs.compiled("package gone\nclass Gone\ntrait Mix")
    val p = program(
      """class Mid extends gone.Gone
        |class Leaf extends Mid
        |class Leaf2 extends Leaf
        |trait Root { def r: Int }
        |class Late extends Root with gone.Mix { def r: Int = 1 }
        |trait Over extends Root { override def r: Int = 2 }
        |class Last extends Late with Over
        |class Owner { def m: Int = 0 }
        |class Mixed extends Owner with gone.Mix
        |class Above extends gone.Gone { def a: Int = 0 }
        |class Under extends Above
        |class Free { def n: Int = 0 }
        |""".stripMargin,
      List(gone),
      read = Some(Nil)
    )
    (p.subclasses("Mid") must_== (Set("Leaf", "Leaf2"), true))
      .and(p.overriding("Late", "Late", "r") must_== Overriding.Supplied)
      .and(p.isFinal("Mid", "Free") must_== List(false, true))
      .and(p.overriding("Owner", "Owner", "m") must_== Overriding.Unknown)
      .and(p.overriding("Above", "Above", "a") must_== Overriding.No)
      .and(p.overriding("Free", "Free", "n") must_== Overriding.No)
  }

  // The library's classes are on the classpath like any dependency's. `Option` is sealed: its
  // children are `Some` and `None`, known without listing anything. `Function1` is extended in
  // the library (`PartialFunction`) and by `Mine`.
  def library = {
    val p = program("class Mine extends (Int => Int) { def apply(x: Int): Int = x }")
    (p.subclasses("scala.Option") must_== (Set("Some", "None$"), true))
      .and(p.isFinal("scala.Some") must_== List(true))
      .and(p.subclasses("scala.Function1")._1 must contain("Mine", "PartialFunction"))
      .and(p.subclasses("scala.Function1")._2 must beTrue)
  }

  // `String` is written final. `Number` is not: `Mine` and the library's `BigInt` extend it, and the JDK's own subclasses
  // (`Integer`) are not listed, so the answer is not complete and its members are not known to
  // be closed. `Thread` has no subclass in the program, and is still not known to be final.
  def jdk = {
    val p = program("""class Mine extends java.lang.Number {
      |  def intValue: Int = 0; def longValue: Long = 0; def floatValue: Float = 0; def doubleValue: Double = 0
      |}""".stripMargin)
    (p.isFinal("java.lang.String", "java.lang.Number", "java.lang.Thread") must_==
      List(true, false, false))
      .and(p.subclasses("java.lang.Number")._1 must contain("Mine", "BigInt"))
      .and(p.subclasses("java.lang.Number")._2 must beFalse)
      .and(
        p.overriding("java.lang.Thread", "java.lang.Thread", "getId") must_==
          Overriding.Unknown
      )
      .and(
        p.overriding("java.lang.Number", "java.lang.Number", "intValue") must_==
          Overriding.Supplied
      )
  }

}
