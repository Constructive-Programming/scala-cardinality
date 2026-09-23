package cardinality

import scala.meta.*

import org.specs2.Specification

class MethodAnalysisSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Generic implementations
      choose between two supplied values                         $choose
      construct four pairs rather than one or two                 $pairs
      build products using independent type parameters            $independent
      reapply an endomorphism only with a starting inhabitant     $iteration
      compose a path from inputs to outputs                      $composition
      do not invent an inhabitant in an unproductive cycle        $unproductive
      keep a shadowed receiver field as its own choice            $shadowedField
      widen a method body's type aliases into their own scope     $bodyAlias
      keep a known identity from becoming an opaque omega         $knownIdentity
      fold a forward alias chain onto the value it names          $aliasChain
    Source-set environment
      capture enclosing values and callable producers             $captures
      preserve type binder identity under shadowing               $shadowing
      normalize a captured alias rather than inventing a value    $aliases
      resolve forward and cross-file aliases in two passes        $twoPass
      preserve qualification when simple names collide            $qualification
      do not claim a number for an incomplete environment         $incomplete
      stop type-resolution cycles with an unresolved diagnostic   $recursive
      do not project a second constructor parameter list          $hiddenField
      do not construct through a private constructor              $privateConstructor
      do not project an abstract case class as a product          $abstractCase
      report a package's concrete producers as unresolved         $concreteProducers
  """

  private def inputs(sources: (String, String)*): List[MethodAnalysis.Input] =
    sources.toList.map { (path, text) =>
      MethodAnalysis.Input(path, dialects.Scala3(text).parse[Source].get)
    }

  private def entry(code: String, name: String): MethodAnalysis.Entry =
    MethodAnalysis.analyze(inputs("Example.scala" -> code)).find(_.name == name).get

  def choose =
    entry("def choose[A](x: A, y: A): A = x", "choose").count === Count.Finite(2)

  def pairs = {
    val constructor = entry("case class Pair[A](x: A, y: A)", "Pair.<init>")
    val application = entry(
      "case class Pair[A](x: A, y: A)\ndef make[A](x: A, y: A): Pair[A] = Pair(x, y)",
      "make"
    )
    (constructor.count === Count.Finite(4)).and(application.count === Count.Finite(4))
  }

  def independent =
    entry("def pair[A, B](x: A, y: B): (A, B) = (x, y)", "pair").count === Count.Finite(1)

  def iteration =
    entry("def use[A](x: A, step: A => A): A = step(x)", "use").count === Count.Countable

  def composition =
    entry(
      "def compose[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))",
      "compose"
    ).count === Count.Finite(1)

  def shadowedField =
    entry("class Env[A](val x: A) { def pick(x: A): A = x }", "Env.pick").count === Count.Finite(2)

  def bodyAlias =
    entry(
      "class E[A] { type T = A; def pick(x: T, y: T): T = { type T = Unit; x } }",
      "E.pick"
    ).count === Count.Finite(2)

  def knownIdentity = {
    val pick = entry(
      "class E[A](seed: A) { def id(x: A): A = x; val step: A => A = id; def pick(): A = seed }",
      "E.pick"
    )
    (pick.count !== Count.Countable)
      .and(pick.count must beLike {
        case Count.Unresolved(reasons) =>
          reasons.exists(_.contains("step")) must beTrue
      })
  }

  def aliasChain =
    // `alias` and `later` name the same value as `seed`, so the choices are x, y and seed.
    entry(
      "class E[A](seed: A) { lazy val alias: A = later; lazy val later: A = seed; " +
        "def pick(x: A, y: A): A = x }",
      "E.pick"
    ).count === Count.Finite(3)

  def hiddenField =
    // The second parameter list is not part of the product, so `extract` has no accessible field.
    MethodAnalysis
      .analyze(
        inputs(
          "Example.scala" ->
            "case class Hidden[A]()(secret: A)\ndef extract[A](h: Hidden[A]): A = h._1"
        )
      )
      .find(_.name == "extract")
      .get
      .count must beLike { case Count.Unresolved(_) => ok }

  def privateConstructor =
    // The private constructor stays internal to the class, so an outside caller has no way to
    // build one: `make` has no construction available and must not claim a number.
    MethodAnalysis
      .analyze(
        inputs(
          "Example.scala" ->
            "case class Secret[A] private (value: A)\ndef make[A](x: A): Secret[A] = x"
        )
      )
      .find(_.name == "make")
      .get
      .count must beLike { case Count.Unresolved(_) => ok }

  def abstractCase =
    MethodAnalysis
      .analyze(
        inputs(
          "Example.scala" ->
            "abstract case class Holder[A]() { val value: A }\ndef extract[A](h: Holder[A]): A = h.value"
        )
      )
      .map(_.count)
      .forall(_.isInstanceOf[Count.Unresolved]) must beTrue

  def concreteProducers = {
    val flagged = MethodAnalysis
      .analyze(
        inputs(
          "Example.scala" ->
            "object Defaults { val flag: Boolean = true }\ndef pick(b: Boolean): Boolean = b"
        )
      )
      .find(_.name == "pick")
      .get
    val generic = MethodAnalysis
      .analyze(
        inputs(
          "Example.scala" ->
            "object Defaults { val flag: Boolean = true }\ndef pick[A](x: A, y: A): A = x"
        )
      )
      .find(_.name == "pick")
      .get
    (flagged.count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("Defaults")) must beTrue
    }).and(generic.count === Count.Finite(2))
  }

  def unproductive =
    entry("def impossible[A](step: A => A): A = ???", "impossible").count === Count.Finite(0)

  def captures = {
    val choose = entry("class Env[A](captured: A) { def choose(x: A, y: A): A = x }", "Env.choose")
    val step = entry("class Env[A](step: A => A) { def use(x: A): A = step(x) }", "Env.use")
    (choose.count === Count.Finite(3))
      .and(choose.captures === List("captured"))
      .and(step.count === Count.Countable)
  }

  def shadowing =
    entry(
      "class Env[A](captured: A) { def choose[A](x: A, y: A): A = x }",
      "Env.choose"
    ).count === Count.Finite(2)

  def aliases =
    entry(
      "class Env[A](captured: A) { val alias: A = captured; def choose(x: A): A = x }",
      "Env.choose"
    ).count === Count.Finite(2)

  def twoPass = {
    val source = inputs(
      "a.scala" -> "package p\ndef select[A](x: Twice[A]): A = x._1",
      "z.scala" -> "package p\ntype Twice[A] = (A, A)"
    )
    val forward = MethodAnalysis.analyze(source).find(_.name == "p.select").get
    (forward.count === Count.Finite(2))
      .and(MethodAnalysis.analyze(source.reverse) === MethodAnalysis.analyze(source))
  }

  def qualification = {
    val source = inputs(
      "a.scala" -> "package a\ntype Choice[A] = A",
      "b.scala" -> "package b\ntype Choice[A] = (A, A)",
      "m.scala" -> "package c\ndef select[A](x: b.Choice[A]): A = x._1"
    )
    MethodAnalysis.analyze(source).find(_.name == "c.select").get.count === Count.Finite(2)
  }

  def incomplete = {
    val representation = entry("opaque type Id[A] = A\ndef choose[A](x: Id[A]): A = ???", "choose")
    val context = entry("def choose[A: Ordering](x: A, y: A): A = x", "choose")
    val capability = entry("def choose[A](x: A)(using ev: Unknown[A]): A = x", "choose")
    (representation.count must beLike { case Count.Unresolved(_) => ok })
      .and(context.count must beLike { case Count.Unresolved(_) => ok })
      .and(capability.count must beLike { case Count.Unresolved(_) => ok })
  }

  def unrelatedImport =
    // An import that cannot hold this method's type parameter cannot add a value of it: a total
    // parametric body builds its result from what it already has.
    entry("import external.*\ndef choose[A](x: A, y: A): A = x", "choose").count === Count.Finite(2)

  def recursive =
    entry(
      "type Loop[A] = Loop[A]\ndef choose[A](x: Loop[A]): A = ???",
      "choose"
    ).count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("recursive type")) must beTrue
    }

}
