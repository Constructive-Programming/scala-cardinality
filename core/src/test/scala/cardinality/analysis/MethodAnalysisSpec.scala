package cardinality.analysis

import scala.meta.*

import cardinality.analysis.inhabitation.*
import cardinality.capacity.{EffectiveOmega, IntSize}
import org.specs2.Specification

class MethodAnalysisSpec extends Specification {
  import Inhabitation.Count

  def is = s2"""
    Generic implementations
      choose between two supplied values                         $choose
      construct four pairs rather than one or two                 $pairs
      multiply a constructor's parameter spaces                  $constructorSpaces
      build products using independent type parameters            $independent
      reapply an endomorphism only with a starting inhabitant     $iteration
      compose a path from inputs to outputs                      $composition
      do not invent an inhabitant in an unproductive cycle        $unproductive
      keep a shadowed receiver field as its own choice            $shadowedField
      widen a method body's type aliases into their own scope     $bodyAlias
      keep a known identity from becoming an opaque omega         $knownIdentity
      fold a forward alias chain onto the value it names          $aliasChain
    Kind slots
      let a kind slot pass through unchanged                      $kindSlotIdentity
      keep an opaque kind slot from inventing a payload           $kindSlotZero
      let a kind slot enter the option's empty case               $kindSlotOption
      keep reordered kind arguments distinct slots                $kindSlotArguments
      let a kind-slot function be applied repeatedly              $kindSlotCycle
      apply a function over kind slots                            $kindSlotApplied
      refuse a kind the caller may not choose freely              $kindBoundedRefused
      refuse a kind named without applying it                     $kindBareRefused
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
      count a declared slot as a supplied value                   $declaredSlot
      let a declared callable be applied repeatedly               $declaredCallable
      count an inherited declared slot                            $inheritedSlot
      ignore a concrete method whose value the scope already has  $concreteMethod
      read an enum the source set defines                         $sourceEnum
      read a sealed family as the sum it enumerates               $sealedSum
      read a sealed family's covariant pin and mirror binders     $sealedFamilyWithBinder
      read an empty sealed enumeration as no values               $sealedEmpty
      refuse a sealed family an open subtype keeps growing         $sealedOpenSubtype
      refuse a sealed family an open abstract intermediate         $sealedOpenIntermediate
      refuse a builtin an import could shadow                     $shadowedBuiltin
      bind an imported type from a sibling package               $importedType
      anchor a package-relative import prefix                    $relativeImport
      bind a renamed import, not the name it leaves behind        $renamedImport
      bind the types a wildcard import opens                     $wildcardImport
      bind a companion wildcard across files                     $companionImport
      deny the builtin a type import shadows                     $importedBuiltin
      bind names inside a transparent alias at its use site       $aliasImportPosition
      keep an import after the reference from binding it          $importOrder
      keep a nested module's binders in scope                     $nestedModule
      let a subpackage sit inside its parent's grant              $packagePrivateSubpackage
      read a package-private abstraction's own reason             $packagePrivateType
      keep a file-private type from a sibling file                $filePrivateType
      read a package-private product from a sibling file          $packagePrivateProduct
  """

  private def inputs(sources: (String, String)*): List[MethodAnalysis.Input] =
    sources.toList.map { (path, text) =>
      MethodAnalysis.Input(path, dialects.Scala3(text).parse[Source].get)
    }

  private def entry(code: String, name: String): MethodAnalysis.Entry =
    MethodAnalysis.analyze(inputs("Example.scala" -> code)).find(_.name == name).get

  def choose =
    entry("def choose[A](x: A, y: A): A = x", "choose").count === Count.Finite(2)

  def kindSlotIdentity =
    entry(
      "trait Box[F[_]]:\n  def id[A](fa: F[A]): F[A] = ???",
      "Box.id"
    ).count === Count.Finite(1)

  def kindSlotZero =
    // The same shape the reviewed CanGet.get answers 0: `fa` is an opaque carrier, and no
    // route out of it exists for the model to count — here seen through the binder F.
    entry(
      "trait Box[F[_, _]]:\n  def get[X, A](fa: F[X, A]): A = ???",
      "Box.get"
    ).count === Count.Finite(0)

  def kindSlotOption =
    entry(
      "trait Box[F[_]]:\n  def opt[A](fa: F[A]): Option[A] = ???",
      "Box.opt"
    ).count === Count.Finite(1)

  def kindSlotArguments =
    // F[X, A] and F[A, X] are different slots: no construction reaches one from the other
    // before the caller's F is known, and `F = (a, b) => (b, a)` would betray that.
    entry(
      "trait Box[F[_, _]]:\n  def flip[X, A](fa: F[X, A]): F[A, X] = ???",
      "Box.flip"
    ).count === Count.Finite(0)

  def kindSlotCycle =
    entry(
      "trait Box[F[_]]:\n  def twice[A](fa: F[A], k: F[A] => F[A]): F[A] = ???",
      "Box.twice"
    ).count === Count.Countable

  def kindSlotApplied =
    entry(
      "trait Box[F[_]]:\n  def app[X, A](fk: F[A] => X, fa: F[A]): X = ???",
      "Box.app"
    ).count === Count.Finite(1)

  def kindBoundedRefused = {
    // A context bound restricts which type the caller may supply — the model would need the
    // evidence's capabilities to count within it, so the binder stays a refusal.
    entry(
      "trait Box[F[_]: Ordering]:\n  def get[A](fa: F[A]): A = ???",
      "Box.get"
    ).count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("bounded or higher-kinded parameter F[_]: Ordering")) must beTrue
    }
  }

  def kindBareRefused =
    // Named without being applied, the kind cannot become a type this scope reads: the
    // enclosing type's argument stays unresolved instead of inventing a slot.
    entry(
      "trait Box2[T]\ntrait Box[F[_]]:\n  def bad[A](x: Box2[F]): A = ???",
      "Box.bad"
    ).count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("bounded or higher-kinded parameter F[_]")) must beTrue
    }

  def pairs = {
    val constructor = entry("case class Pair[A](x: A, y: A)", "Pair.<init>")
    val application = entry(
      "case class Pair[A](x: A, y: A)\ndef make[A](x: A, y: A): Pair[A] = Pair(x, y)",
      "make"
    )
    (constructor.count === Count.Finite(4)).and(application.count === Count.Finite(4))
  }

  def constructorSpaces = {
    // A constructor's count is the product of its parameters' value spaces, read in the scope its
    // own source defines: an `Int` is a `2^32` capacity, a sealed family the sum of its cases, and
    // a `=>` hole countable. A parameter the estimate cannot read whole keeps the shape model's
    // diagnostic instead of a number resting on an unknown name.
    val holder = entry("case class Holder[A](value: A, count: Int)", "Holder.<init>")
    val succ = entry(
      "sealed trait Nat\ncase object Zero extends Nat\ncase class Succ(n: Nat) extends Nat",
      "Succ.<init>"
    )
    val gate = entry("case class Gate(q: Quorum)", "Gate.<init>")
    (holder.count === Count.Bounded(IntSize))
      .and(holder.count.render === "2^32")
      .and(succ.count === Count.Bounded(EffectiveOmega))
      .and(gate.count must beLike {
        case Count.Unresolved(reasons) =>
          reasons.exists(_.contains("Quorum")) must beTrue
      })
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
    // `step` is an inhabitant of `A => A`, and a total parametric inhabitant of that type is the
    // identity (the free theorem): applying it yields values the scope already has, so the choices
    // are still just `seed`. Not ω, and no longer unknown.
    val pick = entry(
      "class E[A](seed: A) { def id(x: A): A = x; val step: A => A = id; def pick(): A = seed }",
      "E.pick"
    )
    (pick.count === Count.Finite(1)).and(pick.count !== Count.Countable)
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

  def declaredSlot =
    // `seed` is not implemented here: the caller supplies it, so it is a value this scope has.
    entry("abstract class Env[A] { def seed: A\ndef pick(): A = seed }", "Env.pick").count === Count
      .Finite(1)

  def declaredCallable =
    // `step` is supplied too, and nothing in its type says it is the identity: it can be applied
    // to its own result, which is a countable family.
    entry(
      "abstract class Env[A] { def step(a: A): A\ndef pick(x: A): A = step(x) }",
      "Env.pick"
    ).count === Count.Countable

  def inheritedSlot =
    entry(
      "trait Base[A] { val seed: A }\nabstract class Env[A] extends Base[A] " +
        "{ def pick(x: A): A = x }",
      "Env.pick"
    ).count === Count.Finite(2)

  def concreteMethod =
    // A total parametric method is a specific inhabitant of its type, and what it can compute is
    // already in the environment: `id` adds nothing that x and y do not already supply.
    entry(
      "class Env[A](x: A, y: A) { def id(a: A): A = a\ndef pick(): A = x }",
      "Env.pick"
    ).count === Count.Finite(2)

  def sourceEnum =
    entry("enum Unit { case One, Two, Three }\ndef pick(): Unit = Unit.One", "pick").count === Count
      .Finite(3)

  def shadowedBuiltin =
    // `import Types.Option` puts a different type behind the name: the binding resolves, and
    // the model never reads the builtin it shadows. Here the alias is `(A, A)`, so `pick`
    // counts its two projections — the answer the import, not the builtin, implies.
    MethodAnalysis
      .analyze(
        inputs(
          "types.scala" -> "object Types { type Option[A] = (A, A) }",
          "use.scala" -> "import Types.Option\ndef pick[A](x: Option[A]): A = x._1"
        )
      )
      .find(_.name == "pick")
      .get
      .count === Count.Finite(2)

  def nestedModule =
    entry(
      "class Env[A](seed: A) { object Inner { val get: A = seed }\ndef pick(): A = Inner.get }",
      "Env.pick"
    ).count === Count.Finite(1)

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

  def packagePrivateProduct = {
    // `private[example]` on a top-level class is the package's own: a sibling file of that
    // package builds and projects through it, so the count is the projection, not a blocker.
    val source = inputs(
      "Wrapper.scala" -> "package example\nprivate[example] case class Wrapper[A](value: A)",
      "Use.scala" -> "package example\ndef unwrap[A](w: Wrapper[A]): A = w.value"
    )
    MethodAnalysis.analyze(source).find(_.name == "example.unwrap").get.count === Count.Finite(1)
  }

  def packagePrivateSubpackage = {
    val source = inputs(
      "Wrapper.scala" -> "package example\nprivate[example] case class Wrapper[A](value: A)",
      "Sub.scala" -> "package example.sub\ndef unwrap[A](w: Wrapper[A]): A = w.value"
    )
    MethodAnalysis.analyze(source).find(_.name == "example.sub.unwrap").get.count === Count.Finite(
      1
    )
  }

  def packagePrivateType = {
    // Resolved, yet still a sealed abstraction the fragment cannot read as a sum: the row must
    // carry the representation's own reason, the one the same declaration gets inside its file.
    val source = inputs(
      "Focus.scala" -> "package example\nprivate[example] abstract class Focus[A]",
      "Use.scala" -> "package example\ndef loop[A](f: Focus[A]): A = loop(f)"
    )
    val loop = MethodAnalysis.analyze(source).find(_.name == "example.loop").get
    loop.count must beLike {
      case Count.Unresolved(reasons) =>
        (reasons.exists(_.startsWith("abstract type or method-valued representation")) must beTrue)
          .and(reasons.exists(_.startsWith("unresolved type")) must beFalse)
    }
  }

  def filePrivateType = {
    // Without the qualifier there is no package grant: a top-level `private` class is its file's
    // alone, so a sibling file's reference stays the name it cannot reach.
    val source = inputs(
      "Hidden.scala" -> "package example\nprivate case class Hidden[A](value: A)",
      "Use.scala" -> "package example\ndef peek[A](h: Hidden[A]): A = h.value"
    )
    MethodAnalysis.analyze(source).find(_.name == "example.peek").get.count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("unresolved type: Hidden")) must beTrue
    }
  }

  def sealedSum = {
    // `sealed` is the compiler's promise that the file enumerates every direct subtype: the
    // family reads as the sum of its leaves, and a Boolean-valued observation over two unit
    // cases is the four functions two-values-to-two-values admits.
    val code =
      """sealed trait Bool
         |case object True extends Bool
         |case object False extends Bool
         |def bit(b: Bool): Boolean = ???""".stripMargin
    entry(code, "bit").count === Count.Finite(4)
  }

  def sealedFamilyWithBinder = {
    // The mirror pattern eo's carriers use: the child's binder pins the parent's slot by the
    // same name, and `Nothing` pins a covariant slot bottom-up — `Missing` belongs to every
    // `Opt[A]`. The family resolves; an `A` has no producer, which is the reviewed `CanGet.get`
    // zero. Sum-domain branching stays deferred exactly as it does for enums — the sealed
    // family and its enum twin answer alike.
    val code =
      """sealed trait Opt[+A]
         |case class Have[A](value: A) extends Opt[A]
         |case object Missing extends Opt[Nothing]
         |def use[A](o: Opt[A]): A = ???""".stripMargin
    val sealedResult = entry(code, "use").count
    val enumTwin =
      entry(
        "enum Opt[+A] { case Have(value: A); case Missing }\ndef use[A](o: Opt[A]): A = ???",
        "use"
      ).count
    (sealedResult === Count.Finite(0)).and(sealedResult === enumTwin)
  }

  def sealedEmpty = {
    // A sealed family that enumerates no subtype has none anywhere: the empty sum is 0 values,
    // and an observation from 0 values has exactly one implementation.
    val code =
      "sealed trait Never\ndef f(n: Never): Boolean = ???"
    entry(code, "f").count === Count.Finite(1)
  }

  def sealedOpenSubtype = {
    // A plain class leaf without `final` can be extended outside the file with its own state:
    // the enumeration is no longer the space. The family stays unreadable.
    val code =
      """sealed trait Shape
         |class Variant(x: Int) extends Shape
         |case object Flat extends Shape
         |def side(s: Shape): Boolean = ???""".stripMargin
    entry(code, "side").count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("sealed subtype outside the closed fragment")) must beTrue
    }
  }

  def sealedOpenIntermediate = {
    // An abstract subtype that is not itself sealed admits further subtypes from anywhere;
    // recursing through it would invent a closed space where the compiler grants none.
    val code =
      """sealed trait Tree
         |abstract class Branch extends Tree
         |case object Leaf extends Tree
         |def size(t: Tree): Boolean = ???""".stripMargin
    entry(code, "size").count must beLike {
      case Count.Unresolved(reasons) =>
        reasons.exists(_.contains("sealed family has an open abstract subtype")) must beTrue
    }
  }

  def importedType = {
    // The import carries the name across packages: `Vec` reads as the sibling package's own
    // product, and the count is the two projections the declaration supplies.
    val source = inputs(
      "Vec.scala" -> "package example.data\ncase class Vec[A](head: A, tail: A)",
      "Use.scala" -> "package example\nimport example.data.Vec\ndef half[A](v: Vec[A]): A = v.head"
    )
    MethodAnalysis.analyze(source).find(_.name == "example.half").get.count === Count.Finite(2)
  }

  def relativeImport = {
    // `package example\npackage data` is how eo nests its clauses, and `import data.Vec` is
    // how it reaches them: the prefix is anchored at the enclosing scopes, nearest first, the
    // way the compiler reads it.
    val source = inputs(
      "Vec.scala" -> "package example\npackage data\ncase class Vec[A](head: A, tail: A)",
      "Use.scala" -> "package example\nimport data.Vec\ndef half[A](v: Vec[A]): A = v.head"
    )
    MethodAnalysis.analyze(source).find(_.name == "example.half").get.count === Count.Finite(2)
  }

  def renamedImport = {
    // `as` binds the new name only: through `V` the product reads, while the original name is
    // left behind in the using file's scope — reaching it stays the name that cannot be found.
    val source = inputs(
      "Vec.scala" -> "package example.data\ncase class Vec[A](head: A, tail: A)",
      "Use.scala" -> (
        "package example\nimport example.data.{Vec as V}\ndef half[A](v: V[A]): A = v.head\n" +
          "def other[A](v: Vec[A]): A = v.head"
      )
    )
    val entries = MethodAnalysis.analyze(source)
    (entries.find(_.name == "example.half").get.count === Count.Finite(2)).and {
      entries.find(_.name == "example.other").get.count must beLike {
        case Count.Unresolved(reasons) =>
          reasons.exists(_.contains("unresolved type: Vec")) must beTrue
      }
    }
  }

  def wildcardImport = {
    val source = inputs(
      "Vec.scala" -> "package example.data\ncase class Vec[A](head: A, tail: A)",
      "Use.scala" -> "package example\nimport example.data.*\ndef half[A](v: Vec[A]): A = v.head"
    )
    MethodAnalysis.analyze(source).find(_.name == "example.half").get.count === Count.Finite(2)
  }

  def companionImport = {
    // `import optics.Optic.*` — the shape eo's sources lean on — opens the companion's own
    // members: the nested case class reads through the wildcard's full path.
    val source = inputs(
      "Optic.scala" ->
        "package example\nobject Optic { case class Lens[A](focus: A) }",
      "Use.scala" ->
        "package other\nimport example.Optic.*\ndef id[A](l: Lens[A]): A = l.focus"
    )
    MethodAnalysis.analyze(source).find(_.name == "other.id").get.count === Count.Finite(1)
  }

  def importedBuiltin = {
    // Unreadable but bound: the import names the builtin's slot and its target lives outside
    // the sources, so the model's own `Option` must not answer for it. Without the clause the
    // same result type reads as the builtin sum, inhabited by `None` alone.
    val plain = entry("def none[A]: Option[A] = ???", "none").count
    val imported = MethodAnalysis
      .analyze(
        inputs(
          "Use.scala" -> "package example\nimport io.circe.Option\ndef none[A]: Option[A] = ???"
        )
      )
      .find(_.name == "example.none")
      .get
      .count
    (plain === Count.Finite(1)).and {
      imported must beLike {
        case Count.Unresolved(reasons) =>
          reasons.exists(
            _.contains("unresolved type: Option (imported from io.circe.Option)")
          ) must beTrue
      }
    }
  }

  def aliasImportPosition = {
    // A transparent alias body, reconstructed under substitution, carries no position of its
    // own. Inside the caller's file the caller's reference position decides the import scope:
    // `Vec` in the body reaches the imported product, and `take` counts its two projections.
    val source = inputs(
      "Vec.scala" -> "package example.data\ncase class Vec[A](head: A, tail: A)",
      "Use.scala" -> (
        "package example\nimport example.data.Vec\nobject Schemes:\n" +
          "  type Pair2[A] = Vec[A]\n  def take[A](p: Pair2[A]): A = p.head"
      )
    )
    MethodAnalysis.analyze(source).find(_.name == "example.Schemes.take").get.count ===
      Count.Finite(2)
  }

  def importOrder = {
    // A clause binds its names from its own position onward: the earlier reference keeps the
    // scope the compiler would give it — where the name is not otherwise reachable, it stays
    // the name it cannot find — while the later one reads the imported product.
    val source = inputs(
      "Vec.scala" -> "package example.data\ncase class Vec[A](head: A, tail: A)",
      "Use.scala" -> (
        "package example\ndef early[A](v: Vec[A]): A = v.head\nimport example.data.Vec\n" +
          "def late[A](v: Vec[A]): A = v.head"
      )
    )
    val entries = MethodAnalysis.analyze(source)
    (entries.find(_.name == "example.late").get.count === Count.Finite(2)).and {
      entries.find(_.name == "example.early").get.count must beLike {
        case Count.Unresolved(reasons) =>
          reasons.exists(_.contains("unresolved type: Vec")) must beTrue
      }
    }
  }

  def incomplete = {
    val representation = entry(
      "object Hidden { opaque type Id[A] = A }\ndef choose[A](x: Hidden.Id[A]): A = ???",
      "choose"
    )
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
