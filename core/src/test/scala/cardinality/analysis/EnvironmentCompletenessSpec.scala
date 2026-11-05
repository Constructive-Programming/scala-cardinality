package cardinality.analysis

import cardinality.analysis.inhabitation.*
import org.specs2.Specification
import org.specs2.matcher.MatchResult

/** Environment completeness: concrete code outside the target, mutable state, names exports and
  * imports bind and by-name parameters (inferred results: `InferredResultsSpec`). Every fixture is
  * valid Scala 3.9.0 except where a name outside the sources (`foreign`, `ext`, `Ext`) is the
  * point, and `Both`, a double definition kept as a refusal; each expected number is derived in the
  * comment beside it.
  */
class EnvironmentCompletenessSpec extends Specification with InventoryFixtures {
  import Inhabitation.Count

  def is = s2"""
    Concrete code outside the target
      a module's Boolean producers add no implementation             $moduleProducers
      a polymorphic module member adds nothing to a Boolean result   $polymorphicProducer
      a constructor's value spaces ignore outside modules            $constructorPositions
      an opaque type outside its scope is one value                  $opaqueOutside
      a class with no public constructor stays unread                $privateConstructor
      a supplied callable's Boolean result is eliminated             $opaqueCallable
      a final template's member is its own concrete code             $finalMembers
      an open class's member is a capability (X5)                    $overridableMembers
    Exports define members
      an export outranks an outer definition                         $exportOutranks
      an export outranks an import of its own body                   $exportBeatsImport
      a wildcard import or a path reads an object's exports          $reexports
      a renamed, hidden or given export leaves the name alone        $exportSelectors
      a wildcard export outside the sources refuses the name         $foreignExport
      an export in a parent or beside a declaration is refused       $exportRefusals
      a parent's export refuses only the names it defines            $exportedNames
      an export of what the exported object inherits outranks        $exportInherited
      a path through a value is never read as an object              $valuePrefix
      cyclic, ambiguous and non-path exports are refused             $exportCycles
    Imports
      a given selector binds no type name                            $givenSelector
      an unimport and a rename leave the builtin                     $unimport
      an unimport under a foreign wildcard leaves the builtin        $foreignUnimport
      a clause whose prefix is no path refuses the builtin           $nonPathImport
      a wildcard import reads the members an object inherits         $inheritedThroughImport
      a protected member an object inherits is not imported          $protectedThroughImport
      an inherited member under type arguments is refused            $argumentsThroughImport
      an object with a parent outside the sources refuses the name   $unsuppliedThroughImport
    Mutable state
      a var holds the value it was given                             $variables
      a var implements an inherited declaration in any template      $variableImplementations
      an abstract var is a declared slot                             $abstractVariable
    Local captures
      a member of the scope hides an enclosing method's parameter    $hiddenCapture
      a capture an outer template keeps stays reachable              $qualifiedCapture
      an export that hides a local capture is refused                $exportedCapture
      an inner import or inherited member hides a local capture      $ambiguousCapture
      an extension method hides a local capture                      $extensionCapture
      every variable a pattern definition binds hides a capture      $patternCapture
      a pattern-bound value is no method and no object               $patternDefinitions
    Parameters
      a by-name parameter reads as its value                         $byName
  """

  private val intPredicate = Count.Symbolic(NaturalSyntax.pow(2, NaturalSyntax.pow(2, 32)))

  // The flag `Defaults.flag` and `not(b)` are values the four functions of `b` already reach: a
  // closed helper denotes one of the in-model functions of the readable inputs.
  def moduleProducers =
    count(
      "object Defaults { val flag: Boolean = true; def not(b: Boolean): Boolean = !b }\n" +
        "def pick(b: Boolean): Boolean = b",
      "pick"
    ) === Count.Finite(4)

  // `dup[X]` cannot reach the target's binder A: true and false only.
  def polymorphicProducer =
    count(
      "object Util { def dup[X](x: X): (X, X) = (x, x) }\ndef test[A](x: A): Boolean = true",
      "test"
    ) === Count.Finite(2)

  // 2 × 2^32 field values.
  def constructorPositions =
    count(
      "object M { val on = true }\nfinal case class Phone(isMobile: Boolean, number: Int)",
      "Phone.<init>"
    ) === Count.Finite(BigInt(2).pow(33))

  // Outside its scope `Token.T` is one value (rule F), so `h` has one implementation, whatever its
  // companion offers.
  def opaqueOutside =
    count(
      "object Token { opaque type T = Boolean; def flip(t: T): T = !t }\n" +
        "def h(t: Token.T): Token.T = Token.flip(t)",
      "h"
    ) === Count.Finite(1)

  // Outside `Secret` (and its companion) `Secret.on` is the only way to one: no constructor `f` may
  // call says what its record holds (rule B). Inside, the private primary is callable: `get` is
  // 2^2 = 4.
  def privateConstructor = {
    val found = rows(
      "final case class Secret private (b: Boolean) { def get: Boolean = b }\n" +
        "object Secret { val on: Secret = new Secret(true) }\ndef f(): Secret = Secret.on"
    )
    (found("Secret.get") === Count.Finite(4))
      .and(refused(found("f"), "receiver has no public constructor: Secret"))
  }

  // `g(a)` is eliminated once (capabilities C10) and the helper `M.not` adds no new function of it:
  // true, false, g(a), !g(a) — 4.
  def opaqueCallable =
    count(
      "object M { def not(b: Boolean): Boolean = !b }\ndef f[A](g: A => Boolean, a: A): Boolean = g(a)",
      "f"
    ) === Count.Finite(4)

  // Cross-check X5 (fixture xprobe/ow): `FBox` is final, so `other` is its own concrete code, which
  // computes `b` from what `target` already has: a or b (2).
  def finalMembers =
    count(
      "final class FBox[A](a: A, b: A) { def other: A = b; def target: A = a }",
      "FBox.target"
    ) === Count.Finite(2)

  // `Box` is open: BoxO overrides `other` to return `a`, so `other` is a capability (capabilities
  // C5, register D20), and `target` may return a, b or `other` (3). With no BoxO (the closed world)
  // Box is final: a or b (2).
  def overridableMembers = {
    val box = "class Box[A](a: A, b: A) { def other: A = b; def target: A = a }"
    (count(
      box + "\nclass BoxO[A](a: A) extends Box[A](a, a) { override def other: A = a }",
      "Box.target"
    ) === Count.Finite(3)).and(count(box, "Box.target") === Count.Finite(2))
  }

  // Cross-check X1 (fixture c3): inside `C`, `T` is the exported `A.T = Boolean`, not `Outer.T =
  // Int`. In the final `C`, g: Boolean => Boolean = 4; P's one field is a Boolean = 2 (the estimate
  // agrees). In the open `D` (DO overrides both) each of g and h is the other's overridable capability, a Boolean =>
  // Boolean (capabilities C5): every Boolean function of t, g(true) and g(false) — 2^8.
  def exportOutranks = {
    val found = rows("""object Outer {
      |  object A { type T = Boolean }
      |  type T = Int
      |  final class C {
      |    export A.T
      |    final case class P(t: T)
      |    def g(t: T): Boolean = true
      |    def h(t: T): T = t
      |  }
      |  class D { export A.T; def g(t: T): Boolean = true; def h(t: T): T = t }
      |  class DO extends D { override def g(t: T): Boolean = false; override def h(t: T): T = t }
      |}""".stripMargin)
    (found("Outer.C.g") === Count.Finite(4))
      .and(found("Outer.C.h") === Count.Finite(4))
      .and(found("Outer.C.P.<init>") === Count.Finite(2))
      .and(found("Outer.D.g") === Count.Finite(256))
      .and(found("Outer.D.h") === Count.Finite(256))
  }

  // Fixture c6: an export defines a member, so it wins over an import of the same body: T = Y.T.
  def exportBeatsImport =
    count(
      "object X { type T = Int }\nobject Y { type T = Boolean }\n" +
        "class C { import X.T; export Y.T; def g(t: T): Boolean = t }",
      "C.g"
    ) === Count.Finite(4)

  // `Option` is `Types.Option = (A, A)` through each re-export: `x._1` and `x._2` (2).
  def reexports = {
    val found = rows("""object Types { type Option[A] = (A, A); type Pair[A] = (A, A) }
      |object Re { export Types.* }
      |object UseRe { import Re.*; def pick[A](x: Option[A]): A = x._1 }
      |object InRe { export Types.*; def pick[A](x: Option[A]): A = x._1 }
      |object Re2 { export Types.Pair }
      |object UseRe2 { def pick[A](x: Re2.Pair[A]): A = x._1 }
      |object Re3 { export Types.Pair }
      |object UseRe3 { import Re3.Pair; def pick[A](x: Pair[A]): A = x._1 }""".stripMargin)
    (found("UseRe.pick") === Count.Finite(2))
      .and(found("InRe.pick") === Count.Finite(2))
      .and(found("UseRe2.pick") === Count.Finite(2))
      .and(found("UseRe3.pick") === Count.Finite(2))
  }

  // A rename exports `U` only, an unimport leaves `T` out, a given selector exports givens: `T` is
  // `Outer.T = Int` in each, so g ranges over Int => Boolean (2^(2^32)); `U` is Boolean (4). `Ren`
  // is final: open, its `h` would be a capability g could also read.
  def exportSelectors = {
    val found = rows("""object Types { type T = Boolean; given g: Boolean = true }
      |object Outer {
      |  type T = Int
      |  final class Ren { export Types.{T => U}; def g(t: T): Boolean = true; def h(u: U): Boolean = u }
      |  class Hid { export Types.{T as _, *}; def g(t: T): Boolean = true }
      |  class Giv { export Types.given; def g(t: T): Boolean = true }
      |}""".stripMargin)
    (found("Outer.Ren.g") === intPredicate)
      .and(found("Outer.Ren.h") === Count.Finite(4))
      .and(found("Outer.Hid.g") === intPredicate)
      .and(found("Outer.Giv.g") === intPredicate)
  }

  // `foreign.lib` may define a `T`: never fall back to `Outer.T`.
  def foreignExport = {
    val found = rows(
      """import ext.Obj
      |import ext.importer
      |object Outer {
      |  type T = Int
      |  class Fw { export foreign.lib.*; def g(t: T): Boolean = true }
      |}
      |object O { export Obj.T; export importer.U; def f(t: T): T = t; def g(u: U): U = u }""".stripMargin
    )
    refused(found("Outer.Fw.g"), "wildcard export not supplied: T (wildcard export of foreign.lib)")
      .and(
        refused(
          found("O.f"),
          "declaration not supplied: ext.Obj.T (selected through named import Obj, exported by O)"
        )
      )
      .and(
        refused(
          found("O.g"),
          "declaration not supplied: ext.importer.U " +
            "(selected through named import importer, exported by O)"
        )
      )
  }

  // `Sub` inherits the alias `Base` exports (Scala 3.9.0 compiles `def g(t: T): Boolean = t`, so
  // `T` is Boolean, beating the other file's `T = Int`; true count 4), which this reading does not
  // enumerate. A body that both declares and exports `T` is a double definition (E161), refused.
  def exportRefusals = {
    val found = counts(
      "a.scala" -> "package p\ntype T = Int",
      "b.scala" -> ("package p\nobject Types { type T = Boolean }\ntrait Base { export Types.T }\n" +
        "class Sub extends Base { def g(t: T): Boolean = t }\n" +
        "class Both { type T = Boolean; export Types.T; def g(t: T): Boolean = t }")
    )
    refused(found("p.Sub.g"), "inherited type member not resolved: T (exports of Base)")
      .and(refused(found("p.Both.g"), "ambiguous type: T (declared and exported)"))
  }

  // An export clause defines only what its selectors name (Scala 3 reference, export clauses): `P`
  // exports `T` alone, so `Boolean` stays the builtin and only `T` is refused (scalac: `Src.T`,
  // true 4). `R` exports `Q` alone, so `Obj` is the enclosing `Outer.Obj`: `U` is Boolean, and the
  // Boolean functions of a Boolean are 4.
  def exportedNames = {
    val found = rows("""object Src { type T = Boolean }
      |trait P { export Src.T }
      |final class C extends P { def f(t: T): Boolean = true }
      |object Other { type Q = Int }
      |trait R { export Other.Q }
      |object Outer {
      |  object Obj { type U = Boolean }
      |  final class D extends R { def k(u: Obj.U): Boolean = true }
      |}""".stripMargin)
    (found("C.f") === Count.Unresolved(
      List("inherited type member not resolved: T (exports of P)")
    ))
      .and(found("Outer.D.k") === Count.Finite(4))
  }

  // Fixture `ei`: `O` only inherits `T = Boolean` from `Base`; `export Types.O.*` still defines `T`
  // in `Inner`, which outranks `Sub`'s inherited `T = Int` (Scala 3.9.0 types Inner's `T` as
  // `Types.O.T`): Boolean => Boolean (4).
  def exportInherited =
    count(
      "object Types { trait Base { type T = Boolean }; object O extends Base }\n" +
        "trait Outer { type T = Int }\n" +
        "class Sub extends Outer { class Inner { export Types.O.*; def g(t: T): Boolean = true } }",
      "Sub.Inner.g"
    ) === Count.Finite(4)

  // `x` is the parameter (or the nearer `val`), whose `T` is abstract, never the object `x`'s `T =
  // Int`: the lookup reads no value's members and refuses (true count 2, the constants).
  def valuePrefix = {
    val found = rows(
      """object x { type T = Int }
      |trait HasT { type T }
      |final class C(val x: HasT) { export x.*; def f(t: T): Boolean = true }
      |final class D(val x: HasT) { export x.T; def g(t: T): Boolean = true }
      |final class E(val x: HasT) { import x.*; def h(t: T): Boolean = true }
      |object Outer { val x: HasT = new HasT { type T = String }; object In { export x.*; def k(t: T): Boolean = true } }
      |trait HasX { val x: HasT }
      |final class F(val y: HasT) extends HasX { val x: HasT = y; class In { export x.*; def m(t: T): Boolean = true } }""".stripMargin
    )
    val reason = "path through a value not followed: x"
    refused(found("C.f"), reason)
      .and(refused(found("D.g"), reason))
      .and(refused(found("E.h"), reason))
      .and(refused(found("Outer.In.k"), reason))
      .and(refused(found("F.In.m"), reason))
  }

  // An export that reaches itself, two that define one name and one whose prefix is no path: no
  // single declaration is read.
  def exportCycles = {
    val found = rows(
      """object Cyc { object E1 { export E2.* }; object E2 { export E1.* }; def f(t: E1.T): Boolean = true }
      |object Amb { object X1 { type T = Int }; object X2 { type T = Boolean }
      |  class C { export X1.T; export X2.T; def f(t: T): Boolean = true } }
      |class Np { object x { type T = Boolean }; export this.x.*; def g(t: T): Boolean = true }""".stripMargin
    )
    refused(found("Cyc.f"), "cyclic export: T")
      .and(refused(found("Amb.C.f"), "ambiguous export: T"))
      .and(refused(found("Np.g"), "export not followed: T (export of this.x)"))
  }

  // A given selector imports given instances, which are terms: x alone (1).
  def givenSelector =
    count(
      "object Types { type Option[A] = (A, A); given Option[Int] = (1, 2) }\n" +
        "object G { import Types.{given Option[Int]}; def pick[A](x: A): A = x }",
      "G.pick"
    ) === Count.Finite(1)

  // `Option` stays scala.Option (Scala 3.9.0 compiles `x.getOrElse(y)` here): None gives y, Some(a)
  // gives a or y — 1 × 2. Read as `(A, A)` it would be 3.
  def unimport = {
    val found = rows(
      """object Types { type Option[A] = (A, A) }
      |object U { import Types.{Option as _, *}; def pick[A](x: Option[A], y: A): A = y }
      |object R { import Types.{Option => Opt, *}; def pick[A](x: Option[A], y: A): A = y }""".stripMargin
    )
    (found("U.pick") === Count.Finite(2)).and(found("R.pick") === Count.Finite(2))
  }

  // The wildcard of an unsupplied `yyy` leaves `Boolean` out, so it is scala.Boolean: Boolean =>
  // Boolean (4) in both.
  def foreignUnimport = {
    val found = rows("""object P { import yyy.{Boolean as _, *}; def g(b: Boolean): Boolean = b }
      |object P2 { import yyy.{Boolean as B, *}; def g2(b: Boolean): Boolean = b }""".stripMargin)
    (found("P.g") === Count.Finite(4)).and(found("P2.g2") === Count.Finite(4))
  }

  // `this.x` is no path lookup reads: `Unit` may be `x`'s `Boolean` (2), so the terminal-goal rule
  // (1) does not apply.
  def nonPathImport =
    refused(
      count(
        "class NP { object x { type Unit = Boolean }; import this.x.Unit; def y(): Unit = ??? }",
        "NP.y"
      ),
      "imported environment not resolved: import this.x.Unit"
    )

  // `T` is `Base.T = Boolean` through `import O.*` and `O.T` (4), beating the other file's `T = Int`.
  def inheritedThroughImport = {
    val found = counts(
      "a.scala" -> "package p\ntype T = Int",
      "b.scala" ->
        ("package p\ntrait Base { type T = Boolean }\nobject O extends Base\n" +
          "object Use { import O.*; def g(t: T): Boolean = t }\n" +
          "object Use2 { def g(t: O.T): Boolean = t }")
    )
    (found("p.Use.g") === Count.Finite(4)).and(found("p.Use2.g") === Count.Finite(4))
  }

  // `O` inherits a protected `U`, which `Use` cannot select: `U` is the other file's Boolean (Boolean
  // => Boolean, 4), through `import O.*` and `export O.*` (Scala 3.9.0 rejects `val u: U = 1` there).
  def protectedThroughImport = {
    val found = counts(
      "a.scala" -> "package ih\ntype U = Boolean",
      "b.scala" -> ("package ih\ntrait Base { protected type U = Int }\nobject O extends Base\n" +
        "object Use { import O.*; def h(u: U): U = u }\nobject UseExp { export O.*; def h(u: U): U = u }")
    )
    (found("ih.Use.h") === Count.Finite(4)).and(found("ih.UseExp.h") === Count.Finite(4))
  }

  // A path carries no type arguments: `O`'s `T` is `Base[Boolean]`'s `X`, which the inherited
  // member's own scope does not read.
  def argumentsThroughImport =
    refused(
      count(
        "trait BaseG[X] { type T = X }\nobject OG extends BaseG[Boolean]\n" +
          "object UseG { import OG.*; def g(t: T): Boolean = true }",
        "UseG.g"
      ),
      "inherited member path not resolved: T (type arguments of BaseG)"
    )

  def unsuppliedThroughImport =
    refused(
      counts(
        "a.scala" -> "package p\ntype T = Int",
        "b.scala" -> "package p\nobject O extends ext.X\nobject Use { import O.*; def g(t: T): Boolean = t }"
      )("p.Use.g"),
      "inherited type member not resolved: T (unsupplied parent ext.X)"
    )

  // Mutation is excluded: `cur` is the value it was initialized with. V: x or the parameter (2);
  // V2: x or seed (2); Cell: init (1); a `null` initializer is no in-model value: x (1). An Int
  // var holding 0 is no input: any Int (2^32).
  def variables = {
    val found = rows("""class V[A](var cur: A) { def pick(x: A): A = x }
      |class V2[A](seed: A) { var cur: A = seed; def pick(x: A): A = x }
      |class Cell[A](init: A) { var cur: A = init; def set(a: A): Unit = cur = a; def get(): A = cur }
      |class Cell3[A] { var cur: A = null.asInstanceOf[A]; def get(x: A): A = cur }
      |class C { var n: Int = 0; def get: Int = n }""".stripMargin)
    (found("V.pick") === Count.Finite(2))
      .and(found("V2.pick") === Count.Finite(2))
      .and(found("Cell.get") === Count.Finite(1))
      .and(found("Cell3.get") === Count.Finite(1))
      .and(found("C.get") === Count.Finite(BigInt(2).pow(32)))
  }

  // Scala lets no member override a mutable variable (E164), so `cur` is `a` even in the open
  // `ImplO`: x or a (2). In the final `ImplP` the parameter is the member: x, cur or b (3).
  def variableImplementations = {
    val found = rows(
      """trait TV[A] { var cur: A }
      |class ImplO[A](a: A) extends TV[A] { var cur: A = a; def pick(x: A): A = x }
      |final class ImplP[A](var cur: A, b: A) extends TV[A] { def pick(x: A): A = x }""".stripMargin
    )
    (found("ImplO.pick") === Count.Finite(2)).and(found("ImplP.pick") === Count.Finite(3))
  }

  // An abstract var is an abstract getter (and a Unit setter): x or cur (2), in the trait and in an
  // abstract class that inherits it unimplemented. `pick` is final: an overridable one would be a
  // capability A => A of `Open`, and `other` would range over x, cur, pick(x), pick(pick(x)), … (ω).
  def abstractVariable = {
    val found = rows("""trait T[A] { var cur: A; final def pick(x: A): A = x }
      |abstract class Open[A] extends T[A] { def other(x: A): A = x }""".stripMargin)
    (found("T.pick") === Count.Finite(2)).and(found("Open.other") === Count.Finite(2))
  }

  // Inside `C`, `seed` is `C.seed`, and a local class has no path to `outer`'s parameter — but the
  // sibling class `E` reads it, and `C` may build one: `new E().k()` is outer's seed (3.9.0 run
  // prints it). So `g` and `seed` read other and seed (2), which this reading refuses rather than
  // count `other` alone (it read 1). A sibling defining its own `seed` reads nothing hidden: H's
  // g is other (1). `E` hides nothing: seed or other (2).
  def hiddenCapture = {
    val found = rows("""object Out {
      |  def outer[X](seed: X, other: X): Int = {
      |    final class C { def seed: X = other; def g(): X = seed }
      |    final class E { def k(): X = seed }
      |    0
      |  }
      |  def own[X](seed: X, other: X): Int = {
      |    final class H { def seed: X = other; def g(): X = seed }
      |    final class F { val seed: X = other; def k(): X = seed }
      |    0
      |  }
      |}""".stripMargin)
    val held = "local capture held by a member that hides it: seed"
    refused(found("Out.outer.C.g"), held)
      .and(refused(found("Out.outer.C.seed"), held))
      .and(found("Out.outer.E.k") === Count.Finite(2))
      .and(found("Out.own.H.g") === Count.Finite(1))
  }

  // `Outer.this.seed` still reaches the class parameter: other or seed (2).
  def qualifiedCapture =
    count(
      "class Outer[X](seed: X, other: X) { final class C { def seed: X = other; def g(): X = seed } }",
      "Outer.C.g"
    ) === Count.Finite(2)

  // Inside `D`, `seed` is the exported `A.seed`; the parameter is hidden. The reading refuses
  // rather than reads which exported names are terms (true count 2).
  def exportedCapture =
    refused(
      count(
        "object A { def seed: Boolean = true }\nobject Out {\n  def outer(seed: Boolean): Int = {\n" +
          "    final class D { export A.seed; def h(): Boolean = seed }\n    0\n  }\n}",
        "Out.outer.D.h"
      ),
      "local capture hidden by an export: seed"
    )

  // Scala 3.9.0 reports `seed` ambiguous (E049) in `C`, between the import and `run`'s parameter,
  // and in `D`, between `Base.seed` and the parameter: neither class reaches the parameter, so `h`
  // has no X (0); the import is refused rather than read. A wildcard export of `A` may define
  // `seed` too.
  def ambiguousCapture = {
    val found = rows("""object Imp { val seed: Int = 0 }
      |trait Base { def seed: Int = 0 }
      |object A { def seed: Boolean = true }
      |object Out {
      |  def run[X](seed: X): Int = {
      |    final class C { import Imp.seed; def g(): X = ??? }
      |    final class D extends Base { def h(): X = ??? }
      |    0
      |  }
      |  def outer(seed: Boolean): Int = {
      |    final class W { export A.*; def h(): Boolean = seed }
      |    0
      |  }
      |}""".stripMargin)
    refused(found("Out.run.C.g"), "local capture may be hidden by an import: seed")
      .and(found("Out.run.D.h") === Count.Finite(0))
      .and(refused(found("Out.outer.W.h"), "local capture may be hidden by an export: seed"))
  }

  // Inside `C`, `seed` is the extension method (scalac reads it as `Int => X`): only `other` (1).
  def extensionCapture =
    count(
      """object Out {
        |  def run[X](seed: X, other: X): Int = {
        |    final class C { extension (i: Int) def seed: X = other; def g(): X = ??? }
        |    0
        |  }
        |}""".stripMargin,
      "Out.run.C.g"
    ) === Count.Finite(1)

  // Inside each class `seed` is the member a pattern binds (scalac compiles `def g(): A = seed`
  // there, and reports the parameter unreachable in `Sub`, E049), which holds `other`; a method's
  // parameter has no qualified path: other only (1) — a tuple, a `var`, a binder, an inherited
  // tuple.
  def patternCapture = {
    val found = rows("""object H {
      |  def outer[A](seed: A, other: A): Unit = {
      |    final class C { val (seed, z) = (other, other); def g(): A = other }
      |    final class V { var (seed, z) = (other, other); def g(): A = other }
      |    final class W { val seed @ _ = other; def g(): A = other }
      |    class Base { val (seed, z) = (other, other) }
      |    final class Sub extends Base { def g(): A = other }
      |    ()
      |  }
      |}""".stripMargin)
    List("C", "V", "W", "Sub").map(c => found(s"H.outer.$c.g") === Count.Finite(1)).reduce(_ and _)
  }

  // `Inner.f` is the pattern's function value (scalac types `g` a Boolean, true 2), never `Outer.f:
  // Int`; `x` is a value holding `Other`, so `x.T` is `Other.T = Int` (scalac accepts `f(1)`; true
  // 2^(2^32)), never the object `x`'s Boolean. Both are refused. A pattern member implementing an
  // inherited declaration is that concrete value: other only (1).
  def patternDefinitions = {
    val found = rows(
      """object Outer {
      |  def f(x: Int): Int = x
      |  object Inner { val (f, z) = ((x: Int) => true, 1); def g = f(1) }
      |}
      |object x { type T = Boolean }
      |object Other { type T = Int }
      |object PInner { val (x, y) = (Other, Other); def f(t: x.T): Boolean = true }
      |trait P[A] { val seed: A }
      |final class C[A](other: A) extends P[A] { val (seed, z) = (other, other); def g(): A = seed }""".stripMargin
    )
    refused(
      found("Outer.Inner.g"),
      "inferred result type not resolved: call of f, not one declared"
    )
      .and(refused(found("PInner.f"), "unavailable stable path type: x"))
      .and(found("C.g") === Count.Finite(1))
  }

  // Evaluation count and timing are not observable in the model: x (1); x or y (2).
  def byName =
    (count("def byName[A](x: => A): A = x", "byName") === Count.Finite(1))
      .and(count("def byName2[A](x: => A, y: A): A = y", "byName2") === Count.Finite(2))

}
