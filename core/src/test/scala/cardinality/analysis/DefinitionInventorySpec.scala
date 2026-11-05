package cardinality.analysis

import scala.meta.*

import cardinality.EoBaseline
import cardinality.analysis.model.GivenNames
import org.specs2.Specification

/** Rows for the definitions beyond defs (docs/research/definition-inventory.md): given aliases and
  * abstract givens, member values, members of anonymous classes, enum-case, abstract-class, trait
  * and given-instance constructors, and secondary constructors — each measured by the rule its kind
  * already has (definitions of a body's inner scopes: `NestedScopesSpec`). Every fixture compiles
  * with Scala 3.9.0 (`$S/probes/b5/spec`, `$S/probes/b5-fix2/spec`); each expected number is
  * derived beside it.
  */
class DefinitionInventorySpec extends Specification with InventoryFixtures {

  def is = s2"""
    Givens
      a given alias is a method of its clauses                        $givenAliases
      an abstract given is a declaration, implemented without itself  $abstractGivens
      a local given alias is a row with clauses, a step without       $localGiven
      an unmatched inherited given leaves the identity unresolved     $givenIdentity
      an anonymous given is the member of its synthesized name        $anonymousGivens
      a synthesized name is the one 3.9.0 gives                       $synthesizedNames
      an inline given alias is read by the inline-method rule         $inlineGivens
    Member values
      a value is a nullary signature of its type                      $memberValues
      a pattern's names are not typed by syntax                       $patternValues
      a local value is no row                                         $localValues
      each name of a declaration is a slot of the others              $siblingDeclarations
      a value implementing a declaration is not its own slot          $ownSlot
      an abstract var is a declaration of its type                    $abstractVar
    Anonymous classes
      a member is a final class's, with the enclosing captures        $anonymousMembers
      each anonymous class of a body is a row of its own              $anonymousOrdinals
      an inherited concrete member is no capability of it             $anonymousFinal
      hidden parent state a body may not reach is refused             $hiddenState
      a level passing the argument or a body import is not followed   $passedLevels
      a value's or a given's context binders reach its members        $valueBinders
      an untyped value takes an overridden declaration's binders      $overriddenBinders
      a context-function literal binds its own clause once            $contextLiterals
    Constructors
      an enum's parameterized case has the case-class constructor     $enumCases
      an enum case is typed where its enum is declared                $enumScope
      a case with a parent keeps the enum's binders                   $enumParents
      abstract classes, traits and conditional givens have one        $templateConstructors
      a bound's evidence position is named by its evidence           $evidencePosition
      a secondary constructor chooses the primary's arguments         $secondaryConstructor
      a secondary constructor of an open class reads no record        $secondaryOpen
      a secondary constructor's body sees the class's members         $secondaryBody
      the primary's arguments: by-name, repeated, none, evidence      $primaryArguments
    Identities
      new rows have unique identities that moving does not change     $identities
  """

  def givenAliases = {
    val found = rendered(
      getter + """
        |object Givens {
        |  given named: Boolean = true
        |  given Option[Unit] = None
        |  given cond[A](using g: Getter[A, A]): Getter[A, A] = g
        |  given pair[A](using a: A): (A, A) = (a, a)
        |}""".stripMargin
    )
    // Boolean: 2. Option[Unit]: None or Some(()), 2 (the anonymous given is named by its line, the
    // fifth: `getter` and the blank line come first).
    // Getter[A, A] is A => A with g: g.get iterated, ω. (A, A) from a alone: 1.
    (found("Givens.named") === "2")
      .and(found("Givens.<given@5>") === "2")
      .and(found("Givens.cond") === "ω")
      .and(found("Givens.pair") === "1")
  }

  def abstractGivens = {
    val found = rendered(
      """|trait Pick[A] { given pick: A }
         |class Picked[A](a: A) extends Pick[A] { given pick: A = a }
         |""".stripMargin
    )
    // The declaration has no A to delegate to: 0. The alias implements it in an open class, so
    // the inherited declaration is the alias itself, not a slot: only a, 1 (2 if it counted itself).
    (found("Pick.pick") === "0").and(found("Picked.pick") === "1")
  }

  // An inline given alias is a row read like an inline method (register D27): its body uses no
  // compile-time operation, so it counts as its given twin — a Boolean, 2; a Boolean from b
  // (negation, identity, the two literals), 4 — while one that summons is refused. Fixture
  // `$S/probes/b5-land/giv.scala` (3.9.0).
  def inlineGivens = {
    val code =
      """|import scala.compiletime.summonInline
         |object G {
         |  inline given g: Boolean = true
         |  inline given h(using b: Boolean): Boolean = !b
         |  inline given k[A](using a: A): A = summonInline[A]
         |}""".stripMargin
    val found = rows(code)
    (found("G.g").render === "2")
      .and(found("G.h").render === "4")
      .and(refused(found("G.k"), "inline method: uses scala.compiletime.summonInline"))
  }

  def localGiven = {
    val found = rendered(
      getter + """
        |object Local {
        |  def local(flag: Boolean): Getter[Unit, Boolean] = {
        |    given g: Getter[Unit, Boolean] = new Getter[Unit, Boolean] { def get(s: Unit): Boolean = flag }
        |    given h(using u: Unit): Boolean = flag
        |    g
        |  }
        |}""".stripMargin
    )
    // A nullary alias is a lazy given value (3.9.0), a step of `local` like a local value: no row,
    // but its anonymous class's member is one, a Boolean from flag: 4. The alias with a clause is a
    // method: a Boolean from flag and u, 4 (g is concrete code).
    (found.contains("Local.local.g") must beFalse)
      .and(found("Local.local.<new#1>.get") === "4")
      .and(found("Local.local.h") === "4")
  }

  // A given with clauses implementing an inherited one in an open class (D extends it): matching
  // their parameter types is not done.
  def givenIdentity =
    refused(
      rows(
        """|trait T[B] { given g[A](using a: A, b: B): A }
           |class C[B](x: B) extends T[B] { given g[A](using a: A, b: B): A = a }
           |class D[B](x: B) extends C[B](x)
           |""".stripMargin
      )("C.g"),
      "inherited override identity not resolved: g"
    )

  // Scala names `given A = a` given_A: in C it implements the parent's abstract given of that name,
  // in D a parent's parameterless def, in the open O the parent's declaration again — never a slot
  // of the given's own row: a or b, 2; a alone, 1 (3, 2 and 3 if the declaration were one more A).
  // In the final E, given_A is concrete code over a, so `other` chooses among a and b: 2, never 3.
  def anonymousGivens = {
    val found = rendered(
      """|trait T[A] { given given_A: A }
         |final class C[A](a: A, b: A) extends T[A] { given A = a }
         |trait U[A] { def given_A: A }
         |final class D[A](a: A) extends U[A] { given A = a }
         |final class E[A](a: A, b: A) extends T[A] {
         |  given A = a
         |  def other: A = b
         |}
         |class O[A](a: A, b: A) extends T[A] { given A = a }
         |""".stripMargin
    )
    (found("C.<given@2>") === "2")
      .and(found("D.<given@4>") === "1")
      .and(found("E.other") === "2")
      .and(found("O.<given@9>") === "2")
  }

  // Each name as 3.9.0's `-Xprint:typer` prints it (fixtures `$S/probes/b5-fix2/gname`). The parser
  // drops the bound of `Ord[? <: String]` (3.9.0: given_Ord_String), so that name is not known.
  def synthesizedNames = {
    val source = dialects
      .Scala3("""|object N {
                 |  given Int = 1
                 |  given Ord[Int] = o
                 |  given [T](using Ord[T]): Ord[List[T]] = o
                 |  given (Int, String) = p
                 |  given Ord[(Int, (Long, Short))] = o
                 |  given (String => Int) = f
                 |  given ((Int, Long) => String) = f
                 |  given (() => Int) = f
                 |  given (Int ?=> String) = f
                 |  given Ord[Long => String] = o
                 |  given scala.collection.immutable.Seq[Char] = s
                 |  given Two[Int, Ord[String]] = t
                 |  given Ord[Short] & Show[Short] = o
                 |  given (Boolean | Char) = b
                 |  given Ord[?] = o
                 |  given Ord[? <: String] = o
                 |  given Ord[Int Two Long] = o
                 |  given Ord[[X] =>> Ord[X]] = o
                 |  given M.x.type = M.x
                 |  given Ord[M.type] = o
                 |  given 3 = 3
                 |  given Ord[Char] with {}
                 |  given Ord[Int] with Show[Int] with {}
                 |}""".stripMargin)
      .parse[Source]
      .get
    val givens = source.collect { case g: (Defn.GivenAlias | Defn.Given) => g }
    (givens.filter(GivenNames.unknown).map(_.pos.text) === List("given Ord[? <: String] = o"))
      .and(
        givens.flatMap(GivenNames.of) === List(
          "given_Int",
          "given_Ord_Int",
          "given_Ord_List",
          "given_Int_String",
          "given_Ord_Int_Long_Short",
          "given_String_to_Int",
          "given_Int_Long_to_String",
          "given_Int",
          "given_Int_to_String",
          "given_Ord_Function",
          "given_Seq_Char",
          "given_Two_Int_Ord",
          "given_&_Ord_Show",
          "given_|_Boolean_Char",
          "given_Ord_",
          "given_Ord_Two",
          "given_Ord_Ord",
          "given_x_type",
          "given_Ord_M_type",
          "given_",
          "given_Ord_Char",
          "given_Ord_Int_Show_Int"
        )
      )
  }

  def memberValues = {
    val found = rendered(
      """|object Vals {
         |  val b: Boolean = true
         |  val inferred = 3
         |  final val pinned = 3
         |  final lazy val later = 3
         |  final var moving = 3
         |  var flag: Boolean = false
         |}""".stripMargin
    )
    // An inferred `val` widens its literal to Int (2^32); a `final` one, lazy or not, keeps the
    // literal type 3, one value (a constant value definition); a `final var` is no constant value
    // definition: 3.9.0 types it Int (E007 against `def useX: 3 = x`), 2^32; a `var` reads as the
    // value it holds.
    (found("Vals.b") === "2")
      .and(found("Vals.inferred") === "2^32")
      .and(found("Vals.pinned") === "1")
      .and(found("Vals.later") === "1")
      .and(found("Vals.moving") === "2^32")
      .and(found("Vals.flag") === "2")
  }

  def patternValues = {
    val found = rows("object Vals { val (left, right) = (true, false) }")
    // Each name's type is a part of the right-hand side's, which only inference finds.
    refused(found("Vals.left"), "a name of a pattern definition")
      .and(refused(found("Vals.right"), "a name of a pattern definition"))
  }

  def localValues =
    rows("object Vals { def local(x: Int): Int = { val y = x; y } }").keySet === Set("Vals.local")

  def siblingDeclarations = {
    val found = rendered("trait Decls[A] { val one: A; val two, three: A }")
    // Each name has the other two as slots of A: 2.
    (found("Decls.one") === "2").and(found("Decls.two") === "2").and(found("Decls.three") === "2")
  }

  def ownSlot = {
    val found = rendered(
      """|trait HasDefault[A] { val default: A }
         |class Default[A](a: A) extends HasDefault[A] { val default: A = a }
         |trait HasSeed[A] { def seed: A }
         |class Seeded[A](a: A) extends HasSeed[A] { val seed: A = a }
         |class Twins[A](a: A) { val m1, m2: A = a }
         |class TwinsO[A](a: A) extends Twins[A](a) { override val m1: A = a; override val m2: A = a }
         |""".stripMargin
    )
    // Each open class's value implements the inherited declaration: a alone, 1. Of two names of
    // one definition, the other is an open member (TwinsO overrides both): a or m2, 2.
    (found("Default.default") === "1")
      .and(found("Seeded.seed") === "1")
      .and(found("Twins.m1") === "2")
      .and(found("Twins.m2") === "2")
  }

  // The declaration's own name is no slot; nothing else of Boolean is in scope: 2.
  def abstractVar = rendered("trait V { var flag: Boolean }")("V.flag") === "2"

  private val anonymous = getter + """
    |abstract class Base[A] { def seed: A; def other: A = seed }
    |trait HasPlate[A] { val plate: A }
    |object Anon {
    |  def held[A](a: A): HasPlate[A] = new HasPlate[A] { val plate: A = a }
    |  def mk[S, A](f: S => A): Getter[S, A] = new Getter[S, A] { def get(s: S): A = f(s) }
    |  def two[S, A](f: S => A, g: S => A): (Getter[S, A], Getter[S, A]) =
    |    (new Getter[S, A] { def get(s: S): A = f(s) }, new Getter[S, A] { def get(s: S): A = g(s) })
    |  def based[A](a: A, b: A): Base[A] = new Base[A] { def seed: A = a }
    |  val first: Getter[Int, Int] = new Getter[Int, Int] { def get(s: Int): Int = s }
    |}""".stripMargin

  def anonymousMembers = {
    val found = rendered(anonymous)
    // f(s): 1, not 2 (the implemented Getter.get is no slot). Int => Int: 2^(32·2^32) = 2^(2^37).
    // An anonymous class whose only member is a value is a scope too: a alone, 1.
    (found("Anon.mk.<new#1>.get") === "1")
      .and(found("Anon.first.<new#1>.get") === "2^(2^37)")
      .and(found("Anon.held.<new#1>.plate") === "1")
  }

  def anonymousOrdinals = {
    val found = rendered(anonymous)
    // f(s) or g(s) in each: 2.
    (found("Anon.two.<new#1>.get") === "2").and(found("Anon.two.<new#2>.get") === "2")
  }

  // `other` is concrete in Base and no subclass of the anonymous class replaces it: its default
  // re-enters `seed`, so it adds nothing to a or b.
  def anonymousFinal = rendered(anonymous)("Anon.based.<new#1>.seed") === "2"

  private val hidden = """
    |abstract class Hide[A](h: A) { def peek: A = h; def seed: A }
    |object Hidden {
    |  def shadowed[A](a: A, b: A): Hide[A] = new Hide[A](a) { val a: A = b; def seed: A = peek }
    |  def named[A](a: A, b: A): Hide[A] = {
    |    final class Impl extends Hide[A](a) { val a: A = b; def seed: A = peek }
    |    new Impl
    |  }
    |  def applied[A](f: Boolean => A): Hide[A] =
    |    new Hide[A](f(true)) { val f: Boolean = false; def seed: A = peek }
    |  def plain[A](a: A, b: A): Hide[A] = new Hide[A](a) { def seed: A = peek }
    |}""".stripMargin

  // `peek` returns h, the argument the template passed: where a member hides a name the argument
  // reads, the body cannot write the argument again, so `peek` is one more value (shadowed: a or b,
  // 2, never 1; applied: f(true), never 0) — refused. Where nothing hides it, `peek` is `a`: a or b,
  // 2.
  def hiddenState = {
    val found = rows(hidden)
    val reason = "open class with hidden constructor state: Hide.h"
    refused(found("Hidden.shadowed.<new#1>.seed"), reason)
      .and(refused(found("Hidden.shadowed.<new#1>.a"), reason))
      .and(refused(found("Hidden.named.Impl.seed"), reason))
      .and(refused(found("Hidden.applied.<new#1>.seed"), reason))
      .and(found("Hidden.plain.<new#1>.seed").render === "2")
  }

  // `peek` returns what Mid passed on, and an import of the body may rebind the argument's name:
  // neither is followed, so both are refused (a or b, 2, in each, read through the argument).
  def passedLevels = {
    val found = rows(
      """|abstract class Hide[A](h: A) { def peek: A = h; def seed: A }
         |abstract class Mid[A](x: A) extends Hide[A](x)
         |object Passed {
         |  def mid[A](a: A, b: A): Hide[A] = new Mid[A](a) { def seed: A = peek }
         |  object Box { val a: Boolean = true }
         |  def imported(a: Boolean, b: Boolean): Hide[Boolean] =
         |    new Hide[Boolean](a) { import Box.*; def seed: Boolean = peek }
         |}""".stripMargin
    )
    val reason = "open class with hidden constructor state: Hide.h"
    refused(found("Passed.mid.<new#1>.seed"), reason)
      .and(refused(found("Passed.imported.<new#1>.seed"), reason))
  }

  def valueBinders = {
    val found = rendered(
      getter + """
        |object Ctx {
        |  given lifted: (Boolean ?=> Getter[Unit, Boolean]) =
        |    new Getter[Unit, Boolean] { def get(s: Unit): Boolean = summon[Boolean] }
        |  val held: Boolean ?=> Getter[Unit, Boolean] =
        |    new Getter[Unit, Boolean] { def get(s: Unit): Boolean = summon[Boolean] }
        |  val plain: Getter[Unit, Boolean] = new Getter[Unit, Boolean] { def get(s: Unit): Boolean = true }
        |}""".stripMargin
    )
    // The context binder is a Boolean the member reads: Boolean => Boolean, 4; without one, 2.
    (found("Ctx.lifted.<new#1>.get") === "4")
      .and(found("Ctx.held.<new#1>.get") === "4")
      .and(found("Ctx.plain.<new#1>.get") === "2")
  }

  def overriddenBinders = {
    val found = rendered(
      getter + """
        |trait Holder { val held: Boolean ?=> Getter[Unit, Boolean] }
        |object Untyped extends Holder {
        |  val held = new Getter[Unit, Boolean] { def get(s: Unit): Boolean = summon[Boolean] }
        |}""".stripMargin
    )
    // The expected type is the declaration's: its binder is in scope, 4; the value's type is that
    // declaration's, 4 too.
    (found("Untyped.held.<new#1>.get") === "4").and(found("Untyped.held") === "4")
  }

  private val literals = getter + """
    |object Literal {
    |  def one[A]: A ?=> Getter[Unit, A] = (x: A) ?=> new Getter[Unit, A] { def get(s: Unit): A = x }
    |  def anon[A]: A ?=> Getter[Unit, A] =
    |    (_: A) ?=> new Getter[Unit, A] { def get(s: Unit): A = summon[A] }
    |  def local[A]: A ?=> A = (x: A) ?=> { def g: A = x; g }
    |  def named2[A]: (A, A) ?=> Getter[Unit, A] =
    |    (x: A, y: A) ?=> new Getter[Unit, A] { def get(s: Unit): A = y }
    |  def two[A]: (A, A) ?=> Getter[Unit, A] =
    |    (_: A, _: A) ?=> new Getter[Unit, A] { def get(s: Unit): A = summon[A] }
    |  val held: Boolean ?=> Getter[Unit, Boolean] =
    |    (b: Boolean) ?=> new Getter[Unit, Boolean] { def get(s: Unit): Boolean = b }
    |  given lifted: (Boolean ?=> Getter[Unit, Boolean]) =
    |    (b: Boolean) ?=> new Getter[Unit, Boolean] { def get(s: Unit): Boolean = b }
    |}""".stripMargin

  // A right-hand side that already is a context-function literal is not wrapped again (Scala 3
  // reference, context functions): its parameters are the clause's binders, once — givens in its
  // body, named or not. One A: 1; x or y: 2; one Boolean: Boolean => Boolean, 4. Two anonymous
  // binders of one type are ambiguous for summon.
  def contextLiterals = {
    val found = rows(literals)
    (found("Literal.one.<lambda#1>.<new#1>.get").render === "1")
      .and(found("Literal.anon.<lambda#1>.<new#1>.get").render === "1")
      .and(found("Literal.local.<lambda#1>.g").render === "1")
      .and(found("Literal.named2.<lambda#1>.<new#1>.get").render === "2")
      .and(found("Literal.held.<lambda#1>.<new#1>.get").render === "4")
      .and(found("Literal.lifted.<lambda#1>.<new#1>.get").render === "4")
      .and(refused(found("Literal.two.<lambda#1>.<new#1>.get"), "ambiguous anonymous evidence"))
  }

  def enumCases = {
    val found = rendered(
      """|enum Tree[+N] {
         |  case Leaf(value: N)
         |  case Node(left: N, right: N)
         |  case Empty
         |}""".stripMargin
    )
    // The cases take the enum's binder: value alone, 1; (N, N) from left and right, 2·2 = 4. A
    // singleton case is a value, with no constructor.
    (found("Tree.Leaf.<init>") === "1")
      .and(found("Tree.Node.<init>") === "4")
      .and(found.contains("Tree.Empty.<init>") must beFalse)
  }

  // 3.9.0 reads `T` here as the outer Boolean (neither the enum's nor its companion's members are
  // in scope of a case): the position's value space, 2.
  def enumScope =
    rendered("object Scoped { type T = Boolean; enum E { case A(x: T) } }")("Scoped.E.A.<init>") ===
      "2"

  // 3.9.0 gives a case with a parent of its own and no type parameters the enum's binders
  // (`case class D[A](y: A) extends E[Int]`): y's slot from y alone, 1, never the outer Boolean's
  // 2; two slots from y and z, 4 (never an unresolved A). A case's own binders stand.
  def enumParents = {
    val found = rendered(
      """|object S {
         |  type A = Boolean
         |  enum E[A] {
         |    case C(x: A)
         |    case D(y: A) extends E[Int]
         |  }
         |}
         |enum Two[A] { case K(y: A, z: A) extends Two[Int] }
         |enum Own[A] { case F[B](b: B) extends Own[B] }
         |""".stripMargin
    )
    (found("S.E.C.<init>") === "1")
      .and(found("S.E.D.<init>") === "1")
      .and(found("Two.K.<init>") === "4")
      .and(found("Own.F.<init>") === "1")
  }

  def templateConstructors = {
    val found = rendered(
      """|trait Monoid[A] { def empty: A }
         |abstract class Base[A](val a: A)
         |trait Param[A](val x: A)
         |trait Plain[A]
         |object Instances {
         |  given listMonoid[A]: Monoid[List[A]] with { def empty: List[A] = Nil }
         |  given unitMonoid: Monoid[Unit] with { def empty: Unit = () }
         |}""".stripMargin
    )
    // Each type-parameter slot from its own parameter: 1; a class with no parameter builds one
    // value: 1. A trait without a parameter clause and an unconditional given (an object) have no
    // constructor.
    (found("Base.<init>") === "1")
      .and(found("Param.<init>") === "1")
      .and(found("Instances.listMonoid.<init>") === "1")
      .and(found.contains("Plain.<init>") must beFalse)
      .and(found.contains("Instances.unitMonoid.<init>") must beFalse)
  }

  // A context bound's evidence has no name: a reason names the position by the evidence it holds.
  def evidencePosition =
    refused(
      rows(
        """|trait Ev[A] { def a: A; def n: Int }
           |object O { given inst[A: Ev]: Ev[Option[A]] with { def a: Option[A] = None; def n: Int = 0 } }
           |""".stripMargin
      )("O.inst.<init>"),
      "transparent ground values: evidence Ev[A]"
    )

  private def bySignature(code: String): Map[(String, String), String] =
    entries(code).map(entry => (entry.kind, entry.signature) -> entry.count.render).toMap

  def secondaryConstructor = {
    val found = bySignature(
      """|final case class Box[A](a: A, b: A) { def this(a: A) = this(a, a) }
         |final class K[A](val x: A) {
         |  val m: A = x
         |  def this(u: Unit, y: A) = this(y)
         |}""".stripMargin
    )
    // The primary's arguments (A, A) from a alone: (a, a), 1 — the secondary constructor is no
    // member, so Box stays a product. The primary's two slots from two parameters: 4. K's member m
    // is not in scope of the self-invocation (the instance does not exist yet): y alone, 1.
    (found(("method", "this(a: A): Box[A]")) === "1")
      .and(found(("constructor", "Box[A](a: A, b: A)")) === "4")
      .and(found(("method", "this(u: Unit, y: A): K[A]")) === "1")
  }

  // A secondary constructor calls the primary one with a Boolean from u: 2. Read as a value of the
  // class, an open class's overridable `flag` (or a subclass's override) would double it, 4 — but
  // no constructor chooses a member.
  def secondaryOpen = {
    val found = bySignature(
      """|class Open(val n: Boolean) { def this(u: Unit) = this(true); def flag: Boolean = n }
         |abstract class AbsM(val n: Boolean) { def this(u: Unit) = this(true); def flag: Boolean }
         |final class Closed(val n: Boolean) { def this(u: Unit) = this(true) }
         |""".stripMargin
    )
    (found(("method", "this(u: Unit): Open")) === "2")
      .and(found(("method", "this(u: Unit): AbsM")) === "2")
      .and(found(("method", "this(u: Unit): Closed")) === "2")
  }

  // The body after the self-invocation is typed as a function body of the class ([30] Constructor
  // Definitions): `pick` reads the member m, `pick2` the class parameter a — a Boolean of a (m is
  // concrete code of a final class): 4 each, as `under` is under `if (a)`.
  def secondaryBody = {
    val found = rows(
      """|final class K(a: Boolean) {
         |  val m: Boolean = !a
         |  def this(x: Unit) = {
         |    this(true)
         |    def pick(y: Unit): Boolean = m
         |    def pick2(y: Unit): Boolean = a
         |    if (a) { def under(y: Unit): Boolean = true; () }
         |  }
         |}""".stripMargin
    )
    (found("K.<init#1>.pick").render === "4")
      .and(found("K.<init#1>.pick2").render === "4")
      .and(found("K.<init#1>.<block#1>.under").render === "4")
  }

  // The primary's arguments as one type: (Boolean, Seq[Boolean]) for a by-name and a repeated
  // parameter, from u: Booleans and countably many sequences, ω; none: Unit, 1; a context bound's
  // evidence last: (A, Ev[A]) from b and the secondary's own evidence, 1 · 2^2 (an Ev[A] is one of
  // two values, chosen as a function of the evidence's), 4 — 1 without the bound.
  def primaryArguments = {
    val found = bySignature(
      """|final class P(f: => Boolean, xs: Boolean*) { def this(u: Unit) = this(true) }
         |final class N() { def this(u: Unit) = this() }
         |final case class Ev[A](flag: Boolean)
         |final class R[A: Ev](a: A) { def this(u: Unit, b: A) = this(b) }
         |final class R2[A](a: A) { def this(u: Unit, b: A) = this(b) }
         |""".stripMargin
    )
    (found(("method", "this(u: Unit): P")) === "ω")
      .and(found(("method", "this(u: Unit): N")) === "1")
      .and(found(("method", "this(u: Unit, b: A): R[A]")) === "4")
      .and(found(("method", "this(u: Unit, b: A): R2[A]")) === "1")
  }

  def identities = {
    val code = """
      |trait Named[A] { def name: Boolean }
      |object Twice {
      |  given Named[Int] = new Named[Int] { def name: Boolean = true }
      |  given Named[Unit] = new Named[Unit] { def name: Boolean = false }
      |}""".stripMargin
    def keys(text: String) = {
      val tree = dialects.Scala3(text).parse[Source].get
      entries(text).map(entry => EoBaseline.identity(entry, tree))
    }
    val secondaries = """
      |final class S(a: Boolean) {
      |  def this(x: Unit) = { this(true); def helper(y: Unit): Boolean = a }
      |  def this(x: Unit, z: Unit) = { this(false); def helper(y: Unit): Boolean = a }
      |}""".stripMargin
    // Both members are `Twice.<given>.<new#1>.name: Boolean` once the lines are dropped: the
    // enclosing given alias tells them apart, and moving the file's lines moves no identity. The
    // two secondary constructors' helpers are `S.<init#1>.helper` and `S.<init#2>.helper`.
    (keys(code).distinct.size === keys(code).size)
      .and(keys(code).toSet === keys("\n\n" + code).toSet)
      .and(keys(secondaries).distinct.size === keys(secondaries).size)
  }

}
