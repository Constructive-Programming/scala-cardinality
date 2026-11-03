# Definition inventory: rows beyond defs

The method report has rows for the definitions beyond `def`s and the primary constructors of
concrete classes (the few kinds that stay without a row are listed under *Not rows*). Each new
kind is measured by the rule its Scala translation already has; no counting rule is new. Sources keep the verification level the
literature pass gave them: [24] Scala 3 reference, *Translation of Enums and ADTs*, and [30] Scala
3 draft specification, ch. 5, both **Inspected (full text) — bibliographer** (bibliography T5).
Compiler fixtures were run with Scala 3.9.0 in this session (`$S/probes/b5/scalac`,
`$S/probes/b5/specfix`, `$S/probes/b5-fix2`, every spec fixture). Executable:
`DefinitionInventorySpec`.

## What is a row

| Definition | Row | Measured as |
|---|---|---|
| given alias `given n[T](using P): R = e` | `owner.n`, `given n[T](using P): R` (`owner.<given@L>`, `given[T](using P): R` when anonymous) | a method of its clauses; nullary when it has none |
| abstract given `given n: R` | `owner.n`, kind `declaration` | an abstract method |
| member `val` / `var` (template or package) | `owner.x`, `val x: T` / `var x: T` | a nullary method of its type; a `var` holds its value (contract §2) |
| abstract `val` / `var` | `owner.x`, kind `declaration` | an abstract nullary method |
| member of an anonymous class `new P { … }` | `owner.<new#k>.m` | a member of a final class |
| definition in a nested scope of a body | `owner.<block#k>.m`, `<case#k>`, `<lambda#k>`, `<for#k>` | as at the top of the body |
| parameterized enum case `case C(ps)` | `E.C.<init>`, `C(ps)` | a case class constructor |
| abstract class, trait with a parameter clause | `C.<init>` | a class constructor |
| conditional structural given `given n[T](using P): R with { … }` | `owner.n.<init>`, `n[T](using P)` | a class constructor |
| secondary constructor `def this(ps) = …` | `C.<init>`, kind `method`, `this(ps): C[T]` | a choice of the primary constructor's arguments |
| definition in a secondary constructor's body | `C.<init#k>.m`, k the constructor's ordinal in the class | a local definition of a method of the class |

**Not rows.** A value local to a block is a step of the definition around it, which that
definition's row counts; so is a nullary given alias there, which 3.9.0 translates to a lazy given
value (the scopes its right-hand side opens are still rows; a local alias with clauses is a method
and a row). A singleton enum case and an unconditional structural given are objects (a value, no
constructor), as Scala translates them; a trait without a parameter clause has no constructor.
Definitions inside a default argument or a named template's parent arguments are not indexed.

## Rules and derivations

**Givens.** Scala 3.9.0 types `given pair[A](using a: A): (A, A) = (a, a)` as `final given def
pair[A](using a: A): Tuple2[A, A]`, `given x: Boolean = true` as `final lazy given val x: Boolean`,
`given listMonoid[A]: Monoid[List[A]] with { … }` as a `given class listMonoid[A]()` beside a
`final given def` building it, and an unconditional `given unitMonoid: … with` as a `module given
val` (fixture `givenprint`, `-Xprint:typer`). So an alias is the method of its clauses (a nullary
one a value) and a conditional structural given is a class, whose constructor takes the given's
clauses. An alias implementing an abstract given or a parameterless declaration of its name is
that declaration (the contract's §2: the measured declaration is not its own implementation),
in an open template too.

An anonymous given is a member of the name 3.9.0 synthesizes from its type: `given_` and the
simple names the type spells (`given A = a` is `given_A`, `given Ord[List[T]]` is `given_Ord_List`,
`given (Int, String)` is `given_Int_String`, `given (String => Int)` is `given_String_to_Int`;
`GivenNames` lists every shape, each checked against `-Xprint:typer`, fixtures
`$S/probes/b5-fix2/gname`). `new C[A](x, y).given_A` compiles, so that name selects it, and an
anonymous `given A = a` in `final class C[A](a: A, b: A) extends T[A]` implements `T`'s `given
given_A: A`: the declaration is the given itself (a or b, 2, never 3), and for every other member
of `C` concrete code. A bounded wildcard in a given alias's type (`given Ord[? <: String]`, named
`given_Ord_String`) loses its bound in the parser: that given's name is not known, so it may be any
`given_…` member — its own identity against one is unresolved, and in a final template it is read
as implementing one (every inherited declaration of a final template is concrete code wherever it
is implemented).

**Inline definitions.** An `inline given` alias is a row read as an inline method (register D27):
when its body passes that rule (no compile-time operation, transitively, and no other channel to
the call site) it counts as its given twin, otherwise it is refused with the inline method's reason. A row in any scope of an
inline method's body (a block, function literal, case clause, comprehension, anonymous class) is
copied into every expansion: it counts in its typing context when the method's body passes and
carries `definition inside an inline method: …` when it does not
(`DefinitionInventorySpec.inlineGivens`, `NestedScopesSpec.inlineOwners`, fixtures
`$S/probes/b5-land`).

**Values.** A `val` is a parameterless member: "a stable member can only be overridden by a stable
member" ([30], *Overriding*), and a 3.9.0 fixture (`specfix/f08`) compiles a `val` implementing a
parameterless `def` of its name, so it is that declaration. Its signature is that of a nullary
method of its type. Without a written type it takes an overridden declaration's result (fixture
`specfix/f11` types `val held = new Getter {…}` overriding `val held: Boolean ?=> Getter[…]` with
the declaration's context function, so `summon[Boolean]` in its body finds the binder), and
otherwise its right-hand side's, as an inferred method result does (D19); a `final` or `inline`
`val` (lazy or not) with a literal keeps the literal's own type: "constant value definitions …
require an explicit `final` modifier" ([30], *`final`*). A `var` is no constant value definition:
3.9.0 types `final var x = 3` as `Int` (`def useX: 3 = x` fails with E007), so it is 2^32. The
names of a pattern (`val (a, b) = …`) are typed by the pattern, which syntax does not give: `?`.
Of a definition of several names (`val a, b: A`), each is its own row, and the others are slots of
its environment.

**Anonymous classes.** No class can name an anonymous class, so none extends it: it is a final
template. Its members read the enclosing captures, its parents' declarations it implements are its
own members (dropped, as for any final class), and an inherited concrete member is concrete code no
instance replaces (D19), never a capability.

That last step holds only where the concrete code computes from values the environment has. A
parent's constructor parameter no public field exposes (`abstract class Hide[A](h: A) { def peek:
A = h }`) holds the argument the template passed, evaluated where the template is declared. The
body reaches the same value by writing the argument again, unless a name the argument reads means
something else inside it: in `new Hide[A](a) { val a: A = b; def seed: A = peek }` the member `a`
hides the outer `a`, so `peek` is one more value (`seed` is a or b, 2) and a reading without it
would say 1. So where a level's own code reads such a parameter and the template does not pass it
directly with arguments whose names (other than its own constructor parameters) no member or
field of the template or of its levels, and no import of its body, rebinds, every row of the
template is refused: `open class with hidden constructor state: Hide.h`. This applies to any
template, named final classes too (`final class Impl extends Hide[A](a) { val a: A = b; … }`), and
an intermediate level passing a parent's argument is not followed.

A member that holds a hidden capture keeps it in the environment. In `def mk(n: Boolean): P = new
P(n) { override def m(b: Boolean): Boolean = !b }` with `class P(val n: Boolean)`, the inherited
field `n` hides the capture `n` (E049), yet the field holds it: `m` reads `b` and `this.n` — 16, as
a 3.9.0 run shows (`List(true, false)` for the two `mk`s); dropping both the capture and the field
as concrete code said 4. So a capture the target cannot name that a parent's field the template
can read (not private to the parent) was passed exactly (`new P(n)`, `extends P(n)`), or that an
own member is (`def seed: Boolean = b`, a capture hidden by the target's own parameter `b`), is put
back when the field's or member's type, read where it is declared (the field's with the arguments
the extends chain gives its parent), is the capture's own; at a type with one value (`seed: Any`,
`class PT[X](val n: X)` given `Any`) it shows nothing and adds nothing (D-top); any other reading of
one — a field passed `!n`, a parameter no field exposes, a member computing from it, a field at a
wider type, a field a nearer level overrides (`override val n = true`: the parent's constructor
still read `n`, and a strict val of the parent keeps it, a 3.9.0 run shows) — refuses the row
(`local capture held by a member that hides it: n`). Only members no instance replaces matter: an
open member is a capability of its own.

**Nested scopes.** A definition below a body's top block is indexed in the scopes around it: a
block, a function literal (whose parameters are parameters, supplied by whoever applies it, as a
method's are), a case clause, a for-comprehension, an anonymous class. Its environment is its
typing context, as for a definition of the top block: the parameters and captures of every
enclosing owner, each ranging over its type (environment-completeness §1) — the signature lambda
lifting gives it, its captures made parameters. Control flow is no part of that context: a branch,
a loop, a short-circuit operand, an argument the callee may never evaluate (a by-name parameter,
`o.getOrElse { … }`), a comprehension's guard and a dead branch (`if (false)`) leave every input
ranging over its type, as a method's callers leave its parameters (*Control flow* below).

Patterns change the context in two ways the reading does not follow. A case clause's binders and a
comprehension's are refused: a binder may be the scrutinee itself (`case b: T`) or a part of a
value the scope has, and two aliases of one value are one choice (contract §2); their types are
the pattern's, which syntax does not give either. A given binder (`case given A`) is one more
contextual value of the arm, refused the same way. The reason names the binder: `binder of an
enclosing case clause not read: a`, `… not read: given A`. And a pattern may add type equations:
where a type parameter of an enclosing method or class is in scope, matching a constructor, typed
or stable-identifier pattern constrains it (`case TInt()` of a `Ty[A]`, with `TInt extends
Ty[Int]`, makes `A` Int in the arm: 3.9.0 accepts `def h: A = 42` there, so `h` has 2^32
implementations, not the 0 a rigid `A` gives). Every count may change, 0 and 1 included, so each
is refused — `pattern of an enclosing case clause may refine a type: case TInt()` (`… for-comprehension
may refine a type: TInt()` for a generator) — except a result with one value (`Unit`: 1 whatever the
context, D-terminal). 3.9.0 refines a method's and a class's type parameters this way, through
extractor, typed and object patterns, but not abstract type members, path-dependent types, binders,
wildcards or literals (`$S/probes/b5-fix2/refine`); the reading does not check that the pattern's
class pins an argument (`case Some(_)` of an `Option[A]` pins none), so it refuses any such pattern
with a type parameter in scope, and none without one. Scala's GADT reasoning is not modelled: both
compilers have known soundness holes there (Parreaux, Boruch-Gruszecki & Giarrusso 2019, Scala
Symposium, **Metadata-verified**: the abstract reports both scalac and Dotty have GADT-related
type soundness holes). Reading a subclass that fixes its parent's type argument as a GADT case is
our inference; Kennedy & Russo 2005 (OOPSLA, **Metadata-verified**) is background: the abstract
reports GADTs are expressible through generics and subclassing.

A context-function literal's parameters are givens in its body, named or not (Scala 3 reference,
*Context Functions*), so they are contextual parameters of its `<lambda#k>` scope; an anonymous one
(`_: A`) is reached by implicit search only, and two of one type are ambiguous. An expression whose
expected type is a context function is wrapped in a literal only when it is not one already, so a
right-hand side that is such a literal binds the result's first clause through its own parameters
— once, never again as the definition's binders (`def one[A]: A ?=> Getter[Unit, A] = (x: A) ?=>
new Getter[Unit, A] { def get(s: Unit): A = x }`: `get` is x alone, 1, not 2). Each literal
directly in the body of the last binds the next clause; only the clauses past them wrap the
innermost body.

**Control flow.** The contract's question is how many implementations a signature admits in its
accessible environment, told apart "across admissible inputs" (§2). For a nested definition the
admissible inputs are the values of its parameters' and captures' types, as for a method: a
method's parameters are never narrowed by what its callers pass. The alternative — telling
implementations apart only across the inputs that reach the definition at run time — cannot be
applied consistently: a local `def h(x: Boolean)` the body calls only as `h(true)` would have its
own parameter narrowed too, and which inputs reach a function literal or a by-name argument
depends on code the reading does not have (`o.foreach { … }` runs only where `o` is non-empty,
`when(b) { … }` only where `when` evaluates its argument). So an `if` or `else` branch, a `while`
body, the operand of `&&` or `||` (infix or dotted), a case arm, a guard, a comprehension's body,
a by-name argument and a dead branch all leave the typing context as it is: `def g: Boolean = b`
under `case true` of `b` is a Boolean function of `b`, 4, as it is at the top of the body.

**Enum cases.** A parameterized case expands "analogous to a final case class in `E`'s companion
object" and, with neither type parameters nor `extends`, takes the enum's type parameters ([24],
rules 6 and 9; a case needs a parameter list: 3.9.0 rejects `case C[B] extends …`, E004). With an
`extends` clause and no type parameters of its own, it takes them only if "at least one of the
parameters `Ts` is mentioned in a parameter type in `<value-params>` or in a type argument in
`<parents>`" ([24], rule 7, **Inspected (full text) — bibliographer**); 3.9.0 does so (`enum E[A,
B] { case D(y: A) extends E[Int, Int] }` prints `final case class D[A, B](y: A) extends E[Int,
Int]`, `-Vprint:typer`), so the case's parameters read the enum's binders either way, never an
outer type of the same name. Its signature
is typed where the enum is declared: 3.9.0 finds neither the enum's body members (E006) nor its
companion's ("illegal reference … from enum case") there (fixture `enumscope`). Its constructor row
is the case class's.

**Abstract classes and traits.** Every class has a primary constructor, an abstract class's being
the one its subclasses call; a trait with a parameter clause has one its first implementing class
calls. Each is measured as a concrete class's constructor (the owner's convention: a position the
estimate reads counts its value space, a type-parameter slot the values supplied).

**Secondary constructors.** "The signature and the self constructor invocation of a constructor
definition are type-checked and evaluated in the scope which is in effect at the point of the
enclosing class definition, augmented by any type parameters of the enclosing class", and the
invocation "must construct a generic instance of the class" ([30], *Constructor Definitions*). The
invocation calls another constructor of the class, and every chain of them ends in the primary
one, so an implementation is a choice of the primary constructor's arguments over the secondary's
parameters (and the class bounds' evidence 3.9.0 adds to it): the row measures the tuple of the
primary's parameter types (a by-name one as its value, a repeated one as its `Seq`, `Unit` for
none), read in the class's enclosing scope with its binders. It is never a value of the class:
`class Open(val n: Boolean) { def this(u: Unit) = this(true); def flag: Boolean = n }` builds an
`Open` from a Boolean, 2, while a value of the open class `Open` is also any override of `flag`, 4.
The rest of its body is "a function body in the current class": its definitions see the class's
members and the secondary's parameters, named under `<init#k>`, k the constructor's ordinal among
the class's, so two secondaries' `helper`s are two rows. A secondary constructor is no member: a
case class with one stays the product of its fields.

## Identities

A row's name is its owners' path. A nested scope is named by its kind and its ordinal among the
scopes of that kind its parent path opens in the file (`<new#1>`, `<new#2>`), so two `loop`s of
two case arms, or the anonymous classes of two overloads, are two rows; a scope's frame id carries
the name too, since a braceless block starts where its first definition does. An anonymous given
alias is `<given@L>` as an anonymous structural given is; `EoBaseline.identity` drops the line and
puts the alias's clauses and type in the enclosing column, as for a structural given, so its rows
and those of its body stay apart (its synthesized member name decides what it implements, not its
row's name). A secondary constructor's body is `<init#k>` by the same ordinals, so the bodies of
two secondaries are two scopes. Ordinals move only when the definition around them changes.

## Limits (`?` and their reasons)

- A name of a pattern definition: `inferred result type not resolved: a name of a pattern
  definition`.
- A case clause's or a comprehension's binder: `binder of an enclosing case clause not read: x`,
  `binder of an enclosing for-comprehension not read: x`, `… not read: given A` for a given
  binder; a function literal's untyped parameter: `missing type of accessible value: x`.
- A pattern that may refine a type parameter in scope: `pattern of an enclosing case clause may
  refine a type: case TInt()` on every count but a one-valued result's; a pattern whose class pins
  no argument (`case Some(_)`) is refused as well.
- Parent state the template's members may not reach: `open class with hidden constructor state:
  P.h`.
- An anonymous class nested in a method of an open template whose parent declares a member of the
  same name as one of its own: `inherited override identity not resolved: m` (the outer template's
  declaration is another member, a reading this does not yet settle); an anonymous given whose
  synthesized name the parser loses, against a `given_…` declaration: `inherited override identity
  not resolved: given_…`.
- Every reason the analyzer already gives for the row's types (foreign names, abstract type
  members, open classes, …).
