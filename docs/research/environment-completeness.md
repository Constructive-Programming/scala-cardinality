# Environment completeness: outside producers, state, exports and inferred results

A signature's environment is complete when every value a body can reach either is in it or
provably adds no implementation. Six readings settle that for cats-eo 1.0.0's environments; each
refuses with a reason where its premise fails. Regressions: `EnvironmentCompletenessSpec` and, for
§5, `InferredResultsSpec` (under `core/src/test/scala/cardinality/analysis/`); their fixtures
compile with Scala 3.9.0 except where a name outside the sources is the point.

## 1. Concrete code that cannot be overridden adds nothing

**Rule.** A module or package member, a local definition, a private or final member, a member of a
final class or object, a concrete `var`, a given and an export forwarder are concrete code nothing
can replace. Such code is not an input: the measurement neither binds it nor reports it. The
blanket `qualified member environment not resolved` diagnostic (every outside module, whenever a
signature spelled `Boolean`) is gone.

**Derivation.** Let Γ be the supplied inputs (parameters of the target and its enclosing owners, the
abstract members of `this`) and τ the goal, every type in them readable, and let the solver answer
over Γ. An implementation over Γ plus fixed concrete code M, abstracted over Γ, is a closed term of
∀ᾱ. Γ → τ: M's members are closed subterms, in-model by the contract's assumption that helper
bodies obey the model (no effects, casts, runtime type tests, `null`). Its denotation is a
parametric inhabitant of that type, which the solver's canonical forms already count (≥ is
immediate: M is fixed, not an input). This is the abstraction theorem (Reynolds 1983) read through
representation independence (Mitchell & Plotkin 1988): how a helper computes is unobservable
through readable types. For ground goals it is ground-scalar-types' Lemma O: a closed helper over
transparent values is one of the functions the literal-built branches already enumerate.

**Readability is the premise.** The rule never speaks where a concrete producer could matter: a
class no public constructor builds (`receiver has no public constructor`), a callable whose
transparent result the solver does not eliminate (`Elimination of an opaque … callable … is
unsupported`), a name outside the sources (`declaration not supplied`) all stay `?` on their own.
An opaque type outside its scope is one value, and a non-public class is built by its
constructors as a public one (v1.md §2, modelling conventions; register D35).

**Not covered: overridable members.** Cross-check X5 (fixture `ow`): a concrete member of an open
class may be overridden in the same file, so it is a capability, not fixed code
(`Box.target` = 3, `FBox.target` = 2). The environment memo's "closed-world dispatch" (§3.0(1)) is
superseded. Reading those members as capabilities is the capability records' rule
([capability records](capability-records.md), register D20), not this one: `ow.Box.target` reads
3 and the final twin `FBox.target` 2.

## 2. A `var` holds the value it was given

The contract excludes mutation, so no program the model counts tells a `var` from the value it was
initialized with. A `var` parameter is a parameter; a concrete `var` member reads as its
initializer (an alias of a capture when the initializer is one, nothing new otherwise — a `null`
or uninitialized one is no in-model value); an abstract `var` is an abstract getter, a declared slot
(its setter returns `Unit`). No member overrides a mutable variable (Scala 3.9.0, E164), so a
`var` implements an inherited declaration in an open template too. The `mutable capture`
diagnostic is retired. `Cell[A](init) { var cur = init; def set(a: A): Unit; def get(): A }` has
`get = 1`: `cur` and `init` differ only after a mutation.

## 3. Names that exports and imports bind

- **An export defines members** (Scala 3 reference, export clauses): `export p.T` is a type alias
  of the template, ranking with its own declarations. It hides every outer declaration, beats an
  import of its own body (fixtures c3, c6), and is followed: named, renamed (`T => U` binds only
  `U`), and wildcard selectors less the names left out (`T as _`); a `given` selector exports no
  type. An object's exports and inherited members answer `import O.*`, `import O.T` and `O.T`
  (a member inherited from a parent with type parameters is refused, as a path carries none). A
  wildcard export whose prefix no supplied namespace answers, or an unsupplied ancestor, may define
  any name: refused (`wildcard export not supplied: T (wildcard export of p)`), never a fall-back
  to an outer binding. An ancestor's export clause defines only what its selectors name, or what a supplied
  namespace its wildcard opens holds: those names are refused (`inherited type member not resolved:
  T (exports of Base)`) rather than followed, and every other name is read past it (`trait P {
  export Src.T }` leaves `Boolean` the builtin). A path's first segment, a term, is refused under
  any selector that may spell it. A name the export binds through an import is reported through
  both (`declaration not supplied: ext.Obj.T (selected through named import Obj, exported by O)`).
- **An export of what the exported object only inherits** defines the member too: `export
  Types.O.*` with `O extends Base { type T = Boolean }` gives the body `T = Boolean`, which
  outranks a `T` the enclosing template inherits (Scala 3.9.0 types it `Types.O.T`).
- **A path through a value is no object.** A path's first segment that a scope binds to a
  parameter, `val` (a pattern's variable included), method or `given` (own or inherited) selects a member of that value's type:
  `export x.*` in `class C(val x: HasT)` never reads a same-named `object x` elsewhere, and the
  lookup refuses (`path through a value not followed: x`).
- **A protected member is not selected from outside.** `import O.*` and `export O.*` of an object
  that inherits a `protected type U` do not bring `U` to an unrelated scope (scalac rejects
  `UseExp.U`); private members are not inherited at all.
- **Definitions, exports, imports and inherited members hide outer terms.** A local class's member
  (every variable of a pattern definition included: `val (seed, z) = …`, `var (seed, z) = …`, `val
  seed @ _ = …`), extension method or export named like an enclosing method's parameter makes that
  name the member inside the class; the parameter has no qualified path and leaves the environment (`def seed: X =
  other` in a local class: `g(): X = seed` is 1; with `extension (i: Int) def seed: X` scalac reads
  `seed` as the extension). An inner import or an inherited member of the same name makes the
  simple name ambiguous (E049), so the parameter is out of reach there too: an inherited definition
  drops it (`final class D extends Base` with `Base.seed`: `h(): X` is 0), an import or export that
  may bind it is refused (`local capture may be hidden by an import: seed`), as is an inherited
  member a parent outside the sources may declare.
- **A `given` selector binds no type name**: `import p.{given T}` and `import p.given` bring given
  instances, which are terms (fixture: `def y(): Unit = ()` compiles after `import Defs.{given
  Unit}` with `Defs.Unit = Boolean`). An unimport (`N as _`) and a rename leave `N` out of the
  wildcard (fixture: `x.getOrElse(y)` compiles, so `Option` is the standard one), also when the
  wildcard's prefix is not supplied (`import yyy.{Boolean as _, *}` leaves `scala.Boolean`). An
  import whose prefix is no path (`import this.x.Unit`) is refused for every name it spells.

The precedence is the specification's, with exports among the definitions: local, inherited or
exported definition > explicit import > wildcard import > another file's package member > root
import.

## 4. By-name parameters

`=> A` reads as `A`: call-by-name and call-by-value differ only in when and how often the argument
is evaluated, which the excluded effects and divergence make unobservable (Knvl: `() → A ~ A`).

## 5. Inferred results, from syntax alone

A method without a declared result is typed where syntax fixes the type, and refused otherwise with
the form named (`inferred result type not resolved: <form>`):

- **An overriding member takes the overridden declaration's result.** Scala 3's namer types a
  member written without a result by every base class's non-private member of its name whose
  parameter types match, seen from the subclass, intersected; only then by its body (fixtures:
  `Some(1)` overriding `Option[Int]` is an `Option[Int]`; `def g(x: Boolean) = true` beside a
  parent's `g(x: Int): Any` and `def h = true` beside a parent's `private def h: Any` are
  `Boolean`). The reading follows that exactly where it can: a candidate is a supplied ancestor's
  non-private method of the same name, clause shape and parameter types (identities of the
  declarations the types name; a method's binders matched by position; a binder an `extends`
  clause binds to a written constructor read as that constructor; by-name and repeated markers
  kept). A known differing parameter type makes an overload, no candidate; an identity not known
  is refused. One candidate is read in its parent's scope under the chain's arguments, with the
  subclass as `this`, so an abstract type member it names is the subclass's (`O.T = Boolean`).
  Any base class outside the sources that declares or may declare the name refuses, since Scala
  intersects it in: `Any` and `Object` for every template and for the top-level object
  (`equals`, `hashCode`, `toString`, `clone`, …; fixture: `def clone() = new C` is an `Object`),
  `Product` and `Equals` for a case class or object and an enum (`productPrefix` is a `String`),
  `scala.reflect.Enum` for an enum, a stdlib parent the model does not list the name under (`Int
  => Boolean`'s `apply`), an unsupplied one (`BooleanSupplier`). Several candidates, one whose own
  result is inferred, a dependent result, an extension method of a template with parents (it may
  override an inherited extension), and an `override` with no candidate are refused. Besides
  methods, an abstract `var` declares a getter and a setter `f_=(x: T): Unit`, and a given (with or
  without `using` clauses) a method of its name: a method implementing one is typed by it
  (fixtures: `def f = true` implementing `var f: Any` or `given f: Any` is an `Any`; `def f_=(x:
  Any) = true` is a `Unit`, its value discarded).
- **A literal** is its class by root-anchored path (`true` is `scala.Boolean` whatever `Boolean`
  names in scope); `null` is refused.
- **An ascription** `e: T` is `T`, unless `T` is a singleton or literal type, written or behind a
  source alias, which the compiler widens (fixture: `type One = 1; def f = (1: One)` is an `Int`),
  or a union, which is refused here.
- **`new C[T](…)`** is `C[T]`; `new C(…)` is `C` when `C` takes no type parameters. A class
  outside the sources is read as written, so its own reading counts it (`new Object`: 1) or names
  its absent declaration (`new StringBuilder`); one the stdlib model gives type parameters
  (`new Some(1)`) needs them.
- **A creator application** `C[T](…)` (or `C(…)` of a monomorphic class) is `C[T]` when every
  constructor clause is applied, the companion declares and inherits no `apply`, and no scope
  nearer than the class's, nor the class's own beside it, binds a term `C` — a parameter, a
  definition, an inherited member, an export or an import (`import A.*` may bring `def C`).
- **A call** `f[T](…)` or `f(…)` reads the nearest binding of `f`, scope by scope, in Scala's
  precedence: a parameter; a template's inherited members (including `Any`'s, and the top-level
  object's), which overload its own; its own definitions; an export, which defines one; an import;
  another file's member of the package, which every binding of the reference's own file outranks.
  Only own definitions are read: exactly one method of the name with a declared result, every
  explicit clause applied, its declared result under the explicit type arguments. An inherited
  member (fixture: `f("s")` in a class that defines `f(Int)` calls the parent's `f(String)`), an
  export, an import (fixture: `import IA.g` beats another file's package `g`), an extension
  method, an overload, a value (a pattern's function value `val (f, z) = …` is applied through
  `apply`), a parameter, a dependent result, a callee whose result is itself inferred, a recursive
  call and a call needing type-argument inference are refused, each naming why. The declared
  result is widened as Scala widens an inferred one (fixtures: `def h = one` with `one: 1` is an
  `Int`; with `1 | 2` too; `this.type` widens to the class, `E.A.type` to `E`): a literal type is
  its class, an object's singleton itself (one value), a union of classes is kept; any other
  singleton — a value's, `this.type`, an enum case's, an alias of a literal, a union holding one —
  is refused (`singleton result of a call of f`).
- A `transparent inline` method's inferred result is refused: call sites see the right-hand side's
  own type.

Matching argument types against parameters is inference (it was prototyped against two compiler
fixtures, but without compiler metadata it is not syntax) and is not done.

## Limits left as `?`

- Overridable concrete members of open classes (X5) are capabilities, read by the capability
  records (D20); their own limits (possible views, polymorphic members) are listed there.
- `Optional`'s `Fused` leaf with a free `M0` (`sealed subtype Fused binds M0 to no slot of
  Optional`): the environment memo reads it as an existential summand, but no eo count can move
  (the summand's existential sits in result positions, which stay `?`), so the sound refusal stays.
- `Slice`'s `private[data]` constructor: callable inside `data`, where its fields are read; elsewhere
  no constructor builds it (rule B, `receiver has no public constructor: …Slice`); the polarity
  refinement frees no eo row (the fields are hidden too).
- A package-level export of another file is not followed (a builtin it may rebind is refused).
- Exported or imported term names that may hide a local capture, inherited members a parent outside
  the sources may declare under its name, the names an ancestor's export clauses define, and the
  type-member closure of an exporting ancestor's members, are refused rather than read. A
  pattern-bound value has no declared type of its own: a path through it is refused. A path through a value (`export x.*` with `x` a
  parameter) is refused, not read as the value's type members.

## References

- Scala 3 reference, *Export clauses* (`other-new-features/export.md`): **Inspected (extraction)**
  (cross-check): "An export clause defines aliases for selected members of an object"; type members
  are aliased by type definitions, term members by method definitions.
- Scala 3 draft specification, ch. 2 \[28\], and SLS 2.13, ch. 2 \[32\]: **Inspected (full text)**
  (verifier) — the precedence list and inner-scope shadowing.
- Reynolds 1983 \[9\]: **bibliography-verified**. Mitchell & Plotkin 1988: **classical** (C-10).
- Pitts & Stark 1998 (HOOTS, CUP): **Metadata-verified**; the abstract reports that local state
  must be handled for representation independence — why the exclusion of mutation is load-bearing,
  not the `var`-as-value reading itself, which rests on the contract.
- Scala 3.9.0 compiler fixtures (this session): E164 (no member overrides a `var`), override result
  typing (parameter types matched, private members and overloads not overridden, `Any`/`Object`,
  `Product`/`Equals` and foreign parents intersected in, an abstract member seen from the
  subclass, an inherited extension method, an abstract `var`'s getter and setter and an abstract
  given implemented by a method), singleton ascriptions and call results widened (literal types,
  unions of literals, aliases, `this.type`, enum cases; a union of classes kept), a call
  resolving to an inherited overload or an import, given selectors and unimports, exports beating
  outer definitions and imports, an export of an inherited member, a protected inherited member not
  importable, a local member, extension method, inner import or inherited member hiding an
  enclosing parameter (E049).
