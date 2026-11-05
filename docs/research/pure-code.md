# Code without side effects: vars, throws and throwables

The counting contract ([v1 §2](../plans/v1.md#_2-agreed-counting-contract)) excludes effects,
mutation-dependent behavior and exceptions. The owner's directive makes the consequence explicit:
the analysis assumes the measured code has **no side effects**. A `var` is read as a `val`, throwing
is ignored, and other effects are excluded like the universal members. This page says how each
reading follows, and where Scala's own typing rules still decide (register D26).

## 1. A `var` is the value it holds

With no mutation, no program can tell a `var` from the value it was given. So each reading treats a
`var` exactly like a `val`, and no refusal or reason mentions mutability.

- A `var` constructor parameter is a field holding its argument, like a `val` parameter. Scala adds
  a getter for both and a setter `x_=` for the `var`, and the setter is an effect (\[30\] ch. 5). So
  a public `var` field is observable in an open class's or final class's record. It is a product
  field of a case class or a sealed leaf. A `private` or `protected` field stays hidden, as a
  `val` would. A plain class's parameter written with neither `val` nor `var` is no field at all
  (scalac E173: only the class's own code reads it), so a sealed leaf with one is refused (`sealed
  subtype hides a parameter`): values that differ only there may be one value.
- An abstract `var` member is a declared slot: its getter. A concrete `var` member is the value it
  was initialized with. Scala lets nothing override a `var` (E164), so a concrete `var` is derived
  code: a record does not count it as a free choice, and it implements an inherited declaration in
  any template.
- Scala's typing rules still hold. A `var` is not a stable path: `p.x` is a stable type only if
  `x` is a `val` or an `object` (\[29\] ch. 3), and scalac rejects `x.type` for a `var` (E083, "not an
  immutable path"). The refusal says `not a stable path: x is a var`. That is a typing fact, not a
  claim about mutation.

## 2. A throw supplies nothing

A throw never returns, so it is no value, and a body that only throws is no implementation. A term
*only throws* when it is one of these (`Throws`):

- `throw e`;
- the root imports' `???` or `sys.error(…)`, both declared to return `Nothing`;
- a block whose result only throws;
- a conditional or match in which every branch only throws.

The `???` and `sys` cases apply only where no nearer parameter, definition, import or inherited
member binds the name. Scala types each such term `Nothing`, and so does the analysis:

- **Inferred results.** A right-hand side that only throws gives the type `Nothing`. A conditional
  with one throwing branch has the other branch's type, because `Nothing` adds nothing to a join.
  So `def boom = throw e` counts 0, and `def f(c: Boolean) = if (c) 1 else throw e` is an `Int`.
- **Concrete code nothing can override** (module members, local and private definitions, members
  of final templates) is never read as a producer (D19). A throwing helper `def any[A]: A = throw e`
  therefore inhabits no type: `def f[B]: B` stays 0. Its throw supplies nothing, so the template
  around it is built and its other members count as usual: in `object O { def fail(msg: String):
  Nothing = throw …; def f(b: Boolean): Boolean = ??? }`, `f` has 4 implementations.
- **An overridable member whose default only throws** reads exactly as its abstract twin. The
  default is no behavior, so the member is a free capability of its declared type, every behavior
  of which an override realizes (D20, X5): `class C { def get: Boolean = ???; def m(b: Boolean):
  Boolean = ??? }` has 16 implementations of `m`, as `abstract class C { def get: Boolean; … }`
  has. Where that type is empty (`Nothing`, `Int => Nothing`, `(Nothing, Int)`), no override
  implements it either, so no instance exists and the context is vacuous: `class K1 { def k(x:
  Int): Nothing = ???; def m(b: Boolean): Boolean = ??? }` has 1 implementation of `m`, as its
  abstract twin has. The default instance (`new K1`) is not a value of the record: if it were,
  `new C` would be a value of C's, its `get` would throw on it, and no implementation of `m` could
  read `get` (4, not 16). One reading serves both: a throwing default is never one of the
  member's behaviors. Subtyping agrees: every `Q extends P` whose `k` only throws is a `P`, whose
  abstract `k: Int => Nothing` no pure code implements, so `P` and `Q` count alike (1). The reading
  is decided at the record's or receiver's type arguments: `K2[A] { def k(x: A): Nothing = ??? }`
  read as `K2[Int]`, or extended as `K4 extends K2[Int]`, is `K1`.
- **A constructor that only throws builds no instance.** Every constructor, a subclass's included,
  runs the primary constructor of each class and trait of its linearization: each parent's
  arguments, each strict `val` or `var` initializer and each statement of the body (a 3.9.0 run:
  `new IV { override val x = true }` still runs `IV`'s `val x = ???` and throws). Where one of
  them only throws, no instance exists. So the template's record is empty (as an input it is
  vacuous, 1; as a goal, 0), and its members are never called: their count is 1. That holds for
  the templates an instance carries too: an inner class's outer instance, a local class's
  enclosing receiver, and a member object's owning instance. A top-level definition lives in the
  synthetic object of its file and package, built with every strict top-level `val` of them (a
  3.9.0 run: calling a top-level `def` runs its file's throwing `val`, another file's not). A
  class or object that a *static* object (one nested only in objects and packages) encloses is
  built without it (a 3.9.0 run calls `O.C` while `O`'s initializer throws). A `lazy val` or a
  method runs only when called, so it builds nothing. A name a constructor evaluates is read in
  its own level's scope: the level's parameters, members and inherited members (never a
  subclass's: a 3.9.0 run throws for a trait's `???` in a subclass that defines `???`), the
  scopes around it, and the imports written before it (an export binds throughout its template).
  A parent's arguments see no member, since no `this` exists yet. The measured declaration's own
  body is excluded: it is the choice being counted. A constructor row keeps its convention: it
  reads its parameters. An inline member of a static object or of a file's top level is the
  exception: it is expanded where it is called, and the expansion builds the object only where it
  selects one of its members (3.9.0 runs: `K.f(true)` returns while `K`'s initializer throws, and
  prints no `K init`; `b && K.z` builds `K`). So it is counted as in a built owner whose own
  members supply nothing — an object or a file declares no slot and has no open member, so none
  of them is an input anyway: `object K { val z: Boolean = ???; inline def f(b: Boolean): Boolean
  = !b }` has 4 implementations of `f` (an inline given `Int` of `K`, 2^32). A class's, a trait's
  or a member object's inline member still needs the receiver its call is made on: 1. Rows nested
  in such an inline body follow it (register D28).
- **A throw's operand reads nothing that a result is built from.** A final implementation that
  reads a hidden capture only to build the exception it throws is not refused. The view rule (C6)
  still sees a re-entry inside the operand, because Scala evaluates the operand before throwing.
- **The stored side** never reads an initializer's value, so a throwing initializer changes no row.
  A value row is `min(1, |T|)` for its declared type `T`, read as whether `T` has a value: a `val`,
  `lazy val` or `var` of an empty declared type (`Nothing`, `Either[Nothing, Nothing]`, `Int =>
  Nothing`, an alias of any) holds no value, 0; one of an inhabited type holds its one value, 1; one
  with no declared type holds one, since the initializer is still never read; and one whose declared
  type may be empty carries that type's reasons. Inhabitation is not the stored algebra's capacity.
  The algebra counts a function from an empty domain 0, because it never runs
  ([type-arithmetic.md](../type-arithmetic.md) §3), but the absurd function is a value of it: `val
  f: Nothing => Int` holds one, 1, as the method side counts the same `val`. So a function type has
  a value when its result type has one or a parameter type is empty, and none when every parameter
  type has one and its result type none; a tuple when every element has one, a union when a branch
  has one. A witness decides the rest without reading the whole type (`ValueRows`): a
  standard-library type the library gives a value of whatever its arguments (the top and marker
  types, `String`, `BigInt`, the comparisons, the throwables, the containers with an empty one),
  spelled where no source binds the name; an enum with a parameterless case without `extends`; and a
  sealed family whose case object is a member of every instantiation (eo's `PSVec[Json]` holds
  `PSVec.Empty` whatever circe's `Json` is). Anything else reads the algebra: 0 with no reason is
  empty unless the read met a function from an empty domain, and any other number with no reason is
  inhabited unless it met an intersection or a refinement, which the algebra bounds from above (`Int
  & String` is empty, its bound 2^32); each is refused (`value not decided: P holds a function from
  an empty type`, `… holds an intersection or refinement`). A class's number counts what its
  constructor stores, not whether one is built, so a template the read meets must also be built, by
  the construction rule above: `final case class K() extends Base(???)` stores nothing (1) and is
  never built, so `val k: K` is refused (`value not decided: K holds K, whose construction evaluates
  a throw`) — and `val f: K => Nothing`, which holds the absurd function exactly when `K` is empty,
  is refused too, never 0. The stored side resolves no term name there, so it refuses where a name
  may be the root imports' `???`, and it refuses a private or protected constructor, a parent's
  context argument left to implicit search, and a parent it does not read. A singleton type `p.type`
  holds what `p` denotes: an object with no parents whose body throws nothing, 1; a value's
  singleton (`val sn: n.type` for `val n: Nothing`) is refused, since the value's type is read where
  the value is declared. This was adopted by the integrated review on a 2/3 vote: the dissent held
  that a value row is documented as "one inhabitant, declared type not measured", so reading the
  type is a model change. The majority's reasons: it reads only the declared type, the initializer
  stays unread, and a value of an empty type is the one row the old reading overcounted.

The cases fit together. A throw is ignored as a behavior: it gives no value and implements nothing,
so a member whose only body throws reads as if that body were absent. Where an override may give the
member a behavior, it is the free capability its abstract twin is; where none may, it is concrete
code that supplies nothing. No count is refused because code throws. What a type lets pure code do
is still read, so an open member type no pure code inhabits leaves no instance, exactly as for an
abstract member. A construction that throws is not a behavior of a value but the absence of one: no
instance exists to observe, which is why its template is empty.

## 3. A throwable is data

A `Throwable` passed, stored or returned is an ordinary value of an open `java.lang` class. The
public API of the root-import throwables (`Throwable`, `Exception`, `Error`, `RuntimeException`,
`IllegalArgumentException`, `IllegalStateException`, `NoSuchElementException`, …; JDK 21, read
with `javap`) splits into three groups:

- **Constructors:** from `()`, a message, a message and a cause, or a cause. Some subclasses take
  fewer.
- **Pure reads:** `getMessage`, `getLocalizedMessage` and `getCause`, each overridable.
- **Members outside the model:**
  - the stack trace: `getStackTrace` reflects on the call that built the value, and
    `setStackTrace`/`fillInStackTrace` change it;
  - `initCause`, `addSuppressed` and `printStackTrace`, which are effects;
  - `getSuppressed`, which is final and filled only by an effect;
  - `toString`, a universal member.

The method side reads every modelled throwable as **an ω transparent value**, the reading `String`
has. For every count this is exact at the model's tiers:

- **As a goal**, `new T(s)` builds a value for every string `s`, and `getMessage` tells them apart.
  So there are infinitely many distinct values, and countably many programs: ω.
- **As an input**, a throwable is observed only through strings and throwables. Every value of
  those types is built from nothing, so it adds no producer the empty environment lacks. What it
  adds is information: a program may branch on the message, and the messages are infinitely many.
  Eliminating it is the uniform split of a closed value (D9). With no choice left (a terminal
  goal, or a goal with no implementation) it is irrelevant: `def f[A](e: Throwable, a: A): A` is
  1, and `Either[Throwable, A] => Option[A]` is 2. With two or more choices the count is ω.
- **Subtyping** only adds producers of types that are already inhabited, so it changes no tier.
- **A `null` message or cause** follows the method model's rule for `T | Null`, under which `Null`
  is empty ([unions and nullability](method-unions-nullability.md)). A throwable whose message is
  `null` is one more value of an open class that already has infinitely many, so the choice changes
  no count.

A throwable is also its own key when it is the argument of a rigid constructor (`F[Throwable]`).

A source subclass is read as any source class:

- A final one is its constructor parameters, because its message and cause are fixed functions of
  them.
- An open one leaves its stdlib parent's overridable reads (`getMessage`, `getLocalizedMessage`,
  `getCause`) to its subclasses. As a capability record it is refused, and the reasons name them:
  `inherited stdlib member a subclass may override: java.lang.Exception.getCause`. The
  sealed-family reading is coarser (§4): it reads a non-final case-class leaf as its fields, so
  there a subclass in another file that overrides `getMessage` is a value it does not count.

The **stored side** keeps its open-class convention. A throwable's values are those of any
subclass, so the estimate gives no size: the row is *unbounded by an open abstraction*, with the
reason `open to implementations (Throwable)`. It never claims a finite size.

## 4. Limits

- A union with a throwable (`Exception | Plan`) is not yet a sum. The runtime-class reading does
  not place an open stdlib class beside source classes, so the reason is `Exception has no runtime
  class this reading tells apart (stdlib model)`.
- `Throws` recognizes the forms in §2 syntactically. A call to a source method declared `Nothing`
  is typed by the ordinary call rule, but no reading follows a body into its callee. A `try`, or a
  match that mixes throwing and returning cases, is typed as before (`?` with the form named), and
  so is a `sys` or `Predef` a parameter or a source definition binds.
- An open member written without a result type is not typed by this reading:
  `overridable member's result type is inferred`. A polymorphic open member, abstract or with a
  throwing default, is a polymorphic capability, read by the polymorphic-member rule (register
  D24) exactly as its abstract twin: `fail[A](msg: String): A` and `k[A](x: A): Nothing` are empty,
  so a receiver holding one is vacuous (1); `fail[A](a: A): A` is the identity and `poly[A]:
  Option[A]` is `None`, which build no value of the goal; a bounded, higher-kinded or undetermined
  binder keeps its reason (`polymorphic capability binder: B (bounded)`).
- **Open question.** A member nothing overrides that implements a declaration of an empty type
  (`object OQ extends P { def k(x: Int): Nothing = ??? }`, eo's `PSVec.Empty.apply`) is concrete
  code no pure program contains: no total body exists. This reading takes its throw as supplying
  nothing, so `OQ` is built and `OQ.f` counts 4, while `P`'s record, read through its contract,
  has no instance (1), although `OQ` is a `P`. Reading `OQ` as never built instead (every instance
  is a `P`) makes the pair agree at 1, but turns `PSVec.Empty.length` into 1; refusing would refuse
  a count because code throws. The owner decides.
- A sealed case, family or enum whose construction would only throw if its names are the root
  imports' is refused (`sealed family construction not resolved`, `enum construction not
  resolved`): the reading of sealed families resolves no term names, so it can tell neither that
  the case is empty nor that it is built. The refusal is that resolution gap, not a doubt about a
  throw. It reads every template a case extends (any source type of the simple name a parent
  spells) and, for an enum with singleton cases, the companion object whose `val`s they are. An
  object whose initializer throws is still one value as a singleton type.
- The sealed-family reading reads neither a leaf's members nor whether a non-final case-class leaf
  may be extended: an abstract `def k: Nothing` of the family's root does not empty it, and a
  subclass of a non-final case leaf (a throwable's included) is a value it does not count. Both
  predate this page and belong to that reading.
- A parent's default body that re-enters the measured declaration's name is judged with the
  subclass's members in scope (`overridable member may re-enter the measured declaration`), which
  may refuse a body that binds the name lexically elsewhere. It refuses, never counts.
- An open subclass of a throwable stays `?`, with the inherited members named. A wildcard import
  that may bind `Throwable` refuses the name, as for every model name.
- `scala.util.Try`, `NonFatal` and `MonadThrow` are not in the model or not supplied, and stay
  `declaration not supplied`.

## Evidence

- \[29\] Scala 3 draft specification, ch. 3, *Types* (**Inspected (full text) — bibliographer**):
  "´p.x´ is a stable type if an only if `t` is a `val` or `object` definition" (re-read for this
  page in the research bibliography's source copy).
- \[30\] Scala 3 draft specification, ch. 5, *Classes and Objects* (**Inspected (full text) —
  bibliographer**): a `val` or `var` class parameter adds an accessor, and a `var` adds a setter
  `x_=`; "A stable member can only be overridden by a stable member".
- **Compiler fixtures** (Scala 3.9.0, `$S/probes/c4/fixtures`, `$S/probes/c4-fix1`,
  `$S/probes/c4-fix2`, `$S/probes/review-C4/r2`):
  - `throw`, `???`, `sys.error` and a block ending in a throw each conform to `Int`, `String`,
    `Boolean` and `Unit`, so each is `Nothing`;
  - `if (c) 1 else throw …` conforms to `Int`;
  - a source `???` is the member called (an `Int`);
  - E164: neither a `def` nor a `val` overrides a `var`;
  - E083: a `var` has no singleton type;
  - E173: a plain class parameter is no member;
  - runs: `def get = ???` overriding `get: Boolean` is overridden with `true`; a strict `val`
    initializer, a trait's included, a body statement, a parent's argument and a `var` initializer
    each throw in every constructor, an overriding subclass's included, while a `lazy val` does
    not; an object's nested class and object are used while its initializer throws; a member
    object's class is not built where its owning instance throws; calling a top-level `def` runs
    its own file's throwing top-level `val`, not another file's; a parent's arguments and a
    trait's body bind `???` to Predef's in a subclass that defines one; an import binds only from
    its position on; a singleton enum case is not built where the companion's initializer throws,
    a class case is.

  Every `PureCodeSpec` and `ThrowableSlotsSpec` fixture compiles except the E083 one, which is the
  refusal under test, and the two that import from `foo`, a package deliberately not supplied.
- **Tool output**: `javap -public` (OpenJDK 21) on the twelve throwables lists exactly the API
  above.
- **Executable**: `PureCodeSpec` (vars, throws, emptiness, construction, throwables, positive and
  adversarial), `ThrowableSlotsSpec` (stored side), `ValueRowsSpec.emptyValues`,
  `valueReasons`, `valueWitnesses`, `unbuiltValues` and `singletonValues` (value rows),
  `StdlibModelSpec.open`, `CapabilitySpec.unreadMembers`.
- The no-side-effects assumption is the owner's model decision (v1 §2), not a theorem. Reading a
  `var` as its value remains a model decision. Related work on why the exclusion is load-bearing:
  - Pitts & Stark 1998, *Operational reasoning for functions with local state*, HOOTS (CUP)
    (**Metadata-verified**: the abstract reports that local state must be handled for
    representation independence and for memoising functions);
  - Ahmed, Dreyer & Rossberg 2009, *State-dependent representation independence*, POPL
    (**Metadata-verified**: the abstract reports that representation independence holds even when
    representations depend on state);
  - Banerjee & Naumann 2005, *Ownership confinement ensures representation independence for
    object-oriented programs*, JACM (**Metadata-verified**, background).
