# Capability records, member classification and override identity

How the method-side count reads a value whose type is a source class or trait, which members of
the measured declaration's own receiver are choices, and when an inherited member is the measured
declaration. Register entry:
[D20](code-cardinality-foundations.md#d20-a-value-of-a-source-class-is-the-record-of-its-observations).
Design memo: capabilities.md C0–C7, C9. Regressions: `CapabilitySpec` and `TargetViewsSpec`
(positive and adversarial; fixtures compiled with scalac 3.9.0), `OverrideIdentityImplementationSpec`,
`ContextualOverrideIdentitySpec`, `MethodAnalysisSpec.openMembers`.

## Rules

**C0 — observation.** A value is observed only through its accessible members. The universal
members (`==`, `equals`, `hashCode`, `toString`, `getClass`, …) are runtime equality and
reflection, which the model excludes, so a value of a class is the tuple of its members'
behaviors: the procedural reading of objects. A restated universal member is one of `Any`'s
overridable ones — `toString`, `hashCode`, `equals` whose parameter resolves to `Any` where it is
written — on records and receivers alike; `canEqual` belongs to `scala.Equals`, so a declared
`canEqual(that: Any): Boolean` is an ordinary member (a free `Any => Boolean`, one Boolean since
`Any` is terminal).

**Abstract members of a record** ([D32](code-cardinality-foundations.md)).
An unbounded abstract type member no level defines does not refuse the record: the members are read
over a witness named by the member, and the record is the package ∃X. members-over-X each value
carries — an input opens it with a witness fresh per value (`Shape.Existential`, the same opening
the wildcard inputs get), so `p`'s and `q`'s never mix, and the witness itself is unobservable. A
goal of the record is a result construction — the implementation would pick the witness — and stays
at the existential result boundary. Bounded or higher-kinded members refuse, and so does any member
the measured signature names through a path (`p.X`): the projection is an atom keyed by the path
(`TypeMembers`), and the opened witness and that atom would have to be
one — a sharing this reading does not yet agree on.

**C1/C2 — the member table.** A receiver's members are read over its supplied linearization, each
level under the arguments its `extends` chain gives it. A concrete definition overrides an abstract
one, and a definition overrides a matching one of every level after it in the receiver's
linearization (SLS §5.1.2, §5.1.4: L(C) = C, L(Cn) +⃗ … +⃗ L(C1)) — not merely of the levels a
breadth-first walk reaches it from: with `trait C extends A`, `trait B extends A` and `trait R
extends C with B`, L(R) = R, B, C, A, so B's `m` overrides A's although the walk meets A through C
first. A record keeps the nearest declaration of a restated member, whose type the member has
(`trait T extends A with B`, `B` refining A's `n: Option[Boolean]` to `Some[Boolean]`: T's `n` is a
`Some`). An effective member is

- *open* when an instance may replace it: abstract, or concrete and not effectively final;
- *derived* when none may: `final`, `private`, a concrete `inline` member, a given (alias or
  `given … with`) or a member object (Scala 3 makes both final), a `var` (a constructor parameter
  or a body `var`, which also implements an inherited declaration of its name: scalac rejects
  overriding one), or any member of a final class, object or given;
- *hidden* when the use site cannot access it.

A parent's constructor `val` (a case class's first clause, a trait's `val` parameter) is a member
too: a subclass may override it — scalac 3.9.0 only warns ("cannot override val parameter") and
the override is what the parent's own code then reads — so it is open unless `final` or
`private`. A `val`, a field, an object, a given and a parameterless method of one name are one
member slot: a `val` restating a `def`, or a constructor `val` implementing one, is counted once.
A member is effective unless a nearer level, or the receiver's own constructor `val`, redefines it.

`open` (the Scala 3 soft modifier) changes nothing: traits and abstract classes are always open,
and any non-final class may be extended somewhere.

**C3 — inputs.** An open member is a free capability of its type: across admissible receivers it
is every behavior its type allows (a subclass whose override returns a stored function realizes
any tuple of member behaviors), its default body only one of them. A derived member is concrete
code over the receiver's state and the environment, so it adds nothing once that state is
observable.

**C4 — one shape for inputs and goals.** A record is read only where its input and goal readings
coincide:

- an *open* template — a trait without parameters, or a class whose constructor parameters are all
  public `val`s and whose constructor every subclass reaches — is the product of its open members
  and `val` fields: an input observes exactly those, and a goal may pick any tuple of them, since an
  anonymous subclass overriding each member realizes it. A parameterized parent no class of the
  template passes arguments to (`class P(val p: Boolean); trait T extends P`) leaves them to the
  class that finally extends it: every instance is `new P(v) with T` for some `v`, so its public
  fields are free fields of the record, and a parameter no public field observes is hidden state
  (refused);
- a *final* class or value class with a public constructor is the product of its constructor
  parameters, each a public `val` or exposed by a public member that is its η-alias (`def get(s:
  S): A = read(s)`, checked to observe the parameter at its own type): an instance is its
  arguments, every member a fixed function of them, and the aliases observe each argument
  injectively.
- A `final abstract` class has no value: the empty sum.

**C5 — the enclosing receiver.** On the measured declaration's own template (and on enclosing
templates, `Outer.this`), an overridable concrete member — own or inherited from a supplied parent —
is an open capability exactly as an abstract one is; a declaration a concrete member implements is
that member, and a same-named member of another signature is an overload, a member of its own. A
constructor reads only its parameters: no member of the class exists before it runs.

In the closed world (register D39) the supplied templates are every subclass there is: an
overridable member no supplied template overrides — none defines its name, in its body, as a
pattern binder or as a constructor parameter, and none that extends the receiver mixes in a parent
whose supplied linearization defines it, or one this reading cannot follow — is final, so derived.
A member a supplied template does override varies across receivers: it stays an open capability,
as an abstract one is (it was refused where only the template's file could extend it, this reading
not enumerating those subclasses).

**C6 — target views.** The model counts recursion-free implementations: none re-enters the
measured declaration in any admissible receiver, and a receiver that overrides nothing is
admissible. A member whose default body evaluates the declaration whenever the member is
evaluated — the head of the body's spine, a selection's receiver, a by-value argument of a callee
whose declaration is read, an operand of `==` or `!=`, a block statement, a tuple field, an `if`
condition, a match scrutinee, a unary operand, the body of a lambda the member returns as a
function (a producer in a long normal form is fully applied) — directly or through another
member's default, is a *definite view*: no implementation uses it, and it is dropped. A mention
anywhere else makes it a *possible view*, which carries the reason `overridable member may re-enter
the measured declaration: C.m`. The reading is by slot: members of one name are grouped by override
identity, and each nested class, trait or object is a slot of its own.

The measured declaration may be a local definition: the member whose body evaluates it is its
view, at the same positions — a name the body binds to that very definition is a mention of it, a
name bound again (a parameter, a pattern, a nearer definition) is not, and the definition's own
body is the choice being counted. A local `def h` that `m` calls at its result makes `m` (and
`other = m(true)`) a definite view, so `h(y)` reads `y` and the capture `b`: 16, as for a final
class and as the lambda-lifted twin `h(b)(y)` reads (it read 2^32); a named argument `id(x = h(b))`
of an eager clause is evaluated as a positional one. A lambda applied in place, a local def calling
`h`, or an η-expansion of it is not on that list: a possible view — and so is a mention anywhere
the reading enters loosely (a match arm, a `try`, an interpolation, a partial function, a `while`
body: `?`, never the 2^16 of `m` left free). Constructing a class runs its constructors: `new
C(…)`, an `extends C(…)` clause or a secondary constructor's `this(…)` may run the secondary
constructors' bodies, the template statements and the strict initializers of every receiver level
among C's supplied base classes, C a subclass of the receiver's own class included (`def m = { new
Sub; true }` with `class Sub extends C`), so a member constructing one is a possible view of what
they run (`def other = new E(()).n` with `def this(u: Unit) = { this(true); val _ = h(true) }`:
`E.h` is `?`, not 2^8). A creator application builds a class as `new` does — `E(true)`,
`E.apply(true)` or `p.E(true)` where E names a supplied class (a case class's companion `apply`
or any class's constructor proxy), and `copy(…)` builds the case class it is a member of (on
`this`, the receiver's own; on another value, one this reading does not follow). A class with a
level outside the receiver's that runs code of its own — statements, strict initializers,
arguments to its parents, secondary constructors — may run any member it inherits on the new
instance, the measured one too: `new Sub().x` with `class Sub extends C { val x = h(true) }` is a
possible view of `C.h` (3.9.0 run: an implementation of h returning other overflows). A class
outside the sources extends no receiver level and runs none of its code; one this reading cannot
follow (an unresolved name, an unread parent, a name a block import may rebind) may be any.

A count refused only by possible views is read between two bounds (register D31): the possible
views P bound as free capabilities give an upper reading, left out a lower one. Every
recursion-free implementation is among the upper reading's terms, and every term of the lower
reading is recursion-free, so lower ≤ count ≤ upper (a member of P with an empty type makes the
upper reading vacuous, but then no implementation uses it and the count is the lower reading): an
upper reading of 0 is the count. Agreeing bounds would fix the count as well; deciding only 0 is a
deliberately narrow choice (the corpus-coverage prune dropped the lower reading's 1), so the
reasons stay there. A member of P whose binding is refused in the upper reading refuses the row. eo's `ComposedGetOptionLaws.getOptionInner` is 1: the record
`CanGetOption[A, B]` has the None-returning implementation alone, with or without the law member.

A precision regression the bounds record rather than repair (FOLLOWUPS 14b): eo's `Optic.to` and
`Optic.from` printed 0 at `b5045b8`, derived independently — `andThen` can return only `rc.Out`,
and every `ReadCompose` instance in eo (`compose/ReadCompose.scala`) fixes `Out` to `Getter`,
`PickFold` or `ForgetFold`, none of which reaches the rigid carrier `F[X, A]` or `T` — but the
possible-view reading refuses both rows (`overridable member may re-enter the measured
declaration: Optic.andThen`). The type-reachability filter that would ignore such views — judged
over the instances the sources declare — is refuted: an implementation may build its own instance
whose `Out` is the goal itself (review fixture `probes/precision/a/closedworld.scala`: every
declared `RC` fixes `Out` to `Unit`, yet `to` reaches `T` through an `RC` it constructs).
Recovering the 0 needs a dependency name index, higher-kinded capability binders and an
existential record reading; until they land the rows stay `?` and the lost 0 is recorded here
instead of reprinted. `Optic.id` and `ModifyF.map.map` never had a number: they wait on the
dependency name index, an owner decision (FOLLOWUPS 16).

Each of those positions is evaluated only if the compiler inserts no code that defers it
(`Insertions`, `ReceiverCalls`):

- A selection's receiver is evaluated when the selected name is a member of the receiver's
  declared template (own or inherited, at the call's arity) or of `Any`; otherwise the compiler
  may apply an extension or an implicit conversion, whose receiver may be by name (scalac 3.9.0
  prints `true` for `target(true).lazily` with `extension (x: => Boolean) def lazily = true`), so
  it counts only when no such declaration may be visible: no source `implicit def` or `implicit
  class` takes its argument by name, no source extension of that name has a by-name receiver, no
  import or export binds the name outside the sources where the selection is read (an import binds
  only after itself, so the lookup is positioned), no import of an enclosing block brings the name
  or opens a wildcard or given import, and no channel to unsupplied declarations
  (`Resolver.unreadChannel`: an unsupplied wildcard import, parent or self type) is open.
- An argument, a tuple field, an `if` condition, a typed `val` and the member's result have an
  expected type, so a by-name implicit conversion may wrap them: they count only when no source
  conversion takes its argument by name, no unsupplied channel is open and no wildcard or given
  import of an enclosing block precedes them. A Scala 3 conversion is a `Conversion` instance,
  whose `apply` is by value.
- A parameter whose type is a context function, a source alias of one, or a binder the call may
  instantiate with one (explicit type arguments for the method's own binders, the extends chain for
  another level's class binders, a qualifier's declared type arguments) wraps its argument in a
  context-function literal: not evaluated.
- The right operand of `==`/`!=`/`eq`/`ne` counts only when no source declares a by-name overload
  or extension of the operator and no unsupplied channel is open (an overload of Any's final `==`
  with a by-name parameter is selected over it).
- A lambda returned at a declared type that is no function or context-function type — a SAM or
  another record — is observable through its other members without being applied: possible.
- Implicit search is not syntax: `summon`, a `using` clause filled implicitly or a conversion may
  select any given or implicit member of the receiver, or of an enclosing template — the measured
  declaration itself included when it is a given or implicit member, abstract or concrete. So
  every body but a literal mentions those members, and one of them that may re-enter the
  declaration (`given self: T = this`, `given g: Boolean = target(true)`) makes the body a
  possible view: with `given g: Boolean` and `def other = summon[Boolean]`, other may re-enter g
  (a 3.9.0 run overflows the stack). A body that only returns one of its parameters, declared with
  exactly its result type, inserts no conversion and searches nothing (`def other(using b:
  Boolean): Boolean = b`).
- `super.m` runs whichever definition of m the instance's linearization puts after the template:
  any definition of m's slot, so a possible mention of it.

- A name evaluates a member only where it applies every value clause of the one member its
  arguments select: overloads resolve by arity first (an overload whose default calls the
  declaration at the declaration's arity is a definite view; one the arity does not settle is
  possible), and an unapplied or partly applied method is η-expanded, not run (`g(target)`, `val
  f: B => B = target`, `(target, true)` are possible views, never definite).
- An argument list fills the clause its `using` flag selects (a `using` list the next `using`
  clause, any other the next clause the compiler does not fill); a method with a Scala 2
  `implicit` clause, a callee bound locally (a parameter, a lambda or a block definition — a
  block's local definitions bind their names in the whole block), and a member of a declared
  member's type whose template has parents leave the arguments unread.
- `l op r` evaluates `l`; a right-associative `l op: r` calls `op` on `r`, and `l` may be by name
  (scalac 3.9.0 prints `true` for `boom() #:: new Lz()` with a by-name `#::`): possible.
- `this`, the template's own `T.this` or its self alias is the receiver; `this` escaping into a
  call is possible. `Outer.this.m` is a member of the enclosing template, not of this receiver:
  it may reach the declaration unless every definition of `m` there is fixed code whose body names
  no member, no `this` and not the declaration.
- A nested object or class whose body may evaluate the declaration makes a member that mentions it
  (`helper.run`, `new H().run`) a possible view; this reading does not follow nested bodies.
- An enclosing receiver's members are judged too: `Inner.target` (every template between an object)
  names the measured declaration through its path, so `def m = Inner.target(true)` on `Outer` is a
  definite view.

A record member of the declaration's name and arity is a possible view whenever one instance may be
of both templates: one extends the other, or (the closed world, register D39) a supplied template
extends both — never two templates no supplied class mixes.

**C7 — irrelevant members (exact case).** A member every application of which builds Unit is
irrelevant to every count (Unit has its one construction whatever the environment holds), so its
unreadable parameter types are never read. Other members still fail eagerly when a type is
unreadable (see Limits).

**C9 — override identity.** An inherited member is the measured declaration when names and
parameter signatures match as seen from the receiver: each side's class binders are keyed through
every `extends` clause between its level and the receiver (a grandparent's `A` is the argument the
chain gives it); a source name is keyed by the declaration the scope-correct lookup binds it to,
imports included, a name an import binds outside the sources by its path, a builtin by its
canonical declaration, a transparent alias by its body and an opaque one by its own declaration.
Result types are not compared: members match by parameters, and a matching member whose result
does not conform is a type error, not an overload. Two keys that differ prove two slots unless one
rests on a foreign or opaque declaration whose equalities the keying cannot see; a different
number of binders or parameters per clause is always another slot.

## Derivations

- *Open member is free.* Fix the measured implementation `e` over an environment with an open
  member `m: τ`. For every term `t: τ` over that environment, the subclass overriding `m` by `t` is
  admissible, so `e`'s observable behavior ranges over `e[m := t]` for all `t` — the same quantifier
  the solver places over an abstract member. A default body is one such `t`.
- *Derived member adds nothing.* A derived member's body `b` is a fixed term over the receiver's
  observable state and the environment; any implementation using it is βη-equal to one that
  inlines `b`, which the solver already counts. This needs the state to be observable — the final
  record's exposure condition, and the open record's restriction to public `val` fields.
- *Coinciding readings.* For an open template every tuple of member behaviors is realized by an
  anonymous subclass, and every value is such a tuple (C0), so input and goal are the same product.
  For a final class the observable fields are the constructor arguments, injectively.
- *Definite views.* If `m`'s default evaluates `t` whenever `m` is evaluated, every implementation
  using `m` (fully applied in a long normal form, under capabilities that may evaluate what they
  are given) re-enters `t` in the receiver that overrides nothing: excluded exactly. When only some
  evaluations re-enter, the exclusion depends on arguments the solver does not split: a reason.
- *Override identity.* Under the input assumption that the sources typecheck, a member whose name
  and parameter types match an inherited one overrides it; keys compare types only through
  declarations the lookup binds, so equal keys are equal types, and differing keys are different
  types except across foreign or opaque declarations.

## Limits (each stays `?` with the reason given)

| Situation | Reason |
|---|---|
| Abstract type member in a record that does not open: bounded or higher-kinded ones, and any the measured signature names through a path (`p.X`, `T#X`) — the opened witness and the path atom would have to be one ([D32](code-cardinality-foundations.md)) | `capability record with abstract type member not read: C.X` |
| Self type | `self type not resolved: T` |
| Hidden constructor state of an open class (X6: `class CNothing(n: Nothing)` has no instance), of a parameterized parent no class passes arguments to, or a free field a nearer `val` overrides | `open class with hidden constructor state: C.p` |
| Final-class parameter nothing observes injectively at the use site: no field it reaches (a `private[p]` field is reached inside `p`, a `private` one in its class), no public η-alias, no parent constructor it passes the parameter to verbatim that observes it in turn without a nearer override (an access fact, never mutation: a `var` is its value) | `final class field not observable injectively: C.p` |
| No public constructor (rule B: a non-public class's own restriction is disregarded), or a public secondary over a space not closed | `receiver has no public constructor: C` / `receiver space of its public secondary constructors not compared: C` |
| Open template whose subclasses only its scope may declare | `capability record of a template only its scope instantiates: C` |
| Member hidden at the use site | `capability record with members hidden at the use site: C` |
| Extension or pattern-defined member (a `var` is its value, register D26: an abstract one its getter, a concrete one derived) | `capability record with an extension member not read: C`, `destructured member of an open receiver not read: C` |
| Trait parameters | `capability record with trait parameters not read: C` |
| Recursive record (a member's type mentions the record) | `recursive type not read: C` (the type reading's own guard) |
| Polymorphic open member not reduced | `polymorphic capability binder: M (…)` ([PB0–PB4](polymorphic-members-and-saturation.md)) |
| Possible view of a member a supplied template overrides | `overridable member may re-enter the measured declaration: C.m` |
| Record member that may be the declaration on its receiver | `capability record member may be the measured declaration on its receiver: C.m` |
| Inherited identity unknown | `inherited declaration identity not resolved: m` / `inherited override identity not resolved: m` |
| Overridable concrete member without a declared type | `overridable member's result type is inferred: m` |
| A view through code the compiler inserts: a by-name extension receiver, conversion or `==` overload, a context-function parameter, a SAM-converted lambda, implicit search reaching a given that may re-enter, `super.m` | `overridable member may re-enter the measured declaration: C.m` |

Case classes keep their product reading, sealed families their sum, enums their cases. A final
case class with more than its product — members, parents, a second clause, a constructor modifier
— is a final class: its record is its constructor arguments, the first clause its public fields,
a later clause hidden state (`final class field not observable injectively: C.b`), a private
constructor the specific reason. A case class no supplied template extends is final in the closed
world (register D39); one a supplied class extends, with more than its product, is refused for what
it is: `case class open to extension: NC (overridable member hit)`.
A record's proper type parameters remember their arguments as written, as a case class's do
(members MT1'), so a match-type field reduces on a tuple argument: `final class Miss[A](val fst:
Fst[A])` read as `Miss[(Unit, Unit)]` has the field `Unit` (eo's `pickFoldMiss`: 1). A member's own
type parameter of a class parameter's name is another binder there: `def out[A](a: A): Fst[(A,
Unit)]` in `Rec[A]` reads its own A, never the argument written for the class's.

Model assumption for views: a declaration outside the sources reaches a body only through a
channel this reading sees — an unsupplied wildcard import, parent or
self type, an import or export of the selected name, or an import of an enclosing block; an
unsupplied *named* import of a by-name implicit conversion, and a by-name `==` overload declared in
an unsupplied class, are assumed absent (Scala 3's own conversions are `Conversion` instances, by
value). **Open gap (found at landing):** the implicit scope of a type outside the sources — the
companion of an expected type or of a qualifier's type — is no channel this reading sees, so a
by-name conversion or extension declared there is not assumed absent but missed: with `final def
g(w: ext.W)`, `def m = g(target(true))` and `object W { implicit def lift(b: => Boolean): W }`, m
never runs target (scalac 3.9.0 run), yet m is read as a definite view (`T.target` reads 4, true
16; fixture `probes/b3land/fc`). Closing it needs the expected type of every typed position and the
type of every selection's qualifier, and a stated assumption for the stdlib's companions. A member
whose parameter type is unreadable and whose result is not Unit fails the environment eagerly
(capabilities.md C7 in full needs a solver marker for an unread parameter).

## References

- Scala 3 reference, *Open Classes* (\[22\], **inspected**): traits and abstract classes are always
  open, and any non-final class may be extended; Scala 3 draft specification ch. 5 and SLS 2.13
  ch. 5 (\[30\], \[33\], **inspected**): a final member may not be overridden, members of final classes
  and objects are implicitly final, a concrete definition overrides an abstract one; Scala 3
  reference *Inline* (\[17\], **inspected**): inline methods are effectively final; *The Matchable
  Trait* (\[19\], **inspected**): the universal members C0 excludes.
- Member matching by parameter types (SLS §5.1.3): **classical**, not among the inspected quotes.
- Cook 2009 (**metadata-verified**: the abstract reports abstract data types and objects as two
  distinct forms of data abstraction); Cook & Palsberg 1989 (**metadata-verified**: the abstract
  reports self-reference as a fixed point, so late binding leaves overridable members open). Pierce
  & Turner 1994 and Bruce, Cardelli & Pierce 1999 (**metadata-verified**, background only): object
  encodings. The record reading and the view rule are this register's derivations from these and
  from the model's exclusions.
- Compiler fixtures (Scala 3.9.0, `probes/b3/spec`, `probes/b3fix1/spec`, `probes/b3fix1r/spec`,
  `probes/b3fix2/spec`): every `CapabilitySpec` and `TargetViewsSpec` fixture compiles; a diamond
  `R extends C with B` reads B's final `m` (run); an extension with a by-name receiver, a by-name
  `==` overload, a by-name conversion, a context-function parameter and `summon` of a given that is
  `this` behave as the view rules say (runs, review round 2); an implementation leaving an
  inherited abstract member undefined must be abstract; overriding a given or a member object is
  rejected ("cannot override final member"); overriding a parent's `val` parameter compiles with a
  warning and the override is what the parent's code reads (run fixture); a by-name left operand of
  a right-associative operator is not evaluated (run fixture); a class in another file of the
  package extends a template with a `private[p]` constructor, and a public subclass of a class with
  a private constructor. At landing (`probes/b3land/fix`, `probes/b3land/fc`, runs): a by-name
  extension imported by the file, the template or a block, or exported by the template, never runs
  its receiver; nor does a by-name conversion in the companion of a parameter's type outside the
  sources (the open gap above).
