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

Only the template's own file may extend it when it is local, when it or an enclosing template is
private to a template, or when its own constructor is private (unqualified, `this`, or qualified
by a template: its companion is in the file). There an overridable member is derived unless
another template of the file (of the enclosing block, for a local one) defines its name — in its
body, as a pattern binder or as a constructor parameter — or a template of the file that may
extend the receiver (naming it, a subclass of it or an alias of either among its parents) mixes in
a parent written outside that scope whose supplied linearization defines the name, or that this
reading cannot follow: `class D extends C with T2` overrides C's `m` with T2's wherever T2 is
written. Then it is refused, since this reading does not enumerate those subclasses. An ancestor's private constructor closes no public subclass, a
top-level `private` template is visible in its whole package, and a `private[p]` constructor is
reached package-wide: those templates are open.

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
  select any given or implicit member of the receiver, or of an enclosing template. So every body
  but a literal mentions those members, and one of them that may re-enter the declaration (`given
  self: T = this`, `given g: Boolean = target(true)`) makes the body a possible view.
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
of both templates — always for two traits, which a subclass may mix; never for two unrelated classes
(a class has one superclass chain) or a fixed owner outside the record's linearization.

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
| Abstract type member in a record (inputs would open an existential that `p.X` in the signature must share) | `capability record with abstract type member not read: C.X` |
| Self type | `self type not resolved: T` |
| Hidden constructor state of an open class (X6: `class CNothing(n: Nothing)` has no instance), of a parameterized parent no class passes arguments to, or a free field a nearer `val` overrides | `open class with hidden constructor state: C.p` |
| Final-class parameter no member observes injectively | `final class field not observable injectively: C.p` |
| Non-public constructor or class, local or private template | `receiver fields constrained by …: C` / `capability record of a template only its scope instantiates: C` |
| Member hidden at the use site | `capability record with members hidden at the use site: C` |
| `var`, extension or pattern-defined member | `capability record with a mutable member not read: C`, `… extension member …`, `destructured member of an open receiver not read: C` |
| Trait parameters | `capability record with trait parameters not read: C` |
| Recursive record (a member's type mentions the record) | `recursive type requires a structural proof: C` (the type reading's own guard) |
| Polymorphic open member | `polymorphic capability binder: M` (B4 adds PB0–PB4) |
| Possible view | `overridable member may re-enter the measured declaration: C.m` |
| Record member that may be the declaration on its receiver | `capability record member may be the measured declaration on its receiver: C.m` |
| Inherited identity unknown | `inherited declaration identity not resolved: m` / `inherited override identity not resolved: m` |
| Overridable concrete member without a declared type | `overridable member's result type is inferred: m` |
| A member of a template only its file extends (own private constructor, private or local scope) that another template of the file defines — in its body, as a pattern binder or as a constructor parameter — or that a subclass of the file may inherit from a parent written outside it | `member of a template only its file extends may be overridden there: C.m` |
| A view through code the compiler inserts: a by-name extension receiver, conversion or `==` overload, a context-function parameter, a SAM-converted lambda, implicit search reaching a given that may re-enter, `super.m` | `overridable member may re-enter the measured declaration: C.m` |

Case classes keep their product reading, sealed families their sum, enums their cases; a case
class with a body is still refused. Model assumption for views: a declaration outside the sources
reaches a body only through a channel this reading sees — an unsupplied wildcard import, parent or
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

- Scala 3 reference, *Open Classes* ([22], **inspected**): traits and abstract classes are always
  open, and any non-final class may be extended; Scala 3 draft specification ch. 5 and SLS 2.13
  ch. 5 ([30], [33], **inspected**): a final member may not be overridden, members of final classes
  and objects are implicitly final, a concrete definition overrides an abstract one; Scala 3
  reference *Inline* ([17], **inspected**): inline methods are effectively final; *The Matchable
  Trait* ([19], **inspected**): the universal members C0 excludes.
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
