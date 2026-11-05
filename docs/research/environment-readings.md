# Environment readings: what a body can actually reach

A count is only as sound as the environment it is taken over. Nine readings of that environment
over- or under-counted (census memo §3, hkt memo §4.3, members memo W1, stdlib memo H1); each now
follows the Scala 3.9.0 rule it stands for, or refuses with a reason naming what is missing.
Regressions: `EnvironmentSoundnessSpec` and `NameLookupSoundnessSpec` (under
`core/src/test/scala/cardinality/analysis/`). Every fixture there compiles with the Scala 3.9.0
compiler (checked this session; bodies filled in).

## 1. Anonymous contextual parameters

An anonymous `using T` has no name, so a body reaches it only through implicit search. Scala
searches the lexical scope first; among the eligible candidates (contextual values whose type
conforms to the searched type) a more deeply nested one wins outright, and candidates at one level
are ranked. Compiler fixtures (3.9.0): two anonymous `Option[A]` parameters — or a named one and an
anonymous one in two clauses of one method, an extension's clauses included — make
`summon[Option[A]]` an ambiguity error (E172); so does a class's evidence against a given of its own
body, or one brought in by `import H.s` or `export H.s`; a method's `using Option[A]` beats its
class's; a given written later in a block is the one a local definition's search selects (run:
`param`, not the outer evidence); against a given inherited from a parent, the class's own parameter
wins (run: `flat: evidence`); and of `using Option[A], Some[A]` the search for `Option[A]` picks the
*more general* one (`None` was returned). The ranking is version-dependent, and the model does not
encode it.

**Rule.** Every parameter binds under its own key — owner and position — so neither the empty name
nor one index in two owners (a method and its local definition, an extension and its method)
collides. An anonymous parameter is counted only when every other contextual candidate at its level
or deeper has a type provably unrelated to its own: a class type of a template that its own type's
template neither extends nor is extended by. Candidates are parameters, given and implicit
definitions of those scopes (a block's wherever they sit in it, read as candidates of unread type),
givens declared by supplied parents, and imports or exports with any selector but an unimport
(unseen candidates, like a parent outside the source set). An
extension group shares its method's level. Then the search for its own type selects it whatever
the ranking. Two class types are unrelated when neither of their templates is a base class of the
other, every base class of both supplied (a class type conforms to another only through a base class: `Acc[F]` and
`RevAcc[F]` are selected separately — compiled), while a base-class relation is an overlap whatever
the shapes (a subtrait's capability record is the wider product).
Otherwise `?`: `ambiguous anonymous evidence: T` (the same type — the same declarations, binders
and arguments — declared by the same template or method) or `anonymous evidence selection not
resolved: T (overlaps U)`. Outer candidates never matter: the parameter is nested deeper and wins.

**Limit.** The exact count would drop an unreachable parameter instead of refusing, and would count
one that beats an inherited given; both need the ranking. Any pair other than two class types is
assumed related.

## 2. Import anchoring and package clauses

`import _root_.p.X` names the top-level `p`; the path keeps its `_root_` anchor and is looked up at
the root only (it used to be stripped at every recursion level, which dropped the import and let a
builtin answer). A package clause opens exactly the path it spells: `package a.b` opens `a.b`, not
`a` (fixtures: a member of `a` is `Not found` there, and `import p.X` binds the top-level `p.X`, not
`a.p.X`), while chained clauses `package a; package b` open both. A relative import prefix is
anchored at each opened path and template, nearest first, then at the root (whose members are the
top-level packages): in `package q`, `import p.X` reaches `q.p.X` before `p.X`. A simple name not
declared or imported is read the same way, except that the empty package's members are invisible
from a named package (fixture); a dotted name whose first segment is a top-level package still
reads from the root. (The lookup used to try every prefix of a dotted clause, root first.)

A path's first segment is a name like any other, bound before the default anchoring: by an object
the scope declares in its file, by a member object its template inherits (`c.X`, `import c.X` and
`import c.*` inside `object In extends HasC` read `HasC`'s `object c`), or by an earlier import
(`import p.q; import q.X` reads `p.q.X`; `import p.Obj` makes the type path `Obj.X` `p.Obj.X`). A
type path written `_root_.p.X` is read at the root. Among the bindings of one scope, Scala ranks a
declaration of the same file and an explicit import above a wildcard import, and a wildcard above a
package member of another file (fixture: with `import lib.*`, `X` and the parent `Base[A]` are
`lib`'s, beside `p.X` and `p.Base` in another file of `p`); an outer wildcard against an inner
package member, or a same-file member against a wildcard, is E049, so a valid program never asks.
Transparent-alias projection reads the same binding (it used to look the name up without imports).

## 3. Inherited declarations

A member inherited through `trait E[Y] extends B[Option[Y]]; trait B[X] extends A[X]` has `A`'s
type as seen from `E` (Scala's base-type substitution): each level's arguments are resolved under
the environment computed for the level that names it, not the intermediate level's own binders. A
parent reached twice with different arguments within one receiver's linearization is refused
(`inherited parent instantiated twice`): for a covariant parent Scala gives the member the
intersection (fixture: `seed: Int & String`), which neither path describes. Two receivers — an inner
template and the one enclosing it — each have their own copy of a parent both extend
(`Inner.this.seed` and `Outer.this.seed` are two slots). Each template then judges its own slots for
the closed split ([ground scalar types](ground-scalar-types.md), "One value per member"): a `val`
restated at two levels is one value, and one member declared as a `def` and a `val` is refused.

A parent clause is resolved where it is written, in the scope around its template: through that
scope's imports, and as a member class the enclosing template inherits (`trait B[Y] extends A1[Y]`
inside `Outer2 extends Outer`, where `Outer` declares `A1`). A parent found nowhere in the source set
is outside it.

Declared callables are keyed by declaration and receiver, so overloads are separate capabilities
(`@targetName` is needed when two erase alike). Within one linearization a same-named declaration at
another level restates the member when its parameter types resolve to the same declarations (each
name the class, trait, enum or opaque type it denotes, an alias the type it names, a binder its
identity). Any other same-named declaration overloads (two `final case class P(b: Boolean)` of two
objects share a shape; fixture: implementing only one is E231). A lexically enclosing template's member
is another receiver's, never merged.

A final concrete class, an object or a given instance that implements an inherited declaration in
its own body has that member as concrete code, not a free capability: a `def` of the same name and
clause shape; for a parameterless `def` or a `val`, a `val`, a constructor `val` or case parameter;
for a `given`, a given of the same name. `final class C[A](a: A) extends P[A] { def get: A = a }`
has one value of `A` at any depth, and so do the `val`, case-parameter and `given` twins. The member
may be the signature measured: its slot is then the measured body, never a capability, and no
override identity is asked. (A same-shaped overload changes nothing: a concrete final template
implements every inherited declaration somewhere, concretely. An `abstract final` class does not,
and is read as open. Implementations in intermediate parents are members gate G1's.) An
implementation in a local template may read a capture of an enclosing method that the target hides
behind a parameter of the same name (`def m(a1: A)` inside `mk(a1: A, …)`): no prefix names it, so
the member is the only route to it. A member that is that capture (`def seed: A = a1`) puts it back
in the environment (fixture `mk(1, 2).m(3)`: 3, 2 or 1 — 3 values); any other reading of it is
refused (`final implementation reads a hidden capture: a1`). The same holds for any member that
holds a hidden capture, not only an implementation: an own alias, or a parent's constructor
`val` the template can read and passed exactly the capture (`new F[A](a1) { override def m(a1: A):
A = a1 }`: 3), keeps it in the environment at its own type; one typed with a single value (`def
seed: Any = a1`) shows nothing of it and adds nothing; another reading — a wider type, a field a
nearer level overrides — refuses (`local capture held by a member that hides it: a1`, [definition
inventory](definition-inventory.md)). A local definition of a block around the target holds a
capture the same way, wherever in the block it stands (a block's methods see each other): a
method, lazy value or given that is the capture puts it back (`def seed = b` beside `def
target(b: Boolean)`: 16, it read 4; 3.9.0 runs print the outer `b` through it), and a local
object or class that reads it — which the target may build — is refused, unless its own member of
the name is what it reads.

Inside a template body a simple type name the template inherits from a supplied parent means that
member before an enclosing or package-level declaration or import (fixture: with `type X = Unit` at
package level, `def g(): X = true` compiles in `final class C extends HasX { … }` where `HasX`
declares `type X = Boolean`). Nearer declarations and binders win; private members are not
inherited; an enclosing class, object, method or block declaring the same name — or a type
parameter of one — makes the reference ambiguous (E049, fixtures), so valid programs never ask. A
parent outside the source set may declare the name: where the meaning would come from a package, an
import or the builtins, the reading is refused (`inherited type member not resolved: X (unsupplied
parent Base)`; fixture: `Base` declaring `type X = Boolean` beside a package `X = Unit` makes `X`
`Boolean`). An import inside the body does not change this (fixture: the inherited `Y` beats `import
q.Y` there). The root and marker types (`Any`, `AnyRef`, `AnyVal`, `Object`, `Matchable`,
`Serializable`, `Product`, `Equals`, `Dynamic`) declare no type member and are exempt — but only
where the name resolves to nothing in the source set or is spelled `scala.`, `java.lang.` or
`java.io.`: `import lib.Serializable` binds the parent to an unsupplied type, which is refused like
any other. (A wildcard import of an unsupplied namespace may bind `Product` too; a marker parent
under such a wildcard is refused by the stdlib model's rule, [stdlib-model.md](stdlib-model.md).)
Override keying does not follow inherited names: such an override in an open template stays
`inherited override identity not resolved`.

## 4. Templates and families

- A `var` constructor parameter, like a `var` member, reads as the value it holds (the model
  excludes mutation); the interim `mutable capture` diagnostic is gone
  ([environment completeness](environment-completeness.md) §2).
- A typed self type (`self: Cap[A] =>`) adds members this reading does not enumerate:
  `self type not resolved: Cap[A]` until capability records read them.
- An enum case with an `extends` clause is a subtype like a sealed family's: its own parameters (the
  declared ones, or the enum's that its fields or its `extends` clause mention — fixtures:
  `case K(a: A) extends G[A]` and `case K(n: Boolean) extends G[A]` make `G.K[String](…)` valid,
  `case M(n: Int) extends G[Unit]` takes none) must bind or bottom-pin the enum's, or the family is
  refused as the sealed twin is.
- `sealed` constrains where subclasses are *declared*; an anonymous instance `new P {…}` in the file
  is a value of `P` no named case enumerates (`sealed family has anonymous instances in its file`),
  and a child declared in a class, trait or given body, or in any block — a method's, a `val`'s, a
  `lazy val`'s, a given alias's, a lambda's, an `if` arm's — is a class per enclosing instance or
  evaluation (`sealed subtype is an inner or local class`; every class, trait, object and enum of
  the file is checked, not only the indexed ones). Children of objects and packages stay summands.
  Children are found by the parent name their clause spells, so a family a child or an anonymous
  instance in its file extends through a type alias (`type SA = S; new SA {}`) is refused
  (`sealed family is extended through an alias`).

## References

- Scala Language Specification, ch. 2 (bindings and precedence: inherited members are definitions
  of the template's scope) and §5.1 (base types, member types as seen from a subclass, matching
  members and overloading). **Classical**; the behaviours relied on are pinned by the 3.9.0
  compiler fixtures above.
- Scala 3 reference, "Changes in Implicit Resolution" (nesting decides between lexical
  candidates) and "Enums" (case desugaring). **Classical**, not reachable this session; the
  fixtures stand in for them.
