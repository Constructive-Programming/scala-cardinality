# Names in the stored-value estimate

A count is only as good as the declaration each name in a signature denotes. The stored-value
estimate (`capacity/*`), and the constructor-slot reading it lends the method side, resolve a
type name the way Scala binds it; when the binding is outside the supplied sources, or not
followed, the name is a reason (`?`), never a same-named declaration found elsewhere.

## The rules

- **A type owns its name; an object is a term.** Scala keeps types and terms in separate
  namespaces. `object Foo`'s value has the type `Foo.type`; the type `Foo` is the trait, class,
  enum or alias of that name. So an object has no type equation: a companion never stands in for
  an open trait (open: no bound) or for a sealed family it holds the cases of. A `case object`
  that extends a sealed parent counts once in the parent's sum — the statement that defines it
  says it is a module, whatever type shares its name (`case object Red` beside `type Red =
  Boolean`). A source total counts a companion as its own one value.
- **Objects and packages are namespaces.** `O.T` is the member `T` that `O`'s body defines,
  `O.P.T` a member of a nested object, `a.b.T` a member of package `a.b`.
- **A qualifier's first name binds as an identifier does.** Innermost first: a term an enclosing
  body binds — an object, a `val` (with its declared type, or the class a `val x = new C`
  constructs), every variable of a pattern `val` (`val (p, n) = …`, of no type read here unless
  ascribed), a template's self alias (`self: Prism =>`, of the type it declares), a constructor or
  method parameter, a `def` — then an explicit import or export (followed to the member it names,
  [stored-value-families.md](stored-value-families.md)), then a term of a
  namespace a wildcard names (latest first; a wildcard over a namespace no source supplies may
  bind it, so it stops the read), then an object, value or subpackage of the packages the read's
  clauses open, innermost first (`package a; package b` opens `a.b` and then `a`, so `a.b.x` —
  an object, or a `val x: Foo` — shadows `a`'s object `x`; a single `package a.b` opens only
  `a.b`), then a root package. A value whose type is not known is not followed: `p.X` stays a
  reason rather than reading an outer object `p`. A wildcard's own path resolves the same way, so
  `import x.*` inside package `p` names `p`'s object `x` before a root package `x`, and
  `import Scanner.*` names an object. There is no "the one object of that name anywhere" fallback.
- **A template body binds what its parents and self type pass on.** Inside the body of a class,
  trait, enum or object, a bare type name or a path's first name binds the template's members —
  inherited ones and its self type's included — before any enclosing package member, import or
  root import (Scala 3.9.0 reads `Boolean` in `object O extends Base { case class U(b: Boolean) }`
  as `Base`'s `type Boolean = Int`, over a package's own `Boolean` too, and `p.X` there through an
  inherited `val p: H.type` as `H.X`). A declaration at or inside the body keeps its meaning, and
  an enclosing class, object, method or block that declares the name too makes the reference
  ambiguous (E049), so a valid program never asks this reading to choose there. Parents are read
  where the `extends` clause is written, outside the body, and so are a primary constructor's
  parameters (`class K(b: Boolean) extends Base` holds the builtin's `Boolean`). A member is read
  in the body of the parent that declares it, through every supplied ancestor. A `private` member
  is not inherited; a qualified-private one (`private[pa] type Boolean = Int`) is: Scala binds
  the name to it from anywhere and rejects an access from outside `pa` (E173), so in a valid
  program the reference is inside `pa` and reads `Int`. An abstract member is the implementing
  instance's to choose (`member T`, as the template's own); an opaque one is outside its scope;
  one whose parent the `extends` clause applies to type arguments is refused (`inherited type
  member not resolved: Boolean (type arguments of BaseP)`). A parent outside the sources may
  declare any name, so where the meaning would otherwise come from a package, an import or the
  root imports, the name is refused with the parent named (`inherited type member not resolved:
  Boolean (unsupplied parent zz.Base)`; a path's first name is a term, `inherited term member not
  resolved: scala (…)`) — unless the stdlib model knows the parent declares no type member
  (`Serializable`, `Product`, the throwables), spelled where no source binds it. A parent spelled
  through an alias or a value is not read either (`parent BA, whose declaration is not read`,
  `parent h.B, whose prefix is a value`). A parent's `val` and `var` parameters are members it
  passes on (a case class's first clause too; a plain parameter is not one), read where the class
  is written. A parent's export clauses define alias members, which are not followed: a name one may
  define is refused (`inherited type member not resolved: Boolean (exports of B)`). This is the
  method side's `InheritedTypeMembers` rule, with the same reasons, so a constructor slot the
  estimate lends reads the same member; before register D29 the estimate read the builtin there
  and the slot was refused as a disagreement.
- **A bare name binds as Scala binds it.** A definition of an enclosing body (or a member a
  template around it passes on, above), then an explicit
  import or export (followed), then the wildcards in scope, latest first — a supplied
  namespace (package or object) that defines it, or a namespace no source supplies, which may
  bind any name and so stops the read (`StringBuilder (may be imported by
  scala.collection.mutable.*)`) — then the packages the read's clauses open, innermost first, then
  the standard library's base types and modelled constructors (`Option`, `List`, …): a root
  import is the outermost binding, so another file's `Int` in the same package, a source trait
  `List[A]`, an imported `Option` or a sealed source `Option` each win over the standard one.
  A wildcard outranks another file's package member; a definition of the reading file itself
  outranks every wildcard (scalac rejects an outer definition and an inner wildcard that both
  bind a name as ambiguous, and in one scope the definition wins). Two wildcards of one body
  that both define the name are an ambiguity Scala rejects: a reason, never either. For the same
  reason a wildcard that only *may* bind the name (its namespace not supplied) is no binding
  where another wildcard of its body defines it, whichever is written last. A wildcard of a
  stdlib namespace binds what `ListedNamespaces` — the table the method side reads too — lists:
  `scala.jdk.CollectionConverters.*` its conversion classes, `scala.quoted.*` its six types and
  eight terms, `quotes.reflect.*` (through `scala.quoted.quotes`) any name but a root spelling the
  estimate reads — and of the root imports' names it binds `Singleton` and `Symbol`, its own. A
  `given` selector (`import zz.given`, `import zz.{given T}`) binds the given instances of its
  namespace, terms of any name: it binds no type, but may bind a path's first name (`compose` in a
  later `import compose.*`) where its namespace is not supplied, and binds a supplied namespace's
  given value only. A package some file declares a member in is read as complete (the
  closed-package assumption); one the sources spell only as the prefix of a deeper package (`zz`
  for `package zz.inner`) holds its subpackages and may hold anything else, so its wildcard may
  bind any other name. A package also holds what its package object's parents pass on and what
  its files' top-level export clauses define, and a read inside the package that finds the name
  nowhere nearer binds that member over the root imports (scalac reads `lib.U(b: Boolean)` as an
  `Int` through `package object lib extends basep.Base`, and `lib3.W`'s through `export Src.*` in
  another file). An export over an object the package defines, in a file that imports nothing
  before it, is followed — `export Src.*` makes `Src.Boolean` the package's `Boolean` and `Src.q`
  its `q` (a path's first name), a renamed selector only the name it binds, a wildcard no given;
  a name the exported object does not define is not exported, so `scala.Boolean` stays the root
  package's. The exported object's own parents may pass on any name (`Boolean (may be exported
  from Src, an inherited member not followed)`). A package object's parent, or any other export
  clause, may bind any name the files do not define, so the name is refused (`Boolean (may be a
  member of package lib, an inherited member not followed)`, `of the empty package` in a file with
  no package clause), and so is a path's first name.
- **An enum's companion holds the enum's cases.** `object E` beside `enum E` — or, with none
  written, the one Scala synthesizes — is where Scala puts the cases: `import E.*`, `export E.*`
  and a bare name inside an explicit `object E` bind them over every outer or other-file name
  (scalac binds `A` to the case class `E.A` over a package's `type A = Unit`). The cases'
  parameters read in the companion too, explicit or not (`case C(a: A)` is `C(E.A)`: scalac
  builds `E.C(E.A(true))`), its parents' members included, under the case's own copy of the
  enum's type parameters; and the enum's own body imports the cases by name (`type W = Option[A]`
  and a body class `In(a: A)` read the case: scalac rejects `e.In(())`). A class case (`case A(i:
  Int)`) is a type and a term, a singleton case (`case B`) a term only (`case Boolean` binds no
  type `Boolean`). A case class is not read as a type of its own, so every name that binds one is
  refused, as the method side refuses it: `enum case type not read: en.E.A`.
- **Clauses bind where Scala says.** An import binds the statements after it in its block or
  template, never the ones before (`UseBare(o: Obj); import x.*; After(o: Obj)` reads two
  different `Obj`s). An export defines members of its template, so it binds the whole body: an
  `export x.Obj` names `x.Obj`; an `export Src.*` makes `Src`'s members the body's,
  outranking every binding outside it.
- **Each file of a package reads with what that file adds.** A package's definitions across files
  solve as one system, but each definition reads with its own file's package clauses and with the
  clauses written before it, and with the other packages behind it: a member whose file imports
  `zz.Widget` holds the import's `Widget` for every reader, never the package's own; a file's
  wildcard outranks a name only another file of the package defines, for every reader too.
  Packages are solved in the order their reads need — a package after every package it reads,
  however long the chain — and packages that read each other in a cycle together, in rounds.
- **An object's members are solved with the body that defines the object.** Each member joins
  that body's system under its key (`O.T`) and reads in the object's lexical view: its own types
  first, its explicit imports shadowing the enclosing body's names, then the enclosing body. A
  recursion across an object boundary is one fixed point (`A(b: Option[Q.B])` with
  `Q.B(a: Option[A])`: `A = 2 + A`, least fixed point ω).
- **Class members and module members are kept apart.** `x.T` with `x: C[ā]` is class `C`'s
  member, read in `C`'s own body — where `T = Span` means `C`'s `Span`, not the reader's — with
  `C`'s parameters bound to `ā` as read where `x` is bound (a package's `val` in its package).
  `C` binds as any type name does: a type parameter (`Holder[Prism <: Other](val p: Prism)`), or
  an alias with no members of its own (`type Prism = Other`), stops the read — never a same-named
  class elsewhere. `O.T` is the object's member.
- **A qualified reference resolves through its qualifier, or is a reason.** There is no
  last-segment fallback: `type Bytes = a.Bytes` is `a`'s `Bytes`, not itself. The one exception
  is the standard library spelled in full under `scala`, `scala.Predef`,
  `scala.collection.immutable`, `scala.util` (and `java.lang.String`), read as the base types and
  constructors the algebra already models — and only when no source namespace of that path
  exists.
- **A qualified read carries the reasons of what it reads.** A member whose value rests on an
  unknown name (`O.Outer` holding an `Inner(s: Schema)`) is no more bounded than the member: the
  reference collects the reasons of every definition the member's equation reads, transitively,
  once each. The member's own type parameters are bound while its template is read (the
  reference supplies them or reads the template on purpose); every other binder stays a reason,
  whichever definition reads it — a nested definition's `A` is not the member's `A`.
- **Binders shadow.** A type parameter no instantiation has bound is the parameter, whatever a
  sibling type or a base type of the same name holds. A type a body defines shadows an enclosing
  definition's parameter of that name — an abstract type member too (`trait Holder { type W;
  U(w: W) }` holds `Holder`'s `W`, open to each implementor, never an outer `W`). Every equation reads with the binders of the place it was
  written, and an instantiation reads its own frame only. Only a definition's own recursion — its
  name applied to its own parameters inside its own equation — borrows the fixed point the solver
  solved under its name; the same spelling anywhere else (`Q[A](p: O.P[A])`) is an instantiation
  at the reader's own `A`, which is the reader's reason.
- **A match type's case body reads where the match type is written.** `O.M[Int]` with
  `type M[X] = X match { case Int => Span }` in `O` is `O.Span`: the arguments, and the parts of
  them a case pattern binds, are read where the reference is, the case body in `M`'s own scope
  with those bound.
- **A generic definition named without arguments is a type constructor.** `type Qual = O.Pair2`
  has no value space of its own (|Pair2\[Nothing\]| = 0, |Pair2\[Boolean\]| = 4): `type
  constructor: O.Pair2`, never the template's placeholder.
- **A constructor slot is read where the constructor is written.** The method side borrows the
  estimate's reading of a constructor's parameter types; that reading descends the package
  clauses, objects, classes, methods and blocks around the constructor — with the clauses written
  before each statement it enters — binds its type parameters (a method's too) and its sibling
  parameters (`Dep(q: Other)(x: q.X)`), and lends a size only when every name it read —
  transitively — is bounded. When it does not, the method side's shape model may not stand in:
  it counts a slot as an argument the constructor is handed (`|U|^|W|`), the convention for a
  slot over a type parameter, not the value space the convention counts for any other slot. So a
  ground slot the reading refuses makes the row `?`, whatever the shape model answers, and so
  does a slot over a type parameter refused for a declaration of the sources the reading could
  not read whole (an import not supplied, a family not summed, a cycle not solved). The row
  names what the reading stopped at — with the clause a name comes from — beside the shape
  model's own reasons, each once.

## No number, and why

- **Sealed families that are not closed.** A family is summed over its file's subtypes, or refused
  with `sealed family not summed: P (why)` — see
  [stored-value-families.md](stored-value-families.md).
- **Lazy cycles the ν completion could not finish.** A by-name or function field naming a
  definition that reaches back (`S(head, tail: => Cell)`, `Cell(s: S)`) — directly or through an
  alias the body defines (`tail: Thunk`, `type Thunk = () => Cell`) — lets a program tie a lazy
  knot (`lazy val c: Cell = Cell(S(true, c))`); when coiteration is blocked (here by the strict
  `s: S`), the least fixed point counts only the finite values, an under-count. Every row on or
  reading such a cycle says `lazy cycle not completed: X`. Source totals keep the μ value.
- **Packages that read each other in a cycle.** Solved together in rounds; a read down the cycle
  longer than the rounds stops at a member of a package not solved yet, and says
  `cross-package cycle not solved: q.B` — never a supplied declaration called unresolved.
- **Clauses the sources do not supply.** An explicitly imported or exported name whose path names
  no supplied declaration stays a reason, with its clause: `Widget (imported from zz.Widget, not
  supplied)`; a name a wildcard over an unsupplied namespace may bind says so: `Int (may be
  imported by cats.syntax.all.*)`; a listed stdlib namespace's own member is the library's
  declaration, not supplied: `Expr (imported from scala.quoted.Expr, not supplied)`. A wildcard
  over an object or a package object binds what its parents pass on and its export clauses define
  too, and no private member outside where it is accessible (Scala 3 reference, given imports;
  fixtures `probes/a7fix2/stored`): this reading follows none of them, so a name the body does not
  define, under a namespace with parents or exports, and a private member are refused: `Two (may
  be imported by Lib.*, an inherited member not followed)`, `compose.C (may be imported by Priv.*,
  a private member not followed)`. A wildcard binds no given, a `given` selector no other member.

## Derivation

Each rule is Scala's name binding, which decides what declaration a name denotes; the counting
rules downstream are unchanged. Types and terms are separate namespaces; a qualified identifier
`p.T` denotes the member `T` of the stable path `p`; package clauses make the members of the
packages they open visible (and only those); and bindings follow the precedence order local
definition or inherited member (or a package member of the same compilation unit), explicit
import, wildcard import, package member of another compilation unit, with a binding in an inner
scope shadowing bindings of lower precedence in its scope and of the same or lower precedence
outside it; a binder is innermost in its definition (Scala 3 draft specification ch. 2 \[28\] and
SLS 2.13 ch. 2 \[32\] in the verified bibliography, both **Inspected (full text)**). The
least-fixed-point reading of an object member solved with its enclosing body agrees with solving
the body first and the object after (Bekić's lemma — **classical**). A lazy knot is a value the
least fixed point does not contain: it is the greatest-fixed-point part the ν completion counts
(docs/type-arithmetic.md §8), so when that completion is blocked the μ value is a lower bound,
not the count.

**Compiler fixtures** (Scala 3.9.0's compiler run on the fixture sources, this session): `Foo.B` with a trait `Foo` and no `object Foo` is rejected; `package a.b` does not
see `a`'s `W` while `package a; package b` does; a single `package dev.app2` clause does not see
`data`; `package a; package b; type Y = x.T; val c: Y = ()` reads the subpackage `a.b.x` over
`a`'s object `x`; `import x.*` binds `x.Obj` over package `p`'s `Obj` from another file; another
file's `final case class Int(b: Boolean)` in the same package binds over `scala.Int`; an inner
object `p`, a `val p = new Prism2(1)` and a sibling parameter `q: Other2` each shadow an outer
object of that name (`val check: X = ()` type-checks); `p.X` with `Prism`'s `X = Span` is
`Prism`'s own `Span` (`val check: Y = p.Span(true)`); a local class in a method body is the one a
sibling local class's parameter names; `lazy val c: O.Cell = O.Cell(new S(true, c))` builds a
value of the blocked cycle; and a cross-file sealed hierarchy is rejected ("Cannot extend sealed
trait Nat in a different source file").

Further fixtures (this session): `UseBare(o: Obj); import x.*; After(o: Obj)` binds `p.Obj` and
then `x.Obj`; an outer definition and an inner wildcard that both bind `Obj` are rejected as
ambiguous, while in one scope the definition wins; `import Scanner.*` binds the object's `Span`
over another file's; `import x.*` inside package `p` binds `p.x`'s `Obj` over the root package
`x`'s; `export Src.*` binds `Src.Obj` over the package's `Obj`; a `val x: Foo` of package `a.b`
shadows `a`'s object `x`; `val (p, n) = …` and `self: Pr =>` bind `p` and `self`; `O.M[Int]` reads
`O.Span`; `trait Holder { type W; U(w: W) }` with `Impl` choosing `type W = Int`.

Further fixtures (Scala 3.9.0, the integrated review's `probes/stored/{inh,self,inst2,inhterm,
unsup,t3,en,en2,en3,en4,pkgobj,pkgobj2}` and `z1b/fx/{inh,chk,spec}`): an inherited alias binds over
a package member of the same unit and the root imports, and an outer object's definition beside
it is E049; a nested object's own definition is nearer than what its owner inherits; inherited
`val p: H.type` and `object Inner` bind a path's first name over the package's objects; a
primary constructor's parameter reads the builtin; `import E.*` over an explicit companion binds
the case class and no singleton case as a type; a package object's parents and another file's
`export Src.{Boolean as Flag}` bind inside the package.

**Executable**: `NamespaceSpec` and `NamespaceScopingSpec` (positive and adversarial
source-to-count regressions for every rule above), `StoredScopesSpec` (inherited members, package
members, enum companions), `ReportSpec.memberByName`,
`ReportSpec.crossSourceSealed`, `ReportSpec.crossSourceImported`.

## Limits (what stays `?`, and why)

- **A wildcard over an unsupplied namespace** may bind any name the reading file does not define
  — a base type, a modelled constructor, another file's package member, another wildcard's first
  name — so each such name is refused, naming the clause (`cats.syntax.all.*`, `zz.given` for a
  path's first name, and `quotes.reflect.*` for any name but a root spelling the estimate reads),
  until those namespaces' declarations are supplied; the stdlib namespaces `ListedNamespaces`
  lists bind only what it lists. A package spelled only as a prefix counts as such a namespace.
- **A definition in another block of the reading file's package** (`package p { type T = … }`
  beside `package p { import zz.*; … T … }` in one file) outranks every wildcard (spec ch. 2;
  Scala 3.9.0 fixture `probes/review-A7/r9`), but each block here reads with its own definitions
  only: the estimate refuses `T` there through the wildcard, a refusal where the method side reads
  the definition. No eo 1.0.0 source writes two blocks of one package.
- **A `given` selector over a supplied namespace** binds a given instance the estimate holds as a
  value (a `given x: T = …` alias); a `given x: T with { … }` instance is no term here, so a path
  through it is not refused by that selector.
- **An enum's class case is not read as a type.** `Shape.Circle`, and every name that binds it —
  through an explicit companion, in a case's parameters, in the enum's body — says `enum case type
  not read: p.Shape.Circle`; so does the enum, when a case holds another case. With no explicit
  companion the enum is no namespace for a reference from outside, and a wildcard over it may bind
  anything.
- **Inherited members are read only where this reading can place them.** A parent applied to type
  arguments is not read under them; a nested object's `extends` clause naming a class of the same
  body is read in the body's second solve (`Walk.system`); a name a method parameter or a block
  outside every template declares is refused past an unread parent where Scala would bind it (a
  constructor slot inside a method sees its parameters).
- **A package's members beyond its files.** An export written after an import, over anything but
  an object the package defines by a plain name, or of a private member, is not followed; a
  package object's parents never are.
- **A parent's export clauses** are never followed: a name one may define is refused in the
  subclass's body (`inherited type member not resolved: Boolean (exports of B)`), even where the
  export would bind what the reading would have found anyway.
- **A blocked lazy cycle with no knot.** `X(b: => X, a: X)` admits no value (the strict `a`
  demands one first), so μ = 0 is exact, but the cycle is still read as blocked.
- **A pattern variable or an untyped self alias** is a value of no type read here: a path through
  it is its own reason (`val (p, n) = (new Prism, 1); type Y = p.X`).
- **A class member through a bounded type parameter or an alias** is `?` (the bound's member is
  the value only when the member is concrete there; not decided here).
- **Cross-package cycles** deeper than their rounds (the cycle's size plus three) say `cross-package
  cycle not solved`; a definition-level recursion across packages (`p.A(b: Option[q.B])`,
  `q.B(a: Option[p.A])`) always does.
- **The method side's own resolver** (`SourceLookup`) scopes type parameters and package members
  on its own, and does not yet bind paths through vals and parameters as this reading does
  (constructor slots catch it as a disagreement). It refuses the names and paths' first names a
  wildcard over an unsupplied namespace may bind as this reading does, through the same table of
  listed namespaces ([stdlib-model.md](stdlib-model.md), "Listed and unsupplied wildcards"). A
  slot the estimate declines makes the constructor row `?` when the slot is ground or rests on a
  declaration the reading could not read whole; a slot over a
  type parameter refused only for syntax the estimate does not model (`A =:= B`, a type lambda, a
  projection) stays the shape model's to count, as the convention for such slots says.
