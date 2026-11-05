# Sealed families, imports and reasons in the stored-value estimate

The stored-value estimate (`capacity/*`) counts a definition's constructor inputs. A number is
reported only when every name it reads is resolved and closed. This page states the rules that
decide that for sealed families, explicit imports, inherited reasons, context bounds, finite
collections, abstract type members, stuck match types, floating-point marks and reason classes,
and what stays `?`. Name resolution itself is [stored-value-names.md](stored-value-names.md).

## Sealed families are tagged sums over their file's subtypes

`sealed` means that every direct subtype of a trait or abstract class is declared in its source
file. Matching on the runtime class tells those subtypes apart. So the parent's value space is
the disjoint union of its subtypes' spaces (type-arithmetic §2; recursion is the least fixed point
of §8):

  |P\[t̄\]| = Σ over the direct subtypes D of |D\[ȳ := t̄\]|

This holds when every value of `P[t̄]` belongs to exactly one subtype the file names, and every
subtype belongs to every instantiation of `P`. The estimate checks both conditions syntactically
(`SealedFamilies`):

- **Where the subtypes are.** Subtypes are the templates in the parent's body and in the objects
  nested in it at any depth (the companion is the usual case), whose `extends` clause names the
  parent plainly, as `P` or `P[ā]`, where that name denotes the parent. The name is read as Scala
  reads it, in the file (`ParentNames`): the innermost scope around the clause that declares,
  inherits or exports a type of that name binds it. Scala 3.9.0 rejects an inner import of a name
  an outer definition of the same file binds as ambiguous, but an inherited member or an export
  rebinds it silently: in `object O extends Base { case object Z extends P }` with `trait Base {
  sealed trait P }`, `Z` extends `Base`'s `P` (scalac: `val y: P = O.Z` fails), and the top-level
  `P` sums `A` alone. A parent or export path the file does not declare may rebind the name, so a
  subtype written under one is a refusal. A subtype may also extend the parent under another name:
  an alias of it, or a name an import or export renames it to (`import x.{P => Q}; case object Z
  extends Q`). Every such name, transitively, is searched for, in the file and in every supplied
  source, and a subtype found through one is a refusal rather than a missing summand.
- **What each subtype contributes.** An object contributes 1. A case class, final class or enum
  contributes its own space. A case class no `final` closes may still be extended in any file (by
  a class that is no case class), and a subclass may override a member it has — its own, a
  parent's of its file, or one of a parent outside the file (`Exception.getMessage`) — so its
  values differ beyond its fields (Scala 3.9.0: a subclass of a non-final case leaf overriding
  `getMessage` prints another message for equal fields). Such a leaf is refused: `a subtype open to
  extension: C (overridable member m)`, or `(parent not read: P)` for a parent that is neither a
  class or trait of the file the name denotes where the clause is written nor one the stdlib
  model reads (one table for both sides: a root or marker type, `AnyVal` among them, opens nothing;
  `Exception` opens on its `getCause`) — and for a library parent with nothing to override, also
  where a source binds its name at the clause (an import of a source `Product`, a declaration of
  the package in another file). Members the compiler synthesizes for a case class (`Product`'s,
  `Equals`'), a restated universal member and a field of the leaf (whose override is another
  value of its type) open nothing; an extension method of the leaf and a parent's `val` parameter
  do. A parameterless member fixed anywhere in the ancestry fixes its name for every subclass
  (`trait P extends GP { final override def d = 2 }` closes GP's `d`); a later clause's plain
  parameter or a member with parameters fixes nothing. In the closed world (register D39) a leaf no
  supplied template extends is final, and nothing opens it. The method side reads its parents through
  the supplied sources and the stdlib model (`sealed family leaf may be extended: C (overridable
  member Exception.getCause)`). A sealed intermediate contributes its own sum. A subtype reached
  twice (`C extends D1 with D2`, both sealed under `D`) is refused, because its values would be
  counted twice.
- **Instantiation.** A subtype `C[Ȳ] extends P[ā]` must bind each of its parameters to exactly
  one slot `aᵢ = Yⱼ`, or pin a slot from below (`Nothing` into a covariant slot, `Any` into a
  contravariant one). `P[t]` holds `C[u]` for every `u` the slot's variance allows, so the
  summand `|C[t]|` is exact only when those `C[u]` add nothing:
  - an invariant slot holds `C[t]` alone;
  - a covariant slot holds `C[u]` for every `u <: t`. That union is `C[t]` when `C` is covariant
    (`C[u] <: C[t]`), or when every field of `C` holds a `u`-value only where a `t`-value stands
    too: the parameter itself, tuples, unions, by-name and repeated ones, a function's result,
    the covariant slots of a constructor of the file that the field's name denotes where it is
    written, and the standard data constructors where no source binds their name. A field `f: A
    => Boolean` of an invariant `E[A]` is refused: `E[u]`'s functions are no `E[t]` fields; so is
    `xs: List[A]` when another file of the package says `type List[A] = A => Boolean`;
  - a contravariant slot holds `C[u]` for every `u >: t`: `Use(C[Any]("x"))` is a `Use` of
    `Use(p: P[Boolean])` with `P[-A]`, whatever `C`'s field says. Only a contravariant `C`
    (`C[u] <: C[t]`) is summed.

  A subtype parameter's bounds must be the slot's: `C[A <: Int] extends W[A]` is no member of
  `W[Boolean]`. Spelled alike is not enough, since the two bounds are read in different scopes: in
  `object O { type X = Nothing; final case class C[A <: X](a: A) extends P[A] }` with a top-level
  `type X = Any; sealed trait P[A <: X]`, `C[Boolean]` is ill-formed and `P[Boolean]` holds no `C`.
  So a bound means its slot's only when each parameter it names is the subtype parameter bound to
  the slot parameter of that name, and every other name reads in the same scope: the subtype is
  a statement beside the parent, with no import between them. A bottom pin must be the standard
  library's `Nothing` or `Any` where the clause is written: with `type Nothing = Boolean`, `Z
  extends P[Nothing]` is a `P[Boolean]` only. The slot logic is `types/SealedSlots`, which the
  method side's representation shares, bounds and variance included; each refusal names the
  subtype and only the parameter at fault (`sealed subtype binds a parameter to no slot of its
  parent: Fused (M0, of Optional)`).
- **Recursion.** An instantiation that recurses on its own parameters is its own least fixed
  point, found by Kleene iteration from 0 (`|L[Nothing]|` = μX. 1 + 0·X = 1, `|T[Nothing]|` =
  μX. 0 + X² = 0). The template's fixed point does not work here, because it reads the
  parameters as placeholders. Iteration widens after two consecutive growths, as the body's
  solver does. A sum of products that grows twice grows without bound. A recursion through `&`
  (read as `min`) may still settle, so the widened value must be one the equation does not fall
  below; otherwise the row reads `recursive instantiation not settled`.
- **Only positive recursion counts** (`Positivity`). Type-arithmetic §8 counts covariant
  constructions. Through a function's domain the equation is no count: `MC(f: M => Boolean)`
  iterated from 0 reads 0 by the empty-domain rule, yet `MC(_ => true)` is a closed value, and
  X ≅ 2^X has no solution (Cantor). A cycle of a system's definitions through a function's
  domain, a partial function's keys or a match type is refused: every definition on it reads
  `recursion through a function's domain: M`, in the body's solver and in an instantiation alike
  (`N[A]`, `Pred[K]`). A recursion through a function's result is the lazy reading of §8 and stays
  counted.
  - A constructor's slots are those of the declaration its name denotes where it is written,
    never those its spelling suggests. A definition of this system, or one solved around it (any
    supplied file or package), gives the slots its own positivity reading found, with their
    cause: `List[A](f: A => Boolean)` in another file makes `P(l: List[P])` a recursion through
    a function's domain, and `Box[A](a: A)` keeps `N(kids: Box[T]) extends T` counted (ω). A bare
    standard name no source binds takes the standard table. Any other constructor — a clause no
    source supplies, a type parameter applied — is not read: `recursion through Chunk, a
    constructor the estimate does not read: T`.
  - A field typed by a member of a value it stores (`x: q.X` beside `q: C`) holds what `C` makes
    `X`. The solver reads that member over an earlier solve of the body, so a cycle through it is
    never iterated from 0, whatever its sign: `type X = D => Boolean` is a function's domain,
    `type X = D` (no value) and `type X = Option[D]` (ω) read `recursion through a type member of a
    value: D`. Before, all three read the earlier solve's ω placeholder as a number.
- **Match types in a subtype.** A match type in a subtype reduces on the parent's arguments as
  the reference wrote them, and each part is read where it was written. For example, `Miss[A](fst:
  Fst[A])` read for `Affine[(T, M0), B]` reads `Fst[(T, M0)]` = `T` in the reference's scope.
  This is what makes eo's `Optional.Fused` depend on its instantiation only.
- **A case is passed over only when provably disjoint** (Scala 3 reference, *Match Types*; draft
  spec ch. 3: a match type reduces to a case "if the scrutinee matches its pattern, and if it is
  provably disjoint from every earlier pattern"). The estimate passes a case that does not match
  only for tuples of different arities or with a disjoint element pair, two different literals,
  or two different classes of which one is final (a tuple, a literal's class, a final standard
  type; a function type is a trait none of them extends). `H[Any]` with `case Int => …; case _ =>
  …` is stuck (scalac: "cannot be shown to be disjoint"), not the second case. A pattern spelled
  as the scrutinee matches only where each of its names means one declaration as the match type
  and the arguments read it: `F[Int]` written where `type Int = String`, against `F`'s `case Int`,
  is stuck (Scala takes `case _`, since `String` is disjoint from `scala.Int`), never the spelling
  match.
- **Enum cases with their own `extends`.** A case `Y extends E[Int]` is an `E[Int]` only, so an
  enum's sum over every case is refused for it, directly and as a family's subtype: `enum case not
  summed: E.Y (…)`. A case bottom-pinned to the standard `Nothing` is a member of every
  instantiation.

**Refusals.** The family, its row and every reference read `sealed family not summed: P (why)`.
The reasons are:
- an anonymous instance (`new P {}`) or a `given P with` anywhere in the file;
- a plain class subtype that is not final;
- an abstract subtype that is not sealed;
- a subtype nested in a class, trait or block (one class per enclosing instance);
- a subtype reached through a qualified name, a type alias or a renaming import or export. A name
  counts if any alias or renaming in the file, or anywhere in the supplied sources, may stand for
  the parent, transitively;
- a subtype whose clause may name another type of the parent's name: an object around it
  extends a parent, or exports through a path, the file does not declare;
- a slot pinned to a fixed type, which is a GADT index (`extends W[Int]`, or an invariant
  `W[Nothing]`), or bottom-pinned with a `Nothing` or `Any` a source binds;
- a field that holds a parameter through a standard data constructor's name a source binds;
- a subtype parameter that no slot binds exactly once (`Optional.Fused`'s `M0`);
- a subtype parameter bounded otherwise than its slot (or alike only in spelling), or varying
  otherwise (above).

Private constructors are not refused here. The estimate asks which values exist, not who may
build them.

## Explicit imports and exports are followed

`import p.q.N` and `import p.q.{N as M}` bind the name to the member `N` of the stable path
`p.q`. The path is read as Scala reads an identifier at the clause, with the clauses written
before it; an export's path too, so `import ea.Inner; export Inner.T` exports `ea.Inner.T`, never
another file's `Inner`. A selector binds a name in the namespaces its member has, and a body's
own definition outranks it in that namespace only: a `val W` beside `import to1.W` leaves the type
`W` imported, and an import of a member that is only a term (`object W`) binds no type, so the
type lookup goes on outward. A renamed member is no longer imported under its own name, by the
wildcard beside it either (`import ra.{W => V, *}` leaves `W` to the package). A path to a supplied package or object reads that namespace's member, and the solver's
dependency graph links the member's key. Any other path is a reason that names the clause:
- `not supplied` when no source declares the path;
- `not followed` for a member reached through a value;
- `<first> may be imported by w.*` when an earlier wildcard over an unsupplied namespace may bind
  the path's first name (as `cats.syntax.all.*` may bind `org`; a listed stdlib namespace binds
  only the terms `ListedNamespaces` lists, so `scala.jdk.CollectionConverters.*` binds no `org`).
  In that case the clause may still name a supplied declaration, so the reason does not say it is
  not supplied.

Exports follow the same rule, and an export defines a member of its template, which outranks an
import of the same name there and an outer definition of it (`import X.T; export Y.T` binds `Y.T`).
A member an object inherits or exports is not followed: its reason says `inherited member not
followed` or `exported member not followed`, not `not supplied`. This reverses the earlier "not followed" behaviour:
`ReportSpec.crossSourceImported` now reads 2.

## A reference carries the reasons of what it reads

Each solved value can hide ω placeholders that stand for unresolved names. A row therefore
collects the non-parameter reasons of every definition it reads, transitively, once per
definition (`Taint`). The parameters a reference supplies are not collected. A reason holds even
when the arithmetic would absorb it (`Zero(n: Nothing, q: Q)` with `Q(s: Schema)` reads `?`, not
0). The method side's constructor-slot reading inherits this rule, so `AvroFailureException.<init>`
is `?` because one case of `AvroFailure` holds a `Throwable`, an open class the estimate leaves to
its implementations (register D26).

## Context bounds are stored evidence

Scala 3 desugars `class C[T: TC]` to `class C[T](using TC[T])`. The evidence is a field, so the
estimate multiplies by `|TC[T]|`, once per bound; a named bound `T: TC as tc` names the field and
its type is still `TC[T]`. An enum case declares its own bounds: scalac does not copy the enum's
bounds onto a case.

## Finite collections keep their element's tier

`|List[A]| = Σₙ |A|ⁿ` (type-arithmetic §4). The n = 1 term alone is `|A|`, so a finite
collection is never below its element's tier:
- over an element with an ε₀ term (`String => String`, `Any`), `List`, `Vector`, `Seq`, `Array`,
  `Set`, a `Map`'s keys or values, and repeated parameters are ε₀;
- over any other non-empty element they are ω (`Set[Boolean]` keeps 2² = 4);
- over `Nothing` they are 1.

The method side keeps its own saturating reading: its constructor slots follow its own model.

## Template members and stuck match types wait for the instantiation

Inside a class, trait or enum, an abstract `type M` is `this.M`, which each instance chooses. A
row that reads it depends on that choice in the same way it depends on a parameter. It renders
as `member M`. An object's abstract member has one instance and stays open.

Some match types are stuck only on parameters the reading has not fixed, such as the
definition's own or the template's a reference reads. Each instantiation either reduces such a
match type or is stuck itself, so those parameters are its reasons. The definitions the
scrutinee names (`Fst[Fst[T]]`) are the same for every instantiation. A match type stuck on
anything else stays `a match type`. That covers a concrete scrutinee that no case matches, and a
parameter an instantiation bound to a size whose syntax is lost.

## Marks and classes

A `Double` or `Float` stands in as a lossy width, and that mark survives sums and products:
`≤2^65 (lossy)`, a bound as every width is. A row's reasons fall into three classes, in this order of precedence:
- **unresolved**: an unknown name, a clause, unmodelled syntax or a refusal;
- **open**: an abstraction any code may extend;
- **instantiation-dependent**: the row's own parameters and members.

A row belongs to the first class any of its reasons is in, and all classes are rendered:
`unresolved: Schema; open to implementations (Open); depends on its instantiation (A)`.

## Evidence

- \[30\] Scala 3 draft specification, ch. 5, and \[33\] SLS 2.13, ch. 5 (**Inspected (full text) —
  bibliographer**, research bibliography T5): a `sealed` class "may not be directly inherited,
  except if the inheriting template is defined in the same source file", and "a `final` class may
  not be inherited by a template".
- \[24\] Scala 3 reference, *Translation of Enums and ADTs* (**Inspected (full text) —
  bibliographer**, T5): an enum expands to a `sealed abstract` class, a class case to an analogue
  of a final case class, and a case with an explicit `extends` takes the enum's parameters only as
  its fields or parents mention them.
- \[22\] Scala 3 reference, *Open Classes* (**Inspected (full text)** (verifier), T5): a sealed class
  "can still be extended, but only in the same source file". \[29\] Scala 3 draft specification,
  ch. 3 (**Inspected — bibliographer**, T5): a sealed class is disjoint only "without anonymous
  child".
- \[15\] Scala 3 reference, *Context Bounds* (**Inspected (full text) — bibliographer**, T2): a
  bound `: Ord` on `T` "indicates a context parameter `using Ord[T]`".
- Scala 3 reference, *Match Types*, and the Scala 3 draft specification, ch. 3, "Match Types":
  reduction passes a case only when the scrutinee is provably disjoint from its pattern
  (**Inspected (full text) — bibliographer**, research bibliography T4).
- Cantor's theorem (no set X ≅ 2^X) for the refusal of negative recursion (**classical**).
- **Compiler fixture** (Scala 3.9.0, `$S/probes/c2r/valid`, `$S/probes/review-C2`,
  `$S/probes/c2f`, `$S/probes/c2f2/valid`): every family, import and container fixture of
  `StoredValueSoundnessSpec` compiles. Scalac also:
  - accepts a subtype through an alias, an anonymous instance through an alias, and a bounded
    subtype parameter;
  - rejects an inner import or wildcard that rebinds an outer family's name in its file as
    ambiguous;
  - rejects an enum case that relies on the enum's context bound;
  - types `O.Z` as `Base.P`, not the top-level `P`, when `object O extends Base` (or `export
    Other.P`) and `trait Base { sealed trait P }` (`val y: P = O.Z` is E007);
  - accepts `Use(C[Any]("x"))` for `Use(p: P[Boolean])` with `P[-A]` and invariant `C`;
  - rejects `val y: P[Boolean] = E.Y` for `case Y extends E[Int]`, and `val no: P[Unit] = Z` for
    `case object Z extends P[Nothing]` under `type Nothing = Boolean`;
  - leaves `H[Any]` unreduced and reduces `H[String]`, `H[(Int, Int)]` and `H[String => String]`
    to the `case _` body;
  - binds `T` to `Y.T` under `import X.T; export Y.T`, and `export Inner.T` after `import
    ea.Inner` to `ea.Inner.T`;
  - rejects `O.C[Boolean](true)` where `O.C`'s bound reads `O.X = Nothing` (E057);
  - accepts `List[P](A, O.Z)` where `O.Z` extends `P` through `import x.{P => Q}`;
  - accepts two distinct closed values of `P(l: List[P])` with a source `List[A](f: A =>
    Boolean)`, and of `D(q: C)(x: q.X)` with `type X = D => Boolean`;
  - reduces `F[Int]` to `case _` where `type Int = String` (warning: `true` discarded as Unit),
    `K[2]` to `Unit` and `K[1]` to `Boolean` for `case 1`, and leaves `K[Int]` unreduced;
  - reads `W` as the package's own under `import ra.{W => V, *}`, as `to1.W` beside `val W`, and
    as the package's under `import ta.W` of an `object W`.
- **Executable**: `StoredValueSoundnessSpec` (positive and adversarial),
  `NamespaceSpec.sealedCompanion` / `childBinders` / `unsummedFamily`,
  `NamespaceScopingSpec.partialFamily` / `exports` / `groundSlot` / `abstractMember`,
  `ReportSpec.crossSourceImported`.

## Limits (what stays `?`)

- **Mutual recursion between generic families.** `Ev[A]` inside `Od[A]` inside `Ev[A]` is
  re-entered with no joint fixed point and reads `recursive instantiation not solved: Ev`.
  The non-generic case is solved by the body's solver.
- **Negative recursion** has no count: `recursion through a function's domain: M`. A recursion
  through a constructor no reading knows reads `recursion through C, a constructor the estimate
  does not read: M`, even when that constructor is positive.
- **Recursion through a value's type member** (`x: q.X`) reads `recursion through a type member of
  a value: D`, even when it is positive. The solver's budget guard that widens a name still growing
  after twice its rounds marks it the same way; no fixture reaches it.
- **Match patterns by spelling.** A non-binder pattern matches a scrutinee written the same way,
  and only where every name of it means one declaration as the pattern and the arguments read it;
  a pattern that would match by subtyping (`case Any`), or one whose names mean different
  declarations, is not recognised, and the match type is stuck there (a `?`, never a number).
- **Bounds read apart.** A bounded subtype parameter is summed only beside its parent with no
  import between, even when the subtype's scope binds the bound's names to the same declarations.
- **The method side's own family reading** (`Representations`) still spells a renamed parent, a
  shadowed bottom pin and a shadowed data constructor by name; its constructor rows rest on the
  estimate's refusal, its other rows are lane types' (B0).
- **Rebinding the parent's name** through a parent or export path the file does not declare is a
  refusal, even when that parent has no such member.
- **A match type over another match type's result** (`Fst[Fst[T]]` applied to a concrete tuple)
  is not reduced. The inner result is a size, not syntax.
- **Absorption.** `0 × ?` stays `?`.
- **Intersections inside a recursion.** An instantiation's recursion through `&` that settles
  late (`μX. 1 + min(X, 2)`) reads `recursive instantiation not settled`. The body's own solver,
  which this page does not change, still widens a syntactic cycle after its budget.
- **Aliases outside the supplied sources.** Aliases outside the supplied sources cannot name a
  family, so they are not searched. An alias that a family's file reaches only through an
  unsupplied wildcard does not exist in a well-formed source set.
