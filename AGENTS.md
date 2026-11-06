# Agent guide

This file is for coding agents working on `scala-cardinality`. The [README](README.md) is the
entry point for people; this file repeats the rules an agent must not get wrong.

## The modelling assumption: no side effects

Every count assumes that **the measured code has no side effects**. That applies to the
stored-value capacity of definitions and to the method-side implementation counts. Hold to it
in code, tests and docs:

- **Treat a `var` as a `val`.**
  - A `var` member, local or constructor parameter holds the value it was given.
  - Reassignment is never modelled.
  - Never refuse a count, or add a reason, because something is mutable. A `var` field of a
    capability record or class is read like a `val` field, and a captured `var` is an ordinary
    capture.
  - Only Scala's own typing rules still distinguish a `var`. It is not a stable path, so
    `p.type` and `p.T` through a `var` do not typecheck. Word any such refusal as a typing fact,
    not as mutation.
- **Ignore throwing.**
  - A `throw`, or a call that may throw, is neither a value nor an implementation. A body or
    default whose result is a throw supplies nothing.
  - Never withhold a count because code might throw.
  - Exceptions used as data are ordinary values of open `java.lang` classes: a `Throwable`
    passed, stored or returned.
- **Exclude every other effect the same way**, like the universal members of `Any`: I/O,
  logging, `printStackTrace`, `fillInStackTrace`, `addSuppressed`, `initCause`, and so on.
  Divergence, a `null` the type does not declare, unsafe casts, reflection and unrestricted
  runtime type or equality tests are outside the model too.

Further idealizations stand beside it (v1.md §2):

- **Key equality is lawful; a single-pass read is replayable.** A `Map` or `Set` is its abstract
  value (a finite partial function, a finite subset), counted over closed arguments; an
  `Iterator`, `IterableOnce`, `ByteBuffer` or `InputStream` is the sequence it yields. Never refuse
  one for equality-building or single-pass reading; a key over a type parameter is still refused.
- **Builtin names are never shadowed by what is not supplied.** An unsupplied wildcard, parent,
  self type or any unresolved reference rebinds no root-import name (`Int`, `Option`, `Any`, …). An
  explicit, resolved declaration or import still shadows it. A JDK or standard-library wildcard
  binds the names it declares by their rooted names (`import java.util.*`: `List` is
  `java.util.List`), and rooted names with the same values count the same (Java and mutable
  collections, boxes, `Optional`) while keeping their own identity as type arguments.
- **A top type is read by its place in its own arrow.** Input = a parameter or constructor slot,
  read by the code receiving it; output = a result. Each function type and each method is read on
  its own, innermost first: `Any` (and `AnyRef`, `Object`, `Matchable`, `AnyVal`) in that arrow's
  input is one value, in its output ω. Nothing flips through nesting: `f: Any => A` has a
  one-valued input, `g: A => Any` an ω output. `Option` values, `Set` elements, `Map` keys and
  values and sequence elements take the place they sit in; an input `Set[Any]` is 2 (its size is
  unknown), an output one ω. Arguments come before the method: when an argument's space holds an
  output top, two or more result choices are ω, one stays 1, none 0. A top result is not
  one-valued. `AnyRef` / `Object` / `Matchable` are `Any` everywhere (unions and `&` included);
  `AnyVal` keeps its reading. `A <:< Any` carries nothing. On the stored-value side `Any`,
  `AnyRef` / `Object` and `Matchable` are ω. Never refuse because a top-typed position cannot be
  enumerated.
- **Constructor = a method; type = a value whose accessible state is observed.** A constructor
  (`<init>` row) is counted like any method: its parameters are inputs, read by the method side, so
  a top in one (spelled or behind `type Top = Any`, whatever is beside it) is 1, and the row never
  hands a top to the stored-value estimate. The type as a value (definitions row, top-level `val`)
  is the stored side's: a top there is ω. `final class Walk(val cur: Any, val n: Int)`: constructor
  2^32, type ω.
- **`X*` is a possibly empty list** (`Σ_{n≥0} Xⁿ`): `def first[A](xs: A*): A` is 0, `Any*` ω.

## The modelling assumption: a closed world

The supplied sources, with the supplied dependency declarations, are **the whole program**. No
subclass, override or implementation exists outside them.

- A concrete class that no supplied template extends (an anonymous `new C { … }` and a given
  included) is final. A concrete member of a class or trait that no supplied template overrides is
  final. `ClosedWorld` writes those `final` modifiers into the sources before anything reads them,
  so every rule about final classes and members applies from there. A constructor `val` and an
  untyped literal `val` keep their spelling; a receiver asks `Resolver.overriding` for them.
  A class with a non-public constructor is final too where nothing supplied extends it: only its
  scope may extend it, and its values are what its public factories build (Rule B).
- A member a supplied template does override varies across receivers, so it stays a free
  capability, as an abstract member is.
- Abstract members stay capabilities (v1.md §2), and a goal of an abstract template is still any
  tuple of them: the counted body is the one new piece of code, and it may implement an abstract
  template. It does not override concrete code.
- Two templates share an instance only when one extends the other or a supplied template extends
  both.
- The closed world settles which templates exist. It does not settle a name the analyzer cannot
  resolve. An unresolved identity, an alias it does not follow or a scope it does not enumerate
  keeps its refusal.
- Templates are matched by simple name. A parent outside the sources whose members are not known
  may define anything: a member only such a parent may override is refused
  (`member may be overridden through a parent outside the sources`). Two same-named templates in
  different packages share their subclasses, so a member one of them overrides stays open: the
  open-world reading, which can overcount. Resolve parents before relying on such a row. A
  compiled dependency stub is the exception: it spells the parent the compiler resolved, so a
  stub's parent that a stub declares under that qualified name is that template alone.

The analyzer does not certify that a body obeys the assumptions. A `0` means "no pure, total,
parametric implementation", not "no Scala body compiles".

Two conventions sit beside these assumptions (v1.md §2, "Modelling conventions"):

- **Rule B, public constructors build the receiver.** A receiver's fields (or a field-bearing
  argument's) hold what its public constructors build: the largest space any of them builds, not
  the sum. "Public" means callable from the measured declaration's site: a `private[p]`
  constructor counts inside `p`, a `private` one inside its class or companion; any other
  restricted constructor is ignored. A restriction on the class
  or trait itself, on a template around it, or a class local to a block is disregarded: its
  constructors count as public. A public primary builds every field value. A public secondary
  builds at most min(its parameters' space, the fields' space), read only where both are closed.
  A class (or sealed case) with no constructor callable from the site is counted by its public
  factories too: the methods and `val`s callable there whose result (through blocks, local `val`s
  and branches) is a direct construction of it (`new C(…)`, `C(…)`), a family-typed factory
  constructing the case it builds. Each builds at most min(its parameters' space, the fields'
  space); one declaring the class whose body is read as no construction counts 1; the class is the
  MAX over them (a sealed case: one summand of that space). Neither a public constructor nor a
  factory is `?`: `receiver has no public constructor`. A class with a non-public constructor that
  nothing supplied extends is final in the closed world.
- **Rule F, an opaque type outside its scope is one value.** Where its representation is hidden,
  an opaque type counts 1 on both sides (it reads as `Unit`; as a type argument it keeps its own
  nominal identity). Inside its scope the representation is read as before. Never refuse a count
  because a representation is hidden.

## Inline methods count as methods

Count an `inline def` exactly like a `def` unless its body uses a compile-time operation.

- Compile-time operations are `inline if`, `inline match`, anything from `scala.compiletime`
  (`summonFrom`, `summonInline`, `erasedValue`, `constValue`, `codeOf`, `compiletime.ops`, …),
  quotes and splices. They count wherever they appear: in the body, in a definition nested in it,
  or in an inline method it calls.
- `transparent` and `inline` parameters do not disqualify a method by themselves. An `inline`
  parameter has its argument's value, since nothing has side effects (boxing aside: only the
  excluded universal members see it).
- Rule X still applies: a `transparent` result is `?` unless its declared type is a singleton,
  because callers see the type of what the expansion returns.
- An abstract `inline def` counts like an abstract `def`: an inline body may call it as a
  capability, and an override implementing it is read through its own body.
- An implicit conversion the body may meet is typed where the body is written, like any call. The
  closed world supplies every conversion, so one refuses only when it is inline and its own
  expansion fails. That includes an applied selection a conversion may answer: it refuses only when
  a supplied inline method of that name fails.
- A conversion is a candidate for a body only where the body may find it (contract rule, v1.md
  §2, register D43):
  - in its lexical scope: a definition around it or an import; or
  - as a member of an object in the implicit scope of a type the body can reach: the types written
    in the body, the enclosing signatures, and the signatures of every declaration the body
    references or given it may summon, repeated to a fixed point. Implicit scope = the companions of
    those types' parts and of their supplied base classes, the objects those classes are members of,
    and what those objects extend.
  - Every inferred type is built from those signatures, so nothing Scala may insert is missed. A
    type the reading cannot resolve, or a declaration outside the body without a declared type,
    keeps every candidate.
- Other channels to the call site without a compile-time operation still refuse:
  - a member a supplied subclass overrides with an inline method whose expansion fails, selected
    on a parameter or on `this` / `C.this` (a function parameter's `apply` included, and the
    `apply` an extra argument clause selects on a call's result);
  - a runtime class that depends on a type parameter, which is type reification in any form: a
    type test, `Array[A]`, `classOf[Array[A]]`;
  - a given left to implicit search, a local method's clauses included;
  - an inline or contextual extension the body may meet.
- Local definitions are read like supplied ones, and `this` inside a nested template is that
  template.
- Example: `inline def pick[A](x: A, y: A): A = x` counts `2`. With `inline erasedValue[A] match`
  in its body it is `?`, and the reason says so.
- A name the check cannot resolve refuses: never assume an unknown callee is harmless.

## Where the model is defined

The analyzer's code does not define what a number means; these documents do.

- [docs/plans/v1.md §2](docs/plans/v1.md), "Agreed counting contract". It sets the two
  questions (implementation cardinality and stored-value cardinality), the scope and
  equivalence, the assumptions and what each output means.
- [docs/counting-type-inhabitants.md](docs/counting-type-inhabitants.md) covers the method side:
  observational equivalence, parametric atoms, sums, products and exponentials, least fixed
  points and absurd elimination.
- [docs/type-arithmetic.md](docs/type-arithmetic.md) covers the stored-value side.
- The per-rule notes are in [docs/research/](docs/research). The evidence register,
  [code-cardinality-foundations.md](docs/research/code-cardinality-foundations.md), records the
  sources and the status of each rule.

## Correct numbers before coverage

- A wrong number is worse than a `?`. When the supplied sources do not determine a count, report
  `?` with a specific reason. Examples are an unsupplied declaration, an overridable member and
  an inferred type that cannot be typed soundly. Never use `ω`, `ε₀` or a convenient finite
  answer as an unknown-value fallback.
- Follow the working loop in v1.md §1:
  - Derive the expected count independently of the analyzer.
  - Add a regression that contains the relevant source and scope.
  - Implement the general rule, never a special case for one signature.
- Every changed count in a report run must be explained. A pinned expectation that changes needs
  its derivation.

## Build and gates

The build uses sbt 2 (`project/build.properties`) and Scala 3.9.0 (`build.sbt`). `core` is the
calculator, a pure library tested with specs2. `plugin` is the sbt plugin, tested with scripted.

```bash
sbt core/testFull                    # core test suite, uncached
sbt plugin/scripted                  # plugin end-to-end tests
sbt scalafmtAll scalafixAll          # formatting and semantic rules (CI checks both)
sbt coverageAll                      # coverage report; CI gates on a floor
bash scripts/check-file-metrics.sh   # per-file length and comment-ratio gate
```

CPD (PMD, duplicates of 25 or more tokens) also gates in CI. sbt 2 keeps a machine-wide task
cache, so a re-run can report "no tests to run"; see the README's note on cold runs.
