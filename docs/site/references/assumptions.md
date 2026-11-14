# Assumptions and limits

Every number rests on two modelling assumptions. The calculator does not check that the
code obeys them; a count describes the pure, total implementations of a signature and the
values of a type.

## The code has no side effects

Treat a `var` as a `val`. Reassignment is not modelled, so a `var` holds the value it was
given: its initializer, or the argument passed for a `var` constructor parameter. A `var`
field adds exactly what a `val` field of the same type would, and a captured `var` is an
ordinary capture. Scala's own typing rules still apply: a `var` is not a stable path, so
`p.type` through one does not typecheck.

Ignore throwing. A `throw`, or a call that may throw, is neither a value nor an
implementation, so it never adds to a count. `def make[A]: A` has `0` implementations even
though `throw new Exception` would compile. Exceptions used as data are ordinary values.

Exclude every other effect the same way: I/O, logging, divergence, a `null` the type does
not declare, unsafe casts, reflection and unrestricted runtime type or equality tests.
The members of `Product` and `Equals` (`productElement`, `canEqual`, …) are reflection and
equality tests, so a trait extending `Product with Serializable` counts as one without
those parents.

Further idealizations:

- **Key equality is lawful.** A `Map` or `Set` is its abstract value, a finite partial
  function or subset. An `Iterator`, `IterableOnce`, `ByteBuffer` or `InputStream` is the
  sequence it yields.
- **Builtin names are not shadowed by code the run does not see.** An unsupplied wildcard
  or parent never rebinds `Int`, `String`, `Option` or another root-import name. A supplied
  declaration or import still shadows it. A JDK or standard-library wildcard binds by
  rooted name (`import java.util.*` makes `List` mean `java.util.List`).
- **A top type is read by its place in its own arrow.** `Any`, `AnyRef`, `Object`,
  `Matchable` and `AnyVal` in an input are one value; in an output they are $\omega$. Each
  function type and method is read on its own, innermost first.
- **A constructor is a method; a type is a value whose accessible state is observed.** A
  constructor's parameters are inputs, so a top in one is `1`. The type as a value holds
  what its fields may store, where a top is $\omega$.
- **`X*` is a possibly empty list.** $\sum_{n=0}^{\infty} X^n$. `def first[A](xs: A*): A`
  is `0`; `Any*` is $\omega$.

## The sources are the whole program

The supplied sources, with the supplied dependency declarations, are the whole program. No
subclass, override or implementation exists outside them.

- A concrete class that no supplied template extends is final, and so is a concrete member
  that no supplied template overrides. The calculator reads the sources as if they said
  `final` there.
- Abstract members stay free capabilities: a value of a trait may still be built by the
  method being counted, which implements its abstract members.
- Two templates share an instance only when one extends the other or a supplied template
  extends both.
- The closed world settles which templates exist, not which names resolve. An unresolved
  identity, an alias the calculator does not follow, or a scope it does not enumerate
  keeps its `?`.
- Templates are matched by simple name. A parent outside the sources may define anything,
  so a member only such a parent may override is refused. Two same-named templates in
  different packages share their subclasses, so a member one overrides stays open.

## Inline methods

An `inline def` counts like a `def` unless its body uses a compile-time operation: `inline
if`, `inline match`, anything from `scala.compiletime` (`summonFrom`, `summonInline`,
`erasedValue`, `constValue`, `codeOf`, …), quotes or splices. That includes an inline
method it calls. A `transparent` result is `?` unless its declared type is a singleton,
because callers see the type of the expansion.

An implicit conversion or extension is a candidate only where the body may find it: in
lexical scope, or as a member of an object in the implicit scope of a type the body can
reach. A conversion that is inline and whose own expansion uses a compile-time operation
makes the body `?`. So do a type test or `classOf[Array[A]]` that reifies a type parameter,
a given left to implicit search, and a member a supplied subclass overrides with an inline
method whose expansion fails.

## Opaque types

Outside its scope, an opaque type counts `1` on both sides: it reads as `Unit`, and as a
type argument it keeps its own nominal identity. Inside its scope the representation is
read as before. A count is never refused because a representation is hidden.

## Constructors and factories

A receiver's fields hold what its public constructors build: the largest space any one of
them builds, not the sum. A constructor counts where the measured declaration may call it.
A class with no callable constructor is counted by its public factories, the methods and
`val`s callable there whose result is a direct construction of it, read through at most two
calls. With no factory either, it stays `?` with `receiver has no public constructor`.

## Approximations

- **Finite capacity.** Counts up to 127 are exact. Above that the calculator keeps a
  binary width, so `2^32` is printed as a bound (`≤2^32`), and `Float`/`Double` are lossy
  stand-ins marked `(lossy)`.
- **Products and powers above the finite tier** are coarse: $2 \cdot \omega = \omega$ and
  $\omega \cdot \omega = \omega$.
- **Recursive widening.** A recursive component that keeps growing is widened to the tier
  it has grown into. Only a component that can reach itself is widened, so a chain of
  forward references settles exactly and an $\varepsilon_0$ payload is never demoted.
- **Lazy-type completion.** A lazy/coinductive type's finite estimate and infinite
  contribution are combined, then normalized to one $\varepsilon_0$ if present, otherwise
  one $\omega$. `LazyList[String]` is $\varepsilon_0$; `LazyList[Boolean]` is $\omega$.
  General addition is unchanged: `Either[LazyList[String], LazyList[String]]` is
  $2\varepsilon_0$.
- **`?` is a first-class answer.** A name outside the sources, an overridable member, or an
  inferred type that cannot be typed soundly gives `?` with a reason. The calculator never
  uses a finite guess, $\omega$ or $\varepsilon_0$ as an unknown-value fallback.

The full contract is [v1.md §2](https://github.com/Constructive-Programming/scala-cardinality/blob/main/docs/plans/v1.md)
and [AGENTS.md](https://github.com/Constructive-Programming/scala-cardinality/blob/main/AGENTS.md).
The evidence for each rule is in the
[research register](https://github.com/Constructive-Programming/scala-cardinality/blob/main/docs/research/code-cardinality-foundations.md).

