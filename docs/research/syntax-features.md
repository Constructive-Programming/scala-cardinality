# Named tuples, literal types, context functions and compile-time evidence

Four Scala 3 features the method side reads, each with its rule, derivation and limits. Sources
are cited with the verification level the literature pass gave them; compiler fixtures were run
with Scala 3.9.0 in the design session (`m1`, `m3`, `m4`, `cf3`, `il`), the cross-check (`cf`,
`cf2`) and the review rounds (`review-A5`, `a5-fix1`, `review-A5 r2`, `a5-gate-review`). Executable:
`SyntaxFeaturesSpec`, `ContextBindersSpec`.

## Named tuples are the products of their fields

**Rule.** `(x: A, y: B)` reads as the product `A × B`, and so does
`scala.NamedTuple.NamedTuple[("x", "y"), (A, B)]` through the stdlib model (D16). As an argument
of a rigid constructor it keys as `NamedTuple[TupleN[names], TupleN[types]]`, the names being
`String` literal types. A match type is stuck on one.

**Derivation.** SIP-58 and the reference page *Named Tuples* ([4], **inspected**): "`NamedTuple`
is an opaque type alias of its second, value parameter", with `toTuple` and positional and named
selection in its API; a regular tuple is a value of it ("a supertype … of its value parameter").
So the two types have one value space and every observation of a named tuple is one of its
fields: the product reading is exact. The names are part of the type ("the order of names in a
named tuple matters"): `F[(x: A, y: A)]`, `F[(A, A)]` and `F[(y: A, x: A)]` are three unrelated
types under a rigid `F`. A match type sees the opaque type, not the tuple: Scala 3.9.0 reduces
neither `case (a, b)` nor `case Int` on `(x: Int, y: String)` and proves neither disjoint
(fixtures `m1`, `m4`), so the reading leaves the match stuck, concrete arguments included: `match
type stuck on a named tuple: (x: Boolean, y: Int) (Scala reduces no case on it)`.

**Limits.** A `TypedParam` anywhere but a tuple is a dependent function's parameter and stays
`unsupported type: x: A`. A named-tuple pattern in a match type is not read. A union of a named
tuple and its tuple is refused (one runtime class). `NamedTuple.NamedTuple` spelled relative to
the root import stays refused, since a top-level package `NamedTuple` would outrank it (fixture:
it compiles to `Int`). The stored-value side's reading of that spelling is unchanged.

## Literal types have one value

**Rule.** A literal type (`1`, `-1`, `1L`, `0.5`, `'c'`, `"a"`, `true`) is a singleton with no
widened binding: a goal of it has one implementation, an input of it adds nothing. Its key, as a
rigid constructor's argument, is its class and constant. A union of literal types is the set of
its distinct constants; beside a class it is refused or read by runtime class, as unions are
(D13), the literal carrying its own class. A literal type is a closed space of one point and a
union of literals one of as many points as constants, so a function into or out of one is counted
like one over `Unit` or a sum of case objects (`def fnIn(g: 1 => Boolean): Boolean` = 4, `def
k1(g: (true | false) => Boolean): Boolean` = 16). A call whose type argument makes its result a
literal type is widened to the literal's class, as Scala widens an inferred result (`def call2(x:
Boolean) = idc[1](1)` with `def idc[T](t: T): T` is an `Int`, `2^64`).

**Derivation.** The draft specification, ch. 3 ([29], **inspected**): "A literal type `lit`
denotes the single literal value `lit`"; SIP-23 ([3], **inspected**): literals in type position
designate "the corresponding singleton type". `null` is not a value of one (Scala 3.9.0 rejects
`def nul: "a" = null`, fixture `m3`), so no null-free premise is needed. The value is a literal of
a transparent ground type (D9), which any body can write, so an input of it changes no count:
`def f(x: 3): Int` has the `2^32` functions of a one-point domain, never `2^(2^37)`. A union of
literals holds exactly their constants. Literal patterns (ground equality, D9) tell constants of
one class apart, and a class test those of different classes, which cooperative equality would
conflate: `case 1` matches `1L` and `case 97` matches `'a'` (fixture `a5-gate-review eq` prints
`(lit1,lit97)`). So `def u(x: 1 | 2): Boolean` = 4, and `lng(x: 1 | 1L)` and `chi(x: 'a' | 97)`
are 4 as well. Constants compare as Scala compares them: `1` and `1L` differ, a floating constant
is the value its spelling rounds to at its own width, compared by its bits as `equals` compares
it. A value of a closed space only yields closed values, each a literal the empty environment
builds, and a literal type's one value is such a literal: so its function space is the
codomain's raised to one, as for `Unit`. Scala types an inferred result by widening a singleton
standing alone or in a union (fixture `review-A5 r2 inf3`: `def t: 1 = call2(true)` finds an
`Int`; `opt[1](1)` keeps its `Option[1]`), and the instantiated shape shows where a type argument
put one.

**Limits.** scalameta reads `-0.0` as `0.0`, and the two are different types, so a floating zero
is refused (`literal type of a floating zero not read (its sign is not kept): 0.0`). `1 | Int` is
`Int`, which the union reading refuses (two alternatives share runtime class `java.lang.Integer`).
A call instantiated with a value's singleton, or with a union of literals, is widened to a type
this reading does not compute (`inferred result type not resolved: singleton result of a call of
idc`).

## Context functions are functions whose binders are givens

**Rule.** `T ?=> R` reads as `T => R`. The parameters of the chain of context functions a
definition's result starts with are contextual candidates nested inside it: between its
parameters and its body, each `?=>` of the chain one level deeper than the last, the binders of
one clause at one level (two of one type are ambiguous). The chain is followed where Scala follows
it — transparent aliases with their arguments, applied type lambdas (also curried applications
such as `Alias[F][X, A]`), a constructor an argument supplies, a type member as seen from the
receiver, the overridden declaration's result for a member without one. For the target they are
the arguments of its result, which an explicit literal names: an anonymous evidence they may
shadow is refused by the anonymous-evidence rule (D10, `anonymous evidence selection not resolved:
A (overlaps context function binder A)`). For an enclosing definition they are anonymous evidence
of everything nested in its body, reached only through implicit search, so the same rule refuses
one that a deeper binder, a nested `using`, a local given or a nested definition's own binder may
shadow, and two of one clause are ambiguous. A candidate — evidence, binder or named `using` —
whose type is a context function `A ?=> R` also answers searches for `R`, and a search for
`A ?=> R` finds a plain `R`: the rule compares every type a search may select a candidate for,
so `(using A ?=> B)(using B)` refuses both. An expression without an expected type that yields a
context function — a call, an ascription — is applied where it is written, so it is no inferred
result.

**Derivation.** The reference, *Context Functions* ([16], **inspected**): context function types
"are shorthands for class types that define `apply` methods with context parameters", "erase to
normal function types", and a literal's parameters "are available as givens in E". No source makes
`?=>` ≅ `=>` a theorem; the reading rests on those statements. A body applies a value of
`T ?=> R` to a `T` it passes explicitly or leaves to implicit search, which selects one of the
values it could pass anyway; it builds one as a literal `(t: T) ?=> e` whose binder it may name.
So inputs and goals are those of `T => R`. What differs is scope: an expression whose expected
type is a context function is wrapped in the literal, whose binders are synthetic
(`contextual$n`): nothing names them, implicit search alone reaches them, and it prefers the
innermost candidate. For a method's result the wrapper encloses the whole body — a block, a
helper call and a local definition included — so the binders hide outer evidence of their types
from every search the body makes (fixture `cf`: `g(using 1)(using 2)` prints 2; `cf2`, `cf3`
`local`, `viaAlias`, `C.g` likewise), and a deeper candidate hides them in turn (fixtures a5-fix1
`r1`: `two[Int](using 1)(using 2)` prints the inner 2; a nested `using A`, a body's `given A` and
a nested `A ?=> A` result each win over the enclosing binder; `(A, A) ?=> A` makes `summon[A]` an
E172 ambiguity). Implicit search applies a context-function candidate to the givens it finds:
`a2[A, B](using A ?=> B)(using B)(using A)` makes `summon[B]`, `summon[A ?=> B]` and a local
`using B` ambiguous between the two (fixtures `review-A5 r2` `cf4`, `cf6`, `amb2`, `amb4`), and
`b1[A, B](using A)(using B): (A ?=> B) ?=> B` selects the binder for `summon[B]` (`amb3` prints
`binder3`). A wrapper around part of a body hides nothing the body cannot reach otherwise:
`val o = summon[T]` before it is the outer value (fixture `cf3` `hoisted` prints 1), so nested
context functions are plain functions. Scala wraps exactly where the expected type, dealiased, is
a context function class: an infix alias, an applied lambda, a match type that reduces to one, a
projection, a path, a member as seen from the receiver and a refinement declaring `apply` wrap;
a union, an intersection, a singleton and an abstract type do not (fixtures a5-fix1 `w1`–`w4`,
`r2`, `r4`, `r8`, review `o1`–`o3`). An expression of type `T ?=> R` with no expected type is
applied to a found given, so `def asc = (k: Boolean ?=> Boolean)` is a `Boolean` (review fixture
`i3`: `boolean asc()`, prints `false` with no given at the use site), as a call `f(a)` of a method
returning one is. Related work: Odersky et al. 2018, *Simplicitly* (PACMPL 2, POPL),
**metadata-verified**.

**Limits.** An overlapping binder or evidence is refused, never dropped: a concrete given computed
from it would still reach it. So `def g[A](using A): A ?=> A` (true 1), `two[A]: A ?=> A ?=> A`'s
local `g: A` (true 1) and `(A, A) ?=> A`'s local definitions read `?`, as do `a4[A, B](using A ?=>
B)(using B)(a: A): B` (true 1: with no given `A` the plain `B` is reachable), `a8[A, B](using
A)(using k: A ?=> B)(using B): B` (true 1) and `b1` (true 1). A chain the reading does not
follow is taken, for this comparison, to be context functions all the way down its shape. The
chain stops with its reason — and refuses the definitions nested in an enclosing method — at a match type, a
projection, a path, a refinement of a context function, an applied inherited alias, an alias or
lambda applied to the wrong number of arguments, `ContextFunctionN` spelled by name, a name not
supplied or not declared at all, a type parameter an `extends` clause substituted with a function
(its argument reaches the reading as a shape that has lost `?=>`), an application left over after
a match type or another form (`M[Int][Boolean]`: `context function behind an applied type not
read`) or applied to a curried type parameter (`G[Int][Boolean]`: `context function behind an
applied type parameter not read`), or a value or variable whose type is inferred. Context
functions are not keyed under rigid constructors.

## Compile-time and type-revealing evidence is refused

**Rule.** An `inline` method (macros and `transparent inline` included) is refused with `inline
method: each expansion may inspect its call site's argument types, constants and givens
(compile-time dispatch)`, and every method defined inside one (a local definition, a method of a
local class) with `definition inside an inline method: …`, except where the goal has one value
(D12). `ClassTag`, `Manifest`, `TypeTest`, `Typeable` and every `Mirror` (`Product`, `Sum`,
`Singleton`, `Of`, `ProductOf`, `SumOf`) are excluded from the stdlib model as type-revealing
evidence; `scala.quoted` stays excluded as compile-time reflection.

**Derivation.** The reference, *Inline*, *Compile-time operations* and *Macros* ([17],
**inspected**): "The scrutinee `x` is examined statically and the inline match is reduced
accordingly"; `erasedValue` "pretends to return a value of its type argument"; `summonFrom`
selects by the givens in scope; a quoted type pattern inspects a type. The expansion is typed at
each call site, so a body may read what only the call site knows. Fixture `il` (Scala 3.9.0):
`inline def byVal(x: Int): Int = inline x match { case 1 => 10; case _ => 20 }` gives 10 for
`byVal(1)` and 20 for `byVal(a)` with `a == 1`, so even an `Int` parameter is not read as a
value; `pick[Int]` and `pick[String]` choose different arguments; `summonInline[Endo[A]]` applies
a given the definition never names. The body is not one function of the values its signature
names, so the parametric count does not bound the implementations: the count is refused whatever
the parameters are, and a goal of one value keeps its 1 (every expansion yields that value). A
definition inside an inline method is copied into every expansion and typed there, so its
`summonFrom` reads the call site too (fixture a5-fix1 `r3`: `f[Int](1, 2)` prints 1, and 7 under
a call-site `given Int = 7`, through a local `g`). A constructor's value space is the same in
every copy.
`ClassTag[T]` "can serve as an extractor that matches only objects of type `T`" ([11],
**inspected**), a `TypeTest` makes an abstract-type test possible ([25], **inspected**), and
`Mirror.Sum.ordinal` discriminates cases ([14], **inspected**): each tells instantiations apart, so
a body holding one is not parametric in its argument. Related work: Washburn & Weirich 2005 (LICS,
**metadata-verified**: the abstract reports that run-time type analysis breaks standard
parametricity) and Neis, Dreyer & Rossberg 2009 (ICFP, **metadata-verified**: the abstract states
that "type abstraction and intensional type analysis are features seemingly at odds"); Stucki,
Brachthäuser & Odersky 2021 (GPCE, **metadata-verified**) as background for macros that analyse
code.

**Limits.** `ValueOf[T]` supplies a value and tests nothing, so it is not excluded. An inline
`given` is not a row yet. The refusal is conservative for an inline method whose body inspects
nothing; the signature alone cannot tell.
