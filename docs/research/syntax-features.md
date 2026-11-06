# Named tuples, literal types, context functions and compile-time evidence

Four Scala 3 features the method side reads, each with its rule, derivation and limits. Sources
are cited with the verification level the literature pass gave them; compiler fixtures were run
with Scala 3.9.0 in the design session (`m1`, `m3`, `m4`, `cf3`, `il`), the cross-check (`cf`,
`cf2`), the review rounds (`review-A5`, `a5-fix1`, `review-A5 r2`, `a5-gate-review`) and the
inline revision (a6 `b1`, `r`, `fb`, `spec`, and `inventory`, which reads the library's TASTy with
the compiler). Executable: `SyntaxFeaturesSpec`, `ContextBindersSpec`, `InlineMethodsSpec`,
`InlineScopesSpec`, `TransparentInlineSpec`.

## Named tuples are the products of their fields

**Rule.** `(x: A, y: B)` reads as the product `A × B`, and so does
`scala.NamedTuple.NamedTuple[("x", "y"), (A, B)]` through the stdlib model (D16). As an argument
of a rigid constructor it keys as `NamedTuple[TupleN[names], TupleN[types]]`, the names being
`String` literal types. A match type is stuck on one.

**Derivation.** SIP-58 and the reference page *Named Tuples* (\[4\], **inspected**): "`NamedTuple`
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

**Derivation.** The draft specification, ch. 3 (\[29\], **inspected**): "A literal type `lit`
denotes the single literal value `lit`"; SIP-23 (\[3\], **inspected**): literals in type position
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

**Derivation.** The reference, *Context Functions* (\[16\], **inspected**): context function types
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

## Inline methods count as methods unless their body uses a compile-time operation

**Rule** (owner decision D-inline, revising A5's refusal of every inline method; register D27). An
`inline def` is counted exactly like a `def` when its body — its parameter defaults, every
definition nested in it, and, transitively, every inline method it calls — uses no compile-time
operation:

- no `inline if` and no `inline match`;
- no member of `scala.compiletime` (`summonFrom`, `summonInline`, `summonAll`, `erasedValue`,
  `constValue`, `constValueOpt`, `constValueTuple`, `error`, `codeOf`, `requireConst`, the type
  operations of `compiletime.ops`, …), however the name is bound: imported, renamed, qualified,
  `_root_`-anchored or through an alias of the package;
- no quote, no splice, nothing of `scala.quoted`.

`transparent` and `inline` parameters do not disqualify a method by themselves. An `inline`
parameter is its argument expression substituted by name; with no side effects (D26) that is
its value. A2's rule X stays: a `transparent inline` result whose declared type callers may see
narrowed is `?` (`transparent inline result: the call site observes the expansion's type, …`).
Every declared result but a singleton type may be narrowed (next paragraph), so a transparent
method counts only when it declares one (`transparent inline def one: 1 = 1`).

**Rule X, read exactly.** The call site types a transparent expansion by what the body returns,
and callers tell two expansions with the same values apart by that type: an overload resolves
differently, a member becomes selectable. Scala 3.9.0 gives an expansion a subclass the body
builds (`new Sub(b)` beside a declared `Foo`), a case of a sealed family (`S2.type` beside a
declared `S`, fixture review-A6 h13: `h(T.s(true))` picks `h(S2.type)`, `h(T.sKept(true))` with
body `(S2: S)` picks `h(S)`), `None.type` beside `Option[Boolean]` (h11), an opaque alias's
representation outside its scope (`Direct[X, A]` read as `A`, h12; `ForgetK` likewise, h14), a
literal's type (`true` beside `Boolean`), and the singleton type of a stable path the body returns:
`transparent inline def id[A](x: A): A = x` is typed `v.type` at `id(v)`, while
`{ val y = x; y }` is typed `Int` (fixture a6-fix1 t1; a `Fin` parameter, an object's `val` alike).
So the values a body computes do not bound what callers observe unless the declared type is
already a singleton. A one-valued type such as `Unit` or a final class without fields is refused
too, because a stable path of that type may be returned; this reading does not enumerate stable
paths.

Every other inline method is refused with what failed, after the prefix `inline method:`:

- `uses inline match`, `uses inline if`, `uses scala.compiletime.summonFrom`, `uses a quote or a
  splice`;
- `calls inline method g, which …` for a callee that fails;
- `calls scala.Predef.valueOf, an inline method not supplied`, and a name the reading cannot
  resolve (`reads Lib, a declaration not supplied: ext.Lib (import of Lib)`);
- `selects f on b, which a call site may type as a subclass of Base that overrides f inline, and
  inline method Sub.f uses …` for a member an expansion may re-select (below) where a supplied
  subclass overrides it (the closed world, register D39) and an inline method of its name fails.

An abstract inline declaration counts as an abstract `def` does, and an implementation of one is
read through its own body (it was refused as having no body to check).

A definition nested in a passing inline method is read like any nested definition. One nested in a
failing method keeps the refusal `definition inside an inline method: f uses …, and every
expansion copies this definition`. A constructor's value space is the same in every copy.
Type-revealing evidence (`ClassTag`, `Manifest`, `TypeTest`, `Typeable`, every `Mirror`) stays
excluded, and so does `scala.quoted`, as A5 left them (next section).

**Resolution.** The check resolves every name the body calls, as Scala binds it (D11): locals, the
method's and enclosing parameters, a template's own and inherited members, imports, other files
of the package, then the root imports. A call must reach a supplied method that is not inline, a
supplied inline method that passes, or a member of the library table. The table lists the
regular methods of `Any`, the scalar classes, `String`, `Array`, `Option`, `Either`, the function
classes, the tuple fields, `Some`, `None`, `Left`, `Right`, `NonFatal` and `math`. The Scala 3.9.0
compiler, reading the library's TASTy, finds no inline member in any of them; `Predef`'s inline
members (`assert`, `eq`, `ne`, `nn`, `runtimeChecked`, `summon`, `valueOf`) and `Tuple`'s inline
operations (`head`, `size`, `++`, …) are left out (fixture a6 `inventory`). Anything else refuses.
This covers an unsupplied parent or self type that may declare the name, an export, a wildcard
import the reading cannot list, an extension method, an import inside the body, and a `for`
expression.

**Derivation.** The Scala 3 reference, *Inline*, *Compile-time operations* and *Macros* (\[17\],
**inspected**), lists every way an expansion is resolved at its call site. An inline match "is
examined statically and … reduced accordingly". `erasedValue` and `constValue` read a type
argument. `summonFrom` and `summonInline` search the call site's givens. A quote or splice runs a
macro. A call of another inline method is expanded where the expansion is. Everything else in the
body is typed once, where the method is written, with one exception the fixtures found (below):
an inline method may override a non-inline one, and an expansion selects a member on the static
type its receiver has at the call site — the argument's class for a parameter, the caller's for
`this` — so a subclass's inline override is expanded there. Fixture a6 `b1` (Scala 3.9.0)
shows that an overload picked in the body is the same at every call site, and that a plain `if`
on a parameter is a function of its value. So a body free of those operations, selecting no
member an argument's class may override, is at every call site the same function of the values
its signature names as the method's own body is — up to the members the model excludes for every
method: universal equality, `getClass` and reflection, which boxing lets an expansion read
differently (fixture review-A6 h1: `x.getClass` of `boxed[Int](1)` is the primitive class, `x ==
x` of `selfEq[Double](NaN)` is false, where the `def` twins see a box). The parametric count of the
signature counts it, as it counts a `def`. A type test is refused even so: in a `def` a test on a
type variable is erased to a fixed class, in an expansion it reads the argument (below), so it
turns an admitted test into the type dispatch the model excludes.

The fixtures pin each way back to the call site:

- `il` (A5): `byVal(1)` is 10 and `byVal(a)` is 20 with `a == 1`; `pick[Int]` and `pick[String]`
  choose different arguments.
- a6 `r`: `viaMatch(y: Int)` passes its by-value parameter to an `inline x match`, and
  `viaMatch(1)` is 10 while `viaMatch(one)` is 20 with `one == 1`.
- a6 `r`: `wrapper` calls a `summonFrom` leaf and prints 1, then 7 under a call-site
  `given Int = 7`. An inline given named in the body is expanded too (`viaBad` prints 0, then 4).

Four more channels reach the call site without any compile-time operation in the source, and
the check refuses each (review round 2 found the last forms of each; `InlineScopesSpec`):

- A member an expansion selects on a narrower class. Fixtures review-A6 h3, h7, h8 (Scala 3.9.0):
  `call(b: Base, x) = b.f(x)` given an `Over` whose `override inline def f` uses `summonFrom`
  prints 5, then 9 under a call-site `given Int = 9`, while its `def` twin prints 5 both times;
  the same through `this` (`twice = step(step(x))` in an open class), through an abstract member
  implemented inline in a final class, and through a function parameter whose argument's class
  has an inline `apply` (eo's `BijectionIso.modify` shape). Fixture a6-fix1 d1 maps which
  receivers are re-selected: a parameter (by value, by name or inline), `this`, a field or member
  result selected on one, a lambda's parameter when the lambda is applied in place; a local `val`
  (declared or inferred), an ascription `(b: Base)` and a transparent callee's result are not. So
  a member selected on a parameter of the expanded method, on `this` of a template a class may
  extend, or on a lambda's parameter is refused when a subclass may override it inline with an
  expansion that differs: a non-final, non-private, non-inline member of a class that is not final
  (an object, a given, an enum and a package's synthetic object are closed) which a supplied
  template overrides (the closed world, register D39), and every member of a function type or
  `Conversion` (the only library classes the table lists that are open) — each only while an
  inline method of the name fails itself in a supplied template that may extend the receiver's
  class (by the closed world's matching; for a function class, one whose ancestry names a function
  type or leaves a parent unread): an override that is not inline is dispatched as the `def` is, an
  inline one that passes is that function, and an extension method or an unrelated template's
  member is never selected as the override. A field selected that way is read as a value
  of a type the subclass may narrow, so a member selected on it is checked the same way. `C.this`
  names the template C around the body (fixture review-A6 r2 `o1`: `Outer.this.f(x)` inside a
  final inner class prints 5, then 9 — the call site's `SubOuter` overrides `f` inline). An
  argument clause a callee does not take applies its result's `apply`: `Obj.g(x)` with `def g:
  Fn` is `Obj.g.apply(x)`, and Fn's inline `apply` is expanded (fixture r2 `m1`, 5 then 9; the
  same through `Obj.m(0)(x)`, a local `def mk: Fn`, `Some(new Fn).get(x)`, `identity(new Fn)(x)`).
  So a callee's declared clauses, a library member's or Predef function's clause count, and a
  case class's or enum case's constructor clauses are matched against the written ones, and each
  clause left over is checked as an application of the result; a result the reading does not type
  refuses.
- A runtime class reified from a type variable: Scala substitutes the type arguments into an
  expansion.
  Fixture a6 `b1`: `x.isInstanceOf[A]` is false for `isA[String](1)`, and true in the
  corresponding method. A type test whose erased class depends on a type parameter or an abstract
  type refuses: `tests the type A at run time, which each expansion instantiates`. An array's
  class is its element type's (fixture review-A6 h5: `isArr[Int](Array("a"))` is false,
  `isArr[String]` true), so `Array[A]` refuses, and an alias is read where it is declared, its
  class's type parameters as variables (`type TA = Array[A]` in `class C[A]`). `classOf` reifies
  the same way (fixture review-A6 r2 `c2`: `intArr[Int]` is true and `intArr[String]` false for
  `isIntArr(classOf[Array[A]])`, where the method's erased array is never `Array[Int]`'s class),
  so its type argument is read like a tested type: `reifies with classOf the class of Array[A] …`.
- A given the body leaves to implicit search: the given chosen where the method is written may be
  an inline one, expanded at each call site. Fixture a6 `b1`: `viaGiven` prints "default", then
  "callsite"; fixtures review-A6 h4 and h7 show the same through an extension group's `using`
  clause ("plain", then "abcd"), a context function read as its result (0, then 4) and an
  eta-expanded method with a `using` clause (1, then 5). A call that leaves a contextual clause, a
  context bound or an extension group's clause to the compiler refuses, and so does a reference
  applied to fewer clauses than a contextual one (eta-expansion fills it), `summon` and
  `implicitly`. A local method or given is a callee like a supplied one (fixtures review-A6 r2
  `l4`, `l6`: a local `def read(using f: Flag)` called as `read`, or with a context bound, prints
  true, then false), and its parameter defaults are read as part of the body (`l5`: a default
  calling an inline `summonFrom` method prints 5, then 9). A value of a context function type is
  refused unless applied with an explicit
  `using` clause, and so is any context function type the body writes, a context function
  literal, a callee whose signature holds one, and a result that holds one: every value of such a
  type is applied by implicit search wherever it is read where no context function is expected,
  and the reading does not type the expected types of lambda parameters and pattern variables.
- An inserted extension or conversion. When no member of the selected name applies to the
  arguments, Scala tries an extension or a conversion of that name. Fixture a6 `fb`: `r.m(5)`
  takes an inline extension `m(Int)` beside the member `m(String)`, and prints 2, then 9 under a
  call-site given. An extension that restates the member never answers, since the member applies
  wherever it would: for `q.m(args)` with `q` of a final supplied class C, its own public member
  m among the alternatives the selection finds (others only add ways for a member to apply), of
  one plain value clause the call fills, every extension of the name the body may meet either
  takes a supplied receiver class that is no base class of C, or takes an invariant base class R
  of C — C itself or a direct parent — with each of the group's binders spelled once in the
  receiver, no binder, default or repeated parameter of its own, and one value clause whose
  parameter names, and whose types read with the binders bound to C's base-type arguments, are
  the member's (by resolved identity). A receiver type headed by a type parameter — the group's
  own, or one of a scope around it — is no class, whatever class shares its name: `extension
  [Other](x: Other)` applies to anything, so it keeps the reason (`binderExtension`; a 3.9.0 run
  shows its inline match reading the call's type). eo's `Getter.andThen`, `BijectionIso.andThen`
  and `Review.andThen` are 1 (`inner.get` / `inner.reverseGet` restate `Optic`'s inline
  extensions). A covariant receiver, swapped base arguments, a binder of the method's own, other
  parameter names or a private member keep the reason (`InlineMethodsSpec.restatedExtension`:
  `Swapped.twice`, `PolyGetter.again`, `Named.call`, `Hidden.call`, `Use.same`; a session
  fixture, Scala 3.9.0, not retained, shows a result the member misses is a type error, never
  the extension). A conversion is inserted wherever an expression's type does not conform (Scala
  3 reference, *Implicit Conversions*, classical here; not re-verified in this session). A
  conversion is read by its type, not its spelling: a given or implicit value of `scala.Conversion`
  (through an alias or a renaming import) or of a supplied class extending it, an implicit object
  extending it, an implicit method of one value clause, an implicit class; it is inline when it
  is an inline given or method or when the `apply` it runs is (fixture review-A6 h10: `given
  toTag: Conversion[Int, Tag] with inline def apply` prints "none", then "abcd"). A conversion or
  extension counts where Scala may find it from the body: in an enclosing scope or an inherited
  template, through an import of the body's scopes, or in an implicit scope — every member of an
  object holding a type or companion to one, of a class, trait or given instance, of a package
  declaring an opaque type — and one the body or a method around it declares, and what an export
  clause defines again in a scope found so (an export defines aliases, D11; fixture a6-fix1b
  `ex2`: an exported given and a given local to the body each expand RichR's inline `m`, 2, then
  9); never one local to another method, nor one `private[p]` to a package `p` the body is
  outside (Scala inserts only an accessible implicit). For an applied selection whose qualifier is
  a path with a declared type the reading resolves, the implicit scope is that type's own (Scala 3
  reference, *Changes in Implicit Resolution*): the companions of the classes its parts name and
  of their supplied base classes, a type parameter read as its upper bound, the objects those
  classes are members of, and the templates those objects extend; a qualifier it does not type, or
  a part it does not resolve (an abstract or opaque type, an alias with parameters, a singleton,
  projection or refinement), keeps every implicit scope above. So an applied selection refuses
  when such an inline or contextual extension of the name exists — the reason says the arguments
  are not typed, since the member may well apply. A conversion is typed where the body is written,
  as any call is, and the closed world (register D39) supplies every one: an applied selection a
  conversion may answer refuses only when such an inline conversion, or a supplied inline method of
  the name, fails itself (`… the implicit conversion c may answer, and inline method R.m uses …`).
  A body refuses when such an inline conversion fails itself (`may expand the inline conversion c,
  which uses …`; an instance whose inline `apply` the check does not place, `which this check does
  not place`), or when a channel outside the sources (an unsupplied wildcard import, parent or self
  type) is open at the method (`may expand an implicit conversion from
  outside the sources (…)`). The one exception is a body that returns one of the method's
  parameters whose declared type is the declared result, aliases visible there unfolded: nothing is
  inserted around it.

**Definitions inside the body.** A local method, given alias, object or given instance is read
under its own frame, as the index reads a supplied one: its type parameters are its own, its
clauses, defaults and declared types are read where it is written, and a call of it is checked
like a call of a supplied method. Inside an anonymous class, a local class or a local given
instance, `this` and the names the template inherits are that template's (fixture review-A6 r2
`a1`: `run(new Mid { def k = this.h })` expands Mid's inline `h`, 5 then 9, though the enclosing
object declares a plain `h`). An anonymous class and a given instance are closed; a local class a
local subclass may extend is open. A local type named like a type the reading resolves outside the
body refuses (`declares the local type Fn, which hides a type of that name …`): the reading would
take the outer one.

Related work: Stucki, Biboudis & Odersky 2018 and Stucki, Brachthäuser & Odersky 2021 (GPCE,
**metadata-verified**), background on macros that generate and analyse code.

**Limits.** The member table is an inventory of what the check needs, not of the library. Two
channels are assumed closed, as the view rule assumes them (D20): an extension or conversion
defined in the companion of a type outside the sources, and one a *named* import of a declaration
outside the sources brings. Either could be selected because a member does not apply, and neither
is seen. A given of the sources whose class is declared outside them is not read as a conversion
either: the class's parents are not supplied. A thrown expression is not read: with no side
effects (D26) what an expansion throws changes nothing counted. An inline method whose body applies
a member whose name an inline or contextual extension of the sources also declares is refused,
even where the member applies (eo's `Getter.andThen`,
`BijectionIso.andThen` and `Review.andThen` apply `get` and `reverseGet`, which `Optic`'s inline
extensions also declare). Lambda parameters without a type, pattern variables and results of
library methods outside a small result table are not typed, so a selection on them refuses. A
member of a sealed class selected on a parameter is refused although every subclass is supplied
(conservative: the subclasses' overrides are not read). A conversion's visibility is
over-approximated (an import is matched by its last segment; a body's own expressions, and a
qualifier the reading does not type, meet the scopes around the body and the implicit scope of the
types the body can reach, matched by name: D43), so some bodies refuse for a conversion Scala would
not consider; one no type the body reaches anchors is no candidate (eo's optics no longer meet
specs2's `FileName.ToFileName`, a test library's companion on the build's classpath). A transparent
result is refused unless its type is a singleton, even where no stable path of the type exists. A
local class, trait or enum is read through its companion and its own body only: a value of its
type has no members this reading resolves, so a selection on one refuses. A passing body goes to
the solver like its `def` twin (eo's `MendTearPrism.getOption` / `replace` 2, `PickMendPrism.replace`
16, `PickFold.andThen` 2 through D25's sum elimination), but a receiver holding a deferred inline
member keeps D24's refusal in an inline target, even one whose body never calls it (conservative:
`InlineScopesSpec.deferredMembers`).

## Type-revealing evidence and quoted code are refused

**Rule.** `ClassTag`, `Manifest`, `TypeTest`, `Typeable` and every `Mirror` (`Product`, `Sum`,
`Singleton`, `Of`, `ProductOf`, `SumOf`) are excluded from the stdlib model as type-revealing
evidence; `scala.quoted` stays excluded as compile-time reflection.

**Derivation.** `ClassTag[T]` "can serve as an extractor that matches only objects of type `T`"
(\[11\], **inspected**), a `TypeTest` makes an abstract-type test possible (\[25\], **inspected**), and
`Mirror.Sum.ordinal` discriminates cases (\[14\], **inspected**): each tells instantiations apart, so
a body holding one is not parametric in its argument. Related work: Washburn & Weirich 2005 (LICS,
**metadata-verified**: the abstract reports that run-time type analysis breaks standard
parametricity) and Neis, Dreyer & Rossberg 2009 (ICFP, **metadata-verified**: the abstract states
that "type abstraction and intensional type analysis are features seemingly at odds").

**Limits.** `ValueOf[T]` supplies a value and tests nothing, so it is not excluded.
