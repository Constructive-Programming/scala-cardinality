# Definition-scope opaque method representations

Opaque types must not be globally replaced with their runtime representation.
The method analyzer now unfolds a source-defined opaque alias only where the
use site has representation access. This is distinct from the stored-value
estimate, which already reads an opaque definition's own representation.

## What this unlocks in eo

The unchanged eo checkout at `5a2802214e7c3f0caf08b6a4852edaf3c62666d0`
declares `opaque type Direct[X, A] = A` in
`core/src/main/scala/dev/constructive/eo/data/Direct.scala`.

The following real rows now report `Finite(1)`:

| Source line | Signature | Independent construction argument |
|---:|---|---|
| 35 | `apply[X, A](a: A): Direct[X, A]` | The visible result representation is `A`; the only supplied `A` is `a`. |
| 38 | extension `value: A` on `d: Direct[X, A]` | The visible receiver representation is the supplied `A`. |
| 43 | `accessor.get[X, A](fa: Direct[X, A]): A` | One projection/identity on `fa`. |
| 48 | `reverseAccessor.reverseGet[X, A](a: A): Direct[X, A]` | One wrapper/identity on `a`. |
| 63 | `applicative.map[X, A, B](fa: Direct[X, A], f: A => B): Direct[X, B]` | Only `f(fa)` produces `B`; `A` and `B` are distinct binders. |
| 66 | `applicative.pure[X, A](a: A): Direct[X, A]` | One wrapper/identity on `a`. |

`X` is phantom, not an additional inhabitant. These arguments concern canonical
pure, total, parametric implementations, not arbitrary Scala bodies or the number
of stored values across all instantiations. They do not assume functor/optic laws.
The interfaces' overridden declarations are not independent supplied capabilities;
the concrete identity-carrier operations do not manufacture additional generic
values. Higher-kinded/evidence-bearing operations are not covered by that argument.

The real report's `Direct.fold.foldMap`, `Direct.traverse.traverse` and the two
composition methods remain unresolved for their other obligations. `ForgetK`
and `MultiFocusK` retain their higher-kinded limitations.

## Visibility and alias expansion

`OpaqueTypes` owns the scope check. The supported fragment distinguishes:

- An alias declared inside a template: its representation is visible in that
  template's lexical scope, including nested scopes.
- A top-level alias: representation access is confined to the same source/package
  top-level scope or its named companion scope. An unrelated same-file object or
  class does not gain access merely by sharing a file.
- Other files, including files in the same package: the representation stays opaque.
- Receiver-qualified opaque member projections remain conservative where receiver
  identity cannot be established.

The compiler-backed fixtures verify the named top-level companion and nested
companion cases against the project's Scala 3.8.4 compiler, as well as an unrelated
same-file object and outside-file consumers. See the Scala reference's
[opaque type details](https://docs.scala-lang.org/scala3/reference/other-new-features/opaques-details.html)
for the synthetic top-level scope and transparency rules.

`ResolutionContext` separates **lexical interpretation** from the **observing
use site**. Following a transparent alias into its declaration changes where its
names and binders resolve; it must not grant the caller that declaration's opaque
representation permissions. The observer survives alias chains, lambda application,
member equations, constructor fields, singleton widening and inherited declarations.

Consequently, a public `type Exported[A] = Hidden[A]` inside an opaque owner does
not expose `Hidden`'s representation to an outside consumer. Conversely, an
external transparent alias referring to that type can be read from a caller that
already has representation access.

## Inherited substitutions must retain failures

Resolving a parent type argument can fail because its opaque representation is
not visible. That failure is retained in the inherited binder substitution.
Dropping it would substitute an unrelated free parent binder and can fabricate
a zero count.

The compiled regression uses an opaque type with a public upper bound:

```scala
opaque type T[A] <: A = A
trait Parent[X] { val value: X }
abstract class Child[A] extends Parent[T[A]] {
  def get: A = value
}
```

This is valid Scala through the public bound. The analyzer does not yet model
that bound, so its answer is unresolved—not zero and not a guessed exact count.

## Binder shadowing

```scala
class Owner[Outer](outer: Outer) {
  opaque type T = Outer
  def get[Inner](a: T): Inner = ???
}
```

There is no `Inner` producer; returning `a` fails compilation. Removing the
method binder yields two choices (`a` or `outer`). Removing the constructor
capture as well yields one. These are compiler-backed regressions, not name-based
unification rules.
