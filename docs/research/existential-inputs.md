# Scoped existential inputs

Method analysis can now open **unbounded wildcard inputs** as fresh rigid types.
This supports uniform parametric reasoning about an unknown witness without
turning it into a caller-instantiable universal quantifier.

## The supported fragment

```scala
case class Packed[A, R](value: A, consume: A => R)

def run[R](p: Packed[?, R]): R = ???
```

For this signature the analysis finds one canonical implementation:
open `p`'s hidden type and pass its own value to its own consumer. Real Scala can
express the opening through a generic helper:

```scala
def run[R](p: Packed[?, R]): R = {
  def opened[A](value: Packed[A, R]): R = value.consume(value.value)
  opened(p)
}
```

Two independently supplied packages admit two choices, not four:

```scala
def run[R](p: Packed[?, R], q: Packed[?, R]): R = ???
```

The valid choices use `p`'s consumer with `p`'s value, or `q`'s consumer with
`q`'s value. The hidden types are not assumed equal, so crossing the packages
is not permitted. Multiple fields within one package do share their witness.

The implementation distinguishes:

- **Template capture names**, describing wildcard positions in a declared type.
  Re-resolving a stable binding's annotation retains these names.
- **Opened rigid identities**, allocated from the input value's provenance.
  Independent values of the same alias still get independent witnesses. A
  validated singleton alias of the same input reuses its identity.

Opening preserves nested quantifier scopes and avoids free-atom name collisions.
Fresh allocation also reserves nested binder names. Atom rewriting alpha-renames
conflicting binders deterministically. The solver keys witnesses by logical
input scope and binder position rather than display spelling, so renaming cannot
give a singleton alias of the same package a new witness.
Tuple/product projection, enclosing captures, arrow-introduced inputs, available
atomic evidence, and repeated-value checks retain their existing rules.

## What opening does not grant

```scala
def manufacture[R](consume: Function1[?, R]): R = ???
```

The hidden input type is rigid. The implementation cannot select `Unit` and call
`consume(())`. Without a value of that particular hidden type, this signature has
no implementation in the supported fragment.

Likewise, a hidden `Box[?]` element cannot be returned as an independently
universally quantified `A`.

## Explicit remaining boundaries

1. **Existential result construction and repackaging.** Returning `Option[?]` or
   `Packed[?, R]` requires witness selection or repackaging. This is not the same
   operation as opening a supplied input; it remains unresolved. A result typed
   by a validated input singleton can still refer to that known input.
2. **Upper/lower bounds.** `? <: A` and `? >: A` retain their bounds in explicit
   unresolved diagnostics. They are never silently treated as unbounded
   captures. A future fragment can model justified read/coercion capabilities.
3. **Opaque callables returning fresh packages.** Each invocation can choose a
   witness. Their elimination needs scoped witness generation after application,
   not a single reusable witness assigned to the callable. It remains unresolved.
4. **Unreadable constructors and representations.** Capturing a wildcard does not
   supply a missing library's type model or turn an arbitrary trait into a product.
   General collection operations, higher-kinded instantiation and variance rules
   remain subject to the existing supported-fragment boundaries.

The tests cover positive uniform consumers, witness sharing/independence, aliases,
singleton provenance, captures, productive/unseeded cycles, bounds/result
diagnostics, and real compiled Scala opening examples.

## Resolver structure

`TypeSubstitution` owns binder-aware syntax replacement. `MethodProjections`
owns syntax-preserving match/transparent-alias normalization and its extractor.
`SingletonIntersections` owns one extractor for singleton references and the
recognized intersection spellings, keeping their distinct semantic operations.
`TypeApplications` centralizes constructor spelling, lambda recognition and
parameter-constraint classification.
