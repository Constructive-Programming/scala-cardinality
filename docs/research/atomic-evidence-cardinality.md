# Atomic equality evidence

The executable contract is `EvidenceTransportCardinalitySpec`. The frontend recognizes both
infix and prefix spellings of Scala's `=:=` and `<:<`, subject to its existing name-resolution
and environment-completeness checks.

## Derivation

In the pure, total model, a Scala equality witness is a proof and its application transports
the same value. It is not an arbitrary `A => B`. In particular, applying `A =:= A` repeatedly
cannot distinguish any new implementations. Witnesses themselves have one observable proof
value in this model: effects, reflection, identity tests, unsafe casts, and user-defined
counterfeit witnesses are excluded.

An **available** `A =:= B` restricts admissible instantiations to ones where those types agree.
Only within that environment, the counter canonicalizes their free-atom identities. It rewrites
product and arrow shapes using that equality, while retaining each term binding's provenance.
Thus `x: A`, `y: B`, `ev: A =:= B` provide two choices for `B`, not one and not an infinite
family. Multiple witnesses do not provide multiple copies of either input.

An available subtype proof (`A <:< B`, or a binder's bound) is not read: a context that holds
one is refused (`unsupported evidence relationship: only equality proofs between endpoints are
read`). A proof the goal must build (a result or a constructor field of type `A =:= B`) is
refused too (`unsupported evidence constraint: a proof as a goal`).

## Supported fragment and limits

- Endpoints must resolve to free atoms or rigid applications (`F[A]`, keyed nominally — see
  [higher-kinded binders](higher-kinded-binders.md)). Structural representation equality does not
  prove nominal Scala type equality: two case classes can have identical product shapes. Therefore
  structural, concrete, dependent, and unresolved endpoints of written evidence remain `?`.
- Equality is propagated through the supported shape algebra, including arrows. An ordinary
  opaque callable remains opaque; equality evidence does not make it an identity function.
- A proof under a sum or returned by a callable is not an available assumption. Relationships
  that cannot be established from the actual context remain `?`. The counter does not merge
  two type binders merely because their evidence type appears in the result.
- Without any evidence, distinct binders remain independent; `x: A` alone supplies no `B`.
  Scope discovery and accessibility still precede the evidence rules.

The fragment extends the repository's parametricity/provenance contract, not its stored-value
estimates. It does not claim to solve the eo relationships `S =:= F[A]` or
`Tuple.Union[T] <:< A`: resolving those endpoints and supporting their structural transports
requires separate proofs and frontend rules.
