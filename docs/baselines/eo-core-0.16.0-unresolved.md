# eo-core definitions: what the estimate can and cannot bound

The [baseline report](eo-core-0.16.0.txt) has 134 definition rows. As of that run they read:

```
13 unresolved · 22 instantiation-dependent · 4 unbounded by an open abstraction ·
 3 with more than one value · 53 with one value · 5 with no values · 34 abstract
```

This ledger is the other half of the baseline: what the rows that carry no number are waiting
for, which capability moved the ones that moved, and what is still missing. It is a coverage
ledger, not a work breakdown — reasons overlap, so the counts summarise rather than add up.

## What the branch's counting work moved

The first baseline (the same sources, before the estimate learned to substitute, to read the
source set as one scope, to reduce match types, to classify an open abstraction, or to model the
lattice's ends) read `40 unresolved · 2 with more than one value · 53 with one value · 5 with no
values · 34 abstract`. Of its 40 rows, 31 moved:

| capability | what it does | rows it moved |
|---|---|---|
| **substitution** | `C[args]` reads the definition's equation with its parameters bound to what the arguments are worth — `Pair[Boolean]` is 4, not an unknown constructor | `Iso`, and every template whose numbers changed shape |
| **one scope per source set** | the report reads all supplied sources as one set: a sibling file's type resolves, and a sealed hierarchy spanning files sums | `ModifyF` (its `Fst`/`Snd` live in `Affine.scala`) |
| **match-type reduction** | `Fst[(Boolean, Boolean)]` reduces on the argument's syntax; a match type over a free parameter stays inert and keeps its reading | `Affine.Hit`, `Affine.Miss`, `ModifyF`, `Fst`, `Snd` |
| **instantiation-dependent rows** | a row whose reasons are only the parameters it declares says so instead of listing names: `depends on its instantiation (A, S, T, B)` | 22 rows, the optics templates and their `X` aliases |
| **open abstractions** | an unsealed trait has no bound at all — any subtype anywhere adds values — so the row says `open to implementations (CanModifyP)` | `CanModify`, `CanModifyA`, `CanModifyF`, `CanPut` |
| **higher-kinded parameters** | `F[_]`/`F[_, _]` name their arity, the way the method side phrases the same gap | `ForgetFold`, `Unfold`, `TraverseTraversal.X` |
| **`Null` and `Any`** | `Null` is the one value `null`; `Any` is the top of the lattice, which sits at the ε₀ tier — so an `Array[Any]` is the countable space its length makes it | `Slice` became a number (`ω`), `AssocSndZ` narrowed to `Xo` alone |

## What is still missing, and what would move it

| gap | rows | what it needs |
|---|---|---|
| **type lambdas** | `Forget[F]`, `MultiFocus[F]` (2) | β-application of `[X, A] =>> ForgetK[F, X, A]`, and with it the higher-kinded reading of `F` and the in-package transparency of `ForgetK` (an opaque type is transparent where it is defined) |
| **an inert match type over a free parameter** | `Affine.Hit`, `Affine.Miss`, `ModifyF` (3) | the parametric reading: `Fst[A]` is a number once `A` is a tuple *and* an inert match type when it is not, so the row is a function of the instantiation with a match type in it — this is where a symbolic count would pay off |
| **a refinement** | `ComposedTraversal` (1) | reading `Optic[...] { type X = Xo }` as the base type plus a member binding; the base type is an unsealed trait here, so the row would then read as open |
| **a path-dependent member type** | `ComposedTraversal.X` (1) | `type X = af.Z`, a member of another instance: member resolution across frames, not just by name |
| **a name outside the supplied sources** | `ForgetFold` (1, `Foldable` from cats) | dependency sources supplied to the same report, or a modelled vocabulary; the name is the honest answer until then |
| **opaque representations** | `Direct`, `ForgetK`, `MultiFocusK` (3) | nothing: `?` outside the defining package is the contract, and the ledger keeps them apart from the gaps |

The first baseline's other findings still stand as rules, not gaps: a concrete application, a
sibling file's type and a sealed hierarchy across files are all read now; the plugin fixtures
show a two-file Peano family end to end.

## The decision that remains

What should a *template's* stored-value estimate be? The report says
`depends on its instantiation (S, A)` and no number, which is honest for the 22 rows whose size
is a function of their parameters. Two further steps are possible:

1. **A symbolic count** (`|A|^|S|`, `Fst[A]` conditional on `A`): what would make two templates
   comparable, and what the three inert-match-type rows need. It is real arithmetic over the
   algebra, and the substitution machinery it builds on is now in place.
2. **A worst-case bound** (parameters as the top tier): cheap, uninformative, and wrong for
   instantiations like `Getter[Unit, Boolean]`. Not recommended.

## Where I would continue

1. **Type lambdas** with opaque transparency: two rows, and it is the last piece of the
   carrier-alias family.
2. **Refinements + member types** together: `ComposedTraversal`'s two rows share a cause (a
   refined `Optic` whose member is another instance's type).
3. **The parametric count** (the decision above) only if templates need to be compared — it is
   the largest piece of work left, and the rows it would move are already classified.
