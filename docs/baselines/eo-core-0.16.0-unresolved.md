# eo-core definitions: what the estimate cannot bound yet

The [baseline report](eo-core-0.16.0.txt) has 134 definition rows: 94 carry a size, and 40 are
`?`. This ledger is the other half of that baseline — for each unresolved row, what stopped it,
what would have to exist for the row to read differently, and the order the work is worth doing
in. Row names, kinds and lines are from the report; the shapes are from the sources jar whose
SHA-256 the report's header records.

**It is a coverage ledger, not a work breakdown**: reasons overlap (a row can be blocked by a
parameter *and* a match type), so the per-capability counts below summarise, they do not add up.

## What the 40 rows say

- **The dominant gap is not a missing rule but a missing answer class.** eo's optics are
  generic (1–8 type parameters) and constructed from *functions over* those parameters —
  `Getter[S, A](read: S => A)`, `Modify[S, T, A, B](modifyFn: (A => B) => S => T)`,
  `SplitCombineLens[S, T, A, B, XA](read: S => A, split: S => (XA, A), combine: (XA, B) => T)`.
  Their stored-value estimate is a *function of the instantiation*: a number exists for
  `Getter[Person, String]`, none for `Getter[S, A]`. 29 of the 40 rows are of this kind, and
  the estimate has no way to say so — it says `unresolved: S, A` instead.
- **One outright rule gap is not about generics at all**: an applied user-defined type is never
  resolved. `case class Pair[A](a: A, b: A); case class Use(p: Pair[Boolean])` reports
  `Use = ω`, `unresolved: Pair`, where 4 is the answer; `type Two = Pair[Boolean]` reads `ω`
  too. This is wrong for *any* project with parameterised types, not just eo.
- **The estimate's scope is one source file.** `Box` in `A.scala` and `Use(b: Box)` in
  `B.scala` reads `ω`, `unresolved: Box`, where 2 is the answer — even with every source
  supplied to the same report. The method side of the same report already resolves across
  sources; the two halves disagree about what "in scope" means.
- **Three rows are `?` by design**, not by a gap: `Direct`, `ForgetK` and `MultiFocusK` are
  opaque outside the package that defines them, which the report marks as no size rather than a
  guessed one.
- **Five rows are honest unboundedness**: a reference to an unsealed trait (a capability the
  caller implements — `CanModifyP`, `CanModifyAP`, `CanModifyFP`, `CanPutP`, and `Optic` inside
  a refinement) has no bound, because any subtype anywhere can add values. These need a class of
  answer ("unbounded by an open abstraction"), not a new counting rule.

## The 40 rows

`blockers` names the classes below; `note` is the shape the row could not read.

| row | kind | line | blockers | note |
|---|---|---|---|---|
| `CanModify[S, A]` | alias | CanModify.scala:23 | open | `= CanModifyP[S, S, A, A]`; `CanModifyP` is an unsealed trait with abstract members |
| `CanModifyA[S, A]` | alias | CanModifyA.scala:21 | open | `= CanModifyAP[S, A, S, A]` |
| `CanModifyF[S, A]` | alias | CanModifyF.scala:19 | open | `= CanModifyFP[S, A, S, A]` |
| `CanPut[T, A]` | alias | CanPut.scala:17 | open | `= CanPutP[T, A, T, A]` |
| `data.Affine.Hit[A, B]` | class | Affine.scala:91 | param, match | carries `Snd[A]` beside `B` |
| `data.Affine.Miss[A]` | class | Affine.scala:81 | param, match | carries `Fst[A]` |
| `data.Direct[X, A]` | opaque | Direct.scala:24 | opaque | transparent inside the package (`= A`), hidden outside |
| `data.Forget[F]` | alias | Forget.scala:34 | lambda, hk | `= [X, A] =>> ForgetK[F, X, A]` |
| `data.ForgetK[F, X, A]` | opaque | Forget.scala:31 | opaque, param | `= F[A]` inside the package |
| `data.Fst[T]` | alias | Affine.scala:13 | match | `T match { case (f, s) => f }`, unreduced when `T` is not a pair |
| `data.ModifyF[A, B]` | class | ModifyF.scala:28 | param, match, cross | `(Fst[A], Snd[A] => B)`; `Fst`/`Snd` live in Affine.scala |
| `data.MultiFocus[F]` | alias | MultiFocus.scala:49 | lambda, hk | `= [X, A] =>> MultiFocusK[F, X, A]` |
| `data.MultiFocusK[F, X, A]` | opaque | MultiFocus.scala:46 | opaque, param | `= (X, F[A])` inside the package |
| `data.MultiFocusK.AssocSndZ[Xo, Xi]` | class | MultiFocus.scala:496 | param, any, null | `(Xo, Array[Int] \| Null, Array[Any])` |
| `data.PSVec.Slice[B]` | class | PSVec.scala:221 | any | `Array[Any]` is the only stored element type |
| `data.PSVec.Single[B]` | class | PSVec.scala:199 | param | stores `B` |
| `data.Snd[T]` | alias | Affine.scala:17 | match | `T match { case (f, s) => s }` |
| `optics.BijectionIso[S, T, A, B]` | class | Iso.scala:49 | param | `(S => A, B => T)` |
| `optics.ComposedTraversal[S, T, A, B, C, D, Xo, Xi]` | class | Traversal.scala:329 | param, open, cross, refine | ctor takes `Optic[S, T, A, B, MultiFocus[PSVec]] { type X = Xo }` |
| `optics.ComposedTraversal.X` | alias | Traversal.scala:334 | select | `= af.Z`, a member type of another instance (`af`) |
| `optics.ForgetFold[S, F, A]` | class | Fold.scala:51 | param, hk, outside | `S => F[A]` with `using Foldable[F]`; `Foldable` is cats, outside the sources |
| `optics.GetReplaceLens[S, T, A, B]` | class | Lens.scala:100 | param | `(S => A, (S, B) => T)` |
| `optics.GetReplaceLens.X` | alias | Lens.scala:107 | param | `= (S, A)` |
| `optics.Getter[S, A]` | class | Getter.scala:25 | param | `S => A` |
| `optics.Iso[S, A]` | alias | Iso.scala:15 | applied, param | `= BijectionIso[S, S, A, A]`, an applied user type |
| `optics.MendTearPrism[S, T, A, B]` | class | Prism.scala:79 | param | `(S => Either[T, A], B => T)` |
| `optics.MendTearPrism.X` | alias | Prism.scala:87 | param | `= (T, A)` |
| `optics.Modify[S, T, A, B]` | class | Modify.scala:54 | param | `(A => B) => S => T` |
| `optics.Modify.X` | alias | Modify.scala:57 | param | `= (S, A)` |
| `optics.Optional[S, T, A, B]` | class | Optional.scala:73 | param | `(S => Either[T, A], (S, B) => T)` |
| `optics.Optional.X` | alias | Optional.scala:80 | param | `= (S, T)` |
| `optics.PickFold[S, A]` | class | AffineFold.scala:40 | param | `S => Option[A]` |
| `optics.PickMendPrism[S, A, B]` | class | Prism.scala:237 | param | `(S => Option[A], B => S)` |
| `optics.PickMendPrism.X` | alias | Prism.scala:245 | param | `= (S, B)` |
| `optics.Review[T, B]` | class | Review.scala:28 | param | `B => T` |
| `optics.SimpleLens[S, A, XA]` | class | Lens.scala:256 | param | `(S => A, S => (XA, A), (XA, A) => S)` |
| `optics.SplitCombineLens[S, T, A, B, XA]` | class | Lens.scala:199 | param | `(S => A, S => (XA, A), (XA, B) => T)` |
| `optics.SplitCombineLens.X` | alias | Lens.scala:207 | param | `= (S, XA, A)` |
| `optics.TraverseTraversal.X` | alias | Traversal.scala:342 | param | `= T[A]` |
| `optics.Unfold[T, B, F]` | class | Unfold.scala:41 | param, hk | `(F[B] => T, () => F[Unit])` |
## The missing capabilities

| capability | what it is | rows it touches | what it would give |
|---|---|---|---|
| **`param`** — instantiation-dependent definitions | a definition whose own type parameters appear in its stored types; the estimate needs an answer *class* for it (see below) | 29 in eo | "depends on its instantiation (S, A)" instead of `unresolved: S, A`; a number only when a caller instantiates it |
| **`applied`** — substitute arguments into an applied user type | `Pair[Boolean]` must evaluate `Pair`'s equation with `A := Boolean`; the scope has to hold parameterised equations and `typeIn` has to substitute | 1 in eo (`Iso`), all parametric code elsewhere | `Use(p: Pair[Boolean]) = 4`, `type Two = Pair[Boolean]` = 4 |
| **`cross`** — one scope for all supplied sources | the estimate is per source; types referenced across files are unresolved. The method side already indexes the whole input | 2 in eo (`ModifyF` → Affine.scala, `ComposedTraversal` → Optic.scala), most real projects' rows | `Use(b: Box)` where `Box` is in another file = 2 |
| **`match`** — reduce match types on a known scrutinee | `Fst[(Boolean, Boolean)]` reduces to `Boolean`; applied to a free parameter it stays inert (and is then `param`'s business) | 5 directly (`Fst`, `Snd`, `Affine.Hit`, `Affine.Miss`, `ModifyF`) | the aliases and their consumers become countable at concrete instantiations |
| **`hk`** — higher-kinded parameters | `F[B]` with `F[_]` free: unbounded, and a distinct reason from a missing name | 4 (`Forget`, `MultiFocus`, `ForgetFold`, `Unfold`) | a named reason and, with `param`, a class of answer |
| **`lambda`** — apply type lambdas | `[X, A] =>> ForgetK[F, X, A]` is a type *constructor*; it needs β-application (and `hk` to be bounded, here) | 2 (`Forget`, `MultiFocus`) | the two carrier aliases stop being "a type lambda" |
| **`select`** — path-dependent member types | `type X = af.Z` is a member of another *instance*; needs member resolution across frames, not just by name | 1 (`ComposedTraversal.X`) | a reason that names the owner, and eventually a size |
| **`any` / `null`** — the two unmodelled base types | `Null` has one value (`null`); `Any` is the top of the lattice, so it sits at or above the ε₀ tier — the algebra needs a decision, not just a case | 2 (`Slice`, `AssocSndZ`) | `Null = 1`; once `Any` has any tier above `Nothing`, `Array[Any] = ω` and `PSVec.Slice` becomes a number (`ω`) — the decision is which tier the top gets |
| **`open`** — open abstractions as a size class | an unsealed trait's reference is unbounded by construction; report it as unbounded-with-reason instead of unresolved | 5 | rows say *why* there is no number, and the leftovers become a work list again |
| **`outside`** — names outside the supplied sources | `Foldable` is cats: no source-level rule can bound it; the report naming it is the honest answer | 1 (`ForgetFold`) | nothing, until dependency sources are supplied too |
| **`refine`** — refinements | `Optic[...] { type X = Xo }`; the base type is an open trait here, so a refinement rule alone changes no number | 1 (`ComposedTraversal`) | defer: cost is high, and `open` already covers the row's reason |
| **`opaque`** — opaque representations | `?` by design: the representation is hidden outside its package | 3 | nothing — this is the contract, not a gap |

## The decision the ledger asks for

What should the *stored-value estimate of a generic definition* be? Three candidates:

1. **An explicit "depends on its instantiation" answer** (recommended first): keep the blank
   `?` out of it, name the parameters, and let the summary count how many rows are of this kind
   (the pre-rebase report had that line — `generic: N of the … depend on their own type
   parameters` — and it was dropped when the summary was rewritten). Cheap, honest, and it turns
   ~30 eo rows from "unresolved name" into "no number exists for a template".
2. **A symbolic count** (`|A|^|S|`): the parametric arithmetic the plan lists as open work. It
   is what would let a *template* be compared with another, and it needs substitution first.
3. **A worst-case bound** (free parameters as the top tier): every such row becomes ε₀ or
   "unbounded", which is uninformative and wrong for instantiations like `Getter[Unit, Boolean]`.

## The order I would work in

1. **`applied` + `cross` together**: one source-set-wide scope whose entries carry their type
   parameters, and a `typeIn` that substitutes arguments. This is the only item that fixes
   *wrong* answers (`Pair[Boolean]`, `Box` across files) rather than missing reasons, and the
   method side's index in `MethodAnalysis` is the shape to copy.
2. **`param` as an answer class** (decision 1 above), including the summary line. Low cost,
   and it is what makes the estimate's `?` mean "no number exists" instead of "not implemented".
3. **`open` as a size class**: same reporting change, applied to unsealed traits; it stops
   pretending that these rows are a backlog.
4. **`match`** reduction on a known scrutinee, then **`hk`** as a named reason; then
   **`any`/`null`** once the tier of `Any` is decided.
5. **`lambda`**, **`select`**, **`refine`**: last, and only if a case outside eo needs them.

The pending targets in `ArticleCardinalitySpec` and `ReportSpec` pin items 1 and 4 as failing
expectations, so each turns into a real assertion when the rule lands.
