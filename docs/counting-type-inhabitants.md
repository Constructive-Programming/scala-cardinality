# Counting type inhabitants — the reference this solver follows

Alex Knvl, *Counting type inhabitants* (24 November 2018):
<https://web.archive.org/web/20181221193229/https://alexknvl.com/posts/counting-type-inhabitants.html>.

`core/src/test/scala/ReferenceInhabitantsSpec.scala` is the executable form of the examples below.
It reads shapes directly, so a failure there is the solver's algebra, not the source frontend's
scope.

## The rules the solver implements

- Inhabitants are counted up to **observational equivalence**: renaming, beta/eta expansion and a
  redundant `case` analysis do not make two expressions different inhabitants. A refinement of that
  (exact equality in the model) is what the solver approximates with canonical constructions.
- `Void ~ 0`, `() ~ 1`, `Bool ~ 2`, `Int ~ 2^32` — the `Size` algebra's `NothingSize`, `UnitSize`,
  `BooleanSize` and the fixed-width sizes.
- A coproduct adds: `Either a b ~ |a| + |b|` — `Shape.Sum`, and `Option a ~ 1 + |a|` in particular.
- A product multiplies: `(a, b) ~ |a| * |b|` — `Shape.Product`; the empty product is `1` and an
  empty alternative is `0`.
- A function is an exponential: `a -> b ~ |b| ^ |a|`, with `1 -> a ~ a`, `0 -> a ~ 1` and
  `a -> 0 ~ 0` for inhabited `a` — `Shape.Function` together with absurd elimination.
- Universal quantification is the counting rule the frontend uses for a **free** type parameter:
  a `forall a.` variable is one atom, never "a type of unknown finite size". `∀a. a -> a ~ 1`,
  `∀a. (a, a) -> a ~ 2`, `∀a. (a, a) -> (a, a) ~ 4`, `∀a. (a, b) -> a ~ 1`.
- A polymorphic value's arguments are consumed as a *product*: `(a, b) -> c ~ a -> b -> c`, and
  `a -> (b, c) ~ (a -> b) * (a -> c)`, which is why the solver treats a product argument by its
  fields and a product result as a product of goals.
- A recursive type is a least fixed point: `∀a. (a -> a) -> a -> a ~ μx. 1 + x ~ ℵ₀` — a *countable*
  number of inhabitants, `Count.Countable` in the solver, and only when a starting inhabitant
  exists: `∀a. (a -> a) -> a ~ 0`, and `∀a. (a, a) -> a ~ 2`, not infinity.
- Negation: `x -> 0` has at most one inhabitant, `x > 0` iff `(x -> 0) -> 0 ~ 1`, and `Set a` is a
  predicate `a -> Bool`, so `|Set a| = 2^|a|` — the frontend's `Set` mapping.

## What the reference supports that the solver does not yet do

- Rank-N and higher-kinded quantification (`∀f. f A -> f B`, Yoneda for higher kinds): the frontend
  reports `?` for a bounded or higher-kinded parameter rather than guessing.
- Laws: "counting inhabitants of types given laws is hard" — the reference says so itself. A functor
  law, or an optic law, is an extra condition on inhabitant counts and is never assumed here.
- Non-regular types such as `Set` beyond the `2^|a|` identity, and types whose inhabitants depend on
  instance methods the model cannot see.

## Where the solver is deliberately narrower

- It counts **canonical pure, total parametric constructions**: no divergence, no effects, no casts,
  no unsafe equality.
- An opaque callable is unconstrained, so applying it repeatedly is counted as countable
  (`∀a. (a -> a) -> a -> a ~ ℵ₀`); a callable the source set defines is a specific inhabitant of its
  type, whose values a total parametric body can only draw from the environment — so it neither
  invents a choice nor blocks one.
- What the fragment cannot decide is `Unresolved` with a reason, never a finite guess and never an
  invented infinity.
