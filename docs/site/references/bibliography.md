# Bibliography

The sources behind the model: the arithmetic, the parametricity that constrains generic
code, the recursion and abstraction rules, and the definability discipline that says a
count is only meaningful once a language, a scope and an equivalence are fixed. How far
each source was verified — inspected, bibliography-verified, metadata-verified — is
recorded per claim in the [evidence register](https://github.com/Constructive-Programming/scala-cardinality/blob/main/docs/research/code-cardinality-foundations.md).

## Counting and inhabitation

- Alex Knvl, *Counting type inhabitants* (24 November 2018) — the arithmetic this
  calculator implements: sums, products, exponentials, quantification, recursion and the
  infinite tiers.
  ([archived](https://web.archive.org/web/20181221193229/https://alexknvl.com/posts/counting-type-inhabitants.html),
  [live mirror](https://wabbit.blog/posts/fp-counting-type-inhabitants.html))
- Gabriel Scherer & Didier Rémy, [Which simple types have a unique inhabitant?](https://doi.org/10.1145/2784731.2784757)
  (ICFP 2015) — uniqueness of inhabitation modulo βη equivalence with sums: the method
  side's "how many implementations" question, decided.
- Gabriel Scherer, [Deciding equivalence with sums and the empty type](https://doi.org/10.1145/3009837.3009901)
  (POPL 2017) — βη equivalence with sums and `Nothing` is decidable and coincides with
  contextual equivalence.
- Paweł Urzyczyn, [Inhabitation in typed lambda-calculi (a syntactic approach)](https://doi.org/10.1007/3-540-62688-3_47)
  (TLCA 1997) — inhabitation is undecidable for the second-order λ-calculus: why a
  polymorphic count above one needs an exact reduction, never a guess.
- José Espírito Santo, Ralph Matthes & Luís Pinto, [Coinductive proof search for polarized logic](https://doi.org/10.4230/LIPIcs.TYPES.2020.4)
  (TYPES 2020) — proof search shaped by polarity; the search the solver approximates.
- Jonas Duregård, Patrik Jansson & Meng Wang, [Feat: functional enumeration of algebraic types](https://doi.org/10.1145/2364506.2364515)
  (Haskell Symposium 2012) — enumerating the inhabitants a count reports.
- Chris Taylor, [The Algebra of Algebraic Data Types](https://web.archive.org/web/20140222124650/chris-taylor.github.io/blog/2013/02/10/the-algebra-of-algebraic-data-types)
  (2013, blog series) — the sum/product/exponential reading for a general audience.

## Parametricity

- John C. Reynolds, *Types, abstraction and parametric polymorphism*, Information
  Processing 83 (IFIP), North-Holland, pp. 513–523 — where the uniformity of generic code
  was first stated.
- John C. Reynolds, [Polymorphism is not set-theoretic](https://doi.org/10.1007/3-540-13346-1_7)
  (Semantics of Data Types, LNCS 173, 1984) — why a polymorphic type cannot be read as a
  set intersection.
- Andrew M. Pitts, [Polymorphism is set theoretic, constructively](https://doi.org/10.1007/3-540-18508-9_18)
  (TACS 1991, LNCS 526) — the constructive reading that restores it.
- Philip Wadler, [Theorems for free!](https://doi.org/10.1145/99370.99404) (FPCA 1989) —
  the free theorems behind `∀a. (a, a) => a` counting `2`.
- Robert Atkey, [Relational Parametricity for Higher Kinds](https://doi.org/10.4230/LIPIcs.CSL.2012.46)
  (CSL 2012) — parametricity at the kinds the higher-kinded rules refuse to guess.
- Dimitrios Vytiniotis & Stephanie Weirich, [Parametricity, Type Equality and Higher-order Polymorphism](https://doi.org/10.1017/S0956796810000079)
  (JFP 20(2), 2010) — why type equality is inhabited only by the identity.
- Paolo Pistone & Luca Tranchini, [The Yoneda reduction of polymorphic types](https://doi.org/10.4230/LIPIcs.CSL.2021.35)
  (CSL 2021) — Yoneda isomorphisms as a type-level rewriting that eliminates quantifiers.
- Bartosz Milewski, [Parametricity: Money for Nothing and Theorems for Free](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/)
  (2014, blog) — the accessible exposition.

## Recursion, types and abstraction

- M. B. Smyth & G. D. Plotkin, [The Category-Theoretic Solution of Recursive Domain Equations](https://homepages.inf.ed.ac.uk/gdp/publications/Category_Theoretic_Solution.pdf)
  (SIAM J. Comput. 11(4), 1982) — recursive equations solved as least and greatest fixed
  points; the μ/ν distinction the recursion rules follow.
- John C. Mitchell & Gordon D. Plotkin, [Abstract types have existential type](https://doi.org/10.1145/44501.45065)
  (TOPLAS 10(3), 1988) — an abstract type as a packed existential: the record reading of
  a capability.
- Luca Cardelli & Xavier Leroy, [Abstract types and the dot notation](https://xavierleroy.org/publi/abstract-types-dot-notation.pdf)
  (DEC SRC Research Report 56, 1990) — path-dependent types read as existential opening.
- William R. Cook, [On understanding data abstraction, revisited](https://doi.org/10.1145/1640089.1640133)
  (OOPSLA/Onward! 2009) — abstract data types and objects as two distinct forms of data
  abstraction.
- William R. Cook & Jens Palsberg, [A Denotational Semantics of Inheritance and its Correctness](http://web.cs.ucla.edu/~palsberg/paper/ic94.pdf)
  (DAIMI PB 284, 1989; Information and Computation, 1994) — self-reference as a fixed
  point: why an overridable member stays open.

## Definability and foundations

- Bertrand Russell, [On Some Difficulties in the Theory of Transfinite Numbers and Order
  Types](https://doi.org/10.1112/plms/s2-4.1.29) (1907, *Proceedings of the London
  Mathematical Society* s2-4, 29–53) — a foundational reference for the counting model:
  which collections a definition may legitimately form, the discipline the counting
  contract applies when it fixes a scope, a language and an equivalence. The
  [citation inventory](https://github.com/Constructive-Programming/scala-cardinality/blob/main/docs/research/russell-citation-inventory.md)
  records what was read and what remains an access gap.
- Luna & Taylor, [Cantor's Proof in the Full Definable Universe](https://doi.org/10.26686/ajl.v9i0.1818)
  (Australasian Journal of Logic 9, 2010) — Cantor's argument restricted to the definable
  universe; the way out is that definitional contexts restrict the scope of quantifiers.
- Fan, [Hobson's Conception of Definable Numbers](https://doi.org/10.1080/01445340.2020.1731784)
  (History and Philosophy of Logic 41(2), 2020) — language-relative definability in the
  Hobson–Richard tradition.
- [Russell's paradox](https://plato.stanford.edu/entries/russell-paradox/), Stanford
  Encyclopedia of Philosophy — the secondary account used where the 1907 full text is
  paywalled.

## Scala

- Martin Odersky et al., [Simplicitly: foundations and applications of implicit function types](https://doi.org/10.1145/3158130)
  (PACMPL 2, POPL 2018) — the contextual argument rules the model reads.
- Lionel Parreaux, Aleksander Boruch-Gruszecki & Paolo G. Giarrusso, [Towards improved GADT reasoning in Scala](https://doi.org/10.1145/3337932.3338813)
  (Scala 2019) — GADT constraints as equations over type variables.
- Fen Hu & Ondřej Lhoták, [Undecidability of D<: and its decidable fragments](https://doi.org/10.1145/3371077)
  (POPL 2020) — the subtyping the resolver walks, and where it becomes undecidable.
- David Blanvillain, Philipp Brachthäuser, Martin Kjaer & Martin Odersky, [Type-Level Programming with Match Types](https://doi.org/10.1145/3498698)
  (POPL 2022) — match types, which the model keeps stuck until instantiation.
- Scala Improvement Process SIP-58, named tuples (2024) — the product reading of named
  tuples.

## Practice

- Yaron Minsky, [Effective ML](https://blog.janestreet.com/effective-ml/) (Jane Street) —
  the origin of "make illegal states unrepresentable".
- Alexis King, [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
  (2019) — narrowing at the boundary as a design discipline.
- [The Hidden Powers of Total Program Cardinality](https://medium.com/@rodolfohansen/the-hidden-powers-of-total-program-cardinality-9351ba774d4d)
  and [part 2](https://medium.com/@rodolfohansen/the-hidden-powers-of-total-program-cardinality-2-of-2-ceb0a3af13dd)
  — the wider programme the calculator serves.
- [The type system is saving your code](https://medium.com/swlh/type-system-saving-55db2ca062ff)
  — the original blog post.
- [Keep your types small](https://slides.com/rodolfohansen/keep-your-types-small) — the
  Scala Love talk.

## Repository evidence

- [Evidence register](https://github.com/Constructive-Programming/scala-cardinality/blob/main/docs/research/code-cardinality-foundations.md)
  — one entry per decision, with its sources and how far each was verified.
- [Russell citation inventory](https://github.com/Constructive-Programming/scala-cardinality/blob/main/docs/research/russell-citation-inventory.md)
  — 376 database-reported citing works, with per-record verification levels.
- [v1 plan](https://github.com/Constructive-Programming/scala-cardinality/blob/main/docs/plans/v1.md)
  — the counting contract, the eo baseline and the remaining work.
- [Research notes](https://github.com/Constructive-Programming/scala-cardinality/tree/main/docs/research)
  — the per-rule notes behind the pages above.
- [Baselines](https://github.com/Constructive-Programming/scala-cardinality/tree/main/docs/baselines)
  — recorded report runs with their inputs and limits.
- Otmar Ertl, [New cardinality estimation algorithms for HyperLogLog sketches](https://oertl.github.io/hyperloglog-sketch-estimation-paper/paper/paper.pdf)
  (2017) — the measurement direction the plan holds as future exploration.
- [Quality gates](https://github.com/Constructive-Programming/scala-cardinality#quality-toolchain)
  — the formatter, linter, coverage, mutation and duplicate checks the build runs, the same
  toolchain eo uses.
