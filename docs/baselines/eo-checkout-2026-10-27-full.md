# cats-eo full-checkout run: processing the whole project

## Run and scope

- Date: 2026-10-27.
- Analyzer: the `feat/import-bindings` head — main after
  `a5fc91ada4c6600b6d0b64605a7092a532e90cc1` (PR #21's package-scope rules), plus the import
  bindings described below.
- cats-eo: tag `v1.0.0`, commit `570460441ab3fc1e118de3934a2aed2c206b64f1` — the release this
  build already depends on (`cats-eo`, `cats-eo-generics`, `cats-eo-schemes` 1.0.0).
- Runtime: sbt 2.1.0-RC1, Scala 3.9.0, scalameta 4.17.4.
- Input: 189 Git-tracked `*/src/main/*.scala` across thirteen modules — avro (23), chimney (3),
  circe (9), core (55), generics (5), iron (3), jsoniter (9), kyo (5), laws (62), schemes (2),
  schemes-laws (3), site (4), zio (6). Benchmarks, tests and demos are excluded; site examples
  and law libraries are included, as in the 2026-10-05 survey. Combining modules is one scope,
  not an emulation of each module's compiler classpath.
- Runner: `cardinality.EoCheckout` (test scope, opt-in — the full-project sibling of the pinned
  `cardinality.EoBaseline`). It writes `eo-checkout.txt` (the complete report),
  `eo-checkout-core.txt` (core alone), `eo-checkout-methods.tsv` (one row per signature: module,
  source, qualified name, kind, enclosing signature, signature, count, reasons) and
  `eo-checkout-blockers.tsv` (distinct signatures affected per blocker category, combined and
  core). Generated artifacts, not checked in; the command below regenerates them.

Every selected source was read and parsed: **zero errors**. The run checks that forward and
reversed input order produce identical results, that no two rows share an identity, and that the
production `Report.of` path renders the same report the assembled run writes.

## Results

| Scope | Sources | Definitions | Signatures | Finite | Countably infinite | Unresolved |
|---|---:|---:|---:|---:|---:|---:|
| Production source set | 189 | 435 | 1,639 | 46 | 38 | 1,555 |
| Core alone | 55 | 146 | 506 | 32 | 5 | 469 |

| Module | Signatures | Finite | Countable | Unresolved |
|---|---:|---:|---:|---:|
| avro | 393 | 7 | 3 | 383 |
| chimney | 29 | 0 | 0 | 29 |
| circe | 135 | 0 | 0 | 135 |
| core | 506 | 32 | 5 | 469 |
| generics | 40 | 1 | 0 | 39 |
| iron | 8 | 0 | 0 | 8 |
| jsoniter | 110 | 1 | 1 | 108 |
| kyo | 34 | 1 | 0 | 33 |
| laws | 277 | 0 | 0 | 277 |
| schemes | 16 | 0 | 0 | 16 |
| schemes-laws | 6 | 0 | 0 | 6 |
| site | 42 | 4 | 29 | 9 |
| zio | 43 | 0 | 0 | 43 |

The `site` module's 29 `ω` rows are the constructor parameter-space rule reading its
`case class Order(name: String)`-style examples: a `String` field gives the constructor the
article's `String ~ ω` value space. They are provisional analyzer output, not reviewed counts.

For context, the published eo-core 0.16.0 jar baseline retains 480 signatures with 32 finite, 4
countably infinite and 444 unresolved; the checkout's `core` module is 55 sources against the
jar's 53, so the inventories differ by input, not by analyzer behavior.

## What the package-scope rules moved

Two general rules landed with this run, each pinned by regressions in `MethodAnalysisSpec`:

1. **Package-private grants.** `accessible` now reads the qualifier of `private[p]`: a
   declaration granted to an enclosing package is visible to every scope inside it — sibling
   files and subpackages included — while an unqualified top-level `private`, `private[this]`,
   a class-scoped qualifier and `protected[p]` (whose subclass condition this model does not
   track) keep the enclosing-declaration rule. `protected[p]` and bare `private` therefore stay
   where adversarial regression `keep a file-private type from a sibling file` pins them.
2. **Ancestor-package candidates.** A file's `package example.sub` clause sits inside
   `example`, but the qualified lookup only ever tried the file's own full path — types from
   enclosing packages were invisible. Lookup now tries every package prefix of every enclosing
   frame, nearest first, the way nested package clauses would.

Against the same tree analyzed one merge-step earlier, on the combined 1,639-signature run:

| Blocker (distinct signatures) | Before | After |
|---|---:|---:|
| `unresolved type` | 1,105 | 1,101 |
| `abstract type or method-valued representation` | 496 | 611 |
| `inherited override identity not resolved` | 150 | 176 |
| reason–signature pairs total | 2,661 | 2,798 |

`unresolved type: AvroFocus` disappeared from all 71 rows that named it, and
`unresolved type: JsonFocus` from all 45: those rows now resolve the name and carry its own
representation reason instead. 145 rows changed their reason lists and **no row changed its
count** — the rules moved scope truth, not the headline numbers. The +26 override-identity rows
are newly *surfaced* obligations: the reference now reaches the declaration, where a previously
invisible name could not be matched at all. On the pinned 0.16.0 jar run, counts and inventory
are unchanged; two rows move between those buckets (179→178, 225→227).

## What the import bindings moved

The same tree, analyzed with import clauses bound by lookup (`feat/import-bindings`):
named and renamed selectors resolve through the import's own path — anchored at every
enclosing package prefix, `_root_` stripped — wildcards bind after declarations, named
bindings and enclosing packages have had their chance, and a bound name whose target lives
outside the supplied sources refuses the model's builtin vocabulary rather than answering for
it (`unresolved type: Option (imported from io.circe.Option)`). A clause binds from its own
position onward, so a reference written before it keeps the scope the compiler gives it.
An import the lookup cannot model — a term-prefixed clause, a `given` selector, any export —
still flags what it shadows, as before.

Transparent alias bodies reconstructed under substitution carry no position of their own: read
inside the caller's file they take the caller's position; read under another file's scope they
anchor at that scope's declaration point, so the clauses written before it bind their names.

Against the previous state, on the same combined 1,639-signature run:

| Measure | Result |
|---|---|
| Rows whose count changed | 0 of 1,639 |
| Rows whose reason list changed | 569 |
| `unresolved type` row–reason pairs | 2,196 → 2,082 |
| `abstract type or method-valued representation` | 732 → 827 |
| `opaque representation is not visible at use site` | 3 → 13 |
| Rows carrying an `… (imported from …)` trace | 477 |

The names the run had flagged as the gap resolved: `PSVec` 34 → 0, `PickFold` 8 → 0, `Getter`
5 → 0, `Review` 1 → 0 blind rows; `Direct` 34 → 24, `Affine` 33 → 18, `Plated` 11 → 4,
`ModifyF` 11 → 5. The remaining blind rows are the ones the resolution reaches through
reconstructed alias and match-type machinery outside the anchor above — a named follow-up,
not a re-scope. New `opaque at use site` rows are the contract working: an import that reaches
an `opaque` type says so at the use site instead of leaving the name unread.

On the pinned 0.16.0 jar run the five reviewed counts, the 480-row inventory, the per-row
method TSV and every stored-value row are unchanged; rows only gain the `… (imported from …)`
trace. `core/testFull`, scalafmt and scalafix checks, and `plugin/scripted` are green, with
seven new source-to-count regressions in `MethodAnalysisSpec` (named, relative, renamed,
wildcard, companion wildcard, builtin refusal, import order, and alias-position anchoring).

## What the sealed-family enumeration moved

`sealed` is the compiler's promise that a trait or abstract class has every direct subtype in
its own source file. `representation` now reads that promise as a tagged sum: an object
contributes one value, a case class or a final class the product of its declared parameters, a
sealed abstract intermediate is enumerated in turn, and a family enumerating no subtype is an
empty sum — which says 0, because nothing can inhabit it. `case object` leaves are read from
the module frames, where the name index keeps terms apart from types; subtype slots must bind
the parent's parameters by plain name or pin them bottom-up (covariant `Nothing`,
contravariant `Any`). The refusals keep the sum honest: a `given` instance reaching into the
family, an open abstract intermediate, a non-final plain class, a parameter the supertype
never pins, or a constructor or field invisible at the use site each leaves the whole parent
unreadable — a sum with one missing branch is a wrong sum, and a construction the reading
cannot write must not be counted as one it can.

The fragment reads families exactly as the already-reviewed enum path reads its own: sealed
and enum twins answer alike, including the reviewed `CanGet.get` zero when a case payload has
no producer. Six source-to-count regressions in `MethodAnalysisSpec` pin the sum, the mirror
binder and covariant pin, the empty enumeration, the open subtype, the open intermediate, and
the enum-twin agreement.

On the same combined run, against the imports step alone:

| Measure | Result |
|---|---|
| Rows whose count changed | 0 of 1,639 |
| Rows whose reason list changed | 230 |
| `abstract type or method-valued representation` | 827 → 595 |
| `unresolved type` | 2,082 → 2,146 |
| `inert match type` / `concrete substitution required` | 18 → 35 each |
| `type argument arity` | 5 → 19 |
| `sealed subtype constructor not visible at use site` | 0 → 34 |

No row changed its count: every family freed from the abstraction bucket carries a remaining
blocker on the same row — `Any` and the fixed-width boundary, a foreign name, a match type, or
the constructor gate above. What moved is the triage: 232 row–reason pairs now name what
really stands between the row and its number. The pinned 0.16.0 jar run keeps its five
reviewed counts, its 480-row inventory and its byte-identical method TSV; its report triage
moves the same way (abstract 225→195 as the jar's own PSVec family reads whole).

## The whole-project gaps these runs name

- **Foreign names and their members.** `Schema` (188), `Json` (126), `IndexedRecord` (111),
  `Quotes` (95), `Decoder`/`Encoder` (121) and the rest belong to dependencies whose sources
  were not supplied — the bounded-dependency-resolution plan's declaration-summary work, not a
  parsing failure. `qualified member environment not resolved` (153) is mostly the same
  dependency boundary seen through member selection.
- **The representation algebra.** `abstract type or method-valued representation` (595 after
  the sealed enumeration) is now what genuinely stays open: capability traits users implement
  (`CanGet` and kin — open by design, correctly unreadable), match types inside families,
  refined carriers, and higher-kinded evidence (`F[_, _]` 134, `F[_]` 81, polymorphic
  capability binders 105). V1.4's branching and closed-family items work this down.
- **Method-side ground types.** `String` (382), `Int` (231), `Any` (150) and `Byte` (133) are
  unresolved *in implementation-shape vocabulary by design*: the stored-value estimate reads
  their spaces, and the constructor parameter-space rule uses that reading (that is what the
  `site` `ω` rows above are), but V1.4 item 2 defers fixed-width reasoning on the method side.
  No number is being withheld there — none is yet justified.
- **Imports not yet in the model:** `export` clauses keep their blanket shadow flag, and
  `given` selectors bring values the model counts no implementations of (no implicit search).
  A small residue of blind names remains behind reconstructed alias/match-type scopes, tracked
  above.

## Reproducing

```bash
git clone --branch v1.0.0 https://github.com/Constructive-Programming/eo.git
sbt "core/Test/runMain cardinality.EoCheckout <checkout-directory> target/eo-checkout <revision>"
```

As always: provisional analyzer output, not independently validated answers. The five reviewed
model counts live with the [0.16.0 review ledger](eo-core-0.16.0-review.md); nothing in this run
adds to that reviewed set.
