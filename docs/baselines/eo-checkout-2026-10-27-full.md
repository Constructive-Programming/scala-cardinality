# cats-eo full-checkout run: processing the whole project

## Run and scope

- Date: 2026-10-27.
- Analyzer: the `feat/package-private-scope` head — main at
  `582561c286cf2392cc6152f033114e59383ac095` plus the package-scope rules described below.
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

## The whole-project gaps these runs name

- **Import bindings (v1 plan §6 V1.2).** `PSVec` (35 rows), `Direct` (35), `Plated` (12) and
  `ModifyF` (11) are defined in supplied sources yet still read as `unresolved type`: they are
  named through `import dev.constructive.eo.data.PSVec`-style bindings, not lexical scope. The
  import/precedence model is the next environment-resolution piece; until then these names stay
  `?` with the reason, as the contract requires.
- **Foreign names.** `Schema` (188), `Json` (126), `IndexedRecord` (111), `Quotes` (95),
  `Decoder`/`Encoder` (121) and the rest belong to dependencies whose sources were not supplied
  — the bounded-dependency-resolution plan's declaration-summary work, not a parsing failure.
- **Method-side ground types.** `String` (382), `Int` (231), `Any` (150) and `Byte` (133) are
  unresolved *in implementation-shape vocabulary by design*: the stored-value estimate reads
  their spaces, and the constructor parameter-space rule uses that reading (that is what the
  `site` `ω` rows above are), but V1.4 item 2 defers fixed-width reasoning on the method side.
  No number is being withheld there — none is yet justified.
- **Higher-kinded evidence.** `F[_, _]` (134) and `F[_]` (81) parameters plus polymorphic
  capability binders (105) remain the algebra's largest open fragments.

## Reproducing

```bash
git clone --branch v1.0.0 https://github.com/Constructive-Programming/eo.git
sbt "core/Test/runMain cardinality.EoCheckout <checkout-directory> target/eo-checkout <revision>"
```

As always: provisional analyzer output, not independently validated answers. The five reviewed
model counts live with the [0.16.0 review ledger](eo-core-0.16.0-review.md); nothing in this run
adds to that reviewed set.
