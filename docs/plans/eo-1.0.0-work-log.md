# Work log: measuring every feature cats-eo 1.0.0 uses

Chronological log of the session that produced the work PR #25 merged, kept for the owner's review.
Each entry records what was done, what was decided and why, what was measured, and what was
rejected.

**What reached main.** PR #25 merged the work through Z1a: the session's integration commit
`7e3e306` is main `4eabd23` (`git diff 4eabd23 7e3e306` is empty). The session's two later commits
were not ported: `7a37804` (Z3: the documentation commit, with fixes to the reading of throws) and
`6c384ff` (Z2: a cleanup that also renamed rows and fixed lookups). The owner discounts throws
(D-pure: a `var` is read as a `val`, throws are ignored, throwables are data) and takes row names
from TASTy. The documentation of both commits is folded, where it still holds on main, into the
commit that added this log, which records the cats-eo ledger at main `36cd3cd` (section 6).
Sections 3 and 4 keep the session record as it was written; each passage there that presents those
two commits as landed is marked "not ported".

Paths such as `notes/…`, `audit/…`, `runs/…`, `probes/…`, `research/…`, `design/…`, `partial/…`,
`resume/…` and `worklog.d/…`, per-step folders such as `b5land/` or `z3/`, and the session files
named without a directory (`SYNTHESIS.md`, `FOLLOWUPS`, `gates.sh`) are in the session's scratch
workspace, which is not part of the repository; they stay so that each entry can be traced to the
record it was written from. The durable results are in the repository:

- the cats-eo ledger at main `36cd3cd`:
  [eo-checkout-2026-11-14-full.md](../baselines/eo-checkout-2026-11-14-full.md) (the session's own
  ledger, `eo-checkout-2026-11-09-full.md`, came with `7a37804` and was not merged);
- the evidence register: [code-cardinality-foundations.md](../research/code-cardinality-foundations.md);
- the sources behind it: the verification record [bibliography.md](../research/bibliography.md) and
  the site's [bibliography](../site/references/bibliography.md);
- the counting contract: [v1.md §2](v1.md#2-agreed-counting-contract).

## Summary for review

**What landed.** Main `613f655` plus 20 commits, the last `7e3e306` (= main `4eabd23`), merged by
PR #25. Gates there: 75 test suites, scalafmt, scalafix, file metrics, CPD 2 duplications, the eo
checkout.

**Headline on cats-eo `v1.0.0`** (189 sources, 435 stored definitions, sources only):

| Analyzer | Signatures | Finite | ω | Beyond countable | `?` | Stored `?` |
|---|---:|---:|---:|---:|---:|---:|
| main `613f655` | 1,639 | 46 | 38 | 1 | 1,554 | 95 |
| `7e3e306` (= main `4eabd23`) | 2,074 | 295 | 133 | 0 | 1,646 | 112 |
| main `36cd3cd` | 2,074 | 354 | 185 | 0 | 1,535 | 118 |

- The signature count grew because B5 (`08dcde1`) gave rows to definitions beyond `def`s: givens,
  member `val`s and `var`s, members of anonymous classes, definitions nested in blocks, lambdas, case
  clauses and comprehensions, constructors of enum cases, abstract classes and traits, and secondary
  constructors. It added 435 rows (127 numeric when it landed) and moved no existing row. Just
  before it (`ecc47dd`) the 1,639 original rows read 188 finite, 100 ω, 1,351 `?`.
- Stored `?` went from 95 to 117 through corrections (`c136109`, `7ab0a74`), then to 112 at
  `83f5e76` (6 rows gained a number, 1 lost one).
- The last row is main after the commits since `4eabd23`, which this work did not audit. Its ledger
  classifies the 1,535 `?` method rows as 1,217 by design (the supplied sources cannot determine the
  count, or the code is outside the model) and 318 analyzer gaps, and the 118 stored `?` rows as 112
  and 6.
- The eo-core 0.16.0 archive reads 644 signatures (151 finite, 20 ω, 473 `?`) at `7e3e306` and 644
  (212 finite, 45 ω, 387 `?`) at main `36cd3cd`, its five reviewed counts unchanged.

**Independent verification**, each derived from the contract and the eo source, never from the
analyzer:

- Full numeric audit at `b5045b8`: all 443 numeric rows (247 method rows, 152 finite and 95 ω; 196
  stored), 13 auditors, three verifiers per suspect. **0 wrong**; one contested row, `CanPlace.place`,
  where the analyzer's 1 was right.
- Delta audits at the 9 later landings that added numbers (`e039cd6` 9, `ad931e2` 27, `382191a` 1
  (audited at `fb5d936`), `cd2c845` 13, `b1eefb9` 4, `ecc47dd` 19, `08dcde1` 127, `83f5e76` 10,
  `7e3e306` 12): **222 rows, 219 correct, 3 wrong**, all at `ad931e2` (`Chimney.iso`,
  `Chimney.prism`, `ModifyF.map.map`). The chimney pair is refused from `382191a` on;
  `ModifyF.map.map` was refused from `83f5e76` to `7e3e306`, and main reads it 1 again under
  register D33, a reading this work did not audit. `280ab80` changed no count.
- Integrated review at `83f5e76`: a fresh random sample of 72 numeric rows (48 method, 24 stored),
  **72 of 72 correct**. Its lenses found ten root causes of wrong counts on reduced fixtures, none on
  an eo row; `280ab80` and `7e3e306` fixed all ten.
- The closing reviews of the two unported commits found three older lookups that print a wrong
  number on reduced fixtures, none on eo: a class reading its companion's type member (4 where the
  truth is 1), a self type's member (1 where the truth is 4) and a path's first name that an
  enclosing export defines (1 where the truth is 4, FOLLOWUPS 26). All three still print so on
  main; the ledger lists them among its known limits.

**Owner decisions** (2026-11-02, section 3):

- **No side effects** (D-pure, register D26): a `var` is read as a `val`; a throw is neither a value
  nor an implementation and never the reason for a `?`; throwables are ordinary data.
- **Inline methods** (D-inline, relaxed the same morning): an `inline def` counts like a `def` unless
  its body, transitively, uses a compile-time operation; a `transparent` result stays under rule X.
- Left with the owner, defaults kept: the two questions of FOLLOWUPS 20, and a per-version
  dependency name index for wildcard imports. Main's listed namespaces (register D33) since leave 7
  `?` method rows whose only by-design reason is such a wildcard (ledger, levers).

**Open items.**

- Known limits, re-derived on main: the ledger's "Known limits" section.
- FOLLOWUPS open at the end of the session (22 and 23 were closed by `6c384ff`, which was not
  ported): 6 (alias parents), 7 (trait `val` parameters), 9 (the checkout's runtime), 14 (b)
  (`Optic.to` / `Optic.from`), 19 (a parent's default body read with the subclass's members in
  scope), 20 (the owner questions above), 24 (a second standard-library spelling table), 25 (stored
  value rows refused where the true count is 1), 26 (a path head an enclosing export defines) and 27
  (a reason for a type imported through a local object). On main, the ledger's probes no longer show
  7 and 19 and still show 6, 14 (b) and 26; the rest were not re-checked.
- The retained `docs/baselines/eo-core-0.16.0.txt` keeps its header, which describes the `7f8bcff`
  run (sbt 2.0.9, Scala 3.8.4) that produced its 480-row body. Main's `EoBaseline` still writes that
  fixed toolchain line on every run (the run at `36cd3cd` says `sbt 2.0.9, Scala 3.8.4` while sbt
  2.1.0-RC1 and Scala 3.10.1 ran it).

Sections 3 and 4 were written at landing time; their Z3 and Z2 entries describe commits that were
not ported.

## 0. Setup and baseline (2026-10-31)

- Toolchain: sbt launcher 1.12.9 (boots sbt 2.1.0-RC1 from `project/build.properties`), JDK 21.
  Maven Central (`repo1.maven.org`) answered 429 to the launcher's parallel boot from this
  container, so every sbt run used the Google Cloud mirror of Central
  (`maven-central.storage-download.googleapis.com`) through `-Dsbt.repository.config` and
  `COURSIER_REPOSITORIES` — no build file was changed for this.
- Input: cats-eo tag `v1.0.0` (`570460441ab3fc1e118de3934a2aed2c206b64f1`), checked out as a
  separate worktree.
- Baseline at analyzer `613f655` (main after PR #23), reproduced exactly as the 2026-10-27
  ledger records it: 189 sources, 435 definitions, 1,639 signatures — 46 finite, 38 countable,
  1 beyond countable, 1,554 unresolved. `core/test`: 33 specifications green.
- A scratch `cardinality.Probe` runner (not committed) printed per-signature counts for ad-hoc
  snippets through plain `java`, so analysis agents could test hypotheses without sbt.

## 1. Feature analysis

Nine analyses ran in parallel, one per feature cluster, each producing a design memo with a
census, the current behaviour, a model decision with a derivation, a soundness boundary, worked
eo examples, regressions and an implementation plan:

| Cluster | Rows touched | Headline finding |
|---|---:|---|
| Ground scalars (`String` 415, `Int` 233, `Byte` 133, `Long`, `Char`) | 623 | Ground types are transparent closed enumerations: a ground input splits into identical branches, so `#(Γ, x:G ⊢ τ) = #(Γ ⊢ τ)^\|G\|` — computed symbolically, never enumerated. `Int => Int` is an exact finite tower, never ω; `String` gives 0, 1 or ω. Needs an exact symbolic natural-number `Count`. |
| Top types and unions (`Any` 175, 57 union rows) | 230 | Under the contract's exclusions `Any` is `∃X.X`, observationally terminal (1). Unions are tagged sums only over pairwise provably disjoint runtime classes. A singleton result is 1 whatever else is unresolved. |
| Higher-kinded and bounded parameters | 459 | Rigid type constructors are uninterpreted applied atoms whose arguments must be keyed *nominally* (structural keys confuse `F[Option[A]]` with `F[Either[Unit, A]]`). Context bounds are anonymous evidence. Found two existing bugs with anonymous `using` parameters. |
| Abstract type members, paths, refinements, match types | 238 | `Optic.X` and kin are path-keyed rigid atoms; refinements and inherited aliases are transparent; stuck tuple projections become canonical atoms. Landing this alone would unmask two existing wrong numbers, so it ships with two gates. |
| Receiver capabilities, override identity, polymorphic members | 635 | A value of a nominal type is observed only through its members (a record). Overridable concrete members of open classes are independent capabilities. Naive binder instantiation is neither sound nor complete; only Yoneda-exact reductions and 0/1 instantiations are accepted. Found three existing bugs (an under-count from silently dropped imported parents, an over-count from implemented inherited declarations, an under-count from skipped overridable siblings). |
| Environment completeness | 245 | Concrete code reachable from a signature (module members, vals, defs, givens, export forwarders) cannot change the count when every supplied input and the result type are readable — under the contract's assumption that helper bodies obey the model — so the blanket "qualified member environment" diagnostic is unnecessary. Mutation is excluded, so a `var` reads as the value it holds. A purely syntactic result-typing tier settles some inferred results. Found five more existing wrong numbers on probes (none on a numeric eo row). |
| Standard library and foreign names | 756 | Only 200 of the 1,554 `?` rows are blocked *only* by names outside the sources. A small FQN-keyed stdlib model, a sequence-elimination rule (0/1/ω/`?`) and an honest `declaration not supplied` reason; supplying dependency sources is not yet safe (five hazards). Found an existing unsoundness under wildcard imports. |
| Stored-value side and report hygiene | 435 defs | 52 of 248 numeric stored-value rows are wrong, each traced to a cause (a companion object shadows its trait: 36; reasons do not travel with references: 11; …). The methods TSV breaks on multi-line reasons. |
| Independent census | all | Found 8 concrete miscounts and 6 feature areas no cluster owned (inline/macro evidence, missing rows for givens and vals, named tuples and literal types, context functions, sealed visibility, value classes). |

## 2. Decisions taken before implementation

The memos disagreed in a few places; the orchestrator's binding decisions:

- **`Any` is terminal on the method side only.** The stored-value side keeps its documented
  `Any = ε₀` convention; the two views stay separate. Constructor slots on the method side are
  read through the method-side model, so `WalkRes(cur: Any, …)` no longer reports a
  beyond-countable implementation count.
- **Constructor rows keep the owner's PR #20 convention** (a position the estimate reads counts
  its value space). What changes is soundness: a reading resting on an unresolved name anywhere
  is `?`, and finite spaces become exact. `CNothing(n: Nothing) = 0` stays — that is the
  convention, not a bug, although the census argued otherwise.
- **ω-saturation (capabilities memo C11) is accepted only in a restricted form**: a reason-free
  productive cycle reachable from the root, an absurd goal that is unproductive even when
  reason-carrying rules are allowed, and no eager resolution error. The memo's "optimistically
  non-absurd" version was rejected as unsound: an unresolved producer might prove absurdity, and
  then the true count is 1.
- **An uninhabitable supplied capability** (`∀C. C`) makes the environment vacuous: count 1, by
  absurd elimination — a decision the capabilities memo asked the owner for; taken here and
  flagged for review.
- **No dependency-sources mode in this PR.** Supplying the seven Scala dependency jars changed
  381 reasons and one count — wrongly — and five hazards (modules leaking into the outside-
  producer scan, re-exports, inherited member types, `Schema.Type`-style selections, Java
  sources) remain. Absence stays an honest `?` with a better reason.
- **Concrete reachable code is irrelevant** when inputs and result are readable (environment
  memo), stated as an explicit model assumption; readability conditions (opaque types outside
  their scope, private constructors, hidden fields, foreign names) are exactly where concrete
  factories would matter, and those stay `?`.
- **`var` reads as the value it holds** (mutation is excluded by the contract); the
  `mutable capture` diagnostic is retired. Flagged for review: the plan listed mutable captures
  as backlog, not as excluded.
- **Anonymous `using` parameters are reachable only by implicit search.** Their synthetic `x$n`
  names compile today but are a compiler implementation detail, not language; two same-typed
  anonymous evidences are ambiguous for `summon` and so unusable.
- **Inferred result types: only a syntactic tier** (literals, ascriptions, `new C[T]`, explicitly
  applied constructors, calls to methods with declared results, an override's inherited declared
  type). Argument-type matching was prototyped and matched the compiler on two fixtures, but
  without compiler metadata it is inference, so it was not adopted.
- **Owner-reviewed expectations** (`CanGet.get = 0`, the five 0.16.0 counts, `EoCaseLedgerSpec`,
  `OpaqueTypeScopeSpec`, the reference examples, the plugin fixture) may change only with a
  derivation, and every such change is listed below for review.

## 3. Implementation lanes

Work ran in three git worktrees off the integration branch, each feature going through:
implementation with regressions → an adversarial soundness review (minimal snippets, compiled
with Scala 3.9.0 where validity mattered, true counts derived by hand) and an independent
code/gates review → fixes → a compare-and-swap landing that rebased onto whatever the other
lanes had landed, reran every gate and the full eo checkout, and explained every changed row.

- `report`: stored-value names (C1), stored-value soundness (C2), report hygiene (C3).
- `types`: existing miscounts (B0), higher-kinded and bounded parameters (B1), abstract type
  members (B2), capability records and override identity (B3), polymorphic members and
  callable sums (B4), rows for givens/vals/anonymous members (B5).
- `solver`: ground scalars and symbolic counts (A1), top types/terminal results/unions (A2),
  the stdlib model and sequences (A3), environment completeness (A4), named tuples, literal
  types, context functions and macro evidence (A5).

### Process events

- **Round-1 reviews were heavy, as intended.** The first feature of each lane drew 17 (A1), 25 (B0)
  and 29 (C1) review findings, of which 7, 12 and 14 were confirmed wrong counts — several of them
  regressions the feature itself introduced (e.g. B0's nearest-first import anchoring reached an
  unopened parent package of a dotted `package a.b` clause; C1's qualified reads ignored the
  defining file's explicit imports). None of these landed.
- **API usage limit (≈ 20:10 UTC).** All three fixers were cut off mid-round. The lane script
  would then have tried to land the *unfixed* commits; the landers were cut off too, so nothing
  reached the integration branch. The script was changed so that an interrupted or failed fixer
  or reviewer stops the lane *without* landing, and the fixers were resumed from their partial
  edits (backed up first). `gates.sh` now runs `core/testFull` (sbt 2's `test` is incremental and
  had skipped unchanged specs, which a quality reviewer caught) and resolves the baseline path
  before changing directory.

- **Second usage-limit stop (≈ 03:10 UTC reset).** Again no unreviewed work landed. On resume, a
  mistake in the replay list briefly restarted two implementers whose earlier prompts had differed;
  both runs were stopped within a minute, the worktrees were compared byte for byte with the
  backups (unchanged), and the lanes were relaunched with the correct list.
- **Literature cross-check (2026-11-01).** At the owner's request a literature search ran beside the
  lanes (section 5). Its adversarial cross-check found one wrong count already on the integration
  branch (exports read as imports, X1) and four planned rules that would have been unsound as
  worded (X2–X6). These were made binding on the owning features before they were implemented.

- **Third usage-limit stop (≈ 13:30 UTC, reset 15:10 UTC).** It hit A3's implementer, C2's
  round-2 soundness reviewer and B1's lander in the middle of a rebase. B1's lander had already
  resolved its conflict markers but not committed, so nothing reached the integration branch. The
  partial work was backed up (`partial/solver-A3-impl-run4-interrupted`,
  `partial/types-land-r1-interrupted`) and each lane resumed at exactly the interrupted step,
  confirmed from the run journals.

- **Fourth usage-limit stop (≈ 18:20 UTC, reset 20:20 UTC, resumed 21:47 UTC).** By then C2 had
  landed (`7ab0a74`). The stop hit A3's lander in the middle of its rebase onto `7ab0a74`, with 10
  files still holding conflict markers, plus B2's round-2 fixer and C3's round-1 fixer. Nothing
  half-done reached the integration branch. The rebase state and C3's uncommitted fixes were backed
  up (`partial/solver-A3-land-interrupted`, `partial/report-C3-fixr1-interrupted`); B2's worktree
  was clean. Each lane resumed at exactly the interrupted step, confirmed from the run journals.

- **Report lane finished (≈ 23:20 UTC).** By then A3 (`2cc8de2`), B2 (`ceccb6d`) and C3 (`b5045b8`)
  had landed, so all of C1–C3 were in. The headline was **152 finite · 95 ω · 1392 ?**, up from
  46 · 38 · 1 beyond · 1554 at the start. The freed capacity went to an **independent audit of every
  numeric eo row** at `b5045b8`: 247 method rows and 196 stored values, in 13 chunks.
  - Each suspected wrong count got three verifiers, each taking a different angle: the documented
    model, a fixture that reproduces the row, and a derivation done from scratch.
  - **Result: 0 of 443 rows wrong.** One row was contested (1 of 3 verifiers):
    `CanPlace.place` = 1. It stands under capabilities.md C6, because `transfer` is a definite
    target view.
  - Verification also exposed one latent defect that no eo row hits: at `b5045b8` an overridable
    concrete member of an open template is never read as an open capability. A fixture
    (`FL2.fromList`) gets 0 where the truth is "not 0". Lane B3 already fixes it, and its landing
    must carry the audit fixtures as regressions.
  - The audit also flagged that B3's run turns the audited 0 of `Optic.to` / `Optic.from` into `?`.
    That is to be reconciled at B3's landing (FOLLOWUPS 14).
  - Full report: `audit/b5045b8/REPORT.md`. Later landings need only their own delta audited.

- **Fifth usage-limit stop (≈ 00:40 UTC on 2026-11-02, reset 02:40, resumed 04:15).** It hit A4's
  round-2 fixer (worktree clean) and B3's round-1 fixer (uncommitted fixes backed up to
  `partial/types-B3-fixr1-interrupted`). Both lanes resumed at exactly the interrupted step,
  confirmed from the journals.

- **A4 landed (≈ 06:10 UTC on 2026-11-02).** `e039cd6`; headline **158 finite · 98 ω · 1383 ?**
  (from 152 · 95 · 1392). Methods `~197`: 9 rows `?` → number, none lost, 188 reason-only (mainly
  `qualified member environment not resolved` −153 rows and `mutable capture` −17). X1's method side is
  now in (an export ranks with own declarations); X5 is deferred to B3 with
  `EnvironmentCompletenessSpec.overridableMembers` pending until B3 lands. A5 implemented on top
  (`15d4e82`) and is in its first fix round; B3 passed two fix rounds and is landing.
  - Delta audit (FOLLOWUPS 14c): I snapshotted `e039cd6` (`research/snap/e039cd6`) and started
    an independent audit of the 9 new numbers (`audit/e039cd6/`), with the same three verifier
    angles as the b5045b8 audit. **Result: 9 of 9 correct, none suspected** (`audit/e039cd6/REPORT.md`).
    Every number came from a member that cannot be overridden (module, private or final-class code)
    or from an override taking its parent's declared result; each fixture re-prints the eo number.
    The latent open-member defect is still present at `e039cd6` as expected (B3 owns it); no eo row
    hits it.

- **B3 landed (≈ 07:05 UTC).** `ad931e2`; headline **183 finite · 98 ω · 1358 ?** (from 158 · 98 ·
  1383). Methods `~793`: 27 `?` → number, **2 number → `?`** (`Optic.to` / `Optic.from`, 0 → `?`: B3
  reads a possible view through the overridable `andThen`; sound but a lost correct number, reported
  as a precision regression), 764 reason-only. The b5045b8 audit's fixtures now all read right
  (OpenStep.place ω, FreeSib.place ?, FL2.fromList ?, Box.target 3, FBox.target 2); three of them
  are not yet pinned as specs (FOLLOWUPS 14, for the cleanup commit). B4 started implementing on
  top. Delta audit of the 27 new numbers: `audit/ad931e2/` (snapshot `research/snap/ad931e2`).

- **Owner directive (≈ 07:40 UTC): the code is assumed to have no side effects.** Vars are
  treated as vals and throwing is ignored. Actions:
  - Recorded as binding decision **D-pure** in `SYNTHESIS.md`. Every lane agent reads that file
    at start, so the running lanes picked it up without a restart.
  - Documented for users in a README section ("Assumption: the code has no side effects"), a new
    `AGENTS.md`, and a short list in v1.md §2. One docs commit, landed after A5 so A5's lander
    need not re-gate.
  - Launched a fourth lane task, **C4**, in the report worktree (run `wf_7aef3702-ea2`, args
    `resume/args-report-C4.json`, cachedImpl `[]`) to make the analyzer obey it. A scan at
    `ad931e2` found the analyzer still special-casing `var` in a few places: a mutable-member
    refusal in capability records, `var` constructor parameters left out of representations, and
    a refinement case. C4 also models `Throwable` as an ordinary open java.lang class with only
    its pure API, because 39 eo method rows and 4 stored rows refuse on it today. And it audits
    every reader of bodies so that a `throw` is never the reason for a `?`.

- **Sixth usage-limit stop (≈ 08:00 UTC, reset 09:10, resumed 09:20).** It hit all four runs at
  once: A5's lander (rebased `382191a`, worktree clean), B4's two round-1 reviewers, C4's
  implementer (still studying the code, worktree clean), and the B3 delta audit (4 verifier
  groups and the synthesizer). Nothing needed backing up. All four resumed with their exact
  args; the journals show the first live agents are the interrupted steps (`A5:land`,
  `B4:soundness-r1` / `B4:quality-r1`, `C4:implement`, `verify:00.*` / `verify:02.0`).

- **A5 landed (≈ 09:45 UTC), and with it the whole solver lane (A1–A5).** `382191a`; headline
  **183 · 98 · 1358 → 159 finite · 93 ω · 1387 ?**. One row went `?` → number
  (`ForgetfulFunctor.glass.map` 1, a named-tuple read) and **30 numbers went to `?`**.
  - **Why the 30 numbers were lost.** They are all `inline` methods, under A5's rule D22: every
    `inline` method whose goal has more than one value is refused.
  - **The rule's grounds.** An expansion is typed at each call site, so a body can dispatch on
    types, constants or givens: `inline match`, `erasedValue`, `summonFrom`. Scala 3.9.0 fixtures
    show it, e.g. `byVal(1)` gives 10 while `byVal(a)` with `a == 1` gives 20.
  - **What it costs.** The rule is sound under the contract, but conservative for a body that
    inspects nothing. That covers most of eo's inline methods, which are inline for
    performance. Among the lost numbers: `Getter/Review/BijectionIso.andThen` 1,
    `Direct.apply` 1, `PathParser.isIdentPart` 2^(2^16) (audited correct at `e039cd6`) and
    `SimpleLens.place` ω.
  - **Owner decision requested:** count an `inline def` like a `def` when it is not
    `transparent`, has no `inline` parameters, and its body uses no compile-time operation?
  - `Chimney.iso` / `prism` going 0 → `?` is a fix (macro-derived).
- **Docs commit landed (≈ 09:50 UTC).** `fb5d936` on top of A5: README "Assumption: the code has
  no side effects", new `AGENTS.md`, and the v1.md §2 list. Lander CAS `382191a` → `fb5d936`.
  `runs/fb5d936…` is a marked copy of A5's run (docs only).

- **Owner answered the inline question (≈ 10:00 UTC): adopt the three criteria.** Recorded as
  **D-inline** in `SYNTHESIS.md` (it revises A5's D22). An `inline def` counts like a `def` when it
  is not `transparent`, has no `inline` parameters, and its body (transitively) uses no
  compile-time operation. Feature **A6** was launched in the solver worktree (run
  `wf_2b74ed4a-8bf`, args `resume/args-solver-A6.json`, cachedImpl `[]`). It owns the docs
  for the rule (README, AGENTS.md, v1 §2). A5's delta audit: 1 of 1 correct (`glass.map` 1,
  `audit/fb5d936/REPORT.md`).

- **Owner relaxed the inline rule (≈ 10:10 UTC): only the third criterion.** An `inline def` counts
  like a `def` whenever its body (transitively) uses no compile-time operation. `transparent` and
  `inline` parameters no longer disqualify on their own: under D-pure an inline parameter is the
  argument substituted by name, which reads as the value. A2's rule X stays: a `transparent`
  result callers may see narrowed is still `?`. That is a typing fact about the call site, not
  dispatch. I stopped the first A6 run (`wf_2b74ed4a-8bf`; worktree still clean) and relaunched it
  with the relaxed brief: run **`wf_6d16220f-be1`**, args `resume/args-solver-A6.json`,
  cachedImpl `[]`. SYNTHESIS D-inline was rewritten to match.

- **B3 delta audit finished (≈ 10:20 UTC).** 27 rows checked. **24 were correct and 3 were
  confirmed wrong** (`audit/ad931e2/REPORT.md`).
  - `Chimney.iso` 0 (3/3) and `Chimney.prism` 0 (2/3): the true count is `?`. These inline
    builders expand chimney macros, and A5's refusal already fixed them. A6 must keep them refused.
  - `ModifyF.map.map` 1 (2/3): the true count is `?`. The method-side lookup ignores unsupplied
    wildcard imports. In Scala `import cats.*` outranks another file's package member, so it may
    bind `Fst`/`Snd`. The defect predates B3; B3's record reading only routes field types through
    it. `Optic.id` is suspected of the same class.
  - Refuted: `Getter.andThen` and `BijectionIso.andThen` (0/3). Their 1 is right, and A6 must
    restore it.
  - The wildcard defect has no owner yet. It becomes a solver follow-up, **A7**, after A6
    (FOLLOWUPS 16).

- **Seventh usage-limit stop (≈ 12:00 UTC, reset 14:10, resumed 14:58; the session's worker also
  restarted in between).** It hit A6's round-1 fixer, C4's round-2 fixer and B4's lander. I
  backed up the uncommitted fixer work to `partial/solver-A6-fixr1-interrupted` (2,594 diff
  lines) and `partial/report-C4-fixr2-interrupted` (645). B4's lander had rebased onto
  `fb5d936` as `cd2c845` with a clean worktree. I checked each args file against its launch in
  the session transcript (all identical), then resumed all three at the interrupted step.
  - State at the stop: A6 implemented (`6842bdd`, 25 rows `?` → their pre-A5 numbers, headline
    179 · 98 · 1362), with round-1 findings including overridable-member dispatch. C4 is through
    round 1 (`5ad2686`: 4 rows `?` → ω), round-2 findings being fixed. B4 has passed both fix
    rounds.

- **B4 landed (≈ 15:20 UTC)** as `cd2c845`. Headline: 172 finite · 93 ω · 1374 ?, up from
  159 · 93 · 1387.
  - 13 rows went `?` → number; none were lost. They include `MultiFocusFromList.fromArraySlice` 1,
    `AffineFold.select` 4, `PickFold.to` 2, several prism/iso `andThen` overloads (1 or 2),
    `Optional.FromFunctions.<init>` 2 and `ComposedGetOptionLaws.composedGetOption` 64.
  - B5 follows in the same lane.
- **C4 landed (≈ 16:00 UTC)** as `b1eefb9`. Headline: **172 finite · 97 ω · 1370 ?**.
  - 4 rows went `?` → ω: `ConfluentWire.strip`, and `WholeRecordBuilder.multiBranch` /
    `noNullBranch` / `pairMismatch`, which build `IllegalArgumentException`s now that throwables
    are data.
  - What D-pure now means in the analyzer:
    - `var` is read as `val`.
    - `throw` is never a value and never a reason for `?`.
    - A member whose default only throws reads exactly like its abstract twin.
    - A template that evaluates a throwing initializer or parent argument is never built.
    - `Throwable` and its subclasses are modelled with their pure API.
  - Register D26; docs in `docs/research/pure-code.md`.
  - Two owner questions came out of it (FOLLOWUPS 20):
    1. A fixed member implementing a declaration of an empty type, e.g. `PSVec.Empty.apply: Int =>
       Nothing = throw …`, is read as concrete code. So `PSVec.Empty.length` keeps 2^32, while the
       parent's record counts as vacuous.
    2. The "default-instance" reading: an open class with `def fail(msg: String): Nothing = throw …`
       reads as having no pure instance, so its other methods count 1.
  - Both are consistent with D-pure as written, and neither changes an eo number today.
- Delta audits started for both landings (`audit/cd2c845`: 13 rows; `audit/b1eefb9`: 4 rows).

- **C4 delta audit:** 4 of 4 correct (`audit/b1eefb9/REPORT.md`). Two side observations:
  - The two sides disagree on `scala.jdk.CollectionConverters.*`. The stored side refuses `Int`
    under it; the method side treats it as a listed namespace. This was added to A7's brief: one
    shared table of listed namespaces for both sides.
  - Unions with a throwable stay `?`. That is a documented limit in pure-code.md §4.

- **B4 delta audit:** 13 of 13 correct (`audit/cd2c845/REPORT.md`).
  - Examples: `select` / `selectReadOnly` = 4 (const None, const Some, filter p, filterNot p), and
    `composedGetOption` = 64 (6 realizable definedness cases, so 2^6 Boolean functions).
  - One conservative `?` outside the delta: `ComposedGetOptionLaws.getOptionInner` (true count 1).
    It comes from C6's re-entry refusal through an eta-expanded `flatMap` argument (FOLLOWUPS 21).
  - **Running audit total since b5045b8: 54 changed numeric rows audited, 51 correct, 3 confirmed
    wrong.** The 3 are `Chimney.iso` / `Chimney.prism`, already fixed by A5, and
    `ModifyF.map.map`, which A7 fixes.

- **A6 landed (≈ 18:30 UTC)** as `ecc47dd`. Headline: **188 finite · 100 ω · 1351 ?**, up from
  172 · 97 · 1370.
  - The rule: an `inline def` with no compile-time operation in its body (transitively) counts as
    a `def`.
  - 19 rows went `?` → number and none were lost:
    - 15 are the lane's rows. They return to the numbers they had before A5's blanket refusal,
      e.g. `PathParser.isIdentPart` 2^(2^16), `SimpleLens.place` ω, `Modify.modify` 1.
    - 4 are integration rows that now reach B4's sum elimination: `MendTearPrism.getOption` /
      `replace` 2, `PickMendPrism.replace` 16 and `PickFold.andThen` 2.
  - Five of A5's lost numbers stay `?`:
    - `Chimney.iso` / `prism`: chimney's macros are not supplied.
    - `Getter` / `BijectionIso` / `Review.andThen` (true count 1): Optic's inline extensions
      `get` / `reverseGet` share their names, so A6's extension-fallback guard refuses them.
      Recovering them needs argument typing.
  - A6 also refuses runtime-class reification on type variables, such as `isInstanceOf[A]` or
    `classOf` in an expansion. Scala reifies these per call site, as fixtures show. README /
    AGENTS.md / v1 §2 now state the rule.
  - Gates: 67/67 suites. CPD fell 3 → 2. plugin/scripted 3/3.
  - **A7** (wildcard imports) started in the solver worktree (run `wf_eeb7a346-c06`). The A6 delta
    audit is running (`audit/ecc47dd`, 19 rows).

- **A6 delta audit:** 19 of 19 correct (`audit/ecc47dd/REPORT.md`).
  - Each of the four integration rows was re-run with a plain-`def` twin or a control:
    `PickFold.andThen` 2, `MendTearPrism.getOption` 2 (4 with an extra `T => A` control),
    `MendTearPrism.replace` 2, `PickMendPrism.replace` 16 = 2^4 definedness branches.
  - The auditors expect A7 to leave these 19 rows unchanged. They make a ready no-change check
    for A7's landing.
  - **Running total since b5045b8: 73 changed numeric rows audited, 70 correct, 3 confirmed
    wrong.** Two of the 3 are fixed (Chimney); A7 fixes the third.

- **Eighth usage-limit stop (≈ 19:00 UTC, reset 19:50, resumed 20:16; the worker restarted
  again).** It hit B5's round-2 fixer (uncommitted work backed up to
  `partial/types-B5-fixr2-interrupted`: 1,585 diff lines, 5 untracked) and A7's round-1 fixer
  (worktree clean). The args matched the transcript, and both runs resumed at the interrupted step.
  - State at the stop: A7 implemented (`ac53f7d`). It turns `ModifyF.map.map` and `Optic.id` into
    `?` (audit F3) and adds 4 method / 6 stored numbers under `CollectionConverters`, now that both
    sides share one listed-namespace table. Headline 189 · 101 · 1349.
  - A7 raised one deviation: the qualified control `dev.r.optics.BijectionIso` is also `?`. The
    head `dev` is an identifier an unsupplied wildcard outranks, and scalac confirms it. Only
    `_root_.dev…` keeps its number.
  - B5 (new report rows) is in fix round 2.

- **B5 landed (≈ 21:00 UTC), and with it the whole types lane (B0–B5).** `08dcde1`. The report now
  has rows for every definition beyond `def`s: givens, member vals/vars, members of anonymous
  classes, definitions nested in blocks / lambdas / cases / comprehensions, and constructors of
  enum cases, abstract classes and traits with a clause, plus secondary constructors.
  - eo: **2074 signatures: 289 finite · 126 ω · 1659 ?**, up from 1639 · 188 · 100 · 1351. That is
    435 new rows, 127 of them numeric. No existing row moved.
  - EoBaseline 0.16.0: 480 → 644 rows, and the five reviewed counts pass.
  - Delta audit of the 127 new numeric rows started (`audit/08dcde1`, 22 chunks).
  - Integration branch: 17 commits. Remaining features: A7 (in fix round 1).

- **B5 delta audit:** **127 of 127 new numeric rows correct** (`audit/08dcde1/REPORT.md`). The rows
  cover constructors of enum cases, abstract and given classes (mostly empty argument tuples = 1),
  Unit-result local loops (1), `Any`-result anonymous members (1), member vals keeping their
  declared ground type (`HeaderLength` 2^32, `Magic` 2^8, `MaxDepth` 2^32: not final, so the
  literal widens), and `SchemaMismatch.<init>` 2^160. **Running total since b5045b8: 200 changed
  or new numeric rows audited, 197 correct, 3 confirmed wrong** (all three fixed or being fixed).

- **A7 landed (≈ 23:30 UTC), the last feature.** `83f5e76`. Headline: **2074 signatures: 289 finite
  · 127 ω · 1658 ?**. All 15 features plus the docs commit are in: **18 commits**.
  - Methods: 3 numbers → `?` (`ModifyF.map.map`, `Optic.id`, `ForgetFold.from`, the audit's F3
    class) and 4 `?` → number. The new numbers are the avro constructors under
    `CollectionConverters`: `WalkRes` ω, `DirectPlan` 2^32, `DirectSlot` 2^64, `SelfKind` 2^32.
  - Stored values: 6 `?` → number and 1 number → `?`. Both sides now share one listed-namespace
    table.
  - Gates: 70/70 suites, CPD 2, scripted 3/3. EoBaseline 0.16.0: five reviewed counts pass, and
    exactly 3 unreviewed rows go number → `?` (the 0.16.0 twins of the same rows).
- **Final phase started:**
  - The A7 delta audit (10 rows, methods and stored) is running.
  - The **integrated adversarial review** is running (run `wf_89062c02-90b`). It has eight lenses:
    1. cross-feature soundness;
    2. a fresh audit of 48 random numeric method rows and 24 stored rows;
    3. the stored side;
    4. the precision follow-ups (`Optic.to/from`, `getOptionInner`, the `andThen` extension guard);
    5. quality / CPD / missing regressions;
    6. docs consistency;
    7. the unresolved-row ledger classification;
    8. a feature-coverage matrix against the census.
  - Suspected wrong counts get three verifiers each. The review then synthesizes the briefs for
    the closing commits Z1 (soundness + precision + regressions), Z2 (cleanup) and Z3 (docs).

- **A7 delta audit:** 10 of 10 correct (4 method rows, 6 stored rows; `audit/83f5e76/REPORT.md`).
  The 4 removed numbers are honest `?`. A Scala 3.9.0 fixture shows the rebinding route really
  works: a given import named `compose` shadows the subpackage, and a following `import compose.*`
  rebinds `Nothing`. **Final audit total since b5045b8: 210 numeric rows audited across 8
  landings, 207 correct, 3 confirmed wrong; all 3 are now fixed.**

- **Ninth usage-limit stop (≈ 01:00 UTC on 2026-11-03, reset 01:10, resumed 02:16).** It hit the
  integrated review after all eight finders had finished.
  - Results so far: cross-feature lens, 52 probe groups (~330 rows), 4 suspected wrong counts. None
    touches an eo numeric row; all are fixture-level interactions of A6×C4, B5×B3 C6 and
    B5×lookup.
  - Fresh-sample lens: **72 of 72 sampled eo rows re-derived correct** (48 method, 24 stored).
  - Stored-side lens: 5 suspected defects.
  - The verifiers and the synthesis were cut off; the review was resumed (cached finders replay).

- **Integrated review finished (≈ 03:00 UTC)** (`audit/integrated/REPORT.md`, ledger in
  `audit/integrated/LEDGER-CLASSIFICATION.md`).
  - **No eo 1.0.0 number is wrong at `83f5e76`.** The fresh sample of 72 rows was 72 / 72 correct.
  - **Ten confirmed wrong-count root causes**, all fixture-level (none reaches an eo row):
    - Seven come from interactions between this PR's features, or this PR made them numeric:
      A6×C4 inline static owner, two B5×C6 views, B5×lookup hidden capture, the enum companion
      namespace, opaque `U`, and `inst2.D`.
    - Five were already wrong on main: package-object parents, FOLLOWUPS #1 inherited members on
      the stored side, opaque `V`, empty-typed value rows, and FOLLOWUPS 18 non-final case leaves.
    - (The enum companion, opaque and FOLLOWUPS #1 causes each have parts in both groups.)
  - Six precision recoveries:
    - Sound now: the R-b view sandwich (`getOptionInner` → 1), the R-c restated extension (three
      `andThen` → 1), match-type substitution (`pickFoldMiss` → 1) and one override predicate.
    - Later: final case classes with a body (12 jsoniter Span rows) and singleton goals.
    - `Optic.to/from`, `Optic.id` and `ModifyF.map.map` stay `?` as known limits.
  - Ledger: of 1658 method `?`, **1395 are by design** (unknowable from the supplied sources or
    outside the model) and **263 are analyzer gaps**. Of 112 stored `?`, 106 are by design and 6
    are gaps.
  - Quality: CPD 2 → 0 verified possible, plus 14 cleanup items. Docs: 16 consistency items.
- **Closing lanes launched in parallel:**
  - **Z1a**, method side (S1–S4, S10, P1–P6, regressions T1–T2): solver worktree, run
    `wf_4ab0785f-958`, args `resume/args-solver-Z1a.json`.
  - **Z1b**, stored side (S5–S9): report worktree, run `wf_4724a8fd-376`, args
    `resume/args-report-Z1b.json`.
  - Then **Z2** cleanup and the final docs / ledger / worklog commit. Planned total ≈ 22 commits.

- **Z1b landed (≈ 07:30 UTC)** as `280ab80`, the stored-side integrated-review fixes. 19 commits.
  - S5: an enum companion holds its cases.
  - S6: package-object parents and exports.
  - S7: inherited and self-type members (FOLLOWUPS #1, closed; register D29).
  - S8: opaque types are not singletons.
  - S9: a value row is min(1, |declared type|), register D30 (the verifier dissent is recorded).
  - Round-2 findings were fixed too: construction steps (Builds.scala), parent exports, val
    parameters, and singleton value types.
  - **eo: no count changed** (8 method and 5 stored rows changed reason only); EoBaseline 0.16.0
    passes. Three files' unreviewed 0.16.0 legacy declared-signature totals moved (Direct,
    Forget, MultiFocus). The lander reproduced them member by member and explained them.
  - Conservative refusals left as `?`: FOLLOWUPS 24–25.

- **Tenth usage-limit stop (≈ 07:00 UTC, reset 07:10, resumed 07:40).** It hit Z1a's lander after
  both fix rounds (lane commit `89964f5`, worktree clean). Z1a resumed at `Z1a:land`.
  - Z1a, as reviewed: eo **295 finite · 133 ω · 1646 ?** (12 rows `?` → number), headline before
    289 · 127 · 1658.
  - It fixes S1–S4 and S10 and recovers precision P1–P5, with the 14a regressions pinned.
  - The two round-2 regressions are fixed: the extension's own binder, and a member binder
    shadowing a class argument.

- **Paused by the owner (2026-11-03 07:54 UTC): "Resume next Monday".**
  - Z1a's lander was stopped mid-gates. lane/solver had rebased cleanly onto `280ab80` as
    `7e3e306`. I killed its sbt process and moved the partial gates run aside so it is redone.
  - The 2-hourly check-in became a single scheduled run on **Mon 2026-11-09 11:37 UTC**. It resumes
    Z1a's landing, then runs the rest: Z1a audit, Z2 cleanup, final docs / ledger / work-log commit,
    gates, PR.
  - Because an idle container may be reclaimed, the branch was **pushed for the first time**:
    origin/ccr-d57d9679-dtp3fh = `f84591f`. That is the 19 integration commits plus one
    preservation commit, "WIP … drop before the PR", holding `.session-state/` (work log, notes,
    audits, resume args, scripts, the Z1a patch, the latest eo runs). The final push drops it with
    `--force-with-lease`.

- **Resumed Mon 2026-11-09 11:38 UTC** on the scheduled run.
  - The container had survived the pause: scratchpad, worktrees and workflow cache were all
    intact, so nothing had to be restored from the preservation commit.
  - The owner had opened **PR #25** from the UI for this branch. From now on it is the PR; pushes
    update it. Its description gets written at the end, and the final force-with-lease push drops
    the WIP preservation commit.
  - Z1a's landing resumed at `Z1a:land` (confirmed in the journal), and the 2-hourly check-in is
    back on.

- **Z1a landed (2026-11-09 ≈ 12:00 UTC)** as `7e3e306`, the method-side integrated-review fixes.
  **20 commits.**
  - eo: **2074 signatures: 295 finite · 133 ω · 1646 ?**, from 289 · 127 · 1658. 12 rows went
    `?` → number:
    - `pickFoldMiss` 1;
    - `Getter` / `BijectionIso` / `Review.andThen` 1, back since A6's extension guard;
    - `getOptionInner` 1;
    - seven jsoniter `JsonPathScanner` / `JsoniterTraversal` rows (ω, `Miss` 2^64), from final
      case classes with a body.
  - The lander rebased cleanly over Z1b, merged three textual conflicts, renumbered the view
    sandwich to register **D31**, and kept CPD at 2.
  - Started together:
    - the Z1a delta audit (`audit/7e3e306`, 12 rows);
    - **Z2** cleanup in the solver worktree (run `wf_f896024a-496`, args
      `resume/args-solver-Z2.json`). Its targets: CPD 2 → 0, metrics, one lookup order, ordinals,
      typed refusals and reason wording, with no count changes.

- **Z1a delta audit:** 12 of 12 correct (`audit/7e3e306/REPORT.md`): `pickFoldMiss` 1, the three
  `andThen`s 1, `getOptionInner` 1, `JsonPathScanner.Miss` 2^64, and six jsoniter rows ω. **Final
  audit total since b5045b8: 222 changed numeric rows audited across 9 landings, 219 correct, 3
  confirmed wrong, all 3 fixed.**

- **Z3 landed (2026-11-09), head 7a37804 (21 commits).** This was the documentation commit:
  - the final ledger `docs/baselines/eo-checkout-2026-11-09-full.md`;
  - `docs/research/bibliography.md`;
  - the register entries and cross-check outcomes;
  - the v1 §2 statements and the README / AGENTS.md updates.
  
  Its two review rounds also found three wrong counts in the pure-code reading of throws. All
  three were fixed in the same commit, each confirmed by running Scala 3.9.0:
  - an inline parameter's default is read where the body reads the parameter;
  - an eta-expanded inline method runs inside a closure;
  - a constructor whose throw carries `this` is refused, not "never built".
  
  None of these shapes occurs in eo. **Delta: methods +0 −0 ~0, stored +0 −0 ~0.** The TSVs are
  byte-identical to 7e3e306. 0.16.0 is unchanged and plugin/scripted passes. A Scala 3.9.0 run
  also showed that the defaults of non-inline parameters compile where the method is written, so
  they are left as they were.

  *Not ported:* `7a37804` never reached main; the owner discounts throws (D-pure), and the
  documentation it carried is folded into the commit that added this log where it still holds.

- **Z2 landed (2026-11-09), head 6c384ff (22 commits).** This was the quality cleanup:
  - CPD duplications 2 → 0, with no file-metrics warnings;
  - one inline predicate and one term-lookup precedence table (FOLLOWUPS 22, closed);
  - typed refusals and lowercase reason heads;
  - rows named by ordinal: `<given#k>`, `<extension#k>`, `<init#k>` (FOLLOWUPS 23, closed).

  Its two review rounds confirmed 26 findings and rebutted none. Two of them were wrong counts that
  the new ordinal names would have introduced, and both were fixed before landing:
  - a class and its companion shared a `<given#k>` path, and later a local class and its local
    companion did too;
  - two overloads' local objects exported a name differently.

  The reviews also found two older wrong counts, which are now refused:
  - a self type's member lost to another file's package type (counted 1, true 4);
  - a false `import E.*` channel.

  *Correction:* the two older wrong counts were a class reading its companion's type member
  (counted 4, true 1) and a self type's member (counted 1, true 4); the `import E.*` channel was a
  false reason, not a wrong count. Neither fix reached main, and the ledger lists both among its
  known limits.

  None of these occurs on eo.

  The lander resolved one conflict: Z3 had moved code out of `BodyWalk` into a new
  `BodyMeanings.scala`, and Z2's edits were re-applied there. **Delta: no count changed.** 175 eo
  rows were renamed (174 anonymous owners plus `TraverseBuf.<init>` → `<init#1>`) and 28 rows
  changed only their reason text. Six blocker categories were renamed with the same counts. The
  methods, definitions and blockers TSVs are byte-identical to both sides' runs. 0.16.0 is
  byte-identical. The test suite grew from 77 to 79 suites, and plugin/scripted passes 3/3.

  *Not ported:* `6c384ff` never reached main; the owner takes row names from TASTy, and the
  documentation it carried is folded into the commit that added this log where it still holds.

## 4. Landed features (in landing order)

### A1 — Ground scalar types and exact symbolic counts (landed)

- **Landed:** `e575d01` on integration `ccr-d57d9679-dtp3fh` (previous `613f655`, fast-forward, no
  rebase — A1 is the first lane commit to land). Gates: `runs/e575d01567a914717801fe5e7dec3e990f7b96b0`
  (test 36/36 suites, cached 0; scalafmt, scalafix, file metrics, EoCheckout pass; CPD 11 = base 11;
  MethodAnalysis.scala 811 → 788 lines, the 800-line warning is gone). Same tree as the reviewed
  `runs/A1-fix2` (91b88a5, message-only amend): methods TSV byte-identical.
- **Rule:** `Byte`/`Short`/`Char`/`Int`/`Long`/`Float`/`Double`/`String` are transparent ground types
  (2^8, 2^16, 2^16, 2^32, 2^64, Float/Double under `equals` 2^32−2^24+3 / 2^64−2^53+3, String ω). A ground
  goal is its literals; every closed environment value (ground, closed function, declared nullary
  member, closed sequence) is split uniformly: `#(Γ, x:T ⊢ τ) = #(Γ ⊢ τ)^|T|`, ω-sized T under the
  finite-program reading (0 / 1 / ω). Exact `Count.Symbolic(Natural)` above 65,536 bits (normal form,
  budgeted representation replaces the magnitude refusal). Constructors read closed positions by exact
  value space (K1–K4). Derivation: `docs/research/ground-scalar-types.md`, evidence register D9,
  `design/ground.md` §3.3; details `notes/A1.md`.
- **Soundness guards added for the split:** `InheritedClosedSlots` (one independent value per closed
  inherited declaration, per enclosing template), `SealedReceivers` (interim G2: enum/sealed receiver
  keeps only terminal / ω-over-inhabited-enum results), `ShadowedBuiltins` (a builtin name reached
  through parents, self types, enum cases, wildcard-imported objects/packages, package-object parents,
  exports refuses), name precedence (innermost package first; explicit import before another file's
  package member).

#### Measured delta (vs 613f655)

Headline 46 finite · 38 ω · 1 beyond · 1554 ? → **47 finite · 46 ω · 1 beyond · 1545 ?**.
Count changes (all `? → n`, 9 rows): `AvroWalk.normalisedName` ω, `normalisedName.go` 1 (Unit),
`AvroFailure.message` ω, `JsonFailure.message` ω, site constructors `avrodocs.Person`,
`circedocs.Person`, `docs.Person`, `chimney.Order`, `chimney.User` ω (String/ω factor; Address/Customer
declared in several packages, now read lexically as closed products). 615 reason-only rows: 794 ground
reason pairs removed (`String` 415, `Int` 233, `Byte` 133, `Long` 11, `Char` 2, + 9 union-wrapped);
441 rows surface the next blocker (mainly `Array` 319, `List` 73, `Throwable` 61, `DecodingFailure` 37,
`Schema` 16), 174 keep only existing blockers. Ten constructor rows keep their text but become exact
`Finite` (e.g. `DirectSlot` 2^64) instead of `Bounded`. Stored-value rows: 0 changes. Reviewed eo rows
unchanged on this run: `CanGet.get` 0, `getOption` 1, `place` 1, `transfer` ω, `replace` 1.

#### Review findings and resolution

- Round 1 (F1–F17): inherited nullary members double-counted (fixed: `InheritedClosedSlots`, incl. a
  shared-`seen` under-count found while fixing); enum/sealed receivers (guarded); K2 finite estimate vs
  infinite space (refused "readings disagree"); ω fallback under-stating ε₀ (size algebra's tier);
  inherited type member shadowing `Int` and two name-precedence bugs (fixed); test gaps
  (`GroundEnvironmentSpec`); `ClosedSpaces.space` totality, dead `Bounded` branch, budget/Float pins,
  commit-body figures, gate evidence. F7 (two anonymous `using Int`, 2^(2^37) vs true 2^32) deferred to
  B0 by D-anon-evidence ownership.
- Round 2 (R1–R13): builtin names shadowed through unwalked channels (fixed: `ShadowedBuiltins`, four
  more channels found while fixing; a kyo over-trigger caught and pinned by `notBuiltin`); open
  declaration from two parents (identity check; three shapes now refused rather than miscounted);
  enclosing-template parameters wrongly valued as constructor positions (now captures); singleton
  beside a value (refused); symbolic `agrees` one-way (fixed, `>=` boundary); report symbolic row test;
  `scanParents` tailrec; one `ClosedSpaces` algebra; test-only `Natural` helpers moved to test. R12
  coverage run separately: 92.39 % statements / 86.71 % branches. R13 (EoCheckout puts ε₀ under
  unresolved) is pre-existing → C3.

#### Expectations changed

- Owner-reviewed: **none** (`CanGet.get`, 0.16.0 five, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec, plugin fixtures unchanged).
- Other tests (derivations in `notes/A1.md`): `InhabitationSpec` 65,537-field product `Unresolved` →
  `Symbolic(2^65537)`; `MethodAnalysisSpec.constructorSpaces` `Holder` `Bounded(IntSize)` →
  `Finite(2^32)` (same render); `MethodUnionNullabilitySpec.unsupported` reason now the union's own;
  `OverrideIdentityImplementationSpec.fileIdentity` fixture rewritten to Boolean (16) so it still
  distinguishes kept from dropped.

#### Open items (`?`)

- ? F7 same-type anonymous `using` ambiguity — B0 lands the guard; the ground regression goes in with
  whichever of A1/B0 lands second (B0 now, when it rebases onto e575d01).
- ? Ground names under a foreign wildcard import still resolve to the builtin (shared with
  `Boolean`/`Option`; A3 decides; `AvroWalk.normalisedName` rests on it; `GroundEnvironmentSpec.wildcard`
  pins today's value as the TODO).
- ? Elimination of an opaque callable's ground result (`f[A](g: A => Int, a: A): Int`) — B4.
- ? Constructor positions mixing type parameters with ground values; symbolic factor beside a finite
  estimate capacity; literal types (A5); `Integer`/`BigInt`/`BigDecimal` (A3); `var` (A4); dotted
  `package p.q` seeing `p`'s members (B0/C1); binding a sealed receiver as its sum (G2, B2).
- ? CodeScene not runnable here (new code split into small methods by design).

### B0 — Environment and family readings that miscounted (landed)

- **Landed:** `58a217b` on integration `ccr-d57d9679-dtp3fh` (previous `e575d01`, A1), compare-and-swap
  after a rebase of the reviewed lane commit `a42caee` (round 2). Gates:
  `runs/58a217b9e38ed19a76ed1dde0c8832f31f0d166f` — test 38/38 suites, scalafmt, scalafix, file
  metrics, EoCheckout PASS; CPD 6 (integration head 11). Methods TSV identical to the pre-message-amend
  run `runs/79cc349…`. Details: `notes/B0.md` (rules, review rounds, "Integration delta").
- **Rules:** anonymous `using` parameters keyed by owner and position and counted only when implicit
  search provably selects them (`ambiguous anonymous evidence` / `anonymous evidence selection not
  resolved`); names bound as Scala binds them (`_root_` anchors, a clause opens exactly its path,
  import-bound path prefixes, wildcard over another file's package member, inherited members first);
  inherited declarations read under the whole `extends` chain's substitution, one slot per receiver;
  declared callables keyed by resolved parameter identities (overloads vs restatements); final
  concrete templates' own implementations are concrete code; names an unsupplied parent may declare
  are refused; enum cases with `extends`; sealed families with anonymous / inner / local / alias-spelled
  children refused; self types refused; `var` constructor params as mutable captures (interim, A4).
  Derivation: `docs/research/environment-readings.md`, evidence register D10.

#### Measured delta (vs e575d01)

Headline 47 finite · 46 ω · 1 beyond · 1545 ? → **63 finite · 46 ω · 1 beyond · 1529 ?**.
Count changes (all `? → n`, 16 rows, exactly B0's own set on 613f655): final optics implementing their
inherited declarations — `BijectionIso.get/modify/replace/reverseGet`, `Modify.modify/replace`,
`Review.reverseGet`, `Getter.get`, `MendTearPrism.to/from/reverseGet`, `PickMendPrism.from/reverseGet`,
`PSVec.Single.head` (1 each), `PickMendPrism.getOption`, `PickFold.getOption` (2). Three-way check
(613f655 / A1 / B0 alone / merged): 0 count interactions; A1's 9 numbers unchanged. 558 reason-only
rows: 354 as B0 changed them alone, 193 the union of both commits' reason changes, 11 where A1's next
blocker meets a B0 reading (6 import-named: avro `Schema.Type`, zio `Chunk`; 5 generics Hearth rows
where `String` is refused under the unsupplied `hearth.MacroCommonsScala3` parent before the ground
reading). Main movements otherwise as on 613f655: `_root_` imports bind (59), import-named unresolved
names (92), unsupplied-parent refusals (167, mostly discipline `Laws`), dropped identity/binder/abstract
reasons under final templates (223). Stored-value rows: 0 changes. Reviewed eo rows unchanged.

#### Review findings and resolution

- Round 1 (25 findings, 12 confirmed wrong counts — e.g. nearest-first anchoring reaching an unopened
  `a` from `package a.b`, colliding anonymous keys across owners, later block givens, `seen` shared
  across receivers, W2 depth-2 over-count, enum-case parameters through `extends`): all fixed with
  compiled fixtures (`notes/B0.md` §Review responses round 1).
- Round 2 (22 findings: hidden local captures behind rule 5, `abstract final`, inherited `val`/`given`
  arms, same-shaped overloads of two scopes merged, local children in val/lambda/if blocks, dotted
  names and import-bound prefixes, wildcard precedence, marker parents via import, test gaps,
  comment share, D9 collision): all fixed (`notes/B0.md` §round 2).
- Landing (integration with A1, each found by the merged suite or the three-way delta): A1's
  `InheritedClosedSlots` now judges B0's per-receiver slots keyed by `DeclaredMembers` (its `borrowed`
  / "several enclosing templates" refusals cannot arise and were removed; `fixed` follows
  `FinalImplementations.isFinal`); `TypeIdentities` knows the ground builtins (else A1's
  `openRestated` was refused); `ShadowedBuiltins` reads `_root_` wildcard prefixes and parents at their
  anchor (B0 keeps `_root_` in import paths, which silently disabled the guard for
  `import _root_.w8.O.*`: 2^(2^37) for a true 4); a plain constructor parameter is no member (B0's
  substitution exposed A1 conflating `SimpleLens(get: S => A)` with `CanGet.get`, a spurious refusal on
  `SimpleLens.place/transfer`; the direct-parent twin was already refused by A1 alone). F7 regression
  added (`def f(using Int, Int)`: refused, true 2^32).

#### Expectations changed

- Owner-reviewed: **none** (`CanGet.get`, the five 0.16.0 counts, EoCaseLedgerSpec,
  OpaqueTypeScopeSpec, ReferenceInhabitantsSpec, ArticleCardinalitySpec, plugin fixtures pass unchanged).
- B0's own (derived in `notes/B0.md`): `MethodAnalysisSpec.packagePrivateSubpackage` fixture uses
  chained clauses (the dotted one does not compile); two `OverrideIdentityImplementationSpec` fixtures
  become abstract classes, a valid object twin added.
- A1's `GroundEnvironmentSpec` at landing (derivations in `notes/B0.md` §Integration delta):
  `overloaded` refused → 2^(2^(2^37+2^69)) (refusal re-pinned on a `def`/`val` pair of one member);
  `nestedReceivers` n3, n5 refused → 16, n6 refused → 4; `openRestated` i1 refused → 2^64;
  `inheritedBuiltin` Sub.f/Leaf.k, `importedParents` w14/w15 shadow refusals → 4;
  `constructorScope` `cp.O.P.<init>` → 2; `moreChannels` ap/bp keep `?` with B0's
  unsupplied-parent reason. New: `restated` val twin (4), `plainParameter` (1, 1), `anonymousGround`.

#### Open items (`?`)

- ? `var` constructor parameters stay `mutable capture` until A4 applies D-var.
- ? Alias parents (`type B = Base; … extends B`) are read as unsupplied parents (refused); following
  them is open.
- ? Same-level contextual ranking, implicit scope of companions, override keying of inherited type
  names in open templates, covariant diamonds (`inherited parent instantiated twice`), intermediate-parent
  implementations (G1, B2), self types (B3) — all `?` with their reasons.
- ? A1 pre-existing, no eo row: trait `val` parameters are not read as fields; the root-field drop
  does not compare the declaration's parameter clauses.
- ? Marker parents under an unsupplied wildcard import (`import lib.*; … extends Product`) still read
  as the marker — A3's wildcard rule.
- ? CodeScene not runnable here.

### C1 — Stored-value names: a type owns its name, namespaces, qualified references (landed)

- **Landed:** `c136109` on integration `ccr-d57d9679-dtp3fh` (previous `58a217b` = A1 + B0), compare-and-swap
  after rebasing the reviewed lane commit `ee2591c` (fix round 2). Gates: `runs/c136109da1fce4964d3ee9cd424a0e5f995f832d`
  — test 40/40 suites (cached 0), scalafmt, scalafix, file metrics, EoCheckout PASS; CPD 6 (= integration head 6).
  Methods TSV byte-identical to the pre-amend run `runs/a592f2e…` (the amend changed docs and the message only).
  Details: `notes/C1.md` (rules, two review rounds, "Integration delta").
- **Rule:** a name in the stored-value estimate denotes the declaration Scala binds it to — types and terms
  apart (no companion stands in for a trait or a sealed family), objects and packages as namespaces, a qualifier's
  first name is the innermost term binding, bare names by Scala's precedence (enclosing bodies > explicit import
  or export > wildcards latest first > opened packages > root), positional imports, wildcards over unsupplied
  namespaces refuse, per-file library reads in SCC order, class members in their class's body, match-type bodies
  in their definition's scope, type constructors named. The constructor-slot reading is read where the
  constructor is written; a ground slot it declines makes the row `?`. Derivation: SLS ch. 2 (classical) +
  scalac 3.9.0 fixtures; Bekić for joint object/body solving. `docs/research/stored-value-names.md`, register D11.

#### Measured delta (vs 58a217b)

Headline 63 finite · 46 ω · 1 beyond · 1529 ? → **60 finite · 47 ω · 0 beyond · 1532 ?**; stored values
58 unresolved / 26 instantiation / 11 open / 3 constructors / 78 many / 163 one / 7 zero / 89 abstract →
**71 / 26 / 13 / 3 / 71 / 127 / 5 / 119** (stored section identical to the lane run: A1/B0 touched none).
Method count changes (7, all C1's own, three-way checked): `avrodocs.Basket`, `circedocs.Basket` `<init>` ? → ω;
`AvroFailureException.<init>` 18ω + 2^161 → ? (a case holds unsupplied `Throwable`); `AvroWalk.WalkRes.<init>` ε₀ → ?,
`WholeRecordBuilder.DirectPlan`/`SelfKind` 2^32 → ?, `DirectSlot` 2^64 → ? (`import scala.jdk.CollectionConverters.*`,
an unsupplied namespace, may bind `Int`/`Any`/`Array` — D-stdlib). Stored values: 30 open traits/abstract classes
1 → — (companion no longer stands in), 6 companion-cased sealed families 1 → ? (not summed), `AvroBytes` 0 → ω,
`AvroSliceFace.X` 0 → ?, two Jsoniter `X` ? → ω, ten AvroWalk/WholeRecordBuilder rows n → ? (the wildcard).
Reason-only: 36 method rows (union of the head's and C1's reasons; A1's next blocker `Array`/`List` dropped where
the estimate now reads the slot), 47 stored rows (clause provenance, families named). No eo slot disagrees.
A1's and B0's 25 count changes all kept. Reviewed eo rows unchanged (`CanGet.get` 0 etc.).

#### Review findings and resolution

- Lane rounds (before landing): round 1 (25+ findings: cross-file imports, self-instantiation, nested binders,
  lazy cycles, class members, Reading containers, …) and round 2 (25 findings, every wrong count confirmed by
  probe and fixed: match-type case scope, positional imports, term first names, owner shadowing, wildcard sources,
  library outranking, refused-slot gate, modelled shadowing, exports, lazy alias, partial families, abstract
  member shadowing, SCC library order, reasons/dedup, dead code, docs) — `notes/C1.md`.
- Landing (integration with A1/B0, each found by the merged suite): (1) C1's estimate gate moved into A1's
  `ConstructorPositions` (`Declined`, applied before A1's classification). (2) **A1's agreement check was unsound
  against C1's estimate**: "literal within the estimate's capacity" accepted `Dep(q: Other)(val x: q.X)` (method side
  2 via an object `q`, wrong; estimate 2^32, right), `O2.U(o: Obj)` under `export Src.*` (1 vs 2) and `UsesT(t: x.T)`
  through a package val (1 vs 2) — three wrong constructor counts on the merged code — while
  `object O extends Base { case class P(x: Int) }` has the same numbers with the method side right. Fixed: a literal
  agrees only when the size algebra's image of the method side's type equals the estimate's finite part (equal exact
  counts ≤ 127, equal capacity widths); otherwise refused. (3) A declined ground slot stays `?` even where the method
  side reads it closed (no cross-check), costing A1's explicit-import constructor its ω.

#### Expectations changed

- Owner-reviewed: **none** (`CanGet.get`, the five 0.16.0 counts, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec, plugin fixtures pass unchanged).
- At landing (derivations in `notes/C1.md` §Integration delta): A1 `GroundEnvironmentSpec` — `disagreement` and
  `symbolicDisagreement` fixtures now agree (ω, ω, 2^(2^(2^37+5))) with `q.X` twins that disagree; `singletonPositions`
  takes the estimate (2^8 bound, 2, 256) + a declined `Missing` twin; `explicitImport` `x.Outer.<init>` ω → ?;
  B0's `constructorScope` `cp.O.P.<init>` 2 → ? (estimate misses the inherited `Base.Int`). C1's own: `exports`,
  `termBindings`, `classMemberScope`, `slotParameterPath` constructor rows → ? (disagreement; the latter also B0's
  environment reason), `slotQualified` reason text (B0's provenance kept), `slotRefusal` loses `Int` (A1 ground).

#### Open items (`?`)

- ? **Stored-value side binds no inherited type member** (pre-existing): `object O extends Base { case class P(x: Int) }`
  with `Base.Int = Boolean` gives stored row 2^32 (true 2) — a wrong stored-value number; constructor rows catch it as a
  disagreement. Needs a parent walk in `Scope.binding` (with A1's unsupplied-parent rule) — lane report follow-up.
- ? Method side does not bind export wildcards, term paths through vals/parameters, or class members in their class's
  body (lane types B2); constructor slots refuse, method signatures are unchecked.
- ? Explicit imports/exports not followed by the estimate (C2/R4) — explicit-import constructors stay `?`.
- ? Wildcards over the standard library refuse base types until A3's stdlib model lists what they define.
- ? Bare reads within one body still lend placeholders (C2/R7); generic/companion-cased sealed families not summed (C2/R2).
- ? Capacity rounding of one type in two fold orders would refuse as a disagreement (none in eo; no fixture found).

### A2 — Top types, terminal results and unions of disjoint classes (landed)

- **Landed:** `4db1023` on integration `ccr-d57d9679-dtp3fh` (previous `c136109` = A1 + B0 + C1), compare-and-swap
  after rebasing the reviewed lane commit `1a3279c` (fix round 2). Gates:
  `runs/4db1023126e82530f9f363a74e94aeb487dfc4f3` — test 41/41 suites (cached 0), scalafmt, scalafix, file metrics,
  EoCheckout PASS; CPD 6 (= integration head 6). Same tree as the scratch run `runs/A2-land-scratch` (822dcd1, the
  amend changed the message only). Details: `notes/A2.md` (rules, two review rounds, "Integration delta").
- **Rule:** on the method side `Any`/`AnyRef`/`Object`/`Matchable`/`AnyVal` are the terminal type 1 (∃X.X with every
  universal member excluded; free theorem ∃X.X ≅ ∀R.R→R ≅ 1), reached only past every shadowing channel — a channel
  to declarations the sources do not supply (foreign self type, unsupplied parent of a wildcard-imported or package
  object, unsupplied wildcard import/export, outer clause of a chained package) refuses the top name. A declared result
  with exactly one value counts 1 whatever the environment leaves unresolved (D-terminal), except under a clause that
  may rebind a builtin or such a channel. `transparent inline` results that a subclass may narrow, and top-typed
  `inline` parameters, are refused. Constructor slots are asked of the estimate as the implementation model reads them
  (top → `()`, `Null` empty), only under constructors holding the top by position (`Set[Any]` is not `Set[Unit]`).
  Unions are tagged sums only for pairwise provably disjoint, discriminable runtime classes with enumerable families
  (coproduct copairing); union aliases flatten, `T | Any = Any`, `T & Any = T`. Derivation:
  `docs/research/top-types-unions.md`, evidence register D12 (tops / one-valued goals) and D13 (disjoint unions),
  `design/top-unions.md`.

#### Measured delta (vs c136109)

Headline 60 finite · 47 ω · 0 beyond · 1532 ? → **96 finite · 47 ω · 0 beyond · 1496 ?** (by module: avro +13,
core +17, jsoniter +4, schemes +2). Count changes: **36, all `? → 1`** — 34 `Unit` results beside an unresolved
environment (e.g. `AvroBinaryCursor.skipValue`, `IntArrBuilder.append`, `Getter.from`,
`Plated.transformMachine.drive`, `JsonPathScanner.walkAll`, `Schemes.unfoldCoalgHeap.enter`) and 2 `Any` results
(`AvroCodec.encode(a: A): Any`, `AvroRecordTraversal.onRec`) — exactly the lane run's 36 `? → 1`. `WalkRes.<init>`'s lane `ε₀ → ?` was already made `?`
by C1 and is now reason-only, naming both refusals. Reason-only: 141 — all 141 rows carrying
`unresolved type: Any` lose it (131 reason-only + 10 count changes); 62 gain `builtin name Any may be shadowed
through <ns>, which is not supplied (wildcard import of <ns>)` (`scala.jdk.CollectionConverters`, `scala.quoted`),
18 `builtin names may be shadowed …`, the next blocker surfaces (`Array` 31, `Vector` 5, `Throwable` 2), 19
transparent selectors take the transparent reason, 5 inline selector extensions the inline-parameter reason.
Stored values: 0 changes. Three-way check (58a217b / lane / C1 / merged): every changed row is exactly the union of
the two commits' changes — 0 count and 0 reason interactions. Reviewed eo rows unchanged (`CanGet.get` 0,
`getOption` 1, `place` 1, `transfer` ω, `replace` 1, `Direct.apply` 1).

#### Review findings and resolution

- Round 1 (18 findings, confirmed wrong counts among them: self types / foreign wildcard imports / package objects
  rebinding `Unit`/`Any`, transparent products of non-final classes, union sums over a family reading that misses an
  enum-case child): all fixed with Scala 3.9.0 fixtures — the nameless `ShadowedBuiltins.unread` walk, rule X over any
  class a subclass may extend, `RuntimeClasses.enumerable`; 28 eo rows reverted to an honest `?` under unsupplied
  stdlib wildcards (A3 can restore them). Quality: total `disjoint`, shared `Union`/`Meet` extractors, one
  `enterAlias`.
- Round 2 (23 findings, all confirmed): `Set[Any]` / `Map[Any, _]` slots (a `Set` merges equal elements: refused,
  rewrite only under positional builtins), a family child extending through `Id[S]` (unions guarded), the outer
  clause of a chained package, `inline` top parameters (`inline match` dispatch), field captures typed in the
  constructor's frame (also fixed an over-count `C.m` 16 → 4); quality refactors (Kind enum, no flag args, caches).
- Landing (integration with C1): C1 reads a constructor slot where it is written; A2's rewritten slot is a new tree
  with no position, so it would have been read at the top of the file (probe: `O.Box(f: (Any, Red))` `?`
  "unresolved type: Red"). Fixed with `Reading.read(tpe, written)` (regression `TopTypesUnionSpec.rewrittenScope`:
  1 and 4). And C1's decline (a slot resting on a name the estimate cannot bind is `?`) now also covers the slots A2
  does not ask the estimate to size (union left over, aliased top, merging container): read as written, only the
  size unused (`R(x: Red | Blue)` under an unsupplied wildcard: `?`, not the method side's 3). `positions.declining`
  wraps the measured count beside rule O. Neither moves an eo row.

#### Expectations changed

- Owner-reviewed: **none** (`CanGet.get`, the five 0.16.0 counts, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec, plugin fixtures pass unchanged).
- A2's own (derivations in `notes/A2.md`): `MethodUnionNullabilitySpec.nominal` `? → 2`, `String | Boolean` `? → ω`
  (now example `builtins`), `PolyLambdaCardinalitySpec.environment` `? → 1`, four examples (five snippets) with a
  `Unit` result switched to `Boolean` so their environment diagnostics stay visible under rule O. None changed at
  landing; new `TopTypesUnionSpec.rewrittenScope`.

#### Open items (`?`)

- ? `Unit`/`Boolean`/`Null` and the other builtins still resolve under an unsupplied wildcard on the method side
  (only the top names and rule O refuse there) — A3's wildcard refusal; A3's stdlib model can also restore the 28
  rows refused under `scala.jdk.CollectionConverters.*` / `scala.quoted.*`.
- ? No eo union resolves yet (each blocked first by an alternative that does not resolve); infix union aliases,
  `A <: Any` (B1), intersections other than `T & Any`, callables returning an open union (B4).
- ? Family reading misses children through parameterised aliases and enum cases of another enum (lane types);
  `ContextualScope.parameters` reads class `using` parameters in the class frame (lane types).
- ? A stdlib model of `Set`/`Map` must not read a top element or key as `()` (A3); binder twins of `inline` dispatch
  and ground `inline match` constants are A5's compile-time-evidence refusal.
- ? `WalkRes.<init>` names one cause twice (method side and estimate) — C3 report hygiene.

### B1 — Higher-kinded and bounded type parameters (landed)

- **Landed:** `bda05e1` on integration `ccr-d57d9679-dtp3fh` (previous `4db1023` = A1 + B0 + C1 + A2),
  compare-and-swap after rebasing the reviewed lane commit `09390c7` (fix round 2, base `58a217b`). Gates:
  `runs/bda05e14dac85b2128e24cf26f67ff35f9dc4f44` — test 45/45 suites, scalafmt, scalafix, file metrics,
  EoCheckout PASS; CPD 6 (= integration head 6). Methods TSV byte-identical to the scratch run
  `runs/B1-land-scratch` (the later amend was a CPD-only refactor and the message). Details: `notes/B1.md`
  (rules, two review rounds, "Integration delta").
- **Rule:** an unbounded higher-kinded binder with plain holes is a rigid constructor — `F[τ…]` an
  uninterpreted base type over its arguments' nominal keys (declarations / canonical builtin names, aliases
  expanded in scope, tuple/function sugar, operators as η-long lambdas, `=:=` rewriting to a proven fixpoint),
  no constructor or eliminator; higher-kinded arguments are operators applied by β-reduction where written,
  callee first, kind-checked; a context bound is one more contextual parameter (two of one type ambiguous); a
  subtype bound is an ambient `<:<` proof (directed view between binders / rigid applications, closed
  structural views upper and constructions lower), with refusals for recursive, cyclic (E140), view, class,
  meeting and shared-case bounds. Derivation: `docs/research/higher-kinded-binders.md`, register D14
  (Girard / Friedman / Dougherty–Subrahmanyam / Atkey / Vytiniotis–Weirich / Pierce at their recorded
  verification levels), `design/hkt.md` R1–R5.

#### Measured delta (vs 4db1023)

Headline 96 finite · 47 ω · 0 beyond · 1496 ? → **108 finite · 47 ω · 0 beyond · 1484 ?** (core 65 → 77).
Count changes: **12, all `? → n`, exactly the lane's 12 with the same numbers**: `Accessor.get` 0,
`PartialAccessor.getOption` 1, `ReverseAccessor.reverseGet` 0, `ForgetfulFunctor.map` 0,
`ForgetfulTraverse.traverse` 0, `ForgetK.apply` 1, `ForgetK.value` 1, `ForgetPull.redistribute` 0,
`MultiFocus.apply` 1, `MultiFocusK.context` 1, `MultiFocusK.foci` 1, `Unfold.<init>` 1 (each derived in
`notes/B1.md`). Reason-only: 605 rows; 362 lose the blanket binder reason (350 reason-only; the lane's 363
less `ForgetFold.from`, already 1 by A2); 168 of them now stop at an abstract-representation (capability)
reason, `Optic`'s on 128; callee-first reports unsupplied callees (`Type` 67, `Ior` 60, `Expr` 40,
`Functor` 26 …) instead of their arguments (`Throwable` 34, `DecodingFailure` 31, `Either` 21 …).
Stored values: 0 changes. Three-way check (58a217b / lane / A2+C1 head / merged): 0 count interactions;
101 rows had reasons changed by both sides — 51 exactly the union, 50 explained (33 A2 `Any`-shadow reasons
inside arguments of an unsupplied callee not read by callee-first; 12 rows A2 made 1; 6 C1 estimate/method
texts joined into one per name; 1 `mapElementsIor` reaching `Ior` once A2 resolves `Any`). Reviewed eo
rows unchanged (`CanGet.get` 0, `getOption` 1, `Direct.apply` 1; 0.16.0 five via the suite).

#### Review findings and resolution

- Lane rounds (before landing): round 1 (20 findings: lower/upper bounds overcounting, degenerate bounds,
  opaque keys outside scope, `F[?]` counted, `MultiFocus.apply` reason, quality) and round 2 (14 findings:
  views of views, F-bounds, E140 cycles incl. unused binders, `java.lang.*` boxed names, `viewed` marker
  collision, AnyRef/AnyVal as no-ops, unchecked operand kinds) — all accepted and fixed (`notes/B1.md`).
- Landing (integration with C1/A2; textual conflicts resolved as unions, merged suite green, then three
  semantic interactions found by probes and fixed): (1) **A2's one-value rule counted invalid programs** —
  `[A >: Option[A]]`, `[A <: B, B <: A]`, `[A <% Int]` clauses with a `Unit` result gave 1; an invalid clause
  now defeats the shortcut (`BinderBounds.invalid`). (2) **B1's no-op bound names bypassed A2's channels** —
  under `import foreign.*`, `[A >: Flag <: AnyRef](x: A): A` was 3 (4 if the wildcard rebinds `AnyRef` to
  `Flag`); `Any` / `Nothing` / class bounds now read through `TopTypes.isAny` / `isNothing` / `denoted` and
  refuse with A2's channel reason. (3) **C1 read a class's context-bound position at the top of no file** —
  the synthetic evidence has no parent, so `Bound[A: Tag]` gave 2 while `Using[A](a: A)(using Tag[A])` gave
  `?` (`sealed family not summed`); `ContextBounds.written` reads it at its binder's declaration. Two CPD
  duplications the merge introduced removed (`Found.named` shared by Callees / RuntimeClasses /
  BinderBounds; `TopTypes.denotes`). No eo row moved by these fixes.

#### Expectations changed

- Owner-reviewed: `OpaqueTypeScopeSpec.higherKindedBoundary` `? → 0` (the lane's, derivation in
  `notes/B1.md`: inside its scope the opaque is `F[A]` with `F` rigid; nothing yields an `A`). None at
  landing.
- Other tests: the lane's (`MethodTypePlaceholderSpec.higherKinded` `? → 2`, `GeneralInfixApplicationSpec
  .externalEffects` reason `unresolved type: <`, endpoint reason rewording in three specs). None changed at
  landing; new `BinderBoundsSpec.invalidTerminal`, `BinderBoundsSpec.rebindableTops`,
  `HigherKindedBindingSpec.classBoundPosition`.

#### Open items (`?`)

- ? Read-site ill-kindedness beside a one-valued result (`x: F[?]`, `Either[A]`, `Phantom[Option]` with a
  `Unit` result) still counts 1 under D12: refusing needs a typed invalid-program signal through resolution.
- ? Variant / bounded higher-kinded binders, open-trait bounds (capability records, B3), F-bound
  inhabitation (fixpoint), meeting structural bounds (nominal conformance), opaque types in keys (D-members),
  structurally substituted arguments (hkt §3.4), `A =:= F[A]` (exact 1, refused by proof).
- ? `README.md:249-250` still says bounded or higher-kinded alias parameters remain guarded — the
  orchestrator's README pass.
- ? CodeScene not runnable here.

### C2 — Stored-value soundness: closed families, imports, taint, bounds, containers, members, match types (landed)

- **Landed:** `7ab0a74` on integration `ccr-d57d9679-dtp3fh` (previous `bda05e1` = A1 + B0 + C1 + A2 + B1),
  compare-and-swap after rebasing the reviewed lane commit `e74edbb` (fix round 2, base `c136109`). Gates:
  `runs/7ab0a745c4935e2d26d75c1a1416e246d3e1a3ad` — test 47/47 suites, scalafmt, scalafix, file metrics, EoCheckout
  PASS; CPD 6 (= integration head 6). Same tree as `runs/fc52b41…` (the amend changed the message only; methods TSV
  and delta byte-identical). Details: `notes/C2.md` (rules, two review rounds, "Integration delta").
- **Rule (stored-value side; constructor-slot reading lent to the method side):** a sealed family is the tagged
  sum of the direct subtypes its file declares in its body and nested objects (R2), each subtype binding every
  parameter to one slot, bounded and varying as the slot does, or bottom-pinned with the standard `Nothing`/`Any`;
  aliases, renamed imports/exports, rebinding objects, GADT pins, shadowed data constructors refuse. Generic
  instantiations are least fixed points through positive positions only (no count through a function's domain, an
  unread constructor or a value's type member). Explicit imports/exports are followed into supplied sources, an
  export outranking an import (R4, CROSSCHECK X1). A row carries every non-parameter reason it reads (R7); context
  bounds are stored evidence (R8); finite collections keep the element's tier (R9, stored side only — the method
  side keeps its own ω); enclosing abstract members and stuck match types wait for the instantiation (R10, R11);
  lossy widths survive sums/products (R12); reasons rank unresolved > open > instantiation and all render (R13).
  Derivation: `docs/research/stored-value-families.md`, register D15 (moved after D14 at landing),
  `design/stored-value.md` R2–R13.

#### Measured delta (vs bda05e1)

Headline unchanged at **108 finite · 47 ω · 0 beyond · 1484 ?** (no method count moves). Stored values
71 U · 26 I · 13 O · 3 C · 71 many · 127 one · 5 zero · 119 abstract → **67 U · 37 I · 13 O · 3 C · 65 many ·
126 one · 5 zero · 119 abstract**. Count changes: **12 stored rows**, the lane's exactly — 7 n → `?`
(`ElementSpan`, `ElementSpans` \[Schema\], `BridgedBytes`, `AvroFailureException` \[Throwable\], `OptionKind`,
`RecordKind` (taint, R7), `TraverseTraversal` \[Traverse evidence, R8\]); `PSVec.Slice`, `TraverseBuf` ω → ε₀
(`Array[Any]`, R9); `docs.Shape`, `Shape2`, `KyoShape` gain `(lossy)` (R12). Reason-only: 60 stored rows (clause
wording `not followed` → `not supplied` / `may be imported by …CollectionConverters.*`, taint, families now summed
or instantiation-dependent: `Affine`, `Fst`, `Snd`, `PSVec`, `Optional.Fused`, `Coalg`, zio `Lens`/`Prism`) and 33
method rows (15 `andThen`/`apply`/`fused` rows read `sealed subtype Fused binds M0 to no slot of Optional`; Avro
`<init>`s drop `sealed family not summed: AvroFocus`; `WholeRecordBuilder` rows add the unread-`List` recursion
reason). Three-way check (base `c136109` / lane / head / merged): stored rows equal the lane's on all 435 (A2/B1
touched none); methods 0 count and 0 reason deviations; 5 rows changed by both sides — 4 keep the head's
method-side wording of a name the lane only reworded in the estimate's text (`SchemaWeakKey`, `CodecKind`,
`CodecPlan`, `ForgetFold` `<init>`; hence 33 not the lane's 37), 1 exact union (`RecordKind.<init>`). Reviewed eo
rows unchanged (`CanGet.get` 0, `getOption` 1, `Direct.apply` 1, `Accessor.get` 0).

#### Review findings and resolution

- Lane rounds (before landing): round 1 (24 findings: object rebinding the parent, contravariant slots, negative
  recursion read as 0, enum GADT cases, undecided match cases, shadowed bottom pins, import reasons, named bounds,
  quality refactors into `types/SealedSlots`/`Traversals`/`Trees`, `Instantiations`) and round 2 (23 findings:
  bounds away from the parent, export paths, renamed parents, positivity through source/standard constructors,
  match patterns by declaration identity, rename hiding, term/type namespaces, a hidden cycle through `q.X`) — all
  accepted, each with a probe-confirmed regression (`StoredValueSoundnessSpec` 29, `ImportClausesSpec` 7).
- Landing: one textual conflict (`capacity/Reading.scala`, A2's `read(tpe, written)` vs C2's saturating probe) —
  union. One semantic interaction, found by the merged suite: B1's landing regression
  `HigherKindedBindingSpec.classBoundPosition` pinned `Bound[A: Tag]` / `Using[A](a: A)(using Tag[A])` to `?`
  because C1's estimate did not sum the generic family `Tag[A] = T1[A]() | T2[A]()`; C2 sums it, so both read
  1 · 2 = 2 (like the fixture's `B[A: Ev]`, `Ev[A] = Boolean`, = 2). To keep the example's purpose (position read
  where the binder is declared), added an object `R` whose family has a subtype through a renamed import: read
  in place both rows are `?` (estimate refusal); with `ContextBounds.written` disabled in a scratch build
  `R.Bound.<init>` read 1 while the truth is 2 (scalac: `given R.Mark[Int] = R.Z.M2[Int]()`). Interaction
  probes (A2 unions beside a refused family, B1 bounds over a summed contravariant family, R10 members beside the
  method side) gave sound results; no code change needed.

#### Expectations changed

- Owner-reviewed: **none** (`CanGet.get`, the five 0.16.0 counts, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec, plugin fixtures pass unchanged).
- At landing: `HigherKindedBindingSpec.classBoundPosition` `O.Bound.<init>` / `O.Using.<init>` `?` → 2 (derivation
  above and in `notes/C2.md` §Integration delta), plus the new `R` twin (`?`, renamed-import refusal).
- The lane's own (derivations in `notes/C2.md`): `CounterSpec`/`SizeSpec` lossy widths, `CardinalitySpec` domain
  recursion 0 → `?`, `ReportSpec`, `NamespaceSpec`, `NamespaceScopingSpec` (summed companion/partial families,
  followed imports/exports, R10/R11 reasons), `GroundEnvironmentSpec.explicitImport` `?` → ω,
  `EnvironmentSoundnessSpec` reason text.

#### Open items (`?`)

- ? Mutual recursion between generic families, a match type over another match type's result, absorption by an
  exact 0 beside an unknown, late-settling recursion through `&`, recursion through a value's type member even
  when positive, reasons stopping at the first refused name (`RecordKind` never reaches `FieldShape`).
- ? Bounded subtype parameters summed only beside their parent; an invariant family with a `Nothing`-pinned case
  object is refused (only covariant bottom pins read); spelled match patterns across scopes stay stuck.
- ? Imports after a wildcard over an unsupplied namespace are not followed even to supplied eo classes
  (`JsonPrism` under `scala.jdk.CollectionConverters.*`) — A3's stdlib model can lift it.
- ? Method side's own family reading (`Representations`, lane types) still reads renamed parents, shadowed bottom
  pins and shadowed data constructors by spelling outside constructor rows; instance paths `x.M` to abstract
  members (B2).
- ? Misleading second reason `qualified member environment not resolved: <unrelated object>`
  (`Measurement.scala:546`, pre-C2) on sound `?` constructor rows in a template with an abstract member — A4 / C3.
- ? CodeScene not runnable here.

### A3 — Standard-library model, sequences and honest absence reasons (landed)

- **Landed:** `2cc8de2` on integration `ccr-d57d9679-dtp3fh` (previous `7ab0a74` = A1 + B0 + C1 + A2 + B1 + C2),
  compare-and-swap after rebasing the reviewed lane commit `6612257` (fix round 2, base `4db1023`). Gates:
  `runs/2cc8de24838aaac8eeee62ca40b7abfc4b261a27` — test 49/49 suites, scalafmt, scalafix, file metrics, EoCheckout PASS; CPD 6 (= integration head 6).
  Details: `notes/A3.md` (rules, two review rounds, "Integration delta").
- **Rule:** declarations the sources do not supply are read only through `StdlibModel` (`stdlib-model/1`, Scala 3.9.0
  on 2.13 collections, keyed by FQN): tuples/`Some`/`Left`/`Right`/`EmptyTuple`/`NamedTuple` products, `Option`/
  `Either` sums, `FunctionN` arrows, `List`/`Vector`/`IndexedSeq`/`Seq`/`ArraySeq`/`Array` (non-rigid element) finite
  sequences; single-pass, equality-built and reflective types excluded; throwables, `Product`, Java collections
  unmodelled. Consulted only where the root imports bind the name (any unsupplied wildcard refuses every builtin and
  model name; `scala.quoted`, `quotes.reflect`, `CollectionConverters` listed). Absence says so:
  `declaration not supplied: <fqn> (<binding>)`. Open owners report foreign parents. Supplied opaque-element
  sequences are eliminated by length (SE: c(0)=0 → 0, one-valued goal → 1, c(0)≥1 ∧ c(k)≥2 for a nonzero k ≤ 2 → ω,
  else `?`; X2 applied); receivers only their own scope builds are not read. Derivation:
  `docs/research/stdlib-model.md`, register D16 (model) and D17 (SE), `design/stdlib-foreign.md`.
- **Landing (merge with B1/C2):** one builtin vocabulary — `BuiltinTypes.canonical` = model FQN or `scala.<scalar>`
  for override, evidence and B1's nominal keys; B1's key and kind readings go through A3's `ShadowedBuiltins`
  (`denoted` / `importedKey`, same refusals as the shape reading), so named imports of model paths key and apply;
  B1's narrow `import java.lang.*` guard is superseded by the general wildcard refusal; an unapplied entry in a
  higher-kinded position is B1's operator; excluded/unmodelled entries hold no parameters (no key, callee reason alone).

#### Measured delta (vs 7ab0a74)

Headline **108 finite · 47 ω · 1484 ? → 144 finite · 91 ω · 1404 ?**; **80 rows `?` → number**, none lost or changed;
946 reason-only; stored values unchanged (435 rows). Three-way (base `4db1023` / lane `A3-fix2` / head / merged):
the lane's 79 count changes land with the lane's numbers (e.g. `AvroBinaryCursor.zigZagInt` ω, `AvroWalk.rebuildPath`
1, `terminalOf` ω); B1's 12 numeric rows unchanged. One interaction row: `MultiFocusFromList.fromList[A](xs:
List[A]): F[A]` ? → **0** (B1 removed `bounded or higher-kinded parameter F[_]`, A3 removed `unresolved type: List`;
F rigid, nothing builds an F\[A\] from Nil; the concrete sibling `fromArraySlice` delegates to the target and is no
second producer, per the reviewed `CanPlace.place` convention; regression `SequenceEliminationSpec.rigidGoal`).
Main reason movements: `unresolved type` −1596 occurrences / `declaration not supplied` +954 (521 rows read the
model names B1 uncovered; foreign callees read first by B1 now in the absence wording), `receiver fields constrained`
+258, `outside the counting model` +199, `inherited parent declaration not supplied` +109; the lane's 42
`higher-kinded argument` refusals give way to B1's operator reading; the lane's 45 false circe
`Encoder (not declared …)` reasons disappear.

#### Review findings and resolution

- Lane rounds (before landing): round 1 (19 findings: FunctionN/arrow keys, singleton aliases of sequence-bearing
  products, model parents under unsupplied wildcards, listed namespaces by written prefix, `scala` subpackages, SE on
  private-constructor receivers, reasons, Array element readings, quality) and round 2 (15: receivers of
  private/protected/local classes, Arrays of rigid singletons, listed wildcard precedence, `Integer` final, one
  `Spelling` rule, tests, docs) — all accepted with probe-confirmed regressions.
- Landing: 11 conflicting files (the shared builtin vocabulary, `Found`, `ShadowedBuiltins`, `MethodAnalysis`,
  `Measurement`, `ConstructorPositions`, `OverrideIdentity`, `TypeIdentities`, `RuntimeClasses`,
  `SourceLookup`, a spec, the register) merged as above. The merged suite found 6 failing examples (reason wording,
  B1's java.lang pin, A3's unapplied pin), all re-derived. Delta review found: (1) the `fromList` interaction —
  derived 0 under the reviewed convention; a guard refusing overridable concrete polymorphic members (X5) was tried
  and rejected because it flips the owner-reviewed `CanPlace.place` 1 → ?; (2) a false
  `higher-kinded parameter used as a proper type: P` on 5 rows (`P[_]: Type`: `scala.quoted.Type` takes AnyKind) —
  fixed by holding no parameters for excluded/unmodelled entries; (3) CPD 8 → 6 (generic reading signatures, a case
  order, a repeated refusal in `StdlibModel.arity`).

#### Expectations changed

- Owner-reviewed: **none** (`CanGet.get` 0, `CanGetOption.getOption` 1, `CanPlace.place` 1, `CanPlace.transfer` ω,
  `CanModifyP.replace` 1, EoCaseLedgerSpec, OpaqueTypeScopeSpec, ReferenceInhabitantsSpec, ArticleCardinalitySpec).
- Other: `HigherKindedRefusalSpec.boxedNames` `int` 1 → ? (general wildcard refusal; true 1) and reason wording;
  `HigherKindedBindingSpec.foreignCallee/foreignBound`, `GeneralInfixApplicationSpec.externalEffects`,
  `NamespaceScopingSpec.groundSlot` reason wording; `StdlibModelSpec.unapplied` re-pinned to the operator reading
  (`Box[Either]` 4, `One[Option]` 27); new `KindedIdentitySpec.modelKeys`, `SequenceEliminationSpec.rigidGoal`. The
  lane's own changed pins are listed in `notes/A3.md`.

#### Open items (`?`)

- ? X5 (overridable concrete members of open classes as open capabilities) vs the reviewed `CanPlace.place`
  convention: `MultiFocusFromList.fromList` = 0 and `CanPlace.place` = 1 move together if X5 wins (A4/B3, owner).
- ? `import java.lang.*` refuses every builtin (`Int` included, true 1) until java.lang's members are listed.
- ? Excluded/unmodelled entries are no nominal keys (`F[Map[K, V]]`, `F[Throwable]`).
- ? Unlisted-namespace wildcard vs another file's package member (D11/X1 on the method side) — A4.
- ? SE beyond three sequences / lengths ≤ 2; sequences in sums, callable results, built functions, existentials.

### B2 — Abstract type members, stable paths, refinements and stuck projections (landed)

- **Landed:** `ceccb6d` on integration `ccr-d57d9679-dtp3fh` (previous `2cc8de2` = A1 + B0 + C1 + A2 + B1 + C2 + A3),
  compare-and-swap after rebasing the reviewed lane commit `e4aa88f` (fix round 2, base `bda05e1`). Gates:
  `runs/ceccb6dddc9b9a01c549a3157024e87aa8025872` — test 52/52 suites, scalafmt, scalafix, file metrics, EoCheckout
  PASS; CPD 5 (integration head 6). eo output byte-identical to the pre-format pass `runs/d7829f62…`. Details:
  `notes/B2.md` (rules, two review rounds, "Integration delta").
- **Rule:** a type member is read on a stable path (`this` of the innermost template, `C.this`, a self alias, a
  parameter that is neither by-name nor `var`, a choosable `val`, `q: p.type` = `p`; a name is its innermost binding).
  Along the path's linearization the first alias is the meaning, read where declared with `this` still the path, so a
  parent's declaration reads its receiver's members (M2; `Optic.to` in `PickFold` is `S => Affine[(Unit, Unit), A]`);
  a member every level leaves abstract, unbounded and first order is the rigid atom `member:<path>#N` (parity with a
  type parameter; a parameter is an existential package the caller opens — Mitchell–Plotkin). A type-only template with
  an instance has one value; a stable input's type-only refinement is its base only where some base value may have
  it. Match types keep written arguments (MT1′), `case _` reduces (MT2), a single-case tuple projection of a rigid
  atom is the atom `proj:n:i:(α)` when one instantiation reduces every projection on α (MT3). Gate G1: a final root's
  inherited declaration another level implements is that level's code; G2 is the sealed-receiver gate. Refused with
  named reasons: bounded / higher-kinded / class members, `R#X` of abstract members, unsubclassable or closed
  templates (final, value class, object, given, private constructor, sealed, private/local scope), values fixed by
  their definition, unsupplied parents, unshown refinements, projections without a common instantiation or with type
  evidence. Derivation: `docs/research/type-members.md`, register **D18**, `design/members.md` (M0–M5, MT1′–MT3,
  G1/G2).

#### Measured delta (vs 2cc8de2)

Headline **144 finite · 91 ω · 1404 ? → 152 finite · 95 ω · 1392 ?** (core 87 → 95 finite, 17 → 21 ω); **12 rows
`?` → number**, none lost or changed: `Affine.Miss.<init>` 1, `Affine.Hit.<init>` 1, `Affine.map.map` 1,
`PartialAccessor.affineAccessor.getOption` 2, `Optic.to` 0, `Optic.from` 0, `Miss/Hit.hashCode` 2^32,
`Miss/Hit.toString` ω, `SimpleLens.place` ω, `SimpleLens.transfer` ω (each derived in `notes/B2.md`); 123 reason-only;
stored values unchanged (435 rows). Four-way (base `bda05e1` / lane `B2-fix2` / head / merged): 12 of the lane's 14
moves land with the lane's numbers; no head number moved; no row the head left alone differs from the lane. The two
others are the interaction: `Optional.FromFunctions.getOrModify` and `Optional.Fused.rebuild` (lane 1 each) stay `?`
under A3's constrained-receiver rule (`final private[optics]` classes: their factories decide which receivers exist),
now with that as their only reason. Main reason movements: lose `abstract … Optic.X` 58, `refined representation
unavailable` 29, `inert match type` 27, `abstract … Optional.M` 16, `nominal identity of …` 10, `polymorphic capability
binder` 9, `unresolved type: rc.Out` 7; gain `abstract … optics.Optic` (record reason, B3) 28, `opaque representation
… MultiFocusK` 11, `higher-kinded abstract type member rc#Out` 7, `unsupported type: context: X` 4 (A5's named tuple).

#### Review findings and resolution

- Lane rounds (before landing): round 1 (24 findings: outer `this`, shadowed self alias, class members, value classes
  and private constructors, unsatisfiable refinements, private aliases, G1 `var`, spelled atoms, the closure-scope gap,
  quality) and round 2 (20: refinement bodies read as Scala types them, types-only templates needing an instance, one
  `TemplateClosures` enumeration incl. ancestors' private constructors and private/local scopes, fields read through
  an alias at their most derived declaration, evidence on stuck projections, open sealed families, term refinements,
  spelled override keys, quality) — all accepted, probe-confirmed, pinned in `TypeMemberSpec` /
  `TypeMemberBoundarySpec` / `TypeMemberScopeSpec`.
- Landing: six files conflicted, all textually (C2's `SealedSlots` / `unnamedInstances` under the lane's
  `closedLeaf` / `openSubtype`; A3's `Resolver` additions carried into the moved `Resolver.scala`; A3's
  `ForeignParents` beside the lane's grouped ancestors; A3's `lexicalType` arms fed `Arguments`; register D18 kept).
  Suite green on the first merged compile; scalafmt fixed two merge lines; a `Frame.isTemplate` tidy-up in A3's
  `ParameterScope` was reverted (it pushed the file over the 25 % comment warning). Delta review: 12 rows break the
  reason union rule, all explained and kept — `ModifyF.<init>` shows the estimate's own `Fst`/`Snd` wildcard refusal
  again because the method side now reads the position (A3 hides it only where the method side refuses), 7 Optional
  rows report A3's environment error instead of the lane's solver limits, 4 `JsoniterPrism` rows lose the
  `inert match type` A3 had surfaced (`X` is a concrete tuple alias, MT1′ reduces it). Interaction probes
  (`probes/b2land/i1`–`i5`): member aliases under unsupplied wildcards, unsupplied parents, `Array` of a member atom
  (A3's ClassTag refusal sees B2's atoms), projections inside sequences, private receivers — all sound.

#### Expectations changed

- Owner-reviewed: **`EoCaseLedgerSpec.optic`** `Optic.to` `?` → **0** (the lane's; derivation in `notes/B2.md`:
  `F` rigid (B1), `X` the atom `Optic.this.X`, nothing yields `F[X, A]` — the reviewed `CanGet.get = 0` shape). None at
  landing; `CanGet.get` 0, `PartialAccessor.getOption` 1, `Direct.apply` 1 unchanged.
- Other: the lane's (`MethodMatchProjectionSpec.inert` → 0, `HigherKindedRefusalSpec.members`, `EnvironmentSoundness
  Spec.inheritedOverride` → 2, `GroundEnvironmentSpec` / `NamespaceSpec` path-type agreements, the invalid
  `AliasOverrideIdentitySpec.abstractMember` fixture replaced, five fixtures given a term member). None at landing.

#### Open items (`?`)

- ? `Optional.FromFunctions.getOrModify` / `Fused.rebuild` (true 1 if every field value is admissible): needs factory
  reading (`Optional.apply` / `fused` pass their arguments through) — not in the model.
- ? Bounded and higher-kinded members, `R#X` projections, nested or bounded stuck projections, opaque types outside
  their scope as atoms (O1, gated by OpaqueTypeScopeSpec), refinements of type parameters.
- ? The capability-record reason now on 28 refined `Optic` inputs (B3); `context: X` named tuple (A5); solver limits
  on opaque sum-producing callables and higher-order application reached by 2 + 1 rows.
- ? Root's own `var` still `mutable capture` until A4's D-var; `GeneralInfixApplicationSpec.products` NPE seen once in
  the lane's first submission, never reproduced (not seen at landing).
- ? CodeScene not runnable here.

### C3 — Report hygiene: one-line fields, blocker categories, definitions TSV, EoDelta (landed)

- **Landed:** `b5045b8` on integration `ccr-d57d9679-dtp3fh` (previous `ceccb6d` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2), compare-and-swap after rebasing the reviewed lane commit `8811f0e` (fix round 2, base `7ab0a74`). Gates:
  `runs/b5045b8901a5dc5f0ee32d0c59cfd16dc67bc531` — test 54/54 suites, scalafmt, scalafix, file metrics, EoCheckout
  PASS; CPD 5 (= integration head 5). Also `sbt plugin/scripted` (library, report, selected) and `EoBaseline` on the
  pinned 0.16.0 jar ("Five reviewed model counts and input-order checks passed"). Details: `notes/C3.md` (rules, two
  review rounds, "Integration delta").
- **Rule (no counting decision):** every artefact field and reason is one line spelled from the source's tokens
  (`types/Spelling`, `TokenLayout`: statement breaks inside braces read `; `, braceless `match` in braces, clause by
  clause); transparent-alias placeholders are spelled back; `; ` is reserved as the reason-list separator outside
  braces and literals; blocker categories cut at the first `: ` outside brackets/literals not ending an operator, code
  words read as `_`, producers name their subject after the separator, unions as `union (A | B): <reason>`
  (`reporting/Blockers`); locations input-relative, jar entries by entry name (`reporting/Locations`); a capacity prints
  as the bound `≤2^b` (lossy `≤2^b (lossy)`), per `docs/type-arithmetic.md` (now `docs/site/arithmetic/counting-data.md`); stored-value members of class bodies are
  `O#T`; `eo-checkout-definitions.tsv`; EoCheckout input-order and identity checks; `cardinality.EoDelta` per-row run
  join. Derivation: `design/stored-value.md` §H / §6.

#### Measured delta (vs ceccb6d)

Headline unchanged **152 finite · 95 ω · 1392 ?**; **0 count changes** (none expected); EoDelta: methods `+0 -0 ~106`
all reason-only — 54 unions parenthesized (57 items), 25 rows / 48 shadowed-builtin subjects after the separator, 15
sealed-subtype, 3 recursion (9 items), 3 receiver subjects, 6 `Indexed` placeholders restored (0
`$cardinalityProjection` left); stored values `+0 -0 ~21` = 15 cells re-rendered as bounds (12 capacities, 3 lossy) +
6 rewordings, 0 class moves — exactly the lane's. Blockers TSV 71 → 44 categories (16 union lines → 5, 16 per-name
builtin lines → 2, receiver/subtype/recursion subjects out of the head, `rc#Out`-style member paths → `_`); per-signature
tallies unchanged. The gate's `eo-delta.py` shows the 403 identity re-spellings and 435 relocations as `+403 -403 ~70` /
`+435 -435` (raw-text keys), as in the lane. Against the lane's own delta (vs `7ab0a74`, 144 rows): the 31 separator
rows are gone (B2 resolves those match types), shadowed-builtin rows 39 → 25 (A3 lists `CollectionConverters`; refuses
per name under `_root_.kyo`, `cats.syntax.*`, `q.reflect`), unions 56 → 54 (4 lane rows lost their union reason to
A3, 2 gained one through B2's stuck projections), 3 receiver rows new (`Affine.fold/aFold`, `PSVec.slice`).

#### Review findings and resolution

- Lane rounds (before landing): round 1 (17 findings: operator colons in categories, class/companion identities,
  namespaces in category heads, constructor-disagreement widths, newline separators, literal spacing, placeholder
  boundaries, TSV fields, invalid fixtures, absolute locations, quality) and round 2 (19: union categories, `_:` colon,
  lossy bounds, refinement `; ` in TSV lists, placeholders in literals, leading prefix operators, `#` marks, pattern
  names, error paths, 32 raw `.syntax` producers, subjects after the separator, braceless match, curried clauses, test
  pins, blockers delta, `Locations`) — all accepted, probe-confirmed, pinned in `ReportHygieneSpec` / `EoArtifactsSpec`.
- Landing: 10 conflicting files (`Measurement`, `MethodAnalysis`, `BinderBounds`, `InheritedTypeMembers`,
  `MethodProjections`, `ShadowedBuiltins`, four specs), merged keeping both sides: B2's `frozen` now records a spelling
  (a stuck projection spells back as its match type); A3's `route` detail kept beside C3's refusal wording, one `why`
  helper for the self-reaching doubt; `StdlibModel.Spelling` vs `types.Spelling` name clash resolved by a renamed
  import. C3's rules extended to the head's new producers: B2's raw `.syntax` reasons → `Spelling.of` (TypeMembers,
  RefinedMembers, ConstructorApplications), B2's copy of `abstract refined member X has non-exact bounds` → C3's
  wording, A3's `top-level package P outside the sources` → package after the separator. Suite: 3 failing specs on
  the first merged run (B2's two `estimate 2^8` pins, `TopTypesUnionSpec.foreignSelf` wording, and
  `ReportHygieneSpec.unionAlternatives`, whose open-trait fixture B2 now reads as one value — fixture moved to
  abstract classes with a method, assertions unchanged). CPD 6 → 5: C3's `Regex` import split a 4-file import-header
  duplication; `Pattern.quote` without the import. Stale reason quotes in five research pages re-quoted.

#### Expectations changed

- Owner-reviewed: **none** (`CanGet.get` 0 and the five 0.16.0 counts pass; EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec unchanged; plugin fixtures as the lane updated them, passing).
- Other (wording only, same values): A3/B2 pins in `GroundTypesSpec`, `GroundEnvironmentSpec`, `StdlibModelSpec`,
  `TypeMemberSpec`, `TopTypesUnionSpec`, `HigherKindedRefusalSpec`, `NamespaceSpec` (`≤2^32`); the lane's own list
  is in `notes/C3.md`.

#### Open items (`?`)

- ? B2's member reasons keep the member path / owner in the category head (`abstract type member X of O is fixed`,
  `stuck match types on A …`, `path through member X of refined p`): categories stay whole while the path is code
  (capital, `#`, `.this`); a lower-case owner or parameter label would split one. None in eo.
- ? Lane limits stand: exact powers print as bounds; class method and companion method share one method identity
  (method-side owner naming, `O#f`, left to the identity owner); indentation regions other than braceless `match`;
  backquoted names holding `.`/`#`; `Definition.size` placeholder ω for `?` rows; gates.sh still uses `eo-delta.py`
  (switch documented in `notes/C3.md`).
- ? README lines quoting `2^32`-style capacities and old header text (listed in `notes/C3.md`) — orchestrator's pass.
- ? CodeScene not runnable here.

### A4 — Environment completeness: outside producers, mutable state, exports, inferred results (landed)

- **Landed:** `e039cd6` on integration `ccr-d57d9679-dtp3fh` (previous `b5045b8` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3), compare-and-swap after rebasing the reviewed lane commit `4029d97` (fix round 2, base `ceccb6d`). Gates:
  `runs/e039cd618666f5d64b1bfdbf21a527facead8137` — test 56/56 suites, scalafmt, scalafix, file metrics, EoCheckout
  PASS; CPD 3 (integration head 5). Also `sbt plugin/scripted` (library, report, selected) and `EoBaseline` on the
  pinned 0.16.0 jar ("Five reviewed model counts and input-order checks passed"). Details: `notes/A4.md` (rules, two
  review rounds, "Integration delta").
- **Rule:** (1) concrete code nothing can override — module/package members, locals, givens, export forwarders,
  members of final templates, `var`s — adds no implementation over readable inputs and goal (a closed in-model term,
  D-env-concrete restricted by X5); `qualified member environment not resolved`, `ConcreteAtoms`,
  `mentionedConcrete` gone; readability stays enforced by the existing refusals. (2) D-var: a `var` holds its value
  (parameter, initializer, declared slot; implements an inherited declaration anywhere, E164); `mutable capture`
  retired. (3) X1 method side: an export defines members of its template, ranks with own declarations (hides outer,
  beats an import of its body), followed or refused, never falls back; ancestors' exports refuse only the names their
  clauses define; a path through a value is refused. (4) Own definitions (pattern variables included), extension
  methods, exports, imports and inherited members hide or make ambiguous a local capture (E049). (5) Given selectors
  bind no type, unimports leave the wildcard, by-name `=> A` reads `A`. (6) D-inference tier 1: overriding member
  (one matching supplied declaration, no foreign base that may intersect), literal, non-singleton ascription, `new`,
  creator application with type arguments, call of the one declared method the nearest binding reaches (widened as
  scalac); tier 2 not done. Derivation: `docs/research/environment-completeness.md`, register D19 (D10/D11
  reworded for X1), `design/environment.md` §§2–5.

#### Measured delta (vs b5045b8)

Headline **152 finite · 95 ω · 1392 ? → 158 finite · 98 ω · 1383 ?**; methods `+0 -0 ~197`: **9 rows `?` →
number**, none lost or changed — `eitherAccessor.getOption` 2, `Affine.Hit/Miss.equals` 2, `Span.isHit` 2^(2^64),
`PathParser.isIdentStart/isIdentPart` 2^(2^16), `JsonPathScanner.matches/stringEqualsAscii` ω, `docs.Phone.<init>` ω
(each re-derived in `notes/A4.md`); 188 reason-only; stored values unchanged. Exactly the lane's 197 rows and values.
Four-way check (base `ceccb6d` / C3 / lane / landed): 114 C3-only rows read as on `b5045b8`, 184 A4-only rows as on
the lane, 13 rows changed by both combine both sides and nothing else (4 keep C3's `union (…)` / shadow wording with
A4's removals; 9 `kyo.RecordIsoMacro` rows take A4's `path through a value not followed: q` in place of C3's re-worded
`q.reflect` refusals). Main reason movements (blocker categories, 44 → 43): `qualified member environment not
resolved` −153 rows, `mutable capture` −17, `builtin name _ … not resolved` −9, `path through a value not followed`
+13, `local capture may be hidden by an import` +1, `inferred result type not resolved` 13 → 7, `declaration not
supplied` 624 → 617, `unresolved type` 24 → 20.

#### Review findings and resolution

- Lane rounds (before landing): round 1 (29 findings: shape-only override matching, private parent members,
  intersected foreign/implicit parents, receiver-less overridden reading, inherited/imported/exported call
  bindings, capture hiding (E049), value-headed export/import prefixes, protected inherited members, foreign
  unimports, singleton aliases, X5 rebuttal/deferral, CPD, quality, test gaps) and round 2 (17: pattern-bound
  definitions, abstract `var`/`given` override candidates, call-result widening, `new` outside the sources, named
  exports refusing every inherited name, export reasons by string replacement, CPD, quality, X5 pins) — all
  accepted (X5 deferred to B3 with `overridableMembers` pending), probe- and compiler-fixture-confirmed.
- Landing: 6 conflicting files (`Measurement`, `SingletonIntersections`, four specs) merged keeping both sides.
  C3's reason rules extended to A4's producers: `Spelling.of` for the export prefix, literal, `new`-of and
  outside-parent spellings; two export reasons put their subject after the separator (`export not followed: N
  (export of p)`, `wildcard export not supplied: N (wildcard export of p)`; no eo row carries them). First merged run:
  2 failing specs, both interactions, re-derived: `GroundEnvironmentSpec` `a.b.UsesS/UsesT.<init>` print `≤2^8` (A4
  takes the estimate's value space; C3 prints a capacity as its bound — same value 2^8), and
  `TopTypesUnionSpec.foreignWildcard` `E.f` now refused by the lookup's wildcard-export reason before C3's builtin
  walk wording (both sound; both channels pinned). C3's capacity wording applied to A4's two `estimate 2^32` pins.

#### Expectations changed

- Owner-reviewed: **none** (`CanGet.get` 0 and the five 0.16.0 counts pass; EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec unchanged; scripted fixtures pass).
- Other (lane, derived in `notes/A4.md`): `MethodAnalysisSpec.concreteProducers` ? → 4; `EnvironmentSoundnessSpec
  .varParameter` (B0 interim) ? → 2 / 1; `NamespaceScopingSpec` `p.O2.U.<init>` → 2, `a.b.UsesT.<init>` → 2;
  `GroundEnvironmentSpec` imported/exported inherited `Int` rows ? → 4 / 1, three disagreements → 2^8 / 2;
  `TopTypesUnionSpec` given-selector rows ? → 1; `GroundTypesSpec.mutable` ? → 2^32; reason-only pins in
  `StdlibModelSpec`, `HigherKindedBindingSpec`, `GroundTypesSpec.exported`. Landing: the two interaction pins above.

#### Open items (`?`)

- ? X5: overridable concrete members of open classes are still skipped (`ow.Box.target` 2, true 3) until B3 lands;
  `EnvironmentCompletenessSpec.overridableMembers` is `pendingUntilFixed` and will flip when it does.
- ? Inferred results beyond tier 1 (`inferred result type not resolved: <form>`: tuples, lambdas, blocks, calls
  needing argument-type inference, selections, union/singleton ascriptions, calls an import/export/parent may bind).
- ? Ancestors' exported aliases refused, not followed (`inherited type member not resolved: T (exports of P)`, true
  count 4 in `expmem`); B2's `TypeMembers` closure check does not see exporting ancestors (no eo row).
- ? `Optional`'s free `M0` (15 rows) and `Slice`'s hidden constructor (23 rows) stay exact refusals.
- ? CodeScene not runnable here.

### B3 — Capability records, member classification and override identity (landed)

- **Landed:** `ad931e2` on integration `ccr-d57d9679-dtp3fh` (previous `e039cd6` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3 + A4), compare-and-swap after rebasing the reviewed lane commit `419f079` (fix round 2, base `ceccb6d`).
  Gates: `runs/ad931e29d72c711bb4e41591b78f4dea9140ae02` — test 58/58 suites, scalafmt, scalafix, file metrics,
  EoCheckout PASS; CPD 3 (integration head 3). Also `sbt plugin/scripted` (library, report, selected) and `EoBaseline`
  on the pinned 0.16.0 jar ("Five reviewed model counts and input-order checks passed"). Details: `notes/B3.md`
  (rules, two review rounds, "Integration delta").
- **Rule:** a value of a source class or trait is the record of its accessible observations (C0–C4: universal members
  excluded; open trait / open class with public `val` parameters = product of open members and fields; final class
  with a public constructor = product of its observable parameters; `final abstract` = empty sum); on the measured
  declaration's receivers, overridable concrete members are capabilities and derived ones concrete code, over the
  Scala linearization (C5, X5, X6); members whose default definitely evaluates the declaration are dropped as views,
  possible views are a reason (C6); a Unit-building member is irrelevant (C7 exact case); override identity is
  matched as seen from the receiver through every level, by the scope-correct lookup (C9). Derivation:
  `docs/research/capability-records.md`, register **D20** (D19 on the lane; A4 holds D19), `design/capabilities.md`.

#### Measured delta (vs e039cd6)

Headline **158 finite · 98 ω · 1383 ? → 183 finite · 98 ω · 1358 ?**; methods `+0 -0 ~793`: **27 `?` → number, 2
number → `?`**, 764 reason-only; stored values unchanged. The lane's 25 count changes land with the lane's numbers
(23 `? → n`: Getter/Review/BijectionIso/Optic.id/AffineFold/Optional.readOnly/Modify/MendTearPrism/ModifyF.map.map,
reconstructSingleton 0, generics.lens, Chimney.iso/prism 0, AffineFoldLaws.af, six Composed*Laws members; 2 `n → ?`:
`Optic.to`/`Optic.from`, possible view through the overridable `andThen`). Four new numbers are interactions with A4,
each re-derived: `Iso.apply` 1 and `Prism.pOptional` 2 (A4 types the inferred result; B3 reads the final class),
`ComposedGetLaws.composedGet` 2 and `ComposedReverseGetLaws.composedReverseGet` 2 (A4 dropped the qualified flag; B3
reads the `CanGet`/`CanReverseGet` records). Four-way (base `ceccb6d` / lane / `e039cd6` / landed): 616 lane-only rows
read as on the lane but 18 in C3's wording (12 unions parenthesized, 4 inert match types, 1 sealed subject, 1
builtin-shadow wording); 172 rows changed by both sides combine both (159 exactly; 13 explained: wording, solver
limits reached once both reasons go, the two numbers). Main reason movements (blocker categories 43 → 53):
`abstract type or method-valued representation` 678 → 12, `inherited override identity not resolved` 42 → 1; new
`capability record with abstract type member not read` 278, `overridable member may re-enter the measured
declaration` 135, `inherited declaration identity not resolved` 11, `open class with hidden constructor state` 10;
`polymorphic capability binder` 41 → 172, `receiver fields constrained by a non-public constructor` 209 → 272,
`declaration not supplied` 617 → 646, `Elimination of an opaque sum-producing callable` 21 → 39.

#### Review findings and resolution

- Lane rounds (before landing): round 1 (26 findings: same-name overloads, enclosing receivers, nested re-entry,
  η-expansion and right-associative operands, `Outer.this`, file-closed classification, one member counted twice,
  givens, parent state, universal members, `InheritedClosedSlots` by name, quality) and round 2 (18: linearization
  diamonds, refined restatements, context-function parameters, by-name extensions/conversions/`==`, SAM lambdas,
  implicit search, `super`, file-closed mixins and pattern vals, body `var`s, tentative keys, quality) — all accepted,
  probe- and scalac-run-confirmed, pinned in `CapabilitySpec` / `TargetViewsSpec`.
- Landing: 8 conflicting files (`Measurement`, `InheritedClosedSlots`, `FinalImplementations`, `InheritedScopes`,
  `TypeOperators`, `GroundEnvironmentSpec`, `NamespaceSpec`, the register) merged keeping both sides; A4's `var` reading
  carried into the moved `CaptureAliases`; A4's pattern binders (`boundNames`) used by B3's `MemberSyntax.defined`;
  `shape`/`parameterNames` kept once (`MemberSyntax`); register entry renumbered D20, A4's D19 and
  `environment-completeness.md` updated (X5 now classified: `ow.Box.target` 3). Two merge duplications removed (CPD
  5 → 3). First merged run: 3 failing specs, all interactions — A4 fixtures with open classes now read their siblings
  as capabilities (fixtures made `final`, open twins pinned: `Outer.D.g`/`h` 2^8), A4's `J.mk`/`J.mk2` creator
  applications read as B3's final record (refusal → 1), C3's union fixture moved to case classes with a body.
- Landing findings (pre-existing on the lane, found probing A4's exports against B3's insertion guard), both fixed and
  pinned (`TargetViewsSpec.importedExtension`, `.blockImport`; scalac 3.9.0 run fixtures `probes/b3land/fix`):
  (1) the named-import guard of the view rule was dead — the lookup was asked without a position, so no import was
  seen: a file- or template-imported by-name extension made `F.target`/`T.target` read **4** (true 2^8); now
  positioned, and an unfollowed import/export counts. (2) block-local imports were no channel: `W.target`/`N.target`
  **4** (true 2^8); a block's named import now brings its names, a wildcard/given import any insertion. eo: no count
  moves; one already-`?` row gains reasons (`MultiFocusLaws.multiFocus`, named import of supplied extensions read as
  possibly by name — imprecise, sound).

#### Expectations changed

- Owner-reviewed: **none** (`CanGet.get` 0, `CanPlace.place` 1, `CanModifyP.modify` 0, `MultiFocusSingleton.singletonTo`
  0; the five 0.16.0 counts pass; EoCaseLedgerSpec, OpaqueTypeScopeSpec, ReferenceInhabitantsSpec,
  ArticleCardinalitySpec unchanged; scripted fixtures pass). `EoCaseLedgerSpec.optic` keeps its reduced trait at 0;
  the eo `Optic.to`/`from` rows go 0 → `?` (the 0.16.0 ledger's "reviewed obligation").
- Other: the lane's (`MethodAnalysisSpec` fixtures made final + `openMembers`, `packagePrivateType` 1,
  `GroundEnvironmentSpec.openImplemented` 4 / `overloaded` 4, override-identity reasons, `BinderBoundsSpec.openBound` 1,
  `GeneralInfixApplicationSpec` 2, `TypeMemberScopeSpec.make` 0, `NamespaceSpec` `UsesY.<init>` 2). At landing:
  `EnvironmentCompletenessSpec.overridableMembers` pending → 3 (A4's X5 pin flips as designed), `exportOutranks` /
  `exportSelectors` / `abstractVariable` fixtures made final (open twin 2^8 pinned), `InferredResultsSpec.
  creatorApplications` `J.mk`/`J.mk2` → 1, `ReportHygieneSpec.unionAlternatives` fixture only.

#### Open items (`?`)

- ? **Soundness gap, not fixed:** a by-name conversion or extension in the companion of a type outside the sources
  (implicit scope of an expected type or a qualifier's type) is no channel the view rule sees — `fc.T.target` reads
  **4**, true 16 (`probes/b3land/fc`, scalac run). Needs expected types of typed positions, qualifier types and a
  stated stdlib-companion assumption; stated as open in `capability-records.md` and D20. For the integrated review.
- ? Records with abstract type members (278 rows), polymorphic open members (B4), possible views, self types, trait
  parameters, case classes with a body; C7 beyond the Unit case (needs a solver marker).
- ? `Prism.optional`, `Lens.first`/`second` (now typed by A4) stay `?` on the solver's sum-producing limit and an
  unexposed `SimpleLens.get` field.
- ? CodeScene not runnable here.

### A5 — Named tuples, literal types, context functions, compile-time evidence (landed)

- **Landed:** `382191a` on integration `ccr-d57d9679-dtp3fh` (previous `ad931e2` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3 + A4 + B3), compare-and-swap after rebasing the reviewed lane commit `0934f77` (fix round 2, base `e039cd6`).
  Gates: `runs/382191af7fb7050862671a978c723c992cfa6b45` — test 60/60 suites, scalafmt, scalafix, file metrics
  (two pre-existing comment-share warnings), EoCheckout PASS; CPD 3 (integration head 3). Also `sbt plugin/scripted`
  (library, report, selected) and `EoBaseline` on the pinned 0.16.0 jar ("Five reviewed model counts and
  input-order checks passed"). Details: `notes/A5.md` (rules, two review rounds, "Integration delta").
- **Rule:** (1) D20 named tuples: `(x: A, y: B)` is the product of its fields (SIP-58: opaque over its values, one
  value space); names are type identity (`NamedTuple[TupleN[name literals], TupleN[keys]]`); a match type is stuck
  on one (`match type stuck on a named tuple: …`). (2) D20 literal types: one-value singletons keyed by class and
  constant (floats by bits; a floating zero refused), dropped as inputs, read as closed one-point spaces; unions of
  literals are sums of distinct constants split by literal patterns, a class test separating classes (`1 | Int`
  refused, cooperative equality); a call whose type arguments make the result a literal type is widened, a value's
  singleton or a possible union holding one is refused. (3) D21 context functions (X4): `T ?=> R` reads as `T => R`;
  the result chain is followed exactly where Scala wraps (aliases, lambdas, operator closures, members as seen from
  the receiver, the overridden declaration), every unfollowed form refused by name; binders are contextual
  candidates one level deeper (anonymous evidence for nested definitions, D10 shadowing/ambiguity), and every
  candidate carries the results its `?=>` chain answers (implicit search applies `A ?=> R` where it wants `R`).
  (4) D22: every `inline` method (and every definition inside one) with a goal of more than one value is refused
  (`inline method: each expansion may inspect its call site's argument types, constants and givens (compile-time
  dispatch)`); `ClassTag`/`Manifest`/`TypeTest`/`Typeable`/`Mirror.*` are type-revealing evidence, outside the model.
  Derivation: `docs/research/syntax-features.md`, register D20–D22; fixtures `probes/a5/`, `probes/a5-fix1/`,
  `probes/a5-fix2/`, `research/xprobe/{cf,cf2}`.

#### Measured delta (vs ad931e2)

Headline **183 finite · 98 ω · 1358 ? → 159 finite · 93 ω · 1387 ?**; methods `+0 -0 ~128`: **1 row `?` → number**
(`ForgetfulFunctor.glass.map` 1, named tuple read), **30 numbers → `?`**, all `inline` methods (rule 4): 25 the lane
measured (`Direct.apply`/`.value`, `ForgetK`, `MultiFocus(K)`, `BijectionIso.modify/replace`, `Modify.modify/replace`,
`MendTearPrism.reverseGet`, `PickMendPrism.getOption/reverseGet`, `PickFold.getOption`, `widenLeft/Right` 1 or 2;
`SimpleLens.place/transfer`, `AvroWalk/JsonWalk.terminalOf`, `JsoniterPrism.scanRange` ω; `Span.isHit` 2^(2^64),
`PathParser.isIdentStart/isIdentPart` 2^(2^16)) and 5 B3 numbered meanwhile (`Getter.andThen`, `Review.andThen`,
`BijectionIso.andThen` 1; `Chimney.iso`, `Chimney.prism` 0); 97 reason-only; stored values unchanged. Four-way check
(base `e039cd6` / B3 / lane / landed): lane-only rows land as on the lane, B3-only rows do not move, 72 of the 79
rows both changed are the exact combination; the other 7 are an environment error stopping before a solver reason
B3 newly reached. The lane's `Indexed.at` ? → 1 does not land (B3's re-entry reason; correct, see below). Main reason
movements (blocker categories 54 → 53): `inline method` +120 rows (74 core), `inline parameter reads a top type` −5
(subsumed), `unsupported type` −5 (`context: X`), `Elimination of an opaque sum-producing callable` 39 → 29,
`Higher-order application is unsupported` 2 → 1.

#### Review findings and resolution

- Lane rounds (before landing): round 1 (18 findings: enclosing binders over-counted — now own levels and anonymous
  under D10; X4 through inferred overrides and substituted parameters; definitions nested in inline methods;
  ascriptions to context functions; infix/projection fall-through replaced by a whitelist; lambda and alias-parameter
  heads; `Mirror.*`/`Manifest` reasons; test gaps, quality, docs) and round 2 (11: context-function candidates
  answering their results (ambiguity Scala reports, fixtures cf4/cf6/amb2/amb4); literal type arguments not
  widened (`idc[x.type](x)` was 1, true 4); literal domains as closed spaces; named-tuple scrutinee reason;
  cooperative-equality docs; parameter lists, recomputed steps, test gaps) — all accepted, each reproduced by a
  probe or a Scala 3.9.0 fixture before the fix.
- Landing: 7 files both sides touched (`Measurement`, `MethodAnalysis`, `Resolver`, `InferredResults`,
  `OverriddenResults`, `ReportHygieneSpec`, the register) merged keeping both sides (B3's `MemberSlot` rename carried
  into the binder binding). Integration rows re-derived with new fixtures: `probes/a5land/integ.scala` (an inline
  `andThen` with a `summonFrom` body prints `20 20` / `20 0` with and without a call-site given; an inline
  `summonInline` `iso` exists where B3's scope-relative 0 said none) and `probes/a5land/indexed.scala` (a subclass
  overriding `Indexed.andThen` makes a second body of `at` print `6` vs `1`: true ≥ 2, so the lane's 1 would have
  been wrong on the B3 head; `?` is right until B4).

#### Expectations changed

- Owner-reviewed: **none** (`CanGet.get` 0, the five 0.16.0 counts, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec unchanged; scripted fixtures pass).
- Other (lane, derived in `notes/A5.md`): `TopTypesUnionSpec.transparentClosed` `T.s` 9 → ?, `T.o` 4 → ? (inline);
  `inlineTop` reason → the inline reason; `GroundTypesSpec.literalType` ? → 2^32; `PolyLambdaCardinalitySpec.evidence`
  reason (undeclared `Type`); `ReportHygieneSpec.literalRows`/`unionAlternatives`, `EoArtifactsSpec.literalFields`
  vehicles moved off counted literal types; `SyntaxFeaturesSpec.contextUnread`/`namedStuck` reasons. Landing: none.
- Reviewed by integration: B3's five inline numbers above (B3's own tests do not pin them).

#### Open items (`?`)

- ? Every inline method with a goal of more than one value (whether its inline family is countable is not modelled).
- ? `Indexed.at`/`modify`/`replace` (overridable polymorphic `andThen` may re-enter — B4's PB rules).
- ? Context-function candidates beside one their result may answer are refused even where the chain's parameters
  cannot be found (`a4`, `curried`: over-refusal, sound); shadowed/ambiguous enclosing binders refused, not dropped.
- ? Chains not followed (match types, projections, refinements, inherited aliases, partial applications, substituted
  or applied type parameters, paths, `ContextFunctionN` by name, budget); inferred results that are context-function
  calls or ascriptions; dependent function types; named-tuple match patterns; floating-zero literal types.
- ? Stored-value side: `NamedTuple.NamedTuple[…]` aliases stay `unresolved` (lane report).
- ? CodeScene not runnable here.

### B4 — Polymorphic capability members and callable-sum elimination (landed)

- **Landed:** `cd2c845` on integration `ccr-d57d9679-dtp3fh` (previous `fb5d936` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3 + A4 + B3 + A5 + the no-side-effects docs), compare-and-swap after rebasing the reviewed lane commit
  `010087f` (fix round 2, base `ad931e2`). Gates: `runs/cd2c845cf976b0c721c4cff8fb3b603b245cba0f` — test 62/62
  suites (head 60 + `PolymorphicMemberSpec`, `NeutralSaturationSpec`; every spec's example count = base + both
  deltas, 1662), scalafmt, scalafix, file metrics (two pre-existing comment-share warnings), EoCheckout PASS; CPD 3
  (integration head 3); `EoBaseline` on the pinned 0.16.0 jar ("Five reviewed model counts and input-order checks
  passed"). Details: `notes/B4.md` (rules, two review rounds, "Integration delta").
- **Rule:** a supplied member's own type parameters are read up to parametricity (capabilities C8, PB0–PB4): a binder
  its type never mentions is dropped when some type meets its bounds (a two-sided bound is refused); a binder met only
  in generators `P → M` and positively elsewhere becomes the sum of the generators' domains (Yoneda: `∀M. (P₁ → M) →
  … → G(M) ≅ G(P₁ + … + Pₖ)`; `∀C. C` is empty, so a receiver holding one is a vacuous environment, count 1); any
  other member stays `Shape.Poly`, instantiated only where matching its result against a goal fixes every binder, a
  count through such an instantiation reported only at 0 or 1 (PB2, coverage lemma restricted to members with no
  bare-binder result — crosscheck X7); unmatched results are irrelevant (PB3); undetermined binders refused (PB4).
  C10: a supplied callable with a sum or ground result is cased on once per argument tuple and path (structural
  neutral keys), over first-order argument spaces; unbounded or non-enumerated spaces refused by name. Restricted C11
  (crosscheck X3): ω beside reasons only through a reason-free, instantiation-free cycle whose rule children are all
  built without reasons and with the empty goal unprovable even optimistically at every node on the path. Deferred
  `inline` members are no terms of non-inline bodies (kept only as the receiver's existence condition). Evidence of
  two class types is apart when neither template is a base class of the other (every base supplied). Derivation:
  `docs/research/polymorphic-members-and-saturation.md`, register **D24**/**D25**, D10's evidence rule,
  `design/capabilities.md` C8/C10/C11; Scherer POPL 2017 and Scherer–Rémy ICFP 2015 cited metadata-verified (X8).

#### Measured delta (vs fb5d936)

Headline **159 finite · 93 ω · 1387 ? → 172 finite · 93 ω · 1374 ?**; methods `+0 -0 ~200`: **13 `?` → number, 0
number → `?`**, 187 reason-only; stored values unchanged. The 13 numbers are exactly the lane's (core optics:
`AffineFold.select` 4, `Optional.selectReadOnly` 4, `PickFold.to` 2, `BijectionIso.andThen(MendTearPrism)` 1,
`Optional.FromFunctions.<init>` 2, `MendTearPrism.andThen(BijectionIso)` 1 / `(PickMendPrism)(using ev)` 2,
`PickMendPrism.andThen(BijectionIso)` 2, `PickMendPrism.to` 2; data: `MultiFocusFromList.fromArraySlice` 1; laws:
`ComposedGetOptionLaws.composedGetOption` 64, `getOptionComposed` 2, `getOptionOuter` 1 — all 20 lane numbers are
re-derived in the notes). The lane's other 7 numbers (`MendTearPrism.modify` 1 / `replace` 2 / `getOption` 2 /
`andThen(MendTearPrism)` 1 / `fuseToMendTear` 1, `PickMendPrism.replace` 16, `PickFold.andThen` 2) do not land:
all are `inline def`s, which A5 refuses eagerly before the solver. Four-way (base `ad931e2` / lane / `fb5d936` /
landed, `b4land/fourway.py`): no row the head moved changes, no row moved by both, every reason follows the union
rule except three more inline rows (`PSVec.isEmpty`, `PickMendPrism.modify` / `andThen(PickMendPrism)`) that keep
only the head's inline reason. Main reason movements (blocker categories 53 → 55): `polymorphic capability binder`
172 → 33 (the eager form gone from 171 rows; 33 keep a bounded / higher-kinded / not-determined form), `Elimination
of an opaque sum-producing callable` 29 → 3, the ground-result category 2 → 0; new `sum elimination over an argument
space not enumerated` 15, `… unbounded argument space` 2, `anonymous evidence selection not resolved` 3 (Morph
laws); refusals the eager reason used to hide surface: `declaration not supplied` 646 → 694, `capability record
member may be the measured declaration` 8 → 16, `sealed subtype binds a parameter to no slot` 15 → 21, `refined
representation unavailable` 7 → 9, `open class with hidden constructor state` 10 → 11.

#### Review findings and resolution

- Lane round 1 (15 findings): deferred inline members read as capabilities (wrong counts `t` 4, `T4.m` 1 → 2 /
  refused by `DeferredInline`), two-sided phantom bounds ω (true 1; refused as bounded), string neutral keys merging
  distinct neutrals (2^32768 vs 2^(2^16); structured keys), register collision (D21/D22 → D24/D25), reason naming
  (`inner#0`), quality and test gaps — all accepted, probe- and scalac-confirmed, pinned.
- Lane round 2 (17 findings): template aliases read as the template's binder (`fc` 2, true 4; plus an
  order-dependent linearization memo — keyed by the path's arguments now), false overlap of unrelated traits
  (`Optic.cross`; base-class rule), the PB2 coverage lemma false for bare-binder results (rule restricted: beside
  such a member only ω and a proved-absurd context are reported), reasons named by the wrong segment (source names
  now), binder-free `Poly` paths refused as higher-order (now ordinary callees), quality and test gaps — all fixed.
- Landing: conflicts in `ContextualEvidence.overlaps` (A5's context-function results beside B4's base-class rule:
  `classes(…).getOrElse(results comparison)` — sound because `classes` answers only for supplied class/trait heads,
  which have no context-function results), `Measurement`, `Resolver`, `ClosedSpaces`, `MethodAnalysis`, the register
  and `top-types-unions.md`, all merged keeping both sides; the first rebased commit failed scalafmt only (two
  lines), fixed by amend. The merge swaps one CPD duplication for another (B4 removed the head's
  `Measurement`/`MethodAnalysis` one; `MemberSyntax.inlined` vs A5's `TransparentResults.inlined` is new): count
  3 = 3, left for A6's rewrite of `TransparentResults` (noted for that lander).

#### Expectations changed

- Owner-reviewed: **none** (`CanGet.get` 0, `CanGetOption.getOption` 1, `CanPlace.place` 1, `CanPlace.transfer` ω,
  `CanModifyP.replace` 1; the five 0.16.0 counts pass; EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec unchanged).
- Other (lane, each derived in the notes): `InheritedOverrideSpec.otherGeneric` ? → 1 (vacuous `∀C. C`),
  `OverrideIdentityImplementationSpec.foreignBinder` / `.nominal` ? → 1, `TypeMemberBoundarySpec.closureImports`
  ? → 1, `TopTypesUnionSpec.callableSum` ? → 27, `EnvironmentCompletenessSpec.opaqueCallable` ? → 4,
  `EnvironmentSoundnessSpec.refinedResult` `Q3.other` ? → 2 (old comment corrected), `GroundTypesSpec.groundCallable`
  ? → 2^(2^37) and ω, `OpaqueSumForwardingSpec` reject cases now counted by saturation (12, 4, 1, ω, 6, 6, 2, 2;
  `?` for payload recall), reason spellings in `KindedIdentitySpec`, `SequenceEliminationSpec.rigidGoal`. No test
  expectation changed at landing.

#### Open items (`?`)

- ? The 7 inline rows above wait for A6 (relaxed D-inline); the A6 lander should check they take the lane's numbers.
- ? PB2 counts above 1, finite counts beside a bare-binder result (`compoundInstantiation`), undetermined / bounded /
  two-sided / higher-kinded member binders, algebra premises (Monoid-like evidence), curried generators.
- ? Callable sums over unbounded or non-enumerated argument spaces (`PSVec.slice` over Int indices; uniform ground
  elimination not attempted); C11 behind a reasoned rule or through a PB2 instantiation (`quotientCycle`, true ω).
- ? Bare-binder results that grow every goal answer the state budget (`growingChain`, true 1); deferred inline
  members whose type may be empty; record callables named by field index (`inner#0`).
- ? A measured method's own two-sided phantom binder still ω (true 1, pre-existing `BinderBounds`, B1's owner);
  `Accessor[F]` beside `Accessor[G]` still an overlap (B1's shape rule). `Indexed.at` stays `?` on B3's re-entry
  reason (correct).
- ? CodeScene not runnable here.

### C4 — No side effects: vars as vals, throwing ignored, throwables as data (landed)

- **Landed:** `b1eefb9` on integration `ccr-d57d9679-dtp3fh` (previous `cd2c845` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3 + A4 + B3 + A5 + the no-side-effects docs + B4). Compare-and-swap after rebasing the reviewed lane commit
  `783e010` (fix round 2, base `ad931e2`). Gates: `runs/b1eefb9455650fc0ce7afcf2893d564754ea5fcb`. Results: test
  64/64 suites (head 62 + `PureCodeSpec`, `ThrowableSlotsSpec`), scalafmt, scalafix, file metrics (the two
  pre-existing comment-share warnings), EoCheckout PASS. CPD 3 (head 3, the same three sites). `EoBaseline` on the
  pinned 0.16.0 jar: "Five reviewed model counts and input-order checks passed", artifacts byte-identical to the
  head's. Details: `notes/C4.md` (rules, two review rounds, "Integration delta").
- **Rule** (owner directive D-pure; register **D26**, renumbered from the lane's D21 because A5/B4 took D21–D25;
  derivation in `docs/research/pure-code.md`):
  - **A `var` is a `val`.** A public `var` parameter is a field (open or final record, case class, sealed leaf). An
    abstract `var` is its getter. A concrete `var` is derived code (E164). No reason cites mutation. Only typing
    still refuses: `not a stable path: x is a var` (E083).
  - **A throw supplies nothing.** `throw`, the root `???` / `sys.error` (also through `Predef` / `_root_` paths), a
    block ending in one and an all-throwing `if`/`match` are typed `Nothing`. An overridable member whose default
    only throws reads exactly as its abstract twin. A template whose construction evaluates a term that only throws
    is never built: its record is empty, and a declaration running in it (or in a template its instances carry)
    is 1. Sealed / enum constructions that would throw are refused (that reading resolves no names).
  - **A throwable is data.** The twelve root-import throwables are `Ground(fqn, ω)` on the method side (javap
    API; the stack trace, `initCause`, `addSuppressed`, `printStackTrace` and `getSuppressed` are excluded). A
    throwable is its own rigid key. As a parent it is a summarized parent with three overridable reads. On the
    stored side it is an open class, `open to implementations (Throwable)`.

#### Measured delta (vs cd2c845)

Headline **172 finite · 93 ω · 1374 ? → 172 finite · 97 ω · 1370 ?**.

- Methods `+0 -0 ~77`. **4 `?` → ω**, exactly the lane's: `ConfluentWire.strip` (`AvroFailure` now reads; a
  `BinaryParseFailed(new Exception(s))` per `s`), and `WholeRecordBuilder.multiBranch` / `noNullBranch` /
  `pairMismatch` (`new IllegalArgumentException(s)` per `s`). No number moved to `?`, and no other number changed.
- 73 reason-only rows: the lane's 67 plus 6 interaction rows. Stored values: 4 reason-only rows (`AvroBridge#X`,
  `BridgedBytes`, `AvroFailure`, `AvroFailureException`: `unresolved: Throwable` → `open to implementations
  (Throwable)`), identical to the lane's.
- Four-way check (base / lane / head / landed, `c4land/fourway.py`): no lane move lost and no head count moved. Every
  row follows the union rule except 6 `GetReplaceLens` rows (`andThen` ×3, `foldMap`, `from`, `replace`). B4 newly
  reached Optional's sealed family on them. C4's plain-parameter leaf rule makes the family refuse first at
  `FromFunctions(read, write)` (eo `Optional.scala:70`, no `val`, E173): `sealed subtype binds a parameter to no
  slot … Fused (M0, of Optional)` → `sealed subtype hides a parameter: FromFunctions`, as on the lane's other 15
  rows. All six stay `?`. This is correct and needs no fix.
- Main reason movements (blocker categories 54 → 58):
  - `open stdlib type with unmodelled members` 52 → 4, and its union form 9 → 1;
  - `sealed subtype binds a parameter to no slot` 21 → 0, `sealed subtype hides a parameter` 0 → 21;
  - surfaced behind the throwable: `receiver fields constrained by a class nested in non-public _` 1 → 3,
    `capability record with abstract type member not read` 278 → 279, union non-public class 3 → 4;
  - new categories: `open to implementations` 1, union non-public constructor 2, union recursive type 2, `union
    alternatives require an overlap and discrimination proof` 1.

#### Review findings and resolution

- **Lane round 1** (17 findings, all confirmed and fixed). Bounded binders and overriding inferred results were
  over-dropped. Emptiness was judged by name. A strict `val` initializer that throws needed the construction rule.
  A plain sealed-leaf parameter was over-counted. `{}` crashed `Throws`. Taint through `Either`, unpinned limits,
  quality, citations (Pitts & Stark 1998 and Ahmed, Dreyer & Rossberg 2009, metadata-verified) and docs were also
  fixed.
- **Lane round 2** (15 findings). Rule (b), which dropped an empty member, disagreed with its instantiations and
  subclasses. The reviewer's direction was rebutted, and rule (b) was replaced by the abstract-twin reading, which
  is the only reading consistent with `OpenGet.m` = 16.
  - Fixed: top-level vals; member objects (static-object nesting); level-scoped construction names; sealed mixins
    and enum companions.
  - Quality: `Construction.scala` split out; dead `Emptiness` and `MemberSyntax.empty` deleted.
  - The non-final case leaf of a throwable is routed to FOLLOWUPS 18, as a D15 limit.
- **Landing.**
  - Conflicts were resolved keeping both sides: `Measurement` (the head's lazy `inferred`/`capabilities` plus C4's
    construction guard), `StdlibModel` (A5's type-revealing exclusions plus transparent throwables), `MemberSyntax`
    (B4's member identity plus `var` observations), `NominalKeys` (`groundKey` plus A5's literal keys), the
    register (D20 boundary merged; C4's entry renumbered to D26 and every C4 `D21` reference updated),
    `ground-scalar-types.md` and `stdlib-model.md`.
  - Lane open question 2: B4's readings replaced the lane's `? polymorphic capability binder` expectations in
    `PureCodeSpec`, each with an abstract twin kept equal (fixtures `probes/c4land`, all compile with scalac 3.9.0).
  - `pure-code.md` §4 and D26's boundary were updated for D24.

#### Expectations changed

- **Owner-reviewed:** none. Unchanged: `CanGet.get` 0, the five 0.16.0 counts, EoCaseLedgerSpec,
  OpaqueTypeScopeSpec, ReferenceInhabitantsSpec, ArticleCardinalitySpec.
- **Lane changes** (derived in the notes):
  - `CapabilitySpec.unreadMembers` `U.mut` ? → 2;
  - `KindedIdentitySpec.modelKeys` `throwable` ? → 1;
  - `StdlibModelSpec.open` `boom`/`missing` ? → ω;
  - `MethodRefinementSpec.mutablePath` reason → `not a stable path: r is a var`;
  - `NamespaceSpec.slotRefusal` re-vehicled to an unsupplied `Widget`;
  - `StoredValueSoundnessSpec.taint` `unresolved[Throwable]` → `open[Throwable]`, plus `Res2`;
  - `TypeMemberSpec.valuePaths` → 1 (an object whose strict val throws is never built).
- **Changed at landing** (PureCodeSpec only, C4's own spec):
  - `OpenFail.m` ? → 1 (∀A. String ⇒ A is empty), twin `AbsFail.m` 1;
  - `K3.use`, `K3a.use` ? → 1 (∀A. A ⇒ Nothing is empty);
  - `Mention.m` ? → 4 (∀A. A ⇒ A is the identity), twin `AMention.m` 4;
  - `H5.m` ? → 1 (∀A. Int ⇒ (A, Int) is empty), twin `AH5.m` 1;
  - `H6.m` ? → 4 (∀A. Option\[A\] = {None}), twin `AH6.m` 4.

#### Open items (`?`)

- ? Owner decision: a fixed member that implements a declaration of an empty type (`object OQ extends P`, eo's
  `PSVec.Empty.apply`/`head`/`length`/`slice`) is read as built while the parent's record is vacuous. A "contract"
  variant was measured and withdrawn (`c4fix2-contract-variant`): it moves `PSVec.Empty.*` to 1.
- ? Owner decision: the default-instance reading (needed for `OpenGet.m` = 16); `getStackTrace`/`getSuppressed`
  excluded; sealed/enum constructions that would throw are refused rather than read exactly.
- ? Unions with a throwable (no runtime class for open stdlib classes); open subclasses of a throwable; a wildcard
  import that may bind the name; `try`; mixed throwing/returning matches; `Try`/`NonFatal`/`MonadThrow`.
- ? Open members with an inferred result; polymorphic open members with bounded or undetermined binders (D24);
  constructions whose names an unread scope may bind.
- ? D15 limit: non-final case-class leaves (a throwable's `getMessage` included) are read as their fields (FOLLOWUPS
  18). The `S0.???` re-entry is judged with the subclass's names (FOLLOWUPS 19).
- ? CodeScene not runnable here.

### A6 — Inline methods without compile-time operations count like defs (landed)

- **Landed:** `ecc47dd` on integration `ccr-d57d9679-dtp3fh` (previous `b1eefb9` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3 + A4 + B3 + A5 + the no-side-effects docs + B4 + C4), compare-and-swap after rebasing the reviewed lane
  commit `340f60e` (fix round 2, base `fb5d936`). Gates: `runs/ecc47dde40aff32f633b4a3d2be89be7ba6db835` — test 67/67 suites (head 64 + `InlineMethodsSpec`,
  `InlineScopesSpec`, `TransparentInlineSpec`), scalafmt, scalafix, file metrics (two pre-existing comment-share
  warnings), EoCheckout PASS; CPD 2 (head 3: the `MemberSyntax`/`TransparentResults` duplication went with the rule-X
  rewrite). `EoBaseline` on the pinned 0.16.0 jar: "Five reviewed model counts and input-order checks passed";
  `sbt plugin/scripted` 3/3. Details: `notes/A6.md` (rule,
  two review rounds, "Integration delta").
- **Rule** (owner decision D-inline, relaxed; register **D27**, renumbered from the lane's D24 because B4 and C4 took
  D24–D26; derivation `docs/research/syntax-features.md` "Inline methods count as methods unless their body uses a
  compile-time operation", rule X in `docs/research/top-types-unions.md`): an `inline def` or inline given alias
  counts exactly like a `def` when its body, defaults, nested definitions and (transitively, a call cycle decided as
  one component) every inline method it calls use no compile-time operation (`inline if` / `inline match`, any
  `scala.compiletime` member however bound, `compiletime.ops`, quotes / splices), every called name resolves (D11
  order) to a supplied method, a passing inline method or a library-table member with its clauses counted, and no
  other channel reaches the call site: a member a subclass may override inline selected on a parameter, lambda
  parameter, `this` / `C.this` of an open template (a function parameter's `apply` included, and the `apply` an extra
  argument clause selects on a result); a runtime class reified from a type variable (type test, `Array[A]`,
  `classOf`); a given left to implicit search (callee / extension-group / local clauses, context bounds, context
  function values and literals, eta-expansion, `summon` / `implicitly` / `valueOf`); an extension or conversion the
  body may find (by type, through alias / export chains of any length). `transparent` and `inline` parameters do not
  disqualify (no side effects: an inline parameter has its argument's value, boxing aside). Rule X now refuses every
  `transparent inline` result whose declared type is not a singleton. Evidence: Scala 3 reference *Inline*,
  *Compile-time operations*, *Macros* (\[17\], inspected) plus Scala 3.9.0 fixtures `probes/a6`, `review-A6`,
  `review-A6/r2`, `a6-fix1`, `a6-fix1b`, `a6-fix2`, `a6-land`.

#### Measured delta (vs b1eefb9)

Headline **172 finite · 97 ω · 1370 ? → 188 finite · 100 ω · 1351 ?**; methods `+0 -0 ~120`: **19 `?` → number, 0
number → `?`**, 101 reason-only; stored values unchanged (core 102/19/385 → 115/20/371).

- 15 lane rows, each the count it had before A5 (re-derived in the notes): `PickFold.getOption` 2,
  `BijectionIso.replace` 1, `SimpleLens.place` ω, `Modify.modify`/`replace` 1, `MendTearPrism.reverseGet` 1,
  `PickMendPrism.getOption` 2 / `reverseGet` 1, `widenLeft`/`widenRight` 1, `Span.isHit` 2^(2^64),
  `PathParser.isIdentStart`/`isIdentPart` 2^(2^16), `AvroWalk`/`JsonWalk.terminalOf` ω.
- 4 interaction rows: numbers B4's lane measured but could not land while A5 refused every inline method; a passing
  body now reaches D25's sum elimination as its `def` twin does — `MendTearPrism.getOption` 2 (`None` / the `Right`),
  `MendTearPrism.replace` 2 (`mend(b)` or the `Left`'s `t`), `PickMendPrism.replace` 16 (`s` or `mend(b)` for each of
  the 4 definedness pairs of `pick(s)`, `pick(mend(b))`: 2^4), `PickFold.andThen` 2 (`None` or the `flatMap`). Pinned
  by `InlineScopesSpec.solverTwins` (fixture `probes/a6-land/sums.scala`, compiled and run; inline and def twins
  equal). B4's other three (`MendTearPrism.modify`, `fuseToMendTear`, `andThen(MendTearPrism)`) stay `?` under A6's
  channels (`apply` on a function parameter; `widenRight` on an unread type).
- Four-way check (`a6land/fourway.py`, base `fb5d936` / lane / head `b1eefb9` / landed): every lane count move
  reproduced, no head count moved, every row's reasons follow the union rule except the 4 interaction rows (whose
  only "expected" reason is the pre-B4 opaque sum-elimination reason B4 removed).
- Main reason movements (blocker categories 58 → 58): `inline method` 120 → 88 (the eager A5 reason replaced by what
  actually fails: 32 quote/splice macro entry points, 30 overridable-member selections, 4 calls of a refused inline
  method, 9 unread names, 9 unread selections, 3 extension fallbacks, 1 `summonInline`), `transparent inline
  result` 20 → 28 (rule X), `Higher-order application is unsupported` 1 → 2 (`Modify.andThen`, its pre-A5 solver
  reason).

#### Review findings and resolution

- **Lane round 1** (20 findings, all confirmed by Scala 3.9.0 runs and fixed): inline overrides re-selected on a
  narrower receiver (`BijectionIso.modify`, `SimpleLens.transfer`, `JsoniterPrism.scanRange` back to `?`), implicit
  search through extension-group clauses / context-function values / eta-expansion, conversions recognised by
  spelling (now by type through aliases, renames, `Conversion` subclasses, found where Scala searches), type tests
  on `Array[A]`, rule X letting closed transparent results through (now: only singletons pass — `Direct.apply` /
  `value`, `ForgetK`, `MultiFocus(K)` back to `?`), the "same function" docs claim (boxing excluded), reason
  phrasing, cycle verdicts (SCC), quality.
- **Lane round 2** (17 findings, all confirmed and fixed): local defs' using clauses / context bounds / defaults,
  extra argument clauses applying a result's `apply`, `Outer.this`, `classOf[Array[A]]`, `this` inside anonymous /
  local classes and local givens, alias/export depth cut-offs (visited sets now), cycle state after an interrupted
  reading (`NonFatal` restore; regression `interrupted`), dead code, universal-member lists, local objects, test
  gaps, metrics (BodyWalk split; TopTypesUnionSpec moved under 800 lines).
- **Landing.** One textual conflict (`top-types-unions.md` refusal table: B4's union-callable row and A6's rule-X row
  both kept). Register: B4/C4 took D24–D26, so the rule is **D27** (moved after D26; every A6 reference renumbered;
  B4's own D24 references untouched); A6's "D-pure" mentions now cite C4's D26. Semantic interactions checked by
  fixture and pinned: B4's `DeferredInline` refuses a receiver with a deferred inline member in an inline target
  (`probes/a6-land/dip.scala`: `g(new A)` true, `g(new B)` false, `g` at a `T` rejected) — `g` carries both
  refusals, a passing `h` keeps B4's conservative refusal, its def twin counts 4 (`InlineScopesSpec.deferredMembers`;
  D27 boundary, `syntax-features.md` limits). C4's `Throws` and A6's skipped `throw` agree; no shared code.

#### Expectations changed

- **Owner-reviewed:** none. `CanGet.get` 0, the five 0.16.0 counts, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec and the scripted fixture unchanged. 14 unreviewed 0.16.0 rows move
  `?` → n with their 1.0.0 twins' numbers.
- **Lane changes** (derived in `notes/A6.md`): `TopTypesUnionSpec.inlineTop` `isInt`/`opt`/`al` ? → 2/4/2;
  `SyntaxFeaturesSpec.inlineMethods`' A5 bodies count (`plainVal` 2^(2^37), `plainPick` 2; refusals pinned on
  inline-match bodies); rule X refuses `SyntaxFeaturesSpec` `closed` (9 → ?) and `nf` (1 → ?) and `TopTypesUnionSpec`
  `T.s` 9, `T.o` 4, `T.f` 1, `u` 1, `t` 1 (→ ?); `InlineMethodsSpec.lam` replaced by a lambda applied in place;
  `extensionFallback`'s receiver made final; `reasonPhrases` names "the function identity"; rule-X examples moved to
  `TransparentInlineSpec` unchanged.
- **Changed at landing:** none; two examples added (`InlineScopesSpec.deferredMembers`, `solverTwins`).

#### Open items (`?`)

- ? Bodies using a compile-time operation or one of the channels (macro entry points, `summonInline`); abstract
  inline declarations and their implementations; definitions inside a refused inline method.
- ? Conservative refusals that a finer reading could count: a sealed receiver whose subclasses are all supplied and
  override nothing inline; every function parameter's `apply` (`BijectionIso.modify`, true 1 unless an argument
  class has an inline `apply`); applied selections a visible inline extension may answer (`Getter` / `BijectionIso` /
  `Review.andThen`, true 1); a receiver with a deferred inline member in a passing inline body (D24's refusal).
- ? Rule X: every transparent result that is not a singleton type (`Direct.apply`/`value`, `ForgetK`,
  `MultiFocus(K)`).
- ? Names the reading does not type: library members outside the table, untyped lambda parameters, pattern
  variables, `super`, imports inside the body, `for`, local classes' members.
- ? Assumed (as D20): no extension or conversion from the companion of a type outside the sources, or a named
  import of one, answers an inapplicable member.
- ? Quality follow-ups for the final cleanup: merge `BodyTerms`' name resolution with `TermBindings` (FOLLOWUPS 22);
  `MemberSyntax.inline` (A6) beside `inlined` (B4) — different scopes (`inline val`), could share one modifier
  reader.
- ? CodeScene not runnable here.

### B5 — Rows for givens, values, anonymous-class members and other missing definitions (landed)

- **Landed:** `08dcde1` on integration `ccr-d57d9679-dtp3fh` (previous `ecc47dd` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3 + A4 + B3 + A5 + the no-side-effects docs + B4 + C4 + A6), compare-and-swap after rebasing the reviewed
  lane commit `7dc5eba` (fix round 2, base `b1eefb9`) and amending the landing changes below. Gates:
  `runs/08dcde1f97a650d503c9143181db5929e926e547` — test 69/69 suites (head 67 + `DefinitionInventorySpec`,
  `NestedScopesSpec`), scalafmt, scalafix, file metrics (two pre-existing comment-share warnings), EoCheckout PASS;
  CPD 2 (head 2). `EoBaseline` on the pinned 0.16.0 jar (`runs/08dcde1…-eobaseline`): 480 → 644 rows, "Five
  reviewed model counts and input-order checks passed"; `sbt plugin/scripted` 3/3. Details: `notes/B5.md` (rules,
  two review rounds, "Integration delta").
- **Rule** (register **D28** — D27 was taken by A6; derivation `docs/research/definition-inventory.md`, census.md §4
  item 2): no counting rule is new; every definition beyond defs gets a row measured by the rule its Scala 3.9.0
  translation already has. Given aliases are methods of their clauses (nullary: a value), abstract givens
  declarations; member `val`/`var` a nullary signature of their type (untyped: overridden declaration's result, else
  the right-hand side, tier 1; `final`/`inline val` of a literal keeps its literal type, a `var` never); members of
  anonymous classes are a final class's; definitions in a body's blocks, function literals, case clauses and
  comprehensions are rows (`<block#k>`, `<lambda#k>`, `<case#k>`, `<for#k>`) counted in their typing context (the
  signature lambda lifting gives; control flow narrows nothing); case/comprehension binders (given binders
  included) are refused, and every count under a pattern that may refine a type with a type parameter in scope;
  parameterized enum cases, abstract classes, traits with a clause and conditional structural givens get
  constructors; a secondary constructor measures the primary's argument tuple over its parameters, its body's
  definitions under `<init#k>`. A value or given implementing a declaration of its name is that declaration
  (anonymous givens under 3.9.0's synthesized `given_…` name). Evidence: Scala 3 reference *Translation of Enums
  and ADTs* \[24\] and draft spec ch. 5 \[30\] (inspected), 3.9.0 `-Xprint:typer` fixtures (`probes/b5*`).

#### Measured delta (vs ecc47dd)

Headline **188 finite · 100 ω · 1351 ? → 289 finite · 126 ω · 1659 ?**; methods `1639 → 2074 rows; +435 -0 ~0`:
**no existing row changed** (count or reason); stored values unchanged (`435 → 435; +0 -0 ~0`). The 435 new rows
carry the lane's identities and **exactly the lane's counts** (127 numeric: 46 abstract-class, 34 enum-case and 11
given constructors, 15 nested local defs, 10 anonymous-class members, 10 member vals, 1 secondary constructor;
re-derived in the notes, e.g. `ConfluentWire.HeaderLength` 2^32, `Plated.transformRecursionLimit` 1 (literal
type), `MultiFocusK.pApply.<new#1>.to` 1, `docs.Shape.Triangle.<init>` (2^64 − 2^53 + 3)^2, `TraverseBuf.<init>`
(secondary) ω, the three jsoniter loops ω). Three-way check (`b5land/cmp.py`, base / head / lane / landed): 2
reason-only interaction rows, `Optic.andThen.<new#1>.to` / `.from`, trade A5's eager "definition inside an inline
method" for A6's verdict on the owner (`andThen uses scala.compiletime.summonInline`) — both stay `?` for three
other reasons too. 0.16.0: head rows identical, the 164 new rows identical to the lane's. Main reason movements
(all from the new rows; blocker categories 58 → 65, combined signatures): `declaration not supplied` 694 → 890,
`capability record with abstract type member not read` 279 → 364, `inherited type member not resolved` 160 → 196,
`receiver fields constrained by a non-public class` 64 → 76, `polymorphic capability binder` 33 → 44, `sealed
subtype constructor not visible at use site` 42 → 52; new categories `binder of an enclosing case clause not read`
8, `pattern of an enclosing case clause may refine a type` 7, `nominal identity of path type not established` 4,
`local capture may be hidden by an inherited member` 4, `definition inside an inline method` 2 (A6's reason on the
two interaction rows); `open class with hidden constructor state` unchanged.

#### Review findings and resolution

- **Lane round 1** (20 findings, all probe-confirmed, fixed): hidden parent constructor state under shadowing
  (`PassedState`), enum cases with `extends` losing the enum's binders, context-function literals bound twice,
  secondary constructors read as values of an open class (now the primary's argument tuple: `TraverseBuf` ? → ω),
  `final var x = 3` given a literal type, overcounts under literal case patterns / branches (refused in round 1),
  secondary-constructor bodies, reason naming, quality and test gaps; local nullary given aliases are steps, not rows.
- **Lane round 2** (17 findings): GADT-refining patterns gave wrong numbers (0/1 where 3.9.0 accepts `42: A`) —
  now refused on every count but a one-valued result; given-pattern binders refused; anonymous givens are members of
  their synthesized name (`GivenNames`); round 1's branch narrowing withdrawn as a model correction (typing-context
  reading; derivation: a nested def is counted as its lambda-lifted signature, whose captures range over their types
  as a method's parameters do) — three jsoniter loops back to ω; secondary bodies `<init#k>`; refactors.
- **Landing.** Conflicts in `SourceIndex.scala` (A6 moved frame construction into companion functions shared with the
  inline-body reader; B5 had rewritten it with explicit paths): the index now builds every named frame with A6's
  functions through B5's `registered` (A6's `opened(frame)` renamed), B5's `child` kept only for enum-case and
  secondary constructors via a shared companion `declared` (one `Frame(...)` construction); `givenName` shared;
  ids and paths unchanged for every named definition, so A6's verdict cache and B5's identities agree. Register
  entry renumbered D27 → D28. The rebase created one CPD duplication (B5's `ContextBinders.rightHandSide` vs A6's
  `InlineBodies.body`): both now use `MemberSyntax.rightHandSide` — no behaviour change (eo TSV byte-identical to
  the pre-change rebased run). Semantic interactions pinned by 3.9.0-compiled fixtures (`probes/b5-land`):
  `DefinitionInventorySpec.inlineGivens` (B5's new `inline given` rows read by A6: 2, 4, refused on `summonInline`)
  and `NestedScopesSpec.inlineOwners` (a `<block#1>` def in a passing inline method 16, in a refused one carries
  A6's "definition inside an inline method"); inline defs in anonymous classes probed (16, 16: the anonymous
  template is final). Docs: `definition-inventory.md` *Inline definitions*, D28 states the interaction.

#### Expectations changed

- **Owner-reviewed:** none. `CanGet.get` 0, the five 0.16.0 counts, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec and the scripted fixture unchanged.
- Other (lane, derived in the notes): `ReportHygieneSpec.operatorNames` lists the abstract class's constructor row
  `P` (1); `MethodAnalysisSpec.abstractCase` pins `Holder.<init>` 1 and `Holder.value` 0 beside `extract` `?`.
  No test expectation changed at landing (two examples added).

#### Open items (`?`)

- ? Names of a pattern definition (`inferred result type not resolved: a name of a pattern definition`);
  case/comprehension binders and untyped function-literal parameters; any count under a pattern that may refine a
  type (whether the pattern actually pins an argument is not read: `case Some(_)` of `Option[A]` is refused,
  true 2).
- ? Control flow is not read by decision (owner open question 1 in the notes: a reachability reading would need
  call-site analysis); anonymous givens with a bounded wildcard lose their synthesized name; givens with clauses
  implementing inherited ones (`inherited override identity not resolved`); an intermediate level passing a
  parent's hidden argument.
- ? Not indexed: definitions in default arguments and named templates' parent arguments; local block values and
  local nullary given aliases are no rows (by decision); conditional structural givens with only type parameters get
  constructor rows (owner question 2).
- ? A6's inline-body reader names anonymous templates `<anonymous@L>` while the index's rows use `<new#k>`
  (independent frames, no shared cache key, no count depends on it; a reason printed from inside such a template may
  use the `@L` spelling) — for the orchestrator's cleanup.
- ? CodeScene not runnable here.

### A7 — Method-side lookup honours unsupplied wildcard imports; one table of listed namespaces (landed)

- **Landed:** `83f5e76` on integration `ccr-d57d9679-dtp3fh` (previous `08dcde1` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3 + A4 + B3 + A5 + the no-side-effects docs + B4 + C4 + A6 + B5), compare-and-swap after rebasing the
  reviewed lane commit `1bd8be9` (fix round 2, base `ecc47dd`) and amending the landing changes below. Gates:
  `runs/83f5e76cf0900f42435502a0415187880d970be1` — test 70/70 suites (head 69 + `UnsuppliedWildcardSpec`),
  scalafmt, scalafix, file metrics (two pre-existing comment-share warnings), EoCheckout PASS; CPD 2 (head 2).
  `EoBaseline` 0.16.0 (`runs/83f5e76…-eobaseline`): "Five reviewed model counts and input-order checks passed";
  `sbt plugin/scripted` 3/3. Details: `notes/A7.md` (rule, two review rounds, "Integration delta").
- **Rule** (register **D11** name-binding order, extended, and **D16** listed namespaces; derivation
  `docs/research/stdlib-model.md` "Wildcard refusal" / "Listed and unsupplied wildcards against the sources' names",
  `docs/research/stored-value-names.md`): Scala 3 ranks a definition of the reading file, an inherited or exported
  member > an explicit import > a wildcard import > another file's package member > the root imports, an inner
  binding shadowing outer ones of the same or lower rank. So a wildcard (or `given` selector, or an importer whose
  prefix is no path) over a namespace the sources do not supply may bind any type name or path head the reading would
  otherwise take from another file, an outer wildcard or a root package: the method side now refuses those
  (`unresolved type: Fst (may be imported by cats.syntax.functor.*)`, `… compose may be imported by …`), as the
  stored side already did; a doubted prefix passes its doubt on. Kept: same-file definitions (sibling package
  blocks included), inherited members, exports, explicit imports, a supplied same-scope wildcard (E049 otherwise),
  `_root_` paths. Path heads over supplied namespaces are read as Scala does (own accessible members, export
  aliases, parents' members; a parent outside the sources refuses). A package some file declares a member in is
  read as complete (closed-package assumption, documented); a prefix-only package may hold anything. One table,
  `ListedNamespaces` (scala-library 3.9.0 class files / TASTy: `scala.quoted`, the twenty CollectionConverters
  conversions, `quotes.reflect` with root spellings `Singleton`/`Symbol`), serves both sides. Every reading goes
  through the lookup (goals, inputs, record fields, constructor slots, projection prefixes, inline-body
  conversions — a doubt hides the supplied declaration, which D27's conversion search still reads). Evidence:
  Scala spec ch. 2 / reference (imports, export clauses, given imports) and Scala 3.9.0 fixtures (`probes/a7`,
  `review-A7`, `a7fix1`, `a7fix2`); the B3 delta audit's dissent (a whole-program argument) not adopted.

#### Measured delta (vs 08dcde1)

Headline **289 finite · 126 ω · 1659 ? → 289 finite · 127 ω · 1658 ?** (on the lane's base `ecc47dd`: 188 / 100 /
1351 → 188 / 101 / 1350); methods `2074 → 2074; +0 -0 ~150`: **3 n → ?, 4 ? → n**, 143 reason-only; stored values
`+0 -0 ~31`: **6 ? → n, 1 n → ?**, 24 reason-only.

- Count changes (all the lane's, same numbers; re-derived in the notes): `data.ModifyF.map.map` 1 → ? (F3: `Fst`,
  `ForgetfulFunctor` may be cats' under `cats.instances.function.*` / doubted `compose.*`), `optics.Optic.id` 1 → ?
  (`BijectionIso` under `cats.syntax.functor.*`; 3.9.0 binds a stub's member), `optics.ForgetFold.from` 1 → ?
  (Fold.scala's `import cats.instances.option.given` may bind `compose`, so the result `Unit` may be an inherited
  member); avro constructors under `scala.jdk.CollectionConverters.*`, which binds no `Any`/`Array`/`Int`:
  `AvroWalk.WalkRes.<init>` ? → ω, `WholeRecordBuilder.DirectPlan.<init>` ? → 2^32, `DirectSlot.<init>` ? → 2^64,
  `SelfKind.<init>` ? → 2^32. Stored: `AvroWalk.State`, `WalkRes` ? → ε₀ (stored `Any` convention), `AvroSliceFace#X`
  ? → ω, `DirectPlan` / `SelfKind` ? → ≤2^32, `DirectSlot` ? → ≤2^64; `ForgetFold#X` 0 → ?.
- Four-way check (`a7land/cmp.py`, base / lane / head / landed): the 1639 rows the lane measured and all stored
  values are identical to the lane run; of B5's 435 rows, 34 change reason only and stay `?` (Forget / ModifyF /
  MultiFocus / Optic anonymous-class members and givens: `AssociativeFunctor`, `optics.Optic`, `Fst`,
  `ForgetfulFunctor`, `Accessor` doubts; 18 kyo givens/vals: `optics`/`data` heads after `import _root_.kyo.*`); the
  `outerProfunctor.dimap.<new#1>` pair drops `nominal identity of path type not established: o.X`, now reported as
  its declared type's refusal (`Optic` may be a member of the unsupplied parent `Profunctor`, round 2 finding 7).
- Main reason movements (blocker categories 65 → 62, combined signatures): `unresolved type` 22 → 128, `inherited
  type member not resolved` 196 → 218, `local capture may be hidden by an inherited member` 4 → 8; down `capability
  record with abstract type member not read` 364 → 343, `declaration not supplied` 890 → 866, `polymorphic
  capability binder` 44 → 34, `inert match type` 9 → 1; gone `nominal identity of path type not established`,
  `recursion through a constructor the estimate does not read`, `sealed subtype not visible at use site`
  (reading stops at the first unread name; stored side reads CollectionConverters' slots).
- 0.16.0: exactly 3 unreviewed rows move n → ? (`ModifyF.map.map`, `ForgetFold.from`, `Optic.id`, the twins).

#### Review findings and resolution

- **Lane round 1** (20 findings, all probe- and 3.9.0-confirmed, fixed): sibling package blocks, builtins under a
  doubted prefix (`ShadowedBuiltins` asks `SourceLookup.doubtedHeads`), `given` selectors (`ImportBinding.givens`),
  prefix-only packages (closed-package assumption, both sides), supplied wildcards binding a val / an enum case,
  importers whose prefix is no path (indexed and refused), a StackOverflowError (clause read at its own position),
  `quotes.reflect` root spellings from TASTy, the `Singleton` marker, docs and quality.
- **Lane round 2** (17 findings, fixed): path heads ignored inherited and exported members, package objects with
  parents, anchors (nearest package), type names as heads, private members (`TermMembers`, stored `Exposure`);
  spurious refusals (enum singleton cases, wildcards importing givens, private givens); the `rc` reason
  (`TypeMembers.named`); one closed-package predicate (`Trees.declaresPackageMember`); quality and docs.
- **Landing.** Conflicts in `ShadowedBuiltins.scala` (B5's `template`/`templateOf` kept, A7's move of
  `declaresMembers` kept) and `SourceIndex.scala` (B5's `registered` with A7's package-object parents). The merge took
  `SourceIndex.scala` to 826 lines (warning): `ImportBinding` / `ExportClause` and their builders moved to
  `analysis/ImportBinding.scala` (727 lines left; eo TSV byte-identical before/after). Interactions checked: B5's
  synthesized anonymous given names are read by A7's given imports as Scala reads them; B5's nested scopes see the
  file's wildcards through `frame.chain`.

#### Expectations changed

- **Owner-reviewed:** none. `CanGet.get` 0, the five 0.16.0 counts, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec and the scripted fixture unchanged.
- Other (lane, derived in the notes): `ImportClausesSpec.importsRefused` row `d.Use` refused → 2
  (CollectionConverters declares no term `a`; the refusal kept as `d2.Use` under `zz.*`);
  `GroundEnvironmentSpec.shadowedPosition` / `symbolicDisagreement` fixtures nested (still `?`, same disagreement
  reasons; the import form's new reason pinned); `GroundEnvironmentSpec.enumCases` reason now `enum case type not
  read`. No test expectation changed at landing.

#### Open items (`?`)

- ? Every name and path head an unsupplied wildcard or `given` selector may bind stays refused until its namespace
  is supplied; a per-version index of a dependency's exported NAMES would let `cats.syntax.functor.*` bind only what
  cats 2.13.0 declares (`ModifyF.map.map`, `Optic.id` back to 1) — owner decision, future work.
- ? Owner to confirm: `Plain.id` with the qualified spelling `dev.r.optics.BijectionIso` is `?` (`dev` may be
  imported by `zzz.syntax.functor.*`, 3.9.0 fixture `probes/a7/plain`), not the 1 the assignment expected; and the
  stored side's same-body rule (supplied member beats an unsupplied wildcard of the same body; no eo row).
- ? Stored side: sibling package blocks, inherited / exported / private members under a wildcard, and `given … with`
  instances are refused there where the method side counts (refusals only, no eo row).
- ? The closed-package assumption is an assumption (documented in D11, stdlib-model.md, stored-value-names.md).
- ? CodeScene not runnable here.

### Z1b — Stored-side soundness: inherited, package and enum-companion members, opaque scopes, value rows (landed)

- **Landed:** `280ab80` on integration `ccr-d57d9679-dtp3fh` (previous `83f5e76` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3 + A4 + B3 + A5 + the no-side-effects docs + B4 + C4 + A6 + B5 + A7), compare-and-swap of the reviewed lane
  commit (fix round 2) unchanged — its base was still the head, so no rebase. Gates:
  `runs/280ab804cf976a02ef949052056dfa7ba6d74849` — test 72/72 suites (head 70 + `StoredScopesSpec`,
  `ValueRowsSpec`), scalafmt, scalafix, file metrics (two pre-existing comment-share warnings), EoCheckout PASS;
  CPD 2 (head 2). `EoBaseline` on the pinned 0.16.0 jar (`runs/280ab80…-eobaseline`): "Five reviewed model counts and
  input-order checks passed", methods TSV byte-identical; `sbt plugin/scripted` 3/3. Details: `notes/Z1b.md` (rules
  S5–S9, three rounds, "Landing").
- **Rules** (integrated-review items S5–S9; register **D29** stored-side binding, **D30** opaque scope + value rows;
  derivations `docs/research/stored-value-names.md`, `docs/research/pure-code.md` §2, `docs/type-arithmetic.md` §10, now `docs/site/arithmetic/counting-data.md`):
  the stored-value estimate now binds names as Scala 3 does. S5: an enum's cases are members of its companion
  (explicit or synthesized), so `import E.*`, `E.A`, a case's parameters and the enum body bind the case class, which
  is refused (`enum case type not read: p.E.A`). S6: a package holds what its package object's parents and its
  top-level exports define; enumerable exports are followed, anything else refuses (`X (may be a member of package
  lib, …)`) instead of falling to the root imports. S7: template bodies bind their parents' and self type's members
  (read in the declaring parent; `val` constructor parameters included; `private[q]` inherited, plain `private` not —
  scalac E173); a parent's exports or an unread parent refuse (`inherited type|term member not resolved: X (…)`, one
  formatter shared with the method side, so lent `<init>` slots agree). S8: an opaque alias is its representation only
  inside its template or, top-level, inside the synthetic package object (top-level defs/givens, implicit classes and
  objects, companions of top-level type aliases — one predicate `MemberSyntax.wrappedInPackageObject` for both sides);
  elsewhere `opaque type read outside its scope`. S9: a value row is min(1, |T|) read as inhabitation (absurd function
  `Nothing => Int` = 1; witnesses: stdlib types with a value, an enum's parameterless case, a family's case object),
  and a class value needs a construction (X6 / D26: ancestors' arguments, defaults, strict members, enum companion;
  a throw, private constructor, implicit-search context argument or unread parent refuses: `value not decided: K holds
  K, whose construction evaluates a throw`). Evidence: Scala 3 spec ch. 2 / reference (enums, export clauses, opaque
  types, package objects) and Scala 3.9.0 fixtures (`z1b/fx`, `z1bfix*`, `probes/review-Z1b`).

#### Measured delta (vs 83f5e76)

Headline unchanged: **289 finite · 127 ω · 1658 ?** (2074 signatures), 435 stored rows. **No count changes** on either
side; methods `+0 -0 ~8` (reason-only), stored values `+0 -0 ~5` (reason-only); the landed run's delta and TSVs are
byte-identical to the lane's `runs/Z1b-fix2`, so no integration delta.

- Reason-only, methods (8): a path head refused through an unread parent now says `inherited term member not
  resolved` — `ModifyF.affine2modify.to`, `either2modify.to`, `tuple2modify.to`, `assocModifyF.composeFrom`,
  `composeTo` (`optics`, parents `Composer[…]` / `AssociativeFunctor[…]`), `EoAccessorBuilder.makeLens`, `makePrism`,
  `makeTraversal` (`Schema`, `optics`, parent `AccessorBuilder`).
- Reason-only, stored (5): `chimney.PartialResult` names its import clause; `ForgetFold#X` names its parent `Optic[…]`
  before the doubted wildcard; `EoAccessorBuilder.Lens` / `.Prism` / `.Traversal` are refused through the unsupplied
  parent `AccessorBuilder` (it may declare `GetReplaceLens` / `PickMendPrism` / `optics`, which would outrank the
  explicit `import optics.{…}`).
- eo's two value rows stay 1, re-derived: `circe.emptyJsonVec: PSVec[Json]` (`PSVec.Empty extends PSVec[Nothing]`
  witnesses every instantiation and is built) and `pickFoldMiss: Affine.Miss[(Unit, Unit)]` (public constructor, trait
  parent, nonzero capacity).
- Blocker categories 62 → 63: `inherited type member not resolved` 218 → 215, new `inherited term member not
  resolved` 8 (core 5).
- 0.16.0: methods unchanged; three unreviewed legacy declared-signature totals move (`Direct.scala` 7ω + 1 → 2ε₀ + 6ω,
  `Forget.scala` 3ε₀ + 8ω + 1 → 5ε₀ + 7ω, `MultiFocus.scala` 20ε₀ + 28ω + 5 → 23ε₀ + 26ω + 4). Lander's per-member
  replica (`z1bland/tool/LegacyProbe.scala`, totals reproduced on base and head classes): six members of objects beside
  top-level opaque aliases (`Direct.apply` / `value`, `ForgetK.apply` / `value`, `MultiFocusK.context` / `foci`,
  `MultiFocus.apply`) read the alias's representation there (S8) instead of the base's blanket 1 — a type-parameter
  expression, the legacy ε₀, as `def poly[A](a: A): A` reads on both sides; control fixture `z1bland/fx/op1.scala`
  (`b(a: Boolean): D[Int, Boolean]` 1 → 4, `val v` 1 → 2). Correct; committed baseline refreshed by the ledger commit.

#### Review findings and resolution

- **Round 0 → 1** (18 findings, all reproduced, none rebutted): qualified-private parent members (wrong count), enum
  case parameters and bodies (wrong count), path heads under followed exports, value rows of root/stdlib types and
  `emptyJsonVec` (round 0's only eo count change, 1 → ?, given back), "type member" for term heads, implicit classes /
  objects / `given … with` in the opaque scope (both sides), `val f: Nothing => Int` = 0 (now 1; `Int & String` = 1
  found and refused), reason wording, quality (one `packageMembers` map, required `Members.defn`, `Lookup`, one
  `Trees.ancestors`), test gaps (full-string pins), commit message.
- **Round 1 → 2** (16 findings, none rebutted): X6 constructibility ignored (wrong count — `Builds`, `Named.built`,
  `Child.built`; `fk: K => Nothing` now `?`, never 0), parents' exports (refused, as the method side does), `val`
  constructor parameters inherited, singleton types over an empty path (refused), one opaque-scope predicate for both
  sides, copied helpers, `conjuncts` via `TypeSubstitution.Meet`, dead code, `MemberKind` enum, witness table recorded
  as FOLLOWUPS 24, test gaps (given body, `import zz.*` witness hinge, labelled rows), probe IDs out of code comments,
  reflow, gated-tree hash recorded (`HEAD^{tree}` = gated tree `04a35ee…`).
- **Landing:** no conflicts; the only finding is the 0.16.0 legacy movement above, examined and recorded in
  `notes/Z1b.md` "Landing".

#### Expectations changed

- **Owner-reviewed:** none. `CanGet.get` 0, the five 0.16.0 counts, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec and the scripted fixture unchanged.
- Other (lane, derived in the notes): `GroundEnvironmentSpec` three disagreements now agree (`shadowedPosition` ? →
  2^64, `constructorScope` ? → 2, `symbolicDisagreement` ? → 2^(2^16)); `StdlibModelSpec.constructorExempt` 2 → ?
  (`lib.Base` may declare `Boolean`); `ReportSpec.crossSourceOpaque` 2 → ? and `CardinalitySpec` "opaque type hides
  cardinality" 1 → ω (references outside the opaque's scope; new in-scope twin = 3); `StoredScopesSpec.valueReasons`
  `val f: Nothing => Int` 0 → 1; `NameLookupSoundnessSpec.unsuppliedObjectPath` / `UnsuppliedWildcardSpec` reasons say
  `inherited term member`. No test expectation changed at landing.

#### Open items (`?`)

- ? Package-object parents, exports written after an import / over a path or value / of a private member, a parent's
  or self type's exports, inherited members under a parent's type arguments, parents spelled through an alias or a
  value: refused, never followed.
- ? An unread parent refuses every name it may bind, even one a nearer method parameter or block binds (conservative).
- ? Enum class cases read as types (`enum case type not read`), and the method side's own resolver disagreeing on a
  companion parent's member in a case parameter (`ecs.G.K.<init>`: refused as a disagreement).
- ? Value rows: a construction not read (throws, private constructor, implicit context argument, unread parent), a sum
  with any unread summand (FOLLOWUPS 25, `?` where true 1), a value's singleton type, intersections/refinements,
  functions from an empty type inside a definition.
- ? `ValueRows.Witnessed` is a second stdlib spelling table beside `StdlibModel` (FOLLOWUPS 24, owner: stdlib model).
- ? Register numbering: D29/D30 are taken by Z1b; Z1a (landing next) must renumber its entries.
- ? CodeScene not runnable here.

### Z1a — Integrated review, method side: soundness gaps closed, precision recovered (landed)

- **Landed:** `7e3e306` on integration `ccr-d57d9679-dtp3fh` (previous `280ab80` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3 + A4 + B3 + A5 + the no-side-effects docs + B4 + C4 + A6 + B5 + A7 + Z1b), compare-and-swap after rebasing
  the reviewed lane commit `89964f5` (fix round 2, base `83f5e76`) and two landing amendments (below). Gates:
  `runs/7e3e3063e872a167a388c74041a744b678bba798` — test 75/75 suites (head 72 + `ReentrySpec`,
  `OwnersAndRecordsSpec`, `LeafFamiliesSpec`), scalafmt, scalafix, file metrics (two pre-existing comment-share
  warnings), EoCheckout PASS; CPD 2 (head 2). `EoBaseline` 0.16.0 (`runs/7e3e306…-eobaseline`): "Five reviewed model
  counts and input-order checks passed"; `sbt plugin/scripted` 3/3. Details: `notes/Z1a.md` (rules, two review
  rounds, "Landing").
- **Rules** (integrated-review items S1–S4, S10, P1–P5; register **D31** view sandwich, boundaries of D15, D19, D20,
  D26, D27; derivations `docs/research/capability-records.md` C6, `pure-code.md`, `definition-inventory.md`,
  `environment-readings.md`, `stored-value-families.md`, `syntax-features.md`). Soundness: an inline member of a
  never-built static object / `given … with` instance / file holder is counted as in a built owner (its expansion
  builds nothing — Scala 3.9.0 runs), class/trait owners keep 1 (S1); C6 views see a local definition by the name its
  block binds and constructions (`new`, `extends`, `this(…)`, creator applications, `copy`) as possible re-entries,
  definite at a definite position (S2); implicit search may select the measured given, except a body returning a
  parameter or a literal of exactly the result type (S3); a capture hidden at the target but held by an unoverridable
  member or a local holder is put back only when held exactly at its own type, dropped at a one-value type (D-top),
  refused otherwise (S4); a non-final case-class leaf with an overridable member (own, extension, source parent's,
  stdlib-listed library parent's) opens its sealed family on both sides, every parent read first (S10); deferred
  givens are abstract (P4). Precision: the view sandwich (lower ≤ count ≤ upper, decided only for {0, 1}); restated
  extensions on final classes skip the inline fallback (P2); records reread written arguments so match-type fields
  reduce (P3); one override predicate `types/Overrides` (P4); final case classes with more than their product read as
  final-class records (P5). Evidence: Scala 3 reference (inline, givens, case classes, extension methods) and Scala
  3.9.0 runtime fixtures (`probes/z1a*`, `probes/review-Z1a`).

#### Measured delta (vs 280ab80)

Headline **289 finite · 127 ω · 1658 ? → 295 finite · 133 ω · 1646 ?** (2074 signatures, 435 stored rows); methods
`+0 -0 ~39`: **12 ? → n**, 27 reason-only; stored values unchanged. Identical row-for-row to the lane's delta against
its base `83f5e76` (three-way check: 0 rows not predicted by combining Z1b's and Z1a's sides) — no integration delta.

- Count changes (re-derived in notes §4): `optics.pickFoldMiss` ? → 1 (P3: `Fst[(Unit, Unit)]` = Unit),
  `laws.ComposedGetOptionLaws.getOptionInner` ? → 1 (P1: both bounds 1, only `None`), `Getter.andThen`,
  `BijectionIso.andThen`, `Review.andThen` ? → 1 (P2: the composite is the only term of the record),
  `jsoniter.JsonPathScanner.Miss` ? → 2^64 (P5: `Span` = (Int, Int)), `JsonPathScanner.find` ×2, `findAll` ×2, `walk`,
  `JsoniterTraversal.spliceAll` ? → ω (P5: countable data into a readable record).
- Reason-only (27, all still `?`): 5 `JsoniterTraversal` rows show their next blocker once `Span` reads; `Cents.from`
  names its private constructor; 6 abstract law givens name the possible views their siblings' searches give;
  `GetReplaceLens.to` / `modify` (a named argument is no mention, so `Optional.fused`'s result family adds its
  reason); 13 law rows whose sibling builds a class the reading cannot follow (`new DefaultRuleSet`, `ModifyF[X,
  A](…)`).
- Main reason movements (blocker categories 63 → 61): gone `abstract type or method-valued representation` (13) and
  `inert match type` (1); `inline method` 88 → 85; `overridable member may re-enter the measured declaration`
  143 → 161; `sealed subtype hides a parameter` 21 → 23; `opaque representation is not visible at use site` 28 → 30.
- 0.16.0: 4 unreviewed rows ? → 1 (`pickFoldMiss`, the three `andThen`s — the same definitions and derivations),
  headline 147 / 20 / 477 → 151 / 20 / 473; methods TSV byte-identical to the lane's own 0.16.0 run.

#### Review findings and resolution

- **Round 0 → 1** (all reproduced with the reviewers' probes, none rebutted): loose mentions and constructions of a
  local target (S2, wrong count), a holder's declared type (S4, wrong count), case-leaf parents resolved where the
  clause is written with one stdlib table for both sides (S10), deferred givens (P4), `given … with` objects as static
  owners (S1), the non-final case-class reason, plus quality (provenance-tracked view refusals, `ViewMode`, removed
  forwarders), test gaps and docs.
- **Round 1 → 2** (all reproduced, none rebutted; 13 eo rows reason-only, no count moved): binder-headed extension
  receivers (P2), a member's own binder shadowing a class argument (P3), extension members and second-clause fields
  of case leaves and final overrides in an intermediate parent (S10), subclass initializers and creator applications
  (S2), literals searching at a type they are not (S3), local holders of a hidden capture (S4; corrected
  `EnvironmentCompletenessSpec.hiddenCapture` — a Scala 3.9.0 run returns the outer seed), D29 → D31 docs (the {0, 1}
  restriction is a choice), quality (enums for nested Options, `contextAt`, pure `FileParents`), test gaps.
- **Landing:** three replay conflicts — `SealedFamilies.summand` (Z1b's `Child.built` over Z1a's optional statement
  view: merged through one local view, so `built` reads what Z1b's total view gave), `Scope.Child`'s doc comment (both
  sentences kept), the register (Z1b holds D29/D30, so the view sandwich is renumbered D31 everywhere); no head line
  lost. The merge raised CPD to 3 (matching import headers of `Inheritance` and `SealedFamilies`):
  amended — `FileParents` to its own file, the field list both sides computed to one helper
  `Overrides.inheritedFields` (identical to Z1b's inline filter). Behaviour unchanged (three-way check), CPD 2.

#### Expectations changed

- **Owner-reviewed:** none. `CanGet.get` 0, the five 0.16.0 counts, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec and the scripted fixture unchanged.
- Other (lane, derived in the notes §5): `EnvironmentCompletenessSpec.privateConstructor` reason only (now the
  non-public-constructor reason); `hiddenCapture` `Out.outer.C.g` / `C.seed` 1 → ? (a sibling local class reads the
  hidden seed: true count 2; twin `H.g` = 1 pinned); `OwnersAndRecordsSpec.caseRefusals`, `ReportHygieneSpec`,
  `StoredValueSoundnessSpec.notClosed` and `CapabilitySpec.openSteps` reason-only. No test expectation changed at
  landing.

#### Open items (`?`)

- ? Possible views past {0, 1} (agreeing bounds past 1 left undecided by choice), loosely read mentions, lambdas
  applied in place, constructions of classes the reading cannot follow.
- ? A holder that is a function of a hidden capture, a local object or class holding one, a field a nearer level
  overrides.
- ? P2 stops at direct parents and invariant receivers (covariant or indirect bases keep the fallback reason).
- ? P6 (singleton goals under an unresolved path, `None` / `Nil` in the stdlib table) not done.
- ? A record input with no instance only through a stuck projection under a member's own binder is not read as
  vacuous (`Rec3.h` reads 2, true 1; pinned `!= 4`) — needs a solver rule "an input whose closed type has no
  implementation makes the count 1".
- ? C6 stays receiver-relative for calls on other instances (`mkE().h(true)`).
- ? README sentence for S1's static-owner reading left to the orchestrator; CodeScene not runnable here.

### Z3 — Documentation: final ledger, bibliography, contract statements; throws a handler may catch (landed)

*Not ported:* this commit landed on the session's branch only; the owner discounts throws (D-pure),
so its rules never reached main.

- **Landed:** `7a37804` on integration `ccr-d57d9679-dtp3fh` (previous `7e3e306` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3 + A4 + B3 + A5 + the no-side-effects docs + B4 + C4 + A6 + B5 + A7 + Z1b + Z1a), compare-and-swap of the
  reviewed lane commit (fix round 2) unchanged — its base was still the head, so no rebase. Gates:
  `runs/7a378045ec416ef41b3116d26b1149f1b20c990a` — test 77/77 suites (head 75 + `CaughtThrowsSpec`,
  `ThrowOperandsSpec`), scalafmt, scalafix, file metrics (two pre-existing comment-share warnings), EoCheckout PASS;
  CPD 2 (head 2). `EoBaseline` 0.16.0 (`runs/7a37804…-eobaseline`): "Five reviewed model counts and input-order checks
  passed", TSVs byte-identical to the head's; `sbt plugin/scripted` 3/3. Details: `notes/Z3.md` (§1 brief items,
  §3 every printed number with its source, two review rounds, "Landing").
- **Docs** (integrated-review brief D1–D16): final ledger `docs/baselines/eo-checkout-2026-11-09-full.md` (run at
  `7e3e306`; per-commit history, unresolved rows classified 1,398 by design / 248 analyzer gaps, levers, known
  limits, independent verification: 443-row full audit, 222 delta-audited rows, 72/72 integrated review);
  `docs/research/bibliography.md` (every cited source with its verification level, the 67 metadata-verified papers as
  "the abstract reports" / background, 12 unverified records, METADATA corrections); register vocabulary and level
  corrections (Friedman, Dougherty & Subrahmanyam → classical; Mitchell & Plotkin classical with metadata-verified
  record), primary-text evidence D3–D23, cross-check appendix X1–X12 + decision labels; v1 §2 assumptions (throwing
  ignored, universal members excluded on every type, helper bodies obey the model, no synthesized evidence names,
  constructor-row output meanings), v1 §5/§9 numbers and backlog from the run; README/AGENTS inventory, toolchain,
  reports sample, rows beyond defs, EoDelta; `sbt siteRender` fixed (174 bracket citations Laika read as link ids).
- **Rules** (soundness fixes found by the review; register D26 point 2 + boundary, D27 decision; derivations
  `docs/research/pure-code.md`, `syntax-features.md`): an inline method counted as a def skips a throw only where it
  passes on to the method's end (blocks, conditionals, matches, strict values/arguments/receivers of supplied methods,
  tuples, ascriptions, handlers, finalizers, a handler-less `try`); elsewhere — under a handler, in a by-name/inline or
  unresolved callee's argument, an `inline` parameter's default where the body reads it, a lambda, an eta-expanded
  inline method, a nested definition, local template or lazy value — it is read, refused with "in a throw this
  reading cannot place outside every handler" (Scala 3.9.0 runs: x/y at the two call sites, the analyzer had 2).
  A throw's operand is read when collecting what a member holds (had 2^(2^32), true 2^(2^33)). A construction whose
  throw may carry the instance (`this`, own name, a member, an inner `new`) is refused ("construction throws a value
  that may hold the instance: C"; had 1, true 4).

#### Measured delta (vs 7e3e306)

Headline unchanged: **295 finite · 133 ω · 1646 ?** (2074 signatures), 435 stored rows (112 `?`). **No count and no
reason changed**: methods `+0 -0 ~0`, stored values `+0 -0 ~0`; methods, definitions and blockers TSVs byte-identical
to the head's run, so the ledger's numbers (recorded at `7e3e306`) hold for the landed analyzer. Each fix-round rule
measured alone was also empty (`runs/Z3-fix1-step1`, `-step2`, `runs/Z3-fix2`). Blocker categories 61, unchanged.
0.16.0: 644 → 644 (151 · 20 · 473), byte-identical. No integration delta.

#### Review findings and resolution

- **Round 0 → 1** (10 findings, all confirmed by probes / Scala 3.9.0 runs, none rebutted): caught throws in inline
  bodies (wrong count; the hinted "body has a `try`" rule was insufficient — by-name/lambda/callee/local/lazy/
  expression-handler shapes added) and the same flaw in `FinalImplementations.readNames` (throw operands); docs —
  universal members on every type, constructor rows reading throwables as open, session-only labels in tests/docs,
  split backtick spans, commit-message over-claim, ledger pointers, register appendix attribution, D30 anchor,
  stale toolchain statements.
- **Round 1 → 2** (13 findings, all confirmed, none rebutted): inline parameter defaults (wrong count; walked where
  the body reads the parameter, keeping the documented control at 3), eta-expanded inline methods (wrong count),
  construction throw carrying `this` (wrong count → refusal), misleading by-value argument / tuple / selection
  reasons (fixed with precision: strict calls walk strictly, `byValueArg` / `tuple` back to 2), ending and caught
  positions pinned, quality (`attempt` arity, `InlineBodies.Key`, one `MemberSyntax.lazyValue`, BodyWalk split into
  `BodyMeanings` 315 + `BodyWalk` 585 lines, specs split under 800 lines).

#### Expectations changed

- **Owner-reviewed:** none. `CanGet.get` 0, the five 0.16.0 counts, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec and the scripted fixture unchanged.
- Other: `PureCodeSpec.throwOperand` `M2.mk.F.m` 2 → refused ("final implementation reads a hidden capture: a1";
  moved to `ThrowOperandsSpec`) — precision given up for soundness (true count 2; telling apart a throwable no handler
  reads needs every handler that may run). `InlineMethodsSpec.caughtThrows` moved to `CaughtThrowsSpec` with the
  reason phrase reworded, same counts.

#### Open items (`?`)

- ? Main-code comments still name session fixtures by bare labels (`review-A6 h3`, `probes/review-A7/r11`,
  "capabilities C5", ~20 files Z2 restructures) — post-Z2 comment pass.
- ? When Z2 lands: re-apply its `BodyWalk` edits (`inlineMethod`, `anonymousFrame`, ordinals) in `BodyMeanings`;
  regenerate the ledger (`z3/make_ledger.py`) and re-check README/v1 numbers, the blocker list, the lazy-cycle and
  sample reasons and the secondary-constructor wording (`<init#k>`); the EoBaseline header's "sbt 2.0.9, Scala 3.8.4"
  (Z2 Q14).
- ? Precision given up (all `?`, none on eo): a throwable built from a hidden capture refuses even with no handler
  reading it; a construction-time throw naming any member of the level refuses (no types to rule the instance out);
  arguments of library members / function values stay caught (strictness unknown); a non-inline default of an
  inline method is still walked for compile-time operations although compiled at the definition.
- ? CodeScene not runnable here.

### Z2 — Quality cleanup: duplication to zero, one lookup order, stable ordinals, typed refusals (landed)

*Not ported:* this commit landed on the session's branch only; the owner takes row names from
TASTy, so its names and lookup changes never reached main.

- **Landed:** `6c384ff` on integration `ccr-d57d9679-dtp3fh` (previous `7a37804` = A1 + B0 + C1 + A2 + B1 + C2 + A3 +
  B2 + C3 + A4 + B3 + A5 + the no-side-effects docs + B4 + C4 + A6 + B5 + A7 + Z1b + Z1a + Z3), compare-and-swap after
  rebasing the reviewed lane commit `07560d1` (fix round 2, base `7e3e306`). Gates:
  `runs/6c384fff761d3f0321cae23b752d842522b166a6` — test 79/79 suites (head 77 + `ReasonWordingSpec`,
  `SyntheticPathsSpec`), scalafmt, scalafix, file metrics (no warnings; head 2), EoCheckout PASS; **CPD 0** (head 2).
  `EoBaseline` 0.16.0 (`runs/6c384ff…-eobaseline`): "Five reviewed model counts and input-order checks passed", TSVs
  byte-identical to the head's; `sbt plugin/scripted` 3/3. Details: `notes/Z2.md` (§1 items, §2 delta, §3
  derivations, two review rounds, "Landing").
- **What landed** (no counting rule; refactor, names, lookup fixes, reason wording; derivations
  `docs/research/definition-inventory.md` Identities, `environment-readings.md`, `stdlib-model.md`,
  `higher-kinded-binders.md` R2): CPD 2 → 0 and comment share under the warning line (Q1, Q2); one inline predicate
  `MemberSyntax.inlineModifier` / `inlineMethod` (Q3); one term-lookup precedence `resolution.TermPrecedence` read by
  both `TermBindings` and `BodyTerms` (Q7); dead code removed (Q8); typed family refusals `types.FamilyRefusal`,
  `Definition.Unbound.Unsummed(family, because: Option[SealedFamilies.Why])` (Q9); `SourceReader.scala` split out
  (Q12); runners print the toolchain they read (Q14). Row names: `<given#k>` / `<extension#k>` by ordinal among the
  file's bodies sharing the owner's path (class + companion, local class + local companion numbered together,
  `SyntheticNames`), secondary constructors `<init#k>` (Q5, Q6). Reason wording: lowercase heads, `head: subject`,
  named callables, local-elsewhere and enum-case wordings, a singleton path's wildcard trace only when nothing outranks
  it (Q10, Q11 a/d/e). Lookup soundness the new names exposed (none on eo): names anchored only at package paths and
  the root, synthetic-segment paths read from the reference's own file, overloads' local objects exporting a name
  differently refused, a self type's possible member refuses a package's or import's reading (pre-existing 1, true 4).

#### Measured delta (vs 7a37804)

Headline unchanged: **295 finite · 133 ω · 1646 ?** (2074 signatures), 435 stored rows (112 `?`). **No count changed.**
Gate delta `+175 -175 ~17` (its script joins only `@L` owners); ordinal-aware `+1 -1 ~28`, identical line for line to
the lane's delta against `7e3e306`: 174 renamed anonymous owners + `data.TraverseBuf.<init>` → `<init#1>` (ω, both),
28 reason-only (9 kyo `Frame` "declared in the sources only local to another body", 8 kyo singleton traces
`(may be imported by _root_.kyo.*)`, 5 `higher-kinded abstract type member: rc#Out`, 3 constrained receivers, 2 merging tops,
6 named callables, 1 array head, 1 channel head; 7 rows carry two). Stored values `+0 -0 ~0`. Blocker categories 61 →
61, six renamed with the same counts (`Higher-order application …` → lowercase, `Elimination of an opaque …`, `Array
of an abstract …`, `builtin names may be shadowed …` → `a name may be bound through a reference not supplied`, the `_`
placeholders filled). Methods/definitions/blockers TSVs byte-identical to the lane run `runs/Z2-fix2`; the head's TSVs
are byte-identical to `7e3e306`'s, so **no integration delta**. 0.16.0: 644 → 644 (151 · 20 · 473), byte-identical
(`EoBaseline.anonymousOwners` drops the ordinal).

#### Review findings and resolution

- **Round 0 → 1** (14 findings, all confirmed by probe, none rebutted): ordinal names read through another file's or a
  companion's namesake (wrong count, g2/g3 4 vs true 1; also a pre-existing class/companion defect) → package-path
  anchoring + same-file synthetic paths; misleading singleton trace → `Resolver.doubtedHead`; class/companion shared
  ordinals; text-matched "typed" refusal → cause carried typed; test gaps (given-instance dedup, named callables);
  "not declared" for a class local elsewhere; silent `<new>` fallback; naming/argument-count quality; docs.
- **Round 1 → 2** (12 findings, all confirmed, none rebutted): local/anonymous class + companion sharing `<given#k>`
  (wrong count, 4 vs 1) → anchor-keyed numbering; overloads' local objects (first export won, 4 vs 1) → refused
  `ambiguous type`; singleton path through another file's given; trace through self-type members / enum cases / inner
  imports, plus a pre-existing false `import E.*` channel; self-type member losing to a package type (pre-existing, 1
  vs true 4) → refused `self type member not resolved`; enum-case wording; test gaps, docs, quality.
- **Landing:** one conflict, `BodyWalk.scala` (Z3 moved the name/receiver reading into `BodyMeanings` and added
  caught-throw handling). Kept Z3's `attempt` / `thrown` and `inlined(_, scope.caught || expanded)`; re-applied Z2's
  edits in `BodyMeanings` (`anonymousFrame` with the index's `<new#k>` name now lives there, taking the `Resolver`;
  `strict` and `body`'s inline-parameter split use `MemberSyntax.inlineModifier`). No test dropped; behaviour verified
  identical (TSVs byte-identical to both sides' runs).

#### Expectations changed

- **Owner-reviewed:** none. `CanGet.get` 0, the five 0.16.0 counts, EoCaseLedgerSpec, OpaqueTypeScopeSpec,
  ReferenceInhabitantsSpec, ArticleCardinalitySpec and the scripted fixture unchanged.
- Other: names only (`<given@L>` / `<extension@L>` → `#k` in DefinitionInventorySpec, EnvironmentSoundnessSpec,
  InferredResultsSpec, InlineMethodsSpec, StoredScopesSpec, EoArtifactsSpec) and Q10 wordings (GroundTypesSpec,
  StdlibModelSpec, TypeMemberSpec, TopTypesUnionSpec, SequenceEliminationSpec, RepeatedParameterFragmentSpec,
  InhabitationSpec, NeutralSaturationSpec, TerminalHigherOrderSpec, ReportHygieneSpec). No count expectation changed.
  None changed at landing.

#### Open items (`?`)

- ? Q11 (c) not done: a refinement pinning a record's type member reads as an open record member or as an unerased
  refinement depending on its spelling (22 eo rows).
- ? Ordinals restart per file (3 eo owners share a name across files; unique with the source column); overloads' local
  definitions share paths (refused where they disagree); a self type's members are refused, not read; an enum class
  case named in its own body stays `enum case type not read`.
- ? `ShadowedBuiltins.shadows` still recognizes its refusal by text (input is a `Resolved` string).
- ? FOLLOWUPS 26 (export-bound path head, wrong count 1 vs true 4, pre-existing) and 27 (import through a local object
  reason) open.
- ? Orchestrator: regenerate the ledger `docs/baselines/eo-checkout-2026-11-09-full.md` (blocker table spells the six
  old heads), re-check README/v1 sample reasons and `<init#k>` wording, `eo-core-0.16.0.txt`'s old toolchain header;
  patch `design/eo-delta.py:34` to `<(extension|given)[@#]\d+>`; mark FOLLOWUPS 22, 23 closed.
- ? Main-code comments still cite session fixture labels (Z3 open item); CodeScene not runnable here.


## 5. Literature behind the counting rules (2026-11-01)

At the owner's request ("do research and find relevant papers"), a literature pass ran beside the
lanes: four searchers (proof counting; values; types; objects), a verifier, a bibliographer, an
adversarial cross-check of every rule against the sources, and a second search-only pass that
confirmed the papers' metadata.

**Access limits, stated honestly.** The container's network policy refuses arXiv, DOI, ACM,
Cambridge, HAL, Dagstuhl, Oracle and scala-lang.org, so no peer-reviewed paper was read in full.
What was verified, at the register's house levels:

- **Inspected (full text):** 32 primary texts read through `raw.githubusercontent.com` — the
  Scala 3 and 2.13 specification chapters on types, names and classes, the Scala 3 reference
  pages (union types, `Matchable`, opaque types, open classes, export clauses, context bounds,
  implicit resolution, context functions, match types, macros), SIP texts at a pinned commit, the
  OpenJDK 21 `Double` / `Float` / `Math` javadoc sources, and Scala library and compiler sources.
  These are the main evidence for every language-level rule.
- **Metadata-verified:** 67 papers whose title, authors, venue and year were confirmed by search
  results from authoritative pages (publisher, conference program, dblp, institutional pages).
  Their content was not read; where the abstract, as shown by the search, states the point relied
  on, the docs say "the abstract reports …"; otherwise the paper is cited as related work only.
  12 more candidates could not be confirmed and are not cited.
- **Classical:** the Yoneda lemma, undecidable System F inhabitation, the Ben-Yelles counting
  algorithm and the like, as the register already treats Turing 1936.

**Papers behind each rule** (all Metadata-verified; the Scala and JDK texts stay primary):

| Rule | Papers |
|---|---|
| Proof counting, absurd elimination, sum splitting | Scherer, *Deciding equivalence with sums and the empty type*, POPL 2017; Scherer & Rémy, *Which simple types have a unique inhabitant?*, ICFP 2015; Scherer's thesis 2016; Altenkirch, Dybjer, Hofmann & Scott, LICS 2001 |
| Productive cycles ⇒ ω, finiteness | Espírito Santo, Matthes & Pinto, TYPES 2020 (LIPIcs 2021) and MSCS 2019; Hindley, *Basic Simple Type Theory* ch. 8 (Ben-Yelles counting); Urzyczyn, TLCA 1997 (PSPACE-completeness, for the search budgets) |
| Ground scalars, float equality | OpenJDK `Double`/`Float`/`Math` (Inspected); JLS SE 21 ch. 4 (edition record); Goldberg 1991; Shannon 1949 |
| `Any` terminal; universal members and type analysis break parametricity | Johann & Voigtländer, POPL 2004 (`seq`); Washburn & Weirich, LICS 2005; Neis, Dreyer & Rossberg, ICFP 2009; Stucki, Brachthäuser & Odersky, GPCE 2021 (macros) |
| Unions as disjoint sums | Scala 3 reference and spec (Inspected); Frisch, Castagna & Benzaken, LICS 2002; Liu, SCALA 2016; Blanvillain et al., POPL 2022 |
| Higher kinds; context bounds as evidence | Atkey, CSL 2012; Voigtländer, ICFP 2009; Oliveira, Moors & Odersky, OOPSLA 2010; Schrijvers et al., COCHIS, JFP 2019 (ambiguity and coherence) |
| Abstract type members, paths | Mitchell & Plotkin, TOPLAS 1988; Hu & Lhoták, POPL 2020 (bad bounds — why bounded members are refused); Rompf & Amin, OOPSLA 2016; Cardelli & Leroy 1990 |
| Match types; sealed families and GADTs | Blanvillain et al., POPL 2022; Parreaux, Boruch-Gruszecki & Giarrusso, Scala 2019 (GADT soundness holes); Kennedy & Russo, OOPSLA 2005; Liu 2016 |
| Objects as records; open classes | Cook, Onward! 2009; Cook & Palsberg, OOPSLA 1989 (late binding leaves overridable members open); Pierce & Turner, JFP 1994 |
| Name binding | Scala specification ch. 2 (Inspected); Neron, Tolmach, Visser & Wachsmuth, ESOP 2015 |
| Mutable state excluded | Pitts & Stark 1998; Ahmed, Dreyer & Rossberg, POPL 2009 |
| Yoneda reduction of polymorphic members | Pistone & Tranchini, CSL 2021; Plotkin & Abadi, TLCA 1993; Urzyczyn 1997 (why only exact reductions or 0/1 are accepted) |

**What the cross-check changed.** Twenty rules were confirmed consistent with the sources. Twelve
issues were raised, each backed by a Scala 3.9.0 compiler fixture or an analyzer probe:

- X1 (wrong count already landed): an `export` defines members (Scala 3 reference), but the name
  resolution ranked it as an import, so an outer definition won. `g(t: T): Boolean` counted
  2^(2^32) where the true count is 4 (re-confirmed by the orchestrator).
- X2–X6 (planned rules unsound as worded): the sequence rule's ω clause (weakening is not monotone
  with the empty type), ω-saturation (absurdity must be checked on every node of the path),
  context functions (their parameters are deeper givens), concrete reachable code (overridable
  members of open classes are open capabilities), and record readings (a class whose constructor
  needs `Nothing` has no instance).
- X7–X12 (justification and wording): a coverage lemma, citation corrections, `Math.copySign` as
  a second NaN-sign observation, synthetic evidence names as a stated model assumption, a
  consistency premise, a superseded line.

Each was made binding on its owning feature before implementation (SYNTHESIS "Cross-check
assignments"); the outcome of each is recorded with the feature that addressed it in section 4 and
summarized in the PR. Files: `research/BIBLIOGRAPHY.md`, `research/METADATA.md`,
`research/CROSSCHECK.md`, `research/register-drafts.md` (scratchpad); the register entries go into
`docs/research/code-cardinality-foundations.md` with the final docs commit.

Re-checking the papers in full needs these hosts allowed in the environment's network settings:
`arxiv.org`, `doi.org`, `dl.acm.org`, `inria.hal.science`, `drops.dagstuhl.de`,
`www.cambridge.org`, `docs.scala-lang.org`, `docs.oracle.com`.

## 6. The documentation commit on main (2026-11-14)

The owner stopped the port of `7a37804` and `6c384ff` on 2026-11-14 and kept their documentation.
The commit that added this log, on main `36cd3cd`, changes documentation and code comments only:

- the cats-eo ledger, regenerated from main's own run
  ([eo-checkout-2026-11-14-full.md](../baselines/eo-checkout-2026-11-14-full.md)): its history ends
  with `7e3e306` (= main `4eabd23`) and one row for main; every unresolved row is classified against
  main's contract (the closed world, rules B and F, lawful key equality); the known limits were
  re-derived by probe on main; the 2026-10-27 ledger stays as the historical record;
- the internal verification record [bibliography.md](../research/bibliography.md), and factual
  corrections to the site's [bibliography](../site/references/bibliography.md). A merge of the
  record's other sources into the site's bibliography was offered and not done: the public list
  keeps the owner's selection;
- the register's verification vocabulary, primary-text evidence, level corrections and cross-check
  appendix, with D26 and D27 as main has them;
- v1 §2's further assumptions and output meanings, §5 and §9 from the ledger, and V1.4's list of
  this work's register entries;
- README, AGENTS.md, the research and site pages where they no longer matched main, and the session
  paths in code comments, now session fixture labels.

Its commit message records the gate run.
