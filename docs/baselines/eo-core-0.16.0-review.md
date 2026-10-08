# EO 0.16.0: refreshed baseline and case review

## Result

The production analyzer at `726ec431bbcb1b9211622d70d32c2852a9562df7`, with the opt-in
baseline runner added in this change, reads the same checksum-pinned 53 sources:

| Inventory / result | Previously retained | Refreshed |
|---|---:|---:|
| Definition rows | 134 | 134 |
| Implementation signature rows | 480 | 480 |
| Finite implementation counts | 15 | 30 |
| Countably infinite implementation counts | 2 | 2 |
| Unresolved implementation counts | 463 | 448 |

All stored-value rows, including their counts and reasons, are unchanged. There is still
**no justified single exact cardinality for EO**: most implementation rows remain unresolved.
The fifteen additional finite rows reflect analyzer work already on main since the old
snapshot; they are not fifteen new results produced by this documentation change.

The old output is retained in
[Git history at 726ec43](https://github.com/Constructive-Programming/scala-cardinality/blob/726ec431bbcb1b9211622d70d32c2852a9562df7/docs/baselines/eo-core-0.16.0.txt).
Its header identifies an earlier PR #10 analyzer checkout, not the commit retaining it.

## Reproduce

From the repository root, with sbt 2.0.9 and the build's Scala 3.8.4:

```bash
mkdir -p target/eo-baseline
curl --fail --location \
  https://repo1.maven.org/maven2/dev/constructive/cats-eo_3/0.16.0/cats-eo_3-0.16.0-sources.jar \
  --output target/eo-baseline/cats-eo_3-0.16.0-sources.jar
sbt 'core/Test/runMain cardinality.EoBaseline target/eo-baseline/cats-eo_3-0.16.0-sources.jar target/eo-baseline 726ec431bbcb1b9211622d70d32c2852a9562df7'
```

The last argument records the **production analyzer revision**, not an automatically detected
Git revision. Update it when measuring changed production code; the retained run used unchanged
production code at the revision above. Use `docs/baselines` as the output directory only when
deliberately updating the retained artifacts.

The runner verifies SHA-256
`61907fc2f4e1c0892fa8723c7a014af942b825b64a916a9b53dd64b3f12dbfe3`, rejects parse/read errors,
checks unique method identities, compares complete method entries with reversed source input
order, and checks the five independently derived numeric cases below against the **full archive**.
It uses the same `Report.of` entry point as the plugin, without installing a consumer plugin.
The ordinary unit suite stays offline; downloading the archive is an explicit workflow.
During the negative checksum check, the Windows sbt 2 client printed the forked JVM failure
but returned zero after disconnecting. Automation should use a fresh output directory and
verify the expected artifacts, rather than trust only the launcher's exit code.

Artifacts:

- [Full report](eo-core-0.16.0.txt): implementation counts followed by stored-value estimates.
- [Method inventory](eo-core-0.16.0-methods.tsv): every implementation row and its review status.
  Its key is `(source, qualified_name, kind, enclosing_signature, signature)`. Overloads keep
  their signatures. Anonymous extension/given owners lose their line-number suffix and gain
  receiver/type/parameter context; line numbers are locations, not identities.
- [Declared-signature estimates](eo-core-0.16.0-signatures.tsv): one coarse legacy
  `Counter.sourceSignature` estimate per file, **not implementation counts**. This is the first
  retained aggregate-signature artifact; there is no previous artifact to compare numerically.

## Every changed numeric result

All fifteen transitions are `?` to a finite count. No previously numeric row changed its count.
The table identifies the rule allowing each transition, not an independent certification of
every new number. Except for `replace`, these rows remain `unreviewed` in the method inventory.

| Source / qualified member (under `dev.constructive.eo`) | Old → new | Supporting rule |
|---|---:|---|
| `CanModify.scala` / `CanModifyP.replace` | ? → 1 | Terminal higher-order application synthesizes `_ => b` for the supplied `modify` capability. |
| `accessor/Accessor.scala` / `Accessor.tupleAccessor.get` | ? → 1 | Method-owned binders and direct inherited override identity; project the supplied pair. |
| `accessor/ReverseAccessor.scala` / `ReverseAccessor.eitherRevAccessor.reverseGet` | ? → 1 | Method-owned binders, direct override identity, and sum construction. |
| `data/Direct.scala` / `Direct.apply` | ? → 1 | Read the opaque representation where it is visible. |
| `data/Direct.scala` / `Direct.<extension>.value` | ? → 1 | Read the opaque receiver's representation in its visibility scope. |
| `data/Direct.scala` / `Direct.accessor.get` | ? → 1 | Visible opaque representation and direct override identity. |
| `data/Direct.scala` / `Direct.reverseAccessor.reverseGet` | ? → 1 | Visible opaque representation and direct override identity. |
| `data/Direct.scala` / `Direct.applicative.map` | ? → 1 | Visible representation, direct override identity, and callable application. |
| `data/Direct.scala` / `Direct.applicative.pure` | ? → 1 | Visible opaque representation and direct override identity. |
| `forgetful/ForgetfulFunctor.scala` / `ForgetfulFunctor.directTuple.map` | ? → 1 | Method-owned binders, direct override identity, products and callable application. |
| `forgetful/ForgetfulFunctor.scala` / `ForgetfulFunctor.directEither.map` | ? → 1 | Method-owned binders, direct override identity, and certified opaque-sum forwarding. |
| `optics/AffineFold.scala` / `PickFold.<init>` | ? → 2 | Certified opaque-sum observation/forwarding in function-valued constructor inputs. |
| `optics/Modify.scala` / `Modify.<init>` | ? → 1 | Certified terminal higher-order application in constructor inputs. |
| `optics/Prism.scala` / `MendTearPrism.<init>` | ? → 1 | Certified opaque-sum observation/forwarding in constructor inputs. |
| `optics/Prism.scala` / `PickMendPrism.<init>` | ? → 2 | Certified opaque-sum observation/forwarding in constructor inputs. |

Diagnostic-only changes are important too. The old catch-all unresolved-type/unsupported-type
reasons have been refined into explicit obligations: polymorphic capability binders (104 rows),
unproved inherited override identity (78), preserved refinement constraints (24), inert match
types (15), missing refined members (7), and opaque visibility/stable-path/higher-kinded argument
guards. These are overlapping reasons, not additive buckets. Higher-order application no longer
appears as a blanket blocker; opaque sum-producing callable elimination remains a blocker in one
row, down from four. See the complete report diff for each row's retained/new/removed reasons;
a renamed or removed diagnostic is not by itself a new numeric answer.

Extension and given aggregate counting changes `Counter.sourceSignature`, whereas the
implementation report indexes extension methods and methods inside given bodies separately.
A given factory is not thereby a new implementation-report row. The two inventories answer
different questions and must not be summed together.

## Small independently derived case ledger

`model-reviewed` means the stated pure, total, parametric model has a derivation and the
full-source runner agrees. It is not a proof of an arbitrary Scala body's behavior. The model
excludes effects, divergence, exceptions, casts, reflection, `null`, and unrestricted runtime
type/equality tests. Algebraic laws are not silently imposed on supplied capabilities.

All method rows not marked `model-reviewed` remain `unreviewed`; all 134 stored-value rows also
remain unreviewed for exactness. The existing definition triage is a coverage ledger, not such a
proof. `Optic.to` below has a reviewed unresolved obligation, but no reviewed numeric count.

| Source | Owner / kind / signature | Analyzer | Independent result / status |
|---|---|---:|---|
| `CanGet.scala` | `CanGet` / declaration / `get(s: S): A` | 0 | 0; model-reviewed |
| `CanGetOption.scala` | `CanGetOption` / declaration / `getOption(s: S): Option[A]` | 1 | 1; model-reviewed |
| `CanPlace.scala` | `CanPlace` / declaration / `place(b: B): T => T` | 1 | 1; model-reviewed |
| `CanPlace.scala` | `CanPlace` / method / `transfer[C](f: C => B): T => C => T` | ω | ω; model-reviewed |
| `CanModify.scala` | `CanModifyP` / method / `replace(b: B): S => T` | 1 | 1; model-reviewed |
| `optics/Optic.scala` | `optics.Optic` / declaration / `to(s: S): F[X, A]` | ? | No number; reviewed obligation |

Owners above are relative to `dev.constructive.eo`; source paths are relative to
`dev/constructive/eo/` in the archive. Full names/signatures are in the method inventory.

### Derivations and relevant environment

1. **`CanGet.get`: 0.** `S` and `A` are distinct binders; the only input is `s: S` and there is
   no sibling producer of `A`. The companion's optic-derived factory requires supplied optic
   evidence, which this method has not received; merely importing a type does not supply a value.
2. **`CanGetOption.getOption`: 1.** The standard `Option` sum supplies `None`. `Some(a)` needs
   an unavailable `a: A`; hence only `s => None`.
3. **`CanPlace.place`: 1.** Eta-expansion exposes `b: B, t: T`; only `t` can produce `T`, giving
   `b => t => t`. Do not reintroduce the target through the concrete sibling `transfer`, whose
   body delegates to `place`. Its companion factory also requires evidence not supplied here.
4. **`CanPlace.transfer`: ω.** The distinct abstract sibling `place: B => T => T` is now a
   supplied capability. With `f, t, c`, let `u₀ = t` and `uₙ₊₁ = place(f(c))(uₙ)`. These are
   distinguishable across admissible supplied capabilities: for example choose `T` as unbounded
   natural numbers and `place` as increment, not a fixed-width integer whose additions wrap.
   Finite expression syntax gives a countable upper bound. The capability is opaque, not a known
   identity; do not impose an additional naturality law on it.
5. **`CanModifyP.replace`: 1.** The abstract sibling `modify: (A => B) => S => T` is supplied.
   With only `b: B`, its callback can only be `_ => b`. Apply the resulting `S => T` to `s`.
   There is no other `T` producer, and independent binders prevent feeding `T` back as `S`
   or `A`. This derivation confirms the newly resolved row.
6. **`Optic.to`: unresolved.** Associated abstract `X`, higher-kinded `F[_, _]`, and the
   member-bearing optic environment are not fully modeled. The archive-only run does not
   resolve Cats dependencies. Lack of a modeled producer is not a proof of zero.

[EoCaseLedgerSpec](../../core/src/test/scala/cardinality/analysis/EoCaseLedgerSpec.scala) makes the five numeric cases
and the unresolved guard executable in reduced relevant scopes. Three adversarial tests add a
captured `A`, a supplied `T => T`, or a fallback `T`; the answers must change. These are not
replacements for the runner's checks against the original archive.

## Recommended next rule

**Close inherited override identity gaps before broad higher-kinded counting.** The refreshed
report exposes 78 affected rows; including the measured implementation again as an inherited
supplied capability can invent inhabitants or cycles. That is a soundness issue, not a coverage
percentage target.

Use `PartialAccessor.eitherAccessor.getOption` as a scoped investigation case: its current
obligations include both inferred result typing and inherited override identity. Compare the
already resolved `Accessor.tupleAccessor.get`, preserve method-owned binders, substitute the
parent carrier, and prove declaration identity. Do not erase either obligation to obtain a
number. A regression should also distinguish a genuinely separate inherited capability from
the target and cover overloads/renamed binders. This is a recommendation, not a claim that this
case has already been counted.

After that, tackle declared polymorphic capability specialization (104 affected rows), starting
from `CanFold.headOption` and its sibling `foldMap[M]`. The latter's `Monoid[M]` evidence needs
an explicit dependency/environment model. Do not jump directly to a guessed `Optic.to` count.
`CanModifyP.replace`, originally a promising higher-order target, is already supported on main
and is now a reviewed regression rather than the next implementation task.
