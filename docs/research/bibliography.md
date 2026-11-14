# Bibliography: every source the counting rules cite

The public, curated list of sources is the site's [bibliography](../site/references/bibliography.md).
This page is the internal verification record behind it and behind the register. It lists, with
its full reference and verification level, every source the
[evidence register](code-cardinality-foundations.md) and the research pages cite: the numbered
primary texts (`[1]`–`[37]`), the claim clusters (`P1`–`P6`, `V1`–`V5`, `T1`–`T5`, `O1`–`O6`), the
classical results (`C-n`), the register's earlier sources, and the papers whose publication
records were confirmed. A level is never higher than what was checked; the levels are the
register's ([definitions](code-cardinality-foundations.md)).

## How the sources were checked

- **Verifier pass.** Twelve objects-cluster texts were read in full from their GitHub sources
  (`raw.githubusercontent.com`, the one host reachable from the research sessions): marked
  **Inspected (full text)** (verifier).
- **Bibliographer pass.** Twenty-one further primary texts were read in full the same way —
  OpenJDK 21 sources, Scala 3 reference and specification sources, SIP texts at a pinned commit,
  Scala library and compiler sources: marked **Inspected (full text) — bibliographer**.
- **Metadata pass.** No publisher or archive host was reachable, so no paper's text was read.
  A search-only pass confirmed the publication record of 67 of 79 candidate papers from
  authoritative pages (publisher and DOI records, dblp, conference programs, institutional
  repositories): **Metadata-verified**. Where a page says "the abstract reports", that is the
  abstract as the search rendered it — never a text read, never **Inspected**. The other 12 are
  **not verified** and not citable. DOIs are printed only where a record displayed them.
- **Classical** results are stated as results; where a record for one was confirmed, the record
  is metadata-verified and the result stays classical.

## 1. Primary texts

URLs for [14]–[31] are `https://raw.githubusercontent.com/scala/scala3/main/<path>` with the paths
given; [32] and [33] are `https://raw.githubusercontent.com/scala/scala/2.13.x/<path>`; the SIP
texts [1], [3], [4] and [8] are at improvement-proposals commit
`d649f6e6f333cd9232d85a12bd0445d18a673f10`. All were read on 2026-11-01.

- **[1]** **Doeraene, S.** (Aug 2023, initial draft). *SIP-56 — Proper Specification for Match Types*.
  scala/improvement-proposals, `content/match-types-spec.md` at commit
  `d649f6e6f333cd9232d85a12bd0445d18a673f10`.
  <https://raw.githubusercontent.com/scala/improvement-proposals/d649f6e6f333cd9232d85a12bd0445d18a673f10/content/match-types-spec.md>.
  **Inspected (full text) — bibliographer**, 2026-11-01.
  - At that commit the front matter reads "stage: implementation, status:
    waiting-for-implementation".
  - Boundary: a proposal text at a pinned commit, not the current specification. Its
    `provablyDisjoint` was later refined ([29]).
- **[2]** **Knvl, A.** (2018). *Counting type inhabitants*. Blog, archived
  <https://web.archive.org/web/20181221193229/https://alexknvl.com/posts/counting-type-inhabitants.html>.
  **Executable** (register D3, D4, D9; `ReferenceInhabitantsSpec`).
  - Boundary: worked examples. Laws and higher kinds are out of scope by its own statement.
- **[3]** **Leontiev, G.; Burmako, E.; Zaugg, J.; Moors, A.; Phillips, P.; Port, O.; Sabin, M.** (2014–2019).
  *SIP-23 — Literal-based singleton types*. scala/improvement-proposals `content/42.type.md` at
  `d649f6e`. **Inspected (full text) — bibliographer.**
  - The front matter reads "stage: completed, status: shipped".
  - Boundary: a Scala 2-era proposal. Use [29] for the Scala 3 value-set reading (null).
- **[4]** **Odersky, M.** (initial draft 13 Jan 2024). *SIP-58 — Named Tuples*. scala/improvement-proposals
  `content/named-tuples.md` at `d649f6e`, plus the Scala 3 reference page *Named Tuples*
  (`docs/_docs/reference/other-new-features/named-tuples.md`, main).
  **Inspected (full text)** (verifier). Lines 82–91 and 153 were re-read by the bibliographer.
  - Corrections kept: the `NamedTuple` object is `@experimental` in the SIP appendix. "Status is
    stale" is an inference.
- **[5]** **OpenJDK** (JDK 21 update releases). `java/lang/Double.java`, class and method javadoc.
  openjdk/jdk21u, branch master, read 2026-11-01.
  <https://raw.githubusercontent.com/openjdk/jdk21u/master/src/java.base/share/classes/java/lang/Double.java>.
  **Inspected (full text) — bibliographer.**
  - This re-verifies the rejected candidate `JDKDouble` from the source the API page is generated
    from.
  - Boundary: the JDK 21 API. The JLS's own value-set wording was not read.
- **[6]** **OpenJDK** (JDK 21 update releases). `java/lang/Float.java`. Same repository and date.
  **Inspected (full text) — bibliographer.** Re-verifies `JDKFloat`.
- **[7]** **OpenJDK** (JDK 21 update releases). `java/lang/Math.java` and `java/lang/StrictMath.java`,
  `copySign` javadoc. Same repository and date. **Inspected (full text) — bibliographer.**
  - Boundary: it establishes that the NaN sign is observable in a platform-dependent way. It says
    nothing about payload propagation.
- **[8]** **Osheim, E.; Vicente Cantero, J.** (initial draft 20 Sep 2017). *SIP-35 — Opaque types*.
  scala/improvement-proposals `content/opaque-types.md` at `d649f6e`. **Inspected (full text) —
  bibliographer.**
  - The front matter reads "stage: completed, status: shipped", with the note "implemented with
    some changes in Scala 3.0.0".
  - Boundary: its companion-scoped visibility is superseded by [21].
- **[9]** **Reynolds, J. C.** (1983). *Types, Abstraction and Parametric Polymorphism*. Information
  Processing 83, pp. 513–523. **Bibliography-verified** (register D3, via Reynolds 1984's
  reference list).
  - Boundary: an idealized calculus. Scala's universal members, casts and reflection are excluded
    by the model, not by the theorem.
- **[10]** **Reynolds, J. C.** (1984). *Polymorphism is not set-theoretic*. LNCS 173,
   DOI 10.1007/3-540-13346-1_7. **Abstract inspected** (register D2).
- **[11]** **Scala 2.13 standard library** (EPFL and Lightbend). `scala/reflect/ClassTag.scala`,
   scala/scala 2.13.x.
   <https://raw.githubusercontent.com/scala/scala/2.13.x/src/library/scala/reflect/ClassTag.scala>.
   **Inspected (full text)** (verifier).
   - Boundary: the 2.13.x file. That Scala 3 uses this library is not stated on the page.
- **[12]** **Scala 2.13 standard library.** `scala/ValueOf.scala`, scala/scala 2.13.x.
   <https://raw.githubusercontent.com/scala/scala/2.13.x/src/library/scala/ValueOf.scala>.
   **Inspected (full text) — bibliographer.**
- **[13]** **Scala 3 compiler.** `compiler/src/dotty/tools/dotc/transform/patmat/Space.scala`, header
   comment, scala/scala3 main.
   <https://raw.githubusercontent.com/scala/scala3/main/compiler/src/dotty/tools/dotc/transform/patmat/Space.scala>.
   **Inspected (full text) — bibliographer.**
   - Boundary: an implementation comment, not a specification. It does not cite Liu 2016, so it
     does not verify that paper.
- **[14]** **Scala 3 library.** `library/src/scala/deriving/Mirror.scala` (main), with the reference page
   *Type Class Derivation*, `docs/_docs/reference/contextual/derivation.md`, § "Mirror".
   **Inspected — bibliographer.** Header and the § "Mirror" opening were read.
- **[15]** **Scala 3 reference.** *Context Bounds*, `docs/_docs/reference/contextual/context-bounds.md`
   (main). **Inspected (full text) — bibliographer.** Re-verifies `Scala3ContextBoundsRef`.
- **[16]** **Scala 3 reference.** *Context Functions* and *Context Functions — More Details*
   (`contextual/context-functions.md`, `context-functions-spec.md`, main).
   **Inspected (full text)** (verifier).
   - Correction kept: the "shorthands", `apply` and erasure sentences are on the `-spec` page.
- **[17]** **Scala 3 reference.** *Macros*, *Compile-time operations*, *Inline*
   (`metaprogramming/macros.md`, `compiletime-ops.md`, `inline.md`, main).
   **Inspected (full text)** (verifier).
   - Correction kept: the `summonFrom` example also supports O2.
- **[18]** **Scala 3 reference.** *Match Types*, `docs/_docs/reference/new-types/match-types.md` (main).
   **Inspected (full text) — bibliographer.** Re-verifies `Scala3MatchTypesSIP56` (reference part).
   - Note: its disjointness list has four items and omits the sealed rule that [1] and [29] state.
- **[19]** **Scala 3 reference.** *The Matchable Trait*, `other-new-features/matchable.md` (main).
   **Inspected (full text) — bibliographer.** Re-verifies `Scala3Matchable`. All four rejected
   sub-claims are present.
- **[20]** **Scala 3 reference.** *Multiversal Equality*, `contextual/multiversal-equality.md` (main).
   **Inspected — bibliographer.** The introduction and the `CanEqual` definition were read.
- **[21]** **Scala 3 reference.** *Opaque Type Aliases* and *Opaque Type Aliases: More Details*
   (`other-new-features/opaques.md`, `opaques-details.md`, main). **Inspected (full text)**
   (verifier).
   - Correction kept: the "only transparent in the scope of private[this]" sentence on
     `opaques.md` contradicts the details page. Do not cite it.
- **[22]** **Scala 3 reference.** *Open Classes*, `other-new-features/open-classes.md` (main).
   **Inspected (full text)** (verifier).
- **[23]** **Scala 3 reference.** *Polymorphic Function Types*,
   `new-types/polymorphic-function-types.md` (main). **Inspected (full text) — bibliographer.**
- **[24]** **Scala 3 reference.** *Translation of Enums and ADTs*, `enums/desugarEnums.md` (main).
   **Inspected (full text) — bibliographer.** Re-verifies `Scala3EnumDesugarRef`.
- **[25]** **Scala 3 reference.** *TypeTest*, `other-new-features/type-test.md` (main).
   **Inspected (full text) — bibliographer.**
- **[26]** **Scala 3 reference.** *Union Types* and *Union Types — More Details*
   (`new-types/union-types.md`, `union-types-spec.md`, main). **Inspected (full text) —
   bibliographer.** Re-verifies `Scala3UnionTypes`.
   - Correction: the current wording is "includes all values of both types", not the searcher's
     "represents values that are either of the type A or of the type B". There are no
     disjointness rules.
- **[27]** **Scala 3 reference.** *Using Clauses*; *Changes in Implicit Resolution*
   (`contextual/using-clauses.md`, `changed-features/implicit-resolution.md`, main).
   **Inspected (full text)** (verifier).
   - Correction kept: the pages do contain same-type ambiguity *examples*, but no general
     same-level rule.
- **[28]** **Scala 3 specification (draft).** Ch. 2, *Identifiers, Names & Scopes*,
   `docs/_spec/02-identifiers-names-and-scopes.md` (main). **Inspected (full text)** (verifier).
   - Boundary: a draft on main, not versioned for 3.9.0.
- **[29]** **Scala 3 specification (draft).** Ch. 3, *Types*, `docs/_spec/03-types.md` (main). The
   verification record is split:
   - **Inspected (full text)** (verifier): literal types, term designators, stable types. The tool
     returned the first ~39,000 characters verbatim; the verifier read those sentences directly.
   - **Inspected (full text) — bibliographer**: "Union and Intersection Types", "Match Types",
     "Disjointness", "Reduction", and the lower-bound-rule remark. The whole file was saved
     locally (85,019 bytes).
- **[30]** **Scala 3 specification (draft).** Ch. 5, *Classes and Objects*,
   `docs/_spec/05-classes-and-objects.md` (main). **Inspected (full text) — bibliographer.** The
   member definitions, overriding, `final`, `sealed` and enum sections were read.
- **[31]** **Scala 3 specification (draft).** Ch. 8, *Pattern Matching*, `docs/_spec/08-pattern-matching.md`
   (main). **Inspected (full text) — bibliographer.** The type-patterns section and the erasure
   example were read.
- **[32]** **Scala Language Specification 2.13.** Ch. 2, *Identifiers, Names & Scopes*, scala/scala
   2.13.x `spec/02-identifiers-names-and-scopes.md`. **Inspected (full text)** (verifier).
   - Boundary: the inherited-below-local split is commented-out and not normative.
- **[33]** **Scala Language Specification 2.13.** Ch. 5, *Classes and Objects*, scala/scala 2.13.x
   `spec/05-classes-and-objects.md`. **Inspected (full text) — bibliographer.** The overriding,
   `final` and `sealed` sections were read.
- **[34]** **Scala Language Specification 2.13 and Scala 3 specification (draft).** Ch. 7, *Implicits*
   (`spec/07-implicits.md`; `docs/_spec/07-implicits.md`). **Inspected (full text)** (verifier).
- **[35]** **Smyth, M. B.; Plotkin, G. D.** (1982). *The Category-Theoretic Solution of Recursive Domain
   Equations*. SIAM J. Computing 11(4). **Bibliography-verified** (register D4).
- **[36]** **tudelft-pl.** *Statix Specification of Scala Imports* (`scala.mstx` README), master.
   <https://raw.githubusercontent.com/tudelft-pl/scala.mstx/master/README.md>.
   **Inspected (full text)** (verifier).
   - Boundary: the README's path order is the repository's own formalization. The associated
     paper (title, authors, OOPSLA 2020, DOI 10.1145/3428248) is **not** verified.
- **[37]** **Wadler, P.** (1989). *Theorems for free!*. FPCA '89, pp. 347–359, DOI 10.1145/99370.99404.
   **Metadata-verified** (register D3).
   - An abstract-level upgrade was proposed by the types searcher, but it is search-level only
     and is **not** applied here.

## 2. Earlier register sources

- Russell, B. (1907). *On Some Difficulties in the Theory of Transfinite Numbers and Order Types*.
  PLMS s2-4, pp. 29–53, DOI 10.1112/plms/s2-4.1.29. **Metadata-verified** (register D1, D2, D5).
- Stanford Encyclopedia of Philosophy, *Russell's Paradox*. **Inspected** (register D1).
- Luna, L. & Taylor, W. (2010). *Cantor's Proof in the Full Definable Universe*. AJL 9, DOI
  10.26686/ajl.v9i0.1818. **Abstract inspected** (register D1, D7).
- Fan, Z. (2020). *Hobson's Conception of Definable Numbers*. HPL, DOI
  10.1080/01445340.2020.1731784. **Bibliography-verified** (register D1, D7).
- Pitts, A. M. (1987). *Polymorphism is set theoretic, constructively*. DOI
  10.1007/3-540-18508-9_18. **Metadata-verified**, bibliography inspected (register D2).
- Milewski, B. (2014). *Parametricity: Money for Nothing and Theorems for Free* (blog).
  **Inspected** (register D3).
- Ertl, O. (2017). *New cardinality estimation algorithms for HyperLogLog sketches*.
  **Inspected** (PDF; register D6).
- Turing, A. M. (1936). *On computable numbers, with an application to the Entscheidungsproblem*.
  Proc. LMS s2-42 (1936–37), correction s2-43 (1937). The halting result is **classical**
  (register D7); the record is **not verified**.
- Girard, J.-Y. (1972). Strong normalisation of System Fω. **Classical** (register D14).
- Friedman, H. (1975). *Equality between functionals*. LNM 453. **Classical**; its publication
  record was not verified (register D14).
- Dougherty, D. & Subrahmanyam, R. (2000). *Equality between functionals in the presence of
  coproducts*. Inf. Comput. 157. **Classical**, not verified by the metadata pass (register D14).
- Vytiniotis, D. & Weirich, S. (2010). *Parametricity, Type Equality and Higher-order
  Polymorphism*. JFP 20(2), DOI 10.1017/S0956796810000079. **Metadata-verified (design memo
  session, not re-checked)** (register D14).
- Pierce, B. C. (1994). *Bounded quantification is undecidable*. Inf. Comput. 112(1).
  **Metadata-verified (design memo session, not re-checked)** (register D14).
- Scala 3 reference, *Export clauses* (`other-new-features/export.md`, main). **Inspected
  (extraction)** (cross-check step): "An export clause defines aliases for selected members of an
  object"; type members are aliased by type definitions, term members by method definitions.
- Scala 3 reference, *Implicit Conversions*. **Classical** here; not re-read (register D27).

## 3. Claim clusters

The research step grouped the rules' claims into clusters; a page that cites `T5` or `O2` points
here. Status is how far verified sources cover the claim; the register entry carries the
derivation and regressions that stand in where a source is missing.

| Cluster | Claim | Status | Register |
|---|---|---|---|
| P1 | Counting is of βη-normal (long, focused) proof terms; observational equivalence for STLC with ×, +, 0; an inconsistent context counts 1 | Partial | D3, D4, D25 |
| P2 | Deciding 0 / 1 / ≥ 2 and finite versus infinite | Partial | D4, D24 |
| P3 | Memoised goals and cycles: a reachable productive cycle gives ω, an unproductive one 0; recursive types are least fixed points; ω-saturation only in environments proven not absurd | Partial | D4, D25 |
| P4 | Expressible implementations are at most ℵ₀ | Covered, classical level only | D2 |
| P5 | A sum-producing call is eliminated once per path, without double counting | Not covered (project derivation, executable) | D25 |
| P6 | Exact symbolic big-number arithmetic | Not covered (executable) | D9 |
| V1 | Ground scalars are closed finite enumerations (`String` countable); a supplied ground value splits; finite functions are definable from literals and equality | Partial | D9 |
| V2 | `Float` and `Double` under the JDK `equals` classes: sizes `2^32 − 2^24 + 3` and `2^64 − 2^53 + 3` | Covered | D9 |
| V3 | The top type is observationally terminal once universal members are excluded, and those members break parametricity | Covered for Scala; type-analysis literature metadata only | D3, D12 |
| V4 | Unions are tagged sums only for pairwise provably disjoint runtime classes | Covered | D13 |
| V5 | Under the finite-program reading `c^ω ∈ {0, 1, ω}`, never `2^ℵ₀` | Partial (derivation on D2) | D2, D9 |
| T1 | Parametricity basis and the Yoneda reduction; polymorphic capability members instantiated only for exact reductions or 0/1 | Partial | D24 |
| T2 | Higher-kinded parameters are rigid uninterpreted constructors; context bounds are evidence | Partial | D14 |
| T3 | Abstract type members and paths are rigid existential atoms keyed by path; opaque types | Partial | D18, D30 |
| T4 | Match types reduce only past provably disjoint cases; otherwise stuck | Covered | D18 |
| T5 | Sealed families and enums are closed sums; GADT pins; exhaustivity | Covered for the language; GADT literature metadata only | D15 |
| O1 | An object is observed only through its members; overridable concrete members of open classes are capabilities | Partial | D20 |
| O2 | An anonymous `using` parameter is reachable only by implicit search; same-type candidates at one level are ambiguous | Partial (a model decision) | D10, D14 |
| O3 | A name denotes the declaration the language binds it to; precedence | Covered | D10, D11 |
| O4 | Macros, inline and compile-time type evidence reveal types; no parametric count | Covered | D23, D27 |
| O5 | Mutable state changes observational equivalence, so the model excludes mutation and reads a `var` as its value | Not covered (a model decision) | D26 |
| O6 | Scala 3 references for named tuples, literal singleton types, opaque types and context functions | Covered | D21, D22, D30 |

## 4. Classical results

- **C-1.** The finite texts over a finite alphabet are countable (register D2).
- **C-4.** Every function on a finite domain is definable by a decision tree from literals and
  equality: the Boole/Shannon expansion (register D9). Record: Shannon 1949, *The synthesis of
  two-terminal switching circuits*, BSTJ 28:59–98, **metadata-verified**; Boole's priority stands.
- **C-5.** Finite exponentials `|B^A| = |B|^|A|`; in a cartesian closed category `Γ × 0 ≅ 0`, and
  the initial object has exactly one map to every object (the absurd rule).
- **C-6.** The Yoneda lemma. Its parametric-model form for polymorphic types is a derivation with
  register D3, not a citation.
- **C-7.** Inhabitation in System F is undecidable. Records: Löb 1976 (JSL 41(4), DOI
  10.2307/2272390) and Urzyczyn 1997 (TLCA, LNCS 1210, DOI 10.1007/3-540-62688-3_47), both
  **metadata-verified**; the abstract of Urzyczyn 1997 reports the result, and that STLC
  inhabitation is PSPACE-complete (the reason for the solver's budgets, register D8).
- **C-8.** A context-free language is infinite iff, after removing useless symbols and ε/unit
  productions, some reachable, generating nonterminal derives itself with non-empty context
  (the Hopcroft–Ullman textbook test).
- **C-9.** Ben-Yelles 1979: for simple implicational types, whether the number of closed β-normal
  inhabitants is finite is decidable, and the count is computable. Records: Hindley 1997, *Basic
  Simple Type Theory*, ch. 8 "Counting a type's inhabitants", and the Ben-Yelles 1979 thesis
  (the weakest record), both **metadata-verified**. Boundary: β-normal forms, implicational
  fragment, not βη.
- **C-10.** Mitchell & Plotkin 1988: abstract types have existential type. Record: TOPLAS 10(3),
  **metadata-verified** (singular title; the DOI was seen only in citing reference lists).
- **C-11.** Unique prime factorisation (register D9, `NaturalSpec`).

## 5. Papers: metadata-verified

Content is never claimed beyond "the abstract reports" (where the search-rendered abstract
states the point) or "background" (record only). Where an inspected primary text above or an
executable spec carries a rule, it stays the main evidence and the paper is supplementary.

### Proof counting and canonical forms

- G. Scherer, D. Rémy. Which simple types have a unique inhabitant? ICFP 2015, Vancouver, pp.
  243–255. **Metadata-verified**; the abstract reports the point cited. For: P2: 0/1/≥2 for STLC
  with sums under βη, decided by saturating focused search (summary). P1. The P5 "cut only new sums"
  passage was not shown.
- G. Scherer. Deciding equivalence with sums and the empty type. POPL 2017, Paris, pp. 374–386.
  arXiv:1610.01213. **Metadata-verified**; the abstract reports the point cited. For: P1: stated
  directly. P3's absurd-context caveat and P5's quasi-normal forms were not shown.
- G. Scherer. *Which types have a unique inhabitant? Focusing on pure program equivalence.* PhD
  thesis, Université Paris Diderot (Paris 7), defended 30 Mar 2016 (manuscript updated 7 Nov 2016).
  Advisor D. Rémy. **Metadata-verified**; the abstract reports the point cited. For: P2: stated. P1
  rests on a paraphrase only.
- J. R. Hindley. *Basic Simple Type Theory.* Cambridge Tracts in TCS 42, CUP, 1997. Ch. 8, "Counting
  a type's inhabitants". **Metadata-verified**; the abstract reports the point cited. For: P2, and
  the record behind C-9: Ben-Yelles' algorithm counts the closed normal forms of a type in TAλ,
  which also tests emptiness (paraphrase).
- R. David, M. Zaionc. Counting proofs in propositional logic. Arch. Math. Logic 48(2):185–199,
  2009. arXiv:0905.2880. **Metadata-verified**; background (record only). For: P3: normal proofs are
  what is counted. The least-fixed-point, integer-or-∞ result was not shown in this pass.
- J. Espírito Santo, R. Matthes, L. Pinto. Inhabitation in simply typed lambda-calculus through a
  lambda-calculus for proof search. MSCS 29(8):1092–1124, 2019 (online 17 Apr 2018).
  arXiv:1604.02086. **Metadata-verified**; the abstract reports the point cited. For: P2: decision
  procedures and counting functions for STLC inhabitation, implicational fragment (paraphrase).
- J. Espírito Santo, R. Matthes, L. Pinto. Coinductive proof search for polarized logic with
  applications to full intuitionistic propositional logic. TYPES 2020, LIPIcs 188, pp. 4:1–4:24,
  2021. DOI 10.4230/LIPIcs.TYPES.2020.4. arXiv:2007.16161. **Metadata-verified**; the abstract
  reports the point cited. For: P2: existence and finiteness of focused inhabitants are decidable
  for LJP and carry over to LJT, full IPL with disjunction (paraphrase). P5. These are focused
  inhabitants, not βη-classes.
- M. Takahashi, Y. Akama, S. Hirokawa. Normal proofs and their grammar. Inf. Comput. 125(2):144–153,
  1996. Conference version: TACS 1994, pp. 465–493. **Metadata-verified**; background (record only).
  For: P2: grammar of normal inhabitants. Second-hand only; no abstract was found.
- S. Broda, L. Damas. On long normal inhabitants of a type. J. Logic Comput. 15(3):353–390, 2005.
  DOI 10.1093/logcom/exi016. **Metadata-verified**; background (record only). For: P2, on the title
  only.
- G. Dowek, Y. Jiang. Enumerating proofs of positive formulae. Computer J. 52(7):799–807, 2009.
  Later arXiv posting: 2305.01440 (2023). **Metadata-verified**; background (record only). For: P2,
  P3: a grammar of schemes for the normal proofs of positive formulae, each scheme giving finitely
  many proofs (paraphrase).
- J. B. Wells, B. Yakobowski. Graph-based proof counting and enumeration with applications for
  program fragment synthesis. LOPSTR 2004 (Verona), LNCS 3573, Springer, 2005, pp. 262–277.
  **Metadata-verified**; background (record only). For: P3: proof counting is solvable by an
  algorithm (paraphrase). The cycle handling P3 needs was not shown.
- S. Lindley. Extensional rewriting with sums. TLCA 2007 (Paris), LNCS 4583, Springer, pp. 255–271.
  DOI 10.1007/978-3-540-73228-0_19. **Metadata-verified**; background (record only). For: P5:
  background on canonicity with sums. It has no empty type and does not state once-per-path
  elimination.
- T. Altenkirch, P. Dybjer, M. Hofmann, P. Scott. Normalization by evaluation for typed lambda
  calculus with coproducts. LICS 2001, pp. 303–310. DOI 10.1109/LICS.2001.932506.
  **Metadata-verified**; the abstract reports the point cited. For: P1: βη for STLC with strong
  binary sums is decidable, by NbE into a sheaf model (paraphrase). It has no empty type, so it does
  not cover the 0 case.
- T. Altenkirch, T. Uustalu. Normalization by evaluation for λ→2. FLOPS 2004, pp. 260–275.
  **Metadata-verified**; background (record only). For: P1, V1: the set-theoretic semantics of λ→2
  is complete, via decision trees (paraphrase).
- M. Fiore, R. Di Cosmo, V. Balat. Remarks on isomorphisms in typed lambda calculi with empty and
  sum types. APAL 141(1–2):35–50, Aug 2006. Conference version: LICS 2002, Copenhagen.
  **Metadata-verified**; background (record only). For: P1, V1: type isomorphism with ×, → and +
  (with or without 1 and 0) is not finitely axiomatisable (paraphrase). The "≅ 0 iff = 0" transfer
  appeared only in the values run's paraphrase.
- R. Di Cosmo, T. Dufour. The equational theory of ⟨N, 0, 1, +, ×, ↑⟩ is decidable, but not finitely
  axiomatisable. LPAR (11th), LNCS, Springer, 2005, pp. 240–256. HAL hal-00149562.
  **Metadata-verified**; background (record only). For: P6, loosely: decidable but not finitely
  axiomatisable (paraphrase). It concerns identities with variables, not closed towers.
- R. Statman. Intuitionistic propositional logic is polynomial-space complete. TCS 9(1):67–72, July
  1979. DOI 10.1016/0304-3975(79)90006-9. **Metadata-verified**; background (record only). For: P2
  and D8 budgets: deciding intuitionistic propositional validity is PSPACE-complete (paraphrase).
- C.-B. Ben-Yelles. Type-assignment in the lambda-calculus: syntax and semantics. PhD thesis,
  University College of Swansea, 1979. Advisor J. R. Hindley. **Metadata-verified**; background
  (record only). For: P2: the primary source behind C-9. The counting algorithm is attributed to it
  only through Hindley 1997 ch. 8.
- N. Ghani. βη-Equality for coproducts. In M. Dezani-Ciancaglini, G. Plotkin (eds.), TLCA '95, LNCS
  902, Springer, 1995, pp. 171–185. DOI 10.1007/BFb0014052. **Metadata-verified**; background
  (record only). For: P1, P5 background: the rewriting approach to deciding βη, extended to sums
  (paraphrase). It has no empty type.
- R. Statman. Completeness, invariance and λ-definability. JSL 47(1):17–26, 1982. DOI
  10.2307/2273377. **Metadata-verified**; background (record only). For: The abstract concerns
  Plotkin's problem of characterising λ-definable functionals (paraphrase). It does **not** state
  finite completeness, so it supports the ≥ direction of P1 only through a citing paper.

### Values, ground types, top and unions

- P. Johann, J. Voigtländer. Free theorems in the presence of seq. POPL 2004, Venice, pp. 99–110;
  also SIGPLAN Notices 39(1). Extended version: The impact of seq on free theorems-based program
  transformations, Fundamenta Informaticae 69(1–2):63–102, 2006. **Metadata-verified**; the abstract
  reports the point cited. For: V3, by analogy: one universal primitive is enough to break the
  standard free theorems. The Scala analogy is the memo's.
- K. B. Bruce, R. Di Cosmo, G. Longo. Provable isomorphisms of types. MSCS 2(2):231–247, 1992.
  **Metadata-verified**; background (record only). For: V1 background.
- R. Di Cosmo. *Isomorphisms of Types: from λ-calculus to information retrieval and language
  design.* Progress in Theoretical Computer Science, Birkhäuser, 1995. viii + 235 pp. ISBN
  0-8176-3763-X. **Metadata-verified**; background (record only). For: V1 background.
- M. P. Fiore. Isomorphisms of generic recursive polynomial types. POPL 2004, pp. 77–88.
  **Metadata-verified**; background (record only). For: V1 note: for recursive types, equal counts
  (ω) cannot decide isomorphism.
- C. E. Shannon. The synthesis of two-terminal switching circuits. Bell System Technical Journal
  28:59–98, 1949. **Metadata-verified**; background (record only). For: V1, and the record for C-4.
- J. C. Mitchell, G. D. Plotkin. Abstract types have existential type. TOPLAS 10(3):470–502, July
  1988. Preliminary version: POPL 1985. **Metadata-verified**; the abstract reports the point cited.
  For: V3, T3, and the record for C-10.
- G. Plotkin, M. Abadi. A logic for parametric polymorphism. In M. Bezem, J. F. Groote (eds.), TLCA
  '93, LNCS 664, Springer, 1993, pp. 361–375. DOI 10.1007/BFb0037118. **Metadata-verified**; the
  abstract reports the point cited. For: V3, T1: encodings under parametricity. The specific `A+B`
  instance is not stated.
- G. Neis, D. Dreyer, A. Rossberg. Non-parametric parametricity. ICFP 2009, Edinburgh, pp. 135–148.
  DOI 10.1145/1596550.1596572. Journal version: JFP 21(4–5):497–562, 2011. **Metadata-verified**;
  the abstract reports the point cited. For: V3, O4: stated directly.
- R. Harper, G. Morrisett. Compiling polymorphism using intensional type analysis. POPL 1995, pp.
  130–141. Technical report CMU-CS-94-185. **Metadata-verified**; background (record only). For: V3,
  O4: type constructors and terms are built by structural recursion on types (paraphrase). The
  abstract does **not** say that parametricity breaks.
- G. Washburn, S. Weirich. Generalizing parametricity using information-flow. LICS 2005, Chicago,
  pp. 62–71. Extended version: Penn technical report MS-CIS-05-04. **Metadata-verified**; the
  abstract reports the point cited. For: V3: run-time type analysis makes standard parametricity
  fail (paraphrase, stated directly).
- A. Frisch, G. Castagna, V. Benzaken. Semantic subtyping. LICS 2002, Copenhagen, IEEE Computer
  Society. **Metadata-verified**; background (record only). For: V4, at metadata level, in place of
  the JACM paper.
- O. Blanvillain, J. I. Brachthäuser, M. Kjaer, M. Odersky. Type-level programming with match types.
  PACMPL 6(POPL), 2022. DOI 10.1145/3498698. **Metadata-verified**; the abstract reports the point
  cited. For: V4, T4. The disjointness rule was **not** shown.
- F. Liu. A generic algorithm for checking exhaustivity of pattern matching (short paper). SCALA
  2016 (7th ACM SIGPLAN Symposium on Scala, SPLASH, Amsterdam), pp. 61–64. DOI
  10.1145/2998392.2998401. **Metadata-verified**; the abstract reports the point cited. For: V4, T5.
  The link to Scala 3's space engine is the memo's, not the abstract's.
- D. Goldberg. What every computer scientist should know about floating-point arithmetic. ACM
  Computing Surveys 23(1), March 1991. DOI 10.1145/103162.103163. **Metadata-verified**; background
  (record only). For: V2 background. No signed-zero or NaN passage was seen, so its content stays
  Classical.
- A. Fog. NaN payload propagation – unresolved issues. Background document, IEEE 754 working group
  (754-2019 revision), 2018. **Metadata-verified**; the abstract reports the point cited. For: V2
  sharpening: NaN payload propagation is not fully standardized, and the working group declined to
  change it (paraphrase).
- J. Gosling, B. Joy, G. Steele, G. Bracha, A. Buckley, D. Smith, G. Bierman. *The Java Language
  Specification, Java SE 21 Edition* (JSR 396, Final Release). Oracle, 2023-08-23. Ch. 4, Types,
  Values, and Variables. **Metadata-verified**; background (record only). For: V1 (§4.2.1), V2
  (§4.2.3). The §4.2 text was **not** shown, so its content stays "classical, recalled".

### Types: parametricity, higher kinds, members, match types, GADTs

- P. Pistone, L. Tranchini. The Yoneda reduction of polymorphic types. CSL 2021, LIPIcs 183, pp.
  35:1–35:22. DOI 10.4230/LIPIcs.CSL.2021.35. arXiv:1907.03481. **Metadata-verified**; the abstract
  reports the point cited. For: T1: quantifier elimination is stated. Finite counting up to
  contextual equivalence, and the "stronger than βη" boundary, were not shown.
- E. S. Bainbridge, P. J. Freyd, A. Scedrov, P. J. Scott. Functorial polymorphism. TCS 70(1):35–64,
  Jan 1990. Corrigendum: TCS 71 (1990), p. 431. **Metadata-verified**; background (record only).
  For: T1: record only.
- R. Hasegawa. Categorical data types in parametric polymorphism. MSCS 4:71–109, 1994. DOI
  10.1017/S0960129500000372. **Metadata-verified**; background (record only). For: T1: record only.
- P. Wadler. Recursive types for free! Unpublished draft manuscript, University of Glasgow, July
  1990 (typos fixed Aug 2008 and Oct 2014). **Metadata-verified**; background (record only). For:
  T1: PB1's exclusion of algebra premises. Weak initiality, and initiality under parametricity, were
  **not** shown.
- P. Urzyczyn. Inhabitation in typed lambda-calculi (a syntactic approach). In P. de Groote, J. R.
  Hindley (eds.), TLCA 1997 (Nancy), LNCS 1210, Springer, 1997. DOI 10.1007/3-540-62688-3_47.
  **Metadata-verified**; the abstract reports the point cited. For: T1, and the record for C-7:
  stated. It also states that STLC inhabitation is PSPACE-complete (the D8 budgets).
- M. H. Löb. Embedding first order predicate logic in fragments of intuitionistic logic. JSL 41(4),
  Dec 1976, starting at p. 705. DOI 10.2307/2272390. **Metadata-verified**; background (record
  only). For: T1, and the record for C-7. Reading it as "System F inhabitation is undecidable" goes
  through Curry–Howard; that step is an inference.
- A. Moors, F. Piessens, M. Odersky. Generics of a higher kind. OOPSLA 2008, pp. 423–438. DOI
  10.1145/1449764.1449798. **Metadata-verified**; background (record only). For: T2, for the
  language mechanism only. It makes no parametricity statement.
- R. Atkey. Relational parametricity for higher kinds. CSL 2012, LIPIcs 16, pp. 46–61. DOI
  10.4230/LIPIcs.CSL.2012.46. **Metadata-verified**; the abstract reports the point cited. For: T2:
  stated for Fω, not for Scala.
- J. Voigtländer. Free theorems involving type constructor classes: functional pearl. ICFP 2009,
  Edinburgh, pp. 173–184. **Metadata-verified**; the abstract reports the point cited. For: T2: the
  `F[_]: Monad` setting is stated.
- B. C. d. S. Oliveira, A. Moors, M. Odersky. Type classes as objects and implicits. OOPSLA 2010
  (SPLASH, Reno/Tahoe), pp. 341–360; SIGPLAN Notices 45(10). DOI 10.1145/1869459.1869489.
  **Metadata-verified**; the abstract reports the point cited. For: T2, O2: type classes encoded
  through implicits is stated. The context-bound desugaring is not.
- T. Schrijvers, B. C. d. S. Oliveira, P. Wadler, K. Marntirosian. COCHIS: Stable and coherent
  implicits. JFP 29, e3, 2019 (online 8 Feb 2019). DOI 10.1017/S0956796818000242.
  **Metadata-verified**; the abstract reports the point cited. For: T2 (R4's ambiguity rule), O2.
- L. Cardelli, X. Leroy. Abstract types and the dot notation. DEC SRC Research Report 56, 10 Mar
  1990. Also in IFIP TC2 Working Conference on Programming Concepts and Methods (Tiberias, April
  1990), North-Holland, pp. 479–504. **Metadata-verified**; background (record only). For: T3. "Two
  openings give non-matching abstract types" was **not** shown.
- M. Odersky, V. Cremet, C. Röckl, M. Zenger. A nominal theory of objects with dependent types.
  ECOOP 2003 (July 2003), Springer LNCS. Also FOOL 10 (Jan 2003); EPFL technical report IC/2002/070.
  **Metadata-verified**; background (record only). For: T3: types as object members; undecidable
  type checking.
- N. Amin, K. S. Grütter, M. Odersky, T. Rompf, S. Stucki. The essence of dependent object types.
  WadlerFest 2016, LNCS 9600, Springer, pp. 249–272. DOI 10.1007/978-3-319-30936-1_14.
  **Metadata-verified**; background (record only). For: T3: the DOT foundation. Bounds subtyping on
  `x.A` was not shown.
- N. Amin, T. Rompf, M. Odersky. Foundations of path-dependent types. OOPSLA 2014, SIGPLAN Notices
  49(10):233–249. DOI 10.1145/2660193.2660216. **Metadata-verified**; background (record only). For:
  T3. The sharpening "refinement or lattice subtyping breaks an intuitive property" was **not**
  shown.
- T. Rompf, N. Amin. Type soundness for dependent object types (DOT). OOPSLA 2016, SIGPLAN Notices
  51(10):624–641. DOI 10.1145/3022671.2984008. Accepted version: Cambridge repository, DOI
  10.17863/CAM.27471. **Metadata-verified**; the abstract reports the point cited. For: T3: DOT with
  refinement and an intersection lattice is sound.
- J. Z. S. Hu, O. Lhoták. Undecidability of `D<:` and its decidable fragments. PACMPL 4(POPL),
  Article 9, 2020. DOI 10.1145/3371077. arXiv:1908.05294. **Metadata-verified**; the abstract
  reports the point cited. For: T3's bad-bounds sharpening: stated.
- L. Parreaux, A. Boruch-Gruszecki, P. G. Giarrusso. Towards improved GADT reasoning in Scala (short
  paper). Scala 2019 (with ECOOP 2019, London). DOI 10.1145/3337932.3338813. **Metadata-verified**;
  the abstract reports the point cited. For: T5: stated.
- H. Xi, C. Chen, G. Chen. Guarded recursive datatype constructors. POPL 2003, pp. 224–235.
  **Metadata-verified**; background (record only). For: T5 background.
- A. Kennedy, C. V. Russo. Generalized algebraic data types and object-oriented programming. OOPSLA
  2005, San Diego. DOI 10.1145/1094811.1094814. **Metadata-verified**; the abstract reports the
  point cited. For: T5. "A subclass fixes the parent's type argument" is an inference.

### Objects, implicits, names, macros, state

- P. Neron, A. P. Tolmach, E. Visser, G. Wachsmuth. A theory of name resolution. ESOP 2015 (ETAPS,
  London), LNCS 9032, Springer, pp. 205–231. DOI 10.1007/978-3-662-46669-8_9. **Metadata-verified**;
  background (record only). For: O3: language-independent resolution over scope graphs (paraphrase).
  Scala's precedence levels rest on [28] and [32].
- B. C. d. S. Oliveira, T. Schrijvers, W. Choi, W. Lee, K. Yi. The implicit calculus: a new
  foundation for generic programming. PLDI 2012, Beijing, pp. 35–44. DOI 10.1145/2254064.2254070.
  Extended report: arXiv:1203.4499. **Metadata-verified**; background (record only). For: O2:
  general design only. Nearest-rule priority was not shown.
- M. Odersky, O. Blanvillain, F. Liu, A. Biboudis, H. Miller, S. Stucki. Simplicitly: foundations
  and applications of implicit function types. PACMPL 2(POPL), Article 42, Jan 2018. DOI
  10.1145/3158130. **Metadata-verified**; background (record only). For: O2, O6. The desugaring and
  erasure facts rest on [16].
- W. R. Cook. On understanding data abstraction, revisited. OOPSLA 2009 (Onward! Essays), pp.
  557–572; SIGPLAN Notices 44(10). DOI 10.1145/1640089.1640133. **Metadata-verified**; the abstract
  reports the point cited. For: O1: the ADT versus object distinction. The record-of-methods reading
  is not stated.
- B. C. Pierce, D. N. Turner. Simple type-theoretic foundations for object-oriented programming. JFP
  4(2):207–247, April 1994. DOI 10.1017/S0956796800001040. **Metadata-verified**; background (record
  only). For: O1: scope only. The existential encoding was not seen in this pass.
- K. B. Bruce, L. Cardelli, B. C. Pierce. Comparing object encodings. Inf. Comput. 155(1–2):108–133,
  Nov 1999. Earlier version: TACS'97, LNCS 1281, p. 415 ff. **Metadata-verified**; background
  (record only). For: O1: record only, no abstract.
- W. Cook, J. Palsberg. A denotational semantics of inheritance and its correctness. OOPSLA 1989.
  Also a DAIMI report (Aarhus), 1989. DOI 10.1145/74877.74922. **Metadata-verified**; the abstract
  reports the point cited. For: O1: self-reference analysed as a fixed point, proved equivalent to
  method-lookup semantics (paraphrase).
- A. Banerjee, D. A. Naumann. Ownership confinement ensures representation independence for
  object-oriented programs. JACM 52(6):894–960, 2005. Preprint arXiv cs/0212003.
  **Metadata-verified**; background (record only). For: O1, O5: representation independence under a
  static confinement analysis (paraphrase).
- (a) N. Stucki, A. Biboudis, M. Odersky. A practical unification of multi-stage programming and
  macros. GPCE 2018 (Boston). DOI 10.1145/3278122.3278139. (b) N. Stucki, J. I. Brachthäuser, M.
  Odersky. Multi-stage programming with generative and analytical macros. GPCE 2021 (Chicago), pp.
  110–122. DOI 10.1145/3486609.3487203. **Metadata-verified**; background (record only). For: O4:
  background. 2021's "a calculus for macros that supports both code generation and code analysis" is
  a paraphrase. The refusal rests on [17].
- A. M. Pitts, I. D. B. Stark. Operational reasoning for functions with local state. In A. D.
  Gordon, A. M. Pitts (eds.), *Higher Order Operational Techniques in Semantics*, Publications of
  the Newton Institute, CUP, 1998, pp. 227–273. **Metadata-verified**; the abstract reports the
  point cited. For: O5: why the exclusion is load-bearing. The `var`-as-value reading is not stated.
- A. Ahmed, D. Dreyer, A. Rossberg. State-dependent representation independence. POPL 2009
  (Savannah), pp. 340–353. **Metadata-verified**; the abstract reports the point cited. For: O5:
  step-indexed possible worlds give representation independence for representations that depend on
  local state (paraphrase).

## 6. Not verified (not citable)

No authoritative page showed these records; they appear in this work only to say what would
close a gap.

- C. Liang, D. Miller. Focusing and polarization in linear, intuitionistic, and classical logics.
  TCS 410(46):4747–4768, 2009. DOI 10.1016/j.tcs.2009.07.041.
- D. E. Knuth. Mathematics and computer science: coping with finiteness. Science
  194(4271):1235–1242, 1976.
- P. Manolios, D. Vroon. Ordinal arithmetic: algorithms and mechanization. J. Autom. Reasoning
  34(4):387–423, 2005.
- G. S. Boolos, J. P. Burgess, R. C. Jeffrey. *Computability and Logic*, 5th ed. CUP, 2007. Ch. 1.
- S. Hirokawa. Infiniteness of proof(α) is polynomial-space complete. TCS 206(1–2):331–339, Oct
  1998.
- H. Friedman. Equality between functionals. In R. Parikh (ed.), *Logic Colloquium* (Boston
  1972–73), LNM 453, Springer, 1975 (pp. 22–37 per citing works).
- R. Loader. The undecidability of λ-definability. In C. A. Anderson, M. Zelëny (eds.), *Logic,
  Meaning and Computation: Essays in Memory of Alonzo Church*, Kluwer, Dordrecht, 2001, pp. 331–342.
- R. Loader. Finitary PCF is not decidable. TCS 266(1–2):341–364, 2001. DOI
  10.1016/S0304-3975(00)00194-8.
- A. Frisch, G. Castagna, V. Benzaken. Semantic subtyping: dealing set-theoretically with function,
  union, intersection, and negation types. JACM 55(4), 2008.
- A. M. Turing. On computable numbers, with an application to the Entscheidungsproblem. Proc. London
  Math. Soc., Series 2, 42:230–265 (issued in two parts, 30 Nov and 23 Dec 1936). Correction:
  s2-43:544–546 (1937).
- IEEE Std 754-2019, IEEE Standard for Floating-Point Arithmetic (revision of 754-2008). Approved 13
  Jun 2019, published 22 Jul 2019, pp. 1–84. DOI 10.1109/IEEESTD.2019.8766229.
- P. Wadler, S. Blott. How to make ad-hoc polymorphism less ad hoc. POPL 1989, pp. 60–76.

## 7. Corrections applied

The metadata pass corrected the earlier records; these are the ones a reader may meet elsewhere:

- PSPACE-completeness for the budgets cites Urzyczyn 1997 (or Statman 1979, *Intuitionistic
  propositional logic is polynomial-space complete*, TCS 9(1):67–72, metadata-verified), not
  Hirokawa 1998, whose record is not verified.
- Loader's results are two works: *The undecidability of λ-definability* (the Church memorial
  volume, Kluwer 2001, pp. 331–342) and *Finitary PCF is not decidable* (TCS 266, 2001). Neither
  record is verified; Statman 1982 (JSL 47(1), metadata-verified) only poses Plotkin's
  λ-definability problem and is never a source for undecidability.
- Hindley 1997 is cited with its ch. 8 title, "Counting a type's inhabitants".
- Cardelli & Leroy 1990 is *Abstract types and the dot notation* (DEC SRC RR 56), not
  "Abstracting existential types".
- Mitchell & Plotkin 1988 uses the singular title; Turing 1936 is dated 1936; the JLS is the
  Java SE 21 edition; Frisch, Castagna & Benzaken is cited by its LICS 2002 version; Blanvillain
  et al. 2022 carries no article number; Goldberg 1991 is the ACM original.
- DOIs are printed only where a record displayed them.
