# Citation inventory: Russell (1907), *On Some Difficulties in the Theory of Transfinite Numbers and Order Types*

**Target:** Bertrand Russell, *On Some Difficulties in the Theory of Transfinite Numbers and Order
Types*, [Proceedings of the London Mathematical Society, series 2, volume 4, pp. 29–53](https://doi.org/10.1112/plms/s2-4.1.29)
(DOI `10.1112/plms/s2-4.1.29`; publisher year 1907, often cited as 1906).

Machine-readable records for every citing work found: [russell-citations.json](russell-citations.json).

## Why this inventory exists

This paper is a foundational reference for our counting model, not an algorithmic one: it is where
Russell works through which collections a definition may legitimately form — the same discipline
our [counting contract](../plans/v1.md#2-agreed-counting-contract) applies when it fixes a scope,
a language, and an equivalence before assigning a cardinality. The inventory below records who
has built on that work, so the [evidence register](code-cardinality-foundations.md) can cite the
line of descent rather than a single 1907 data point.

## Coverage and provenance

Retrieved 2026-09-28, by direct public-API queries (reproducible from the URLs below):

| Index | Query | Records | Notes |
|---|---|---:|---|
| OpenAlex | `cites:W2143162796`, 2 cursor pages at 200/page | 301 | all pages exhausted; terminal cursor null |
| Semantic Scholar | citations of paper `6a8e5460f95b92c81c264e94dbffe4d3b1ed904c`, limit 1000 | 212 | one page, no `next` |
| Crossref | `10.1112/plms/s2-4.1.29` | 70 | count only; Crossref exposes no citing list |

Union under a DOI-or-title+year merge key: **376 records** (132 in both indexes, 169 OpenAlex
only, 75 Semantic Scholar only). An earlier working pass with a more conservative merge reported
368 — the union size is a function of the deduplication rule, not a disagreement between indexes.

**What "all papers that reference it" can honestly mean here:** every citing work indexed by these
two databases as of the retrieval date, deduplicated as above. It is **not** an exhaustive census
of every paper that cites Russell 1907 (index coverage differs, and citations without a DOI in
either database are invisible to this merge), and it is **not** a full-text review of 376 works.
Verification levels are tracked per record below.

## Classification by title keywords

Titles are the sole classification basis except where a verification level is stated. Counts do
not sum to coverage totals across indexes because of the union.

| Class | Records |
|---|---:|
| Definability and paradox (definable, Richard, Berry, diagonal, Cantor) | 59 |
| Russell and history (Russell, Frege, Principia, logicism) | 50 |
| Set-theory foundations (axioms, Zermelo, foundations) | 37 |
| Type theory and polymorphism (predicativity, impredicativity, lambda) | 17 |
| Cardinality and order (cardinal, ordinal, transfinite, continuum) | 8 |
| Other / unclassified | 205 |

## Most relevant citers

These are the records a reviewer of our counting model is most likely to be asked about,
curated from the classes above. Each states how far it was verified.

### Bibliography-verified (the citing reference was inspected)

| Work | Cites Russell via | What it contributes |
|---|---|---|
| Fan, *Hobson's Conception of Definable Numbers*, Hist. & Phil. of Logic 41(2), 2020, [DOI](https://doi.org/10.1080/01445340.2020.1731784) | Crossref reference list entry `10.1112/plms/s2-4.1.29` | Language-relative definability in the Hobson–Richard tradition; connects the diagonal generation of definitions to later computability. Full text paywalled. |

### Abstract-inspected (abstract read this session; citation database-reported)

| Work | Index evidence | What the abstract contributes |
|---|---|---|
| Luna & Taylor, *Cantor's Proof in the Full Definable Universe*, Australasian J. of Logic 9, 2010, [DOI](https://doi.org/10.26686/ajl.v9i0.1818) | both indexes | Cantor's proof restricted to the *definable* universe yields a Richard-style paradox: that universe "seems to be countable on one account and uncountable on another". The way out is that **definitional contexts restrict the scope of quantifiers** — precisely our rule that a count is only meaningful once language, scope and equivalence are fixed. |
| *The entanglement of logic and set theory, constructively*, Inquiry, 2019, [DOI](https://doi.org/10.1080/0020174x.2019.1651080) | both indexes | Intuitionistic/predicative treatment of infinite quantification; relevant to predicative readings of our environment. |
| *In Praise of Impredicativity* (meta-programming formalization), J. Logic Comput., 2019, [DOI](https://doi.org/10.1017/s1471068419000024) | both indexes | Where impredicativity is *useful* despite the paradoxes; documents the boundary we draw when we refuse to let a declaration supply its own implementation. |
| *Basic cardinal arithmetic* (chapter), 2004, [DOI](https://doi.org/10.1093/acprof:oso/9780199269730.003.0015) | OpenAlex | Set-theoretic sum/product/exponent identities — the same shapes our `Size` algebra uses, in their home setting. |

### Title-screened candidates (title and index match only; not yet read)

| Work | Index evidence | Why it may matter |
|---|---|---|
| *Polymorphism and the obstinate circularity of second order logic*, Bull. Symb. Logic, 2017, [DOI](https://doi.org/10.1017/bsl.2017.43) | Semantic Scholar only | Second-order quantification's circularity; the predicativity boundary for polymorphic counting. |
| *Predicativity and parametric polymorphism of Brouwerian implication*, 2017 | Semantic Scholar only (arXiv:1710.07704) | A constructive/predicative parametricity; closest modern heir to our uniformity assumptions. |
| *Polymorphism and the free bicartesian closed category*, 2019 | Semantic Scholar only (arXiv:1907.03481) | Categorical semantics of the type shapes we count. |
| *Russell's 1903–1905 Anticipation of the Lambda Calculus*, Hist. & Phil. of Logic, 2003, [DOI](https://doi.org/10.1080/0144534031000076237) | both indexes | Direct historical bridge from Russell's substitutional theory to the calculus our fragment restricts. |

The remaining 360+ records are in [russell-citations.json](russell-citations.json) with year, DOI
where available, index flags, and title class. Nothing in the machine-readable file should be
read as verified beyond "database-reported".

## Access gaps

The publisher full text of Russell 1907 is behind a bot-blocked 403 (Wiley); the [contemporary
review](https://doi.org/10.2307/2011035) is a different work, not the paper. Secondary
supporting material used in place of the primary text is recorded per claim in the
[evidence register](code-cardinality-foundations.md).
