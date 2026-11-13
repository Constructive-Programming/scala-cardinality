# Vendored web assets

## temml.mjs

[temml](https://temml.org/) — LaTeX to MathML Core, MIT licensed (`temml.LICENSE`).

- Source: npm package `temml@0.14.0`, file `dist/temml.mjs`, copied verbatim.
- Used by `scripts/render-math.mjs`, the pre-pass `siteRender` runs over `docs/site/`
  before Laika renders. No `npm install`: the module is imported directly.
- Upgrades: replace this file with `dist/temml.mjs` from a newer npm tarball
  (`npm pack temml`), update the version here, and re-render the site.

## ../render-math.mjs

Node ≥ 18. Reads a directory of markdown, writes a copy with `$…$` inline math and
```math fenced blocks replaced by MathML. Exits non-zero on the first formula temml
cannot parse, naming the file and the source text.
