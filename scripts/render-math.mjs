// render-math.mjs — the math pre-pass behind `sbt siteRender`.
//
// Reads the published pages under docs/site/ and writes a copy under the staging
// directory, replacing:
//
//   $…$          inline math       → <math>…</math>
//   ```math … ``` display math     → <math display="block">…</math>
//
// with MathML Core produced by temml (scripts/vendor/temml.mjs). Laika then renders
// the staged tree with raw content enabled, so the MathML passes through unescaped.
//
// Both notations are GitHub-native, so the source stays readable there too. Formula
// conversion skips fenced code blocks and inline code spans: those are literal text
// (Scala types, report output) and must never be touched.
//
// Any formula temml cannot parse fails the run with the file and the source text, so
// a broken formula is a build error, not a silently degraded page.

import { createRequire } from "node:module";
import fs from "node:fs";
import path from "node:path";

const require = createRequire(import.meta.url);
const temml = (await import("./vendor/temml.mjs")).default;

const [inputDir, outputDir] = process.argv.slice(2);
if (!inputDir || !outputDir) {
  console.error("usage: node render-math.mjs <input-dir> <output-dir>");
  process.exit(2);
}

let formulas = 0;
let failures = 0;

// Laika's Markdown span parser still runs over raw inline HTML, so the MathML's
// square brackets would read as link-id references and asterisks as emphasis.
// Escaping them as HTML entities keeps the markup intact — the entities travel
// through Laika's raw content unchanged and the browser prints the characters.
// Only text outside <…> tags is escaped: tags never contain these characters.
function escapeMarkdown(text) {
  return text.replace(/\[|\]|\*|_|`|\\/g, (ch) =>
    ({ "[": "&#91;", "]": "&#93;", "*": "&#42;", "_": "&#95;", "`": "&#96;", "\\": "&#92;" })[ch]
  );
}

function render(latex, display, file, where) {
  formulas += 1;
  try {
    const mathml = temml.renderToString(latex, { displayMode: display, throwOnError: true, strict: false });
    return mathml.replace(/<[^>]*>|[^<]+/g, (part) =>
      part.startsWith("<") ? part : escapeMarkdown(part)
    );
  } catch (error) {
    failures += 1;
    console.error(`${file}: ${where}: temml cannot parse ${JSON.stringify(latex)}`);
    console.error(`  ${error.message}`);
    return null;
  }
}

// Inline pass over one line: leaves `code` spans alone, converts $…$ between them.
function inlineMath(line, file, lineNo) {
  let out = "";
  let rest = line;
  for (;;) {
    const open = rest.indexOf("`");
    if (open === -1) break;
    const run = /^`+/.exec(rest.slice(open))[0];
    const close = rest.indexOf(run, open + run.length);
    if (close === -1) break; // Unmatched backtick: leave the rest of the line alone.
    out += mathSpans(rest.slice(0, open), file, lineNo) + rest.slice(open, close + run.length);
    rest = rest.slice(close + run.length);
  }
  return out + mathSpans(rest, file, lineNo);
}

function mathSpans(text, file, lineNo) {
  return text.replace(/\$([^$\n]+)\$/g, (_, latex) => {
    const mathml = render(latex.trim(), false, file, `line ${lineNo}`);
    return mathml ?? `$${latex}$`;
  });
}

// The lines a closed fence contributes to the output: a math fence becomes one
// display equation; any other fence passes through verbatim with its closer.
function closeFence(fence, closer, file) {
  if (fence.info !== "math") {
    return [fence.marker + fence.info, ...fence.body, closer];
  }
  const latex = fence.body.join("\n").trim();
  const mathml = render(latex, true, file, `math block at line ${fence.startLine}`);
  return [mathml ?? fence.body.join("\n")];
}

// Advance the fence state machine by one line, appending to state.out.
function advance(state, line, file) {
  if (state.fence) {
    if (line.trimStart().startsWith(state.fence.marker)) {
      state.out.push(...closeFence(state.fence, line, file));
      state.fence = null;
    } else {
      state.fence.body.push(line);
    }
    return;
  }
  const opened = /^\s*(`{3,}|~{3,})(.*)$/.exec(line);
  if (opened) {
    state.fence = {
      marker: opened[1],
      info: opened[2].trim().split(/\s+/)[0] ?? "",
      body: [],
      startLine: state.lineNo,
    };
  } else {
    state.out.push(inlineMath(line, file, state.lineNo));
  }
}

function transformMarkdown(source, file) {
  const state = { out: [], fence: null, lineNo: 0 };
  for (const line of source.split("\n")) {
    state.lineNo += 1;
    advance(state, line, file);
  }
  return state.out.join("\n");
}

function walk(dir) {
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const full = path.join(dir, entry.name);
    return entry.isDirectory() ? walk(full) : [full];
  });
}

for (const file of walk(inputDir)) {
  const relative = path.relative(inputDir, file);
  const target = path.join(outputDir, relative);
  fs.mkdirSync(path.dirname(target), { recursive: true });
  if (file.endsWith(".md")) {
    fs.writeFileSync(target, transformMarkdown(fs.readFileSync(file, "utf8"), relative));
  } else {
    fs.copyFileSync(file, target);
  }
}

console.log(`render-math: ${formulas} formula(s), ${failures} failure(s)`);
process.exit(failures === 0 ? 0 : 1);
