```bash
./mill {{module}}.cardinalityReport   # per module; written to out/{{module}}/cardinalityReport.dest/report.txt
```

- Reports are **per module**: every module that mixes in `CardinalityModule` reports its own
  sources. Read the summary and the rows per module, never as one merged number.
- Narrow a single signature with `./mill {{module}}.cardinalityReportSelected <qualified.Name>`.
- Measure sources this build does not compile — a dependency's published sources jar, another
  checkout — with `./mill {{module}}.cardinalityReportOf <path-or-jar>`.
- Keep a checked-in baseline: copy the report to a path under version control (for example
  `docs/baselines/cardinality-<date>.txt`), recording the date, the command and the plugin
  version in its header. Diff a new run against it; refresh the baseline only when a change lands.
