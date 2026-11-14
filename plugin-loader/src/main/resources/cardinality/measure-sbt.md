```bash
sbt cardinalityReport   # per module; also written to cardinalityReportFile (default: target/cardinality/report.txt)
```

- Reports are **per module**: every project the plugin applies to reports its own sources.
  Read the summary and the rows per module, never as one merged number.
- Narrow a single signature with `sbt 'cardinalityReportSelected <qualified.Name>'`.
- Measure sources this build does not compile — a dependency's published sources jar, another
  checkout — with `sbt 'cardinalityReportOf <path-or-jar>'`.
- Keep a checked-in baseline: point `cardinalityReportFile` at a path under version control
  (for example `docs/baselines/cardinality-<date>.txt`) or copy the report there, recording the
  date, the command and the plugin version in its header. Diff a new run against it; refresh the
  baseline only when a change lands.
