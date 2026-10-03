# nomologR: Guided scale development and construct validation

`nomologR` coordinates established psychometric and structural-equation
modeling tools while adding evidence-guided diagnostics, transparent
decision logging, and teaching-oriented interpretation.

## Details

The package follows a measurement-first philosophy: understand the item
set and measurement model before interpreting a theory-specified
structural network. Numerical reference values are prompts for
investigation rather than universal deletion or validity rules.

## Stability and deprecation

From version 0.3.0, the first release submitted to CRAN, the public
interface changes only after a deprecation period. Code written against
one release keeps working in the next.

**What is covered.** The interface is:

- the exported functions;

- their documented arguments and defaults;

- the documented fields of the objects they return;

- the `type` values
  [`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md)
  accepts;

- the columns of decision logs. Each analysis's decision log has the
  columns `stage`, `object`, `metric`, `value`, `reference`, `severity`,
  `observation`, `recommendation`, `decision`, and `rationale`; the
  guided workflow keeps its own log, described in
  [`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md).

Functions reached only with `:::`, undocumented fields, the wording of
decision-log rows, and the layout of rendered reports are not covered.
Names and values that differ from one table to another are covered as
they stand; **Conventions in returned tables** below sets them out.

**Deprecation before removal.** A breaking change is deprecated for at
least one minor release before it takes effect. Breaking changes include
removing or renaming any part of the interface, or changing a default in
a way that changes results. While deprecated, the function or argument
keeps working, warns once per session naming its replacement, and is
listed in NEWS.

**What is not breaking.** Any release may add new functions, new
arguments whose defaults leave results unchanged, new fields in returned
objects, or new decision-log rows. Address fields by name, not position.

**APA formatting.** The `type` values of
[`nomo_apa_table()`](https://juhalt.github.io/nomologR/reference/nomo_apa_table.md)
and the structure of the object it returns are covered. The formatting
of a table, meaning its headings, number formats, and notes, may still
be corrected where it departs from APA style, with the correction
described in NEWS.

**Experimental.** Two functions remain experimental after 1.0.0. They
may change during 1.x without a deprecation period, with each change
described in NEWS:

- [`nomo_method_variance()`](https://juhalt.github.io/nomologR/reference/nomo_method_variance.md).
  Its output may be reorganized as the marker technique is extended
  beyond continuous indicators.

- [`nomo_power_simulate()`](https://juhalt.github.io/nomologR/reference/nomo_power_simulate.md).
  The metric of its estimates and its summaries may be refined as it is
  extended beyond complete continuous data.

**The contentvalidR handoff.** The exchange object is versioned by its
producer. Within a schema version, fields are only added, and nomologR
ignores fields it does not know. Anything else is a new schema version,
agreed with `contentvalidR`. nomologR refuses a schema version it does
not read, naming both package versions.

## Conventions in returned tables

A few names and values differ from one table to another. They are part
of the interface described above and are kept as they are, so they are
set out here.

**Proportions and percentages.** A column named `pct_*` holds a
proportion between 0 and 1, not a percentage, although printed output
shows it as a percentage: `pct_missing` in the `item_summary` and
`case_summary` of
[`nomo_screen()`](https://juhalt.github.io/nomologR/reference/nomo_screen.md)
and in the `variables` table of
[`nomo_missing()`](https://juhalt.github.io/nomologR/reference/nomo_missing.md),
`pct_incomplete` in the `pattern` table of
[`nomo_missing()`](https://juhalt.github.io/nomologR/reference/nomo_missing.md),
and `pct_dropped` in the `sample_summary` of
[`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md).
Columns named `*_prop` or `proportion_*` are proportions too. The one
percentage is `percent_unique` in the `item_summary` of
[`nomo_screen()`](https://juhalt.github.io/nomologR/reference/nomo_screen.md),
the number of distinct responses as a percentage (0 to 100) of the
observed responses, which is the scale of the near-zero-variance rule.
Its reference in
[`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md),
`nzv_percent_unique_reference`, is a percentage as well.

**Flags.** A stored flag uses one of three vocabularies:

- Loading tables. `attention` in the `item_summary` of
  [`nomo_efa()`](https://juhalt.github.io/nomologR/reference/nomo_efa.md)
  and in the `standardized_loadings` of
  [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
  and
  [`nomo_validity()`](https://juhalt.github.io/nomologR/reference/nomo_validity.md)
  is `"KEEP"`, `"REVIEW"`, or `"STRONG REVIEW"`.

- The item review of
  [`summary.nomo_screen()`](https://juhalt.github.io/nomologR/reference/summary.nomo_screen.md).
  `attention` is `"none"`, `"review"`, or `"concern"`.

- Decision logs and other evidence. `severity` in each analysis's
  decision log and in the `heywood` table of
  [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md),
  and the `attention`, `measurement_attention`, and `signal` columns of
  the other evidence tables, are `"info"`, `"review"`, or `"concern"`.
  An evidence table may also mark a value that could not be computed as
  `"unavailable"`, as the `fit_evidence` of
  [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
  does.

The three describe the same levels. Printed output, plots, and reports
show them in one wording: no flag for `"KEEP"`, `"none"`, and `"info"`;
"review" for `"REVIEW"` and `"review"`; "concern" for `"STRONG REVIEW"`
and `"concern"`; and "not computed" for `"unavailable"`. The stored
values do not change, so code that filters a table uses that table's own
values.

**P-values and fit indices.** A p-value is named `p_value`, or
`<model>_p_value` where a table holds one for each model. Three kinds of
table differ:

- `lavaan`'s parameter tables, `parameter_estimates` and
  `standardized_solution`, keep `lavaan`'s name, `pvalue`.

- The `models` tables of
  [`nomo_esem()`](https://juhalt.github.io/nomologR/reference/nomo_esem.md),
  [`nomo_method_variance()`](https://juhalt.github.io/nomologR/reference/nomo_method_variance.md),
  and
  [`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md),
  and the `fit_evidence` of
  [`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md)
  and
  [`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md),
  name the fit indices they hold as
  [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
  does: `chisq`, `df`, `pvalue`, `cfi`, `tli`, `rmsea`, `srmr`, `aic`,
  and `bic`. In
  [`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md),
  the likelihood-ratio test against the level before is `lrt_chisq`,
  `lrt_df`, and `lrt_p`.

- The `fit_evidence` of
  [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
  is long, one row per index named in `metric`. It and the `fit` table
  of
  [`nomo_missing()`](https://juhalt.github.io/nomologR/reference/nomo_missing.md)
  spell the indices `chi_square`, `df`, `p_value`, `CFI`, `TLI`,
  `RMSEA`, and `SRMR`.

The **Fit tables** section of
[`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md)
names the index columns of each fit table.

## See also

Useful links:

- <https://github.com/JUhalt/nomologR>

- <https://juhalt.github.io/nomologR/>

- Report bugs at <https://github.com/JUhalt/nomologR/issues>

## Author

**Maintainer**: Joshua Uhalt <Josh.Uhalt@gmail.com>

Authors:

- Joshua Uhalt <Josh.Uhalt@gmail.com>
