# Render a reproducible nomologR analysis report

`nomo_report()` renders a self-contained HTML document from a `nomo_run`
object. The report archives researcher inputs, sample roles, item and
factor evidence, EFA/CFA results, reliability, convergent/discriminant
evidence, optional invariance and nomological-network results,
researcher decisions, deviations/post-hoc decisions, method citations,
an evidence trace, and session information.

## Usage

``` r
nomo_report(
  x,
  file = "nomologR-report.html",
  title = "nomologR reproducible analysis report",
  include_plots = TRUE,
  include_session = TRUE,
  max_table_rows = 50L,
  overwrite = FALSE,
  quiet = TRUE
)
```

## Arguments

- x:

  An object created by
  [`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md).

- file:

  Output HTML path.

- title:

  Report title.

- include_plots:

  Logical; include a compact set of component plots when those plots are
  available.

- include_session:

  Logical; include full
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) output.

- max_table_rows:

  Positive integer used for ordinary display tables. The final
  evidence-trace appendix is not truncated.

- overwrite:

  Logical; replace an existing `file`.

- quiet:

  Logical passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

## Value

The normalized report file path, invisibly.

## Details

The report is a presentation and provenance layer. It does not refit
models, alter data, free parameters, remove items, or manufacture
additional statistical conclusions.

Reports can be rendered from complete, paused, or blocked `nomo_run`
objects. Incomplete stages are labeled as such rather than silently
omitted.

## Examples

``` r
if (FALSE) { # \dontrun{
run <- nomo_run(
  data = dat,
  scales = list(WellBeing = c("w1", "w2", "w3", "w4")),
  decisions = list(
    factor_count = 1L,
    cfa_model = "WellBeing =~ w1 + w2 + w3 + w4",
    measurement_model = "proceed"
  )
)

nomo_report(
  run,
  file = "well-being-validation.html"
)
} # }
```
