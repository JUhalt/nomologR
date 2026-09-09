# Evaluate measurement invariance across groups

`nomo_invariance()` evaluates increasingly constrained multi-group CFA
models while retaining every generated
[`semTools::measEq.syntax()`](https://rdrr.io/pkg/semTools/man/measEq.syntax.html)
object and every fitted lavaan model.

## Usage

``` r
nomo_invariance(
  model,
  data,
  group,
  ordered = NULL,
  levels = NULL,
  partial = NULL,
  localize = TRUE,
  estimator = NULL,
  missing = NULL,
  ID.fac = "std.lv",
  ID.cat = "Wu.Estabrook.2016",
  parameterization = "theta",
  guidance = nomo_defaults()
)
```

## Arguments

- model:

  A researcher-specified lavaan measurement-model syntax string or an
  object created by
  [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md).

- data:

  A non-empty data frame.

- group:

  Character scalar naming the grouping variable in `data`.

- ordered:

  Optional character vector naming ordered indicators.

- levels:

  Optional invariance levels. `NULL` uses the sequence implied by the
  indicator category structure.

- partial:

  Optional researcher-specified partial-invariance releases from
  [`nomo_partial()`](https://juhalt.github.io/nomologR/reference/nomo_partial.md).

- localize:

  Logical. If `TRUE`, retain equality-constraint score tests as
  diagnostic evidence. Diagnostics never trigger automatic
  release/refitting.

- estimator:

  Optional lavaan estimator. Ordered models default to WLSMV.

- missing:

  Optional lavaan missing-data option.

- ID.fac:

  Factor-identification method passed to
  [`semTools::measEq.syntax()`](https://rdrr.io/pkg/semTools/man/measEq.syntax.html).
  `"std.lv"` is the default.

- ID.cat:

  Ordered-indicator identification method. Wu-Estabrook is the default.

- parameterization:

  Lavaan categorical parameterization. `"theta"` is the default for
  ordered indicators.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

A `nomo_invariance` object containing generated syntax objects/text,
fitted models, fit/change evidence, category information,
partial-invariance provenance, score diagnostics, and a decision log.

## Details

The sequence is adapted to the observed category structure. Continuous
indicators use configural -\> metric -\> scalar -\> strict. Ordered
indicators with four or more categories permit a separate threshold
step. Three-category indicators require threshold equality as part of
the metric step. When any binary indicator is present, threshold,
loading, and intercept restrictions are imposed together at the first
equality step (`strong`) because those restrictions cannot be treated as
independent tests under Wu-Estabrook identification.

Partial invariance is researcher controlled. Supply an object from
[`nomo_partial()`](https://juhalt.github.io/nomologR/reference/nomo_partial.md)
to request specific equality-constraint releases. Releases are carried
forward to more restrictive levels and their rationales are retained.
`nomo_invariance()` never searches for a combination of releases that
makes a fit rule pass.

When `localize = TRUE`, univariate score tests for equality constraints
are retained as diagnostic evidence. They are explicitly not used to
modify the fitted model.
