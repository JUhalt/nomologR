# Guided confirmatory factor analysis

`nomo_cfa()` fits a researcher-specified confirmatory factor model with
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and adds a
transparent guidance layer around estimator choice, convergence,
standardized loadings, global fit, local residuals, Heywood diagnostics,
and modification indices.

## Usage

``` r
nomo_cfa(
  model,
  data,
  ordered = NULL,
  estimator = NULL,
  missing = NULL,
  std.lv = FALSE,
  control = NULL,
  modification_indices = TRUE,
  mi_top = 10L,
  guidance = nomo_defaults()
)
```

## Arguments

- model:

  A researcher-specified `lavaan` measurement-model syntax string or an
  object created by
  [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md).

- data:

  A data frame containing the model indicators.

- ordered:

  Optional character vector naming binary/ordinal indicators. When
  supplied and `estimator = NULL`, `nomo_cfa()` explicitly requests
  `WLSMV`.

- estimator:

  Optional `lavaan` estimator. For continuous indicators, leaving this
  as `NULL` preserves
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html)'s default.
  Robust ML variants such as `"MLR"` remain explicit researcher choices.

- missing:

  Optional `lavaan` missing-data option such as `"fiml"` for continuous
  ML models or `"pairwise"` where supported.

- std.lv:

  Logical. Passed directly to
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- control:

  Optional named list passed to lavaan's optimizer through
  `lavaan::cfa(control = ...)`. This is an advanced
  troubleshooting/reproducibility option; non-default optimizer controls
  are recorded in the decision log.

- modification_indices:

  Logical; if `TRUE`, compute modification indices as quarantined
  diagnostics. They never trigger automatic respecification.

- mi_top:

  Number of largest modification indices to retain in the compact
  `$top_modification_indices` view. The full table remains available in
  `$modification_indices`.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

A `nomo_cfa` object containing the unchanged `lavaan` fit plus
standardized loadings, factor correlations, fit evidence, residual
diagnostics, Heywood checks, modification indices, engine warnings, and
a decision log.

## Details

The fitted `lavaan` object is retained unchanged in `$fit`. `nomologR`
does not free parameters, add residual covariances, delete indicators,
or refit a different model in response to fit statistics or modification
indices.

## Examples

``` r
if (FALSE) { # \dontrun{
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
out <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
summary(out)
} # }
```
