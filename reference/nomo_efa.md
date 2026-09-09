# Guided exploratory factor analysis

`nomo_efa()` estimates a common-factor exploratory factor analysis while
keeping the consequential analytical choices visible. Oblique rotation
is the default, item-level diagnostics are framed as prompts for
inspection, and no item is automatically deleted or model silently
refit.

## Usage

``` r
nomo_efa(
  data,
  items = NULL,
  factors,
  factor_count = NULL,
  rotation = "oblimin",
  fm = "minres",
  correlation = NULL,
  types = NULL,
  missing = NULL,
  smooth = NULL,
  guidance = nomo_defaults()
)
```

## Arguments

- data:

  A data frame containing candidate items.

- items:

  Optional character vector identifying item columns. If `factors` is a
  `nomo_factors` object and `items = NULL`, its item set is inherited.
  Otherwise all columns are used when `items = NULL`.

- factors:

  A positive integer number of factors to extract, or a `nomo_factors`
  object. For a `nomo_factors` object, the selected parallel-analysis
  factor count is used unless `factor_count` is supplied.

- factor_count:

  Optional positive integer. This may be supplied only when `factors` is
  a `nomo_factors` object. It preserves the M2 item/modeling/
  correlation/missingness context while recording a researcher-selected
  EFA factor count instead of pretending the primary parallel-analysis
  suggestion was adopted.

- rotation:

  Rotation passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html). The default is
  `"oblimin"`. Orthogonal rotations are allowed but are recorded as a
  researcher choice.

- fm:

  Common-factor extraction method passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html). The default is
  `"minres"`. Supported values are `"minres"`, `"uls"`, `"ols"`,
  `"wls"`, `"gls"`, `"pa"`, `"ml"`, `"minchi"`, `"minrank"`, `"alpha"`,
  and `"old.min"`.

- correlation:

  Optional correlation strategy: `"auto"`, `"pearson"`, `"polychoric"`,
  `"tetrachoric"`, or `"mixed"`. If `NULL`, the value is inherited from
  a supplied `nomo_factors` object when possible; otherwise `"auto"` is
  used.

- types:

  Optional named character vector declaring selected items as
  `"continuous"`, `"ordinal"`, or `"binary"`. If `NULL`, modeling types
  are inherited from a supplied `nomo_factors` object when possible;
  otherwise they are inferred using the same conservative rules as
  [`nomo_factors()`](https://juhalt.github.io/nomologR/reference/nomo_factors.md).

- missing:

  Optional missing-data strategy: `"pairwise"` or `"complete"`. If
  `NULL`, the value is inherited from a supplied `nomo_factors` object
  when possible; otherwise `"pairwise"` is used.

- smooth:

  Optional logical. If `NULL` (default), smoothing is inherited from a
  supplied `nomo_factors` object when that object explicitly used
  smoothing; otherwise `FALSE`. If `FALSE`, a non-positive-definite
  correlation matrix stops the analysis. If `TRUE`,
  [`psych::cor.smooth()`](https://rdrr.io/pkg/psych/man/cor.smooth.html)
  is used explicitly and the intervention is recorded.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

An object of class `nomo_efa` containing the fitted EFA, tidy pattern
and structure matrices, factor correlations, item diagnostics,
communalities/uniquenesses, residual diagnostics, adequacy information,
and a decision log.

## Details

The factor count may be supplied directly as a positive integer or by
passing a `nomo_factors` object. When a `nomo_factors` object is
supplied, its primary parallel-analysis suggestion is used as the
requested EFA factor count and, unless overridden, its item set,
modeling types, correlation model, and missing-data strategy are carried
forward.

## Examples

``` r
set.seed(2026)
f1 <- rnorm(250)
f2 <- 0.35 * f1 + sqrt(1 - 0.35^2) * rnorm(250)
dat <- data.frame(
  a1 = .75 * f1 + rnorm(250, sd = .65),
  a2 = .70 * f1 + rnorm(250, sd = .70),
  a3 = .80 * f1 + rnorm(250, sd = .60),
  a4 = .72 * f1 + rnorm(250, sd = .68),
  b1 = .75 * f2 + rnorm(250, sd = .65),
  b2 = .70 * f2 + rnorm(250, sd = .70),
  b3 = .80 * f2 + rnorm(250, sd = .60),
  b4 = .72 * f2 + rnorm(250, sd = .68)
)
efa <- nomo_efa(dat, factors = 2)
efa
#> <nomo_efa>
#> Cases: 250 | Items: 8 | Factors: 2 (researcher specified)
#> Correlation: pearson | Extraction: minres | Rotation: oblimin
#> Off-diagonal RMSR: 0.017
#> Item review: 8 KEEP | 0 REVIEW | 0 STRONG REVIEW
#> No items were automatically deleted or refit.
summary(efa)
#> nomologR exploratory factor analysis
#> 250 cases | 8 items | 2 factors (researcher specified)
#> Correlation: pearson | Extraction: minres | Rotation: oblimin
#> Supporting adequacy: KMO = 0.827 | Bartlett chi-square(28) = 866.38, p < .001
#> 
#> Item-level structural review
#> # A tibble: 8 × 7
#>   item  primary_factor primary_loading secondary_factor secondary_loading
#>   <chr> <chr>                    <dbl> <chr>                        <dbl>
#> 1 a1    F2                       0.813 F1                        -0.0490 
#> 2 a2    F2                       0.676 F1                         0.0316 
#> 3 a3    F2                       0.801 F1                        -0.0157 
#> 4 a4    F2                       0.666 F1                         0.0902 
#> 5 b1    F1                       0.794 F2                        -0.00424
#> 6 b2    F1                       0.813 F2                         0.0326 
#> 7 b3    F1                       0.816 F2                         0.00313
#> 8 b4    F1                       0.749 F2                        -0.0371 
#> # ℹ 2 more variables: communality <dbl>, attention <chr>
#> 
#> No configured numeric EFA review flags were triggered.
#> 
#> Factor correlations
#>      F1   F2
#> F1 1.00 0.27
#> F2 0.27 1.00
#> 
#> Off-diagonal RMSR: 0.017
#> 
#> Largest localized residuals
#> # A tibble: 5 × 4
#>   item1 item2 residual abs_residual
#>   <chr> <chr>    <dbl>        <dbl>
#> 1 a2    a3      0.0279       0.0279
#> 2 b3    b4     -0.0275       0.0275
#> 3 a1    a4      0.0270       0.0270
#> 4 b1    b2     -0.0268       0.0268
#> 5 a2    b4      0.0251       0.0251
#> 
#> Interpretation rule: numerical references trigger inspection, not automatic deletion or hidden refitting.
```
