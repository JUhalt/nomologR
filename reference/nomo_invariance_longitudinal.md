# Evaluate measurement invariance across occasions

`nomo_invariance_longitudinal()` asks whether the same measures mean the
same thing each time the same people answer them. It fits the
measurement model at every occasion in one model and imposes
increasingly strict equality constraints across occasions, as
[`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md)
does across groups.

## Usage

``` r
nomo_invariance_longitudinal(
  model,
  data,
  occasions,
  columns = "{item}_{occasion}",
  ordered = NULL,
  levels = NULL,
  partial = NULL,
  localize = TRUE,
  auto = "all",
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

  A measurement model for one occasion, in the items' own names: a
  lavaan model string or a
  [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md)
  object. Loadings are free; the model contains factors and their items
  only.

- data:

  A data frame with one row per person and one column per item and
  occasion.

- occasions:

  Two or more occasion labels, in time order. The first is the reference
  for latent change.

- columns:

  A pattern for the column names, containing `{item}` and `{occasion}`.

- ordered:

  Optional names of ordered items, as in `model`. Items whose columns
  are stored as ordered factors are modeled as ordered whether or not
  they are named here: the result's `ordered_detected` lists them, the
  decision log lists them for review, and the rules for ordered
  indicators in
  [`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md)
  apply.

- levels:

  Optional invariance levels. `NULL` uses the sequence implied by the
  indicator category structure.

- partial:

  Optional researcher-specified releases from
  [`nomo_partial()`](https://juhalt.github.io/nomologR/reference/nomo_partial.md),
  naming items and factors as in `model`.

- localize:

  Logical. If `TRUE`, retain equality-constraint score tests as
  diagnostic evidence. Diagnostics never trigger automatic
  release/refitting.

- auto:

  The lags over which each item's unique factors are correlated: `"all"`
  (default) or a positive whole number, such as `1` for adjacent
  occasions only.

- estimator:

  Optional lavaan estimator. Ordered models default to WLSMV.

- missing:

  Optional lavaan missing-data option.

- ID.fac:

  Factor-identification method passed to
  [`semTools::measEq.syntax()`](https://rdrr.io/pkg/semTools/man/measEq.syntax.html).
  `"std.lv"` is the default.

- ID.cat:

  Ordered-indicator identification passed to
  [`semTools::measEq.syntax()`](https://rdrr.io/pkg/semTools/man/measEq.syntax.html).
  Only Wu and Estabrook's (2016) identification is supported, as
  `"Wu.Estabrook.2016"` (the default) or a semTools alias for it
  (`"Wu.2016"`, `"Wu.Estabrook"`, `"Wu"`): the level sequences and their
  notes are built for it, and under semTools' other choices the levels
  would not constrain what their names say.

- parameterization:

  Lavaan categorical parameterization. `"theta"` is the default for
  ordered indicators.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

A `nomo_invariance_longitudinal` object, which is also a
`nomo_invariance` object, so
[`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md),
[`nomo_apa_table()`](https://juhalt.github.io/nomologR/reference/nomo_apa_table.md),
and [`plot()`](https://rdrr.io/r/graphics/plot.default.html) work as
they do for groups. The fields to read are those of
[`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md),
with `occasions` in place of `groups`, and:

- `latent_means`: at each level that holds intercepts equal, each later
  occasion's latent mean relative to the first, in the first occasion's
  latent standard deviations, with its interval.

- `longitudinal_model`: the configural model across occasions, before
  equality constraints.

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

## Details

**Why.** A change in scores is a change in the construct only if the
measurement relations hold across occasions. Equal loadings let changes
in the construct's variance and relations be compared, and equal
intercepts let changes in its mean be compared (Widaman, Ferrer, &
Conger, 2010). An item whose meaning drifts, through practice, an
intervention, or development, otherwise shows up as change in the
construct.

**The model.** `model` describes one occasion, in the items' own names.
`columns` says how an item and an occasion name a column of `data`: with
the default `"{item}_{occasion}"`, item `w1` at occasion `t2` is column
`w1_t2`. Each factor is measured at every occasion, and the occasion
factors are correlated. Each item's unique factor is correlated with the
same item's unique factor on the other occasions, because what an item
measures besides the construct is usually stable too, and leaving those
covariances out biases the estimates (Widaman et al., 2010). `auto` sets
how many lags they span.

**The sequence.** The levels, the ordered-item identification, partial
releases, and score diagnostics are those of
[`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md),
applied across occasions instead of groups. A partial release names the
item as in `model`, and frees that parameter across all occasions:
`"w3 ~ 1"` releases the intercept of `w3`. A release naming a column,
such as `"w3_t2 ~ 1"`, is an error, as is any release that frees no
parameter. Ordered items follow Wu and Estabrook's (2016)
identification, the only `ID.cat` supported; Liu et al. (2017) discuss
testing invariance over time with ordered-categorical measures. An item
whose columns are stored as ordered factors is modeled as ordered on
every occasion, whether or not `ordered` names it.

**Latent change.** Once intercepts are invariant, fully or partially,
the construct's mean can be compared across occasions. Under the default
`ID.fac = "std.lv"` the first occasion's latent mean is 0 and its
variance 1, so `latent_means` gives each later occasion's latent mean as
a change in the first occasion's latent standard deviations, with its
interval. With every intercept of a factor's items released, its later
means are fixed at 0 rather than estimated; the decision log says so,
and they are left out.

## References

Liu, Y., Millsap, R. E., West, S. G., Tein, J.-Y., Tanaka, R., & Grimm,
K. J. (2017). Testing measurement invariance in longitudinal data with
ordered-categorical measures. *Psychological Methods, 22*(3), 486-506.
[doi:10.1037/met0000075](https://doi.org/10.1037/met0000075)

Widaman, K. F., Ferrer, E., & Conger, R. D. (2010). Factorial invariance
within longitudinal structural equation models: Measuring the same
construct across time. *Child Development Perspectives, 4*(1), 10-18.
[doi:10.1111/j.1750-8606.2009.00110.x](https://doi.org/10.1111/j.1750-8606.2009.00110.x)

Wu, H., & Estabrook, R. (2016). Identification of confirmatory factor
analysis models of different levels of invariance for ordered
categorical outcomes. *Psychometrika, 81*(4), 1014-1045.
[doi:10.1007/s11336-016-9506-0](https://doi.org/10.1007/s11336-016-9506-0)

## See also

[`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md)
across groups, and
[`nomo_partial()`](https://juhalt.github.io/nomologR/reference/nomo_partial.md)
for researcher-specified releases.

## Examples

``` r
# \donttest{
long <- nomo_invariance_longitudinal(
  "Wellbeing =~ w1 + w2 + w3 + w4",
  data = nomo_demo_longitudinal,
  occasions = c("t1", "t2", "t3")
)
long
#> <nomo_invariance_longitudinal> Measurement invariance across occasions
#> Occasions: t1, t2, t3 | Indicators: continuous
#> Requested: configural -> metric -> scalar -> strict
#> Completed: configural -> metric -> scalar -> strict
#> 
#>   Level         CFI  RMSEA   SRMR  CFI change  RMSEA change   LRT p
#>   configural  1.000  0.000  0.020          --            --      --
#>   metric      0.999  0.012  0.030      -0.001        +0.012    .018
#>   scalar      0.963  0.058  0.045      -0.036        +0.046  < .001
#>   strict      0.964  0.053  0.047      +0.001        -0.005    .669
#> Localized equality-constraint diagnostics retained: 48
#> 
#> Fit changes and score diagnostics are evidence. They are not pass/fail rules,
#> and nomologR never frees a parameter because of them.
head(nomo_table(long, "local_strain"))
#> # A tibble: 6 × 8
#>   level  constraint_index constraint    score_x2    df p_value diagnostic_only
#>   <chr>             <int> <chr>            <dbl> <dbl>   <dbl> <lgl>          
#> 1 metric                8 .p4. == .p12.    4.23      1  0.0398 TRUE           
#> 2 metric                6 .p3. == .p11.    4.13      1  0.0421 TRUE           
#> 3 metric                1 .p1. == .p5.     3.52      1  0.0607 TRUE           
#> 4 metric                7 .p4. == .p8.     3.21      1  0.0731 TRUE           
#> 5 metric                4 .p2. == .p10.    0.557     1  0.455  TRUE           
#> 6 metric                2 .p1. == .p9.     0.522     1  0.470  TRUE           
#> # ℹ 1 more variable: constraint_display <chr>

# The w3 intercept drifts upward over time. Releasing it, as a documented
# decision, leaves the latent change to the other items.
release <- nomo_partial(
  level = "scalar",
  syntax = "w3 ~ 1",
  rationale = "The local-strain diagnostics point to the w3 intercept."
)
long_partial <- nomo_invariance_longitudinal(
  "Wellbeing =~ w1 + w2 + w3 + w4",
  data = nomo_demo_longitudinal,
  occasions = c("t1", "t2", "t3"),
  levels = c("configural", "metric", "scalar"),
  partial = release
)
nomo_table(long_partial, "latent_means")
#> # A tibble: 2 × 9
#>   level  occasion reference_occasion factor    estimate     se ci_lower ci_upper
#>   <chr>  <chr>    <chr>              <chr>        <dbl>  <dbl>    <dbl>    <dbl>
#> 1 scalar t2       t1                 Wellbeing    0.321 0.0508    0.222    0.421
#> 2 scalar t3       t1                 Wellbeing    0.549 0.0611    0.430    0.669
#> # ℹ 1 more variable: p_value <dbl>
# }
```
