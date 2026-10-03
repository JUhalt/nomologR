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

  Optional character vector naming ordered indicators. Model indicators
  stored as ordered factors are modeled as ordered whether or not they
  are named here, since lavaan fits them as categorical; the result's
  `ordered` includes them, `ordered_detected` lists them, and the
  decision log lists them for review. They then follow the rules for
  ordered indicators: the ordered `levels` sequence,
  `ID.fac = "std.lv"`, and no ML-family estimator or FIML. Convert such
  columns to numeric to model them as continuous.

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

A `nomo_invariance` object. The fields to read are:

- `groups` and `requested_levels`.

- `completed_levels`: the levels that were estimated and converged.
  Fitting stops at the first level that fails or does not converge; that
  level is in `fit_evidence` but not here.

- `fit_evidence`: one row per level, with its fit, its change from the
  level before, the likelihood-ratio test, and any warning or error.

- `local_strain`: score diagnostics for each equality constraint, which
  localize strain without releasing anything.

- `partial`: the researcher-specified releases, when given, each at the
  level it applies from.

- `latent_means`: at each level that holds intercepts equal, each
  group's latent means relative to the reference group, in the reference
  group's latent standard deviations, with their intervals. See **Latent
  means**.

- `fits`: the fitted `lavaan` model at each level.

- `syntax_text`: the model syntax at each level.

- `engine_warnings`: warnings `lavaan` raised at each level.

- `indicator_type`, `identification_note`, and `decision_log`.

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

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

Each release must name a loading, intercept, threshold, or residual
variance of an indicator in `model`, and must free that parameter in the
generated model:
[`semTools::measEq.syntax()`](https://rdrr.io/pkg/semTools/man/measEq.syntax.html)
ignores a release it cannot match, so a misspelled name, or a marker
loading fixed at 1, is an error rather than a fully constrained model
reported as partial. A release applies from the level that first holds
its parameter type equal (loadings at `metric`, intercepts at `scalar`,
and so on, as the sequence for the indicators sets them). Declared at an
earlier level, where it would change nothing, it is moved to that first
level and the decision log says so. Declared at a later level, it is an
error, because the earlier model would hold the parameter equal and the
later one free it, so the two would not be nested.

When `localize = TRUE`, univariate score tests for equality constraints
are retained as diagnostic evidence. They are explicitly not used to
modify the fitted model.

## Latent means

Once intercepts are invariant, fully or with researcher-specified
partial releases, groups can be compared on the construct itself (Byrne,
Shavelson, & Muthén, 1989). This is the structured-means form of
known-groups evidence: a difference theory predicts between groups that
differ on the construct. At each level that holds intercepts equal,
`latent_means` gives each group's latent means relative to the reference
group, the first group, which the default `ID.fac = "std.lv"` fixes at a
mean of 0 and a variance of 1. Each mean is then a difference in the
reference group's latent standard deviations, the effect size Hancock
(2001) describes. The fitted model decides which means are reported:
only those of factors whose reference mean is fixed at 0 and variance at
1, and only where the compared group's mean is estimated. So the table
is empty under another identification, and for a model with higher-order
factors, which
[`semTools::measEq.syntax()`](https://rdrr.io/pkg/semTools/man/measEq.syntax.html)
identifies by unit loadings (the decision log records the switch). A
factor whose intercepts are all released has no estimated difference;
the decision log says so. Where the intercepts are not invariant, the
means are not comparable until the non-invariant intercepts are released
with
[`nomo_partial()`](https://juhalt.github.io/nomologR/reference/nomo_partial.md)
(Vandenberg & Lance, 2000).

## References

Historical foundations:

Jöreskog, K. G. (1971). Simultaneous factor analysis in several
populations. *Psychometrika, 36*(4), 409-426.
[doi:10.1007/BF02291366](https://doi.org/10.1007/BF02291366)

Hancock, G. R. (2001). Effect size, power, and sample size determination
for structured means modeling and MIMIC approaches to between-groups
hypothesis testing of means on a single latent construct.
*Psychometrika, 66*(3), 373-388.
[doi:10.1007/BF02294440](https://doi.org/10.1007/BF02294440)

Meredith, W. (1993). Measurement invariance, factor analysis and
factorial invariance. *Psychometrika, 58*(4), 525-543.
[doi:10.1007/BF02294825](https://doi.org/10.1007/BF02294825)

Vandenberg, R. J., & Lance, C. E. (2000). A review and synthesis of the
measurement invariance literature: Suggestions, practices, and
recommendations for organizational research. *Organizational Research
Methods, 3*(1), 4-70.
[doi:10.1177/109442810031002](https://doi.org/10.1177/109442810031002)

Change-in-fit evidence:

Chen, F. F. (2007). Sensitivity of goodness of fit indexes to lack of
measurement invariance. *Structural Equation Modeling, 14*(3), 464-504.
[doi:10.1080/10705510701301834](https://doi.org/10.1080/10705510701301834)

Cheung, G. W., & Rensvold, R. B. (2002). Evaluating goodness-of-fit
indexes for testing measurement invariance. *Structural Equation
Modeling, 9*(2), 233-255.
[doi:10.1207/S15328007SEM0902_5](https://doi.org/10.1207/S15328007SEM0902_5)

Putnick, D. L., & Bornstein, M. H. (2016). Measurement invariance
conventions and reporting: The state of the art and future directions
for psychological research. *Developmental Review, 41*, 71-90.
[doi:10.1016/j.dr.2016.06.004](https://doi.org/10.1016/j.dr.2016.06.004)

Ordered-categorical indicators:

Svetina, D., Rutkowski, L., & Rutkowski, D. (2020). Multiple-group
invariance with categorical outcomes using updated guidelines: An
illustration using Mplus and the lavaan/semTools packages. *Structural
Equation Modeling, 27*(1), 111-130.
[doi:10.1080/10705511.2019.1602776](https://doi.org/10.1080/10705511.2019.1602776)

Wu, H., & Estabrook, R. (2016). Identification of confirmatory factor
analysis models of different levels of invariance for ordered
categorical outcomes. *Psychometrika, 81*(4), 1014-1045.
[doi:10.1007/s11336-016-9506-0](https://doi.org/10.1007/s11336-016-9506-0)

## See also

[`nomo_partial()`](https://juhalt.github.io/nomologR/reference/nomo_partial.md)
for researcher-specified releases.

## Examples

``` r
inv <- nomo_invariance(
  "Agency =~ ag1 + ag2 + ag3 + ag4",
  data = nomo_demo_network,
  group = "group",
  levels = c("configural", "metric", "scalar")
)
inv
#> <nomo_invariance> Measurement invariance
#> Grouping variable: group (2 groups: online, paper) | Indicators: continuous
#> Requested: configural -> metric -> scalar
#> Completed: configural -> metric -> scalar
#> 
#>   Level         CFI  RMSEA   SRMR  CFI change  RMSEA change   LRT p
#>   configural  1.000  0.000  0.002           -             -       -
#>   metric      1.000  0.000  0.028      +0.000        +0.000    .146
#>   scalar      0.954  0.121  0.065      -0.046        +0.121  < .001
#> Localized equality-constraint diagnostics retained: 12
#> 
#> Fit changes and score diagnostics are evidence. They are not pass/fail rules,
#> and nomologR never frees a parameter because of them.
nomo_table(inv, "fit")
#> # A tibble: 3 × 19
#>   level     constraints partial_requested status converged  chisq    df   pvalue
#>   <chr>     <chr>       <chr>             <chr>  <lgl>      <dbl> <dbl>    <dbl>
#> 1 configur… none        ""                estim… TRUE       0.327     4 9.88e- 1
#> 2 metric    loadings    ""                estim… TRUE       5.71      7 5.75e- 1
#> 3 scalar    loadings, … ""                estim… TRUE      68.9      10 7.12e-11
#> # ℹ 11 more variables: cfi <dbl>, rmsea <dbl>, srmr <dbl>, delta_cfi <dbl>,
#> #   delta_rmsea <dbl>, delta_srmr <dbl>, lrt_chisq <dbl>, lrt_df <dbl>,
#> #   lrt_p <dbl>, warnings <chr>, error <chr>
head(nomo_table(inv, "local_strain"))
#> # A tibble: 6 × 8
#>   level  constraint_index constraint    score_x2    df  p_value diagnostic_only
#>   <chr>             <int> <chr>            <dbl> <dbl>    <dbl> <lgl>          
#> 1 metric                3 .p3. == .p17.   4.92       1 2.66e- 2 TRUE           
#> 2 metric                2 .p2. == .p16.   1.03       1 3.10e- 1 TRUE           
#> 3 metric                4 .p4. == .p18.   0.690      1 4.06e- 1 TRUE           
#> 4 metric                1 .p1. == .p15.   0.0273     1 8.69e- 1 TRUE           
#> 5 scalar                7 .p7. == .p21.  61.1        1 5.33e-15 TRUE           
#> 6 scalar                5 .p5. == .p19.  11.3        1 7.68e- 4 TRUE           
#> # ℹ 1 more variable: constraint_display <chr>

# \donttest{
# A release is a documented researcher decision, never an automatic search.
release <- nomo_partial(
  level = "scalar",
  syntax = "ag3 ~ 1",
  rationale = "Prior evidence anticipated a mode difference in ag3 wording."
)
inv_partial <- nomo_invariance(
  "Agency =~ ag1 + ag2 + ag3 + ag4",
  data = nomo_demo_network,
  group = "group",
  levels = c("configural", "metric", "scalar"),
  partial = release
)
nomo_table(inv_partial, "fit")
#> # A tibble: 3 × 19
#>   level  constraints partial_requested status converged chisq    df pvalue   cfi
#>   <chr>  <chr>       <chr>             <chr>  <lgl>     <dbl> <dbl>  <dbl> <dbl>
#> 1 confi… none        ""                estim… TRUE      0.327     4  0.988     1
#> 2 metric loadings    ""                estim… TRUE      5.71      7  0.575     1
#> 3 scalar loadings, … "ag3 ~ 1"         estim… TRUE      6.79      9  0.659     1
#> # ℹ 10 more variables: rmsea <dbl>, srmr <dbl>, delta_cfi <dbl>,
#> #   delta_rmsea <dbl>, delta_srmr <dbl>, lrt_chisq <dbl>, lrt_df <dbl>,
#> #   lrt_p <dbl>, warnings <chr>, error <chr>
nomo_table(inv_partial, "partial")
#> # A tibble: 1 × 4
#>   release_id level  syntax  rationale                                           
#>   <chr>      <chr>  <chr>   <chr>                                               
#> 1 P1         scalar ag3 ~ 1 Prior evidence anticipated a mode difference in ag3…
# }
```
