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

  Optional character vector naming binary/ordinal indicators. Model
  indicators stored as ordered factors are treated as declared whether
  or not they are named here, because `lavaan` fits them as categorical;
  the decision log flags them for review. Names that are not variables
  in the model are not used, and the decision log lists them. When any
  indicator is ordered and `estimator = NULL`, `nomo_cfa()` explicitly
  requests `WLSMV`.

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

A `nomo_cfa` object. The fields to read are:

- `fit`: the unchanged `lavaan` fit, for anything else `lavaan` reports.

- `model`: the model syntax fitted.

- `converged`, `estimator`, `data_n`, `n_used`, and `sample_summary`.

- `fit_evidence`: global fit indices with their teaching references;
  `variant` names the `lavaan` measure each value comes from. Robust
  values are preferred, then scaled ones, then standard ones. When
  `lavaan` was asked for a scaled test statistic but could not compute
  it, every value is the standard one, and the decision log says so.

- `standardized_loadings`: one row per loading, with its interval, flag,
  and explanation.

- `factor_correlations`: latent correlations with intervals. A
  correlation the model fixes, such as the zero correlations of a
  bifactor model, is listed at its fixed value;
  [`summary()`](https://rdrr.io/r/base/summary.html) shows it as fixed
  rather than estimated.

- `heywood`: improper-solution signals, if any.

- `parameter_estimates` and `standardized_solution`: `lavaan`'s
  parameter tables, as tibbles.

- `residual_matrix` and `residual_pairs`: residual correlations, the
  pairs largest first.

- `modification_indices` and `top_modification_indices`: diagnostics
  only; nothing is freed.

- `engine_warnings`: warnings `lavaan` raised.

- `decision_log`.

In `sample_summary`, `pct_dropped` is a proportion between 0 and 1. The
tables flag in different vocabularies: `attention` is `"info"`,
`"review"`, or `"unavailable"` in `fit_evidence`, and `"KEEP"`,
`"REVIEW"`, or `"STRONG REVIEW"` in `standardized_loadings`; `severity`
in `heywood` and `decision_log` is `"info"`, `"review"`, or `"concern"`.
See **Conventions in returned tables** in
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md).

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

[`print()`](https://rdrr.io/r/base/print.html) shows the cases used, the
estimator, convergence, the fit indices, and how many flags were raised.
[`summary()`](https://rdrr.io/r/base/summary.html) adds the chi-square
test, the fit indices with their references, the standardized loadings,
the factor correlations, any improper solution, the largest residual
correlations and modification indices, and each flag with its
explanation; for a model that did not converge it shows only the flags.

## Details

The fitted `lavaan` object is retained unchanged in `$fit`. `nomologR`
does not free parameters, add residual covariances, delete indicators,
or refit a different model in response to fit statistics or modification
indices.

## References

Historical foundations:

Bentler, P. M., & Bonett, D. G. (1980). Significance tests and goodness
of fit in the analysis of covariance structures. *Psychological
Bulletin, 88*(3), 588-606.
[doi:10.1037/0033-2909.88.3.588](https://doi.org/10.1037/0033-2909.88.3.588)

Jöreskog, K. G. (1969). A general approach to confirmatory maximum
likelihood factor analysis. *Psychometrika, 34*(2), 183-202.
[doi:10.1007/BF02289343](https://doi.org/10.1007/BF02289343)

Fit evaluation:

Bentler, P. M. (1990). Comparative fit indexes in structural models.
*Psychological Bulletin, 107*(2), 238-246.
[doi:10.1037/0033-2909.107.2.238](https://doi.org/10.1037/0033-2909.107.2.238)

Browne, M. W., & Cudeck, R. (1992). Alternative ways of assessing model
fit. *Sociological Methods & Research, 21*(2), 230-258.
[doi:10.1177/0049124192021002005](https://doi.org/10.1177/0049124192021002005)

Hu, L.-T., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
covariance structure analysis: Conventional criteria versus new
alternatives. *Structural Equation Modeling, 6*(1), 1-55.
[doi:10.1080/10705519909540118](https://doi.org/10.1080/10705519909540118)

Marsh, H. W., Hau, K.-T., & Wen, Z. (2004). In search of golden rules:
Comment on hypothesis-testing approaches to setting cutoff values for
fit indexes and dangers in overgeneralizing Hu and Bentler's (1999)
findings. *Structural Equation Modeling, 11*(3), 320-341.
[doi:10.1207/s15328007sem1103_2](https://doi.org/10.1207/s15328007sem1103_2)

Estimation, respecification, and improper solutions:

Flora, D. B., & Curran, P. J. (2004). An empirical evaluation of
alternative methods of estimation for confirmatory factor analysis with
ordinal data. *Psychological Methods, 9*(4), 466-491.
[doi:10.1037/1082-989X.9.4.466](https://doi.org/10.1037/1082-989X.9.4.466)

Kolenikov, S., & Bollen, K. A. (2012). Testing negative error variances:
Is a Heywood case a symptom of misspecification? *Sociological Methods &
Research, 41*(1), 124-167.
[doi:10.1177/0049124112442138](https://doi.org/10.1177/0049124112442138)

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490-504.
[doi:10.1037/0033-2909.111.3.490](https://doi.org/10.1037/0033-2909.111.3.490)

Rhemtulla, M., Brosseau-Liard, P. É., & Savalei, V. (2012). When can
categorical variables be treated as continuous? A comparison of robust
continuous and categorical SEM estimation methods under suboptimal
conditions. *Psychological Methods, 17*(3), 354-373.
[doi:10.1037/a0029315](https://doi.org/10.1037/a0029315)

Rosseel, Y. (2012). lavaan: An R package for structural equation
modeling. *Journal of Statistical Software, 48*(2), 1-36.
[doi:10.18637/jss.v048.i02](https://doi.org/10.18637/jss.v048.i02)

## Examples

``` r
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
out <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
summary(out)
#> <nomo_cfa summary> Confirmatory factor analysis
#> Cases: 301 | Estimator: ML | Converged: yes
#> 
#> Global fit
#>   chi-square(24) = 85.31, p < .001
#>   Index  Value  Reference  90% CI          Flag
#>   CFI     .931       .950                  Review
#>   TLI    0.896      0.950                  Review
#>   RMSEA  0.092      0.060  [0.071, 0.114]  Review
#>   SRMR   0.065      0.080
#>   References are teaching values for review, not cutoffs.
#> 
#> Standardized loadings
#>   Factor   Indicator  Loading    SE  95% CI        Flag
#>   visual   x1            0.77  0.05  [0.66, 0.88]
#>   visual   x2            0.42  0.06  [0.31, 0.54]  Review
#>   visual   x3            0.58  0.06  [0.47, 0.69]
#>   textual  x4            0.85  0.02  [0.81, 0.90]
#>   textual  x5            0.86  0.02  [0.81, 0.90]
#>   textual  x6            0.84  0.02  [0.79, 0.88]
#>   speed    x7            0.57  0.05  [0.47, 0.67]
#>   speed    x8            0.72  0.05  [0.62, 0.82]
#>   speed    x9            0.67  0.05  [0.56, 0.77]
#> 
#> Factor correlations
#>   Factor 1  Factor 2    r  95% CI
#>   visual    textual   .46  [.33, .58]
#>   visual    speed     .47  [.33, .61]
#>   textual   speed     .28  [.15, .42]
#> 
#> Improper solutions
#>   No improper-solution signal, such as a negative residual variance, was
#>   detected.
#> 
#> Largest residual correlations
#>   Item 1  Item 2  Residual
#>   x7      x2         -0.19
#>   x5      x3         -0.15
#>   x9      x1          0.15
#>   x9      x3          0.15
#>   x7      x1         -0.14
#> 
#> Modification indices (diagnostic only)
#>   Parameter         MI    EPC  Std. EPC
#>   visual =~ x9   36.41   0.58      0.52
#>   x7 ~~ x8       34.15   0.54      0.86
#>   visual =~ x7   18.63  -0.42     -0.35
#>   x8 ~~ x9       14.95  -0.42     -0.81
#>   textual =~ x3   9.15  -0.27     -0.24
#>   Modification indices locate strain. They do not authorize freeing a
#>   parameter, and nomologR never does so automatically.
#> 
#> Flagged
#>   - CFI (Review): The value, .931, is below the teaching reference of .950;
#>     inspect global and localized model strain.
#>   - TLI (Review): The value, 0.896, is below the teaching reference of 0.950;
#>     inspect global and localized model strain.
#>   - RMSEA (Review): The value, 0.092, is above the teaching reference of
#>     0.060; inspect global and localized model strain.
#>   - x2 on visual (Review): Absolute standardized loading is below the
#>     configured teaching reference of 0.50; inspect item content, precision,
#>     and model specification.
#> 
#> What these columns mean
#>   ML -- Maximum likelihood.
#>   CFI -- Comparative fit index.
#>   TLI -- Tucker-Lewis index.
#>   RMSEA -- Root mean square error of approximation.
#>   SRMR -- Standardized root mean square residual.
#>   CI -- Confidence interval.
#>   SE -- Standard error.
#>   MI -- Modification index, the expected drop in the chi-square if the
#>       parameter were freed.
#>   EPC -- Expected parameter change if the parameter were freed.
#>   Std. EPC -- The expected parameter change with all variables standardized.
#> 
#> Global fit, local strain, and parameter estimates are evidence to interpret
#> together; no single cutoff establishes model validity.
#> 
#> See nomo_table(x, "decision_log") for the decision log and x$fit for lavaan's
#> full output.

# \donttest{
# Declared ordered indicators request WLSMV rather than ML
ordinal_model <- nomo_model(list(
  A = c("a1", "a2", "a3", "a4", "a5"),
  B = c("b1", "b2", "b3", "b4", "b5")
))
out_ord <- nomo_cfa(
  ordinal_model,
  data = nomo_demo_ordinal,
  ordered = names(nomo_demo_ordinal)
)
out_ord$fit_evidence
#> # A tibble: 9 × 7
#>   metric                value variant  reference direction attention explanation
#>   <chr>                 <dbl> <chr>        <dbl> <chr>     <chr>     <chr>      
#> 1 chi_square     93.5         chisq.s…     NA    informat… info      Descriptiv…
#> 2 df             34           df.scal…     NA    informat… info      Descriptiv…
#> 3 p_value         0.000000188 pvalue.…     NA    informat… info      Descriptiv…
#> 4 CFI             0.971       cfi.rob…      0.95 higher    info      At or abov…
#> 5 TLI             0.961       tli.rob…      0.95 higher    info      At or abov…
#> 6 RMSEA           0.0535      rmsea.r…      0.06 lower     info      At or belo…
#> 7 RMSEA_CI_lower  0.0348      rmsea.c…     NA    informat… info      Descriptiv…
#> 8 RMSEA_CI_upper  0.0719      rmsea.c…     NA    informat… info      Descriptiv…
#> 9 SRMR            0.0475      srmr          0.08 lower     info      At or belo…
# }
```
