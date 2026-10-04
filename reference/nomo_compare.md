# Compare confirmatory measurement models

`nomo_compare()` places two or more fitted
[`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
models side by side and reports the evidence that bears on the
comparison: a difference test matched to the estimator when the models
are nested, changes in global fit indices, information criteria when
they are defined, and standardized loadings, reliability, and
construct-separation evidence for each model. A researcher rationale is
required and recorded. The function never selects a "winning" model.

## Usage

``` r
nomo_compare(
  ...,
  rationale,
  origin = c("a_priori", "post_hoc"),
  nested = c("auto", "yes", "no"),
  reference = 1L,
  method = "default",
  evidence = TRUE,
  guidance = nomo_defaults()
)
```

## Arguments

- ...:

  Two or more `nomo_cfa` objects. Argument names become model labels,
  for example
  `nomo_compare(full = cfa_full, reduced = cfa_reduced, rationale = "...")`.
  A label must be unique and cannot be `factor` or `item`, which name
  the key columns of the loadings table.

- rationale:

  Required character scalar recording why these models are compared (for
  example, the theoretical question each model represents).

- origin:

  Whether the comparison was planned before seeing results
  (`"a_priori"`) or prompted by results such as modification indices or
  residuals (`"post_hoc"`). Post hoc comparisons are flagged in the
  decision log.

- nested:

  `"auto"` (default) checks nesting automatically; `"yes"` declares the
  models nested; `"no"` declares them non-nested and skips the
  difference test.

- reference:

  The model every other model is compared with, as an index or label.
  Defaults to the first model.

- method:

  Difference-test method passed to
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html).
  `"default"` lets lavaan choose the method appropriate to the
  estimator.

- evidence:

  Logical. If `TRUE` (default), compute side-by-side reliability
  ([`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md))
  and convergent/discriminant
  ([`nomo_validity()`](https://juhalt.github.io/nomologR/reference/nomo_validity.md))
  evidence for each model.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

A `nomo_compare` object. The fields to read are:

- `models`: fit and information criteria for each model. `df` is the
  degrees of freedom of the chi-square shown, which for
  mean-and-variance adjusted tests (such as `MLMVS`) differ from the
  model's.

- `comparisons`: for each model against the `reference`, the nesting
  relation, the difference test, changes in fit and information
  criteria, and an interpretation. `relation` is one of
  `"more_constrained"`, `"less_constrained"`, `"equivalent"`,
  `"non_nested"`, `"different_variables"`, or `"undetermined"` (the
  nesting check could not run and nesting was not declared).
  `nesting_check` is one of `"nested"`, `"not_nested"`, `"equivalent"`,
  `"unavailable"`, `"not_run"` (declared non-nested), or
  `"not_applicable"` (different observed variables). `df_difference` is
  the difference in the models' degrees of freedom.

- `loadings`: standardized loadings side by side.

- `evidence`: reliability, AVE, and HTMT2 by model, when
  `evidence = TRUE`. An HTMT2 pair is labeled in model order, as
  [`nomo_validity()`](https://juhalt.github.io/nomologR/reference/nomo_validity.md)
  lists it.

- `fits`: the fitted `nomo_cfa` objects; each keeps the warnings
  `lavaan` raised while fitting it in its own `engine_warnings`.

- `engine_warnings`: for each model compared with the `reference`, the
  warnings raised by the nesting check and the difference test.

- `reference`, `rationale`, `origin`, `references`, and `decision_log`.

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

[`print()`](https://rdrr.io/r/base/print.html) shows each comparison
with the reference model in one line (the difference test, the changes
in CFI and RMSEA, and the change in AIC) and any flags.
[`summary()`](https://rdrr.io/r/base/summary.html) adds the fit of every
model, the difference tests and changes in fit as tables, the
interpretations, the loadings side by side, and the measurement
evidence.

## Details

**Requirements.** All models must be converged `nomo_cfa` objects fitted
with the same estimator and missing-data handling to the same cases and
data. Otherwise the comparison is refused with an explanation, because
test statistics and fit indices would not be comparable. Missing-data
handling is compared as `lavaan` applied it, so `missing = "fiml"` and
`missing = "ml"` are the same handling. A model with an improper
solution, such as a negative residual variance, is compared, and the
decision log records a concern naming it (Kolenikov & Bollen, 2012).

**Nesting.** With `nested = "auto"`, nesting is checked from the models'
implied moments with
[`semTools::net()`](https://rdrr.io/pkg/semTools/man/net.html) (Bentler
& Satorra, 2010). The difference test is reported only for nested
models. If the check cannot run (for example for some categorical
models), the relation is reported as `"undetermined"` and flagged for
review; set `nested = "yes"` when one model is obtained from the other
by fixing or constraining parameters. A declaration that the check
contradicts is recorded as a concern and no test is reported.

**Difference tests.** Tests come from
[`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html):
the ordinary chi-square difference test for ML, the scaled difference
test for robust ML estimators (Satorra & Bentler, 2001), and the
scaled-and-shifted test for categorical estimators such as WLSMV
(Satorra, 2000). The method lavaan used is reported. A test that lavaan
cannot compute, or a negative scaled statistic, is reported as
unavailable with lavaan's message rather than replaced by a different
method; `method` can request an alternative explicitly.

**Information criteria.** AIC and BIC are reported for likelihood-based
estimators when the models contain the same observed variables. They are
not defined for WLSMV and are reported as unavailable, not substituted.

**Removing an item.** Models with different observed variables describe
different data, so neither a difference test nor information criteria
applies; only descriptive evidence is shown. To test whether an item is
needed, keep it in both models and fix its loading to zero in the
reduced model (for example `B =~ b1 + b2 + b3 + b4 + 0*b5`). Reliability
for such a model still includes the zero-loading item in the composite,
and the evidence table says so.

## References

Akaike, H. (1974). A new look at the statistical model identification.
*IEEE Transactions on Automatic Control, 19*(6), 716-723.
[doi:10.1109/TAC.1974.1100705](https://doi.org/10.1109/TAC.1974.1100705)

Bentler, P. M., & Satorra, A. (2010). Testing model nesting and
equivalence. *Psychological Methods, 15*(2), 111-123.
[doi:10.1037/a0019625](https://doi.org/10.1037/a0019625)

Burnham, K. P., & Anderson, D. R. (2004). Multimodel inference:
Understanding AIC and BIC in model selection. *Sociological Methods &
Research, 33*(2), 261-304.
[doi:10.1177/0049124104268644](https://doi.org/10.1177/0049124104268644)

Chen, F. F. (2007). Sensitivity of goodness of fit indexes to lack of
measurement invariance. *Structural Equation Modeling, 14*(3), 464-504.
[doi:10.1080/10705510701301834](https://doi.org/10.1080/10705510701301834)

Cheung, G. W., & Rensvold, R. B. (2002). Evaluating goodness-of-fit
indexes for testing measurement invariance. *Structural Equation
Modeling, 9*(2), 233-255.
[doi:10.1207/S15328007SEM0902_5](https://doi.org/10.1207/S15328007SEM0902_5)

Kolenikov, S., & Bollen, K. A. (2012). Testing negative error variances:
Is a Heywood case a symptom of misspecification? *Sociological Methods &
Research, 41*(1), 124-167.
[doi:10.1177/0049124112442138](https://doi.org/10.1177/0049124112442138)

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490-504.
[doi:10.1037/0033-2909.111.3.490](https://doi.org/10.1037/0033-2909.111.3.490)

Raftery, A. E. (1995). Bayesian model selection in social research.
*Sociological Methodology, 25*, 111-163.
[doi:10.2307/271063](https://doi.org/10.2307/271063)

Satorra, A. (2000). Scaled and adjusted restricted tests in multi-sample
analysis of moment structures. In R. D. H. Heijmans, D. S. G. Pollock, &
A. Satorra (Eds.), *Innovations in multivariate statistical analysis*
(pp. 233-247). Springer.
[doi:10.1007/978-1-4615-4603-0_17](https://doi.org/10.1007/978-1-4615-4603-0_17)

Satorra, A., & Bentler, P. M. (2001). A scaled difference chi-square
test statistic for moment structure analysis. *Psychometrika, 66*(4),
507-514. [doi:10.1007/BF02296192](https://doi.org/10.1007/BF02296192)

Satorra, A., & Bentler, P. M. (2010). Ensuring positiveness of the
scaled difference chi-square test statistic. *Psychometrika, 75*(2),
243-248.
[doi:10.1007/s11336-009-9135-y](https://doi.org/10.1007/s11336-009-9135-y)

Schwarz, G. (1978). Estimating the dimension of a model. *The Annals of
Statistics, 6*(2), 461-464.
[doi:10.1214/aos/1176344136](https://doi.org/10.1214/aos/1176344136)

## See also

[`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
to fit the models.

## Examples

``` r
full <- nomo_cfa(
  "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
  data = nomo_demo_continuous
)
# Keep b5 in the data but fix its loading to zero to test whether it is needed
no_b5 <- nomo_cfa(
  "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + 0*b5",
  data = nomo_demo_continuous
)

cmp <- nomo_compare(
  full = full,
  no_b5 = no_b5,
  rationale = "Evaluate whether the weakly loading item b5 contributes to factor B.",
  evidence = FALSE
)
cmp
#> <nomo_compare> Measurement-model comparison
#> Models: 2 | Reference: full | Estimator: ML | Cases: 473 of 500 used
#> Origin: a priori
#> Rationale: Evaluate whether the weakly loading item b5 contributes to factor
#> B.
#> 
#> Compared with full
#>   - no_b5 (nested, more constrained): Delta chi-square(1) = 46.91, p < .001;
#>     CFI change -.029, RMSEA change +0.022; AIC change +44.91
#> 
#> ML = maximum likelihood; CFI = comparative fit index; RMSEA = root mean square
#> error of approximation; AIC = Akaike information criterion.
#> 
#> No model was selected automatically; read the comparison with theory and the
#> recorded rationale.
#> 
#> See summary(x) for the interpretations and measurement evidence and
#> nomo_table(x, "comparisons") for every test.
nomo_table(cmp, "comparisons")
#> # A tibble: 1 × 23
#>   model reference relation    nested_declared nesting_check nested df_difference
#>   <chr> <chr>     <chr>       <chr>           <chr>         <lgl>          <dbl>
#> 1 no_b5 full      more_const… auto            nested        TRUE               1
#> # ℹ 16 more variables: test <chr>, method <chr>, chisq_diff <dbl>,
#> #   df_diff <dbl>, p_value <dbl>, test_available <lgl>, test_note <chr>,
#> #   delta_cfi <dbl>, delta_tli <dbl>, delta_rmsea <dbl>, delta_srmr <dbl>,
#> #   delta_aic <dbl>, delta_bic <dbl>, ic_available <lgl>, ic_note <chr>,
#> #   interpretation <chr>

# \donttest{
# Side-by-side loadings, reliability, AVE, and HTMT2 for each model
cmp_evidence <- nomo_compare(
  full = full,
  no_b5 = no_b5,
  rationale = "Evaluate whether the weakly loading item b5 contributes to factor B."
)
summary(cmp_evidence)
#> <nomo_compare summary> Measurement-model comparison
#> Rationale: Evaluate whether the weakly loading item b5 contributes to factor
#> B.
#> Reference: full | Estimator: ML | Cases: 473 of 500 used | Origin: a priori
#> 
#> Model fit
#>   Model  Chi-square  df       p   CFI    TLI  RMSEA   SRMR  Parameters
#>   full        75.83  34  < .001  .973  0.965  0.051  0.052          21
#>   no_b5      122.75  35  < .001  .944  0.928  0.073  0.093          20
#> 
#> Information criteria
#>   Model       AIC       BIC  Loadings fixed to zero
#>   full   11981.02  12068.36                       0
#>   no_b5  12025.93  12109.11                       1
#> 
#> Difference tests against the reference model
#>   Model  Delta chi-square  df       p  Relation
#>   no_b5             46.91   1  < .001  nested, more constrained
#> 
#> Changes in fit (model minus reference)
#>   Model    CFI     TLI   RMSEA    SRMR     AIC     BIC
#>   no_b5  -.029  -0.036  +0.022  +0.042  +44.91  +40.75
#> 
#> Interpretation
#>   - `no_b5` is nested within `full` and has 1 more degree of freedom
#>     (additional constraints). Delta chi-square(1) = 46.91, p < .001. A small p
#>     value indicates that the extra constraints are not fully consistent with
#>     the data; with large samples, even small misspecifications produce small p
#>     values. Change in fit (`no_b5` minus `full`): CFI -.029, TLI -0.036, RMSEA
#>     +0.022, SRMR +0.042. AIC +44.91 and BIC +40.75 (`no_b5` minus `full`);
#>     lower values favor a model for these data, and only differences are
#>     interpretable. No model is selected automatically; read this evidence with
#>     theory and the recorded rationale.
#> 
#> Standardized loadings by model
#>   Factor  Indicator  full  no_b5
#>   A       a1         0.77   0.77
#>   A       a2         0.74   0.74
#>   A       a3         0.67   0.67
#>   A       a4         0.74   0.74
#>   A       a5         0.60   0.60
#>   B       b1         0.79   0.79
#>   B       b2         0.69   0.70
#>   B       b3         0.76   0.76
#>   B       b4         0.63   0.62
#>   B       b5         0.34   0.00
#> 
#> Measurement evidence by model
#>   Construct  Metric  full  no_b5
#>   A          omega    .83    .83
#>   B          omega    .78    .62
#>   A          alpha    .83    .83
#>   B          alpha    .77    .77
#>   A          AVE      .50    .50
#>   B          AVE      .43    .52
#>   A vs. B    HTMT2   0.53   0.53
#>   - no_b5: The loading fixed to zero for b5 keeps that item in this composite;
#>     the coefficient does not describe a shortened scale.
#> 
#> What these columns mean
#>   ML -- Maximum likelihood.
#>   CFI -- Comparative fit index.
#>   TLI -- Tucker-Lewis index.
#>   RMSEA -- Root mean square error of approximation.
#>   SRMR -- Standardized root mean square residual.
#>   df -- Degrees of freedom.
#>   AIC -- Akaike information criterion.
#>   BIC -- Bayesian information criterion.
#>   AVE -- Average variance extracted.
#>   HTMT2 -- Heterotrait-monotrait ratio of correlations, geometric-mean
#>       version.
#> 
#> No model was selected automatically. Difference tests, changes in fit,
#> information criteria, and measurement evidence answer different questions;
#> read them together with theory and the recorded rationale.
#> 
#> See nomo_table(x, "decision_log") for the decision log and x$fits for each
#> model's own analysis.
plot(cmp_evidence, type = "loadings")

# }
```
