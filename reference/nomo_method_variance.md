# Method variance from a marker variable: the comprehensive CFA marker technique

`nomo_method_variance()` tests whether a marker variable carries method
variance into a measurement model's indicators, whether that variance
biases the correlations among the substantive factors, and how much of
each factor's reliability it accounts for. It follows the comprehensive
CFA marker technique of Williams, Hartman, and Cavazotte (2010).

## Usage

``` r
nomo_method_variance(
  model,
  data,
  marker,
  alpha = 0.05,
  estimator = NULL,
  missing = NULL,
  marker_name = "Marker"
)
```

## Arguments

- model:

  A lavaan measurement model for the substantive factors, as a string or
  a
  [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md)
  object. Indicators load on one factor each.

- data:

  A data frame holding the substantive and marker indicators.

- marker:

  The marker variable's indicators: two or more numeric column names
  that `model` does not use.

- alpha:

  Significance level for the model comparisons. Default `.05`.

- estimator, missing:

  Optional lavaan `estimator` and `missing` options. With a robust
  estimator, the comparisons use lavaan's scaled difference tests.

- marker_name:

  Name given to the marker factor. Default `"Marker"`. It names a factor
  in lavaan syntax, so it must be a syntactic name, such as
  `"SocialDesirability"`, without spaces.

## Value

A `nomo_method_variance` object. The fields to read are:

- `models`: each model's chi-square, degrees of freedom, p value
  (`pvalue`), CFI, TLI, RMSEA, and SRMR. With a robust estimator, the
  chi-square is scaled and the indices are robust, as in
  [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md).

- `comparisons`: the three model comparisons, with their chi-square
  differences (`chisq_diff`), degrees of freedom (`df_diff`), and
  `p_value`.

- `retained`: `"Method-C"` or `"Method-U"`.

- `method_loadings`: each substantive indicator's standardized
  substantive and method loadings in the retained model, the share of
  its variance the marker accounts for, and the method loading's
  `p_value`.

- `reliability`: each substantive factor's Baseline reliability, its
  substantive and method parts, and `method_share`.

- `correlations`: each pair of substantive factors (`factor1`,
  `factor2`) and their correlation in the CFA, Baseline, retained,
  Method-S(.05), and Method-S(.01) models, with p values in the retained
  and sensitivity models (`retained_p_value`, `method_s_05_p_value`,
  `method_s_01_p_value`).

- `marker_correlations`: the marker's correlation with each substantive
  factor in the CFA model; `factor1` is the substantive factor and
  `factor2` the marker.

- `engine_warnings`: the warnings lavaan raised, by model and for the
  comparisons.

- `fits` and `decision_log`.

A table with one p value for a test or an estimate names it `p_value`,
as the rest of the package does. `correlations` has one for each model,
so each is named `<model>_p_value`. In `models`, `pvalue` is the p value
of each model's chi-square test, named as in the fit tables of
[`nomo_esem()`](https://juhalt.github.io/nomologR/reference/nomo_esem.md)
and
[`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md).

Other fields record the call, the settings used, and the syntax fitted.
They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

[`print()`](https://rdrr.io/r/base/print.html) shows the model
comparisons, the reliability decomposition, the substantive correlations
across the models, and any flag.
[`summary()`](https://rdrr.io/r/base/summary.html) adds each model's
fit, the loadings in the retained model, and each flag's recommendation.

## Details

**Experimental.** This function is experimental and remains so after
nomologR 1.0.0. Its output may be reorganized during 1.x, without a
deprecation period, as the marker technique is extended beyond
continuous indicators. Any change will be described in NEWS; see the
package help page,
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md),
for the stability policy.

**The marker.** A marker variable is theoretically unrelated to the
substantive variables, so what it shares with them is taken to be method
variance (Lindell & Whitney, 2001). Williams et al. (2010) expand the
definition: a marker should also tap one or more of the biases present
in the measurement context, such as social desirability, transient mood,
or common scale anchors (Podsakoff et al., 2003). A marker chosen only
because it is unrelated, such as a demographic, may capture no method
variance at all. The marker is measured by two or more indicators, three
or more by the authors' recommendation.

**Phase I: model comparisons.** Five models are fitted with
`lavaan::cfa(std.lv = TRUE)`:

1.  *CFA*: the substantive factors and the marker factor, all
    correlated, with no method loadings. It supplies the marker
    indicators' loadings and error variances.

2.  *Baseline*: those loadings and error variances fixed at their CFA
    values, so the marker factor keeps its meaning, and the marker made
    orthogonal to the substantive factors.

3.  *Method-C*: Baseline plus a loading from the marker on every
    substantive indicator, constrained equal.

4.  *Method-U*: the same loadings, free to differ.

5.  *Method-R*: the retained method model with the substantive factor
    correlations fixed at their Baseline values.

Baseline versus Method-C tests whether marker-based method variance is
present. Method-C versus Method-U tests whether its effects are equal,
and decides which method model is retained. The retained model versus
Method-R tests whether the method variance biases the substantive
correlations. Each comparison is a likelihood-ratio test at `alpha`.

**Phase II: reliability decomposition.** From the completely
standardized estimates, each substantive factor's reliability in the
Baseline model is decomposed, using the retained method model, into a
substantive part and a method part (Williams et al., 2010, equations
1-3). `method_share` is the method part as a proportion of the Baseline
reliability.

**Phase III: sensitivity.** Because the method loadings are estimates,
the retained model is refitted with them fixed at the ends of their 95%
and 99% confidence intervals farther from zero (Method-S(.05) and
Method-S(.01)), so a negative loading, as from a marker keyed opposite
to the substantive items, becomes more negative. The substantive
correlations are then compared across the models.

**Checks.** lavaan's warnings are kept for each model and comparison, in
`engine_warnings` and as review rows of the decision log, and a model
with an improper solution, such as a negative variance, is a concern.
The marker's loadings and error variances from the CFA anchor every
later model, so a CFA that does not converge, or gives the marker a
negative error variance or no standard errors, stops the analysis, as
does any model that does not converge. With two marker indicators, the
marker's loadings are identified only through its correlations with the
substantive factors, which the technique assumes are zero, so the
decision log asks for review.

**What the technique cannot do.** It requires the marker to be
orthogonal to the substantive factors. In simulations, with an ideal
marker it did not find method variance that was absent. With a nonideal
marker it sometimes did, and with either kind it did not recover the
substantive correlations accurately (Richardson, Simmering, & Sturman,
2009). The results are evidence about the method variance the chosen
marker captures, not corrected estimates, and other sources of method
variance may remain (Podsakoff, MacKenzie, & Podsakoff, 2012).

## References

Lindell, M. K., & Whitney, D. J. (2001). Accounting for common method
variance in cross-sectional research designs. *Journal of Applied
Psychology, 86*(1), 114-121.
[doi:10.1037/0021-9010.86.1.114](https://doi.org/10.1037/0021-9010.86.1.114)

Podsakoff, P. M., MacKenzie, S. B., Lee, J.-Y., & Podsakoff, N. P.
(2003). Common method biases in behavioral research: A critical review
of the literature and recommended remedies. *Journal of Applied
Psychology, 88*(5), 879-903.
[doi:10.1037/0021-9010.88.5.879](https://doi.org/10.1037/0021-9010.88.5.879)

Podsakoff, P. M., MacKenzie, S. B., & Podsakoff, N. P. (2012). Sources
of method bias in social science research and recommendations on how to
control it. *Annual Review of Psychology, 63*, 539-569.
[doi:10.1146/annurev-psych-120710-100452](https://doi.org/10.1146/annurev-psych-120710-100452)

Richardson, H. A., Simmering, M. J., & Sturman, M. C. (2009). A tale of
three perspectives: Examining post hoc statistical techniques for
detection and correction of common method variance. *Organizational
Research Methods, 12*(4), 762-800.
[doi:10.1177/1094428109332834](https://doi.org/10.1177/1094428109332834)

Williams, L. J., Hartman, N., & Cavazotte, F. (2010). Method variance
and marker variables: A review and comprehensive CFA marker technique.
*Organizational Research Methods, 13*(3), 477-514.
[doi:10.1177/1094428110366036](https://doi.org/10.1177/1094428110366036)

## Examples

``` r
# \donttest{
# Two substantive factors and a marker, all sharing a method factor
# (simulated).
population <- "
  A =~ 0.7*a1 + 0.7*a2 + 0.6*a3 + 0.6*a4
  B =~ 0.7*b1 + 0.6*b2 + 0.6*b3 + 0.5*b4
  M =~ 0.7*m1 + 0.7*m2 + 0.6*m3
  CMV =~ 0.3*a1 + 0.3*a2 + 0.3*a3 + 0.3*a4 + 0.3*b1 + 0.3*b2 + 0.3*b3 +
         0.3*b4 + 0.3*m1 + 0.3*m2 + 0.3*m3
  A ~~ 0.4*B
  A ~~ 0*M
  B ~~ 0*M
  CMV ~~ 0*A + 0*B + 0*M
"
set.seed(2010)
dat <- lavaan::simulateData(population, sample.nobs = 600, standardized = TRUE)
mv <- nomo_method_variance(
  "A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4",
  data = dat, marker = c("m1", "m2", "m3")
)
mv
#> <nomo_method_variance> Marker-based method variance
#> Williams, Hartman, and Cavazotte (2010).
#> Marker: m1, m2, m3 | Cases: 600 | Estimator: ML
#> Retained: Method-C | Comparisons at alpha = .05
#> 
#> Model comparisons
#>   Comparison             Question                  Delta chi-square  df       p
#>   Baseline vs. Method-C  Method variance present?             11.89   1  < .001
#>   Method-C vs. Method-U  Method effects equal?                 2.78   7    .905
#>   Method-C vs. Method-R  Correlations biased?                  0.07   1    .790
#> 
#> Reliability decomposition
#>   Factor  Total  Substantive  Method  Method share
#>   A         .80          .79     .02          2.1%
#>   B         .78          .77     .02          2.1%
#> 
#> Substantive correlations
#>   Factors    CFA  Baseline  Method-C  Method-S(.05)  Method-S(.01)
#>   A with B  .434      .434      .422           .422           .423
#> 
#> Flagged
#>   - Baseline vs. Method-C (Review): Marker-based method variance is present
#>     (Delta chi-square(1) = 11.89, p < .001).
#> 
#> Models
#>   CFA -- Confirmatory factor analysis of the substantive factors and the
#>       marker, all correlated, with no method loadings; it gives the marker's
#>       loadings and error variances.
#>   Baseline -- The marker uncorrelated with the substantive factors, its
#>       loadings and error variances fixed at their CFA values.
#>   Method-C -- Baseline plus equal marker loadings on every substantive item.
#>   Method-U -- Baseline plus marker loadings free to differ.
#>   Method-R -- The retained model with the substantive correlations fixed at
#>       their Baseline values.
#>   Method-S(.05), Method-S(.01) -- The retained method loadings fixed at the
#>       ends of their 95% and 99% intervals farther from zero.
#> 
#> df = degrees of freedom; ML = maximum likelihood.
#> 
#> The results describe the method variance this marker captures; they are not
#> corrected estimates, and other sources of method variance may remain.
#> 
#> See summary(x) for each model's fit and the method loadings and
#> nomo_table(x, "decision_log") for every recorded decision.
nomo_table(mv, "reliability")
#> # A tibble: 2 × 5
#>   factor reliability_total reliability_substantive reliability_method
#>   <chr>              <dbl>                   <dbl>              <dbl>
#> 1 A                  0.804                   0.788             0.0166
#> 2 B                  0.784                   0.767             0.0162
#> # ℹ 1 more variable: method_share <dbl>
# }
```
