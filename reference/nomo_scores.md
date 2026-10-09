# Score a measurement model, with the evidence for the scoring choice

Computes scores from a fitted measurement model and reports what those
scores are and are not. Scoring is a modeling decision, and
`nomo_scores()` documents the decision rather than making it.

## Usage

``` r
nomo_scores(
  fit,
  method = c("sum", "mean", "regression", "bartlett"),
  guidance = nomo_defaults()
)
```

## Arguments

- fit:

  A `nomo_cfa` object or fitted `lavaan` measurement model. Single-group
  and single-level, with every factor measured by observed items and no
  structural paths or covariates: no regressions, no covariance between
  a factor and an observed variable, and no higher-order factors.

- method:

  Scoring method. `"sum"` and `"mean"` are unit weighted; `"regression"`
  and `"bartlett"` are model weighted.

- guidance:

  A `nomo_guidance` object from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).
  It is checked and recorded with the result; the references the notes
  apply are fixed values from the literature (see Details), not settings
  read from it.

## Value

An object of class `nomo_scores`. The fields to read are:

- `scores`: one column per factor, one row per case used.

- `rows`: for each row of `scores`, the row of the data the model was
  fitted to. A case the model did not use, as listwise deletion drops an
  incomplete one, has no score, so `scores` can have fewer rows than the
  data. `data[x$rows, ]` holds the scored cases in the order of
  `scores`.

- `method` and `weighting`: how the scores were computed.

- `diagnostics`: Grice's validity, univocality, and correlational
  accuracy per factor.

- `unit_weighting`: the loading evidence on whether unit weights suit
  the model, and `parallel_test`, the test of the parallel model that
  unit weighting assumes.

- `score_correlations` and `factor_correlations`: for comparing the two.

- `notes`: what the scores do and do not estimate.

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

## Details

**Unit weighting is a model.** McNeish and Wolf (2020) show that adding
items is not a model-free arithmetic calculation but a *parallel* factor
model, assuming equal unstandardized loadings and equal residual
variances. For `method = "sum"` and `method = "mean"`, that constrained
model is fitted and compared with the model supplied, so a researcher
can see whether the assumption their sum score makes is consistent with
their data. The parallel model is the supplied model with those two sets
of constraints added and nothing else changed, estimated as the supplied
model was: the same cases, estimator, and missing-data handling, with
factor covariances and residual covariances kept as specified. When the
comparison cannot be made, `parallel_test$note` says why.

**A score is not the latent variable.** Grice (2001) evaluates factor
scores on three criteria, all reported here and all computed from the
fitted model:

- `validity`, the correlation between a score and the factor it
  estimates. For `method = "regression"` this equals the factor
  determinacy coefficient reported by
  [`nomo_hierarchical()`](https://juhalt.github.io/nomologR/reference/nomo_hierarchical.md),
  because that method maximizes it.

- `univocality`, its largest correlation with a factor it does not
  represent.

- `correlational_accuracy`, how far correlations among scores sit from
  the correlations among the factors they stand in for: for each factor,
  the score correlation minus the factor correlation for the pair where
  the two differ most, so 0 is best.

The third deserves attention before scores are used in later analyses. A
relationship estimated from scores carries that discrepancy as bias, and
its direction depends on the scoring method and the model rather than
being a constant that can be corrected for. Where a question can be
asked of the latent variables instead, asking it of scores replaces an
unbiased answer with a biased one.

**Univocality is judged against the factor correlations.** When factors
correlate, a score correlates with the other factors through its own: by
the factor correlation times its validity. Bartlett scores, and sum
scores of items that each load on one factor, correlate with the other
factors by exactly that much. Only a departure from it is something a
score takes from another factor directly, so a note is raised when a
score's correlation with another factor departs from the factor
correlation times its validity by .05 or more, and not merely because
factors correlate (Grice, 2001).

**Ordered indicators.** With ordered indicators lavaan computes
`"regression"` scores as empirical Bayes modal scores, and `"bartlett"`
scores as maximum-likelihood scores, from the categorical model; `"sum"`
and `"mean"` add the observed category numbers. The diagnostics are
computed from linear weights on the continuous latent responses
underlying the items, so for ordered indicators they approximate the
properties of the scores returned, and a note says so.

**One design recovers a regression.** For a linear regression among
factors, Skrondal and Laake (2001) proved that regression-method scores
for the predictors and Bartlett scores for the outcome, each block
scored from a measurement model of its own, give consistent estimates of
the regression coefficients. Both conditions matter: scoring every
factor from one model, or using the same method for both blocks, does
not. Standard errors that treat the scores as observed are not corrected
by this, and the result does not extend to nonlinear models.
[`vignette("scoring", package = "nomologR")`](https://juhalt.github.io/nomologR/articles/scoring.md)
works through the design.

**Thresholds are context.** Gorsuch's (1983) recommendation that
validity reach .80, and above .90 for scores serving as substitutes for
the factors themselves, is reported where a value falls below it and is
never applied as a rule. These references, and the .05 for a discrepancy
between correlations, are fixed values from the literature; they are not
read from `guidance`.

The supplied data is never modified, and scores are computed only for
the cases the model used. `rows` records which rows of the data those
are, so the scores can be matched to the data when some cases were
dropped. A unit-weighted score needs every item of its factor: under
FIML a case the model used can lack an item, and its sum or mean is then
`NA`, which a note counts. Regression and Bartlett scores use the items
a case has.

[`print()`](https://rdrr.io/r/base/print.html) shows the score
properties with a key to their columns, each flagged note's first
sentence, and the parallel-model test when it is not flagged;
[`summary()`](https://rdrr.io/r/base/summary.html) adds the loading
spread and every note in full.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws each
score's validity against Gorsuch's references.

## References

Gorsuch, R. L. (1983). *Factor analysis* (2nd ed.). Lawrence Erlbaum.

Grice, J. W. (2001). Computing and evaluating factor scores.
*Psychological Methods, 6*(4), 430-450.
[doi:10.1037/1082-989X.6.4.430](https://doi.org/10.1037/1082-989X.6.4.430)

McNeish, D., & Wolf, M. G. (2020). Thinking twice about sum scores.
*Behavior Research Methods, 52*(6), 2287-2305.
[doi:10.3758/s13428-020-01398-0](https://doi.org/10.3758/s13428-020-01398-0)

Skrondal, A., & Laake, P. (2001). Regression among factor scores.
*Psychometrika, 66*(4), 563-575.
[doi:10.1007/BF02296196](https://doi.org/10.1007/BF02296196)

## See also

[`nomo_hierarchical()`](https://juhalt.github.io/nomologR/reference/nomo_hierarchical.md)
for factor determinacy and construct replicability.

## Examples

``` r
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)

# A sum score, with the parallel model it assumes fitted and compared
summed <- nomo_scores(cfa, method = "sum")
summed
#> <nomo_scores> Scores from a measurement model
#> Method: sum (unit weighted) | Factors: 3 | Cases: 301
#> 
#> Score properties (Grice, 2001)
#>   Factor   Items  Validity  Univocality  Correlational accuracy
#>   visual       3       .79          .37                    -.16
#>   textual      3       .94          .43                    -.12
#>   speed        3       .83          .39                    -.16
#> 
#> What these columns mean
#>   Validity -- Correlation of the score with its own factor; higher is better.
#>   Univocality -- Largest correlation of the score with another factor. Through
#>       its own factor, a score reaches another by the factor correlation times
#>       its validity; a departure of .05 or more from that is noted.
#>   Correlational accuracy -- Score correlation minus factor correlation, for
#>       the pair of factors where they differ most; 0 is best.
#> 
#> Flagged
#>   - Correlational accuracy (Concern): Correlations among these scores do not
#>     reproduce the correlations among the factors: the largest discrepancy is
#>     -.16, between visual and speed.
#>   - Unit weighting (Review): The parallel model that unit weighting assumes
#>     fits worse than the model you fitted (Delta chi-square(12) = 43.27,
#>     p < .001).
#>   - Validity (Review): Validity is below .90 for visual and speed.
#> 
#> No value here is a pass/fail threshold, and no scoring method is chosen for
#> you.
#> 
#> See summary(x) for every note in full and the loading spread and
#> nomo_table(x, "diagnostics") for the score properties as a table.
summed$unit_weighting
#> # A tibble: 3 × 6
#>   factor  n_items min_loading max_loading loading_ratio loading_sd
#>   <chr>     <int>       <dbl>       <dbl>         <dbl>      <dbl>
#> 1 visual        3       0.424       0.772          1.82    0.174  
#> 2 textual       3       0.838       0.855          1.02    0.00901
#> 3 speed         3       0.570       0.723          1.27    0.0775 

# Regression-method factor scores, with Grice's criteria
refined <- nomo_scores(cfa, method = "regression")
refined$diagnostics
#> # A tibble: 3 × 5
#>   factor  n_items validity univocality correlational_accuracy
#>   <chr>     <int>    <dbl>       <dbl>                  <dbl>
#> 1 visual        3    0.849       0.520                 0.123 
#> 2 textual       3    0.942       0.469                 0.0931
#> 3 speed         3    0.849       0.504                 0.123 
head(refined$scores)
#> # A tibble: 6 × 3
#>    visual textual   speed
#>     <dbl>   <dbl>   <dbl>
#> 1 -0.818  -0.138   0.0615
#> 2  0.0495 -1.01    0.625 
#> 3 -0.761  -1.87   -0.841 
#> 4  0.419   0.0185 -0.271 
#> 5 -0.416  -0.122   0.194 
#> 6  0.0233 -1.33    0.709 

# Scores joined to the data, by the rows the model used
scored <- lavaan::HolzingerSwineford1939
scored[refined$rows, names(refined$scores)] <- refined$scores
head(scored[, c("id", names(refined$scores))])
#>   id      visual     textual       speed
#> 1  1 -0.81767524 -0.13754501  0.06150726
#> 2  2  0.04951940 -1.01272402  0.62549360
#> 3  3 -0.76139670 -1.87228634 -0.84057276
#> 4  4  0.41934153  0.01848569 -0.27133710
#> 5  5 -0.41590481 -0.12225009  0.19432951
#> 6  6  0.02325632 -1.32981727  0.70885348
```
