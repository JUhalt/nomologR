# Scoring a measurement model with nomo_scores()

After a measurement model is established, researchers usually need
numbers to carry into later analyses.
[`nomo_scores()`](https://juhalt.github.io/nomologR/reference/nomo_scores.md)
produces them, and reports what they are and are not.

Scoring is a modeling decision.
[`nomo_scores()`](https://juhalt.github.io/nomologR/reference/nomo_scores.md)
documents the decision; it does not make it.

## A model to score

``` r

items <- paste0("x", 1:8)

model <- "
  Agency      =~ x1 + x2 + x3 + x4
  Persistence =~ x5 + x6 + x7 + x8
"

set.seed(33)
loadings <- cbind(
  Agency      = c(.80, .75, .70, .65, 0, 0, 0, 0),
  Persistence = c(0, 0, 0, 0, .60, .55, .50, .45)
)
phi <- matrix(c(1, .5, .5, 1), 2)
sigma <- loadings %*% phi %*% t(loadings)
diag(sigma) <- 1
dimnames(sigma) <- list(items, items)

dat <- as.data.frame(matrix(rnorm(600 * 8), 600, 8) %*% chol(sigma))
names(dat) <- items

fit <- lavaan::cfa(model, data = dat, std.lv = TRUE)
```

## Adding items is a model, not arithmetic

The most common way to score a scale is to add its items. McNeish and
Wolf (2020) show that this is not a model-free calculation: adding items
assumes a *parallel* model, in which the unstandardized loadings and the
residual variances are equal across items. That assumption needs the
same justification as any other measurement model.

[`nomo_scores()`](https://juhalt.github.io/nomologR/reference/nomo_scores.md)
fits that constrained model and compares it with the model you supplied:

``` r

summed <- nomo_scores(fit, method = "sum")
summed
#> <nomo_scores> Scores from a measurement model
#> Method: sum (unit weighted) | Factors: 2 | Cases: 600
#> 
#> Score properties (Grice, 2001)
#>   Factor       Items  Validity  Univocality  Correlational accuracy
#>   Agency           4       .91          .52                    -.19
#>   Persistence      4       .75          .43                    -.19
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
#>     -.19, between Agency and Persistence.
#>   - Unit weighting (Review): The parallel model that unit weighting assumes
#>     fits worse than the model you fitted (Delta chi-square(12) = 30.70,
#>     p = .002).
#>   - Validity (Review): Validity is below .90 for Persistence.
#> 
#> No value here is a pass/fail threshold, and no scoring method is chosen for
#> you.
#> 
#> See summary(x) for every note in full and the loading spread and
#> nomo_table(x, "diagnostics") for the score properties as a table.
```

The loadings here were generated to differ, so the constraints are
rejected. That does not forbid a sum score. It establishes that the
items are not interchangeable in the way adding them assumes, so the
choice needs a reason beyond convenience, and the diagnostics say what
it costs.

## What a score is, and is not

Grice (2001) evaluates scores on three criteria, all reported here and
all computed from the fitted model rather than from the scores
themselves:

``` r

nomo_table(summed, "diagnostics")
#> # A tibble: 2 × 5
#>   factor      n_items validity univocality correlational_accuracy
#>   <chr>         <int>    <dbl>       <dbl>                  <dbl>
#> 1 Agency            4    0.909       0.525                 -0.186
#> 2 Persistence       4    0.746       0.431                 -0.186
```

`validity` is the correlation between a score and the factor it
estimates. Gorsuch (1983) recommended at least .80, and above .90 where
scores are used as substitutes for the factors themselves. For
`method = "regression"` this quantity is the factor determinacy
coefficient that
[`nomo_hierarchical()`](https://juhalt.github.io/nomologR/articles/hierarchical-models.md)
reports, because that method maximizes it.

`univocality` is a score’s largest correlation with a factor it does
*not* represent. When the factors correlate, a score reaches the others
through its own factor, by the factor correlation times its validity, so
a sizable value is expected: Bartlett scores, and sums of items that
each load on one factor, correlate with the other factors by exactly
that much. Only a departure from it is something the score takes from
another factor directly, and
[`nomo_scores()`](https://juhalt.github.io/nomologR/reference/nomo_scores.md)
notes a departure of .05 or more. A score that is not univocal in that
sense cannot be treated as though it measured its own factor alone.

`correlational_accuracy` is the one to read before using scores in a
later analysis. It is the difference between the correlations among the
scores and the correlations among the factors they stand in for.

## Scoring changes the answer

Compare what each method implies about the relationship between the two
constructs:

``` r

comparison <- lapply(c("sum", "regression", "bartlett"), function(m) {
  s <- nomo_scores(fit, method = m)
  data.frame(
    method = m,
    factor_correlation = s$factor_correlations[1, 2],
    score_correlation = s$score_correlations[1, 2],
    discrepancy = s$diagnostics$correlational_accuracy[[1]]
  )
})

do.call(rbind, comparison)
#>       method factor_correlation score_correlation discrepancy
#> 1        sum          0.5771865         0.3915393  -0.1856472
#> 2 regression          0.5771865         0.7278535   0.1506670
#> 3   bartlett          0.5771865         0.3950339  -0.1821526
```

The factor correlation is the same in every row, because it is a
property of the model. The score correlation is not. Unit-weighted and
Bartlett scores understate the relationship here, and regression scores
overstate it.

The direction is not a constant that could be corrected for: it depends
on the scoring method and on the model. Grice reports the discrepancy
running one way for orthogonal factors and the other for oblique ones.
So `nomologR` reports it and leaves it visible rather than adjusting for
it.

The practical consequence is worth stating plainly. A relationship
estimated from scores carries this discrepancy as bias. Where the
question can be asked of the latent variables instead, asking it of
scores replaces an unbiased answer with a biased one.

## One design recovers a regression

There is one established exception, and its conditions are specific. For
a linear regression of one factor on others, Skrondal and Laake (2001)
proved that the regression coefficients are estimated consistently when

- the predictors are scored with the regression method and the outcome
  with the Bartlett method, and
- each block is scored from a measurement model of its own, not from one
  model containing both.

Here is the design applied to the data above, with Agency predicting
Persistence, next to the two ways of getting it wrong:

``` r

agency_model <- lavaan::cfa("Agency =~ x1 + x2 + x3 + x4",
                            data = dat, std.lv = TRUE)
persistence_model <- lavaan::cfa("Persistence =~ x5 + x6 + x7 + x8",
                                 data = dat, std.lv = TRUE)

slope <- function(outcome, predictor) unname(coef(lm(outcome ~ predictor))[2])

agency <- nomo_scores(agency_model, method = "regression")$scores$Agency
persistence_bartlett <-
  nomo_scores(persistence_model, method = "bartlett")$scores$Persistence
persistence_regression <-
  nomo_scores(persistence_model, method = "regression")$scores$Persistence

joint_agency <- nomo_scores(fit, method = "regression")$scores$Agency
joint_persistence <- nomo_scores(fit, method = "bartlett")$scores$Persistence

data.frame(
  design = c(
    "Latent variables (the target)",
    "Regression then Bartlett, separate models",
    "Regression scores for both, separate models",
    "Regression then Bartlett, one joint model"
  ),
  slope = c(
    lavaan::lavInspect(fit, "cov.lv")["Agency", "Persistence"],
    slope(persistence_bartlett, agency),
    slope(persistence_regression, agency),
    slope(joint_persistence, joint_agency)
  )
)
#>                                        design     slope
#> 1               Latent variables (the target) 0.5771865
#> 2   Regression then Bartlett, separate models 0.5703658
#> 3 Regression scores for both, separate models 0.3203264
#> 4   Regression then Bartlett, one joint model 0.6873507
```

Both factors have a variance of 1, so the target slope is their
covariance in the fitted model. The design lands next to it. Using the
regression method for both blocks falls well short, and scoring both
factors from the one two-factor model overshoots, although it uses the
right method for each. Each condition matters.

Three limits come with the result. It is about regression coefficients,
not the correlations `correlational_accuracy` describes. The standard
errors [`lm()`](https://rdrr.io/r/stats/lm.html) reports treat the
scores as observed data, and Skrondal and Laake note that corrected
standard errors and confidence intervals may require resampling. And it
does not extend to nonlinear models, for which they caution that factor
score regression appears to perform very badly.

`nomologR` does not apply the design for you, because it depends on
which factor is the outcome, which is part of the research question.
Where the question can be asked of the latent variables themselves, that
remains the simpler route.

## Choosing a method

``` r

nomo_table(nomo_scores(fit, method = "regression"), "diagnostics")
#> # A tibble: 2 × 5
#>   factor      n_items validity univocality correlational_accuracy
#>   <chr>         <int>    <dbl>       <dbl>                  <dbl>
#> 1 Agency            4    0.916       0.575                  0.151
#> 2 Persistence       4    0.791       0.667                  0.151
```

With one factor, regression and Bartlett scores differ only by a scaling
constant and rank people identically, so the choice between them matters
only when there is more than one factor.

Reliability coefficients follow the weighting. The reliability of a
unit-weighted scale is omega computed from the fitted model, which
[`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md)
reports; coefficient alpha equals it only when the items’ loadings are
equal (essential tau-equivalence). Coefficient H, reported by
[`nomo_hierarchical()`](https://juhalt.github.io/nomologR/articles/hierarchical-models.md),
belongs to optimally weighted scores. Reporting one for the other
describes a scale that was not used.

## What is never done

The data you supply is never modified, and scores are returned only for
the cases the model actually used, so a case dropped from the fit does
not acquire a score.

No threshold here is applied as a rule. Gorsuch’s recommendations appear
where a value falls below them, labeled as his, in the same way fixed
fit-index cutoffs are treated elsewhere in the package.

## References

Gorsuch, R. L. (1983). *Factor analysis* (2nd ed.). Lawrence Erlbaum.

Grice, J. W. (2001). Computing and evaluating factor scores.
*Psychological Methods, 6*(4), 430–450.
<https://doi.org/10.1037/1082-989X.6.4.430>

McNeish, D., & Wolf, M. G. (2020). Thinking twice about sum scores.
*Behavior Research Methods, 52*(6), 2287–2305.
<https://doi.org/10.3758/s13428-020-01398-0>

Skrondal, A., & Laake, P. (2001). Regression among factor scores.
*Psychometrika, 66*(4), 563–575. <https://doi.org/10.1007/BF02296196>
