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

  A `nomo_cfa` object or fitted `lavaan` measurement model.
  Single-group, single-level, with no regressions among latent
  variables.

- method:

  Scoring method. `"sum"` and `"mean"` are unit weighted; `"regression"`
  and `"bartlett"` are model weighted.

- guidance:

  A `nomo_guidance` object from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

An object of class `nomo_scores`: the `scores` themselves, a
`diagnostics` table of Grice's three criteria per factor, the
`unit_weighting` evidence, `notes`, and the fitted model.

## Details

**Unit weighting is a model.** McNeish and Wolf (2020) show that adding
items is not a model-free arithmetic calculation but a *parallel* factor
model, assuming equal unstandardized loadings and equal residual
variances. For `method = "sum"` and `method = "mean"`, that constrained
model is fitted and compared with the model supplied, so a researcher
can see whether the assumption their sum score makes is consistent with
their data.

**A score is not the latent variable.** Grice (2001) evaluates factor
scores on three criteria, all reported here and all computed from the
fitted model:

- `validity`, the correlation between a score and the factor it
  estimates. For `method = "regression"` this equals the factor
  determinacy coefficient reported by
  [`nomo_hierarchical()`](https://juhalt.github.io/nomologR/reference/nomo_hierarchical.md),
  because that method maximizes it.

- `univocality`, its correlation with the factors it does not represent.

- `correlational_accuracy`, how far correlations among scores sit from
  the correlations among the factors they stand in for.

The third deserves attention before scores are used in later analyses. A
relationship estimated from scores carries that discrepancy as bias, and
its direction depends on the scoring method and the model rather than
being a constant that can be corrected for. Where a question can be
asked of the latent variables instead, asking it of scores replaces an
unbiased answer with a biased one.

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
never applied as a rule.

The supplied data is never modified, and scores are computed only for
the cases the model used.

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
