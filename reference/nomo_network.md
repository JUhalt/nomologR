# Evaluate a theory-specified nomological network

`nomo_network()` combines a researcher-specified measurement/SEM model
with a machine-readable object from
[`nomo_hypotheses()`](https://juhalt.github.io/nomologR/reference/nomo_hypotheses.md).
Hypothesized relations that are not already present in `model` can be
added transparently before fitting, so the theory object itself can
define the structural portion of the network.

## Usage

``` r
nomo_network(
  model,
  data,
  hypotheses,
  validation_data = NULL,
  add_missing = TRUE,
  ordered = NULL,
  estimator = NULL,
  missing = NULL,
  std.lv = TRUE,
  control = NULL,
  equivalence_alpha = 0.05,
  guidance = nomo_defaults()
)
```

## Arguments

- model:

  One non-empty lavaan SEM/measurement-model syntax string or an object
  created by
  [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md).

- data:

  A non-empty data frame or a `nomo_split` object. For a `nomo_split`,
  the calibration subset is the primary sample and the validation subset
  is reserved for replication.

- hypotheses:

  A `nomo_hypotheses` object.

- validation_data:

  Optional independent validation data frame. Do not use this together
  with a `nomo_split` object.

- add_missing:

  Logical. If `TRUE` (default), theory-specified relations absent from
  `model` are appended transparently before estimation.

- ordered:

  Optional character vector naming ordered indicators.

- estimator:

  Optional lavaan estimator. When ordered indicators are declared and
  `estimator = NULL`, WLSMV is requested.

- missing:

  Optional lavaan missing-data option.

- std.lv:

  Logical passed to
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

- control:

  Optional optimizer-control list passed to
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

- equivalence_alpha:

  One number strictly between 0 and .5. For quantitative negligible
  predictions, the equivalence confidence level is
  `1 - 2 * equivalence_alpha`.

- guidance:

  Guidance settings returned by
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

A `nomo_network` object retaining the primary fitted SEM, optional
validation fit, measurement context, relation-level theory evidence,
replication evidence, and decision log.

## Details

Directed `A -> B` hypotheses map to lavaan regression paths `B ~ A`.
Association `A <-> B` hypotheses map to covariance paths `A ~~ B`.

For quantitative `negligible(within = ...)` predictions,
`nomo_network()` evaluates the SESOI using a normal-approximation
equivalence confidence interval. With the default
`equivalence_alpha = .05`, this is a 90 percent interval, corresponding
to the usual two one-sided tests logic. A bare
[`negligible()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
prediction remains non-confirmable from `p > .05`.

The function can also fit the same prespecified model in a validation
sample. Pass `validation_data` explicitly, or pass a `nomo_split` object
as `data` to use its calibration and validation subsets. No model
relation is added or removed on the basis of validation results.

## Replication status when the sign changes

When a directional prediction's primary and validation point estimates
have opposite signs, `replication_status` is decided by the 95 percent
confidence intervals, not by the point estimates alone:

- `"sign_reversal"`: both intervals exclude zero, on opposite sides.
  Each sample on its own supports a relation in a different direction.

- `"direction_not_replicated"`: exactly one interval excludes zero. The
  other sample does not support that direction, but it does not
  establish the opposite direction either.

- `"sign_change_within_uncertainty"`: neither interval excludes zero, or
  an interval is unavailable. Neither sample distinguishes the relation
  from zero, and the sign change is compatible with sampling variability
  around a small or null relation.

Point estimates scattered around a null relation differ in sign about
half the time, so a sign change without interval evidence is not treated
as a substantive discrepancy. Requiring both intervals to exclude zero
is the interval counterpart of each sample separately rejecting a zero
relation in its own direction. The rule does not turn a non-significant
result into evidence of no relation: that claim needs a
`negligible(within = ...)` prediction with a researcher-specified
equivalence region (Lakens, Scheel, & Isager, 2018).

## Relationships estimated between observed variables

A hypothesis whose endpoints are latent variables is estimated with
their measurement error modelled. When an endpoint is an observed
variable, that error enters unmodelled. If the observed variable is a
composite of several items, such as a sum, mean, or factor score, the
estimated relationship carries the discrepancy
[`nomo_scores()`](https://juhalt.github.io/nomologR/reference/nomo_scores.md)
reports as correlational accuracy, which can be substantial and runs in
either direction depending on the scoring method and the model.

`nomo_network()` cannot tell from the model syntax whether an observed
variable is a composite or a single measured quantity, so it does not
guess. It classifies each hypothesis by whether its endpoints are
latent, and the decision log discloses observed endpoints: for review
when both ends are observed, and for information when one is. A single
measured variable, such as a criterion recorded without items, is not a
composite, and the disclosure says so.

Where the observed variables are composites, modelling their items as
indicators of latent variables removes the discrepancy, and
[`lavaan::sam()`](https://rdrr.io/pkg/lavaan/man/sam.html) estimates the
structural relationships after the measurement model (Rosseel & Loh,
2024).

Where they are factor scores and the hypothesis is a linear regression,
Skrondal and Laake (2001) showed that one scoring design gives
consistent estimates of the regression coefficients: regression-method
scores for the predictors and Bartlett scores for the outcome, each
block scored from a measurement model of its own. Scoring both with the
same method, or scoring all the factors from one model, does not, and
[`vignette("scoring", package = "nomologR")`](https://juhalt.github.io/nomologR/articles/scoring.md)
shows both failures. The standard errors still treat the scores as
observed, and Skrondal and Laake note that corrected ones may require
resampling. The result does not extend to nonlinear models. No
correction is applied automatically.

## References

Anderson, J. C., & Gerbing, D. W. (1988). Structural equation modeling
in practice: A review and recommended two-step approach. *Psychological
Bulletin, 103*(3), 411-423.
[doi:10.1037/0033-2909.103.3.411](https://doi.org/10.1037/0033-2909.103.3.411)

Cronbach, L. J., & Meehl, P. E. (1955). Construct validity in
psychological tests. *Psychological Bulletin, 52*(4), 281-302.
[doi:10.1037/h0040957](https://doi.org/10.1037/h0040957)

Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing
for psychological research: A tutorial. *Advances in Methods and
Practices in Psychological Science, 1*(2), 259-269.
[doi:10.1177/2515245918770963](https://doi.org/10.1177/2515245918770963)

Messick, S. (1995). Validity of psychological assessment: Validation of
inferences from persons' responses and performances as scientific
inquiry into score meaning. *American Psychologist, 50*(9), 741-749.
[doi:10.1037/0003-066X.50.9.741](https://doi.org/10.1037/0003-066X.50.9.741)

Rosseel, Y., & Loh, W. W. (2024). A structural after measurement
approach to structural equation modeling. *Psychological Methods,
29*(3), 561-588.
[doi:10.1037/met0000503](https://doi.org/10.1037/met0000503)

Schuirmann, D. J. (1987). A comparison of the two one-sided tests
procedure and the power approach for assessing the equivalence of
average bioavailability. *Journal of Pharmacokinetics and
Biopharmaceutics, 15*(6), 657-680.
[doi:10.1007/BF01068419](https://doi.org/10.1007/BF01068419)

Skrondal, A., & Laake, P. (2001). Regression among factor scores.
*Psychometrika, 66*(4), 563-575.
[doi:10.1007/BF02296196](https://doi.org/10.1007/BF02296196)

## Examples

``` r
model <- nomo_model(list(
  Agency = c("ag1", "ag2", "ag3", "ag4"),
  Persistence = c("pe1", "pe2", "pe3", "pe4"),
  SocialDesirability = c("sd1", "sd2", "sd3")
))

h <- nomo_hypotheses(
  "Agency -> Persistence" = positive(min = .20),
  "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15)),
  "Agency -> Performance" = positive()
)

net <- nomo_network(model, data = nomo_demo_network, hypotheses = h)
net
#> <nomo_network>
#> Primary sample: N = 800 | Converged: TRUE
#> Theory relations: 3 | Added transparently to model: 2
#> Measurement context: INFO | no configured measurement-context review signal was triggered
#> 
#> # A tibble: 3 × 9
#>   id    relation                      prediction theoretical_region estimate
#>   <chr> <chr>                         <chr>      <chr>                 <dbl>
#> 1 H1    Agency -> Persistence         positive   [0.2, +Inf)         0.458  
#> 2 H2    Agency <-> SocialDesirability negligible [-0.15, 0.15]       0.00773
#> 3 H3    Agency -> Performance         positive   (0, +Inf)           0.389  
#>   ci_lower ci_upper concordance confirmatory_status
#>      <dbl>    <dbl> <chr>       <chr>              
#> 1   0.389    0.526  Concordant  A priori           
#> 2  -0.0794   0.0948 Concordant  A priori           
#> 3   0.325    0.454  Concordant  A priori           
#> 
#> Interpretation rule: theory concordance, uncertainty, measurement quality, and replication are distinct evidence streams. Statistical significance alone is not a validity verdict.
nomo_table(net, "hypotheses")
#> # A tibble: 3 × 17
#>   id    relation    prediction theoretical_region scale estimate     se ci_lower
#>   <chr> <chr>       <chr>      <chr>              <chr>    <dbl>  <dbl>    <dbl>
#> 1 H1    Agency -> … positive   [0.2, +Inf)        stan…  0.458   0.0350   0.389 
#> 2 H2    Agency <->… negligible [-0.15, 0.15]      stan…  0.00773 0.0444  -0.0794
#> 3 H3    Agency -> … positive   (0, +Inf)          stan…  0.389   0.0329   0.325 
#> # ℹ 9 more variables: ci_upper <dbl>, p_value <dbl>,
#> #   equivalence_ci_lower <dbl>, equivalence_ci_upper <dbl>,
#> #   equivalence_supported <lgl>, concordance <chr>, confirmatory_status <chr>,
#> #   evidence_scope <chr>, measurement_attention <chr>

# \donttest{
# Evaluate the same prespecified network in calibration and validation rows
s <- nomo_split(nomo_demo_network, validation_prop = 0.40, seed = 2026)
net_rep <- nomo_network(model, data = s, hypotheses = h)
nomo_table(net_rep, "replication")
#> # A tibble: 3 × 10
#>   id    relation  prediction primary_estimate validation_estimate estimate_shift
#>   <chr> <chr>     <chr>                 <dbl>               <dbl>          <dbl>
#> 1 H1    Agency -… positive             0.472               0.441         -0.0308
#> 2 H2    Agency <… negligible          -0.0285              0.0531         0.0815
#> 3 H3    Agency -… positive             0.400               0.371         -0.0292
#> # ℹ 4 more variables: primary_concordance <chr>, validation_concordance <chr>,
#> #   replication_status <chr>, interpretation <chr>
# }
```
