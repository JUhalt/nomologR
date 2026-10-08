# Evaluate a theory-specified nomological network

`nomo_network()` combines a researcher-specified measurement/SEM model
with a machine-readable object from
[`nomo_hypotheses()`](https://juhalt.github.io/nomologR/reference/nomo_hypotheses.md).
Hypothesized relations that are not already present in `model` can be
added before fitting, so the theory object itself can define the
structural portion of the network. Each addition, and each relation the
additions change, is recorded.

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
  single_indicators = NULL,
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
  `model` are appended transparently before estimation: the directed
  paths first, then the associations the model still lacks.

- ordered:

  Optional character vector naming ordered indicators. A variable of the
  fitted model stored as an ordered factor, in `data` or in
  `validation_data`, is treated as declared whether or not it is named
  here, because lavaan fits such a column as ordered-categorical either
  way. The decision log lists the columns found this way for review. A
  column that is an ordered factor in only one of the two samples is
  declared ordered in both, so both are fitted with the same estimator;
  the sample that stores it as numbers is then fitted with a
  categorical-data estimator, and its estimates differ from a fit that
  treats the column as continuous. An exogenous covariate is the
  exception: lavaan does not model a covariate as ordered-categorical,
  so one stored as an ordered factor is refused. Supply it as a numeric
  column or as dummy-coded columns.

- estimator:

  Optional lavaan estimator. When ordered indicators are declared and
  `estimator = NULL`, WLSMV is requested.

- missing:

  Optional lavaan missing-data option.

- std.lv:

  Logical passed to
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html). It also
  sets the metric of an unstandardized estimate that involves a latent
  variable: with `TRUE`, each factor's variance, or residual variance
  for an outcome, is 1; with `FALSE`, each factor takes the units of its
  first indicator. See `scale` in
  [nomo_expectations](https://juhalt.github.io/nomologR/reference/nomo_expectations.md).
  The decision log names the identification for each such hypothesis
  (`unstandardized_metric`).

- control:

  Optional optimizer-control list passed to
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

- equivalence_alpha:

  One number strictly between 0 and .5. For quantitative negligible
  predictions, the equivalence confidence level is
  `1 - 2 * equivalence_alpha`.

- single_indicators:

  Optional composites to model as single-indicator latent variables: a
  named list, or a named numeric vector, whose names are observed
  variables that `model` uses and whose values are reliabilities, given
  as one number or as a
  [`nomo_single_indicator()`](https://juhalt.github.io/nomologR/reference/nomo_single_indicator.md)
  record. See **Single indicators**.

- guidance:

  Guidance settings returned by
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

A `nomo_network` object. The fields to read are:

- `hypothesis_evidence`: one row per hypothesis, with its prediction,
  estimate, interval, `concordance` with the prediction (see
  **Concordance**), `evidence_scope` (see **Evidence scope**), and
  interpretation.

- `replication_evidence`: the same comparison in `validation_data`, when
  given, with its `replication_status` (see **Replication status**).

- `fit_evidence`: global fit of the fitted model, with `chisq_version`
  and `index_version` naming the version of the chi-square and of CFI,
  TLI, and RMSEA reported: `"standard"`, `"scaled"`, or `"robust"`.

- `parameter_estimates` and `standardized_solution`: `lavaan`'s
  parameter tables, as tibbles.

- `measurement_context`: the loadings, the variances, and the fit of the
  measurement model alone (`fit`), with the test of the structural
  restrictions against it (`structural_test`), which qualify the
  structural evidence. See **Measurement context and model fit**.

- `model_fitted` and `model_relations`: the syntax fitted, and for each
  hypothesis whether its relation was already in the model to be fitted
  (`already_in_model`) or was added (`added_from_hypothesis`).

- `model_changes`: each relation the added paths fixed to zero or lavaan
  added (`change`), with the outcomes among its variables (`outcome`)
  and the variables the hypotheses brought into the model
  (`new_variable`).

- `data_n` and `n_used`: the rows of the primary sample and the cases
  the fit analyzed; `validation_n` and `validation_n_used` for the
  validation sample.

- `fit`: the `lavaan` fit, and `validation`, the validation fit, when
  given.

- `single_indicators`: one row per composite modeled as a single
  indicator, with its reliability, the reliability's standard error,
  coefficient, and source, the composite's variance, and the error
  variance fixed. Empty when `single_indicators` is not used.

- `single_indicator_sensitivity`: each hypothesis's estimate, interval,
  and concordance with each composite's reliability shifted by up to
  .10.

- `converged`, `engine_warnings`, and `decision_log`.

`measurement_attention` in `hypothesis_evidence`, `attention` in the
`summary` and `loadings` tables of `measurement_context`, and `severity`
in `decision_log` are `"info"`, `"review"`, or `"concern"`. See
**Conventions in returned tables** in
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md).

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

## Details

Directed `A -> B` hypotheses map to lavaan regression paths `B ~ A`.
Association `A <-> B` hypotheses map to covariance paths `A ~~ B`.

A directed path is in the model when `model` writes it. An association
is judged against the model that will be fitted, which is `model` with
the hypothesized directed paths in place: it is in the model when
`model` writes it or when lavaan adds it there automatically, as it does
between exogenous factors. `model_relations` records which relations
were already in the model and which were added.

An added path changes more than its own relation.
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) covaries
exogenous factors with one another, holds the covariances of observed
exogenous predictors at their sample values, and covaries the residuals
of outcomes that predict no other variable. It relates no other pair of
variables that neither `model` nor the hypotheses join: not a factor and
an observed predictor, not an outcome and an exogenous variable, and not
an outcome that predicts another variable and a variable it does not
predict. So once a path makes a variable an outcome, every relation it
had with an exogenous variable other than its predictors is fixed to
zero, although `model` estimated it, and two outcomes can gain a
residual covariance that neither `model` nor the hypotheses name. A
variable the hypotheses bring into the model is related only as these
rules allow: an observed predictor of a factor is uncorrelated with the
exogenous factors, for example. These zeros are restrictions of the
network, and their misfit counts against the theory's structure.
`model_changes` lists each relation fixed to zero although `model`
estimated it (`"fixed_to_zero"`), each relation of a variable the
hypotheses bring in that the fitted model fixes to zero
(`"not_estimated"`), and each residual covariance of two outcomes that
lavaan added (`"added_by_lavaan"`). The decision log records the first
two for review (`relation_constrained`) and the third for information
(`relation_auto_freed`). A relation the theory allows belongs in the
hypotheses or in `model`.

When the fitted model also predicts an endpoint of an association,
`A ~~ B` is a covariance between residuals. The model predicts a
variable when a directed path points to it, and when it is an indicator
of a factor, which includes a factor that loads on a higher-order
factor. The estimate is then the association that remains after those
predictors, a residual association, and it can differ from the overall
association in size and in sign. Such a hypothesis has `evidence_scope`
`"residual_association"`, its interpretation says so, and the decision
log flags it for review. If the theory concerns the overall association,
evaluate it in a model that does not predict those variables.

A pair of variables carries a directed path or an association, not both:
`nomo_network()` stops when an association hypothesis names two
variables that the model to be fitted joins with a directed path. It
also stops when a hypothesis would add a path opposite to another,
because a reciprocal pair is not identified without further
restrictions. A reciprocal pair that `model` itself writes is fitted as
written, and hypotheses about either direction are evaluated; whether
that model is identified, by instruments for example, is for the
researcher to establish.

For quantitative `negligible(within = ...)` predictions,
`nomo_network()` evaluates the smallest effect size of interest (SESOI),
the region the researcher treats as negligible, using a
normal-approximation equivalence confidence interval. With the default
`equivalence_alpha = .05`, this is a 90 percent interval, corresponding
to the usual two one-sided tests logic. A bare
[`negligible()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
prediction remains non-confirmable from `p > .05`.

The function can also fit the same prespecified model in a validation
sample. Pass `validation_data` explicitly, or pass a `nomo_split` object
as `data` to use its calibration and validation subsets. No model
relation is added or removed on the basis of validation results. The
validation sample's rows in the decision log have the stage
`"network_validation"`, so they are never read as a second entry for the
same relation.

Rows with a missing value are dropped by lavaan's default listwise
deletion. `n_used` records the cases analyzed, the output reports them
beside the rows supplied ("Cases: 479 of 800"), and the decision log
flags the loss for review (`cases_used`). With continuous indicators,
`missing = "fiml"` uses every case that has data.

[`print()`](https://rdrr.io/r/base/print.html) shows each hypothesis's
concordance, estimate, and the interval its concordance was judged on,
the replication evidence, and what is flagged about the cases, the
measurement context, the model fit, and the relations the added paths
changed. [`summary()`](https://rdrr.io/r/base/summary.html) adds the fit
of the network and of the measurement model alone, the changed
relations, each prediction in full, the concordance counts, and every
flag with its recommendation.

## Measurement context and model fit

The fit of the network model mixes two sources of misfit, the
measurement model and the structural restrictions, which Anderson and
Gerbing (1988) separate by fitting the measurement model first.
`nomo_network()` also fits the measurement model alone: the network with
its structural part saturated, in which every pair of factors and
observed variables outside the factor definitions that no path or
covariance joins is allowed to covary. The measurement context judges
the measurement model's own fit, with its loadings and variances. The
model fit, a separate decision-log row (`model_fit`), judges the
network's fit and, when the measurement model meets the references,
attributes the misfit to the structural part. The difference test
between the two (`structural_test`, a Delta chi-square from
[`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html),
scaled as the estimator requires) tests the structural restrictions: the
pairs the measurement model alone frees, those `model` itself leaves out
as well as those `model_changes` lists as fixed to zero. A model without
latent variables has no measurement model to judge.

The fit indices are the robust versions when lavaan reports them, and
the chi-square is the scaled one; `chisq_version` and `index_version` in
`fit_evidence` say which, and
[`summary()`](https://rdrr.io/r/base/summary.html) prints them.

## Concordance

`concordance` in `hypothesis_evidence` compares each estimate and its
confidence interval with the theoretical region of its prediction, on
the scale the prediction names (standardized by default). For
[`positive()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
and
[`negative()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
predictions the interval is the 95 percent confidence interval. For
`negligible(within = ...)` it is the equivalence interval, with
confidence `1 - 2 * equivalence_alpha` (90 percent by default). A bound
given as `min`, `max`, or `within` belongs to the region; zero, the
bound of a prediction of direction alone, does not. The values, in the
order the rules are applied:

- `"not_evaluable"`: the model did not converge, no parameter of the
  fitted model matches the relation, or the estimate has no standard
  error, as happens when the model is not identified. The estimate is
  not compared with the prediction.

- `"not_confirmable_without_sesoi"`: a bare
  [`negligible()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
  prediction, which gives no region to compare the interval with.

- `"concordant"`: the whole interval lies inside the region.

- `"directionally_concordant_imprecise"`: the estimate lies inside the
  region, and its interval extends outside it.

- `"inconsistent"`: the estimate and its whole interval lie outside the
  region. A relation with the predicted sign is inconsistent when its
  interval excludes the predicted magnitude.

- `"direction_concordant_below_magnitude"`: for a
  [`positive()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
  or
  [`negative()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
  prediction with `min` or `max`, the estimate has the predicted sign
  and is smaller in magnitude than the region requires, and its interval
  reaches the region.

- `"direction_concordant_above_magnitude"`: the same, with an estimate
  larger in magnitude than the region allows.

- `"inconclusive"`: any other estimate outside the region whose interval
  reaches the region, such as an estimate of the wrong sign whose
  interval includes values of the predicted sign.

The decision log records `"concordant"` for information,
`"inconsistent"` and `"not_evaluable"` as concerns, and the other values
for review. None is a verdict on validity: each is one piece of
evidence, read with the measurement context, the fit, and whether the
prediction was made a priori. Printed output and plots show the values
in short words, such as "In region, imprecise" for
`"directionally_concordant_imprecise"`, which also reads for a
[`negligible()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
prediction; the stored values do not change.

## Evidence scope

`evidence_scope` in `hypothesis_evidence` names what kind of parameter
the estimate is. For a directed path: `"latent_structural"` (factor to
factor), `"latent_to_observed_outcome"`, `"observed_to_latent"`, and
`"observed_structural"`. For an association: `"latent_association"`,
`"latent_observed_association"`, `"observed_association"`, and
`"residual_association"` when the fitted model predicts an endpoint, by
a directed path or a factor loading, whatever the endpoints are. A
relation without an estimate keeps the scope its endpoints give it.

## Replication status

With a validation sample, `replication_status` in `replication_evidence`
compares the two samples' estimates and concordance. The values, in the
order the rules are applied:

- `"not_evaluable"`: the relation is `"not_evaluable"` or has no
  estimate in at least one sample.

- `"sign_reversal"`, `"direction_not_replicated"`, and
  `"sign_change_within_uncertainty"`: a
  [`positive()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
  or
  [`negative()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
  prediction whose two point estimates have opposite signs. See
  **Replication status when the sign changes**.

- `"replicated_concordance"`: `"concordant"` in both samples.

- `"not_replicated"`: compatible with the prediction in the primary
  sample and `"inconsistent"` in the validation sample. Compatible means
  `"concordant"`, `"directionally_concordant_imprecise"`,
  `"direction_concordant_below_magnitude"`, or
  `"direction_concordant_above_magnitude"`, the values whose interval
  reaches the region.

- `"unstable"`: `"inconsistent"` in the primary sample and compatible
  with the prediction in the validation sample.

- `"replicated_inconsistency"`: `"inconsistent"` in both samples.

- `"direction_replicated_but_uncertain"`: a
  [`positive()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
  or
  [`negative()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
  prediction whose estimates have the same sign in both samples, without
  meeting a rule above. When that shared sign is the opposite of the
  prediction, the interpretation says so, and printed output labels it
  "Opposite in both" rather than "Same direction".

- `"mixed_or_inconclusive"`: any other pattern.

The decision log records `"replicated_concordance"` for information;
`"sign_reversal"`, `"direction_not_replicated"`, `"not_replicated"`,
`"unstable"`, `"replicated_inconsistency"`, and a
`"direction_replicated_but_uncertain"` opposite to the prediction as
concerns; and the other values for review.

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
their measurement error modeled. When an endpoint is an observed
variable, that error enters unmodeled. If the observed variable is a
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

Where the observed variables are composites, modeling their items as
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

Where they are composites whose reliability is known,
`single_indicators` corrects them (see **Single indicators**).

## Single indicators

A composite named in `single_indicators` becomes the one indicator of a
latent variable with the composite's name, so the model syntax and the
hypotheses are unchanged. Its error variance is fixed at \\(1 -
\rho)\sigma^2\\, the reliability's complement times the composite's
variance in the data being fitted, and the relationships it enters are
corrected for its unreliability (Bollen, 1989; Savalei, 2019).
[`nomo_single_indicator()`](https://juhalt.github.io/nomologR/reference/nomo_single_indicator.md)
explains the method's origin and evidence, and which reliability to use.

The correction is only as good as the reliability, and Savalei (2019)
found that misestimating it by more than about .05 costs accuracy. So
each hypothesis is refitted with each composite's reliability .05 and
.10 lower and higher, one composite at a time, and
`single_indicator_sensitivity` records the estimates, intervals, and
concordance at each. The decision log flags any hypothesis whose
concordance changes across that range.

When a reliability's standard error is supplied, the standard errors,
intervals, and concordance of the hypotheses add its uncertainty,
following Oberski and Satorra (2013): the variance of an estimate gains
its squared rate of change in the reliability times the reliability's
variance. The rate of change is taken from the refits within .05 of the
reliability. Without a standard error, the log says that the standard
errors treat the reliability as known.

## References

Anderson, J. C., & Gerbing, D. W. (1988). Structural equation modeling
in practice: A review and recommended two-step approach. *Psychological
Bulletin, 103*(3), 411-423.
[doi:10.1037/0033-2909.103.3.411](https://doi.org/10.1037/0033-2909.103.3.411)

Bollen, K. A. (1989). *Structural equations with latent variables*.
Wiley.
[doi:10.1002/9781118619179](https://doi.org/10.1002/9781118619179)

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

Oberski, D. L., & Satorra, A. (2013). Measurement error models with
uncertainty about the error variance. *Structural Equation Modeling,
20*(3), 409-428.
[doi:10.1080/10705511.2013.797820](https://doi.org/10.1080/10705511.2013.797820)

Rosseel, Y., & Loh, W. W. (2024). A structural after measurement
approach to structural equation modeling. *Psychological Methods,
29*(3), 561-588.
[doi:10.1037/met0000503](https://doi.org/10.1037/met0000503)

Savalei, V. (2019). A comparison of several approaches for controlling
measurement error in small samples. *Psychological Methods, 24*(3),
352-370. [doi:10.1037/met0000181](https://doi.org/10.1037/met0000181)

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
#> <nomo_network> Nomological network
#> Cases: 800 | Converged: yes
#> Theory relations: 3 | Added to the model from hypotheses: 2
#> Measurement context: no flags | Model fit: no flags
#> 
#> Hypothesis evidence
#>   ID  Relation                       Concordance  Estimate  CI
#>   H1  Agency -> Persistence          Concordant       0.46  [0.39, 0.53]
#>   H2  Agency <-> SocialDesirability  Concordant        .01  [-.07, .08]
#>   H3  Agency -> Performance          Concordant       0.39  [0.32, 0.45]
#>   Estimates are standardized. CI = confidence interval (95%); for H2, a
#>   negligible() prediction, it is the 90% equivalence interval the concordance
#>   is judged on.
#> 
#> Flagged
#>   - Review: `Persistence <-> SocialDesirability`, estimated in the model as
#>     given, is fixed to zero in the fitted model: with the hypothesized paths,
#>     `Persistence` is an outcome, and lavaan does not covary an outcome's
#>     residual with a variable that does not predict it.
#>   - Review: `Performance <-> SocialDesirability` is fixed to zero in the
#>     fitted model: the hypotheses bring `Performance` into the model, and
#>     neither they nor lavaan's defaults relate the two.
#> 
#> Theory concordance, uncertainty, measurement quality, and replication are
#> distinct evidence streams. Statistical significance alone is not a validity
#> verdict.
#> 
#> See summary(x) for the fit and the flagged evidence in full and
#> nomo_table(x, "hypotheses") for every column.
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

# A composite corrected for its unreliability: the Persistence mean, with
# omega from its own measurement model.
dat <- nomo_demo_network
dat$persistence <- rowMeans(dat[c("pe1", "pe2", "pe3", "pe4")])
rel <- nomo_reliability(nomo_cfa("P =~ pe1 + pe2 + pe3 + pe4", dat))
net_si <- nomo_network(
  "Agency =~ ag1 + ag2 + ag3 + ag4",
  data = dat,
  hypotheses = nomo_hypotheses("Agency -> persistence" = positive()),
  single_indicators = list(persistence = nomo_single_indicator(rel))
)
nomo_table(net_si, "single_indicators")
#> # A tibble: 1 × 9
#>   variable    indicator      reliability    se coefficient source     n variance
#>   <chr>       <chr>                <dbl> <dbl> <chr>       <chr>  <int>    <dbl>
#> 1 persistence persistence_si       0.816    NA omega       this …   800    0.645
#> # ℹ 1 more variable: error_variance <dbl>
# }
```
