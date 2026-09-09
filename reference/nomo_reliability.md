# Model-based reliability evidence

`nomo_reliability()` estimates score reliability from a fitted
first-order CFA measurement model. The primary estimate is a model-based
omega-type composite reliability from
[`semTools::compRelSEM()`](https://rdrr.io/pkg/semTools/man/compRelSEM.html).
Coefficient alpha is available as a familiar secondary statistic and is
explicitly qualified by its stronger assumptions.

## Usage

``` r
nomo_reliability(
  fit,
  obs.var = TRUE,
  ordinal_scale = TRUE,
  include_alpha = TRUE,
  ci = c("none", "bootstrap"),
  ci_level = 0.95,
  ci_boot = 1000L,
  ci_seed = NULL,
  guidance = nomo_defaults()
)
```

## Arguments

- fit:

  A fitted
  [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
  object or a fitted `lavaan` CFA model.

- obs.var:

  Logical passed to
  [`semTools::compRelSEM()`](https://rdrr.io/pkg/semTools/man/compRelSEM.html).
  `TRUE` (default) uses observed covariances in the denominator; `FALSE`
  uses model-implied covariances.

- ordinal_scale:

  Logical passed as `ord.scale` to
  [`semTools::compRelSEM()`](https://rdrr.io/pkg/semTools/man/compRelSEM.html).
  For an all-ordered composite, `TRUE` (default) estimates reliability
  on the actual ordinal-score scale using the Green-Yang correction
  implemented by `semTools`; `FALSE` estimates the latent-response-scale
  coefficient.

- include_alpha:

  Logical. If `TRUE` (default), also return coefficient alpha as a
  secondary statistic where the requested estimand is defined. For
  ordered indicators with `ordinal_scale = TRUE`, observed-score alpha
  is deliberately reported as unavailable rather than silently switching
  to latent-response ("ordinal alpha") or numeric-score alpha. Alpha is
  not treated as the preferred reliability estimate for a general
  congeneric CFA.

- ci:

  Character. `"none"` (default) returns point estimates only.
  `"bootstrap"` adds nonparametric percentile confidence intervals by
  repeatedly refitting the same CFA with
  [`lavaan::bootstrapLavaan()`](https://rdrr.io/pkg/lavaan/man/bootstrap.html).

- ci_level:

  Confidence level for bootstrap intervals. Default is `0.95`.

- ci_boot:

  Number of ordinary bootstrap resamples when `ci = "bootstrap"`.
  Default is `1000`.

- ci_seed:

  Optional integer seed for reproducible bootstrap intervals.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).
  The configured reliability reference is a review prompt, not a
  pass/fail criterion.

## Value

A `nomo_reliability` object containing direct `semTools` results, tidy
reliability evidence, model-fit context, item-type context, literature
references, and a structured decision log.

## Details

The function is deliberately measurement-first: it refuses nonconverged
models, structural SEMs, higher-order models, and cross-loaded
first-order indicators in the v0.1 workflow. When global CFA strain or
an improper solution is present, reliability is still inspectable but
the decision log warns that model-based reliability can be distorted by
misspecification.

Average variance extracted (AVE) is not a reliability coefficient and is
intentionally handled by
[`nomo_validity()`](https://juhalt.github.io/nomologR/reference/nomo_validity.md)
instead.

## References

Dunn, T. J., Baguley, T., & Brunsden, V. (2014). From alpha to omega: a
practical solution to the pervasive problem of internal consistency
estimation. *British Journal of Psychology, 105*, 399-412.

Flora, D. B. (2020). Your coefficient alpha is probably wrong, but which
coefficient omega is right? A tutorial on using R to obtain better
reliability estimates. *Advances in Methods and Practices in
Psychological Science, 3*, 484-501.

Bell, S. M., Chalmers, R. P., & Flora, D. B. (2024). The impact of
measurement model misspecification on coefficient omega estimates of
composite reliability. *Educational and Psychological Measurement, 84*,
5-39.

Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using
structural equation modeling: an alternative to coefficient alpha.
*Psychometrika, 74*, 155-167.

Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for
population reliability coefficients: Evaluation of methods,
recommendations, and software for composite measures. *Psychological
Methods, 21*, 69-92.

## Examples

``` r
if (FALSE) { # \dontrun{
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
rel <- nomo_reliability(cfa)
rel$evidence
rel$decision_log
} # }
```
