# Convergent and discriminant construct-validity evidence

`nomo_validity()` summarizes several complementary forms of measurement
evidence from a fitted first-order CFA. Convergent evidence includes
standardized loadings and average variance extracted (AVE). Discriminant
evidence includes latent-factor correlations and, where appropriate,
HTMT2 as the primary heterotrait-monotrait summary, with the original
HTMT available as a sensitivity/teaching comparison.

## Usage

``` r
nomo_validity(
  fit,
  ave_obs_var = TRUE,
  htmt = c("both", "htmt2", "htmt", "none"),
  htmt_missing = "default",
  fornell_larcker = FALSE,
  guidance = nomo_defaults()
)
```

## Arguments

- fit:

  A fitted
  [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
  object or fitted `lavaan` CFA model.

- ave_obs_var:

  Logical passed to
  [`semTools::AVE()`](https://rdrr.io/pkg/semTools/man/AVE.html). `TRUE`
  (default) uses observed variances in the denominator; `FALSE` uses
  model-implied variances. For ordinal indicators, `semTools` calculates
  AVE using the polychoric correlation structure.

- htmt:

  Which heterotrait-monotrait summaries to request: `"both"` (default),
  `"htmt2"`, `"htmt"`, or `"none"`. HTMT2 is prioritized because its
  geometric-mean formulation is designed for congeneric indicators;
  original HTMT assumes tau-equivalence.

- htmt_missing:

  Missing-data option passed to
  [`semTools::htmt()`](https://rdrr.io/pkg/semTools/man/htmt.html). The
  default `"default"` delegates the unrestricted-correlation
  missing-data handling to
  [`lavaan::lavCor()`](https://rdrr.io/pkg/lavaan/man/lavCor.html)
  rather than silently imposing listwise deletion. Other supported
  values are `"listwise"`, `"pairwise"`, `"direct"`, `"ml"`, and
  `"fiml"`.

- fornell_larcker:

  Logical. If `TRUE`, also produce a legacy Fornell-Larcker matrix and
  pairwise comparison where available. It is not used as the primary
  discriminant-validity criterion.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).
  The AVE and HTMT references are review prompts rather than universal
  pass/fail cutoffs.

## Value

A `nomo_validity` object containing standardized loading evidence,
direct [`semTools::AVE()`](https://rdrr.io/pkg/semTools/man/AVE.html)
results, latent correlations, HTMT2/HTMT evidence, optional legacy
Fornell-Larcker information, model-fit context, research references, and
a structured decision log.

## Details

The function deliberately avoids a binary declaration that a construct
is "valid" or "invalid". Numerical references trigger inspection and
explanation. HTMT-family results are considered together with theory,
indicator content, CFA fit, and latent-factor overlap. The
Fornell-Larcker comparison is available only by explicit request and is
labeled legacy/ supporting evidence because simulation work has shown
that it can miss discriminant-validity problems that HTMT detects.

## References

Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation
models with unobservable variables and measurement error. *Journal of
Marketing Research, 18*, 39-50. doi:10.2307/3151312

Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for
assessing discriminant validity in variance-based structural equation
modeling. *Journal of the Academy of Marketing Science, 43*, 115-135.
doi:10.1007/s11747-014-0403-8

Roemer, E., Schuberth, F., & Henseler, J. (2021). HTMT2–An improved
criterion for assessing discriminant validity in structural equation
modeling. *Industrial Management & Data Systems, 121*, 2637-2650.
doi:10.1108/IMDS-02-2021-0082

Voorhees, C. M., Brady, M. K., Calantone, R., & Ramirez, E. (2016).
Discriminant validity testing in marketing: An analysis, causes for
concern, and proposed remedies. *Journal of the Academy of Marketing
Science, 44*, 119-134. doi:10.1007/s11747-015-0455-4

## Examples

``` r
if (FALSE) { # \dontrun{
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
val <- nomo_validity(cfa)
val$ave
val$htmt2
val$decision_log
} # }
```
