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

Historical context:

Cronbach, L. J. (1951). Coefficient alpha and the internal structure of
tests. *Psychometrika, 16*(3), 297-334.
[doi:10.1007/BF02310555](https://doi.org/10.1007/BF02310555)

Contemporary model-based reliability:

Bell, S. M., Chalmers, R. P., & Flora, D. B. (2024). The impact of
measurement model misspecification on coefficient omega estimates of
composite reliability. *Educational and Psychological Measurement,
84*(1), 5-39.
[doi:10.1177/00131644231155804](https://doi.org/10.1177/00131644231155804)

Dunn, T. J., Baguley, T., & Brunsden, V. (2014). From alpha to omega: A
practical solution to the pervasive problem of internal consistency
estimation. *British Journal of Psychology, 105*(3), 399-412.
[doi:10.1111/bjop.12046](https://doi.org/10.1111/bjop.12046)

Flora, D. B. (2020). Your coefficient alpha is probably wrong, but which
coefficient omega is right? A tutorial on using R to obtain better
reliability estimates. *Advances in Methods and Practices in
Psychological Science, 3*(4), 484-501.
[doi:10.1177/2515245920951747](https://doi.org/10.1177/2515245920951747)

Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using
structural equation modeling: An alternative to coefficient alpha.
*Psychometrika, 74*(1), 155-167.
[doi:10.1007/s11336-008-9099-3](https://doi.org/10.1007/s11336-008-9099-3)

Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for
population reliability coefficients: Evaluation of methods,
recommendations, and software for composite measures. *Psychological
Methods, 21*(1), 69-92.
[doi:10.1037/a0040086](https://doi.org/10.1037/a0040086)

McNeish, D. (2018). Thanks coefficient alpha, we'll take it from here.
*Psychological Methods, 23*(3), 412-433.
[doi:10.1037/met0000144](https://doi.org/10.1037/met0000144)

Sijtsma, K. (2009). On the use, the misuse, and the very limited
usefulness of Cronbach's alpha. *Psychometrika, 74*(1), 107-120.
[doi:10.1007/s11336-008-9101-0](https://doi.org/10.1007/s11336-008-9101-0)

## Examples

``` r
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
rel <- nomo_reliability(cfa)
summary(rel)
#> nomologR reliability evidence
#> # A tibble: 3 × 7
#>   construct block   indicator_type omega alpha omega_scale         signal
#>   <chr>     <chr>   <chr>          <chr> <chr> <chr>               <chr> 
#> 1 speed     overall continuous     0.686 0.688 observed_continuous review
#> 2 textual   overall continuous     0.885 0.883 observed_continuous info  
#> 3 visual    overall continuous     0.612 0.626 observed_continuous review
#> 
#> Sampling uncertainty was not bootstrapped. For report-ready intervals, rerun with `ci = "bootstrap"`.
#> 
#> Measurement-model context requires review: reliability is conditional on the fitted CFA.
#> 
#> Interpretation rule: omega is primary for the congeneric CFA workflow; alpha is secondary and assumption-dependent. Reliability contributes score-precision evidence, not construct validity.
rel$decision_log
#> # A tibble: 9 × 10
#>   stage       object metric  value reference severity observation recommendation
#>   <chr>       <chr>  <chr>   <dbl> <chr>     <chr>    <chr>       <chr>         
#> 1 reliability measu… coeff… NA     Dunn et … info     Model-base… Interpret rel…
#> 2 reliability measu… model… NA     Bell, Ch… review   At least o… Investigate t…
#> 3 reliability speed… alpha… NA     Dunn et … info     Coefficien… Report alpha …
#> 4 reliability visual omega   0.612 configur… review   The coeffi… Inspect score…
#> 5 reliability textu… omega   0.885 configur… info     The coeffi… Carry this re…
#> 6 reliability speed  omega   0.686 configur… review   The coeffi… Inspect score…
#> 7 reliability visual alpha   0.626 configur… review   The coeffi… Inspect score…
#> 8 reliability textu… alpha   0.883 configur… info     The coeffi… Carry this re…
#> 9 reliability speed  alpha   0.688 configur… review   The coeffi… Inspect score…
#> # ℹ 2 more variables: decision <chr>, rationale <chr>

# \donttest{
# Bootstrap intervals refit the same CFA repeatedly; use more resamples
# (for example 1000) for final reporting.
rel_ci <- nomo_reliability(cfa, ci = "bootstrap", ci_boot = 100, ci_seed = 2026)
summary(rel_ci)
#> nomologR reliability evidence
#> # A tibble: 3 × 7
#>   construct block   indicator_type omega                alpha omega_scale signal
#>   <chr>     <chr>   <chr>          <chr>                <chr> <chr>       <chr> 
#> 1 speed     overall continuous     0.686 [0.585, 0.752] 0.68… observed_c… review
#> 2 textual   overall continuous     0.885 [0.859, 0.900] 0.88… observed_c… info  
#> 3 visual    overall continuous     0.612 [0.542, 0.686] 0.62… observed_c… review
#> Bracketed values are bootstrap confidence intervals.
#> 
#> Measurement-model context requires review: reliability is conditional on the fitted CFA.
#> 
#> Interpretation rule: omega is primary for the congeneric CFA workflow; alpha is secondary and assumption-dependent. Reliability contributes score-precision evidence, not construct validity.
# }
```
