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

  Missing-data handling for the correlations behind HTMT and HTMT2,
  passed to
  [`semTools::htmt()`](https://rdrr.io/pkg/semTools/man/htmt.html). The
  default, `"default"`, follows the fitted model, so HTMT rests on the
  same cases as the CFA: `"fiml"` when the CFA used full-information
  maximum likelihood, `"pairwise"` when it used pairwise deletion, and
  `"listwise"` otherwise. Other supported values are `"listwise"`,
  `"pairwise"`, `"direct"`, `"ml"`, and `"fiml"`. The handling used and
  the number of cases appear in `htmt_status` and the decision log.

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

A `nomo_validity` object. The fields to read are:

- `ave`: average variance extracted per construct, as convergent
  evidence. AVE is not defined for a factor with a cross-loaded
  indicator; its row has an `NA` estimate and the attention
  `"unavailable"`.

- `latent_correlations`: construct correlations with intervals.

- `htmt2` and `htmt`: heterotrait-monotrait ratios per pair.

- `discriminant`: each pair's separation evidence with its reference and
  interpretation.

- `htmt_status`: which HTMT variants were computed, and why any was not.
  For a computed variant, `missing` is the missing-data handling used
  and `n` the number of cases: the complete cases under listwise
  deletion, the smallest pairwise count under pairwise deletion, and
  every case with an indicator observed otherwise.

- `fornell_larcker_pairs`: the historical comparison, when requested.

- `standardized_loadings`, `references`, and `decision_log`.

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

## Details

The function deliberately avoids a binary declaration that a construct
is "valid" or "invalid". Numerical references trigger inspection and
explanation. HTMT-family results are considered together with theory,
indicator content, CFA fit, and latent-factor overlap. The
Fornell-Larcker comparison is available only by explicit request and is
labeled legacy/ supporting evidence because simulation work has shown
that it can miss discriminant-validity problems that HTMT detects.

## References

Historical context:

Campbell, D. T., & Fiske, D. W. (1959). Convergent and discriminant
validation by the multitrait-multimethod matrix. *Psychological
Bulletin, 56*(2), 81-105.
[doi:10.1037/h0046016](https://doi.org/10.1037/h0046016)

Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation
models with unobservable variables and measurement error. *Journal of
Marketing Research, 18*(1), 39-50.
[doi:10.2307/3151312](https://doi.org/10.2307/3151312)

Contemporary construct-separation evidence:

Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for
assessing discriminant validity in variance-based structural equation
modeling. *Journal of the Academy of Marketing Science, 43*(1), 115-135.
[doi:10.1007/s11747-014-0403-8](https://doi.org/10.1007/s11747-014-0403-8)

Roemer, E., Schuberth, F., & Henseler, J. (2021). HTMT2–An improved
criterion for assessing discriminant validity in structural equation
modeling. *Industrial Management & Data Systems, 121*(12), 2637-2650.
[doi:10.1108/IMDS-02-2021-0082](https://doi.org/10.1108/IMDS-02-2021-0082)

Rönkkö, M., & Cho, E. (2022). An updated guideline for assessing
discriminant validity. *Organizational Research Methods, 25*(1).
[doi:10.1177/1094428120968614](https://doi.org/10.1177/1094428120968614)

Voorhees, C. M., Brady, M. K., Calantone, R., & Ramirez, E. (2016).
Discriminant validity testing in marketing: An analysis, causes for
concern, and proposed remedies. *Journal of the Academy of Marketing
Science, 44*(1), 119-134.
[doi:10.1007/s11747-015-0455-4](https://doi.org/10.1007/s11747-015-0455-4)

## Examples

``` r
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
val <- nomo_validity(cfa)
summary(val)
#> <nomo_validity summary> Convergent and discriminant evidence
#> 
#> Convergent evidence by construct
#>   Construct    AVE  Min |loading|  Median |loading|  Loadings flagged  Flag
#>   visual     0.371          0.424             0.581                 1  review
#>   textual    0.721          0.838             0.852                 0
#>   speed      0.424          0.570             0.665                 0  review
#> 
#> Construct separation
#>   Construct 1  Construct 2  Latent r  95% CI          HTMT2   HTMT
#>   visual       textual         0.459  [0.334, 0.584]  0.384  0.424
#>   visual       speed           0.471  [0.328, 0.613]  0.387  0.467
#>   textual      speed           0.283  [0.148, 0.418]  0.280  0.290
#> 
#> Standardized loadings and AVE address convergent evidence; latent correlations
#> and HTMT-family statistics address construct separation. These are
#> complementary questions, not interchangeable pass/fail tests.
val$decision_log
#> # A tibble: 12 × 10
#>    stage    object  metric   value reference severity observation recommendation
#>    <chr>    <chr>   <chr>    <dbl> <chr>     <chr>    <chr>       <chr>         
#>  1 validity measur… evide…  NA     Fornell … info     "Convergen… Interpret num…
#>  2 validity x2      stand…   0.424 configur… review   "Absolute … Inspect item …
#>  3 validity visual  AVE      0.371 configur… review   "AVE is be… Inspect stand…
#>  4 validity textual AVE      0.721 configur… info     "AVE is at… Carry AVE for…
#>  5 validity speed   AVE      0.424 configur… review   "AVE is be… Inspect stand…
#>  6 validity textua… HTMT2    0.280 configur… info     "HTMT2 doe… Interpret thi…
#>  7 validity visual… HTMT2    0.387 configur… info     "HTMT2 doe… Interpret thi…
#>  8 validity visual… HTMT2    0.384 configur… info     "HTMT2 doe… Interpret thi…
#>  9 validity textua… HTMT     0.290 configur… info     "HTMT does… Interpret thi…
#> 10 validity visual… HTMT     0.467 configur… info     "HTMT does… Interpret thi…
#> 11 validity visual… HTMT     0.424 configur… info     "HTMT does… Interpret thi…
#> 12 validity measur… htmt_… 301     semTools… info     "HTMT-fami… No action nee…
#> # ℹ 2 more variables: decision <chr>, rationale <chr>
```
