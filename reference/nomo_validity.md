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
  AVE from the polychoric correlation structure in the model's
  unstandardized metric, so a fit with lavaan's theta parameterization
  gives a different AVE from the delta fit of the same model; the
  decision log then gives the mean squared standardized loading, which
  is the same under either.

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

- `latent_correlations`: construct correlations with intervals, and
  their `reference` and `attention`. A correlation is flagged for review
  when it could exceed the HTMT-family reference: the upper limit of its
  interval (Rönkkö & Cho, 2022), or the estimate when there is no
  interval, is above it. A correlation beyond 1 in absolute value is a
  concern.

- `htmt2` and `htmt`: heterotrait-monotrait ratios per pair.

- `discriminant`: each pair's separation evidence with its reference and
  interpretation.

- `htmt_status`: which HTMT variants were computed, and why any was not.
  For a computed variant, `missing` is the missing-data handling used
  and `n` the number of cases: the complete cases under listwise
  deletion, the smallest pairwise count under pairwise deletion, and
  every case with an indicator observed otherwise. HTMT is not defined
  with one construct or for a construct with one indicator, which has no
  within-construct correlations; such pairs are left out, and the
  decision log records it as information rather than as missing
  evidence.

- `fornell_larcker_pairs`: the historical comparison, when requested.

- `standardized_loadings`, `references`, and `decision_log`.

`attention` in `standardized_loadings` is `"KEEP"`, `"REVIEW"`, or
`"STRONG REVIEW"`, as in
[`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md).
`attention` in `ave` and `discriminant`, and `severity` in
`decision_log`, are `"info"`, `"review"`, or `"concern"`. `attention` in
`fornell_larcker_pairs` is `"info"` or `"review"`, or `"unavailable"`
when the comparison could not be computed, as when an AVE is missing or
negative and so has no square root. See **Conventions in returned
tables** in
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md).

In the `"discriminant"` table of
[`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md),
`signal` is the HTMT-family flag (HTMT2, or HTMT when HTMT2 was not
computed). Where neither was computed, as with several groups, a
cross-loading, or `htmt = "none"`, it is the latent correlation's
`attention`, so the pair is still evaluated; a latent correlation beyond
1 is a concern either way. Beside an HTMT-family value, a latent
correlation flagged for review is recorded in the decision log as
information, and the summary names the pair.

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

[`print()`](https://rdrr.io/r/base/print.html) counts the convergent and
construct-separation flags and names each flagged construct or pair;
[`summary()`](https://rdrr.io/r/base/summary.html) adds the AVE and
loading summary for each construct, the latent correlation and
HTMT-family values for each pair, and the full reason for each flag.

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
discriminant validity. *Organizational Research Methods, 25*(1), 6-47.
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
#> Constructs: 3 | AVE reference: .50 | HTMT reference: 0.85
#> Convergent flags: 2 review, 0 concern (3 constructs)
#> Separation flags: none (3 pairs)
#> 
#> Convergent evidence by construct
#>   Construct  AVE  Min |loading|  Median |loading|  Loadings flagged  Flag
#>   visual     .37           0.42              0.58                 1  Review
#>   textual    .72           0.84              0.85                 0
#>   speed      .42           0.57              0.67                 0  Review
#> 
#> Construct separation
#>   Construct 1  Construct 2  Latent r  95% CI      HTMT2  HTMT
#>   visual       textual           .46  [.33, .58]   0.38  0.42
#>   visual       speed             .47  [.33, .61]   0.39  0.47
#>   textual      speed             .28  [.15, .42]   0.28  0.29
#> 
#> Flagged
#>   - x2 (Review): Standardized loading 0.42 is below the review reference 0.50
#>     in absolute value; inspect item content, precision, and model
#>     specification.
#>   - visual (Review): AVE (.37) is below the configured convergent-evidence
#>     reference (.50). Inspect standardized loadings, indicator-specific error,
#>     and content coverage; do not automatically delete items.
#>   - speed (Review): AVE (.42) is below the configured convergent-evidence
#>     reference (.50). Inspect standardized loadings, indicator-specific error,
#>     and content coverage; do not automatically delete items.
#> 
#> What these columns mean
#>   AVE -- Average variance extracted, the mean share of its indicators'
#>       variance a construct explains.
#>   |loading| -- Absolute standardized loading.
#>   Latent r -- Correlation between two constructs in the CFA.
#>   CI -- Confidence interval, as lavaan computes it.
#>   HTMT2 -- Heterotrait-monotrait ratio with geometric means (Roemer et al.,
#>       2021).
#>   HTMT -- Heterotrait-monotrait ratio (Henseler et al., 2015).
#> 
#> Standardized loadings and AVE address convergent evidence; latent correlations
#> and HTMT-family statistics address construct separation. These are
#> complementary questions, not interchangeable pass/fail tests.
#> 
#> See nomo_table(x, "discriminant") for every value and x$decision_log for the
#> reasoning behind each flag.
val$decision_log
#> # A tibble: 12 × 10
#>    stage    object  metric   value reference severity observation recommendation
#>    <chr>    <chr>   <chr>    <dbl> <chr>     <chr>    <chr>       <chr>         
#>  1 validity measur… evide…  NA     Fornell … info     "Convergen… Interpret num…
#>  2 validity x2      stand…   0.424 configur… review   "Standardi… Inspect item …
#>  3 validity visual  AVE      0.371 configur… review   "AVE (.37)… Inspect stand…
#>  4 validity textual AVE      0.721 configur… info     "AVE (.72)… Carry AVE for…
#>  5 validity speed   AVE      0.424 configur… review   "AVE (.42)… Inspect stand…
#>  6 validity textua… HTMT2    0.280 configur… info     "HTMT2 (0.… Interpret thi…
#>  7 validity visual… HTMT2    0.387 configur… info     "HTMT2 (0.… Interpret thi…
#>  8 validity visual… HTMT2    0.384 configur… info     "HTMT2 (0.… Interpret thi…
#>  9 validity textua… HTMT     0.290 configur… info     "HTMT (0.2… Interpret thi…
#> 10 validity visual… HTMT     0.467 configur… info     "HTMT (0.4… Interpret thi…
#> 11 validity visual… HTMT     0.424 configur… info     "HTMT (0.4… Interpret thi…
#> 12 validity measur… htmt_… 301     semTools… info     "HTMT-fami… No action nee…
#> # ℹ 2 more variables: decision <chr>, rationale <chr>
```
