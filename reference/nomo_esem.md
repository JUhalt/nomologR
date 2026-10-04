# Exploratory structural equation modeling beside its CFA

`nomo_esem()` fits a measurement model as exploratory structural
equation modeling (ESEM), in which every item may load on every factor,
and fits the same factors as an independent-clusters CFA, in which each
item loads on one factor only. It compares their fit, their factor
correlations, and the cross-loadings the CFA fixes at zero.

## Usage

``` r
nomo_esem(
  model,
  data,
  rotation = c("target", "geomin"),
  ordered = NULL,
  estimator = NULL,
  missing = NULL,
  guidance = nomo_defaults()
)
```

## Arguments

- model:

  A measurement model in which each indicator loads on one factor: a
  lavaan model string or a
  [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md)
  object. It defines the factors, the items, and the target.

- data:

  A data frame with the indicators.

- rotation:

  `"target"` (default) or `"geomin"`.

- ordered:

  Optional character vector of ordered indicators.

- estimator, missing:

  Optional lavaan `estimator` and `missing` options.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md);
  its `efa_loading_reference` and `efa_crossloading_reference` are used,
  and they are checked before any model is fitted.

## Value

A `nomo_esem` object. The fields to read are:

- `loadings`: each item's standardized loading on each factor in the
  ESEM, whether it is the item's main loading or a cross-loading, its
  standard error and p value, and the CFA's loading for main loadings.

- `factor_correlations`: each pair of factors' correlation (`factor1`,
  `factor2`) in the ESEM and the CFA, and their difference.

- `models`: both models' chi-square, degrees of freedom, p value, CFI,
  TLI, RMSEA, SRMR, AIC, and BIC. With a robust estimator, the
  chi-square is scaled and the indices are robust, as in
  [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md).

- `comparisons`: the chi-square difference test of the CFA against the
  ESEM, with `chisq_diff`, `df_diff`, and `p_value`. It is the
  likelihood-ratio test under ML, the scaled difference test (Satorra &
  Bentler, 2001) under a robust ML estimator, and the scaled-and-shifted
  test (Satorra, 2000) under WLSMV, as
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
  chooses.

- `n` and `data_n`: the cases the models used and the rows of `data`.

- `fits`: the fitted `lavaan` models, named `ESEM` and `CFA`.

- `engine_warnings`: the warnings `lavaan` raised, as a list with
  elements `ESEM`, `CFA`, and `comparison` (the difference test).

- `decision_log`.

Other fields record the call and the settings used. They may change
between releases and are not part of the stable interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

[`print()`](https://rdrr.io/r/base/print.html) shows both models' fit,
the difference test, the factor correlations in each model, and the
flags. [`summary()`](https://rdrr.io/r/base/summary.html) adds the ESEM
loadings beside the CFA's and each flag's recommendation.

## Details

**Why ESEM.** A CFA fixes every cross-loading at zero. When items in
fact have small cross-loadings, as items of related constructs usually
do, the constraint pushes that shared variance into the factor
correlations, which are then inflated, and into misfit (Asparouhov &
Muthén, 2009). ESEM estimates the cross-loadings instead, within a
structural equation model, so fit, standard errors, and further
structure remain available (Marsh, Morin, Parker, & Kaur, 2014).

**Rotation.** With an a priori structure, Marsh et al. (2014) recommend
target rotation: each item's loading on its intended factor is free, and
its cross-loadings are targeted towards zero without being fixed there.
This is Browne's (2001) partially specified target rotation, `"target"`
here, built from `model`. Geomin rotation (`"geomin"`), which uses no
target, is available for a more exploratory reading. The solution
depends on the rotation, as any exploratory factor solution does.

**Reading the comparison.** Marsh et al. (2014) suggest preferring ESEM
when it fits better and its factor correlations are lower, which shows
that the CFA's zero cross-loadings distort the structure; otherwise the
CFA is the more parsimonious account. Fit is compared on TLI and RMSEA,
which penalize the ESEM's extra parameters, so the CFA can fit better on
them. The CFA is nested in the ESEM, and their chi-square difference
test is reported too, but in large samples it rejects trivial misfit.
The decision log also flags cross-loadings at or above the guidance's
cross-loading reference and main loadings below its loading reference.

**Admissibility.** Both models must converge; otherwise the function
stops and names the model that did not. The warnings `lavaan` raises are
kept, and a negative variance, a standardized loading above 1, or a
latent correlation above 1 in either model is recorded as a concern, as
[`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
records it. Cases `lavaan` did not use, such as incomplete cases without
`missing = "fiml"`, are reported.

## References

Asparouhov, T., & Muthén, B. (2009). Exploratory structural equation
modeling. *Structural Equation Modeling, 16*(3), 397-438.
[doi:10.1080/10705510903008204](https://doi.org/10.1080/10705510903008204)

Browne, M. W. (2001). An overview of analytic rotation in exploratory
factor analysis. *Multivariate Behavioral Research, 36*(1), 111-150.
[doi:10.1207/S15327906MBR3601_05](https://doi.org/10.1207/S15327906MBR3601_05)

Marsh, H. W., Morin, A. J. S., Parker, P. D., & Kaur, G. (2014).
Exploratory structural equation modeling: An integration of the best
features of exploratory and confirmatory factor analysis. *Annual Review
of Clinical Psychology, 10*, 85-110.
[doi:10.1146/annurev-clinpsy-032813-153700](https://doi.org/10.1146/annurev-clinpsy-032813-153700)

Satorra, A. (2000). Scaled and adjusted restricted tests in multi-sample
analysis of moment structures. In R. D. H. Heijmans, D. S. G. Pollock, &
A. Satorra (Eds.), *Innovations in multivariate statistical analysis*
(pp. 233-247). Springer.
[doi:10.1007/978-1-4615-4603-0_17](https://doi.org/10.1007/978-1-4615-4603-0_17)

Satorra, A., & Bentler, P. M. (2001). A scaled difference chi-square
test statistic for moment structure analysis. *Psychometrika, 66*(4),
507-514. [doi:10.1007/BF02296192](https://doi.org/10.1007/BF02296192)

## Examples

``` r
# \donttest{
model <- nomo_model(list(
  Agency = paste0("ag", 1:4),
  Persistence = paste0("pe", 1:4)
))
es <- nomo_esem(model, nomo_demo_network)
es
#> <nomo_esem> ESEM beside its CFA
#> Asparouhov and Muthén (2009); Marsh et al. (2014).
#> Rotation: target | Cases: 800 | Factors: 2
#> 
#> Fit
#>   Model  Chi-square  df     p    CFI    TLI  RMSEA   SRMR
#>   ESEM        14.93  13  .312   .999  0.998  0.014  0.010
#>   CFA         18.52  19  .488  1.000  1.000  0.000  0.015
#>   CFA vs. ESEM: Delta chi-square(6) = 3.58, p = .733.
#> 
#> Factor correlations
#>   Factors                  ESEM  CFA  Difference
#>   Agency with Persistence   .45  .46         .00
#> 
#> ESEM = exploratory structural equation modeling; CFA = confirmatory factor
#> analysis; CFI = comparative fit index; TLI = Tucker-Lewis index; RMSEA = root
#> mean square error of approximation; SRMR = standardized root mean square
#> residual; df = degrees of freedom.
#> 
#> Cross-loadings are estimated, not fixed at zero. The comparison is evidence
#> for choosing a model, not a verdict.
#> 
#> See summary(x) for the loadings and each flag's recommendation and
#> nomo_table(x, "models") for every fit index.
nomo_table(es, "factor_correlations")
#> # A tibble: 1 × 5
#>   factor1 factor2      esem   cfa difference
#>   <chr>   <chr>       <dbl> <dbl>      <dbl>
#> 1 Agency  Persistence 0.455 0.457   -0.00281
# }
```
