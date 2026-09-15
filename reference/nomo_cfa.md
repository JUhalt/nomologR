# Guided confirmatory factor analysis

`nomo_cfa()` fits a researcher-specified confirmatory factor model with
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and adds a
transparent guidance layer around estimator choice, convergence,
standardized loadings, global fit, local residuals, Heywood diagnostics,
and modification indices.

## Usage

``` r
nomo_cfa(
  model,
  data,
  ordered = NULL,
  estimator = NULL,
  missing = NULL,
  std.lv = FALSE,
  control = NULL,
  modification_indices = TRUE,
  mi_top = 10L,
  guidance = nomo_defaults()
)
```

## Arguments

- model:

  A researcher-specified `lavaan` measurement-model syntax string or an
  object created by
  [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md).

- data:

  A data frame containing the model indicators.

- ordered:

  Optional character vector naming binary/ordinal indicators. When
  supplied and `estimator = NULL`, `nomo_cfa()` explicitly requests
  `WLSMV`.

- estimator:

  Optional `lavaan` estimator. For continuous indicators, leaving this
  as `NULL` preserves
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html)'s default.
  Robust ML variants such as `"MLR"` remain explicit researcher choices.

- missing:

  Optional `lavaan` missing-data option such as `"fiml"` for continuous
  ML models or `"pairwise"` where supported.

- std.lv:

  Logical. Passed directly to
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- control:

  Optional named list passed to lavaan's optimizer through
  `lavaan::cfa(control = ...)`. This is an advanced
  troubleshooting/reproducibility option; non-default optimizer controls
  are recorded in the decision log.

- modification_indices:

  Logical; if `TRUE`, compute modification indices as quarantined
  diagnostics. They never trigger automatic respecification.

- mi_top:

  Number of largest modification indices to retain in the compact
  `$top_modification_indices` view. The full table remains available in
  `$modification_indices`.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

A `nomo_cfa` object containing the unchanged `lavaan` fit plus
standardized loadings, factor correlations, fit evidence, residual
diagnostics, Heywood checks, modification indices, engine warnings, and
a decision log.

## Details

The fitted `lavaan` object is retained unchanged in `$fit`. `nomologR`
does not free parameters, add residual covariances, delete indicators,
or refit a different model in response to fit statistics or modification
indices.

## References

Historical foundations:

Bentler, P. M., & Bonett, D. G. (1980). Significance tests and goodness
of fit in the analysis of covariance structures. *Psychological
Bulletin, 88*(3), 588-606.
[doi:10.1037/0033-2909.88.3.588](https://doi.org/10.1037/0033-2909.88.3.588)

Jöreskog, K. G. (1969). A general approach to confirmatory maximum
likelihood factor analysis. *Psychometrika, 34*(2), 183-202.
[doi:10.1007/BF02289343](https://doi.org/10.1007/BF02289343)

Fit evaluation:

Bentler, P. M. (1990). Comparative fit indexes in structural models.
*Psychological Bulletin, 107*(2), 238-246.
[doi:10.1037/0033-2909.107.2.238](https://doi.org/10.1037/0033-2909.107.2.238)

Browne, M. W., & Cudeck, R. (1992). Alternative ways of assessing model
fit. *Sociological Methods & Research, 21*(2), 230-258.
[doi:10.1177/0049124192021002005](https://doi.org/10.1177/0049124192021002005)

Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
covariance structure analysis: Conventional criteria versus new
alternatives. *Structural Equation Modeling, 6*(1), 1-55.
[doi:10.1080/10705519909540118](https://doi.org/10.1080/10705519909540118)

Marsh, H. W., Hau, K.-T., & Wen, Z. (2004). In search of golden rules:
Comment on hypothesis-testing approaches to setting cutoff values for
fit indexes and dangers in overgeneralizing Hu and Bentler's (1999)
findings. *Structural Equation Modeling, 11*(3), 320-341.
[doi:10.1207/s15328007sem1103_2](https://doi.org/10.1207/s15328007sem1103_2)

Estimation, respecification, and improper solutions:

Flora, D. B., & Curran, P. J. (2004). An empirical evaluation of
alternative methods of estimation for confirmatory factor analysis with
ordinal data. *Psychological Methods, 9*(4), 466-491.
[doi:10.1037/1082-989X.9.4.466](https://doi.org/10.1037/1082-989X.9.4.466)

Kolenikov, S., & Bollen, K. A. (2012). Testing negative error variances:
Is a Heywood case a symptom of misspecification? *Sociological Methods &
Research, 41*(1), 124-167.
[doi:10.1177/0049124112442138](https://doi.org/10.1177/0049124112442138)

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490-504.
[doi:10.1037/0033-2909.111.3.490](https://doi.org/10.1037/0033-2909.111.3.490)

Rhemtulla, M., Brosseau-Liard, P. É., & Savalei, V. (2012). When can
categorical variables be treated as continuous? A comparison of robust
continuous and categorical SEM estimation methods under suboptimal
conditions. *Psychological Methods, 17*(3), 354-373.
[doi:10.1037/a0029315](https://doi.org/10.1037/a0029315)

Rosseel, Y. (2012). lavaan: An R package for structural equation
modeling. *Journal of Statistical Software, 48*(2), 1-36.
[doi:10.18637/jss.v048.i02](https://doi.org/10.18637/jss.v048.i02)

## Examples

``` r
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
out <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
summary(out)
#> nomologR confirmatory factor analysis
#> 301 cases used of 301 | Estimator: ML
#> Converged: yes | Ordered indicators: 0
#> 
#> Global fit evidence
#> # A tibble: 9 × 5
#>   metric           value variant        reference attention
#>   <chr>            <dbl> <chr>              <dbl> <chr>    
#> 1 chi_square     8.53e+1 chisq              NA    info     
#> 2 df             2.4 e+1 df                 NA    info     
#> 3 p_value        8.50e-9 pvalue             NA    info     
#> 4 CFI            9.31e-1 cfi                 0.95 review   
#> 5 TLI            8.96e-1 tli                 0.95 review   
#> 6 RMSEA          9.21e-2 rmsea               0.06 review   
#> 7 RMSEA_CI_lower 7.14e-2 rmsea.ci.lower     NA    info     
#> 8 RMSEA_CI_upper 1.14e-1 rmsea.ci.upper     NA    info     
#> 9 SRMR           6.52e-2 srmr                0.08 info     
#> 
#> Standardized loadings
#> # A tibble: 9 × 7
#>   factor  item  loading     se ci_lower ci_upper attention
#>   <chr>   <chr>   <dbl>  <dbl>    <dbl>    <dbl> <chr>    
#> 1 visual  x1      0.772 0.0550    0.664    0.880 KEEP     
#> 2 visual  x2      0.424 0.0596    0.307    0.540 REVIEW   
#> 3 visual  x3      0.581 0.0551    0.473    0.689 KEEP     
#> 4 textual x4      0.852 0.0225    0.807    0.896 KEEP     
#> 5 textual x5      0.855 0.0223    0.811    0.899 KEEP     
#> 6 textual x6      0.838 0.0234    0.792    0.884 KEEP     
#> 7 speed   x7      0.570 0.0532    0.465    0.674 KEEP     
#> 8 speed   x8      0.723 0.0505    0.624    0.822 KEEP     
#> 9 speed   x9      0.665 0.0511    0.565    0.765 KEEP     
#> 
#> Loading flags requiring inspection
#> # A tibble: 1 × 4
#>   factor item  attention
#>   <chr>  <chr> <chr>    
#> 1 visual x2    REVIEW   
#>   explanation                                                                   
#>   <chr>                                                                         
#> 1 Absolute standardized loading is below the configured teaching reference of 0…
#> 
#> Factor correlations
#> # A tibble: 3 × 5
#>   factor1 factor2 correlation ci_lower ci_upper
#>   <chr>   <chr>         <dbl>    <dbl>    <dbl>
#> 1 visual  textual       0.459    0.334    0.584
#> 2 visual  speed         0.471    0.328    0.613
#> 3 textual speed         0.283    0.148    0.418
#> 
#> No configured Heywood/improper-solution signal was detected.
#> 
#> Largest localized residual correlations
#> # A tibble: 5 × 4
#>   item1 item2 residual abs_residual
#>   <chr> <chr>    <dbl>        <dbl>
#> 1 x7    x2      -0.189        0.189
#> 2 x5    x3      -0.151        0.151
#> 3 x9    x1       0.149        0.149
#> 4 x9    x3       0.147        0.147
#> 5 x7    x1      -0.140        0.140
#> 
#> Top modification indices - diagnostic only
#> # A tibble: 5 × 6
#>   lhs     op    rhs      mi    epc sepc.all
#>   <chr>   <chr> <chr> <dbl>  <dbl>    <dbl>
#> 1 visual  =~    x9    36.4   0.577    0.515
#> 2 x7      ~~    x8    34.1   0.536    0.859
#> 3 visual  =~    x7    18.6  -0.422   -0.349
#> 4 x8      ~~    x9    14.9  -0.423   -0.805
#> 5 textual =~    x3     9.15 -0.272   -0.238
#> Modification indices do not authorize automatic respecification.
#> 
#> Interpretation rule: global fit, local strain, and parameter estimates are evidence to interpret together; no single cutoff establishes model validity.

# \donttest{
# Declared ordered indicators request WLSMV rather than ML
ordinal_model <- nomo_model(list(
  A = c("a1", "a2", "a3", "a4", "a5"),
  B = c("b1", "b2", "b3", "b4", "b5")
))
out_ord <- nomo_cfa(
  ordinal_model,
  data = nomo_demo_ordinal,
  ordered = names(nomo_demo_ordinal)
)
out_ord$fit_evidence
#> # A tibble: 9 × 7
#>   metric                value variant  reference direction attention explanation
#>   <chr>                 <dbl> <chr>        <dbl> <chr>     <chr>     <chr>      
#> 1 chi_square     93.5         chisq.s…     NA    informat… info      Descriptiv…
#> 2 df             34           df.scal…     NA    informat… info      Descriptiv…
#> 3 p_value         0.000000188 pvalue.…     NA    informat… info      Descriptiv…
#> 4 CFI             0.971       cfi.rob…      0.95 higher    info      At or abov…
#> 5 TLI             0.961       tli.rob…      0.95 higher    info      At or abov…
#> 6 RMSEA           0.0535      rmsea.r…      0.06 lower     info      At or belo…
#> 7 RMSEA_CI_lower  0.0348      rmsea.c…     NA    informat… info      Descriptiv…
#> 8 RMSEA_CI_upper  0.0719      rmsea.c…     NA    informat… info      Descriptiv…
#> 9 SRMR            0.0475      srmr          0.08 lower     info      At or belo…
# }
```
