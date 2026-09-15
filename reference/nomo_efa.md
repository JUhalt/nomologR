# Guided exploratory factor analysis

`nomo_efa()` estimates a common-factor exploratory factor analysis while
keeping the consequential analytical choices visible. Oblique rotation
is the default, item-level diagnostics are framed as prompts for
inspection, and no item is automatically deleted or model silently
refit.

## Usage

``` r
nomo_efa(
  data,
  items = NULL,
  factors,
  factor_count = NULL,
  rotation = "oblimin",
  fm = "minres",
  correlation = NULL,
  types = NULL,
  missing = NULL,
  smooth = NULL,
  guidance = nomo_defaults()
)
```

## Arguments

- data:

  A data frame containing candidate items.

- items:

  Optional character vector identifying item columns. If `factors` is a
  `nomo_factors` object and `items = NULL`, its item set is inherited.
  Otherwise all columns are used when `items = NULL`.

- factors:

  A positive integer number of factors to extract, or a `nomo_factors`
  object. For a `nomo_factors` object, the selected parallel-analysis
  factor count is used unless `factor_count` is supplied.

- factor_count:

  Optional positive integer. This may be supplied only when `factors` is
  a `nomo_factors` object. It preserves the M2 item/modeling/
  correlation/missingness context while recording a researcher-selected
  EFA factor count instead of pretending the primary parallel-analysis
  suggestion was adopted.

- rotation:

  Rotation passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html). The default is
  `"oblimin"`. Orthogonal rotations are allowed but are recorded as a
  researcher choice.

- fm:

  Common-factor extraction method passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html). The default is
  `"minres"`. Supported values are `"minres"`, `"uls"`, `"ols"`,
  `"wls"`, `"gls"`, `"pa"`, `"ml"`, `"minchi"`, `"minrank"`, `"alpha"`,
  and `"old.min"`.

- correlation:

  Optional correlation strategy: `"auto"`, `"pearson"`, `"polychoric"`,
  `"tetrachoric"`, or `"mixed"`. If `NULL`, the value is inherited from
  a supplied `nomo_factors` object when possible; otherwise `"auto"` is
  used.

- types:

  Optional named character vector declaring selected items as
  `"continuous"`, `"ordinal"`, or `"binary"`. If `NULL`, modeling types
  are inherited from a supplied `nomo_factors` object when possible;
  otherwise they are inferred using the same conservative rules as
  [`nomo_factors()`](https://juhalt.github.io/nomologR/reference/nomo_factors.md).

- missing:

  Optional missing-data strategy: `"pairwise"` or `"complete"`. If
  `NULL`, the value is inherited from a supplied `nomo_factors` object
  when possible; otherwise `"pairwise"` is used.

- smooth:

  Optional logical. If `NULL` (default), smoothing is inherited from a
  supplied `nomo_factors` object when that object explicitly used
  smoothing; otherwise `FALSE`. If `FALSE`, a non-positive-definite
  correlation matrix stops the analysis. If `TRUE`,
  [`psych::cor.smooth()`](https://rdrr.io/pkg/psych/man/cor.smooth.html)
  is used explicitly and the intervention is recorded.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

An object of class `nomo_efa` containing the fitted EFA, tidy pattern
and structure matrices, factor correlations, item diagnostics,
communalities/uniquenesses, residual diagnostics, adequacy information,
and a decision log.

## Details

The factor count may be supplied directly as a positive integer or by
passing a `nomo_factors` object. When a `nomo_factors` object is
supplied, its primary parallel-analysis suggestion is used as the
requested EFA factor count and, unless overridden, its item set,
modeling types, correlation model, and missing-data strategy are carried
forward.

## References

Browne, M. W. (2001). An overview of analytic rotation in exploratory
factor analysis. *Multivariate Behavioral Research, 36*(1), 111-150.
[doi:10.1207/S15327906MBR3601_05](https://doi.org/10.1207/S15327906MBR3601_05)

Conway, J. M., & Huffcutt, A. I. (2003). A review and evaluation of
exploratory factor analysis practices in organizational research.
*Organizational Research Methods, 6*(2), 147-168.
[doi:10.1177/1094428103251541](https://doi.org/10.1177/1094428103251541)

Costello, A. B., & Osborne, J. W. (2005). Best practices in exploratory
factor analysis: Four recommendations for getting the most from your
analysis. *Practical Assessment, Research, and Evaluation, 10*, Article
7. [doi:10.7275/jyj1-4868](https://doi.org/10.7275/jyj1-4868)

Fabrigar, L. R., Wegener, D. T., MacCallum, R. C., & Strahan, E. J.
(1999). Evaluating the use of exploratory factor analysis in
psychological research. *Psychological Methods, 4*(3), 272-299.
[doi:10.1037/1082-989X.4.3.272](https://doi.org/10.1037/1082-989X.4.3.272)

Kaiser, H. F. (1958). The varimax criterion for analytic rotation in
factor analysis. *Psychometrika, 23*(3), 187-200.
[doi:10.1007/BF02289233](https://doi.org/10.1007/BF02289233)

Watkins, M. W. (2018). Exploratory factor analysis: A guide to best
practice. *Journal of Black Psychology, 44*(3), 219-246.
[doi:10.1177/0095798418771807](https://doi.org/10.1177/0095798418771807)

## Examples

``` r
efa <- nomo_efa(nomo_demo_continuous, factors = 2)
efa
#> <nomo_efa>
#> Cases: 500 | Items: 10 | Factors: 2 (researcher specified)
#> Correlation: pearson | Extraction: minres | Rotation: oblimin
#> Off-diagonal RMSR: 0.018
#> Item review: 7 KEEP | 2 REVIEW | 1 STRONG REVIEW
#> No items were automatically deleted or refit.
efa$item_summary[, c("item", "primary_loading", "secondary_loading", "attention")]
#> # A tibble: 10 × 4
#>    item  primary_loading secondary_loading attention    
#>    <chr>           <dbl>             <dbl> <chr>        
#>  1 a1              0.808          -0.0444  KEEP         
#>  2 a2              0.709           0.0643  KEEP         
#>  3 a3              0.667           0.0153  KEEP         
#>  4 a4              0.770          -0.0377  KEEP         
#>  5 a5              0.414           0.335   REVIEW       
#>  6 b1              0.783           0.0141  KEEP         
#>  7 b2              0.693          -0.0200  KEEP         
#>  8 b3              0.783          -0.00673 KEEP         
#>  9 b4              0.637          -0.0119  REVIEW       
#> 10 b5              0.332           0.0286  STRONG REVIEW

# Carry retention evidence and its modeling decisions into the EFA
fac <- nomo_factors(nomo_demo_continuous, n_iter = 20, seed = 2026)
efa_from_evidence <- nomo_efa(nomo_demo_continuous, factors = fac)
summary(efa_from_evidence)
#> nomologR exploratory factor analysis
#> 500 cases | 10 items | 2 factors (from nomo_factors())
#> Correlation: pearson | Extraction: minres | Rotation: oblimin
#> Supporting adequacy: KMO = 0.874
#> 
#> Item-level structural review
#> # A tibble: 10 × 7
#>    item  primary_factor primary_loading secondary_factor secondary_loading
#>    <chr> <chr>                    <dbl> <chr>                        <dbl>
#>  1 a1    F1                       0.808 F2                        -0.0444 
#>  2 a2    F1                       0.709 F2                         0.0643 
#>  3 a3    F1                       0.667 F2                         0.0153 
#>  4 a4    F1                       0.770 F2                        -0.0377 
#>  5 a5    F1                       0.414 F2                         0.335  
#>  6 b1    F2                       0.783 F1                         0.0141 
#>  7 b2    F2                       0.693 F1                        -0.0200 
#>  8 b3    F2                       0.783 F1                        -0.00673
#>  9 b4    F2                       0.637 F1                        -0.0119 
#> 10 b5    F2                       0.332 F1                         0.0286 
#> # ℹ 2 more variables: communality <dbl>, attention <chr>
#> 
#> Items requiring review
#> # A tibble: 3 × 3
#>   item  attention    
#>   <chr> <chr>        
#> 1 a5    REVIEW       
#> 2 b4    REVIEW       
#> 3 b5    STRONG REVIEW
#>   explanation                                                                   
#>   <chr>                                                                         
#> 1 secondary loading |0.34| meets/exceeds the 0.30 cross-loading reference       
#> 2 communality 0.40 is below the 0.40 teaching reference                         
#> 3 primary loading |0.33| is below the 0.40 teaching reference; communality 0.12…
#> 
#> Factor correlations
#>       F1    F2
#> F1 1.000 0.439
#> F2 0.439 1.000
#> 
#> Off-diagonal RMSR: 0.018
#> 
#> Largest localized residuals
#> # A tibble: 5 × 4
#>   item1 item2 residual abs_residual
#>   <chr> <chr>    <dbl>        <dbl>
#> 1 b4    b5      0.0425       0.0425
#> 2 a5    b5     -0.0388       0.0388
#> 3 a4    b5      0.0380       0.0380
#> 4 a2    a5      0.0295       0.0295
#> 5 b2    b5     -0.0277       0.0277
#> 
#> Interpretation rule: numerical references trigger inspection, not automatic deletion or hidden refitting.
```
