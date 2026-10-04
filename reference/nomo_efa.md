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
  a `nomo_factors` object. It keeps that result's items, modeling types,
  correlations, and missing-data handling while recording a
  researcher-selected factor count, rather than implying that the
  parallel-analysis suggestion was adopted. It is required when parallel
  analysis suggested 0 factors, since there is then no count to adopt.

- rotation:

  Rotation passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html). The default is
  `"oblimin"`. The oblique rotations are `"oblimin"`, `"quartimin"`,
  `"simplimax"`, `"geominQ"`, `"bentlerQ"`, `"promax"`, `"Promax"`, and
  `"cluster"`; the orthogonal ones are `"varimax"`, `"Varimax"`,
  `"quartimax"`, `"equamax"`, `"varimin"`, `"geominT"`, and
  `"bentlerT"`; `"none"` keeps the extracted solution without rotation.
  Orthogonal rotations and `"none"` are allowed but are recorded as a
  researcher choice for review. Target rotations are not available,
  because `nomo_efa()` does not pass a target matrix. `"bifactor"` and
  `"biquartimin"` are not available either, because
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) runs them
  through `GPArotation` only when `psych` is attached and only for three
  or more factors; a general factor is tested with
  `nomo_model(structure = "bifactor")` and
  [`nomo_hierarchical()`](https://juhalt.github.io/nomologR/reference/nomo_hierarchical.md).
  A one-factor solution is not rotated.

- fm:

  Common-factor extraction method passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html). The default is
  `"minres"`. Supported values are `"minres"`, `"uls"`, `"ols"`,
  `"wls"`, `"gls"`, `"pa"`, `"ml"`, `"minchi"`, `"alpha"`, and
  `"old.min"`. `"alpha"` needs at least two factors, because
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) cannot fit a
  one-factor alpha solution. For `"minchi"`, the number of cases
  observed for each item pair is passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html), which weights
  the residuals by it. `"minrank"` is not supported, because
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) needs the
  `Rcsdp` package for it.

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
  A list holding only the settings to change is completed from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

An object of class `nomo_efa`. The fields to read are:

- `items`, `n_factors`, and `factor_source`, which records whether the
  count was the researcher's or taken from a `nomo_factors` result.

- `n_cases`: the number of rows analyzed (every row under
  `missing = "pairwise"`, including rows with no item data; the complete
  rows under `missing = "complete"`).

- `min_pairwise_n`: the smallest number of cases observed jointly on any
  item pair, the effective sample size under pairwise deletion. It
  equals `n_cases` when no item value is missing or
  `missing = "complete"`.

- `correlation_method` and `correlation_matrix`: the correlations
  analyzed.

- `pattern_matrix`, `structure_matrix`, and `factor_correlations`.

- `communalities`, `uniquenesses`, and `complexity`.

- `item_summary`: one row per item, with its primary and secondary
  loadings, communality, flags, and the explanation of any flag.

- `residual_matrix`, `residual_pairs`, and `rmsr`: local misfit.

- `sample_adequacy`: `n_cases`, `min_pairwise_n`, `n_items`,
  `cases_per_item` (`min_pairwise_n` divided by `n_items`), `kmo`, and
  `bartlett`.

- `decision_log`.

The flag in `item_summary`, `attention`, is `"KEEP"`, `"REVIEW"`, or
`"STRONG REVIEW"`; `severity` in `decision_log` is `"info"`, `"review"`,
or `"concern"`. See **Conventions in returned tables** in
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md).

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

## Details

The factor count may be supplied directly as a positive integer or by
passing a `nomo_factors` object. When a `nomo_factors` object is
supplied, its primary parallel-analysis suggestion is used as the
requested EFA factor count and, unless overridden, its item set,
modeling types, correlation model, and missing-data strategy are carried
forward.

The decision log also records what can make a solution improper or hard
to interpret, as concerns or prompts for review: a communality of .995
or more, which leaves a unique variance at or near 0 (a Heywood case;
psych's extractions often stop just short of 1), a model with more
parameters than the correlations can identify (negative degrees of
freedom) or exactly as many (zero), an extraction that did not converge,
and any warning or message from
[`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html).

[`print()`](https://rdrr.io/r/base/print.html) shows the settings, the
residual misfit, the item flags, and any problem with the solution;
[`summary()`](https://rdrr.io/r/base/summary.html) adds the item
loadings and communalities with an explanation of every flag, the factor
correlations, and the largest residual correlations.

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
#> <nomo_efa> Exploratory factor analysis
#> Cases: 500 (minimum pairwise N: 473) | Items: 10
#> Factors: 2 (researcher specified)
#> Correlation: Pearson | Extraction: minres | Rotation: oblimin (oblique)
#> RMSR: 0.018 | Item flags: 2 review, 1 concern
#> RMSR = root mean square of the off-diagonal residual correlations. No item was
#> deleted and no model was refit automatically.
#> 
#> See summary(x) for the loadings and the reason for each flag.
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
#> <nomo_efa summary> Exploratory factor analysis
#> Cases: 500 (minimum pairwise N: 473) | Items: 10
#> Factors: 2 (from nomo_factors())
#> Correlation: Pearson | Extraction: minres | Rotation: oblimin (oblique)
#> KMO: .87
#> 
#> Item structure
#>   Item  Factor  Loading  Next factor  Loading  Communality  Flag
#>   a1    F1         0.81  F2             -0.04          .62
#>   a2    F1         0.71  F2              0.06          .55
#>   a3    F1         0.67  F2              0.02          .45
#>   a4    F1         0.77  F2             -0.04          .57
#>   a5    F1         0.41  F2              0.34          .41  Review
#>   b1    F2         0.78  F1              0.01          .62
#>   b2    F2         0.69  F1             -0.02          .47
#>   b3    F2         0.78  F1             -0.01          .61
#>   b4    F2         0.64  F1             -0.01        .3997  Review
#>   b5    F2         0.33  F1              0.03          .12  Concern
#> 
#> Flagged
#>   - b5 (Concern): The primary loading, 0.33 in absolute value, is below the
#>     0.40 teaching reference. The communality, .12, is below the .40 teaching
#>     reference.
#>   - a5 (Review): The secondary loading, 0.34 in absolute value, is at or above
#>     the 0.30 cross-loading reference.
#>   - b4 (Review): The communality, .3997, is below the .40 teaching reference.
#> 
#> Factor correlations
#>   Factor 1  Factor 2    r
#>   F1        F2        .44
#> 
#> Largest residual correlations
#>   RMSR: 0.018
#>   Item 1  Item 2  Residual
#>   b4      b5          .042
#>   a5      b5         -.039
#>   a4      b5          .038
#>   a2      a5          .030
#>   b2      b5         -.028
#> 
#> Abbreviations
#>   KMO -- Kaiser-Meyer-Olkin measure of sampling adequacy.
#>   RMSR -- Root mean square of the off-diagonal residual correlations.
#> 
#> Numerical references trigger inspection, not automatic deletion or hidden
#> refitting.
#> 
#> See nomo_table(x, "pattern") for the full pattern matrix and
#> nomo_table(x, "decision_log") for every recorded decision.
```
