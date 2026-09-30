# Evaluate evidence about the number of latent factors

`nomo_factors()` combines multiple pieces of factor-retention evidence
rather than treating any single rule as definitive. The primary
retention evidence is a common-factor parallel analysis, complemented by
Velicer's original and revised MAP criteria and, under the default
`criterion_set = "core"`, the empirical Kaiser criterion (EKC). Extended
criterion sets can add NEST, Hull (CAF), comparison data, and the legacy
Kaiser-Guttman rule when their assumptions are compatible with the
analyzed data. Scree information, Kaiser-Meyer-Olkin (KMO) sampling
adequacy, and Bartlett's test are returned as supporting diagnostics.

## Usage

``` r
nomo_factors(
  data,
  items = NULL,
  correlation = c("auto", "pearson", "polychoric", "tetrachoric", "mixed"),
  types = NULL,
  missing = c("pairwise", "complete"),
  criterion_set = c("core", "minimal", "extended", "all"),
  parallel_rule = c("percentile", "mean", "crawford"),
  n_iter = NULL,
  quantile = NULL,
  max_factors = NULL,
  seed = 1234L,
  fm = "minres",
  smooth = FALSE,
  guidance = nomo_defaults()
)
```

## Arguments

- data:

  A data frame containing candidate items.

- items:

  Optional character vector identifying item columns. If `NULL`, all
  columns are treated as candidate items.

- correlation:

  Correlation strategy: `"auto"`, `"pearson"`, `"polychoric"`,
  `"tetrachoric"`, or `"mixed"`.

- types:

  Optional named character vector overriding modeling types for selected
  items. Allowed values are `"continuous"`, `"ordinal"`, and `"binary"`.
  Explicit overrides are applied before default-type rejection, so
  researchers can intentionally model otherwise ambiguous storage (for
  example, an unordered factor whose levels already encode a substantive
  order). Overrides do not reorder, relabel, or recode the supplied
  data. For example, `c(item1 = "ordinal", item2 = "ordinal")`.

- missing:

  Missing-data handling for correlation estimation. `"pairwise"` uses
  pairwise-complete observations; `"complete"` restricts the analysis to
  cases complete on all selected items.

- criterion_set:

  Retention-criterion bundle. `"minimal"` uses parallel analysis plus
  original MAP; `"core"` (default) adds revised MAP and EKC;
  `"extended"` adds NEST and Hull where supported; `"all"` additionally
  requests comparison data and the legacy Kaiser-Guttman rule. Criteria
  whose assumptions are not compatible with the current data are
  explicitly marked as skipped rather than silently substituted.

- parallel_rule:

  Parallel-analysis decision rule: `"percentile"` (default), `"mean"`,
  or `"crawford"`. All three rules are computed from the same null
  simulations and retained in the result as sensitivity evidence.

- n_iter:

  Number of null-data iterations used for parallel analysis. If `NULL`,
  the value in `guidance$factor_parallel_iterations` is used.

- quantile:

  Quantile of null eigenvalues used as the parallel-analysis reference.
  If `NULL`, the value in `guidance$factor_parallel_quantile` is used.

- max_factors:

  Maximum number of factors/components evaluated for MAP. If `NULL`, up
  to 10 or `p - 1`, whichever is smaller, are evaluated.

- seed:

  Integer seed for the null-data simulation. The caller's random number
  state is restored before return.

- fm:

  Common-factor extraction method passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) when obtaining
  factor eigenvalues. The default is `"minres"`.

- smooth:

  Logical. If `FALSE` (default), a non-positive-definite observed
  correlation matrix blocks factor-retention analysis. If `TRUE`,
  smoothing is explicit, recorded, and performed with
  [`psych::cor.smooth()`](https://rdrr.io/pkg/psych/man/cor.smooth.html).

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

An object of class `nomo_factors`. The fields to read are:

- `items`, `n_cases`, and `n_items`.

- `item_types`: each item's screened and modeling type.

- `correlation_method` and `correlation_matrix`: the correlations
  analyzed.

- `kmo` and `bartlett`: sampling-adequacy evidence.

- `parallel`, `map`, and `scree`: each criterion's result.

- `criterion_status`: which criteria ran, and why any did not.

- `evidence`, `family_evidence`, and `family_concordance`: the suggested
  factor counts by method and by family of related methods.

- `plausible_factors` and `recommendation`: the synthesis, which is
  evidence for a researcher's choice, not a choice.

- `decision_log`.

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

## Details

Correlation choice is explicit and visible. With `correlation = "auto"`,
continuous indicators use Pearson correlations, all-binary indicators
use tetrachoric correlations, ordinal/binary indicators use polychoric
correlations, and genuinely mixed indicator sets use mixed correlations.
Numeric variables with a small number of integer-like response values
are treated conservatively as continuous unless the user explicitly
overrides their modeling type through `types`.

The function does not claim that a scale "has exactly" a particular
number of factors. It reports which factor counts deserve investigation
and records disagreements among retention methods.

## References

Historical retention rules shown as context:

Cattell, R. B. (1966). The scree test for the number of factors.
*Multivariate Behavioral Research, 1*(2), 245-276.
[doi:10.1207/s15327906mbr0102_10](https://doi.org/10.1207/s15327906mbr0102_10)

Guttman, L. (1954). Some necessary conditions for common-factor
analysis. *Psychometrika, 19*(2), 149-161.
[doi:10.1007/BF02289162](https://doi.org/10.1007/BF02289162)

Kaiser, H. F. (1960). The application of electronic computers to factor
analysis. *Educational and Psychological Measurement, 20*(1), 141-151.
[doi:10.1177/001316446002000116](https://doi.org/10.1177/001316446002000116)

Contemporary retention evidence:

Achim, A. (2017). Testing the number of required dimensions in
exploratory factor analysis. *The Quantitative Methods for Psychology,
13*(1), 64-74.
[doi:10.20982/tqmp.13.1.p064](https://doi.org/10.20982/tqmp.13.1.p064)

Braeken, J., & van Assen, M. A. L. M. (2017). An empirical Kaiser
criterion. *Psychological Methods, 22*(3), 450-466.
[doi:10.1037/met0000074](https://doi.org/10.1037/met0000074)

Crawford, A. V., Green, S. B., Levy, R., Lo, W.-J., Scott, L., Svetina,
D., & Thompson, M. S. (2010). Evaluation of parallel analysis methods
for determining the number of factors. *Educational and Psychological
Measurement, 70*(6), 885-901.
[doi:10.1177/0013164410379332](https://doi.org/10.1177/0013164410379332)

Horn, J. L. (1965). A rationale and test for the number of factors in
factor analysis. *Psychometrika, 30*(2), 179-185.
[doi:10.1007/BF02289447](https://doi.org/10.1007/BF02289447)

Lorenzo-Seva, U., Timmerman, M. E., & Kiers, H. A. L. (2011). The Hull
method for selecting the number of common factors. *Multivariate
Behavioral Research, 46*(2), 340-364.
[doi:10.1080/00273171.2011.564527](https://doi.org/10.1080/00273171.2011.564527)

Ruscio, J., & Roche, B. (2012). Determining the number of factors to
retain in an exploratory factor analysis using comparison data of known
factorial structure. *Psychological Assessment, 24*(2), 282-292.
[doi:10.1037/a0025697](https://doi.org/10.1037/a0025697)

Velicer, W. F. (1976). Determining the number of components from the
matrix of partial correlations. *Psychometrika, 41*(3), 321-327.
[doi:10.1007/BF02293557](https://doi.org/10.1007/BF02293557)

Velicer, W. F., Eaton, C. A., & Fava, J. L. (2000). Construct
explication through factor or component analysis: A review and
evaluation of alternative procedures for determining the number of
factors or components. In R. D. Goffin & E. Helmes (Eds.), *Problems and
solutions in human assessment* (pp. 41-71). Springer.
[doi:10.1007/978-1-4615-4397-8_3](https://doi.org/10.1007/978-1-4615-4397-8_3)

Supporting adequacy diagnostics:

Bartlett, M. S. (1950). Tests of significance in factor analysis.
*British Journal of Statistical Psychology, 3*(2), 77-85.
[doi:10.1111/j.2044-8317.1950.tb00285.x](https://doi.org/10.1111/j.2044-8317.1950.tb00285.x)

Kaiser, H. F. (1974). An index of factorial simplicity. *Psychometrika,
39*(1), 31-36.
[doi:10.1007/BF02291575](https://doi.org/10.1007/BF02291575)

## Examples

``` r
fac <- nomo_factors(nomo_demo_continuous, n_iter = 20, seed = 2026)
fac
#> <nomo_factors> Factor-retention evidence
#> Cases: 500 | Items: 10 | Correlation: pearson
#> Criterion set: core | Available methods: 3 | Families: 2 | Skipped: 1
#> Parallel analysis (percentile): 2 | MAP TR2/TR4: 2/2 | KMO: 0.874
#> All 2 available criterion families (3 methods) point to 2 factors. Related
#> methods within a family are grouped before concordance is summarized; this is
#> strong converging evidence for investigating that solution, not proof of
#> dimensionality. 1 requested method was not evaluated; see criterion status for
#> the documented reason.
summary(fac)
#> <nomo_factors summary> Factor-retention evidence
#> Cases: 500 | Items: 10 | Correlation: pearson | Criteria: core
#> 
#> Retention evidence
#>   Method              Factors  Role
#>   Parallel analysis         2  primary
#>   MAP (original TR2)        2  complementary
#>   MAP (revised TR4)         2  complementary
#> 
#> Parallel-analysis rule sensitivity
#>   Rule        Factors  Used
#>   percentile        2  selected
#>   mean              2
#>   crawford          2
#> 
#> Criteria requested but not run
#>   - Empirical Kaiser criterion: EKC needs one common sample size for the
#>     analyzed matrix; pairwise missing-data handling produced varying pairwise
#>     Ns.
#> 
#> Concordance across criterion families
#>   Factors  Families  Which
#>         2         2  Parallel analysis; MAP
#> 
#> Supporting adequacy evidence
#>   - KMO: 0.874
#>   - Bartlett: Bartlett's test was not computed because pairwise missing-data
#>     handling does not provide one common sample size for the full matrix.
#> 
#> Synthesis
#>   All 2 available criterion families (3 methods) point to 2 factors. Related
#>   methods within a family are grouped before concordance is summarized; this
#>   is strong converging evidence for investigating that solution, not proof of
#>   dimensionality. 1 requested method was not evaluated; see criterion status
#>   for the documented reason.
#> 
#> Factor counts are candidates for investigation, not automatic dimensionality
#> verdicts. Common-factor eigenvalues come from a reduced common-variance
#> matrix; later values can be negative.

# \donttest{
# Ordered five-category items are analyzed with polychoric correlations
fac_ord <- nomo_factors(nomo_demo_ordinal, n_iter = 20, seed = 2026)
fac_ord$correlation_method
#> [1] "polychoric"
# }
```
