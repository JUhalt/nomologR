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
  order). Overrides do not reorder or relabel categories, and the
  supplied data are not modified. Polychoric and mixed correlations
  score ordinal items by the rank of their observed values, in numeric
  order or a factor's level order, so codes such as 0/25/50/75/100 or
  1/3/5, and factor levels nobody chose, are analyzed as consecutive
  categories. Pearson correlations use the values as coded (a factor's
  level positions). Binary items are coded 0/1. Polychoric correlations
  model at most 8 categories; an ordinal item with more is refused with
  the alternatives. For example,
  `c(item1 = "ordinal", item2 = "ordinal")`.

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
  or `"crawford"`. A factor is retained while its observed eigenvalue
  exceeds the null reference. `"percentile"` compares every eigenvalue
  with the `quantile` of the null eigenvalues; `"mean"` compares it with
  their mean; `"crawford"` uses the `quantile` for the first eigenvalue
  and the mean for the rest (Crawford et al., 2010). All three rules are
  computed from the same null simulations and retained in the result as
  sensitivity evidence.

- n_iter:

  Number of null-data iterations used for parallel analysis. If `NULL`,
  the value in `guidance$factor_parallel_iterations` is used. It also
  sets the number of simulated data sets for NEST and Hull, which
  default to 1000 in `EFAtools`, so a small `n_iter` makes those
  criteria coarser too.

- quantile:

  Quantile of null eigenvalues used as the parallel-analysis reference.
  If `NULL`, the value in `guidance$factor_parallel_quantile` is used.
  It also sets the percentile Hull compares its fit values with.

- max_factors:

  Maximum number of factors/components evaluated for MAP, and the
  largest factor count comparison data tries. If `NULL`, up to 10 or
  `p - 1`, whichever is smaller, are evaluated.

- seed:

  Integer seed for the null-data simulation. The caller's random number
  state is restored before return.

- fm:

  Common-factor extraction method passed to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) when obtaining
  the factor eigenvalues for parallel analysis. The default is
  `"minres"`. Supported values are `"minres"`, `"uls"`, `"ols"`,
  `"wls"`, `"gls"`, `"pa"`, `"ml"`, `"minchi"`, and `"old.min"`. For
  `"minchi"`, the number of cases observed for each item pair is passed
  to [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html), which
  weights the residuals by it. `"alpha"` is not available here: the
  eigenvalues come from a one-factor solution, which
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) cannot fit with
  alpha factoring (use it in
  [`nomo_efa()`](https://juhalt.github.io/nomologR/reference/nomo_efa.md)
  with two or more factors). `"minrank"` is not supported, because
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) needs the
  `Rcsdp` package for it.

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

- `items`, `n_cases`, and `n_items`. `n_cases` is the number of rows
  analyzed (every row under `missing = "pairwise"`, including rows with
  no item data; the complete rows under `missing = "complete"`).

- `min_pairwise_n`: the smallest number of cases observed jointly on any
  item pair, the effective sample size under pairwise deletion. It
  equals `n_cases` when no item value is missing or
  `missing = "complete"`.

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

Comparison data (`criterion_set = "all"`) simulates populations of known
structure with the settings `factor_cd_population` (5000 cases),
`factor_cd_samples` (100 samples per candidate structure), and
`factor_cd_alpha` (.30) from
[`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).
These are smaller than the 10000 cases and 500 samples
[`EFAtools::efa_cd()`](https://mdsteiner.github.io/EFAtools/reference/efa_cd.html)
uses by default, to keep the run short; raise them in `guidance` for a
final analysis. The settings used are recorded in the decision log.

[`print()`](https://rdrr.io/r/base/print.html) shows the count each main
criterion suggests and the synthesis;
[`summary()`](https://rdrr.io/r/base/summary.html) adds the evidence by
method, the parallel-analysis rule sensitivity, the criteria that did
not run and why, the concordance across criterion families, and the
supporting adequacy evidence.

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
#> Cases: 500 (minimum pairwise N: 473) | Items: 10 | Correlation: Pearson
#> Criterion set: core | Methods run: 3 | Families: 2 | Not run: 1 | Flags: none
#> Parallel analysis (percentile rule): 2 | MAP: 2 (TR2), 2 (TR4) | KMO: .87
#> MAP = Velicer's minimum average partial criterion, original (TR2) and revised
#> (TR4); KMO = Kaiser-Meyer-Olkin measure of sampling adequacy.
#> 
#> Both available criterion families (3 methods) point to 2 factors. Agreement
#> between two criterion families is limited evidence for investigating that
#> solution, not proof of dimensionality. 1 requested method was not evaluated;
#> nomo_table(x, "criteria") gives the reason.
#> 
#> See summary(x) for the evidence by method and the criteria that did not run.
summary(fac)
#> <nomo_factors summary> Factor-retention evidence
#> Cases: 500 (minimum pairwise N: 473) | Items: 10 | Correlation: Pearson
#> Criterion set: core
#> 
#> Retention evidence
#>   Method              Factors  Role
#>   Parallel analysis         2  Primary
#>   MAP (original TR2)        2  Complementary
#>   MAP (revised TR4)         2  Complementary
#> 
#> Parallel-analysis rule sensitivity
#>   Rule        Factors  Used
#>   Percentile        2  Selected
#>   Mean              2
#>   Crawford          2
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
#>   - KMO: .87
#>   - Bartlett's test was not computed because pairwise missing-data handling
#>     does not provide one common sample size for the full matrix.
#> 
#> Synthesis
#>   Both available criterion families (3 methods) point to 2 factors. Agreement
#>   between two criterion families is limited evidence for investigating that
#>   solution, not proof of dimensionality. 1 requested method was not evaluated;
#>   nomo_table(x, "criteria") gives the reason.
#> 
#> Abbreviations
#>   MAP -- Minimum average partial criterion (Velicer). TR2, the original,
#>       averages squared partial correlations; TR4, the revised, averages fourth
#>       powers.
#>   KMO -- Kaiser-Meyer-Olkin measure of sampling adequacy.
#>   EKC -- Empirical Kaiser criterion.
#> 
#> Factor counts are candidates for investigation, not automatic dimensionality
#> verdicts. Common-factor eigenvalues come from a reduced common-variance
#> matrix; later values can be negative.
#> 
#> See nomo_table(x, "criteria") for the status of every requested criterion.

# \donttest{
# Ordered five-category items are analyzed with polychoric correlations
fac_ord <- nomo_factors(nomo_demo_ordinal, n_iter = 20, seed = 2026)
fac_ord$correlation_method
#> [1] "polychoric"
# }
```
