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

An object of class `nomo_factors` containing the analyzed correlation
matrix, item modeling types, convenience aliases `correlation` and
`modeling_types`, KMO and Bartlett diagnostics, parallel-analysis
results, MAP results, criterion availability/status, method- and
family-level concordance, scree information, a cautious retention
synthesis, and a decision log.

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

## Examples

``` r
set.seed(42)
f <- rnorm(150)
dat <- data.frame(
  i1 = 0.8 * f + rnorm(150, sd = 0.6),
  i2 = 0.8 * f + rnorm(150, sd = 0.6),
  i3 = 0.7 * f + rnorm(150, sd = 0.7),
  i4 = 0.7 * f + rnorm(150, sd = 0.7)
)

fac <- nomo_factors(dat, n_iter = 10, seed = 2026)
fac
#> <nomo_factors>
#> Cases: 150 | Items: 4 | Correlation: pearson
#> Criterion set: core | Available methods: 4 | Families: 3 | Skipped: 0
#> Parallel analysis (percentile): 1 | MAP TR2/TR4: 1/1 | KMO: 0.814
#> All 3 available criterion families (4 methods) point to 1 factor. Related methods within a family are grouped before concordance is summarized; this is strong converging evidence for investigating that solution, not proof of dimensionality. 
summary(fac)
#> <summary_nomo_factors>
#> Cases: 150 | Items: 4 | Correlation: pearson | Criteria: core
#> 
#> Parallel-analysis rule sensitivity:
#> # A tibble: 3 × 3
#>   rule       n_factors selected
#>   <chr>          <int> <lgl>   
#> 1 percentile         1 TRUE    
#> 2 mean               1 FALSE   
#> 3 crawford           1 FALSE   
#> 
#> Retention evidence:
#> # A tibble: 4 × 3
#>   method                     n_factors role         
#>   <chr>                          <int> <chr>        
#> 1 Parallel analysis                  1 primary      
#> 2 MAP (original TR2)                 1 complementary
#> 3 MAP (revised TR4)                  1 complementary
#> 4 Empirical Kaiser criterion         1 complementary
#> 
#> Criterion-family concordance:
#> # A tibble: 1 × 3
#>   n_factors n_families families                                          
#>       <int>      <int> <chr>                                             
#> 1         1          3 Parallel analysis; MAP; Empirical Kaiser criterion
#> 
#> Supporting adequacy evidence:
#> # A tibble: 2 × 2
#>   metric   display                         
#>   <chr>    <chr>                           
#> 1 KMO      0.814                           
#> 2 Bartlett chi-square(6) = 250.99, p < .001
#> 
#> Synthesis:
#> All 3 available criterion families (4 methods) point to 1 factor. Related methods within a family are grouped before concordance is summarized; this is strong converging evidence for investigating that solution, not proof of dimensionality. 
#> 
#> Factor counts are candidates for investigation, not automatic dimensionality verdicts.
#> Common-factor eigenvalues come from a reduced common-variance matrix; later values can be negative.
```
