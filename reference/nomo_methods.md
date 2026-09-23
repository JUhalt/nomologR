# The methods registry: what nomologR computes, and where it comes from

`nomo_methods()` returns a machine-readable registry of the statistical
methods `nomologR` implements. Each entry records the workflow stage,
where the method sits in the literature, what it estimates, its key
assumptions, the function that implements it, the computational engine,
and DOI-verified references.

## Usage

``` r
nomo_methods(x = NULL, stage = NULL, lineage = NULL, references = FALSE)
```

## Arguments

- x:

  Optional `nomologR` result object. If supplied, only the methods
  actually used in producing that object are returned, in registry
  order. If `NULL` (default), the whole registry is returned.

- stage:

  Optional character vector restricting the result to these workflow
  stages. One or more of `"screen"`, `"factors"`, `"efa"`, `"cfa"`,
  `"compare"`, `"reliability"`, `"validity"`, `"invariance"`,
  `"network"`, `"workflow"`.

- lineage:

  Optional character vector restricting the result to `"historical"`,
  `"contemporary"`, or `"emerging"` methods.

- references:

  If `FALSE` (default), each row is one method and the `references`
  column holds short in-text citations. If `TRUE`, the result is
  expanded to one row per method per reference, with full `citation` and
  `doi` columns, suitable for a reference list.

## Value

A tibble. With `references = FALSE`, one row per method. With
`references = TRUE`, one row per method-reference pair.

## Details

The registry answers a question learners ask constantly and software
rarely answers: *why this method, and where did it come from?* Every
entry carries a `lineage` label:

- `"historical"`: techniques a reader will meet in published work,
  retained so they can be recognized and interpreted. They are labeled
  and qualified.

- `"contemporary"`: what the current methodological literature
  recommends.

- `"emerging"`: recent proposals with a smaller evidence base.

A lineage label summarizes where a method sits in the literature. It is
a teaching aid, not a claim that an older method is always wrong.

The `role` column says how `nomologR` uses the method: `"primary"`
evidence, `"supporting"` evidence, or `"context"`. A `"context"` method
is displayed for recognition and is deliberately excluded from any
synthesis: the eigenvalue-greater-than-one rule and fixed fit-index
cutoffs are shown so a reader can interpret older reports, never so
`nomologR` can act on them.

**The registry describes only what the package actually computes.**
Methods that are planned but not implemented are discussed in the
research-basis article and tracked in issues; they are deliberately
absent here, so that `nomo_methods(run)` can never credit a run with a
method it did not use.

Passing a `nomologR` result object returns only the methods that object
actually used, which is what
[`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
cites.

## Examples

``` r
# The whole registry
nomo_methods()
#> # A tibble: 86 × 10
#>    id      stage method lineage role  estimand assumptions implemented_by engine
#>    <chr>   <chr> <chr>  <chr>   <chr> <chr>    <chr>       <chr>          <chr> 
#>  1 missin… scre… Item … contem… supp… Proport… Descriptiv… nomo_screen()  nomol…
#>  2 respon… scre… Respo… contem… supp… Categor… Requires i… nomo_screen()  nomol…
#>  3 item_r… scre… Corre… contem… supp… Correla… Depends on… nomo_screen()  nomol…
#>  4 item_t… scre… Fixed… histor… cont… A conve… No simulat… nomo_screen()  nomol…
#>  5 near_z… scre… Near-… contem… supp… Frequen… Heuristic … nomo_screen()  nomol…
#>  6 long_s… scre… Long-… contem… supp… Longest… Reads raw … nomo_screen()  nomol…
#>  7 inter_… scre… Inter… contem… supp… Within-… Detects ra… nomo_screen()  nomol…
#>  8 mahala… scre… Mahal… contem… supp… Multiva… Unaffected… nomo_screen()  nomol…
#>  9 even_o… scre… Even-… contem… supp… Within-… Needs at l… nomo_screen()  nomol…
#> 10 psycho… scre… Psych… contem… supp… Within-… Needs at l… nomo_screen()  nomol…
#> # ℹ 76 more rows
#> # ℹ 1 more variable: references <chr>

# What is shown only as historical context, and why
nomo_methods(lineage = "historical")[, c("method", "role", "assumptions")]
#> # A tibble: 18 × 3
#>    method                                               role       assumptions  
#>    <chr>                                                <chr>      <chr>        
#>  1 Fixed item-total correlation reference (about .30)   context    No simulatio…
#>  2 Eigenvalue-greater-than-one rule                     context    Over-extract…
#>  3 Scree test                                           context    Requires sub…
#>  4 Kaiser-Meyer-Olkin sampling adequacy                 supporting Supporting a…
#>  5 Bartlett's test of sphericity                        supporting Strongly sam…
#>  6 Orthogonal (varimax) rotation                        context    Forces facto…
#>  7 Fixed loading cutoff                                 context    No universal…
#>  8 Chi-square exact-fit test                            supporting Power increa…
#>  9 Fixed fit-index cutoffs                              context    Derived unde…
#> 10 Modification indices                                 context    Data-driven …
#> 11 Coefficient alpha                                    supporting Assumes equa…
#> 12 Schmid-Leiman decomposition                          supporting Originally a…
#> 13 Standardized loadings and average variance extracted supporting Convergent e…
#> 14 Fornell-Larcker comparison                           context    Simulation w…
#> 15 Unit-weighted sum or mean score                      primary    Not a model-…
#> 16 Regression (Thurstone) factor scores                 primary    Indeterminat…
#> 17 Bartlett factor scores                               primary    Indeterminat…
#> 18 Nomological network of construct relations           primary    Evidence for…

# A reference list for one stage
nomo_methods(stage = "reliability", references = TRUE)[, c("method", "citation")]
#> # A tibble: 24 × 2
#>    method                                         citation                      
#>    <chr>                                          <chr>                         
#>  1 Model-based coefficient omega                  Dunn, T. J., Baguley, T., & B…
#>  2 Model-based coefficient omega                  McNeish, D. (2018). Thanks co…
#>  3 Model-based coefficient omega                  Flora, D. B. (2020). Your coe…
#>  4 Model-based coefficient omega                  Bell, S. M., Chalmers, R. P.,…
#>  5 Reliability on the ordered-score scale         Green, S. B., & Yang, Y. (200…
#>  6 Coefficient alpha                              Cronbach, L. J. (1951). Coeff…
#>  7 Coefficient alpha                              Sijtsma, K. (2009). On the us…
#>  8 Bootstrap confidence intervals for reliability Kelley, K., & Pornprasertmani…
#>  9 Omega hierarchical                             Reise, S. P. (2012). The redi…
#> 10 Omega hierarchical                             Reise, S. P., Bonifay, W. E.,…
#> # ℹ 14 more rows
```
