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
  `"scores"`, `"network"`, `"workflow"`.

- lineage:

  Optional character vector restricting the result to `"historical"`,
  `"contemporary"`, or `"emerging"` methods.

- references:

  If `FALSE` (default), each row is one method and the `references`
  column holds short in-text citations. If `TRUE`, the result is
  expanded to one row per method per reference, with full `citation` and
  `doi` columns, suitable for a reference list.

## Value

A tibble with columns `id`, `stage`, `method`, `lineage`, `role`,
`estimand`, `assumptions`, `implemented_by`, `engine`, `introduced`, and
`contemporary_practice`. With `references = FALSE` there is one row per
method, and `references` holds short in-text citations. With
`references = TRUE` there is one row per method-reference pair, with
`citation_key`, `citation`, and `doi`.

The result is an ordinary tibble with no
[`print()`](https://rdrr.io/r/base/print.html) or
[`summary()`](https://rdrr.io/r/base/summary.html) method of its own, so
the console shortens its long text columns; select columns, or call
[`as.list()`](https://rdrr.io/r/base/list.html) on one row, to read an
entry in full.

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

Two columns record how practice changed:

- `introduced`: the year of the publication that introduced the method.
  It is given only when the registry cites that publication, and is `NA`
  when the registry cites only a later review or critique. It is never
  dated from a secondary source.

- `contemporary_practice`: for a historical method, the registry's
  contemporary methods that now address the question it answered, such
  as parallel analysis for the eigenvalue-greater-than-one rule, or
  omega for coefficient alpha.

[`vignette("research-basis")`](https://juhalt.github.io/nomologR/articles/research-basis.md)
draws a timeline from these columns.

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
cites. They are read from what the object records: the estimator that
ran, the extraction and rotation of an exploratory solution, and an
equivalence test only where one could be computed.

## Examples

``` r
# The whole registry, one row per method
methods <- nomo_methods()
methods[, c("method", "lineage", "role")]
#> # A tibble: 102 × 3
#>    method                                             lineage      role      
#>    <chr>                                              <chr>        <chr>     
#>  1 Item and case missingness audit                    contemporary supporting
#>  2 Response distribution and category-use audit       contemporary supporting
#>  3 Corrected item-rest correlation                    contemporary supporting
#>  4 Fixed item-total correlation reference (about .30) historical   context   
#>  5 Near-zero-variance screening                       contemporary supporting
#>  6 Long-string analysis                               contemporary supporting
#>  7 Inter-item standard deviation                      contemporary supporting
#>  8 Mahalanobis distance screen                        contemporary supporting
#>  9 Even-odd consistency                               contemporary supporting
#> 10 Psychometric antonyms                              contemporary supporting
#> # ℹ 92 more rows

# Everything the registry records about one method
as.list(methods[methods$id == "omega", ])
#> $id
#> [1] "omega"
#> 
#> $stage
#> [1] "reliability"
#> 
#> $method
#> [1] "Model-based coefficient omega"
#> 
#> $lineage
#> [1] "contemporary"
#> 
#> $role
#> [1] "primary"
#> 
#> $estimand
#> [1] "Proportion of composite-score variance attributable to the common factor."
#> 
#> $assumptions
#> [1] "Requires a fitted measurement model; misspecification of that model biases the estimate."
#> 
#> $implemented_by
#> [1] "nomo_reliability()"
#> 
#> $engine
#> [1] "semTools"
#> 
#> $introduced
#> [1] NA
#> 
#> $contemporary_practice
#> [1] NA
#> 
#> $references
#> [1] "Dunn et al. (2014); McNeish (2018); Flora (2020); Bell et al. (2024)"
#> 

# What is shown only as historical context, and why
nomo_methods(lineage = "historical")[, c("method", "role", "assumptions")]
#> # A tibble: 21 × 3
#>    method                                                      role  assumptions
#>    <chr>                                                       <chr> <chr>      
#>  1 Fixed item-total correlation reference (about .30)          cont… No simulat…
#>  2 Eigenvalue-greater-than-one rule                            cont… Over-extra…
#>  3 Scree test                                                  cont… Requires s…
#>  4 Kaiser-Meyer-Olkin sampling adequacy                        supp… Supporting…
#>  5 Bartlett's test of sphericity                               supp… Strongly s…
#>  6 Orthogonal (varimax) rotation                               cont… Forces fac…
#>  7 Other orthogonal rotation (quartimax, equamax, varimin, ge… cont… Forces fac…
#>  8 Fixed loading cutoff                                        cont… No univers…
#>  9 Chi-square exact-fit test                                   supp… Power incr…
#> 10 Fixed fit-index cutoffs                                     cont… Derived un…
#> # ℹ 11 more rows

# How practice changed: historical methods and what now does their work
old <- nomo_methods(lineage = "historical")
old[!is.na(old$contemporary_practice),
    c("introduced", "method", "contemporary_practice")]
#> # A tibble: 16 × 3
#>    introduced method                                       contemporary_practice
#>         <int> <chr>                                        <chr>                
#>  1         NA Fixed item-total correlation reference (abo… item_rest_correlation
#>  2       1954 Eigenvalue-greater-than-one rule             parallel_analysis; m…
#>  3       1966 Scree test                                   parallel_analysis    
#>  4       1958 Orthogonal (varimax) rotation                oblique_rotation     
#>  5         NA Other orthogonal rotation (quartimax, equam… oblique_rotation     
#>  6         NA Fixed loading cutoff                         loading_diagnostics  
#>  7       1969 Chi-square exact-fit test                    incremental_fit; rms…
#>  8       1999 Fixed fit-index cutoffs                      local_strain         
#>  9         NA Modification indices                         local_strain; revisi…
#> 10       1951 Coefficient alpha                            omega                
#> 11       1957 Schmid-Leiman decomposition                  bifactor_model       
#> 12       1981 Fornell-Larcker comparison                   htmt2; latent_correl…
#> 13         NA Unit-weighted sum or mean score              parallel_model_test  
#> 14       1955 Nomological network of construct relations   two_step_sem; predic…
#> 15         NA Listwise deletion                            fiml; missing_sensit…
#> 16         NA Pairwise deletion                            fiml; missing_sensit…

# A reference list for one stage
nomo_methods(stage = "reliability", references = TRUE)[, c("method", "citation")]
#> # A tibble: 30 × 2
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
#> # ℹ 20 more rows
```
