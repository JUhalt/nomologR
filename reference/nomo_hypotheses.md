# Specify a priori expectations for a nomological network

`nomo_hypotheses()` converts named theoretical predictions into a
machine-readable object that can later be evaluated by
[`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md).
Relations use `A -> B` for directed structural paths and `A <-> B` for
associations/covariances.

## Usage

``` r
nomo_hypotheses(...)
```

## Arguments

- ...:

  Named
  [nomo_expectations](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
  created by
  [`positive()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md),
  [`negative()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md),
  or
  [`negligible()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md).
  Names must specify relations using `->` or `<->`.

## Value

A `nomo_hypotheses` object containing a tidy, machine-readable
hypothesis table.

## Details

The function records theory; it does not inspect data, fit a model, or
infer predictions from statistical significance.

## Examples

``` r
h <- nomo_hypotheses(
  "GSE -> Spirituality" = positive(),
  "GSE -> Religiosity" = negligible(within = c(-.10, .10)),
  "Religiosity <-> Spirituality" = positive(min = .20)
)
h
#> <nomo_hypotheses>
#> 3 theory-specified relation(s)
#> 
#> # A tibble: 3 × 6
#>   id    relation                     prediction region      scale       
#>   <chr> <chr>                        <chr>      <chr>       <chr>       
#> 1 H1    GSE -> Spirituality          positive   (0, +Inf)   standardized
#> 2 H2    GSE -> Religiosity           negligible [-0.1, 0.1] standardized
#> 3 H3    Religiosity <-> Spirituality positive   [0.2, +Inf) standardized
#>   origin  
#>   <chr>   
#> 1 a_priori
#> 2 a_priori
#> 3 a_priori
```
