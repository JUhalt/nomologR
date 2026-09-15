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

## References

Cronbach, L. J., & Meehl, P. E. (1955). Construct validity in
psychological tests. *Psychological Bulletin, 52*(4), 281-302.
[doi:10.1037/h0040957](https://doi.org/10.1037/h0040957)

Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing
for psychological research: A tutorial. *Advances in Methods and
Practices in Psychological Science, 1*(2), 259-269.
[doi:10.1177/2515245918770963](https://doi.org/10.1177/2515245918770963)

Nosek, B. A., Ebersole, C. R., DeHaven, A. C., & Mellor, D. T. (2018).
The preregistration revolution. *Proceedings of the National Academy of
Sciences, 115*(11), 2600-2606.
[doi:10.1073/pnas.1708274114](https://doi.org/10.1073/pnas.1708274114)

## See also

[`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md)
to evaluate the hypotheses.

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
