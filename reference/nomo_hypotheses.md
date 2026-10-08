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

A `nomo_hypotheses` object whose `hypotheses` field is one row per
hypothesis: its relation, prediction, theoretical region, scale, and
origin. `n` is the number of hypotheses.

[`print()`](https://rdrr.io/r/base/print.html) shows the relations with
each predicted region in words, their scale, and their origin.
[`summary()`](https://rdrr.io/r/base/summary.html) adds the counts by
origin, how many predictions can be confirmed as specified, and what
each prediction claims.

## Details

The function records theory; it does not inspect data, fit a model, or
infer predictions from statistical significance.

A pair of variables carries a directed path or an association, not both.
Hypotheses that give the same two variables both are refused, because
the two cannot be estimated side by side. Directed paths in both
directions are accepted here, because whether a reciprocal pair is
identified depends on the model.
[`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md)
evaluates both when `model` itself writes the two paths, as a model of
reciprocal effects identified by instruments does, and stops rather than
add one of them.

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
#> <nomo_hypotheses> Theory-specified relations
#> Relations: 3 | A priori: 3 | Post hoc: 0
#> 
#>   ID  Relation                      Prediction  Region         Origin
#>   H1  GSE -> Spirituality           positive    > 0            A priori
#>   H2  GSE -> Religiosity            negligible  [-0.10, 0.10]  A priori
#>   H3  Religiosity <-> Spirituality  positive    >= .20         A priori
#>   Every relation is on the standardized scale.
#> 
#> The relations record theory; only nomo_network() evaluates them against data.
#> 
#> See nomo_table(x) for every column and nomo_network(model, data, x) for the
#> evidence.
```
