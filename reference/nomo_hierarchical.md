# Evaluate general-factor strength in bifactor and higher-order models

`nomo_hierarchical()` answers the question behind many published scales
that report both a total score and subscale scores: how much of each
score reflects a general factor, and how much reliable variance does
each subscale carry beyond it? It evaluates a fitted bifactor or
higher-order CFA and reports omega total, omega hierarchical, explained
common variance (ECV), the percentage of uncontaminated correlations
(PUC), and subscale omega and omega hierarchical subscale, each with its
estimand stated.

## Usage

``` r
nomo_hierarchical(
  fit,
  general = NULL,
  obs.var = TRUE,
  guidance = nomo_defaults()
)
```

## Arguments

- fit:

  A `nomo_cfa` object or fitted `lavaan` model containing a bifactor or
  higher-order measurement model. Single-group, single-level models
  only.

- general:

  Optional name of the general (bifactor) or second-order (higher-order)
  factor. If `NULL`, it is identified from the model and the call is
  refused if that is ambiguous.

- obs.var:

  Logical. `TRUE` (default, matching
  [`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md))
  uses observed covariances in each omega's denominator; `FALSE` uses
  model-implied covariances, which reproduces the formulas in Rodriguez,
  Reise, and Haviland (2016).

- guidance:

  Guidance settings returned by
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

A `nomo_hierarchical` object with the detected `structure`, the
`general` factor, the `groups` and their items, an `indices` table, a
`subscales` table, an item-level `loadings` table, identification and
estimand notes, and a `decision_log`.

## Details

**Structure.** The structure is read from the fitted model, so syntax
written by hand works as well as syntax from
[`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md):

- *Higher-order:* one second-order factor measured by first-order
  factors, each measured by its own items. The first-order factors'
  disturbances are the group sources. Their loadings are the
  Schmid-Leiman decomposition of the higher-order solution, which is
  exact for a confirmatory model.

- *Bifactor:* one general factor measured by every item, plus group
  factors measured by disjoint subsets of items. Items may load on the
  general factor alone.

The general and group sources must be orthogonal, because variance can
be attributed to a source only when sources do not covary. A model that
allows them to correlate is refused with an explanation.

**Estimands.** Every omega is the proportion of the variance of a
unit-weighted composite that a set of sources explains. With continuous
indicators the composite is the observed sum score. With ordered
indicators the indices describe the *latent-response* composite, an
upper bound on the reliability of the observed ordinal sum score; the
observed ordinal-scale version is not provided. ECV and PUC describe the
item set and do not depend on the denominator.

**No verdicts.** The indices are reported with plain-language
interpretation, but no value is treated as a pass/fail threshold. Reise
(2012) notes that no benchmark value of ECV establishes when a general
factor is strong enough, and that when PUC is very high, even modest ECV
can yield relatively unbiased estimates from a unidimensional model.

**Choosing a structure.** A bifactor model will usually fit at least as
well as a correlated-factors or higher-order model of the same items,
including when it is not the model that generated the data (Reise,
2012), and a higher-order model is a constrained version of the bifactor
model (Yung, Thissen, & McLeod, 1999). Compare the alternatives with
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)
and choose on substantive grounds, not on fit alone.
`nomo_hierarchical()` does not choose.

## References

Holzinger, K. J., & Swineford, F. (1937). The bi-factor method.
*Psychometrika, 2*(1), 41-54.
[doi:10.1007/BF02287965](https://doi.org/10.1007/BF02287965)

Reise, S. P. (2012). The rediscovery of bifactor measurement models.
*Multivariate Behavioral Research, 47*(5), 667-696.
[doi:10.1080/00273171.2012.715555](https://doi.org/10.1080/00273171.2012.715555)

Reise, S. P., Bonifay, W. E., & Haviland, M. G. (2013). Scoring and
modeling psychological measures in the presence of multidimensionality.
*Journal of Personality Assessment, 95*(2), 129-140.
[doi:10.1080/00223891.2012.725437](https://doi.org/10.1080/00223891.2012.725437)

Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Evaluating
bifactor models: Calculating and interpreting statistical indices.
*Psychological Methods, 21*(2), 137-150.
[doi:10.1037/met0000045](https://doi.org/10.1037/met0000045)

Schmid, J., & Leiman, J. M. (1957). The development of hierarchical
factor solutions. *Psychometrika, 22*(1), 53-61.
[doi:10.1007/BF02289209](https://doi.org/10.1007/BF02289209)

Yung, Y.-F., Thissen, D., & McLeod, L. D. (1999). On the relationship
between the higher-order factor model and the hierarchical factor model.
*Psychometrika, 64*(2), 113-128.
[doi:10.1007/BF02294531](https://doi.org/10.1007/BF02294531)

## Examples

``` r
set.seed(2026)
n <- 500
g <- rnorm(n)
s <- matrix(rnorm(n * 3), n, 3)
dat <- as.data.frame(sapply(1:9, function(i) {
  .6 * g + .45 * s[, ceiling(i / 3)] + rnorm(n, sd = .65)
}))
names(dat) <- paste0("x", 1:9)

factors <- list(
  A = c("x1", "x2", "x3"),
  B = c("x4", "x5", "x6"),
  C = c("x7", "x8", "x9")
)

bf <- nomo_cfa(nomo_model(factors, structure = "bifactor"), data = dat)
h <- nomo_hierarchical(bf)
h
#> <nomo_hierarchical>
#> Bifactor model | general factor: G | group factors: A, B, C
#> Estimand: unit-weighted observed composite
#> 
#> Total score
#> # A tibble: 5 × 2
#>   index                       estimate
#>   <chr>                          <dbl>
#> 1 omega_total                    0.896
#> 2 omega_hierarchical             0.741
#> 3 omega_hierarchical_relative    0.827
#> 4 ecv                            0.609
#> 5 puc                            0.75 
#> 
#> Subscales
#> # A tibble: 3 × 4
#>   subscale n_items omega_subscale omega_hierarchical_subscale
#>   <chr>      <int>          <dbl>                       <dbl>
#> 1 A              3          0.798                       0.33 
#> 2 B              3          0.791                       0.354
#> 3 C              3          0.799                       0.239
#> 
#> Notes
#> - [review] A bifactor model will usually fit at least as well as correlated-factors or higher-order models of the same items, even when it did not generate the data (Reise, 2012), and a higher-order model is a constrained version of it (Yung, Thissen, & McLeod, 1999). Compare the alternatives with nomo_compare() and choose on substantive grounds, not on fit alone.
#> 
#> No index is treated as a pass/fail threshold; see nomo_table(x, "indices").
nomo_table(h, "subscales")
#> # A tibble: 3 × 6
#>   subscale n_items omega_subscale omega_hierarchical_s…¹ omega_hs_relative items
#>   <chr>      <int>          <dbl>                  <dbl>             <dbl> <chr>
#> 1 A              3          0.798                  0.330             0.414 x1, …
#> 2 B              3          0.791                  0.354             0.448 x4, …
#> 3 C              3          0.799                  0.239             0.298 x7, …
#> # ℹ abbreviated name: ¹​omega_hierarchical_subscale
```
