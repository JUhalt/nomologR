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
model (Yung, Thissen, & McLeod, 1999). Bonifay, Lane, and Reise (2017)
treat that tendency to show superior goodness of fit as a particular
concern, and note that the superior performance "may be a symptom of
overfitting", capturing unwanted noise as well as real trends. Murray
and Johnson (2013) compared these two structures directly and found the
comparison itself biased: unless there was essentially no unmodeled
complexity, their simulation favored the bifactor model even when a
higher-order model generated the data, and they concluded that the
choice "should not rely on which is better fitting". Compare the
alternatives with
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)
and choose on substantive grounds, not on fit alone.
`nomo_hierarchical()` does not choose.

**Factor scores: two indices that answer different questions.** The
`factors` table reports, for the general factor and each group factor,
factor determinacy and construct replicability.

*Factor determinacy* (Beauducel, 2011; Rodriguez, Reise, & Haviland,
2016) is the correlation between a factor and its estimated factor
score. It is computed from the whole model-reproduced correlation
matrix, so a group factor's score estimate can use the other items to
partial out the general factor. `determinacy_r2` is its square, the
proportion of variance in factor scores explained by the factor, and
`min_competing_r` is the minimum possible correlation between two
equally valid sets of factor scores, twice the squared determinacy minus
one. A negative value there means two researchers scoring the same data
could rank people in opposite orders and both be consistent with the
model.

*Construct replicability* H (Hancock & Mueller, 2001) is the proportion
of variance in a factor explainable by its own indicators when optimally
weighted. It uses only that factor's loadings and treats the remainder
of each item as uncorrelated residual.

The two are the same quantity when the data are unidimensional, and can
differ under a bifactor model, where an item's residual with respect to
one factor contains the other factors and is correlated across items.
Rodriguez et al. (2016) state this and decline to prefer either, so both
are reported. Determinacy is always computed from the model-reproduced
matrix, whatever `obs.var` is set to, because that is what the formula
is defined on.

Gorsuch (1983) recommended using factor score estimates only when
determinacy exceeds .90, with competing score sets correlating above
.70, and Hancock and Mueller (2001) proposed .70 as a standard for H.
These are reported as their authors' recommendations where a value falls
below them, as with fixed fit-index cutoffs elsewhere in the package,
and are never applied as rules.

## References

Beauducel, A. (2011). Indeterminacy of factor score estimates in
slightly misspecified confirmatory factor models. *Journal of Modern
Applied Statistical Methods, 10*(2), 583-598.
[doi:10.22237/jmasm/1320120900](https://doi.org/10.22237/jmasm/1320120900)

Bonifay, W., Lane, S. P., & Reise, S. P. (2017). Three concerns with
applying a bifactor model as a structure of psychopathology. *Clinical
Psychological Science, 5*(1), 184-186.
[doi:10.1177/2167702616657069](https://doi.org/10.1177/2167702616657069)

Gorsuch, R. L. (1983). *Factor analysis* (2nd ed.). Lawrence Erlbaum.

Hancock, G. R., & Mueller, R. O. (2001). Rethinking construct
reliability within latent variable systems. In R. Cudeck, S. du Toit, &
D. Sorbom (Eds.), *Structural equation modeling: Present and future*
(pp. 195-216). Scientific Software International.

Holzinger, K. J., & Swineford, F. (1937). The bi-factor method.
*Psychometrika, 2*(1), 41-54.
[doi:10.1007/BF02287965](https://doi.org/10.1007/BF02287965)

Murray, A. L., & Johnson, W. (2013). The limitations of model fit in
comparing the bi-factor versus higher-order models of human cognitive
ability structure. *Intelligence, 41*(5), 407-422.
[doi:10.1016/j.intell.2013.06.004](https://doi.org/10.1016/j.intell.2013.06.004)

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
#> Factor scores
#> # A tibble: 4 × 5
#>   factor role    factor_determinacy min_competing_r construct_replicability
#>   <chr>  <chr>                <dbl>           <dbl>                   <dbl>
#> 1 G      general              0.867           0.502                   0.829
#> 2 A      group                0.704          -0.009                   0.489
#> 3 B      group                0.715           0.023                   0.501
#> 4 C      group                0.641          -0.178                   0.406
#> 
#> Notes
#> - [review] A bifactor model will usually fit at least as well as correlated-factors or higher-order models of the same items, even when it did not generate the data (Reise, 2012), and a higher-order model is a constrained version of it (Yung, Thissen, & McLeod, 1999). Bonifay, Lane, and Reise (2017) call the bifactor model's tendency to show superior goodness of fit in model comparison studies a particular concern, and say that superior fit may be a symptom of overfitting: modeling not only the trends in the data but also unwanted noise. Murray and Johnson (2013) compared these two structures directly and found the comparison biased in favor of the bifactor model: unless there was essentially no unmodeled complexity, their simulation favored the bifactor model even when a higher-order model generated the data. They concluded that which model to adopt should not rely on which is better fitting. Compare the alternatives with nomo_compare() and choose on substantive grounds, not on fit alone.
#> - [review] Factor determinacy is at or below .90 for G, A, B, C. Gorsuch (1983, p. 260) recommended using factor score estimates only above that value. This is his recommendation reported as context, not a rule applied here; the score may still be usable for some purposes.
#> - [review] Two equally valid sets of factor scores could correlate as low as G (0.50), A (-0.01), B (0.02), C (-0.18). Gorsuch (1983, p. 260) suggested this minimum be above .70. A negative value means two researchers scoring the same data could rank people in opposite orders and both be consistent with the model.
#> - [review] Construct replicability H is below .70 for A, B, C. Hancock and Mueller (2001) proposed .70 as a standard; a factor below it is not well defined by its own indicators and is expected to change across studies. Reported as their standard, not applied as a rule.
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
