# Build confirmatory factor-analysis syntax

`nomo_model()` is a small convenience helper for reflective CFA models.
It converts a named list of factor-to-indicator assignments into
`lavaan` measurement-model syntax. It deliberately does not add residual
covariances, cross-loadings, equality constraints, or other post-hoc
changes.

## Usage

``` r
nomo_model(
  factors,
  structure = c("correlated", "higher_order", "bifactor"),
  general = "G"
)
```

## Arguments

- factors:

  A named list. Each element name is a latent-factor name and each
  element value is a character vector of observed indicators. For
  hierarchical structures these are the first-order or group factors.

- structure:

  One of `"correlated"`, `"higher_order"`, or `"bifactor"`.

- general:

  Name of the second-order or general factor. Used only when `structure`
  is `"higher_order"` or `"bifactor"`.

## Value

A character scalar of class `nomo_model` that can be passed directly to
[`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
or to [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).
Attributes record the `factors`, `structure`, `general` factor, and any
identification `notes`.

## Details

Three structures are available, and the researcher chooses among them.
The helper never derives a structure from exploratory results.

- `"correlated"` (default): one factor per element of `factors`, with
  factors free to correlate.

- `"higher_order"`: the elements of `factors` become first-order
  factors, and a second-order factor named by `general` explains their
  correlations. At least three first-order factors are required, because
  with two the second-order loadings are not identified without further
  constraints. With exactly three, the second-order part is just
  identified: the model fits exactly as well as the correlated-factors
  model, so fit cannot distinguish the two.

- `"bifactor"`: a general factor named by `general` is measured by every
  indicator, and each element of `factors` becomes a group factor. All
  factors are orthogonal. Identification is written into the syntax
  (each factor's first loading is freed and its variance fixed to 1), so
  the model is identified the same way whatever `std.lv` is used to fit
  it. At least two group factors with at least two indicators each are
  required; configurations known to be fragile are noted.

Any identification notes are attached as the `"notes"` attribute and
printed with the syntax. Use
[`nomo_hierarchical()`](https://juhalt.github.io/nomologR/reference/nomo_hierarchical.md)
to evaluate a fitted higher-order or bifactor model.

## References

Holzinger, K. J., & Swineford, F. (1937). The bi-factor method.
*Psychometrika, 2*(1), 41-54.
[doi:10.1007/BF02287965](https://doi.org/10.1007/BF02287965)

Reise, S. P. (2012). The rediscovery of bifactor measurement models.
*Multivariate Behavioral Research, 47*(5), 667-696.
[doi:10.1080/00273171.2012.715555](https://doi.org/10.1080/00273171.2012.715555)

Yung, Y.-F., Thissen, D., & McLeod, L. D. (1999). On the relationship
between the higher-order factor model and the hierarchical factor model.
*Psychometrika, 64*(2), 113-128.
[doi:10.1007/BF02294531](https://doi.org/10.1007/BF02294531)

## Examples

``` r
factors <- list(
  engagement = c("e1", "e2", "e3"),
  belonging = c("b1", "b2", "b3"),
  efficacy = c("f1", "f2", "f3")
)

nomo_model(factors)
#> engagement =~ e1 + e2 + e3
#> belonging =~ b1 + b2 + b3
#> efficacy =~ f1 + f2 + f3
nomo_model(factors, structure = "higher_order", general = "Wellbeing")
#> engagement =~ e1 + e2 + e3
#> belonging =~ b1 + b2 + b3
#> efficacy =~ f1 + f2 + f3
#> Wellbeing =~ NA*engagement + belonging + efficacy
#> Wellbeing ~~ 1*Wellbeing
#> 
#> Identification notes:
#> - With three first-order factors the second-order part is just identified: this model fits exactly as well as the correlated-factors model, so model fit cannot distinguish the two.
nomo_model(factors, structure = "bifactor", general = "Wellbeing")
#> Wellbeing =~ NA*e1 + e2 + e3 + b1 + b2 + b3 + f1 + f2 + f3
#> engagement =~ NA*e1 + e2 + e3
#> belonging =~ NA*b1 + b2 + b3
#> efficacy =~ NA*f1 + f2 + f3
#> Wellbeing ~~ 1*Wellbeing
#> engagement ~~ 1*engagement
#> belonging ~~ 1*belonging
#> efficacy ~~ 1*efficacy
#> Wellbeing ~~ 0*engagement + 0*belonging + 0*efficacy
#> engagement ~~ 0*belonging + 0*efficacy
#> belonging ~~ 0*efficacy
```
