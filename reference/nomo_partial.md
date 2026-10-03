# Specify researcher-controlled partial-invariance releases

`nomo_partial()` records equality constraints that the researcher has
decided to release at a specific invariance level. It does not inspect
modification indices, search for a better-fitting model, or choose
releases automatically.

## Usage

``` r
nomo_partial(level, syntax, rationale)
```

## Arguments

- level:

  Character vector naming the first invariance level at which each
  release should apply. Supported labels are `thresholds`, `metric`,
  `scalar`, `strong`, and `strict`; the applicable subset depends on the
  indicator category structure. It should be the level that first holds
  the released parameter's type equal, such as `metric` for a loading.

- syntax:

  Character vector of lavaan parameter expressions to exclude from the
  corresponding equality constraints, naming the factors and indicators
  as the model does.

- rationale:

  Character vector documenting why each release was chosen. A single
  rationale may be recycled across multiple releases.

## Value

A `nomo_partial` object whose `releases` field is one row per release:
its identifier, level, syntax, and rationale. `n` is the number of
releases.

## Details

Releases are written using ordinary lavaan parameter syntax accepted by
the `group.partial` argument of
[`semTools::measEq.syntax()`](https://rdrr.io/pkg/semTools/man/measEq.syntax.html),
for example `"F =~ x2"`, `"x3 ~ 1"`, or `"u2 | t1"`.

A release first requested at one level is carried forward to later,
more-restrictive levels so that a loading freed at the metric level does
not silently become constrained again at the scalar level.

[`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md)
and
[`nomo_invariance_longitudinal()`](https://juhalt.github.io/nomologR/reference/nomo_invariance_longitudinal.md)
check each release against the model when they fit it. A release must
name a loading, intercept, threshold, or residual variance of an
indicator in the model, and must free it in the generated model;
otherwise it is an error. It applies from the level that first holds its
parameter type equal: declared earlier, it is moved to that level, and
the decision log says so; declared later, it is an error, because the
two models would not be nested.

## References

Byrne, B. M., Shavelson, R. J., & Muthén, B. (1989). Testing for the
equivalence of factor covariance and mean structures: The issue of
partial measurement invariance. *Psychological Bulletin, 105*(3),
456-466.
[doi:10.1037/0033-2909.105.3.456](https://doi.org/10.1037/0033-2909.105.3.456)

Putnick, D. L., & Bornstein, M. H. (2016). Measurement invariance
conventions and reporting: The state of the art and future directions
for psychological research. *Developmental Review, 41*, 71-90.
[doi:10.1016/j.dr.2016.06.004](https://doi.org/10.1016/j.dr.2016.06.004)

## Examples

``` r
partial <- nomo_partial(
  level = c("metric", "scalar"),
  syntax = c("F =~ x2", "x3 ~ 1"),
  rationale = c(
    "Loading difference was theoretically anticipated.",
    "Intercept difference was prespecified from prior evidence."
  )
)
partial
#> <nomo_partial> Partial invariance releases
#> 2 researcher-specified releases
#> 
#>   - P1 (metric): F =~ x2. Loading difference was theoretically anticipated.
#>   - P2 (scalar): x3 ~ 1. Intercept difference was prespecified from prior
#>     evidence.
#> 
#> No release was selected automatically by nomologR.
```
