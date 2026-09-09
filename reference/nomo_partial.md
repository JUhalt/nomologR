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
  indicator category structure.

- syntax:

  Character vector of lavaan parameter expressions to exclude from the
  corresponding equality constraints.

- rationale:

  Character vector documenting why each release was chosen. A single
  rationale may be recycled across multiple releases.

## Value

A `nomo_partial` object with a report-ready release table.

## Details

Releases are written using ordinary lavaan parameter syntax accepted by
the `group.partial` argument of
[`semTools::measEq.syntax()`](https://rdrr.io/pkg/semTools/man/measEq.syntax.html),
for example `"F =~ x2"`, `"x3 ~ 1"`, or `"u2 | t1"`.

A release first requested at one level is carried forward to later,
more-restrictive levels so that a loading freed at the metric level does
not silently become constrained again at the scalar level.

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
#> <nomo_partial>
#> 2 researcher-specified release(s)
#> 
#> # A tibble: 2 × 4
#>   release_id level  syntax 
#>   <chr>      <chr>  <chr>  
#> 1 P1         metric F =~ x2
#> 2 P2         scalar x3 ~ 1 
#>   rationale                                                 
#>   <chr>                                                     
#> 1 Loading difference was theoretically anticipated.         
#> 2 Intercept difference was prespecified from prior evidence.
#> 
#> No release was selected automatically by nomologR.
```
