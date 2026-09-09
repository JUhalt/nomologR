# Specify directional or negligible theoretical expectations

These helpers create machine-readable theoretical expectations for use
with
[`nomo_hypotheses()`](https://juhalt.github.io/nomologR/reference/nomo_hypotheses.md).
They describe what theory predicts before the empirical network is
evaluated.

## Usage

``` r
positive(
  min = NULL,
  max = NULL,
  scale = c("standardized", "unstandardized"),
  origin = c("a_priori", "post_hoc")
)

negative(
  max = NULL,
  min = NULL,
  scale = c("standardized", "unstandardized"),
  origin = c("a_priori", "post_hoc")
)

negligible(
  within = NULL,
  scale = c("standardized", "unstandardized"),
  origin = c("a_priori", "post_hoc")
)
```

## Arguments

- min:

  Optional finite lower bound. For `positive()` it must be greater than
  zero. For `negative()` it must be less than zero.

- max:

  Optional finite upper bound. For `positive()` it must be greater than
  zero. For `negative()` it must be less than zero.

- scale:

  Scale on which the expectation is defined. Standardized coefficients
  are the default because magnitude expectations such as `.20` are
  otherwise not portable across arbitrary raw units.

- origin:

  Whether the expectation was specified `a_priori` or added `post_hoc`.
  Post-hoc expectations remain machine-readable but are never presented
  as confirmatory evidence.

- within:

  Optional length-two finite numeric vector defining the
  negligible-effect region. The interval must contain zero.

## Value

An object of class `nomo_expectation`.

## Details

`positive()` and `negative()` can express direction alone or add a
researcher-specified magnitude boundary. `negligible()` deliberately
distinguishes an unquantified null-like expectation from a quantitative
smallest effect size of interest (SESOI) region.

A bare `negligible()` expectation is **not confirmable as negligible**
from a non-significant p-value. Supply `within = c(lower, upper)` when
theory or the study design provides a defensible negligible-effect
region.

## Examples

``` r
positive()
#> $prediction
#> [1] "positive"
#> 
#> $lower
#> [1] 0
#> 
#> $upper
#> [1] Inf
#> 
#> $lower_inclusive
#> [1] FALSE
#> 
#> $upper_inclusive
#> [1] FALSE
#> 
#> $scale
#> [1] "standardized"
#> 
#> $origin
#> [1] "a_priori"
#> 
#> $magnitude_specified
#> [1] FALSE
#> 
#> $confirmable
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "nomo_expectation" "list"            
positive(min = .20)
#> $prediction
#> [1] "positive"
#> 
#> $lower
#> [1] 0.2
#> 
#> $upper
#> [1] Inf
#> 
#> $lower_inclusive
#> [1] TRUE
#> 
#> $upper_inclusive
#> [1] FALSE
#> 
#> $scale
#> [1] "standardized"
#> 
#> $origin
#> [1] "a_priori"
#> 
#> $magnitude_specified
#> [1] TRUE
#> 
#> $confirmable
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "nomo_expectation" "list"            
negative(max = -.20)
#> $prediction
#> [1] "negative"
#> 
#> $lower
#> [1] -Inf
#> 
#> $upper
#> [1] -0.2
#> 
#> $lower_inclusive
#> [1] FALSE
#> 
#> $upper_inclusive
#> [1] TRUE
#> 
#> $scale
#> [1] "standardized"
#> 
#> $origin
#> [1] "a_priori"
#> 
#> $magnitude_specified
#> [1] TRUE
#> 
#> $confirmable
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "nomo_expectation" "list"            
negligible()
#> $prediction
#> [1] "negligible"
#> 
#> $lower
#> [1] NA
#> 
#> $upper
#> [1] NA
#> 
#> $lower_inclusive
#> [1] NA
#> 
#> $upper_inclusive
#> [1] NA
#> 
#> $scale
#> [1] "standardized"
#> 
#> $origin
#> [1] "a_priori"
#> 
#> $magnitude_specified
#> [1] FALSE
#> 
#> $confirmable
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "nomo_expectation" "list"            
negligible(within = c(-.10, .10))
#> $prediction
#> [1] "negligible"
#> 
#> $lower
#> [1] -0.1
#> 
#> $upper
#> [1] 0.1
#> 
#> $lower_inclusive
#> [1] TRUE
#> 
#> $upper_inclusive
#> [1] TRUE
#> 
#> $scale
#> [1] "standardized"
#> 
#> $origin
#> [1] "a_priori"
#> 
#> $magnitude_specified
#> [1] TRUE
#> 
#> $confirmable
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "nomo_expectation" "list"            
```
