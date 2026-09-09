# Build simple confirmatory factor-analysis syntax

`nomo_model()` is a small convenience helper for ordinary reflective CFA
models. It converts a named list of factor-to-indicator assignments into
`lavaan` measurement-model syntax. It deliberately does not add residual
covariances, cross-loadings, equality constraints, or other post-hoc
changes.

## Usage

``` r
nomo_model(factors)
```

## Arguments

- factors:

  A named list. Each element name is a latent-factor name and each
  element value is a character vector of observed indicators.

## Value

A character scalar of class `nomo_model` that can be passed directly to
[`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
or to [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

## Examples

``` r
model <- nomo_model(list(
  engagement = c("e1", "e2", "e3"),
  belonging = c("b1", "b2", "b3")
))
model
#> engagement =~ e1 + e2 + e3
#> belonging =~ b1 + b2 + b3
```
