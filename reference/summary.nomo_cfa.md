# Summarize a guided confirmatory factor analysis

Summarize a guided confirmatory factor analysis

## Usage

``` r
# S3 method for class 'nomo_cfa'
summary(object, ...)
```

## Arguments

- object:

  A `nomo_cfa` object.

- ...:

  Unused.

## Value

An object of class `summary_nomo_cfa`. Printing it shows the chi-square
test and fit indices with their references, the standardized loadings,
the factor correlations, any improper solution, the largest residual
correlations and modification indices, and each flag with its
explanation.
