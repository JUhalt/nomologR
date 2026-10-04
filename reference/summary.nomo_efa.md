# Summarize a guided exploratory factor analysis

Summarize a guided exploratory factor analysis

## Usage

``` r
# S3 method for class 'nomo_efa'
summary(object, ...)
```

## Arguments

- object:

  A `nomo_efa` object.

- ...:

  Unused.

## Value

An object of class `summary_nomo_efa`. Printing it shows the settings,
the supporting adequacy evidence, each item's loadings and communality
with the reason for every flag, the factor correlations, any problem
with the solution, and the largest residual correlations.
