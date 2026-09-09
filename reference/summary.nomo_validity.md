# Summarize convergent and discriminant measurement evidence

Summarize convergent and discriminant measurement evidence

## Usage

``` r
# S3 method for class 'nomo_validity'
summary(object, ...)
```

## Arguments

- object:

  A `nomo_validity` object.

- ...:

  Unused.

## Value

An object of class `summary_nomo_validity`. `$convergent` combines AVE
with a compact loading summary, while `$discriminant` aligns latent
correlations, HTMT2, and original HTMT without repeating the full CFA
loading table.
