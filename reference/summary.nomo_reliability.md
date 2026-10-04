# Summarize model-based reliability evidence

Summarize model-based reliability evidence

## Usage

``` r
# S3 method for class 'nomo_reliability'
summary(object, ...)
```

## Arguments

- object:

  A `nomo_reliability` object.

- ...:

  Unused.

## Value

An object of class `summary_nomo_reliability`. Printed, it shows each
construct's omega and alpha with their intervals, score scale, and flag,
why any alpha was not computed, and the reason for each flag in the
decision log, including those that qualify the coefficients, such as a
theta parameterization or intervals that could not be computed.
