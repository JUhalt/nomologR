# Plot model-based reliability evidence

Plot model-based reliability evidence

## Usage

``` r
# S3 method for class 'nomo_reliability'
plot(x, type = "coefficients", ...)
```

## Arguments

- x:

  A `nomo_reliability` object.

- type:

  Plot type. `"coefficients"`, the only one, shows omega and alpha for
  each construct.

- ...:

  Unused.

## Value

A `ggplot2` object with one row per construct and coefficient, omega
drawn larger than alpha. Each point's shape and color show its flag,
with a legend whenever a coefficient is flagged. The conceptual 0-to-1
reliability range is shown by default and expands only if an estimate or
interval lies outside it.
