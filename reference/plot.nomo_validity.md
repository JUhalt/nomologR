# Plot convergent or discriminant validity evidence

Plot convergent or discriminant validity evidence

## Usage

``` r
# S3 method for class 'nomo_validity'
plot(x, type = c("ave", "discriminant"), ...)
```

## Arguments

- x:

  A `nomo_validity` object.

- type:

  Plot type: `"ave"` or `"discriminant"`.

- ...:

  Unused.

## Value

A `ggplot2` object. The conceptual 0-to-1 coefficient range is shown by
default and expands only if an empirical value lies outside it.
