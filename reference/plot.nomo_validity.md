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

  Plot type: `"ave"` or `"discriminant"`. The discriminant plot draws
  each pair at the value that sets its flag: HTMT2, HTMT when HTMT2 was
  not computed, and the latent correlation with its interval when
  neither was.

- ...:

  Unused.

## Value

A `ggplot2` object. The conceptual 0-to-1 coefficient range is shown by
default and expands only if an empirical value lies outside it. Each
point's shape and color show its flag, the same flag as in
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md),
with a legend whenever a point is flagged.
