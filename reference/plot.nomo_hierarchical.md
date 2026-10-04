# Plot hierarchical measurement evidence

Plot hierarchical measurement evidence

## Usage

``` r
# S3 method for class 'nomo_hierarchical'
plot(x, type = c("variance", "loadings"), ...)
```

## Arguments

- x:

  A `nomo_hierarchical` object.

- type:

  `"variance"` (default) shows, for the total score and each subscale
  composite, how its variance divides among the general factor, the
  group factor, and everything else. `"loadings"` shows each item's
  standardized general and group loadings.

- ...:

  Unused.

## Value

A `ggplot` object. A composite whose shares rest on a negative variance
estimate is left out of the variance plot, and a loading that rests on
one is left out of the loadings plot; the caption names what was left
out.
