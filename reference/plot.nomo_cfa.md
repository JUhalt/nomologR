# Plot confirmatory factor-analysis evidence

Plot confirmatory factor-analysis evidence

## Usage

``` r
# S3 method for class 'nomo_cfa'
plot(x, type = c("loadings", "fit", "residuals", "modification_indices"), ...)
```

## Arguments

- x:

  A `nomo_cfa` object.

- type:

  Plot type: `"loadings"`, `"fit"`, `"residuals"`, or
  `"modification_indices"`.

- ...:

  Unused.

## Value

A `ggplot2` object. Flags are drawn with the package's status shapes and
colors (filled circle for no flag, open circle for review, filled square
for concern), with a legend whenever a flag is drawn.
