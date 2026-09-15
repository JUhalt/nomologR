# Plot a measurement-model comparison

Plot a measurement-model comparison

## Usage

``` r
# S3 method for class 'nomo_compare'
plot(x, type = c("fit", "loadings"), ...)
```

## Arguments

- x:

  A `nomo_compare` object.

- type:

  `"fit"` shows CFI, TLI, RMSEA, and SRMR for each model with the
  configured teaching references; `"loadings"` shows standardized
  loadings for each model side by side.

- ...:

  Unused.

## Value

A `ggplot2` object.
