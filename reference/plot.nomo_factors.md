# Plot factor-retention evidence

Plot factor-retention evidence

## Usage

``` r
# S3 method for class 'nomo_factors'
plot(
  x,
  y = NULL,
  type = c("retention", "parallel_rules", "scree", "map", "evidence", "concordance",
    "kmo"),
  show_values = TRUE,
  ...
)
```

## Arguments

- x:

  A `nomo_factors` object.

- y:

  Unused; included for the base
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) generic.

- type:

  Plot type. `"retention"` compares observed common-factor eigenvalues
  with the selected parallel-analysis reference; `"parallel_rules"`
  compares PA factor counts under mean, percentile, and Crawford rules;
  `"scree"` displays component and common-factor eigenvalues; `"map"`
  displays original TR2 and revised TR4 MAP curves; `"evidence"`
  compares available retention criteria; `"concordance"` groups related
  variants into criterion families before showing support for each
  factor count; and `"kmo"` displays item-level KMO/MSA values.

- show_values:

  Logical; label values where useful.

- ...:

  Additional arguments, currently ignored.

## Value

A `ggplot` object.
