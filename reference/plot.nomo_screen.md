# Plot a nomo_screen audit

Visual diagnostics complement the numerical screening output. The
default evidence map integrates multiple diagnostic signals without
converting them into a pass/fail scale. Additional plot types display
item-rest relationships, inter-item correlations, response-category use,
or missingness.

## Usage

``` r
# S3 method for class 'nomo_screen'
plot(
  x,
  y = NULL,
  type = c("evidence", "item_rest", "interitem", "responses", "missingness"),
  items = NULL,
  show_values = TRUE,
  ...
)
```

## Arguments

- x:

  A `nomo_screen` object.

- y:

  Ignored; included for compatibility with the base
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) generic.

- type:

  Plot type: `"evidence"`, `"item_rest"`, `"interitem"`, `"responses"`,
  or `"missingness"`.

- items:

  Optional character vector of candidate items to display.

- show_values:

  Logical; add numerical labels where useful.

- ...:

  Additional arguments, currently ignored.

## Value

A `ggplot2` plot object.
