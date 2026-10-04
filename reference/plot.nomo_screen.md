# Plot a nomo_screen audit

Visual diagnostics complement the numerical screening output. The
default evidence map integrates multiple diagnostic signals without
converting them into a pass/fail scale. Additional plot types display
item-rest relationships, inter-item correlations, response-category use,
or missingness. The evidence map and the item-rest plot show each flag
by shape, with color repeating it: a filled circle for no flag, an open
circle for review, a filled square for concern, and a cross where a
diagnostic was not computed.

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
  or `"missingness"`. `"responses"` draws a bar for each response
  category of a categorical item, and a histogram of observed values for
  a continuous item (`item_type` `"numeric_continuous"`). When the
  selected items mix the two, the categorical items are drawn and the
  caption names the continuous ones, which can be plotted by passing
  them as `items`.

- items:

  Optional character vector of candidate items to display.

- show_values:

  Logical; add numerical labels where useful.

- ...:

  Additional arguments, currently ignored.

## Value

A `ggplot2` plot object.
