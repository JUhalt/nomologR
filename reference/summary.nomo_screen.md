# Summarize a nomo_screen audit

`summary.nomo_screen()` integrates the descriptive, relationship, and
decision-log evidence from
[`nomo_screen()`](https://juhalt.github.io/nomologR/reference/nomo_screen.md)
into an item-level review table. The resulting `attention` field is
deliberately phrased as `none`, `review`, or `concern`; it is not an
item-retention decision.

## Usage

``` r
# S3 method for class 'nomo_screen'
summary(object, ...)
```

## Arguments

- object:

  A `nomo_screen` object.

- ...:

  Additional arguments, currently ignored.

## Value

An object of class `summary_nomo_screen` containing an overview,
integrated item-review table, decision log, relationship-method note,
and guidance settings.
