# Extract report-ready evidence tables

`nomo_table()` returns compact tibbles intended for manuscripts, audit
appendices, teaching materials, and the future
[`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
workflow. It never rounds away the underlying object: full engine
results remain stored in the original `nomologR` object.

## Usage

``` r
nomo_table(x, ...)
```

## Arguments

- x:

  A supported `nomologR` result object.

- ...:

  Additional arguments passed to methods.

## Value

A tibble.
