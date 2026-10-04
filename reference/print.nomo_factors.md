# Print factor-retention evidence

[`print()`](https://rdrr.io/r/base/print.html) shows the count each main
criterion suggests, the sampling adequacy, and the synthesis;
[`summary()`](https://rdrr.io/r/base/summary.html) adds the evidence by
method, the parallel-analysis rule sensitivity, the criteria that did
not run and why, and the concordance across criterion families.

## Usage

``` r
# S3 method for class 'nomo_factors'
print(x, ...)
```

## Arguments

- x:

  A `nomo_factors` object.

- ...:

  Additional arguments, currently ignored.

## Value

`x`, invisibly.
