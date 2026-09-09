# Default guidance settings for nomologR

The numerical values returned by this function are teaching/reference
points, not universal pass/fail criteria. They are intended to trigger
inspection and explanation in downstream `nomologR` functions.

## Usage

``` r
nomo_defaults(profile = "teaching")
```

## Arguments

- profile:

  Guidance profile. Only `"teaching"` is implemented during the initial
  development series.

## Value

A named list of guidance settings.
