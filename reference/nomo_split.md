# Create a reproducible calibration/validation split

`nomo_split()` creates an explicit random split for workflows in which
EFA and CFA should be evaluated on different observations when the
available sample permits it. The function does not claim that sample
splitting is always preferable: dividing a modest sample reduces
precision in both subsets, and an external validation sample is
generally stronger evidence when one is available.

## Usage

``` r
nomo_split(
  data,
  validation_prop = 0.5,
  seed = 1234L,
  guidance = nomo_defaults()
)
```

## Arguments

- data:

  A non-empty data frame.

- validation_prop:

  Proportion of rows assigned to the validation sample. Must be strictly
  between 0 and 1.

- seed:

  Integer seed used to make the split reproducible.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

A `nomo_split` object containing `calibration`, `validation`, a
row-level `assignment` table, split sizes, the seed, and a decision log.

## Details

The caller's random-number-generator state is restored after the split
so that using `nomo_split()` does not silently alter later stochastic
analyses.

## Examples

``` r
split <- nomo_split(mtcars, validation_prop = 0.40, seed = 2026)
split
#> <nomo_split>
#> Rows: 32 total | 19 calibration | 13 validation
#> Validation proportion: 0.400 requested | 0.406 realized | Seed: 2026
#> Use splitting only when the gain in independence justifies the loss of precision.
nrow(split$calibration)
#> [1] 19
nrow(split$validation)
#> [1] 13
```
