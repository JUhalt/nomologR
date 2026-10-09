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

  Integer seed used, with the random-number generator recorded in
  `rng_kind`, to make the split reproducible.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

A `nomo_split` object. The fields to read are:

- `calibration` and `validation`: the two samples.

- `assignment`: which sample each row went to.

- `n_total`, `n_calibration`, `n_validation`, and
  `validation_prop_realized`.

- `seed`, `rng_kind`, and `decision_log`. `rng_kind` is the
  random-number generator, normal, and sample kinds of
  [`RNGkind()`](https://rdrr.io/r/base/Random.html) that drew the split.

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

[`print()`](https://rdrr.io/r/base/print.html) shows the sizes of the
two samples, the proportion requested and realized, the seed, and the
random-number generator.

## Details

The caller's random-number-generator state is restored after the split
so that using `nomo_split()` does not silently alter later stochastic
analyses.

The split depends on the seed and on the random-number generator in use,
which `rng_kind` records: the same seed under another
[`RNGkind()`](https://rdrr.io/r/base/Random.html), such as
`"L'Ecuyer-CMRG"` or `sample.kind = "Rounding"`, gives a different
split. To reproduce a split, set the recorded kinds with
[`RNGkind()`](https://rdrr.io/r/base/Random.html) before calling
`nomo_split()` with the same seed, or keep `assignment`.

## References

Fokkema, M., & Greiff, S. (2017). How performing PCA and CFA on the same
data equals trouble: Overfitting in the assessment of internal structure
and some editorial thoughts on it. *European Journal of Psychological
Assessment, 33*(6), 399-402.
[doi:10.1027/1015-5759/a000460](https://doi.org/10.1027/1015-5759/a000460)

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490-504.
[doi:10.1037/0033-2909.111.3.490](https://doi.org/10.1037/0033-2909.111.3.490)

## Examples

``` r
split <- nomo_split(nomo_demo_continuous, validation_prop = 0.40, seed = 2026)
split
#> <nomo_split> Calibration and validation split
#> Rows: 500 | Calibration: 300 | Validation: 200
#> Validation proportion: .40 requested, .40 realized | Seed: 2026
#> Generator: Mersenne-Twister, Inversion, Rejection
#> 
#> Use splitting only when the gain in independence justifies the loss of
#> precision.
#> 
#> See x$assignment for the sample each row went to.
nrow(split$calibration)
#> [1] 300
nrow(split$validation)
#> [1] 200
```
