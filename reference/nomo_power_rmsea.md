# Power of the RMSEA tests of model fit

`nomo_power_rmsea()` gives the power of MacCallum, Browne, and
Sugawara's (1996) RMSEA-based tests of fit at given sample sizes, or the
smallest sample size that reaches a target power.

## Usage

``` r
nomo_power_rmsea(
  model = NULL,
  n = NULL,
  df = NULL,
  power = 0.8,
  test = c("close", "not_close", "exact"),
  rmsea_null = NULL,
  rmsea_alt = NULL,
  alpha = 0.05
)
```

## Arguments

- model:

  The planned model: a lavaan model string, a
  [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md)
  object, a fitted
  [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
  result, or a `lavaan` fit. Its degrees of freedom are used. Give
  `model` or `df`, not both. A multi-group fit is refused (see Details).

- n:

  Optional sample sizes at which to compute power.

- df:

  The model's degrees of freedom, instead of `model`.

- power:

  Target power for the smallest sample size. Default `.80`.

- test:

  `"close"` (default), `"not_close"`, or `"exact"`.

- rmsea_null, rmsea_alt:

  Optional null and alternative RMSEA values, replacing the test's
  defaults. The null is 0 for `"exact"` and above 0 for the other tests.

- alpha:

  Significance level. Default `.05`.

## Value

A `nomo_power` object. The fields to read are `test`, `df`,
`rmsea_null`, `rmsea_alt`, `alpha`, `target_power`, `power` (a table of
sample sizes and their power), and `n_required` (the smallest sample
size reaching `target_power`, or `NA` if none up to one million does).
[`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md)
returns the `"power"` table, its one `type`.
[`print()`](https://rdrr.io/r/base/print.html) shows the test, its RMSEA
values, and the power at each sample size;
[`summary()`](https://rdrr.io/r/base/summary.html) shows the same.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws the power
curve with the target power and the smallest sample size that reaches
it.

Other fields record the kind of power analysis. They may change between
releases and are not part of the stable interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

## Details

MacCallum et al. (1996) framed the power of a covariance structure
model's overall test in terms of RMSEA, whose noncentral chi-square
distribution has noncentrality \\(N - 1) \\ df \\ \varepsilon^2\\. Three
tests are available:

- `"close"`: the test of close fit, with null RMSEA .05 and alternative
  .08 by default. Power is the chance of rejecting close fit when the
  fit is in fact mediocre.

- `"not_close"`: the test of not-close fit, with null .05 and
  alternative .01. Power is the chance of rejecting not-close fit when
  the fit is in fact close, which is what supports a claim of good fit.

- `"exact"`: the test of exact fit, with null 0 and alternative .05.

Each test is named by its null hypothesis, so a `rmsea_null` that
contradicts it is refused: the test of exact fit has a null of 0, and
the tests of close and not-close fit a null above 0.

Power depends on the degrees of freedom: a model with few of them needs
a large sample to test its fit. This is power for the model's overall
fit test, not for any one parameter;
[`nomo_power_simulate()`](https://juhalt.github.io/nomologR/reference/nomo_power_simulate.md)
gives that.

The calculation is for a single-group model. A multi-group `lavaan` fit
is refused, because lavaan's RMSEA for several groups carries a factor
of the square root of their number, and the noncentrality above would
overstate the power for the RMSEA lavaan reports.

## References

MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power
analysis and determination of sample size for covariance structure
modeling. *Psychological Methods, 1*(2), 130-149.
[doi:10.1037/1082-989X.1.2.130](https://doi.org/10.1037/1082-989X.1.2.130)

## See also

[`nomo_power_simulate()`](https://juhalt.github.io/nomologR/reference/nomo_power_simulate.md)
for the power to detect particular parameters.

## Examples

``` r
model <- nomo_model(list(A = paste0("a", 1:4), B = paste0("b", 1:4)))
nomo_power_rmsea(model)
#> <nomo_power> Power of the test of close fit
#> MacCallum, Browne, and Sugawara (1996).
#> df: 19 | RMSEA: null 0.050, alternative 0.080 | alpha = .05
#> Smallest N for power .80: 453
#> 
#> Power by sample size
#>     N  Power
#>   453   .801
#> 
#> RMSEA = root mean square error of approximation; df = degrees of freedom;
#> N = sample size.
#> 
#> This is power for the overall fit test, not for any one parameter;
#> nomo_power_simulate() gives that.
#> 
#> See nomo_table(x, "power") for the power at each sample size.
nomo_power_rmsea(model, n = c(100, 200, 400), test = "not_close")
#> <nomo_power> Power of the test of not-close fit
#> MacCallum, Browne, and Sugawara (1996).
#> df: 19 | RMSEA: null 0.050, alternative 0.010 | alpha = .05
#> Smallest N for power .80: 490
#> 
#> Power by sample size
#>     N  Power
#>   100    .14
#>   200    .30
#>   400    .67
#> 
#> RMSEA = root mean square error of approximation; df = degrees of freedom;
#> N = sample size.
#> 
#> This is power for the overall fit test, not for any one parameter;
#> nomo_power_simulate() gives that.
#> 
#> See nomo_table(x, "power") for the power at each sample size.
```
