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
  `model` or `df`, not both.

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
  defaults.

- alpha:

  Significance level. Default `.05`.

## Value

A `nomo_power` object. The fields to read are `test`, `df`,
`rmsea_null`, `rmsea_alt`, `alpha`, `target_power`, `power` (a table of
sample sizes and their power), and `n_required` (the smallest sample
size reaching `target_power`, or `NA` if none up to one million does).

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

Power depends on the degrees of freedom: a model with few of them needs
a large sample to test its fit. This is power for the model's overall
fit test, not for any one parameter;
[`nomo_power_simulate()`](https://juhalt.github.io/nomologR/reference/nomo_power_simulate.md)
gives that.

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
#> df: 19 | RMSEA: null 0.05, alternative 0.08 | alpha: 0.05
#>   Smallest N for power 0.8: 453.
#> 
#> Power by sample size
#>     N  Power
#>   453  0.801
#> 
#> MacCallum, Browne, & Sugawara (1996). This is power for the overall fit test,
#> not for any one parameter; nomo_power_simulate() gives that.
nomo_power_rmsea(model, n = c(100, 200, 400), test = "not_close")
#> <nomo_power> Power of the test of not-close fit
#> df: 19 | RMSEA: null 0.05, alternative 0.01 | alpha: 0.05
#>   Smallest N for power 0.8: 490.
#> 
#> Power by sample size
#>     N  Power
#>   100  0.144
#>   200  0.302
#>   400  0.674
#> 
#> MacCallum, Browne, & Sugawara (1996). This is power for the overall fit test,
#> not for any one parameter; nomo_power_simulate() gives that.
```
