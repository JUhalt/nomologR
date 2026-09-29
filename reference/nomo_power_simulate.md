# Monte Carlo power and sample size for a planned model

`nomo_power_simulate()` estimates, for each sample size, how often a
planned model recovers its parameters: whether it converges, whether the
solution is proper, how biased the estimates and their standard errors
are, how often the intervals cover the population values, and how often
each parameter is detected. This is Muthén and Muthén's (2002) Monte
Carlo approach.

## Usage

``` r
nomo_power_simulate(
  population,
  n,
  reps = 500,
  analysis = NULL,
  focus = NULL,
  alpha = 0.05,
  seed = NULL,
  standardized = TRUE
)
```

## Arguments

- population:

  A lavaan model with population values for its parameters.

- n:

  Sample sizes to simulate.

- reps:

  Replications per sample size. Default 500; Muthén and Muthén used
  10,000, and more replications give steadier estimates.

- analysis:

  Optional analysis model. By default, `population` with its numeric
  values removed.

- focus:

  Optional parameters whose power is of interest, written as in lavaan
  (`"A~~B"`, `"B~A"`, `"A=~a1"`). By default, every parameter with a
  nonzero population value.

- alpha:

  Significance level for detection. Default `.05`.

- seed:

  Optional integer seed, for a reproducible simulation. The session's
  random-number state is restored afterwards.

- standardized:

  Passed to
  [`lavaan::simulateData()`](https://rdrr.io/pkg/lavaan/man/simulateData.html).
  Default `TRUE`.

## Value

A `nomo_power` object. The fields to read are `parameters` (for each
sample size and parameter: population value, mean estimate, relative
bias, standard deviation of the estimates, mean standard error, standard
error bias, coverage, and power), `summary` (for each sample size: the
proportions that converged and that were improper, the smallest power in
`focus`, the largest absolute biases, the coverage range, and
`meets_references`), `n_required` (the smallest simulated sample size
that meets the references, or `NA`), `focus`, `reps`, and `alpha`.

## Details

Data are generated from `population`, a lavaan model whose parameters
carry their population values (for example `A =~ 0.7*a1`), with
[`lavaan::simulateData()`](https://rdrr.io/pkg/lavaan/man/simulateData.html).
With `standardized = TRUE`, the default, loadings and regressions are in
a standardized metric and the residual variances are set so the observed
variables have unit variance. The `analysis` model, which by default is
`population` without its values, is fitted to each data set with
`lavaan::sem(std.lv = TRUE)`.

Muthén and Muthén (2002) suggest choosing the sample size at which three
conditions hold, and power for the parameter of interest is close to
.80:

- parameter and standard error biases are within 10% for every
  parameter;

- the standard error bias of the parameter whose power is assessed is
  within 5%;

- coverage of the 95% interval lies between .91 and .98.

`meets_references` records whether they hold at each sample size, for
the parameters in `focus`. Wolf, Harrington, Clark, and Miller (2013)
showed that the sample size a model needs varies widely with its
structure, so rules of thumb such as a fixed N or a ratio of cases to
parameters are no substitute. They also counted improper solutions,
which are reported here. Summaries use the replications that converged.

## References

Muthén, L. K., & Muthén, B. O. (2002). How to use a Monte Carlo study to
decide on sample size and determine power. *Structural Equation
Modeling, 9*(4), 599-620.
[doi:10.1207/S15328007SEM0904_8](https://doi.org/10.1207/S15328007SEM0904_8)

Wolf, E. J., Harrington, K. M., Clark, S. L., & Miller, M. W. (2013).
Sample size requirements for structural equation models: An evaluation
of power, bias, and solution propriety. *Educational and Psychological
Measurement, 73*(6), 913-934.
[doi:10.1177/0013164413495237](https://doi.org/10.1177/0013164413495237)

## See also

[`nomo_power_rmsea()`](https://juhalt.github.io/nomologR/reference/nomo_power_rmsea.md)
for the power of the overall fit tests.

## Examples

``` r
# \donttest{
population <- "
  A =~ 0.7*a1 + 0.7*a2 + 0.6*a3 + 0.5*a4
  B =~ 0.7*b1 + 0.6*b2 + 0.6*b3 + 0.5*b4
  A ~~ 0.3*B
"
pw <- nomo_power_simulate(population, n = c(100, 200), reps = 100,
                          focus = "A~~B", seed = 2026)
pw
#> <nomo_power> Monte Carlo power and sample size
#> Replications: 100 per N | alpha: 0.05 | Focus: A~~B
#> 
#> By sample size
#>     N  Converged  Improper  Min power  Max bias  Max SE bias  Coverage   Meets
#>   100  100%       0%             0.64      0.03         0.13  0.90-0.99  no
#>   200  100%       0%             0.91      0.04         0.14  0.90-0.95  no
#>   No simulated N meets the references; try larger ones.
#> 
#> Biases are absolute and relative. References (Muthén & Muthén, 2002):
#> parameter and SE bias within 10%, SE bias within 5% for the focus parameters,
#> coverage .91-.98, and power .80 for the focus parameters. They are guides for
#> choosing N, not rules.
# }
```
