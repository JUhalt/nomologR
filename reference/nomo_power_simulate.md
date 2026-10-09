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
  random-number state is restored afterwards. A seed reproduces the same
  samples only under the same version of lavaan:
  [`lavaan::simulateData()`](https://rdrr.io/pkg/lavaan/man/simulateData.html)
  changed its default generator in lavaan 0.7-3, so the same seed draws
  different samples, and gives slightly different estimates of power,
  bias, and coverage, before and after that version.

- standardized:

  Passed to
  [`lavaan::simulateData()`](https://rdrr.io/pkg/lavaan/man/simulateData.html).
  Default `TRUE`, which gives the observed variables unit variance where
  the population values allow it (see Details).

## Value

A `nomo_power` object. The fields to read are `parameters` (for each
sample size and parameter: population value, mean estimate, relative
bias, standard deviation of the estimates, mean standard error, standard
error bias, coverage, and power), `summary` (for each sample size: the
proportions that converged and that were improper, the smallest power in
`focus`, the largest absolute biases, the coverage range, and
`meets_references`), `n_required` (the smallest simulated sample size
that meets the references, or `NA`), `focus`, `reps`, `alpha`, and
`seed` (the seed given, or `NA` without one).
[`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md)
returns the `"summary"` table, its default `type`, or the `"parameters"`
table. [`print()`](https://rdrr.io/r/base/print.html) shows the table by
sample size with a key to its columns and the references;
[`summary()`](https://rdrr.io/r/base/summary.html) adds the table by
sample size and parameter.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws the power
of each focus parameter by sample size.

Other fields record the call, the population and analysis models, and
the kind of power analysis. They may change between releases and are not
part of the stable interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

## Details

**Experimental.** This function is experimental and remains so after
nomologR 1.0.0. The metric of its estimates and its summaries may be
refined during 1.x, without a deprecation period, as it is extended
beyond complete continuous data. Any change will be described in NEWS;
see the package help page,
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md),
for the stability policy.

Data are generated from `population`, a lavaan model whose parameters
carry their population values (for example `A =~ 0.7*a1`), with
[`lavaan::simulateData()`](https://rdrr.io/pkg/lavaan/man/simulateData.html).
With `standardized = TRUE`, the default, the residual variances of the
observed variables are set so that each has unit variance. Values that
explain more than a variable's whole variance leave no residual variance
to set, and lavaan then generates the data in another metric; a warning
names the variances `population` implies when any is not

1.  The `analysis` model, which by default is `population` without its
    values, is fitted to each data set with
    `lavaan::sem(std.lv = TRUE)`.

**The metric.** Each estimate is the analysis model's unstandardized
estimate, and it is compared with the population value as written. Both
are in the metric that `std.lv = TRUE` sets: an exogenous factor has
variance 1, and an endogenous factor has residual variance 1. With the
default `standardized = TRUE`, loadings on exogenous factors are
therefore standardized, and covariances between exogenous factors are
correlations. A latent regression is not standardized. In `B ~ 0.4*A`,
B's total variance is 1.16, its residual variance of 1 plus 0.16 from A,
so the completely standardized coefficient is 0.4 divided by the square
root of 1.16, or .37, and B's indicators have standardized loadings 1.08
times their values. Bias, coverage, and power are for the values as
written.

Muthén and Muthén (2002) suggest choosing the sample size at which three
conditions hold, and power for the parameter of interest is close to
.80:

- parameter and standard error biases are within 10% for every parameter
  given a population value;

- the standard error bias of the parameter whose power is assessed is
  within 5%;

- coverage of the 95% interval lies between .91 and .98.

`meets_references` records whether they hold at each sample size, for
the parameters in `focus`. Only parameters given a population value in
`population` (loadings, regressions, and covariances) are checked, so
residual and factor variances are outside the check.

Wolf, Harrington, Clark, and Miller (2013) showed that the sample size a
model needs varies widely with its structure, so rules of thumb such as
a fixed N or a ratio of cases to parameters are no substitute. They also
counted improper solutions, which are reported here. Summaries use the
replications that converged.

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
#> Muthén and Muthén (2002).
#> Replications: 100 per N | alpha = .05 | Focus: A~~B
#> 
#> By sample size
#>     N  Meets  Converged  Improper  Min power  Max bias  Max SE bias  Coverage
#>   100  no        100.0%      0.0%        .72      6.9%        10.7%  .92 to .97
#>   200  no        100.0%      0.0%        .84      7.0%        12.2%  .90 to .97
#>   No simulated N meets the references; try larger ones.
#> 
#> What these columns mean
#>   N -- Sample size.
#>   Meets -- Whether the references below hold at this N.
#>   Converged, Improper -- Share of the replications that converged, and of
#>       those, the share with an improper solution.
#>   Min power -- Lowest power among the focus parameters.
#>   Max bias, Max SE bias -- Largest absolute relative bias of an estimate, and
#>       of its standard error (SE), across the parameters given a population
#>       value.
#>   Coverage -- Lowest to highest proportion of 95% confidence intervals that
#>       contain the population value, across those parameters.
#> 
#> References: parameter and SE bias within 10% for every parameter given a
#> population value, SE bias within 5% for the focus parameters, coverage
#> .91 to .98, and power .80 for the focus parameters. They are guides for
#> choosing N, not rules.
#> 
#> See summary(x) for every parameter at every N and nomo_table(x, "summary") for
#> the table by sample size.
# }
```
