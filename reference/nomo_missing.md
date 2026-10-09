# Missing-data sensitivity: does a result depend on how missing data were handled?

`nomo_missing()` refits the same prespecified model under alternative
missing-data strategies. It reports whether the cases used, the
estimates, fit, reliability, or theory evidence change. It does not
choose a strategy and does not change the model.

## Usage

``` r
nomo_missing(fit, data, strategies = NULL, ...)

# S3 method for class 'nomo_cfa'
nomo_missing(fit, data, strategies = NULL, reliability = TRUE, ...)

# S3 method for class 'nomo_network'
nomo_missing(fit, data, strategies = NULL, ...)
```

## Arguments

- fit:

  A `nomo_cfa` or `nomo_network` object.

- data:

  The data the model was fitted to, including any cases listwise
  deletion removed. For a network fitted to a `nomo_split`, supply the
  same `nomo_split`; the comparison uses its calibration sample. `data`
  is checked by refitting the original strategy, which must reproduce
  the fitted model.

- strategies:

  Optional character vector of lavaan `missing` options to compare. The
  default compares listwise deletion with FIML for continuous
  indicators, and with pairwise deletion for ordered indicators.
  lavaan's aliases `"fiml"` and `"direct"` are treated as `"ml"`.

- ...:

  Unused.

- reliability:

  For a `nomo_cfa`, whether to compare reliability across strategies
  with
  [`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md).
  Default `TRUE`. Reliability for ordered indicators is slow to compute,
  so `FALSE` is useful when only the estimates are of interest.

## Value

A `nomo_missing` object. The fields to read are:

- `reference` and `fitted_as`: the strategy the others are compared
  with, and the strategy the supplied model was fitted with, each named
  by its lavaan `missing` option, such as `"ml"` or `"listwise"`.

- `pattern`: missingness in the modeled variables. `pct_incomplete` is a
  proportion, from 0 to 1.

- `variables`: missing values per variable. `pct_missing` is a
  proportion.

- `strategies`: one row per strategy.

- `fit`: fit indices by strategy.

- `estimates`: standardized loadings and factor correlations for a
  `nomo_cfa`, or hypothesis estimates for a `nomo_network`, compared
  with the reference.

- `reliability`: for a `nomo_cfa`, reliability by strategy, unless
  `reliability = FALSE`.

- `decision_log`.

Other fields record the call, the class of the model compared
(`object`), whether its indicators are ordered (`ordered`), and the
refitted models (`fits`). They may change between releases and are not
part of the stable interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

`pct_incomplete` in `pattern` and `pct_missing` in `variables` are
proportions between 0 and 1, not percentages. See **Conventions in
returned tables** in
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md).

## Details

**Which strategies.** For continuous indicators, listwise deletion is
compared with full-information maximum likelihood (FIML), and FIML is
the reference. lavaan does not offer FIML for ordered indicators
estimated with WLSMV. For those, listwise deletion is compared with
pairwise deletion, and pairwise deletion is the reference. The strategy
the model was fitted with is always included.

Each strategy records what was requested and what lavaan actually used,
because the two can differ, and differently across lavaan versions. When
`missing = "ml"` is requested with ULS, lavaan 0.7 runs its two-stage
method instead, as it does with GLS, while lavaan 0.6-21 refuses; with
MLM it is refused. A refused strategy is reported as unavailable, with
lavaan's reason. If the reference itself cannot be fitted, another
strategy that was fitted becomes the reference, and the decision log
says so.

**What each strategy assumes.** Listwise and pairwise deletion require
data missing completely at random (MCAR). FIML requires data missing at
random (MAR). Enders and Bandalos (2001) found all three unbiased under
MCAR, with FIML the most efficient. Under MAR, FIML remained unbiased,
while listwise and pairwise deletion were biased. FIML also assumes
multivariate normality, as complete-data maximum likelihood does.

**What a comparison cannot show.** Whether data are MAR cannot in
general be tested from the data at hand (Schafer & Graham, 2002).
Agreement between strategies shows that a result does not depend on the
choice between them. It does not show that either strategy is unbiased;
neither is guaranteed to be when data are missing not at random.

**When a difference is flagged.** Each estimate is compared with the
reference strategy's estimate and expressed in units of the reference
standard error. Schafer and Graham (2002) treat a bias larger than about
half a standard error as practically important. Beyond that size it
noticeably degrades the coverage of confidence intervals. A difference
of that size is flagged for review, with three qualifications:

- The difference estimates listwise deletion's bias only if the data are
  MAR and the model is correct, since only then is FIML consistent. With
  ordered indicators, both strategies require MCAR, so a difference
  cannot be attributed to either one.

- The strategies analyze different cases, so part of any difference is
  sampling variability.

- Schafer and Graham's half a standard error judges a bias, measured
  over simulated samples. Here it is applied to a difference within one
  sample, so under MCAR, where listwise deletion is unbiased, sampling
  variability alone can exceed it, and does so more often the more cases
  are incomplete. A flag marks a difference to review, not a bias found.

A hypothesis whose concordance with its prediction differs between
strategies is also flagged for review.

**The flagging rule** is adapted from Schafer and Graham (2002, p. 157):
a bias beyond about half a standard error is practically important,
because it degrades interval coverage. Separating sampling variability
from bias would be new output, added alongside this rule rather than
replacing it.

**What lavaan estimated.** A strategy is labeled by the method lavaan
used. When lavaan substitutes one method for another, as it runs
two-stage ML when FIML is requested with ULS, the label names both, such
as "Two-stage ML (requested FIML)", and a difference from it is
attributed to no strategy.

[`print()`](https://rdrr.io/r/base/print.html) shows the missingness,
the strategies compared, the largest differences from the reference, and
each flag's observation;
[`summary()`](https://rdrr.io/r/base/summary.html) adds missing values
by variable, fit and reliability by strategy, every difference, and each
recorded decision with its recommendation. With more than one comparison
strategy, each strategy's differences and coefficients are a table of
their own. [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
draws each difference from the reference in reference standard errors.

**Not implemented.** Mean substitution is not offered. It understates
variances and distorts covariances. Schafer and Graham (2002) show that
even under MCAR it narrows confidence intervals below their nominal
coverage. Multiple imputation and models for data missing not at random
are outside this function.

## References

Enders, C. K., & Bandalos, D. L. (2001). The relative performance of
full information maximum likelihood estimation for missing data in
structural equation models. *Structural Equation Modeling, 8*(3),
430-457.
[doi:10.1207/S15328007SEM0803_5](https://doi.org/10.1207/S15328007SEM0803_5)

Schafer, J. L., & Graham, J. W. (2002). Missing data: Our view of the
state of the art. *Psychological Methods, 7*(2), 147-177.
[doi:10.1037/1082-989X.7.2.147](https://doi.org/10.1037/1082-989X.7.2.147)

## See also

[`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md),
[`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md),
and
[`nomo_screen()`](https://juhalt.github.io/nomologR/reference/nomo_screen.md)
for describing missingness before a model is fitted.

## Examples

``` r
model <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
fit <- nomo_cfa(model, data = nomo_demo_continuous)

sensitivity <- nomo_missing(fit, data = nomo_demo_continuous, reliability = FALSE)
sensitivity
#> <nomo_missing> Missing-data sensitivity
#> Model: CFA | Reference: FIML | Fitted with: Listwise deletion
#> Cases: 27 of 500 incomplete (5.4%) | Patterns: 3
#> Lowest covariance coverage: .95 (a2, b3)
#> 
#> Strategies
#>   Strategy           lavaan    Needs  Role          N  Converged  Admissible
#>   Listwise deletion  listwise  MCAR   comparison  473  yes        yes
#>   FIML               ml        MAR    reference   500  yes        yes
#> 
#> Largest differences from the reference (standardized estimates)
#>   Parameter  Strategy           Estimate  Reference  Difference (SE)
#>   A ~~ B     Listwise deletion      .499       .480            +0.45
#>   B =~ b5    Listwise deletion     0.337      0.353            -0.37
#>   A =~ a5    Listwise deletion     0.598      0.590            +0.23
#>   A =~ a3    Listwise deletion     0.669      0.675            -0.21
#>   B =~ b3    Listwise deletion     0.757      0.762            -0.17
#> 
#> What these terms mean
#>   CFA -- Confirmatory factor analysis.
#>   FIML -- Full-information maximum likelihood.
#>   Needs -- What the strategy requires of the missing data: MCAR, missing
#>       completely at random, or MAR, missing at random; -- where no requirement
#>       is stated for the method lavaan used.
#>   N -- Cases the strategy analyzed.
#>   Covariance coverage -- Proportion of cases with both variables of a pair
#>       observed; the lowest pair is shown.
#>   Difference (SE) -- The estimate minus the reference strategy's estimate, in
#>       the reference standard error (SE); beyond half of one is flagged for
#>       review.
#> 
#> Whether data are missing at random cannot be tested from these data, and
#> agreement between strategies does not show that either is unbiased.
#> 
#> See summary(x) for every difference and each recommendation and
#> nomo_table(x, "decision_log") for every recorded decision.
nomo_table(sensitivity, "strategies")
#> # A tibble: 2 × 12
#>   strategy label     lavaan_missing requires role  as_fitted available converged
#>   <chr>    <chr>     <chr>          <chr>    <chr> <lgl>     <lgl>     <lgl>    
#> 1 listwise Listwise… listwise       MCAR     comp… TRUE      TRUE      TRUE     
#> 2 ml       FIML      ml             MAR      refe… FALSE     TRUE      TRUE     
#> # ℹ 4 more variables: admissible <lgl>, n_used <dbl>, n_discarded <dbl>,
#> #   note <chr>
```
