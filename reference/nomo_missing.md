# Missing-data sensitivity: does a result depend on how missing data were handled?

`nomo_missing()` refits the same prespecified model under alternative
missing-data strategies. It reports whether the cases used, the
estimates, fit, reliability, or theory evidence change. It does not
choose a strategy and does not change the model.

## Usage

``` r
nomo_missing(x, data, strategies = NULL, ...)

# S3 method for class 'nomo_cfa'
nomo_missing(x, data, strategies = NULL, reliability = TRUE, ...)

# S3 method for class 'nomo_network'
nomo_missing(x, data, strategies = NULL, ...)
```

## Arguments

- x:

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

A `nomo_missing` object containing:

- `pattern`: missingness in the modeled variables.

- `variables`: missing values per variable.

- `strategies`: one row per strategy.

- `fit`: fit indices by strategy.

- `estimates`: standardized loadings and factor correlations for a
  `nomo_cfa`, or hypothesis estimates for a `nomo_network`, compared
  with the reference.

- `reliability`: for a `nomo_cfa`, reliability by strategy, unless
  `reliability = FALSE`.

- `decision_log`.

- `fits`: the refitted objects.

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
of that size is flagged for review, with two qualifications:

- The difference estimates listwise deletion's bias only if the data are
  MAR and the model is correct, since only then is FIML consistent. With
  ordered indicators, both strategies require MCAR, so a difference
  cannot be attributed to either one.

- The strategies analyze different cases, so part of any difference is
  sampling variability.

A hypothesis whose concordance with its prediction differs between
strategies is also flagged for review.

**Experimental.** This function is experimental until nomologR 1.0.0.
Its rule for flagging a difference may be refined to separate sampling
variability from bias. Any change will be described in NEWS; see the
package help page,
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md),
for the stability policy.

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
#> <nomo_missing>
#> Missing-data sensitivity for a nomo_cfa | reference: FIML | fitted with: Listwise deletion
#> 27 of 500 cases incomplete (5.4%) in 3 pattern(s); lowest covariance coverage 0.946 (a2, b3)
#> 
#> Strategies
#> # A tibble: 2 × 8
#>   label             lavaan_missing requires role       available n_used
#>   <chr>             <chr>          <chr>    <chr>      <lgl>      <dbl>
#> 1 Listwise deletion listwise       MCAR     comparison TRUE         473
#> 2 FIML              ml             MAR      reference  TRUE         500
#>   converged admissible
#>   <lgl>     <lgl>     
#> 1 TRUE      TRUE      
#> 2 TRUE      TRUE      
#> 
#> Largest differences from the reference, in reference standard errors
#> # A tibble: 5 × 5
#>   parameter strategy          estimate reference difference_in_se
#>   <chr>     <chr>                <dbl>     <dbl>            <dbl>
#> 1 A ~~ B    Listwise deletion    0.499     0.48              0.45
#> 2 B =~ b5   Listwise deletion    0.337     0.353            -0.37
#> 3 A =~ a5   Listwise deletion    0.598     0.59              0.23
#> 4 A =~ a3   Listwise deletion    0.669     0.675            -0.21
#> 5 B =~ b3   Listwise deletion    0.757     0.762            -0.17
#> 
#> Whether data are missing at random cannot be tested from these data; see nomo_table(x, "decision_log").
nomo_table(sensitivity, "strategies")
#> # A tibble: 2 × 12
#>   strategy label     lavaan_missing requires role  as_fitted available converged
#>   <chr>    <chr>     <chr>          <chr>    <chr> <lgl>     <lgl>     <lgl>    
#> 1 listwise Listwise… listwise       MCAR     comp… TRUE      TRUE      TRUE     
#> 2 ml       FIML      ml             MAR      refe… FALSE     TRUE      TRUE     
#> # ℹ 4 more variables: admissible <lgl>, n_used <dbl>, n_discarded <dbl>,
#> #   note <chr>
```
