# Simulated two-factor item data with known teaching features

A simulated scale-development dataset with two correlated latent factors
and five candidate indicators per factor. Because the population model
is known, learners can compare `nomologR` evidence with the structure
that actually generated the data.

## Usage

``` r
nomo_demo_continuous
```

## Format

A data frame with 500 rows and 10 numeric columns:

- a1, a2, a3, a4, a5:

  Indicators written for factor A. Population standardized loadings are
  .80, .75, .70, .72, and .45 (with a .35 cross-loading on factor B for
  `a5`).

- b1, b2, b3, b4, b5:

  Indicators written for factor B. Population standardized loadings are
  .78, .74, .80, .70, and .30.

## Source

Simulated with a fixed seed by `data-raw/nomo_demo.R` in the package
source repository.

## Details

The data are deliberately imperfect in ways that scale developers
routinely encounter:

- `a5` cross-loads on both factors (population loadings .45 and .35);

- `b5` is a weak indicator (population loading .30);

- `a2` (15 cases) and `b3` (12 cases) contain values missing completely
  at random.

These features are included so that item-audit, factor-retention, and
EFA output have something substantive to explain. They are not
instructions to delete `a5` or `b5`; whether such items remain depends
on theory, content coverage, and later evidence.

Population model: factors A and B are standard normal with correlation
.40. Each indicator is a linear function of its factor(s) plus normal
unique variance chosen so that the indicator has unit population
variance. Scores are reported on a continuous rating metric (population
mean 4, SD 1) and rounded to two decimals; the linear transformation
does not affect correlations or standardized estimates.

## See also

[nomo_demo_ordinal](https://juhalt.github.io/nomologR/reference/nomo_demo_ordinal.md)
for five-category ordered versions of the same latent responses and
[nomo_demo_network](https://juhalt.github.io/nomologR/reference/nomo_demo_network.md)
for a multi-construct validation study.

## Examples

``` r
str(nomo_demo_continuous)
#> 'data.frame':    500 obs. of  10 variables:
#>  $ a1: num  2.23 3.47 4.8 2.87 5.22 4.02 3.23 3.12 3.62 4.66 ...
#>  $ a2: num  1.46 3.06 4.16 2.46 3.71 6.06 4.16 3.45 NA 5.27 ...
#>  $ a3: num  2.25 4.71 4.08 3.45 5.94 4.3 3.39 3.41 3.21 5.31 ...
#>  $ a4: num  2.59 3.39 4.59 3.85 4.01 3.65 4.09 3.81 4.77 5.16 ...
#>  $ a5: num  1.85 3.23 4.54 4.22 3.62 4.67 4.14 3.46 4.95 3.08 ...
#>  $ b1: num  2.67 4.05 5.31 4.6 3.57 3.11 4.16 3.99 3.49 2.71 ...
#>  $ b2: num  4.21 3.91 3.65 3.72 3.94 3.01 4.32 2.72 3.17 3.95 ...
#>  $ b3: num  2.33 4.72 4.85 4.63 2.84 2.95 3.75 4.02 3.69 3.08 ...
#>  $ b4: num  4.79 3.72 4.24 4.27 4.6 2.28 5.01 4.62 2.94 2.87 ...
#>  $ b5: num  3.42 4.47 5.4 4.38 3.87 2.97 4.75 5.63 3.65 2.97 ...
colSums(is.na(nomo_demo_continuous))
#> a1 a2 a3 a4 a5 b1 b2 b3 b4 b5 
#>  0 15  0  0  0  0  0 12  0  0 

scr <- nomo_screen(nomo_demo_continuous)
scr
#> <nomo_screen>
#> Cases: 500 | Candidate items: 10
#> Items with missing responses: 2 | Constant: 0 | All missing: 0
#> Relationship diagnostics: 10 eligible items | 10 item-rest estimates
#> Response concentration flags: 0 | Near-zero variance: 0
#> Decision log: 3 info | 1 review | 0 concern
#> No rows or items were removed or modified.
```
