# Simulated repeated measures of one construct

A simulated dataset for longitudinal measurement invariance: the same
four Wellbeing items answered by the same people on three occasions.

## Usage

``` r
nomo_demo_longitudinal
```

## Format

A data frame with 500 rows and 12 columns, `w1_t1` to `w4_t3`: items
`w1` to `w4` at occasions `t1`, `t2`, and `t3`. Population standardized
loadings at `t1` are 0.80, 0.75, 0.70, and 0.65, and the loadings are
equal across occasions.

## Source

Simulated with a fixed seed by `data-raw/nomo_demo.R` in the package
source repository.

## Details

Wellbeing has a latent variance of 1 at every occasion. Its mean rises
from 0 at `t1` to 0.30 at `t2` and 0.50 at `t3`, in `t1` standard
deviations, and it correlates .60 between adjacent occasions and .45
between `t1` and `t3`.

Longitudinal teaching features: each item's unique factor correlates .20
with the same item's unique factor on the other occasions, and the
intercept of `w3` is 0.40 higher at `t2` and `t3` than at `t1`. The `w3`
intercept is therefore a known source of scalar non-invariance over
time, and holding it equal inflates the apparent change in Wellbeing.
Indicators are on a continuous rating metric (population mean 4 at `t1`)
and rounded to two decimals.

## See also

[`nomo_invariance_longitudinal()`](https://juhalt.github.io/nomologR/reference/nomo_invariance_longitudinal.md)

## Examples

``` r
str(nomo_demo_longitudinal)
#> 'data.frame':    500 obs. of  12 variables:
#>  $ w1_t1: num  3.61 2.59 3.59 4.81 5.69 4.68 4.97 3.79 4.16 3.46 ...
#>  $ w2_t1: num  2.97 3.54 4.23 3.63 5.22 4.77 4.47 5.08 5 1.99 ...
#>  $ w3_t1: num  4.65 2.62 2.64 4.54 4.95 4.67 5.74 3.79 3.93 4.13 ...
#>  $ w4_t1: num  1.86 3.59 3.35 5.79 4.09 5.37 5.39 4.92 4.71 2.32 ...
#>  $ w1_t2: num  3.81 4.03 4.5 4.95 5.33 3.46 5.8 6.04 3.92 3.88 ...
#>  $ w2_t2: num  4.64 4.71 3.92 4.71 5.23 3.6 4.13 4.58 4.9 2.93 ...
#>  $ w3_t2: num  3.34 3.26 4.61 5.48 5.64 3.32 5.07 4.38 4.83 3.32 ...
#>  $ w4_t2: num  3.79 2.84 3.78 5.56 4.59 4.2 3.36 4.11 2.72 3.3 ...
#>  $ w1_t3: num  3.21 5.43 3.15 6 5.53 4.54 4.9 5.31 5.16 4.21 ...
#>  $ w2_t3: num  4.19 5.68 4.78 7.13 5.12 4.99 5.23 4.61 4.93 2.98 ...
#>  $ w3_t3: num  4.35 5.8 4.26 6.98 5.69 3.81 4.53 4.46 5.82 3.41 ...
#>  $ w4_t3: num  4.6 5.11 3.55 6.73 4.09 3.64 3.6 4.36 4.74 2.95 ...
colMeans(nomo_demo_longitudinal[c("w3_t1", "w3_t2", "w3_t3")])
#>   w3_t1   w3_t2   w3_t3 
#> 3.98258 4.53520 4.72832 
```
