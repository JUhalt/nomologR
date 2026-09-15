# Simulated multi-construct validation study

A simulated construct-validation dataset for confirmatory measurement,
measurement-invariance, and theory-specified nomological-network
examples. Three latent constructs are measured by multiple indicators,
an observed outcome is available, and responses were collected in two
administration groups.

## Usage

``` r
nomo_demo_network
```

## Format

A data frame with 800 rows and 13 columns:

- ag1, ag2, ag3, ag4:

  Agency indicators. Population standardized loadings .80, .75, .70,
  .78.

- pe1, pe2, pe3, pe4:

  Persistence indicators. Population standardized loadings .78, .72,
  .76, .70.

- sd1, sd2, sd3:

  Social desirability indicators. Population standardized loadings .70,
  .75, .65.

- Performance:

  Observed outcome score (population mean 70, SD 10).

- group:

  Administration group: a factor with levels `"online"` and `"paper"`
  (400 cases each).

## Source

Simulated with a fixed seed by `data-raw/nomo_demo.R` in the package
source repository.

## Details

Within each group, the population structural model is:

- Persistence is regressed on Agency with a standardized coefficient of
  .45;

- Performance is regressed on Agency (.40) and **not** on Persistence
  (0);

- Agency and Social desirability are uncorrelated (0).

Because Persistence is correlated with Agency, Persistence and
Performance are still correlated marginally (about .18 in the
population) even though the direct Persistence-to-Performance path is
zero. This makes the dataset useful for teaching the difference between
a marginal association and a theory-specified structural path.

Measurement-invariance teaching features: all loadings are equal across
groups, the latent Agency mean is .25 SD higher in the `paper` group,
and the intercept of `ag3` is .50 higher in the `paper` group. The `ag3`
intercept is therefore a known source of scalar non-invariance.
Indicators are reported on a continuous rating metric (population mean
4, SD 1 in the `online` group) and rounded to two decimals.

## See also

[`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md),
[`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md),
[`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)

## Examples

``` r
str(nomo_demo_network)
#> 'data.frame':    800 obs. of  13 variables:
#>  $ ag1        : num  5.87 5.89 2.79 6.42 5.25 4.91 4.45 4.15 5.25 5.09 ...
#>  $ ag2        : num  4.43 5.96 3.14 5.5 3.85 4.94 5.25 3.13 5.02 4.32 ...
#>  $ ag3        : num  5.38 4.58 1.65 5.26 3.5 4.99 3.17 4.72 4.65 5.42 ...
#>  $ ag4        : num  4.25 5 2.48 5.35 3.66 6.04 3.92 4.14 5.45 4.49 ...
#>  $ pe1        : num  4.61 3.84 3.41 4.96 2.92 5.46 4.88 3.82 4.3 4.47 ...
#>  $ pe2        : num  6.26 4.79 3.34 3.24 4.13 4.95 4.95 4.81 3.88 4.15 ...
#>  $ pe3        : num  4.81 5.1 2.96 5.11 3.96 4.8 5.19 4.94 4.53 4.07 ...
#>  $ pe4        : num  4.39 2.84 4.52 3.37 3.99 4.53 3.92 5.07 5.02 5.19 ...
#>  $ sd1        : num  6.4 3.24 3.1 3.18 4.61 3.34 4.91 3.7 2.71 4.98 ...
#>  $ sd2        : num  4.6 2.18 3.65 2.69 4.34 4.66 4.71 4.62 2.6 4.13 ...
#>  $ sd3        : num  4.72 3.6 1.93 3.08 3.39 2.52 4.57 2.93 1.94 4.5 ...
#>  $ Performance: num  76.1 74.7 52.8 69.2 79.2 71.1 62.7 83.1 73 64.5 ...
#>  $ group      : Factor w/ 2 levels "online","paper": 1 1 1 1 1 1 1 1 1 1 ...
table(nomo_demo_network$group)
#> 
#> online  paper 
#>    400    400 
```
