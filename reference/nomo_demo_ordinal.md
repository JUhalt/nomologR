# Simulated five-category ordered item data

An ordered-categorical version of
[nomo_demo_continuous](https://juhalt.github.io/nomologR/reference/nomo_demo_continuous.md).
The same latent item responses (before missing values were introduced)
were cut into five ordered response categories, as is typical of
Likert-type rating scales. This dataset supports polychoric
factor-retention, ordinal EFA, WLSMV CFA, and ordinal reliability
examples.

## Usage

``` r
nomo_demo_ordinal
```

## Format

A data frame with 500 rows and 10 ordered factors with levels `"1"` \<
`"2"` \< `"3"` \< `"4"` \< `"5"`:

- a1, a2, a3, a4, a5:

  Ordered indicators written for factor A (`a5` cross-loads on factor
  B).

- b1, b2, b3, b4, b5:

  Ordered indicators written for factor B (`b5` is weak).

## Source

Simulated with a fixed seed by `data-raw/nomo_demo.R` in the package
source repository.

## Details

Latent responses were thresholded at -1.80, -0.80, 0.20, and 1.20, so
the observed distributions lean toward the upper categories. The dataset
has no missing values, which keeps the first ordinal examples focused on
estimator and correlation choices rather than on missing-data handling
for categorical models.

## See also

[nomo_demo_continuous](https://juhalt.github.io/nomologR/reference/nomo_demo_continuous.md),
[nomo_demo_network](https://juhalt.github.io/nomologR/reference/nomo_demo_network.md)

## Examples

``` r
str(nomo_demo_ordinal)
#> 'data.frame':    500 obs. of  10 variables:
#>  $ a1: Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 2 3 4 2 5 3 3 2 3 4 ...
#>  $ a2: Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 1 2 3 2 3 5 3 3 4 5 ...
#>  $ a3: Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 2 4 3 3 5 4 3 3 3 5 ...
#>  $ a4: Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 2 3 4 3 3 3 3 3 4 4 ...
#>  $ a5: Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 1 3 4 4 3 4 3 3 4 2 ...
#>  $ b1: Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 2 3 5 4 3 2 3 3 3 2 ...
#>  $ b2: Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 4 3 3 3 3 2 4 2 2 3 ...
#>  $ b3: Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 2 4 4 4 2 2 3 3 3 2 ...
#>  $ b4: Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 4 3 4 4 4 2 4 4 2 2 ...
#>  $ b5: Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 3 4 5 4 3 2 4 5 3 2 ...
table(nomo_demo_ordinal$a1)
#> 
#>   1   2   3   4   5 
#>  13 105 179 145  58 
```
