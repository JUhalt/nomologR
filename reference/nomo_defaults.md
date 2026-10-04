# Default guidance settings for nomologR

The numerical values returned by this function are teaching/reference
points, not universal pass/fail criteria. They are intended to trigger
inspection and explanation in downstream `nomologR` functions.

## Usage

``` r
nomo_defaults(profile = "teaching")
```

## Arguments

- profile:

  Guidance profile. `"teaching"`, the only profile, holds reference
  values commonly taught in measurement courses.

## Value

A named list of guidance settings.

## Details

Each value is a commonly taught reference point from the literature
cited in the corresponding analysis function (for example,
[`nomo_efa()`](https://juhalt.github.io/nomologR/reference/nomo_efa.md),
[`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md),
[`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md),
and
[`nomo_validity()`](https://juhalt.github.io/nomologR/reference/nomo_validity.md)).
Changing a reference changes which evidence is flagged for review; it
never deletes items, respecifies models, or declares validity.

`long_string_min_items` is a count of items, not a reference value for a
statistic:
[`nomo_screen()`](https://juhalt.github.io/nomologR/reference/nomo_screen.md)
flags a case on long-string only when at least that many items are
screened, because half the length of a shorter item set is a run that
attentive respondents give often.

`factor_cd_population`, `factor_cd_samples`, and `factor_cd_alpha` are
the simulation settings for the comparison-data criterion of
[`nomo_factors()`](https://juhalt.github.io/nomologR/reference/nomo_factors.md):
the size of each simulated population, the number of samples drawn per
candidate structure, and the significance level of the test that stops
adding factors. The defaults (5000 cases, 100 samples, .30) are smaller
than those of
[`EFAtools::efa_cd()`](https://mdsteiner.github.io/EFAtools/reference/efa_cd.html)
(10000 cases, 500 samples) to keep the run short; raise them for a final
analysis.

`auto_delete` and `auto_respecify` are always `FALSE`. They record a
design rule rather than switch a feature: nomologR never deletes an item
or respecifies a model on its own.
[`nomo_factors()`](https://juhalt.github.io/nomologR/reference/nomo_factors.md)
and
[`nomo_efa()`](https://juhalt.github.io/nomologR/reference/nomo_efa.md)
stop with an explanation when either is set to `TRUE`.

## Examples

``` r
guidance <- nomo_defaults()
guidance$efa_loading_reference
#> [1] 0.4
guidance$fit_reference
#> $cfi
#> [1] 0.95
#> 
#> $tli
#> [1] 0.95
#> 
#> $rmsea
#> [1] 0.06
#> 
#> $srmr
#> [1] 0.08
#> 

# A deliberately stricter loading reference flags more items for review.
stricter <- nomo_defaults()
stricter$efa_loading_reference <- 0.60
efa <- nomo_efa(nomo_demo_continuous, factors = 2, guidance = stricter)
efa$item_summary[, c("item", "primary_loading", "attention")]
#> # A tibble: 10 × 3
#>    item  primary_loading attention    
#>    <chr>           <dbl> <chr>        
#>  1 a1              0.808 KEEP         
#>  2 a2              0.709 KEEP         
#>  3 a3              0.667 KEEP         
#>  4 a4              0.770 KEEP         
#>  5 a5              0.414 STRONG REVIEW
#>  6 b1              0.783 KEEP         
#>  7 b2              0.693 KEEP         
#>  8 b3              0.783 KEEP         
#>  9 b4              0.637 REVIEW       
#> 10 b5              0.332 STRONG REVIEW
```
