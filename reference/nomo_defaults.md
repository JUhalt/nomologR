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

  Guidance profile. Only `"teaching"` is implemented during the initial
  development series.

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
