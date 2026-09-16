# Extract report-ready evidence tables

`nomo_table()` returns compact tibbles intended for manuscripts, audit
appendices, teaching materials, and
[`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md).
It never rounds away the underlying object: full engine results remain
stored in the original `nomologR` object.

## Usage

``` r
nomo_table(x, ...)
```

## Arguments

- x:

  A supported `nomologR` result object.

- ...:

  Additional arguments passed to methods, usually `type`.

## Value

A tibble.

## Details

Supported objects and `type` values:

- `nomo_hypotheses`: the machine-readable hypothesis table (no `type`).

- `nomo_network`: `"hypotheses"` (default), `"fit"`, `"measurement"`,
  `"relations"`, `"replication"`, `"decision_log"`.

- `nomo_invariance`: `"fit"` (default), `"categories"`, `"partial"`,
  `"local_strain"`, `"decision_log"`. The local-strain table keeps
  lavaan's internal `constraint` label and adds a human-readable
  `constraint_display` column (for example,
  `Intercept: ag3 (online vs. paper)`).

- `nomo_compare`: `"comparisons"` (default), `"models"`, `"loadings"`,
  `"evidence"`, `"decision_log"`; see
  [`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md).

- `nomo_run`: `"stages"` (default), `"requests"`, `"decisions"`,
  `"component_log"`, `"scales"`, `"recipe"`, `"settings"`, `"lineage"`;
  see
  [`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
  and
  [`nomo_revise()`](https://juhalt.github.io/nomologR/reference/nomo_revise.md).
  `"lineage"` returns one row per revision, or no rows for a workflow
  that was never revised.

## Examples

``` r
h <- nomo_hypotheses(
  "Agency -> Persistence" = positive(min = .20),
  "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15))
)
nomo_table(h)
#> # A tibble: 2 × 16
#>   id    relation    source target operator relation_type prediction lower  upper
#>   <chr> <chr>       <chr>  <chr>  <chr>    <chr>         <chr>      <dbl>  <dbl>
#> 1 H1    Agency -> … Agency Persi… ->       directed      positive    0.2  Inf   
#> 2 H2    Agency <->… Agency Socia… <->      association   negligible -0.15   0.15
#> # ℹ 7 more variables: lower_inclusive <lgl>, upper_inclusive <lgl>,
#> #   region <chr>, scale <chr>, origin <chr>, magnitude_specified <lgl>,
#> #   confirmable <lgl>

inv <- nomo_invariance(
  "Agency =~ ag1 + ag2 + ag3 + ag4",
  data = nomo_demo_network,
  group = "group",
  levels = c("configural", "metric", "scalar")
)
nomo_table(inv, "fit")
#> # A tibble: 3 × 19
#>   level     constraints partial_requested status converged  chisq    df   pvalue
#>   <chr>     <chr>       <chr>             <chr>  <lgl>      <dbl> <dbl>    <dbl>
#> 1 configur… none        ""                estim… TRUE       0.327     4 9.88e- 1
#> 2 metric    loadings    ""                estim… TRUE       5.71      7 5.75e- 1
#> 3 scalar    loadings, … ""                estim… TRUE      68.9      10 7.12e-11
#> # ℹ 11 more variables: cfi <dbl>, rmsea <dbl>, srmr <dbl>, delta_cfi <dbl>,
#> #   delta_rmsea <dbl>, delta_srmr <dbl>, lrt_chisq <dbl>, lrt_df <dbl>,
#> #   lrt_p <dbl>, warnings <chr>, error <chr>
head(nomo_table(inv, "local_strain"))
#> # A tibble: 6 × 8
#>   level  constraint_index constraint    score_x2    df  p_value diagnostic_only
#>   <chr>             <int> <chr>            <dbl> <dbl>    <dbl> <lgl>          
#> 1 metric                3 .p3. == .p17.   4.92       1 2.66e- 2 TRUE           
#> 2 metric                2 .p2. == .p16.   1.03       1 3.10e- 1 TRUE           
#> 3 metric                4 .p4. == .p18.   0.690      1 4.06e- 1 TRUE           
#> 4 metric                1 .p1. == .p15.   0.0273     1 8.69e- 1 TRUE           
#> 5 scalar                7 .p7. == .p21.  61.1        1 5.33e-15 TRUE           
#> 6 scalar                5 .p5. == .p19.  11.3        1 7.68e- 4 TRUE           
#> # ℹ 1 more variable: constraint_display <chr>
```
