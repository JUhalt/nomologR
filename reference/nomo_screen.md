# Audit item-level data before factor modeling

`nomo_screen()` performs a conservative audit of candidate item data
before factor-retention, EFA, or CFA decisions are made. It summarizes
item storage and observed response patterns, missingness, response
concentration, and case-level completeness. It also creates a decision
log that distinguishes observations from recommendations.

## Usage

``` r
nomo_screen(data, items = NULL, guidance = nomo_defaults())
```

## Arguments

- data:

  A data frame containing candidate items.

- items:

  Optional character vector identifying item columns. If `NULL`, all
  columns are audited and the decision log reminds the user to verify
  that identifiers, demographics, and other non-item columns were not
  included.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

An object of class `nomo_screen` containing item summaries, response
distributions, case-level completeness diagnostics, an evidence-guided
decision log, and the guidance settings used.

## Details

The function never removes rows or items, changes scores, reverse-keys
items, or decides whether a scale is valid.

Item-type labels are descriptive, not modeling decisions. In particular,
`numeric_discrete` means that the observed numeric values are
integer-like with 10 or fewer distinct observed values. It does **not**
automatically mean that the item should be treated as ordinal in later
analyses.

A `constant` item has only one distinct observed value. An `all_missing`
item has no observed values. These are hard data conditions rather than
psychometric cutoff rules.

Response concentration and near-zero-variance flags use configurable
teaching references from
[`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).
They are screening heuristics, not psychometric laws or automatic
item-retention rules. Ordered and numeric-discrete items also receive
descriptive boundary concentration summaries. Continuous-like numeric
indicators receive descriptive skewness and excess-kurtosis summaries
without a pass/fail normality judgment.

## Examples

``` r
dat <- data.frame(
  item1 = c(1, 2, 3, 4, 5),
  item2 = c(1, 2, NA, 4, 5),
  item3 = c(3, 3, 3, 3, 3)
)

out <- nomo_screen(dat)
out$item_summary
#> # A tibble: 3 × 23
#>   item  storage item_type     n n_observed n_missing pct_missing n_unique mode_n
#>   <chr> <chr>   <chr>     <int>      <int>     <int>       <dbl>    <int>  <int>
#> 1 item1 numeric numeric_…     5          5         0         0          5      1
#> 2 item2 numeric numeric_…     5          4         1         0.2        4      1
#> 3 item3 numeric binary        5          5         0         0          1      5
#> # ℹ 14 more variables: mode_prop <dbl>, min <dbl>, max <dbl>, mean <dbl>,
#> #   sd <dbl>, constant <lgl>, all_missing <lgl>, percent_unique <dbl>,
#> #   frequency_ratio <dbl>, near_zero_variance <lgl>, floor_prop <dbl>,
#> #   ceiling_prop <dbl>, skewness <dbl>, excess_kurtosis <dbl>
out$decision_log
#> # A tibble: 5 × 10
#>   stage  object       metric value reference severity observation recommendation
#>   <chr>  <chr>        <chr>  <dbl> <chr>     <chr>    <chr>       <chr>         
#> 1 screen item_select… all_c…   3   ""        info     Because `i… Verify that i…
#> 2 screen item1        item_…   5   "Descrip… info     `item1` ha… Do not infer …
#> 3 screen item2        missi…   0.2 "Descrip… info     `item2` ha… Inspect the p…
#> 4 screen item2        item_…   4   "Descrip… info     `item2` ha… Do not infer …
#> 5 screen item3        const…   1   "At leas… concern  `item3` ha… Inspect codin…
#> # ℹ 2 more variables: decision <chr>, rationale <chr>
```
