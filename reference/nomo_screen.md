# Audit item-level data before factor modeling

`nomo_screen()` performs a conservative audit of candidate item data
before factor-retention, EFA, or CFA decisions are made. It summarizes
item storage and observed response patterns, missingness, response
concentration, and case-level completeness. It also creates a decision
log that distinguishes observations from recommendations.

## Usage

``` r
nomo_screen(
  data,
  items = NULL,
  guidance = nomo_defaults(),
  effort = FALSE,
  scales = NULL,
  reverse = NULL,
  scale_range = NULL,
  pair_magnitude = 0.6
)
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

- effort:

  Logical. If `TRUE`, case-level indices of careless or
  insufficient-effort responding are added. See **Careless responding**.

- scales:

  Optional named list of character vectors assigning items to scales.
  Needed for even-odd consistency and for the within-scale versions of
  long-string and inter-item standard deviation.

- reverse:

  Optional character vector naming reverse-keyed items. Used only to
  recode an internal copy for the indices that need it; the data is
  never recoded.

- scale_range:

  Numeric `c(min, max)` of the response scale. Required whenever
  `reverse` is supplied, and never inferred from the data.

- pair_magnitude:

  Minimum absolute between-person correlation for an item pair to count
  as a psychometric antonym or synonym. Curran (2016) suggests .60 while
  saying there is no firm basis for it, so it is an argument rather than
  a constant.

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

**Careless responding.** With `effort = TRUE`, each case receives the
indices Meade and Craig (2012), Huang et al. (2012), and Curran (2016)
describe: long-string, inter-item standard deviation (Marjanovic et al.,
2015), Mahalanobis distance, even-odd consistency, and psychometric
antonym and synonym correlations. Curran recommends these be used in
series because each has blind spots, and the output is built to show
where they disagree rather than to combine them into one score.

They disagree for a reason. Inter-item standard deviation detects random
responding and gives a respondent who answers every item identically the
best possible score; long-string detects exactly that respondent. When
the two disagree about a case, the decision log says so.

A case is flagged only where a source states a rule, and each flag
carries the source's own qualification: long-string at half the number
of items, which Curran offers as a conservative starting point and says
is not the best cut score for every scale, and a positive antonym or
negative synonym correlation. The other indices have no stated cut score
and are reported without a flag. Huang et al. found the indices they
recommended identified attentive respondents well and random responders
poorly, so an unflagged case is not thereby shown to be attentive.

Each respondent's antonym, synonym, and even-odd value is a correlation
whose N is the number of pairs or scales. With two, every value is
exactly +1 or -1, so at least three are required; with fewer than five
the log says the flags are coarse. Cases are flagged, never removed.

## References

Curran, P. G. (2016). Methods for the detection of carelessly invalid
responses in survey data. *Journal of Experimental Social Psychology,
66*, 4-19.
[doi:10.1016/j.jesp.2015.07.006](https://doi.org/10.1016/j.jesp.2015.07.006)

Huang, J. L., Curran, P. G., Keeney, J., Poposki, E. M., & DeShon, R. P.
(2012). Detecting and deterring insufficient effort responding to
surveys. *Journal of Business and Psychology, 27*(1), 99-114.
[doi:10.1007/s10869-011-9231-8](https://doi.org/10.1007/s10869-011-9231-8)

Marjanovic, Z., Holden, R., Struthers, W., Cribbie, R., & Greenglass, E.
(2015). The inter-item standard deviation (ISD): An index that
discriminates between conscientious and random responders. *Personality
and Individual Differences, 84*, 79-83.
[doi:10.1016/j.paid.2014.08.021](https://doi.org/10.1016/j.paid.2014.08.021)

Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in
survey data. *Psychological Methods, 17*(3), 437-455.
[doi:10.1037/a0028085](https://doi.org/10.1037/a0028085)

Clark, L. A., & Watson, D. (2019). Constructing validity: New
developments in creating objective measuring instruments. *Psychological
Assessment, 31*(12), 1412-1427.
[doi:10.1037/pas0000626](https://doi.org/10.1037/pas0000626)

Kuhn, M., & Johnson, K. (2013). *Applied predictive modeling*. Springer.
[doi:10.1007/978-1-4614-6849-3](https://doi.org/10.1007/978-1-4614-6849-3)

Nunnally, J. C., & Bernstein, I. H. (1994). *Psychometric theory* (3rd
ed.). McGraw-Hill.

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

# Simulated scale-development data with known teaching features
scr <- nomo_screen(nomo_demo_continuous)
summary(scr)
#> <summary_nomo_screen>
#> Cases: 500 | Items: 10 | No review flag: 9 | Review: 1 | Concern: 0
#> Missingness flags: 2 | Constant: 0 | All missing: 0 | Relationship eligible: 10
#> 
#> Integrated item review:
#> # A tibble: 10 × 7
#>    item  item_type          attention pct_missing mode_prop
#>    <chr> <chr>              <ord>           <dbl>     <dbl>
#>  1 a1    numeric_continuous none            0        0.01  
#>  2 a2    numeric_continuous none            0.03     0.0124
#>  3 a3    numeric_continuous none            0        0.01  
#>  4 a4    numeric_continuous none            0        0.012 
#>  5 a5    numeric_continuous none            0        0.016 
#>  6 b1    numeric_continuous none            0        0.016 
#>  7 b2    numeric_continuous none            0        0.014 
#>  8 b3    numeric_continuous none            0.024    0.0102
#>  9 b4    numeric_continuous none            0        0.012 
#> 10 b5    numeric_continuous review          0        0.014 
#>    corrected_item_rest_r review_metrics       
#>                    <dbl> <chr>                
#>  1                 0.559 ""                   
#>  2                 0.580 ""                   
#>  3                 0.511 ""                   
#>  4                 0.549 ""                   
#>  5                 0.588 ""                   
#>  6                 0.602 ""                   
#>  7                 0.513 ""                   
#>  8                 0.571 ""                   
#>  9                 0.481 ""                   
#> 10                 0.279 "corrected_item_rest"
#> 
#> `attention` is a review aid, not an automatic retention/deletion decision.
```
