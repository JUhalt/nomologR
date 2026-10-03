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
  included. May also be a handoff from `contentvalidR`'s
  `content_handoff()`; see **Items from content review**.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

- effort:

  Logical. If `TRUE`, case-level indices of careless or
  insufficient-effort responding are added. See **Careless responding**.

- scales:

  Optional named list of character vectors assigning items to scales.
  Needed for even-odd consistency and for the within-scale versions of
  long-string and inter-item standard deviation. With two or more
  scales, each item's corrected item-rest correlation is also computed
  against the rest of its own scale, and the item review uses that
  value. Negative inter-item correlations are then reviewed only within
  a scale, since items of different constructs need not correlate
  positively. A `contentvalidR` handoff supplies its scales here.

- reverse:

  Optional character vector naming reverse-keyed items, each of which
  must be an item being screened. Used only to recode an internal copy:
  for the careless-responding indices that need it, and to say whether a
  declared item's negative item-rest correlation is the sign expected
  before recoding. The data is never recoded.

- scale_range:

  Numeric `c(min, max)` of the response scale, with `min` below `max`.
  It is needed to use `reverse` and is never inferred from the data.
  With `effort = TRUE`, naming reverse-keyed items without it is
  refused. With `effort = FALSE` the audit runs, and the decision log
  records that the declared keying was not used.

- pair_magnitude:

  Minimum absolute between-person correlation for an item pair to count
  as a psychometric antonym or synonym. Curran (2016) suggests .60 while
  saying there is no firm basis for it, so it is an argument rather than
  a constant.

## Value

An object of class `nomo_screen`. The fields to read are:

- `items`: the items screened, and `n_cases`, the number of rows.

- `item_summary`: one row per item, with its storage, inferred type,
  missingness, most common response, descriptive statistics, and
  near-zero-variance indicators.

- `response_distribution`: counts and proportions of each response.

- `case_summary`: missingness per row.

- `relationship_summary`: each item's corrected item-rest correlation
  and summary of its inter-item correlations. With two or more declared
  scales, `scale`, `scale_item_rest_r`, and `scale_item_rest_n` give
  each item's scale and its item-rest correlation within that scale, and
  `scale_negative_interitem_n` counts its negative correlations with
  items of the same scale; otherwise they are `NA`.

- `inter_item_correlations`: one row per item pair.

- `decision_log`: the evidence and its explanations (see
  [`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md)).

- `effort`, `effort_pairs`, and `effort_settings`: the
  careless-responding indices per row, the pairs they used, and their
  settings, when `effort = TRUE`. The settings include
  `long_string_rule_applied`, which is `FALSE` when too few items were
  screened for the long-string rule.

- `handoff`: the content-review handoff read from `items`, when one was
  supplied.

`pct_missing`, in `item_summary` and `case_summary`, is a proportion
between 0 and 1, as are the `*_prop` and `proportion_*` columns.
`percent_unique` is a percentage, 0 to 100, as the near-zero-variance
rule states it. See **Conventions in returned tables** in
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md).

Other fields record the call, the settings used, and intermediate engine
results. They may change between releases and are not part of the stable
interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

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

The long-string rule is applied only when at least
`guidance$long_string_min_items` items are screened (20 in
[`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md)).
Half the length of a shorter item set is a run of a few responses, which
attentive respondents give often: with two items every case reaches it.
On a shorter set each case's longest run is still reported in
`long_string`, no case is flagged on it, and the decision log states the
number of items and the share of cases that reached half the length.

Each respondent's antonym, synonym, and even-odd value is a correlation
whose N is the number of pairs or scales. With two, every value is
exactly +1 or -1, so at least three are required; with fewer than five
the log says the flags are coarse. The same holds for each respondent:
one who answered fewer than three of the pairs, or both halves of fewer
than three scales, has no value. Even-odd consistency is Spearman-Brown
corrected, and the correction has no meaning below -1, so the value is
bounded there: it lies between -1 and 1. Cases are flagged, never
removed.

**Items from content review.** `items` may be the handoff that
`contentvalidR`'s `content_handoff()` produces after content review.
Only items it marks as carried are screened. Every item it held back is
listed in the decision log with its status and recommendation quoted in
`contentvalidR`'s own words, and is never analyzed or reinstated here.
The log also records the producing version, workflow, and carry rule,
and that item membership came from content review rather than from these
data.

A carried item that is not a column of `data` is refused, never dropped.
Where the call leaves `scales`, `reverse`, or `scale_range` unset, the
handoff's scales and declared keying fill them. An item is never treated
as forward keyed because keying was undeclared, and a response scale the
handoff did not record is never inferred. A handoff with a schema
version this release does not read is refused, naming both package
versions. The interface is specified in nomologR issue \#46, and
`contentvalidR` is not needed to read it.

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
#> <nomo_screen summary> Item and data audit
#> Cases: 500 | Items: 10 | Flags: 1 review, 0 concern
#> Items with missing responses: 2 | Constant: 0 | All missing: 0
#> Relationship eligible: 10
#> 
#> Item review
#>   Item  Type        Missing  Top share  Item-rest r  Flag
#>   a1    continuous     0.0%       1.0%        0.559
#>   a2    continuous     3.0%       1.2%        0.580
#>   a3    continuous     0.0%       1.0%        0.511
#>   a4    continuous     0.0%       1.2%        0.549
#>   a5    continuous     0.0%       1.6%        0.588
#>   b1    continuous     0.0%       1.6%        0.602
#>   b2    continuous     0.0%       1.4%        0.513
#>   b3    continuous     2.4%       1.0%        0.571
#>   b4    continuous     0.0%       1.2%        0.481
#>   b5    continuous     0.0%       1.4%        0.279  review
#>   Top share is the proportion of responses in the most common category.
#> 
#> Flagged items
#>   - b5 (review): `b5` has a corrected item-rest correlation of r = 0.28
#>     (n = 473), below the teaching reference.
#> 
#> Flags are review aids, not decisions to keep or delete an item.
```
