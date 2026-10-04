# From content review to empirical screening

Content review and an empirical screen answer different questions. An
expert panel judges whether an item represents the construct as it was
defined. Response data show how the item behaves: whether it varies,
what it shares with the other items, and whether it works the same way
in different groups. Neither answer contains the other, and an item can
pass one and fail the other.

This article takes one item set across that boundary. A content review
sorted twelve items and carried ten of them forward. Here those ten are
screened on 400 simulated responses. The content-review half, meaning
how the panel’s sort was analyzed and how the handoff was made, is the
joint walkthrough in `contentvalidR`, [One item set, both
stages](https://juhalt.github.io/contentvalidR/articles/one-item-set-both-stages.html).
Both articles use the same data. The data are simulated from a model
that is documented in
[`?nomo_demo_walkthrough`](https://juhalt.github.io/nomologR/reference/nomo_demo_walkthrough.md),
so every claim below can be checked against the truth.

## What content review decided

`contentvalidR` records its decisions in a *handoff*. This package
stores the handoff for these items, so the article runs without
`contentvalidR` installed:

``` r

handoff <- readRDS(
  system.file("extdata", "content-handoff-walkthrough.rds", package = "nomologR")
)
handoff$item_evidence[, c("item", "scale", "status", "carried", "keying")]
#>    item scale    status carried keying
#> 1   EF1    EF Supported    TRUE      1
#> 2   EF2    EF Supported    TRUE     -1
#> 3   EF3    EF Supported    TRUE      1
#> 4   EF4    EF Supported    TRUE      1
#> 5   EF5    EF    Review   FALSE      1
#> 6   EF6    EF Supported    TRUE      1
#> 7   TF1    TF Supported    TRUE      1
#> 8   TF2    TF Supported    TRUE     -1
#> 9   TF3    TF Supported    TRUE      1
#> 10  TF4    TF Supported    TRUE      1
#> 11  TF5    TF    Review   FALSE      1
#> 12  TF6    TF Supported    TRUE      1
```

The panel carried ten items. It held back `EF5` and `TF5`, whose sorts
did not meet the criterion. It placed the carried items in two facets:

- Effort Regulation (`EF`), keeping going when the work is dull or hard;
- Task Focus (`TF`), staying on the task rather than switching away.

Keying `-1` marks `EF2` and `TF2` as reverse-worded: a high answer means
*less* persistence.

## The responses, and what each item was built to do

``` r

nomo_demo_walkthrough_items[, c("item", "role")]
#>    item                                                               role
#> 1   EF1           ordinary: the panel places it and it behaves as intended
#> 2   EF2                   reverse-worded: behaves as intended once recoded
#> 3   EF3            flagged by an empirical screen and worth keeping anyway
#> 4   EF4      passes content review, then carries almost no common variance
#> 5   EF5 fails content review: the wording pulls judges toward test anxiety
#> 6   EF6        meets the content criterion by one judge, then behaves well
#> 7   TF1           ordinary: the panel places it and it behaves as intended
#> 8   TF2                   reverse-worded: behaves as intended once recoded
#> 9   TF3           ordinary: the panel places it and it behaves as intended
#> 10  TF4                   passes content review, then loads on both facets
#> 11  TF5     fails content review: a competing facet takes more assignments
#> 12  TF6                             behaves differently in the two cohorts
```

A real study does not come with a `role` column. Here it is known
because the data were simulated to contain specific problems, and it is
the answer key for what follows.

## Screening the data as they arrive

[`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
takes the handoff whole: its carried items and facets become the run’s
scales, and each facet is screened on its own.

``` r

run <- nomo_run(nomo_demo_walkthrough, scales = handoff)
run
#> <nomo_run> Guided workflow
#> Status: PAUSED | Mode: teaching | Sample design: same sample
#> Exploratory N = 400 | Confirmatory N = 400 | Scales: 2
#> Completed: screen -> factors | Next: efa
#> 
#> Key evidence
#>   - Item audit: 10 items; flags: 10 review, 0 concern
#>   - Parallel analysis suggests: EF 1, TF 1
#> 
#> Researcher decision required: efa (EF, TF)
#>   Reason: The EFA factor count changes the fitted model. Retention evidence
#>   can inform that choice, but it does not authorize the pipeline to choose for
#>   the researcher.
#>   Options: Inspect the full `nomo_factors` result, compare plausible
#>   neighboring solutions when appropriate, and supply a positive integer for
#>   each scale.
#>   Consequence: No EFA is fitted until an explicit researcher factor-count
#>   decision is supplied.
#>   - EF: Parallel analysis currently suggests 1 factor; the retained plausible
#>     set is 1. The pipeline has not adopted a factor count.
#>   - TF: Parallel analysis currently suggests 1 factor; the retained plausible
#>     set is 1. The pipeline has not adopted a factor count.
#>   Example: decisions = list(factor_count = c(EF = <integer>, TF = <integer>))
#> 
#> No later stage has been run automatically while this consequential decision is
#> unresolved.
```

Every item is flagged. The Effort Regulation audit shows why:

``` r

summary(run$results$screen$EF)
#> <nomo_screen summary> Item and data audit
#> Cases: 400 | Items: 5 | Flags: 5 review, 0 concern
#> Items with missing responses: 0 | Constant: 0 | All missing: 0
#> Relationship eligible: 5
#> 
#> Item review
#>   Item  Type      Missing  Top share  Item-rest r  Flag
#>   EF1   discrete     0.0%      34.8%        0.173  review
#>   EF2   discrete     0.0%      28.5%       -0.505  review
#>   EF3   discrete     0.0%      78.5%        0.210  review
#>   EF4   discrete     0.0%      33.8%        0.168  review
#>   EF6   discrete     0.0%      32.5%        0.175  review
#>   Top share is the proportion of responses in the most common category.
#> 
#> Flagged items
#>   - EF1 (review): `EF1` has a corrected item-rest correlation of r = 0.17
#>     (n = 400), below the teaching reference.
#>   - EF2 (review): `EF2` has a negative corrected item-rest correlation
#>     (r = -0.50, n = 400). It is declared reverse-keyed, and this is the sign
#>     such an item shows before it is recoded: recoded on the declared 1 to 5
#>     scale, its item-rest correlation is r = 0.50. The data were not recoded.
#>   - EF3 (review): `EF3` has a corrected item-rest correlation of r = 0.21
#>     (n = 400), below the teaching reference.
#>   - EF4 (review): `EF4` has a corrected item-rest correlation of r = 0.17
#>     (n = 400), below the teaching reference.
#>   - EF6 (review): `EF6` has a corrected item-rest correlation of r = 0.18
#>     (n = 400), below the teaching reference.
#> 
#> Flags are review aids, not decisions to keep or delete an item.
```

`EF2` correlates negatively with the rest of its facet. The audit says
what that most likely means: the item is declared reverse-keyed, and
recoded as declared its correlation would be positive. The negative sign
is a coding matter, not evidence against the item.

Left as answered, the item also makes the rest of the facet look bad.
Every other EF item’s correlation with the rest of the scale includes
`EF2` running the wrong way, so all of them fall below the teaching
reference. A screen of data that are not yet recoded cannot say much
about any item in the facet.

`nomologR` never recodes data. Recoding is the researcher’s step, and it
belongs in the analysis code where it can be seen.

## Recoding as content review declared

``` r

walk <- nomo_demo_walkthrough
reversed <- handoff$item_evidence$item[handoff$item_evidence$keying %in% -1]
walk[reversed] <- 6L - walk[reversed]
```

The responses are now recoded, so the run should not treat any item as
still needing it. `reverse = character(0)` says so, in place of the
reverse-keyed items the handoff declared. It replaces only that part of
the handoff’s keying: the response scale still comes from the handoff,
and the run’s decision log records the change.

``` r

run <- nomo_run(
  walk,
  scales = handoff,
  settings = list(screen = list(reverse = character(0)))
)
run$decision_log$observation[run$decision_log$id == "keying"]
#> [1] "`settings$screen$reverse` (none) was used in place of the reverse-keyed item(s) the content-review handoff declared (EF2, TF2)."
ef <- run$results$screen$EF
summary(ef)
#> <nomo_screen summary> Item and data audit
#> Cases: 400 | Items: 5 | Flags: 1 review, 0 concern
#> Items with missing responses: 0 | Constant: 0 | All missing: 0
#> Relationship eligible: 5
#> 
#> Item review
#>   Item  Type      Missing  Top share  Item-rest r  Flag
#>   EF1   discrete     0.0%      34.8%        0.559
#>   EF2   discrete     0.0%      28.5%        0.505
#>   EF3   discrete     0.0%      78.5%        0.339
#>   EF4   discrete     0.0%      33.8%        0.228  review
#>   EF6   discrete     0.0%      32.5%        0.470
#>   Top share is the proportion of responses in the most common category.
#> 
#> Flagged items
#>   - EF4 (review): `EF4` has a corrected item-rest correlation of r = 0.23
#>     (n = 400), below the teaching reference.
#> 
#> Flags are review aids, not decisions to keep or delete an item.
```

Recoded, `EF2` correlates 0.50 with the rest of its facet, and one EF
item remains flagged: `EF4`, at 0.23. Eighteen of twenty judges placed
`EF4` in Effort Regulation, and they were right about its wording.
Keeping to a self-set study schedule reads as effort. In the responses,
though, it shares almost nothing with the other items. This is what
content review cannot see.

`EF3` is not flagged here (0.34), but its distribution stands out: the
“Top share” column shows most answers in a single category. Almost
everyone finishes the assignments that count toward their grade. The
next stage shows what that costs.

## The measurement model

An exploratory factor analysis of the ten recoded items, with the two
facets the panel defined:

``` r

efa <- nomo_efa(walk[handoff$items], factors = 2)
summary(efa)
#> <nomo_efa summary> Exploratory factor analysis
#> Cases: 400 | Items: 10 | Factors: 2 (researcher specified)
#> Correlation: Pearson | Extraction: minres | Rotation: oblimin (oblique)
#> KMO: .84 | Bartlett's test: chi-square(45) = 810.20, p < .001
#> 
#> Item structure
#>   Item  Factor  Loading  Next factor  Loading  Communality  Flag
#>   EF1   F1         0.78  F2             -0.07          .56
#>   EF2   F1         0.65  F2              0.01          .44
#>   EF3   F1         0.37  F2              0.08          .17  Concern
#>   EF4   F1         0.28  F2              0.02          .08  Concern
#>   EF6   F1         0.57  F2              0.05          .35  Review
#>   TF1   F2         0.69  F1              0.02          .50
#>   TF2   F2         0.56  F1              0.09          .37  Review
#>   TF3   F2         0.60  F1             -0.01          .35  Review
#>   TF4   F1         0.44  F2              0.32          .42  Review
#>   TF6   F2         0.64  F1             -0.08          .37  Review
#> 
#> Flagged
#>   - EF3 (Concern): The primary loading, 0.37 in absolute value, is below the
#>     0.40 teaching reference. The communality, .17, is below the .40 teaching
#>     reference.
#>   - EF4 (Concern): The primary loading, 0.28 in absolute value, is below the
#>     0.40 teaching reference. The communality, .08, is below the .40 teaching
#>     reference.
#>   - EF6, TF3 (Review): The communality, .35, is below the .40 teaching
#>     reference.
#>   - TF2, TF6 (Review): The communality, .37, is below the .40 teaching
#>     reference.
#>   - TF4 (Review): The secondary loading, 0.32 in absolute value, is at or
#>     above the 0.30 cross-loading reference.
#> 
#> Factor correlations
#>   Factor 1  Factor 2    r
#>   F1        F2        .44
#> 
#> Largest residual correlations
#>   RMSR: 0.021
#>   Item 1  Item 2  Residual
#>   EF4     TF6        -.063
#>   EF2     TF2        -.046
#>   EF4     TF4         .040
#>   TF3     TF4         .037
#>   EF2     TF6         .035
#> 
#> Abbreviations
#>   KMO -- Kaiser-Meyer-Olkin measure of sampling adequacy.
#>   RMSR -- Root mean square of the off-diagonal residual correlations.
#> 
#> Numerical references trigger inspection, not automatic deletion or hidden
#> refitting.
#> 
#> See nomo_table(x, "pattern") for the full pattern matrix and
#> nomo_table(x, "decision_log") for every recorded decision.
```

Three items stand apart:

- `EF4` has almost no common variance, as the screen suggested.
- `EF3` loads weakly as well. Its answers are piled at the top of the
  scale, and a variable that barely varies cannot correlate strongly
  with anything.
- `TF4` loads on both factors. Working through a task without taking
  breaks is effort as much as focus. The panel saw one facet, and the
  data see two.

A confirmatory model with the panel’s structure locates the same strain:

``` r

cfa <- nomo_cfa(nomo_model(handoff$scales), data = walk)
cfa
#> <nomo_cfa> Confirmatory factor analysis
#> Cases: 400 of 400 used | Estimator: ML | Converged: yes
#> Fit: CFI 0.941 | TLI 0.922 | RMSEA 0.058 | SRMR 0.056
#> Loadings: 10 | Flags: 2 review, 0 concern
#> No parameter was freed and no model was refit automatically. summary() shows
#> the evidence.
head(nomo_table(cfa, "modification_indices"), 3)
#> # A tibble: 3 × 8
#>   lhs   op    rhs      mi    epc sepc.lv sepc.all sepc.nox
#>   <chr> <chr> <chr> <dbl>  <dbl>   <dbl>    <dbl>    <dbl>
#> 1 EF    =~    TF4   53.6   0.786   0.640    0.529    0.529
#> 2 EF1   ~~    TF4   11.4   0.164   0.164    0.214    0.214
#> 3 EF    =~    TF6    8.35 -0.321  -0.262   -0.210   -0.210
```

The largest modification index asks for `TF4` to load on Effort
Regulation too. It is the same item the exploratory analysis flagged,
found from the other direction. As always in this package, the index
locates strain; it does not free the parameter.

## Two cohorts

The respondents came from two cohorts. Before comparing their means,
check that the items measure the same thing in both:

``` r

inv <- nomo_invariance(
  nomo_model(handoff$scales),
  data = walk,
  group = "cohort",
  levels = c("configural", "metric", "scalar")
)
summary(inv)
#> <nomo_invariance summary> Measurement invariance
#> Indicators: continuous | Groups: A, B
#> Levels completed: configural -> metric -> scalar
#> 
#> Identification and sequence
#>   Continuous indicators use the conventional configural, metric, scalar, and
#>   strict sequence.
#> 
#> Fit by level
#>   Level       Chi-square  df       p    CFI  RMSEA   SRMR
#>   configural      114.09  68  < .001  0.941  0.058  0.057
#>   metric          121.68  76  < .001  0.941  0.055  0.064
#>   scalar          153.60  84  < .001  0.911  0.064  0.071
#>   Held equal: loadings from metric; intercepts from scalar.
#> 
#> Changes from the preceding level
#>   Level   CFI change  RMSEA change  SRMR change  LRT chi-square  df       p
#>   metric      +0.001        -0.003       +0.007            7.58   8    .475
#>   scalar      -0.031        +0.010       +0.008           31.92   8  < .001
#> 
#> Latent means relative to A (its latent SD)
#>   Level   Group  Factor  Difference  95% CI            p
#>   scalar  B      EF           -0.14  [-0.36, 0.08]  .218
#>   scalar  B      TF            0.22  [0.00, 0.44]   .054
#>   Comparable only with invariant intercepts, full or partial.
#> 
#> Largest equality-constraint score diagnostics (diagnostic only)
#>   Level   Constraint                    Score  df       p
#>   scalar  Intercept: TF6 (A vs. B)      24.30   1  < .001
#>   scalar  Intercept: TF4 (A vs. B)       6.40   1    .011
#>   metric  Loading: TF -> TF6 (A vs. B)   2.71   1    .100
#>   scalar  Intercept: EF1 (A vs. B)       2.55   1    .110
#>   metric  Loading: EF -> EF3 (A vs. B)   2.32   1    .127
#>   scalar  Intercept: TF2 (A vs. B)       2.24   1    .135
#>   scalar  Loading: EF -> EF3 (A vs. B)   2.23   1    .136
#>   metric  Loading: TF -> TF1 (A vs. B)   2.07   1    .150
#>   scalar  Loading: TF -> TF1 (A vs. B)   1.69   1    .194
#>   scalar  Intercept: EF2 (A vs. B)       1.49   1    .222
#> 
#> No single delta-CFI, delta-RMSEA, delta-SRMR, chi-square difference, or score
#> diagnostic is treated as a universal invariance rule.
```

Equal loadings hold; equal intercepts cost fit. The largest score
diagnostic is `TF6`’s intercept. Putting a phone away while studying is
a specific behavior, and the two cohorts faced different classroom rules
about phones. An item can be content-valid and still not comparable
across groups. Whether to release that intercept (see
[`nomo_partial()`](https://juhalt.github.io/nomologR/reference/nomo_partial.md))
is a researcher decision, and it needs a rationale such as that one.

## Where the two stages disagree

| Item | Content review | Empirical evidence | What it teaches |
|----|----|----|----|
| `EF4` | Carried: 18 of 20 judges | Almost no common variance | Evidence against the item that no panel could see. Revise it, or drop it with a rationale. |
| `EF3` | Carried: 18 of 20 judges | Weak loading, from answers piled at the ceiling | The screen is not the final word. `EF3` is the only item about finishing required work; dropping it narrows the domain the panel defined. |
| `TF4` | Carried: 16 of 20 judges | Loads on both facets | A question about content. Revise the wording, or model the cross-loading with a rationale. |
| `TF6` | Carried: 18 of 20 judges | Intercept differs by cohort | Explain the difference before comparing cohort means. |
| `EF2`, `TF2` | Carried; reverse-worded | Negative until recoded | Coding, not evidence. |
| `EF5`, `TF5` | Held back | Not screened | They remain in `nomo_demo_walkthrough`, so you can see what keeping them would have done. |

Neither stage overrules the other. `EF4` is the case for not trusting a
panel alone, and `EF3` is the case for not trusting a screen alone.

## What each stage establishes

Content review establishes whether items represent the construct as it
was defined: whether the domain is covered, and whether each item
belongs where it was written to belong. It cannot see how an item
varies, what it shares with the others, or how it behaves in different
groups.

An empirical screen establishes how the responses behave. It cannot see
whether the domain is covered. A screen would happily keep a scale of
ten well-behaved items that all measured one corner of the construct.

`nomologR` keeps both in the record. A report from a run that started
from a handoff opens with the content review, quoting the panel’s
decisions in `contentvalidR`’s words, and each empirical flag carries
its own explanation. Neither is presented as a verdict on the other:

``` r

nomo_report(run, file = "walkthrough-report.html")
```
