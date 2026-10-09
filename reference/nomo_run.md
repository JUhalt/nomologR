# Run the guided nomologR workflow

`nomo_run()` coordinates the package's staged evidence workflow while
stopping at consequential researcher decisions. Evidence can be computed
automatically when doing so does not change the researcher's data or
model, but `nomo_run()` does not silently select factor counts, remove
items, construct a CFA model, free parameters, or respecify a model.

## Usage

``` r
nomo_run(
  data = NULL,
  scales = NULL,
  mode = c("teaching", "research"),
  guidance = nomo_defaults(),
  decisions = list(),
  settings = list(),
  resume = NULL
)
```

## Arguments

- data:

  A non-empty data frame or a
  [`nomo_split()`](https://juhalt.github.io/nomologR/reference/nomo_split.md)
  object. A split uses calibration rows for exploratory stages and
  validation rows for CFA, reliability, validity, and invariance. A
  network branch receives the split object so the same prespecified
  network can be evaluated across calibration and validation samples.

- scales:

  A non-empty named list. Each element is a character vector of
  candidate item-column names for one scale/construct, at least three
  per scale, since factor-retention evidence needs three. May also be a
  handoff from `contentvalidR`'s `content_handoff()`, whose carried
  items and construct mapping then define the scales, as described for
  [`nomo_screen()`](https://juhalt.github.io/nomologR/reference/nomo_screen.md).
  A handoff from a review with no construct mapping is refused, because
  a guided run needs scales and nomologR does not invent them.

- mode:

  Presentation mode: `"teaching"` or `"research"`. Mode changes
  presentation, not statistical behavior.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).
  `auto_delete` and `auto_respecify` cannot be `TRUE`, since nomologR
  never deletes an item or respecifies a model.

- decisions:

  Named list of explicit researcher decisions. Decisions may be supplied
  one pause at a time or all at once for a fully prespecified one-call
  run. A structured decision can be written as
  `list(value = ..., rationale = "...")`.

- settings:

  Named list of stage-specific argument lists. Examples include
  `list(factors = list(n_iter = 200, seed = 2026))`,
  `list(invariance = list(group = "group"))`, or
  `list(network = list(hypotheses = h))`. Pipeline-controlled arguments
  such as component data/model inputs cannot be overridden through
  `settings`. `settings$invariance$group` must name a column of the data
  the confirmatory stages use. When resuming, settings for future stages
  may be supplied without recomputing completed stages.

  `list(screen = list(effort = TRUE))` adds careless-responding indices
  (see
  [`nomo_screen()`](https://juhalt.github.io/nomologR/reference/nomo_screen.md)).
  They describe a respondent across the whole instrument, and even-odd
  consistency cannot be computed within one scale. So they are computed
  once, over every item in the run with the run's scales, and never
  inside the per-scale item audits. `reverse`, `scale_range`,
  `pair_magnitude`, and `scales` may be given alongside `effort`.
  `reverse` and `scale_range` also reach the per-scale item audits, with
  or without `effort`, and `reverse` must name items in the run's
  scales. When the scales came from a `contentvalidR` handoff, each of
  `reverse` and `scale_range` is taken from its declared keying unless
  given here, as in
  [`nomo_screen()`](https://juhalt.github.io/nomologR/reference/nomo_screen.md);
  giving one replaces only that one. The decision log records a declared
  value replaced, or a response scale the handoff did not record
  supplied here.

  The invariance and network stages refit the measurement model with the
  CFA stage's `ordered`, `estimator`, and `missing` from `settings$cfa`,
  unless `settings$invariance` or `settings$network` names its own
  (`NULL` for the default). The decision log records what was inherited
  and any value that differs from the CFA's.

  Two further requests attach evidence to the measurement model:

  - `list(scores = list(method = "sum"))` scores it with
    [`nomo_scores()`](https://juhalt.github.io/nomologR/reference/nomo_scores.md),
    using a method the researcher names; nomologR does not choose one.

  - `list(missing = list())` compares missing-data strategies with
    [`nomo_missing()`](https://juhalt.github.io/nomologR/reference/nomo_missing.md).
    `strategies`, lavaan `missing` options such as `"listwise"` or
    `"ml"`, and `reliability` may be given. The comparison covers the
    network too when one is requested.

  Both run after convergent and discriminant evidence, so they are in
  view when the researcher decides whether to carry the model forward.
  Neither is a stage of its own. Requested evidence that cannot be
  computed is recorded in the design log, and the workflow continues,
  since no later stage depends on it.

- resume:

  Optional prior `nomo_run` object. When supplied, the existing source
  data, scales, guidance, completed component results, decisions, and
  provenance are reused.

## Value

A `nomo_run` object. The fields to read are:

- `status` and `next_stage`: where the run is.

- `results`: each component's result, by stage and then by scale; for
  example `results$screen$Agency` is a `nomo_screen` object.

- `stage_status`: one row per stage.

- `decision_requests`: the decisions the run is waiting for.

- `decision_log`: the workflow's own log, one row per design choice,
  decision, or recorded event, with the columns `id`, `stage`, `scope`,
  `observation`, `reason`, `options`, `consequence`, `decision`,
  `rationale`, and `source`. `source` is `"researcher_input"` (given in
  the call, such as the scales or a setting), `"researcher_decision"` (a
  decision at a pause, or a revision), `"content_review"` (from a
  `contentvalidR` handoff), or `"pipeline"` (recorded by the workflow).
  These columns differ from those of the components' decision logs,
  which `nomo_table(x, "component_log")` returns with their own columns.

- `scales`, `mode`, `sample_design`, `sample_n`, `decisions`, and
  `settings`.

[`print()`](https://rdrr.io/r/base/print.html) shows where the run is,
one line of key evidence per component, and the decision the run waits
for or why it is blocked.
[`summary()`](https://rdrr.io/r/base/summary.html) adds every stage, the
scales, the recorded decisions, the component recipe, the methods used,
and each flag the components raised.

[`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md)
returns the run's tables, including the component recipe, the settings
with their values, the component decision logs, and the revision
lineage. Other fields hold the source data and state needed to resume or
revise the run. They may change between releases and are not part of the
stable interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

## Details

The guided workflow is resumable. Completed component objects are
retained rather than recomputed. Future-stage settings may be added or
revised until that stage runs. Settings given when resuming are merged
argument by argument: a named argument is added or replaces the earlier
value, the others keep theirs, and the decision log records the change.
Settings are locked for a stage that has completed or blocked, for an
invariance or network branch a completed run marked not requested, and
for `scores` and `missing` once the CFA has been fitted, since they
would never run; a new `nomo_run()` is then the way to request them. At
the pause after a `"revise"` decision, settings for invariance, the
network, `scores`, or `missing` may still be given when this run has not
computed that evidence:
[`nomo_revise()`](https://juhalt.github.io/nomologR/reference/nomo_revise.md)
carries the settings into the revision, which runs it.

Consequential decisions are currently:

1.  `factor_count`: the EFA factor count for each named scale;

2.  `cfa_model`: the prespecified confirmatory measurement model;

3.  `measurement_model`: `"proceed"` or `"revise"` after reviewing CFA,
    reliability, and convergent/discriminant evidence.

Optional invariance and nomological-network branches are requested
explicitly through `settings$invariance` and `settings$network`. A
network branch must include a `nomo_hypotheses` object. Invariance
diagnostics never free parameters automatically, and network results
never collapse to a one-number validity score.

## References

Boateng, G. O., Neilands, T. B., Frongillo, E. A., Melgar-Quiñonez, H.
R., & Young, S. L. (2018). Best practices for developing and validating
scales for health, social, and behavioral research: A primer. *Frontiers
in Public Health, 6*, 149.
[doi:10.3389/fpubh.2018.00149](https://doi.org/10.3389/fpubh.2018.00149)

Clark, L. A., & Watson, D. (1995). Constructing validity: Basic issues
in objective scale development. *Psychological Assessment, 7*(3),
309-319.
[doi:10.1037/1040-3590.7.3.309](https://doi.org/10.1037/1040-3590.7.3.309)

Flake, J. K., & Fried, E. I. (2020). Measurement schmeasurement:
Questionable measurement practices and how to avoid them. *Advances in
Methods and Practices in Psychological Science, 3*(4), 456-465.
[doi:10.1177/2515245920952393](https://doi.org/10.1177/2515245920952393)

Flake, J. K., Pek, J., & Hehman, E. (2017). Construct validation in
social and personality research: Current practice and recommendations.
*Social Psychological and Personality Science, 8*(4), 370-378.
[doi:10.1177/1948550617693063](https://doi.org/10.1177/1948550617693063)

Hinkin, T. R. (1998). A brief tutorial on the development of measures
for use in survey questionnaires. *Organizational Research Methods,
1*(1), 104-121.
[doi:10.1177/109442819800100106](https://doi.org/10.1177/109442819800100106)

Simmons, J. P., Nelson, L. D., & Simonsohn, U. (2011). False-positive
psychology: Undisclosed flexibility in data collection and analysis
allows presenting anything as significant. *Psychological Science,
22*(11), 1359-1366.
[doi:10.1177/0956797611417632](https://doi.org/10.1177/0956797611417632)

## Examples

``` r
scales <- list(
  Agency = c("ag1", "ag2", "ag3", "ag4"),
  Persistence = c("pe1", "pe2", "pe3", "pe4")
)

# Evidence is computed, then the run pauses for the factor-count decision.
run <- nomo_run(
  data = nomo_demo_network,
  scales = scales,
  settings = list(factors = list(n_iter = 20, seed = 2026))
)
run
#> <nomo_run> Guided workflow
#> Status: Paused | Mode: teaching | Sample design: same sample
#> Exploratory cases: 800 | Confirmatory cases: 800 | Scales: 2
#> Completed: screen -> factors | Next: EFA
#> 
#> Key evidence
#>   - Item audit: 8 items; flags: none
#>   - Parallel analysis suggests: Agency 1, Persistence 1
#> 
#> Researcher decision required on the factor counts
#>   Reason: The EFA factor count changes the fitted model. Retention evidence
#>   can inform that choice, but it does not authorize the pipeline to choose for
#>   the researcher.
#>   Options: Inspect the full `nomo_factors` result, compare plausible
#>   neighboring solutions when appropriate, and supply a positive integer for
#>   each scale.
#>   Consequence: No EFA is fitted until an explicit researcher factor-count
#>   decision is supplied.
#>   - Agency: Parallel analysis currently suggests 1 factor; the retained
#>     plausible set is 1. The pipeline has not adopted a factor count.
#>   - Persistence: Parallel analysis currently suggests 1 factor; the retained
#>     plausible set is 1. The pipeline has not adopted a factor count.
#>   Example: decisions = list(factor_count = c(Agency = <integer>,
#>   Persistence = <integer>))
#> 
#> No later stage has been run automatically while this consequential decision is
#> unresolved.
#> 
#> EFA = exploratory factor analysis.
#> 
#> See summary(x) for the stages and recorded decisions and
#> nomo_table(x, "requests") for the decision requests.
nomo_table(run, "requests")
#> # A tibble: 2 × 8
#>   id                  stage scope observation reason options consequence example
#>   <chr>               <chr> <chr> <chr>       <chr>  <chr>   <chr>       <chr>  
#> 1 factor_count:Agency efa   Agen… Parallel a… The E… Inspec… No EFA is … decisi…
#> 2 factor_count:Persi… efa   Pers… Parallel a… The E… Inspec… No EFA is … decisi…

# \donttest{
# Each decision resumes the run, which computes the next evidence and pauses
# again.
run <- nomo_run(
  resume = run,
  decisions = list(
    factor_count = list(
      value = c(Agency = 1, Persistence = 1),
      rationale = "Each scale was written to measure one construct."
    )
  )
)

run <- nomo_run(
  resume = run,
  decisions = list(
    cfa_model = list(
      value = "Agency =~ ag1 + ag2 + ag3 + ag4; Persistence =~ pe1 + pe2 + pe3 + pe4",
      rationale = "Prespecified two-construct measurement model."
    )
  )
)

run <- nomo_run(
  resume = run,
  decisions = list(
    measurement_model = list(
      value = "proceed",
      rationale = "Measurement evidence reviewed before downstream analyses."
    )
  )
)
nomo_table(run, "stages")
#> # A tibble: 8 × 3
#>   stage       status        detail                                              
#>   <chr>       <chr>         <chr>                                               
#> 1 screen      completed     Candidate items audited exactly as supplied; no dat…
#> 2 factors     completed     Factor-retention evidence computed; no factor count…
#> 3 efa         completed     EFA fitted using explicit researcher factor-count d…
#> 4 cfa         completed     Researcher-specified CFA estimated; the model was n…
#> 5 reliability completed     Reliability evidence computed from the retained CFA…
#> 6 validity    completed     Convergent/discriminant evidence computed; no valid…
#> 7 invariance  not_requested Measurement invariance was not requested in this wo…
#> 8 network     not_requested A theory-specified nomological network was not requ…
# }
```
