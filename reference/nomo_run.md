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
  candidate item-column names for one scale/construct.

- mode:

  Presentation mode: `"teaching"` or `"research"`. Mode changes
  presentation, not statistical behavior.

- guidance:

  Guidance settings from
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

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
  `settings`. When resuming, settings for future stages may be supplied
  without recomputing completed stages.

- resume:

  Optional prior `nomo_run` object. When supplied, the existing source
  data, scales, guidance, completed component results, decisions, and
  provenance are reused.

## Value

A `nomo_run` object containing component results, stage status,
outstanding decision requests, researcher decisions/rationales, stage
settings, workflow provenance, component decision logs, and source
inputs required for reproducibility.

## Details

The guided workflow is resumable. Completed component objects are
retained rather than recomputed. Future-stage settings may be added or
revised until that stage has completed; settings for completed/blocked
stages are locked.

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
# \donttest{
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
#> <nomo_run>
#> Guided nomologR workflow | teaching mode
#> Status: PAUSED
#> Sample design: same_sample | Exploratory N = 800 | Confirmatory N = 800
#> Completed stages: screen -> factors
#> Next stage: efa
#> 
#> Researcher decision required
#> 
#> [efa / Agency]
#> Observation: Parallel analysis currently suggests 1 factor; the retained plausible set is 1. The pipeline has not adopted a factor count.
#> Reason: The EFA factor count changes the fitted model. Retention evidence can inform that choice, but it does not authorize the pipeline to choose for the researcher.
#> Options: Inspect the full `nomo_factors` result, compare plausible neighboring solutions when appropriate, and supply a positive integer for this scale.
#> Consequence: No EFA is fitted until an explicit researcher factor-count decision is supplied.
#> Example: decisions = list(factor_count = c(Agency = <integer>, Persistence = <integer>))
#> 
#> [efa / Persistence]
#> Observation: Parallel analysis currently suggests 1 factor; the retained plausible set is 1. The pipeline has not adopted a factor count.
#> Reason: The EFA factor count changes the fitted model. Retention evidence can inform that choice, but it does not authorize the pipeline to choose for the researcher.
#> Options: Inspect the full `nomo_factors` result, compare plausible neighboring solutions when appropriate, and supply a positive integer for this scale.
#> Consequence: No EFA is fitted until an explicit researcher factor-count decision is supplied.
#> Example: decisions = list(factor_count = c(Agency = <integer>, Persistence = <integer>))
#> 
#> No later stage has been run automatically while this consequential decision is unresolved.
nomo_table(run, "requests")
#> # A tibble: 2 × 8
#>   id                  stage scope observation reason options consequence example
#>   <chr>               <chr> <chr> <chr>       <chr>  <chr>   <chr>       <chr>  
#> 1 factor_count:Agency efa   Agen… Parallel a… The E… Inspec… No EFA is … decisi…
#> 2 factor_count:Persi… efa   Pers… Parallel a… The E… Inspec… No EFA is … decisi…

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
