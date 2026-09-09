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

## Examples

``` r
if (FALSE) { # \dontrun{
h <- nomo_hypotheses(
  "WellBeing -> criterion" = positive(min = .20)
)

run <- nomo_run(
  data = dat,
  scales = list(WellBeing = c("w1", "w2", "w3", "w4")),
  settings = list(
    factors = list(seed = 2026),
    network = list(hypotheses = h)
  )
)

run <- nomo_run(
  resume = run,
  decisions = list(factor_count = 1L)
)

run <- nomo_run(
  resume = run,
  decisions = list(
    cfa_model = list(
      value = "WellBeing =~ w1 + w2 + w3 + w4",
      rationale = "Prespecified one-factor measurement model."
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
} # }
```
