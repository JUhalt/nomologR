# Guided workflow with nomo_run()

[`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
is an orchestrator, not an automatic scale-decider.

Its job is to make the staged workflow easier to reproduce while
preserving the package’s central rule:

> Evidence may flow automatically. Consequential decisions do not.

## A fresh run

``` r

run <- nomo_run(
  data = dat,
  scales = list(
    WellBeing = c("w1", "w2", "w3", "w4")
  )
)
```

The initial run audits the supplied items and computes factor-retention
evidence. It then pauses before EFA.

The factor count is not silently copied from parallel analysis.

``` r

nomo_table(run, "requests")
```

The decision request separates:

- **Observation** - what the evidence shows;
- **Reason** - why a researcher decision is required;
- **Options** - what can defensibly happen next;
- **Consequence** - what the choice changes.

## Resume with an explicit factor count

``` r

run <- nomo_run(
  resume = run,
  decisions = list(
    factor_count = list(
      value = 1L,
      rationale = "Retention evidence and theory support one exploratory factor."
    )
  )
)
```

The completed screening and factor-retention objects are reused. EFA is
fitted using the explicit factor-count decision, with M2
correlation/modeling context retained.

The workflow then pauses before CFA model specification.

## Confirmatory model handoff

``` r

run <- nomo_run(
  resume = run,
  decisions = list(
    cfa_model = list(
      value = "WellBeing =~ w1 + w2 + w3 + w4",
      rationale = "Prespecified one-factor confirmatory model."
    )
  )
)
```

[`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
does not translate the EFA into CFA syntax. The researcher owns the
confirmatory model.

Once supplied, the pipeline computes:

``` text
nomo_cfa()
  |
  +-- nomo_reliability()
  |
  +-- nomo_validity()
```

and pauses again.

## Why pause after measurement evidence?

A model can converge and still contain evidence that deserves review.
Reliability or AVE can look strong while construct separation or local
CFA evidence remains strained.

Therefore the pipeline does not automatically carry the model into
invariance or structural theory tests.

``` r

nomo_table(run, "requests")
```

The researcher chooses either:

``` r

measurement_model = "proceed"
```

or:

``` r

measurement_model = "revise"
```

Choosing `revise` does not trigger automated modification-index search,
parameter freeing, or item deletion. The current workflow remains an
auditable record and the revised analysis starts as a new run.

## Optional invariance and network branches

Future-stage settings can be added at a pause without recomputing
completed stages.

``` r

h <- nomo_hypotheses(
  "WellBeing -> criterion" = positive(min = .20)
)

run <- nomo_run(
  resume = run,
  settings = list(
    invariance = list(
      group = "group",
      levels = c("configural", "metric"),
      localize = TRUE
    ),
    network = list(
      hypotheses = h
    )
  )
)
```

Then continue explicitly:

``` r

run <- nomo_run(
  resume = run,
  decisions = list(
    measurement_model = list(
      value = "proceed",
      rationale = "Measurement evidence reviewed before planned downstream tests."
    )
  )
)
```

The invariance branch retains diagnostics without automatically
releasing constraints. The network branch uses explicit hypotheses and
does not create a single nomological-validity score.

## Prespecified one-call workflow

Researchers who have already made the consequential decisions can supply
them together:

``` r

run <- nomo_run(
  data = dat,
  scales = list(
    WellBeing = c("w1", "w2", "w3", "w4")
  ),
  decisions = list(
    factor_count = 1L,
    cfa_model = "WellBeing =~ w1 + w2 + w3 + w4",
    measurement_model = "proceed"
  ),
  settings = list(
    factors = list(seed = 2026),
    network = list(hypotheses = h)
  )
)
```

This is still not hidden automation: every consequential decision was
explicit before the run began.

## Reproducibility and provenance

``` r

nomo_table(run, "stages")
nomo_table(run, "decisions")
nomo_table(run, "settings")
nomo_table(run, "recipe")
nomo_table(run, "component_log")
```

The guided object retains:

- source sample roles;
- supplied scale membership;
- component settings;
- explicit decisions and rationales;
- complete component result objects;
- outstanding decision requests;
- component decision/evidence logs;
- the sequence of resume calls.

This structure is designed to feed the reproducible
[`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
workflow in Milestone 9.
