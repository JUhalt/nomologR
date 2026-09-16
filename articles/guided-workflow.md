# Guided workflow with nomo_run()

[`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
is an orchestrator, not an automatic scale-decider.

Its job is to make the staged workflow easier to reproduce while
preserving the package’s central rule:

> Evidence may flow automatically. Consequential decisions do not.

This walkthrough uses `nomo_demo_network`, a simulated validation study
with three constructs (Agency, Persistence, and Social desirability), an
observed Performance outcome, and two administration groups. See
[`?nomo_demo_network`](https://juhalt.github.io/nomologR/reference/nomo_demo_network.md).

## A fresh run

``` r

scales <- list(
  Agency = c("ag1", "ag2", "ag3", "ag4"),
  Persistence = c("pe1", "pe2", "pe3", "pe4"),
  SocialDesirability = c("sd1", "sd2", "sd3")
)

run <- nomo_run(
  data = nomo_demo_network,
  scales = scales,
  settings = list(factors = list(seed = 2026))
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
#> Example: decisions = list(factor_count = c(Agency = <integer>, Persistence = <integer>, SocialDesirability = <integer>))
#> 
#> [efa / Persistence]
#> Observation: Parallel analysis currently suggests 1 factor; the retained plausible set is 1. The pipeline has not adopted a factor count.
#> Reason: The EFA factor count changes the fitted model. Retention evidence can inform that choice, but it does not authorize the pipeline to choose for the researcher.
#> Options: Inspect the full `nomo_factors` result, compare plausible neighboring solutions when appropriate, and supply a positive integer for this scale.
#> Consequence: No EFA is fitted until an explicit researcher factor-count decision is supplied.
#> Example: decisions = list(factor_count = c(Agency = <integer>, Persistence = <integer>, SocialDesirability = <integer>))
#> 
#> [efa / SocialDesirability]
#> Observation: Parallel analysis currently suggests 1 factor; the retained plausible set is 1. The pipeline has not adopted a factor count.
#> Reason: The EFA factor count changes the fitted model. Retention evidence can inform that choice, but it does not authorize the pipeline to choose for the researcher.
#> Options: Inspect the full `nomo_factors` result, compare plausible neighboring solutions when appropriate, and supply a positive integer for this scale.
#> Consequence: No EFA is fitted until an explicit researcher factor-count decision is supplied.
#> Example: decisions = list(factor_count = c(Agency = <integer>, Persistence = <integer>, SocialDesirability = <integer>))
#> 
#> No later stage has been run automatically while this consequential decision is unresolved.
```

The initial run audits each scale’s items and computes factor-retention
evidence. It then pauses before EFA. The factor count is not silently
copied from parallel analysis.

``` r

nomo_table(run, "requests")
#> # A tibble: 3 × 8
#>   id                  stage scope observation reason options consequence example
#>   <chr>               <chr> <chr> <chr>       <chr>  <chr>   <chr>       <chr>  
#> 1 factor_count:Agency efa   Agen… Parallel a… The E… Inspec… No EFA is … decisi…
#> 2 factor_count:Persi… efa   Pers… Parallel a… The E… Inspec… No EFA is … decisi…
#> 3 factor_count:Socia… efa   Soci… Parallel a… The E… Inspec… No EFA is … decisi…
```

Each decision request separates:

- **Observation** — what the evidence shows;
- **Reason** — why a researcher decision is required;
- **Options** — what can defensibly happen next;
- **Consequence** — what the choice changes.

## Resume with an explicit factor count

``` r

run <- nomo_run(
  resume = run,
  decisions = list(
    factor_count = list(
      value = c(Agency = 1, Persistence = 1, SocialDesirability = 1),
      rationale = paste(
        "Each scale was written to measure one construct, and the",
        "retention evidence was reviewed before choosing one factor."
      )
    )
  )
)

run
#> <nomo_run>
#> Guided nomologR workflow | teaching mode
#> Status: PAUSED
#> Sample design: same_sample | Exploratory N = 800 | Confirmatory N = 800
#> Completed stages: screen -> factors -> efa
#> Next stage: cfa
#> 
#> Researcher decision required
#> 
#> [cfa / measurement_model]
#> Observation: EFA has completed for every supplied scale. No confirmatory measurement model has been constructed or fitted.
#> Reason: CFA syntax encodes consequential choices about item retention, factor membership, cross-loadings, and correlated residuals; these cannot be chosen silently.
#> Options: Inspect the EFA evidence and theory, then supply a prespecified lavaan measurement model (or `nomo_model()` object).
#> Consequence: The CFA will use the same sample unless a new workflow is started with a holdout/external design. Same-sample confirmation must remain labeled as such.
#> Example: decisions = list(cfa_model = list(value = model, rationale = "Prespecified measurement model"))
#> 
#> No later stage has been run automatically while this consequential decision is unresolved.
```

The completed screening and factor-retention objects are reused. EFA is
fitted using the explicit factor-count decision, with the retention
stage’s correlation and modeling context retained. The workflow then
pauses before CFA model specification.

## Confirmatory model handoff

``` r

run <- nomo_run(
  resume = run,
  decisions = list(
    cfa_model = list(
      value = nomo_model(scales),
      rationale = "Prespecified three-construct measurement model."
    )
  )
)

run
#> <nomo_run>
#> Guided nomologR workflow | teaching mode
#> Status: PAUSED
#> Sample design: same_sample | Exploratory N = 800 | Confirmatory N = 800
#> Completed stages: screen -> factors -> efa -> cfa -> reliability -> validity
#> Next stage: measurement_review
#> 
#> Researcher decision required
#> 
#> [measurement_review / measurement_model]
#> Observation: CFA converged and reliability/validity evidence was computed. Across these components, 0 concern and 1 review log entries are retained.
#> Reason: Invariance and nomological-network interpretations inherit the measurement model. Continuing downstream is therefore a researcher decision, not a fit-index side effect.
#> Options: Choose `proceed` to retain this prespecified model for configured downstream branches, or choose `revise` to stop here and continue with `nomo_revise()`, which records a substantively justified revised model and keeps this workflow as its parent.
#> Consequence: Proceeding does not declare the model valid and does not remove any review flags. Revising triggers no automatic parameter freeing, item deletion, or respecification.
#> Example: decisions = list(measurement_model = list(value = "proceed", rationale = "Evidence reviewed; model retained for the planned analyses."))
#> 
#> No later stage has been run automatically while this consequential decision is unresolved.
```

[`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
does not translate the EFA into CFA syntax. The researcher owns the
confirmatory model;
[`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md)
simply writes the simple-structure syntax for the scales that were
already declared.

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
CFA evidence remains strained. Therefore the pipeline does not
automatically carry the model into invariance or structural theory
tests.

``` r

nomo_table(run, "requests")
#> # A tibble: 1 × 8
#>   id                stage   scope observation reason options consequence example
#>   <chr>             <chr>   <chr> <chr>       <chr>  <chr>   <chr>       <chr>  
#> 1 measurement_model measur… meas… CFA conver… Invar… Choose… Proceeding… "decis…
```

The researcher chooses either `measurement_model = "proceed"` or
`measurement_model = "revise"`. Choosing `revise` does not trigger an
automated modification-index search, parameter freeing, or item
deletion; the current workflow remains an auditable record. To carry a
revision forward with its reasoning attached, use
[`nomo_revise()`](https://juhalt.github.io/nomologR/reference/nomo_revise.md),
shown in [Revising with a recorded
lineage](#revising-with-a-recorded-lineage) below.

## Optional invariance and network branches

Future-stage settings can be added at a pause without recomputing
completed stages.

``` r

h <- nomo_hypotheses(
  "Agency -> Persistence" = positive(min = .20),
  "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15)),
  "Agency -> Performance" = positive()
)

run <- nomo_run(
  resume = run,
  settings = list(
    invariance = list(
      group = "group",
      levels = c("configural", "metric", "scalar"),
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

run
#> <nomo_run>
#> Guided nomologR workflow | teaching mode
#> Status: COMPLETE
#> Sample design: same_sample | Exploratory N = 800 | Confirmatory N = 800
#> Completed stages: screen -> factors -> efa -> cfa -> reliability -> validity -> invariance -> network
#> Next stage: none
#> 
#> All requested stages are complete or explicitly marked not requested.
#> No hidden item deletion, model respecification, parameter freeing, or validity verdict was performed.
nomo_table(run, "stages")
#> # A tibble: 8 × 3
#>   stage       status    detail                                                  
#>   <chr>       <chr>     <chr>                                                   
#> 1 screen      completed Candidate items audited exactly as supplied; no data or…
#> 2 factors     completed Factor-retention evidence computed; no factor count was…
#> 3 efa         completed EFA fitted using explicit researcher factor-count decis…
#> 4 cfa         completed Researcher-specified CFA estimated; the model was not r…
#> 5 reliability completed Reliability evidence computed from the retained CFA mod…
#> 6 validity    completed Convergent/discriminant evidence computed; no valid/inv…
#> 7 invariance  completed Requested invariance evidence computed; diagnostics did…
#> 8 network     completed Theory-specified network evidence computed without a on…
```

The completed component objects remain available for direct inspection:

``` r

nomo_table(run$results$invariance, "fit")[, c("level", "cfi", "rmsea", "delta_cfi")]
#> # A tibble: 3 × 4
#>   level        cfi  rmsea delta_cfi
#>   <chr>      <dbl>  <dbl>     <dbl>
#> 1 configural 0.995 0.0205  NA      
#> 2 metric     0.993 0.0233  -0.00195
#> 3 scalar     0.974 0.0442  -0.0193

nomo_table(run$results$network, "hypotheses")[, c(
  "relation", "estimate", "ci_lower", "ci_upper", "concordance"
)]
#> # A tibble: 3 × 5
#>   relation                      estimate ci_lower ci_upper concordance
#>   <chr>                            <dbl>    <dbl>    <dbl> <chr>      
#> 1 Agency -> Persistence          0.458     0.389    0.526  concordant 
#> 2 Agency <-> SocialDesirability  0.00773  -0.0794   0.0948 concordant 
#> 3 Agency -> Performance          0.389     0.325    0.454  concordant
```

The invariance branch retains its diagnostics without automatically
releasing constraints (see [Measurement
invariance](https://juhalt.github.io/nomologR/articles/measurement-invariance.md)
for the `ag3` intercept). The network branch uses explicit hypotheses
and does not create a single nomological-validity score.

## Prespecified one-call workflow

Researchers who have already made the consequential decisions — for
example in a preregistration — can supply them together.
`mode = "research"` gives a more compact presentation of the same
statistical behavior:

``` r

run_prespecified <- nomo_run(
  data = nomo_demo_network,
  scales = scales,
  mode = "research",
  decisions = list(
    factor_count = c(Agency = 1, Persistence = 1, SocialDesirability = 1),
    cfa_model = nomo_model(scales),
    measurement_model = "proceed"
  ),
  settings = list(
    factors = list(seed = 2026),
    network = list(hypotheses = h)
  )
)

run_prespecified
#> <nomo_run> mode=research | status=complete | design=same_sample
#> Completed: screen -> factors -> efa -> cfa -> reliability -> validity -> network | Next: none
#> Scales: 3 | Exploratory N: 800 | Confirmatory N: 800
#> 
#> Requested workflow complete. Use `nomo_table(x, "recipe")` for the component map and `nomo_table(x, "component_log")` for retained evidence provenance.
```

This is still not hidden automation: every consequential decision was
explicit before the run began.

## Revising with a recorded lineage

Suppose the measurement evidence prompts a change.
[`nomo_revise()`](https://juhalt.github.io/nomologR/reference/nomo_revise.md)
creates a child workflow that keeps the parent as its documented
ancestor, instead of starting an unrelated run:

``` r

revised <- nomo_revise(
  run,
  cfa_model = paste(
    "Agency =~ ag1 + ag2 + ag3 + ag4",
    "Persistence =~ pe1 + pe2 + pe3 + pe4",
    "SocialDesirability =~ sd1 + sd2 + sd3",
    "ag1 ~~ ag2",
    sep = "\n"
  ),
  rationale = paste(
    "Items ag1 and ag2 use nearly identical wording, so their residual",
    "association plausibly exceeds what the common factor explains."
  ),
  origin = "post_hoc"
)

revised
#> <nomo_run>
#> Guided nomologR workflow | teaching mode
#> Status: PAUSED
#> Sample design: same_sample | Exploratory N = 800 | Confirmatory N = 800
#> Completed stages: screen -> factors -> efa -> cfa -> reliability -> validity
#> Next stage: measurement_review
#> Revisions: 1 (post-hoc); see `nomo_table(x, "lineage")`
#> 
#> Researcher decision required
#> 
#> [measurement_review / measurement_model]
#> Observation: CFA converged and reliability/validity evidence was computed. Across these components, 0 concern and 1 review log entries are retained.
#> Reason: Invariance and nomological-network interpretations inherit the measurement model. Continuing downstream is therefore a researcher decision, not a fit-index side effect.
#> Options: Choose `proceed` to retain this prespecified model for configured downstream branches, or choose `revise` to stop here and continue with `nomo_revise()`, which records a substantively justified revised model and keeps this workflow as its parent.
#> Consequence: Proceeding does not declare the model valid and does not remove any review flags. Revising triggers no automatic parameter freeing, item deletion, or respecification.
#> Example: decisions = list(measurement_model = list(value = "proceed", rationale = "Evidence reviewed; model retained for the planned analyses."))
#> 
#> No later stage has been run automatically while this consequential decision is unresolved.
```

The child reruns the staged evidence with the revised model and pauses
at the measurement review, so the revised evidence is inspected before
any downstream branch runs. The parent object is unchanged and remains a
complete record.

``` r

nomo_table(revised, "lineage")[, c(
  "revision", "change_type", "origin", "rationale", "comparison"
)]
#> # A tibble: 1 × 5
#>   revision change_type origin   rationale                             comparison
#>      <int> <chr>       <chr>    <chr>                                 <chr>     
#> 1        1 model       post_hoc Items ag1 and ag2 use nearly identic… Chi-Squar…
```

Because a revision is a claim that one model is preferable to another,
[`nomo_revise()`](https://juhalt.github.io/nomologR/reference/nomo_revise.md)
compares them with
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md):

``` r

nomo_table(revised$revision_comparison, "comparisons")[, c(
  "model", "relation", "chisq_diff", "df_diff", "p_value", "delta_cfi"
)]
#> # A tibble: 1 × 6
#>   model   relation         chisq_diff df_diff p_value delta_cfi
#>   <chr>   <chr>                 <dbl>   <dbl>   <dbl>     <dbl>
#> 1 revised less_constrained     0.0105       1   0.918 -0.000333
```

Here the revision changes almost nothing (chi-square difference = 0.01,
df = 1, p = 0.918; CFI changes by -0.0003). Once the common factor is
accounted for, the residual association these two items were expected to
share is close to zero, so the data give no reason to prefer the revised
model over its parent. A revision is not automatically an improvement,
and
[`nomo_revise()`](https://juhalt.github.io/nomologR/reference/nomo_revise.md)
does not choose: it records the attempt, its rationale, and the
comparison, so keeping the parent model is as visible as adopting the
revision.

The origin still matters. The change was prompted by inspecting these
data, so the decision log records it as post hoc and says what follows
from that:

``` r

cat(rev_log$consequence)
#> This revision was prompted by results, so it is recorded as post hoc. Data-driven respecification capitalizes on chance. The revision is evaluated on the same sample that motivated it. Confirm the revised model in independent data, for example with `nomo_split()` or a new sample.
```

Revisions chain. A revision of `revised` would carry two lineage rows,
so the full path from the original model to the reported one stays
visible in `nomo_table(x, "lineage")` and in the report’s
revision-lineage section.

## Reproducibility and provenance

``` r

nomo_table(run, "decisions")
#> # A tibble: 10 × 10
#>    id      stage scope observation reason options consequence decision rationale
#>    <chr>   <chr> <chr> <chr>       <chr>  <chr>   <chr>       <chr>    <chr>    
#>  1 sample… desi… samp… 800 rows a… Sampl… Contin… Later CFA … "same_s… ""       
#>  2 scale_… desi… Agen… Scale `Age… Item … Review… Screening,… "ag1, a… ""       
#>  3 scale_… desi… Pers… Scale `Per… Item … Review… Screening,… "pe1, p… ""       
#>  4 scale_… desi… Soci… Scale `Soc… Item … Review… Screening,… "sd1, s… ""       
#>  5 factor… efa   Agen… Parallel a… The E… Inspec… A 1-factor… "1"      "Each sc…
#>  6 factor… efa   Pers… Parallel a… The E… Inspec… A 1-factor… "1"      "Each sc…
#>  7 factor… efa   Soci… Parallel a… The E… Inspec… A 1-factor… "1"      "Each sc…
#>  8 cfa_mo… cfa   meas… EFA has co… CFA s… Inspec… The CFA wi… "Agency… "Prespec…
#>  9 measur… meas… meas… CFA conver… Invar… Choose… Proceeding… "procee… "Measure…
#> 10 workfl… work… pipe… All reques… The p… Inspec… No additio… "comple… ""       
#> # ℹ 1 more variable: source <chr>
nomo_table(run, "settings")
#> # A tibble: 8 × 3
#>   stage       configured setting_names            
#>   <chr>       <lgl>      <chr>                    
#> 1 screen      FALSE      ""                       
#> 2 factors     TRUE       "seed"                   
#> 3 efa         FALSE      ""                       
#> 4 cfa         FALSE      ""                       
#> 5 reliability FALSE      ""                       
#> 6 validity    FALSE      ""                       
#> 7 invariance  TRUE       "group, levels, localize"
#> 8 network     TRUE       "hypotheses"
nomo_table(run, "recipe")
#> # A tibble: 14 × 6
#>    stage       scope           function_name data_role status researcher_control
#>    <chr>       <chr>           <chr>         <chr>     <chr>  <chr>             
#>  1 screen      Agency          nomo_screen() explorat… compl… candidate item me…
#>  2 factors     Agency          nomo_factors… explorat… compl… retention evidenc…
#>  3 efa         Agency          nomo_efa()    explorat… compl… explicit `factor_…
#>  4 screen      Persistence     nomo_screen() explorat… compl… candidate item me…
#>  5 factors     Persistence     nomo_factors… explorat… compl… retention evidenc…
#>  6 efa         Persistence     nomo_efa()    explorat… compl… explicit `factor_…
#>  7 screen      SocialDesirabi… nomo_screen() explorat… compl… candidate item me…
#>  8 factors     SocialDesirabi… nomo_factors… explorat… compl… retention evidenc…
#>  9 efa         SocialDesirabi… nomo_efa()    explorat… compl… explicit `factor_…
#> 10 cfa         measurement_mo… nomo_cfa()    confirma… compl… explicit `cfa_mod…
#> 11 reliability measurement_mo… nomo_reliabi… confirma… compl… evidence computed…
#> 12 validity    measurement_mo… nomo_validit… confirma… compl… evidence computed…
#> 13 invariance  configured_bra… nomo_invaria… confirma… compl… requested through…
#> 14 network     configured_bra… nomo_network… same sam… compl… requested through…
head(nomo_table(run, "component_log"), 10)
#> # A tibble: 10 × 12
#>    pipeline_component pipeline_scope stage   object  metric      value reference
#>    <chr>              <chr>          <chr>   <chr>   <chr>       <dbl> <chr>    
#>  1 factors            Agency         factors correl… corre… NA         Indicato…
#>  2 factors            Agency         factors cases   pairw…  8   e+  2 Minimum …
#>  3 factors            Agency         factors correl… kmo     8.21e-  1 0.60/0.5…
#>  4 factors            Agency         factors correl… bartl…  1.89e-279 Supporti…
#>  5 factors            Agency         factors retent… paral…  1   e+  0 Common-f…
#>  6 factors            Agency         factors retent… paral…  1   e+  0 Agreemen…
#>  7 factors            Agency         factors retent… map_o…  1   e+  0 Velicer …
#>  8 factors            Agency         factors retent… map_r…  1   e+  0 Velicer …
#>  9 factors            Agency         factors retent… ekc     1   e+  0 Braeken …
#> 10 factors            Agency         factors retent… reten…  3   e+  0 Converge…
#> # ℹ 5 more variables: severity <chr>, observation <chr>, recommendation <chr>,
#> #   decision <chr>, rationale <chr>
```

The guided object retains:

- source sample roles;
- supplied scale membership;
- component settings;
- explicit decisions and rationales;
- complete component result objects;
- outstanding decision requests;
- component decision and evidence logs;
- the sequence of resume calls.

This structure feeds the reproducible report described in [Archiving a
nomologR
workflow](https://juhalt.github.io/nomologR/articles/reproducible-report.md).

## Research basis

Staged scale development with explicit decisions follows guidance such
as Clark and Watson (1995, 2019), Hinkin (1998), and Boateng et
al. (2018). Recording decisions and rationales responds to evidence that
undisclosed analytic flexibility undermines measurement claims (Simmons,
Nelson, & Simonsohn, 2011; Flake, Pek, & Hehman, 2017; Flake & Fried,
2020). Full references are in
[`?nomo_run`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
and the [research
basis](https://juhalt.github.io/nomologR/articles/research-basis.md)
article.
