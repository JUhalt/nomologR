# Archiving a nomologR workflow with nomo_report()

[`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
is the final presentation and provenance layer in the `nomologR`
workflow.

It does **not** rerun the analyses. It renders the evidence and
researcher decisions already stored in a `nomo_run` object.

## A workflow to archive

This example uses `nomo_demo_network` and supplies every consequential
decision up front, as a researcher following a preregistered plan might:

``` r

scales <- list(
  Agency = c("ag1", "ag2", "ag3", "ag4"),
  Persistence = c("pe1", "pe2", "pe3", "pe4")
)

h <- nomo_hypotheses(
  "Agency -> Persistence" = positive(min = .20),
  "Agency -> Performance" = positive()
)

run <- nomo_run(
  data = nomo_demo_network,
  scales = scales,
  mode = "research",
  decisions = list(
    factor_count = list(
      value = c(Agency = 1, Persistence = 1),
      rationale = "Each scale was written to measure one construct."
    ),
    cfa_model = list(
      value = nomo_model(scales),
      rationale = "Prespecified two-construct measurement model."
    ),
    measurement_model = list(
      value = "proceed",
      rationale = "Measurement evidence reviewed before planned downstream tests."
    )
  ),
  settings = list(
    factors = list(seed = 2026),
    invariance = list(group = "group", levels = c("configural", "metric", "scalar")),
    network = list(hypotheses = h)
  )
)

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
nomo_table(run, "decisions")
#> # A tibble: 8 × 10
#>   id       stage scope observation reason options consequence decision rationale
#>   <chr>    <chr> <chr> <chr>       <chr>  <chr>   <chr>       <chr>    <chr>    
#> 1 sample_… desi… samp… 800 rows a… Sampl… Contin… Later CFA … "same_s… ""       
#> 2 scale_d… desi… Agen… Scale `Age… Item … Review… Screening,… "ag1, a… ""       
#> 3 scale_d… desi… Pers… Scale `Per… Item … Review… Screening,… "pe1, p… ""       
#> 4 factor_… efa   Agen… Parallel a… The E… Inspec… A 1-factor… "1"      "Each sc…
#> 5 factor_… efa   Pers… Parallel a… The E… Inspec… A 1-factor… "1"      "Each sc…
#> 6 cfa_mod… cfa   meas… EFA has co… CFA s… Inspec… The CFA wi… "Agency… "Prespec…
#> 7 measure… meas… meas… CFA conver… Invar… Choose… Proceeding… "procee… "Measure…
#> 8 workflo… work… pipe… All reques… The p… Inspec… No additio… "comple… ""       
#> # ℹ 1 more variable: source <chr>
```

## Render a report

``` r

report_file <- nomo_report(
  run,
  file = file.path(report_dir, "construct-validation-report.html"),
  title = "Construct validation audit: Agency and Persistence"
)

file.exists(report_file)
#> [1] TRUE
```

The HTML file is self-contained, so it can be archived, attached to a
project record or thesis appendix, or shared with a collaborator without
a separate figures directory or a network dependency. The `title`
argument becomes the document title.

[`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
works the same from the console, a script, or a chunk inside your own R
Markdown or Quarto document, such as a thesis chapter. This article
renders reports from its own chunks and needs no special setup.

Rendering a report from inside another document nests two renders that
share knitr’s state, so
[`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
isolates them: the report is rendered with knitr’s default chunk
options, which its template then sets for itself, and your document’s
options are restored afterwards. Your chunk options therefore do not
change the report, and rendering the report does not change your
document.

## What the report contains

The report mirrors the full guided workflow:

1.  researcher inputs and data/sample roles;
2.  item audit;
3.  factor-retention evidence;
4.  EFA;
5.  CFA;
6.  reliability;
7.  convergent/discriminant evidence;
8.  invariance, if requested;
9.  nomological-network evidence, if requested;
10. workflow decisions and outstanding decisions;
11. deviations and post-hoc decisions;
12. methods and citations: the methods this workflow actually used, each
    with its lineage and role, a reference list for those methods, and
    software citations;
13. reproducibility information and session details.

The methods table is built from `nomo_methods(run)`, which returns only
the methods the run used; a method the package offers but the workflow
did not use is not cited. The [research
basis](https://juhalt.github.io/nomologR/articles/research-basis.md)
article explains the lineage labels.

A final evidence-trace appendix keeps each autogenerated recommendation
on the same row as the component, metric, reference, severity, and
observation that produced it.

## Reports from incomplete workflows

A report can also be rendered from a paused or blocked workflow:

``` r

paused_run <- nomo_run(
  data = nomo_demo_network,
  scales = scales,
  settings = list(factors = list(seed = 2026))
)

nomo_table(paused_run, "stages")
#> # A tibble: 8 × 3
#>   stage       status            detail                                          
#>   <chr>       <chr>             <chr>                                           
#> 1 screen      completed         "Candidate items audited exactly as supplied; n…
#> 2 factors     completed         "Factor-retention evidence computed; no factor …
#> 3 efa         awaiting_decision "Explicit researcher factor-count decisions are…
#> 4 cfa         not_started       ""                                              
#> 5 reliability not_started       ""                                              
#> 6 validity    not_started       ""                                              
#> 7 invariance  not_started       ""                                              
#> 8 network     not_started       ""
```

``` r

paused_file <- nomo_report(
  paused_run,
  file = file.path(report_dir, "paused-workflow-audit.html"),
  include_plots = FALSE
)

file.exists(paused_file)
#> [1] TRUE
```

Missing stages are labeled as incomplete or not requested rather than
silently disappearing. This is useful when a researcher wants to archive
why an analysis stopped or which consequential decision remained
unresolved.

## Plots and session information

The default includes a compact, nonredundant set of plots and full
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) output. Both
can be turned off, as in the paused report above:

``` r

nomo_report(
  run,
  file = "compact-report.html",
  include_plots = FALSE,
  include_session = FALSE
)
```

## Overwriting is explicit

Existing reports are protected. Rendering to a file that already exists
stops unless `overwrite = TRUE` is supplied:

``` r

nomo_report(run, file = report_file)
#> Error:
#> ! Report file already exists: /tmp/Rtmpu7075z/nomologR-reports-1f7c64f6f3fc/construct-validation-report.html. Use `overwrite = TRUE` to replace it.
```

As elsewhere in `nomologR`, consequential or destructive behavior is not
silently assumed.
