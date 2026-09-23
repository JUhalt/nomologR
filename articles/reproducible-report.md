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

## Tables for a manuscript

A report archives a workflow. A thesis or manuscript needs tables.
[`nomo_apa_table()`](https://juhalt.github.io/nomologR/reference/nomo_apa_table.md)
formats the evidence in a result as a table in APA 7 style — a bold
number, an italic title, no vertical rules, and notes below — which
knits directly into an R Markdown or Quarto document:

``` r

nomo_apa_table(run$results$cfa, "loadings", number = 1)
```

**Table 1**

*Standardized Factor Loadings*

| Item | Agency | Persistence |
|:-----|:------:|:-----------:|
| ag1  |  0.82  |             |
| ag2  |  0.75  |             |
| ag3  |  0.71  |             |
| ag4  |  0.78  |             |
| pe1  |        |    0.77     |
| pe2  |        |    0.70     |
| pe3  |        |    0.74     |
| pe4  |        |    0.69     |

*Note.* Standardized loadings from a confirmatory factor analysis.
Estimated with ML; *N* = 800. Blank cells are loadings fixed to zero by
the model.

``` r

nomo_apa_table(run$results$cfa, "fit", number = 2)
```

**Table 2**

*Model Fit*

| Model             |  χ²   | *df* | *p*  |  CFI  |  TLI  |    RMSEA \[90% CI\]    | SRMR  |
|:------------------|:-----:|:----:|:----:|:-----:|:-----:|:----------------------:|:-----:|
| Measurement model | 18.52 |  19  | .488 | 1.000 | 1.000 | 0.000 \[0.000, 0.030\] | 0.015 |

*Note.* Estimated with ML; *N* = 800. CFI = comparative fit index; TLI =
Tucker-Lewis index; RMSEA = root mean square error of approximation;
SRMR = standardized root mean square residual. Fit indices are reported
as evidence, not against fixed cutoffs.

``` r

nomo_apa_table(run$results$reliability, number = 3)
```

**Table 3**

*Reliability Estimates*

| Construct   |  ω  |  α  |
|:------------|:---:|:---:|
| Agency      | .85 | .85 |
| Persistence | .82 | .82 |

*Note.* ω = coefficient omega; α = coefficient alpha. Coefficient alpha
assumes equal loadings and is reported alongside omega for comparison
with published work.

``` r

nomo_apa_table(run$results$invariance, number = 4)
```

**Table 4**

*Measurement Invariance Across Groups*

| Model      |   χ²   | *df* |  CFI  | RMSEA | SRMR  | ΔCFI  | ΔRMSEA | Δχ² (Δ*df*) |   *p*   |
|:-----------|:------:|:----:|:-----:|:-----:|:-----:|:-----:|:------:|:-----------:|:-------:|
| Configural | 32.75  |  38  | 1.000 | 0.000 | 0.017 |   —   |   —    |      —      |    —    |
| Metric     | 41.15  |  44  | 1.000 | 0.000 | 0.027 | .000  | 0.000  |  8.40 (6)   |  .210   |
| Scalar     | 106.08 |  50  | .977  | 0.053 | 0.044 | -.023 | 0.053  |  64.92 (6)  | \< .001 |

*Note.* Grouping variable: group. Each model adds constraints to the one
above it; changes are relative to the preceding model. Changes in fit
are reported as evidence and are not compared with fixed cutoffs.

``` r

nomo_apa_table(run$results$network, number = 5)
```

**Table 5**

*Theory-Specified Relations*

| Hypothesis                 | Prediction | Estimate \[95% CI\] |  Evidence  |
|:---------------------------|:----------:|:-------------------:|:----------:|
| H1: Agency -\> Persistence |  positive  | 0.46 \[0.39, 0.53\] | Concordant |
| H2: Agency -\> Performance |  positive  | 0.39 \[0.32, 0.45\] | Concordant |

*Note.* Estimates are on the standardized scale. Evidence describes how
each estimate relates to the prediction registered for it; it is
evidence about the prediction, not a verdict on the measure.

Notice the leading zeros. APA 7 drops the zero before a decimal point
only for a statistic that *cannot* exceed 1, so the rule follows the
statistic rather than the value it happens to take. Reliability
coefficients, correlations, CFI, and *p* values lose it; TLI, RMSEA,
SRMR, and standardized loadings keep it, because each can exceed 1 — TLI
is not bounded above, and a standardized loading does in an improper
solution. Many published tables print standardized loadings without the
zero; these tables follow the rule as written.

The notes keep the package’s reference-value language. Fit indices are
reported as evidence rather than against fixed cutoffs, and the
hypotheses table describes how each estimate relates to its prediction
without calling any of them a pass or a fail. A hypothesis specified
after the data were seen is marked with a note saying it is exploratory.

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
#> ! Report file already exists: /tmp/RtmpkSjePJ/nomologR-reports-200c7b29e887/construct-validation-report.html. Use `overwrite = TRUE` to replace it.
```

As elsewhere in `nomologR`, consequential or destructive behavior is not
silently assumed.
