# Get started with nomologR

``` r

library(nomologR)
```

## Who nomologR is for

`nomologR` is written for three audiences:

- **graduate students** (master’s and doctoral) who are learning scale
  development and construct validation and need to understand *why* each
  analysis is performed, not only how to run it;
- **faculty and advisors** who teach measurement and want output that
  explains its own reasoning; and
- **researchers** who need a defensible, reproducible record of how a
  measure was evaluated.

The package coordinates established engines — `psych`, `EFAtools`,
`lavaan`, and `semTools` — and adds a teaching layer: diagnostics framed
as evidence, explanations tied to the methodological literature, and a
record of every consequential researcher decision.

## The question behind every stage

At each step, `nomologR` tries to help answer one question:

> **What does this result tell me about the claim that these
> observations measure the construct I say they measure, and what should
> I investigate next?**

The package flags, explains, and documents. It never silently deletes
items, respecifies models, or declares a scale “valid.”

## The workflow at a glance

| Stage | Question it answers | Functions | Walkthrough |
|----|----|----|----|
| 1\. Item and data audit | What do the items and responses look like? | [`nomo_screen()`](https://juhalt.github.io/nomologR/reference/nomo_screen.md) | [From item audit to exploratory structure](https://juhalt.github.io/nomologR/articles/exploratory-workflow.md) |
| 2\. Dimensionality | How many latent dimensions deserve investigation? | [`nomo_factors()`](https://juhalt.github.io/nomologR/reference/nomo_factors.md) | [From item audit to exploratory structure](https://juhalt.github.io/nomologR/articles/exploratory-workflow.md) |
| 3\. Exploratory structure | What does a requested factor solution look like? | [`nomo_efa()`](https://juhalt.github.io/nomologR/reference/nomo_efa.md) | [From item audit to exploratory structure](https://juhalt.github.io/nomologR/articles/exploratory-workflow.md) |
| 4\. Confirmatory measurement model | Does a prespecified model reproduce the data, and where is there strain? | [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md), [`nomo_split()`](https://juhalt.github.io/nomologR/reference/nomo_split.md), [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md) | [From CFA to a defensible measurement model](https://juhalt.github.io/nomologR/articles/measurement-model-evidence.md) |
| 5\. Reliability and construct-validity evidence | How precise are scores, and are constructs distinguishable? | [`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md), [`nomo_validity()`](https://juhalt.github.io/nomologR/reference/nomo_validity.md) | [From CFA to a defensible measurement model](https://juhalt.github.io/nomologR/articles/measurement-model-evidence.md) |
| 6\. Generalizability | Is the construct measured comparably across groups? | [`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md), [`nomo_partial()`](https://juhalt.github.io/nomologR/reference/nomo_partial.md) | [Measurement invariance](https://juhalt.github.io/nomologR/articles/measurement-invariance.md) |
| 7\. Nomological network | Does the construct relate to others as theory predicted? | [`nomo_hypotheses()`](https://juhalt.github.io/nomologR/reference/nomo_hypotheses.md), [`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md) | [Theory-specified nomological networks](https://juhalt.github.io/nomologR/articles/nomological-network.md) |
| Guided workflow | How do the stages fit together with explicit decisions? | [`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md) | [Guided workflow](https://juhalt.github.io/nomologR/articles/guided-workflow.md) |
| Reporting | How do I archive the evidence and decisions? | [`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md), [`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md) | [Archiving a workflow](https://juhalt.github.io/nomologR/articles/reproducible-report.md) |

## Teaching datasets

The examples and walkthroughs use simulated datasets whose population
models are documented. Because the truth is known, you can check what
the package reports against what actually generated the data — something
real data never allow.

| Dataset | Design | What it teaches |
|----|----|----|
| `nomo_demo_continuous` | Two correlated factors, five candidate items each, 500 cases | A cross-loading item (`a5`), a weak item (`b5`), and a small amount of missing data |
| `nomo_demo_ordinal` | The same latent responses cut into five ordered categories | Polychoric correlations, WLSMV estimation, and ordinal reliability |
| `nomo_demo_network` | Three constructs, an observed outcome, two administration groups, 800 cases | Theory-specified predictions, equivalence regions, replication, and a known source of scalar non-invariance |

See
[`?nomo_demo_continuous`](https://juhalt.github.io/nomologR/reference/nomo_demo_continuous.md),
[`?nomo_demo_ordinal`](https://juhalt.github.io/nomologR/reference/nomo_demo_ordinal.md),
and
[`?nomo_demo_network`](https://juhalt.github.io/nomologR/reference/nomo_demo_network.md)
for the full population models.

## A first look

An item audit describes the data without changing it:

``` r

scr <- nomo_screen(nomo_demo_continuous)
scr
#> <nomo_screen>
#> Cases: 500 | Candidate items: 10
#> Items with missing responses: 2 | Constant: 0 | All missing: 0
#> Relationship diagnostics: 10 eligible items | 10 item-rest estimates
#> Response concentration flags: 0 | Near-zero variance: 0
#> Decision log: 3 info | 1 review | 0 concern
#> No rows or items were removed or modified.
```

Factor-retention evidence triangulates several criteria rather than
trusting a single rule:

``` r

fac <- nomo_factors(nomo_demo_continuous, seed = 2026)
fac
#> <nomo_factors>
#> Cases: 500 | Items: 10 | Correlation: pearson
#> Criterion set: core | Available methods: 3 | Families: 2 | Skipped: 1
#> Parallel analysis (percentile): 2 | MAP TR2/TR4: 2/2 | KMO: 0.874
#> All 2 available criterion families (3 methods) point to 2 factors. Related methods within a family are grouped before concordance is summarized; this is strong converging evidence for investigating that solution, not proof of dimensionality. 1 requested method was not evaluated; see criterion status for the documented reason.
```

The population model for these data has two factors, and the retention
evidence points to investigating two. Notice the wording: the evidence
supports *investigating* a two-factor solution; it does not prove that
the scale “has” two factors.

## How to read nomologR output

- **Observation, reason, options, consequence.** Recommendations state
  what was found, why it may matter, what defensible choices exist, and
  what each choice changes downstream.
- **Reference values are prompts, not verdicts.** Familiar numbers such
  as a loading of .40 or a CFI of .95 flag evidence for review. Items
  receive `KEEP`, `REVIEW`, or `STRONG REVIEW`, never `DELETE`. The
  references live in
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md),
  and changing them is a visible researcher choice.
- **Historical methods are labeled.** Techniques you will meet in
  published work — eigenvalues greater than one, coefficient alpha, the
  Fornell–Larcker comparison — are shown as context where useful,
  qualified, and kept separate from contemporary evidence.
- **Engine results are never hidden.** Every result keeps the underlying
  `psych` or `lavaan` object (usually in `$fit`) for direct inspection.
- **Decisions are recorded.** Each result carries a decision log, and
  [`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
  records the researcher’s decisions and rationales across stages.

## A suggested learning path

1.  [From item audit to exploratory
    structure](https://juhalt.github.io/nomologR/articles/exploratory-workflow.md)
    — screening, factor retention, and EFA.
2.  [From CFA to a defensible measurement
    model](https://juhalt.github.io/nomologR/articles/measurement-model-evidence.md)
    — confirmatory fit, reliability, and convergent and discriminant
    evidence.
3.  [Measurement
    invariance](https://juhalt.github.io/nomologR/articles/measurement-invariance.md)
    — comparability across groups, localized strain, and
    researcher-controlled partial invariance.
4.  [Theory-specified nomological
    networks](https://juhalt.github.io/nomologR/articles/nomological-network.md)
    — predictions, equivalence regions, and replication.
5.  [Guided
    workflow](https://juhalt.github.io/nomologR/articles/guided-workflow.md)
    — the stages combined with explicit decisions.
6.  [Archiving a
    workflow](https://juhalt.github.io/nomologR/articles/reproducible-report.md)
    — a reproducible report.

Throughout, the [research
basis](https://juhalt.github.io/nomologR/articles/research-basis.md)
article explains where each method came from, what the contemporary
literature recommends, and what `nomologR` implements. Every function’s
help page lists its references.

## Where nomologR is heading

The next release, `v0.2.0`, focuses on making the workflow
research-backed from historical to contemporary practice and more useful
to graduate students and researchers: model comparison, auditable model
revision, bifactor and higher-order models, missing-data sensitivity,
score guidance, careless-response screening, and manuscript-ready
tables. See the
[roadmap](https://github.com/JUhalt/nomologR/blob/master/ROADMAP.md) and
the [v0.2.0 milestone](https://github.com/JUhalt/nomologR/milestone/2).

## Citing nomologR

``` r

citation("nomologR")
#> To cite nomologR in publications, please use:
#> 
#>   Uhalt J (2026). _nomologR: Guided Scale Development and Construct
#>   Validation_. R package version 0.3.0,
#>   <https://github.com/JUhalt/nomologR>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {nomologR: Guided Scale Development and Construct Validation},
#>     author = {Joshua Uhalt},
#>     year = {2026},
#>     note = {R package version 0.3.0},
#>     url = {https://github.com/JUhalt/nomologR},
#>   }
```
