
# nomologR

[![R-CMD-check](https://github.com/JUhalt/nomologR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JUhalt/nomologR/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/JUhalt/nomologR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/JUhalt/nomologR/actions/workflows/test-coverage.yaml)

**Current stable release: `0.1.0`. Development version: `0.1.0.9000`,
targeting `v0.2.0`.**

`nomologR` is a guided, evidence-based workflow for **empirical scale
development and construct validation**. It coordinates established R
engines while adding transparent diagnostics, literature-linked
explanations, decision logging, and theory-aware guidance.

> **Core rule:** Flag, explain, and document. Never silently delete.

## Installation

Install the stable release from the JUhalt R-universe:

``` r
install.packages(
  "nomologR",
  repos = c(
    "https://juhalt.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

To install the current GitHub version:

``` r
# install.packages("pak")
pak::pak("JUhalt/nomologR")
```

## Where nomologR fits

For new measures,
[`contentvalidR`](https://github.com/JUhalt/contentvalidR) is the
natural upstream companion: it addresses conceptual/content
representation and substantive validity. `nomologR` begins when
item-level empirical data are available and follows the measure through
dimensionality, measurement modeling, reliability, construct-validity
evidence, invariance, and theory-specified nomological networks.

## What works now

### 1. `nomo_screen()` — data and item audit

The Milestone 1 module audits candidate items **without modifying the
supplied data**. Screening includes item/case missingness, response
distributions, category use, zero/near-zero variance, concentration,
corrected item-rest and inter-item relationships, reverse-key/coding
review signals, optional continuous-like shape summaries, integrated
review tables, and five diagnostic plot views.

``` r
items <- data.frame(
  item1 = c(1, 2, 3, 4, 5, 5),
  item2 = c(1, 2, 3, 4, 4, 5),
  item3 = c(5, 4, 3, 2, 1, 1),
  item4 = c(1, 2, NA, 4, 5, 5)
)

scr <- nomo_screen(items)
summary(scr)
plot(scr)
```

The package flags reasons to inspect an item; it does not automatically
reverse-score, delete, collapse, or recode it.

### 2. `nomo_factors()` — factor-retention evidence

Milestone 2 asks a different question:

> **How many latent dimensions deserve investigation?**

The default `core` workflow triangulates four pieces of retention
evidence:

- common-factor parallel analysis (primary);
- Velicer original MAP / TR2;
- Velicer revised MAP / TR4;
- empirical Kaiser criterion (EKC).

Scree information, KMO/MSA, and Bartlett’s test are kept as supporting
evidence rather than factor-count decision rules.

``` r
set.seed(42)
f <- rnorm(300)

dat <- data.frame(
  i1 = 0.8 * f + rnorm(300, sd = 0.6),
  i2 = 0.8 * f + rnorm(300, sd = 0.6),
  i3 = 0.7 * f + rnorm(300, sd = 0.7),
  i4 = 0.7 * f + rnorm(300, sd = 0.7),
  i5 = 0.8 * f + rnorm(300, sd = 0.6)
)

fac <- nomo_factors(dat, seed = 2026)
fac
summary(fac)
```

#### Criterion bundles

Different jobs need different amounts of computation and triangulation:

``` r
nomo_factors(dat, criterion_set = "minimal")
nomo_factors(dat, criterion_set = "core")       # default
nomo_factors(dat, criterion_set = "extended")
nomo_factors(dat, criterion_set = "all")
```

- `minimal`: parallel analysis + original MAP (TR2)
- `core`: adds revised MAP (TR4) + EKC
- `extended`: adds NEST + Hull (CAF) when their assumptions are
  supported
- `all`: adds comparison data + legacy Kaiser-Guttman (\> 1)

The legacy Kaiser-Guttman result is displayed for historical context but
is **excluded from the synthesis**. If a requested method is
incompatible with the current indicator/correlation/missing-data setup,
it is marked `skipped` with a reason rather than being silently replaced
by another analysis.

#### Parallel-analysis sensitivity

Parallel analysis itself contains analytical choices. `nomologR`
computes three rules from the same null simulations:

``` r
fac$parallel$sensitivity

nomo_factors(dat, parallel_rule = "percentile")  # default
nomo_factors(dat, parallel_rule = "mean")
nomo_factors(dat, parallel_rule = "crawford")
```

The selected rule drives the primary PA suggestion; the other rules
remain visible as sensitivity evidence.

#### Retention plots

``` r
plot(fac)                            # observed vs selected PA null reference
plot(fac, type = "parallel_rules")   # PA decision-rule sensitivity
plot(fac, type = "scree")            # component + common-factor scree
plot(fac, type = "map")              # original TR2 + revised TR4 MAP curves
plot(fac, type = "evidence")         # criterion-by-criterion suggestions
plot(fac, type = "concordance")      # where recommended evidence clusters
plot(fac, type = "kmo")              # item-level MSA
```

The concordance view first groups closely related variants into
**criterion families** (for example, original and revised MAP belong to
one MAP family). This avoids making two variants of the same criterion
look like two independent votes. Internally split families remain
visible rather than being forced into a single count. A synthesis may
say:

> “Parallel analysis suggests 2 factors, and 4 of 5 available criterion
> families point to that same count. At the criterion-family level, MAP
> points to 1. Compare the plausible neighboring solutions in EFA.”

It should never say:

> “The scale has exactly 2 factors.”

#### Correlation choice is explicit

Under `correlation = "auto"`, continuous, binary, ordinal, and genuinely
mixed item sets are routed to Pearson, tetrachoric, polychoric, or mixed
correlations as appropriate. The selected method and modeling
assumptions are also exposed through the convenience fields
`fac$correlation` and `fac$modeling_types`.

When EKC is used with a non-Pearson correlation matrix, `nomologR` keeps
the criterion available but surfaces an explicit qualification that its
reference series is approximate under that correlation model.

Numeric-discrete storage is deliberately **not** treated as proof of
ordinal measurement. For numeric Likert items, make the modeling choice
explicitly:

``` r
fac_ord <- nomo_factors(
  dat_likert,
  types = c(
    i1 = "ordinal",
    i2 = "ordinal",
    i3 = "ordinal",
    i4 = "ordinal",
    i5 = "ordinal"
  )
)
```

#### Researcher control with documented guardrails

`types` is an explicit researcher decision, not a request for `nomologR`
to guess. A valid override is applied **before** default-type rejection,
which means an otherwise ambiguous storage format can be used when the
researcher has encoded it intentionally. The override is recorded in the
decision log.

For example, an ordinary R factor is nominal by default. If its factor
levels already encode the intended response order, the researcher can
declare those items ordinal:

``` r
response_levels <- c(
  "Strongly disagree",
  "Disagree",
  "Agree",
  "Strongly agree"
)

dat_factor$q1 <- factor(dat_factor$q1, levels = response_levels)
dat_factor$q2 <- factor(dat_factor$q2, levels = response_levels)

fac_factor <- nomo_factors(
  dat_factor,
  items = c("q1", "q2", "q3", "q4"),
  types = c(
    q1 = "ordinal", q2 = "ordinal",
    q3 = "ordinal", q4 = "ordinal"
  )
)

fac_factor$modeling_types
fac_factor$decision_log
```

The control is deliberately bounded by storage-safety checks:

- `types = "ordinal"` **does not reorder categories**. For factor-coded
  items, the existing factor-level order is used. Set that order
  intentionally first.
- Character/text columns are not silently converted to ordered scores.
  Recode them deliberately to numeric/factor/ordered storage before
  modeling.
- `types = "continuous"` requires numeric storage.
- `types = "binary"` requires exactly two observed response values.
- Constant or all-missing items fail first with a direct data-quality
  error; an override cannot manufacture variance that is not present.

This is the intended balance in `nomologR`: researchers retain control
over substantive modeling choices, while consequential assumptions
remain visible, documented, and protected from silent coercion.

### 3. `nomo_efa()` — exploratory structure without automatic purification

Milestone 3 turns a researcher-controlled factor count into a
transparent common-factor exploratory model. The default uses MINRES
with oblimin rotation, keeps factor correlations visible, and reports
evidence that may deserve review without silently deleting indicators or
refitting a different model.

The cleanest handoff is directly from `nomo_factors()`:

``` r
set.seed(2026)

f1 <- rnorm(500)
f2 <- 0.35 * f1 + sqrt(1 - 0.35^2) * rnorm(500)

dat2 <- data.frame(
  A1 = .82 * f1 + rnorm(500, sd = .55),
  A2 = .78 * f1 + rnorm(500, sd = .60),
  A3 = .75 * f1 + rnorm(500, sd = .62),
  A4 = .80 * f1 + rnorm(500, sd = .58),
  B1 = .82 * f2 + rnorm(500, sd = .55),
  B2 = .78 * f2 + rnorm(500, sd = .60),
  B3 = .75 * f2 + rnorm(500, sd = .62),
  B4 = .80 * f2 + rnorm(500, sd = .58)
)

scr <- nomo_screen(dat2)
fac <- nomo_factors(dat2, criterion_set = "core", seed = 2026)
efa <- nomo_efa(dat2, factors = fac)

summary(efa)
efa$item_summary
```

Passing a `nomo_factors` object carries forward its item set,
modeling-type decisions, correlation model, missing-data strategy, and
explicit smoothing choice where applicable. Those decisions are recorded
as **inherited**, not misrepresented as new EFA-stage researcher
overrides.

#### What `nomo_efa()` returns

The public result keeps the exploratory evidence reproducible and
inspectable:

- neutral factor labels (`F1`, `F2`, …), while the underlying engine
  object remains available in `efa$fit`;
- pattern and structure matrices;
- communalities, uniquenesses, and loading complexity;
- primary/secondary loading diagnostics and loading gaps;
- factor correlations;
- reproduced and residual correlation matrices;
- ranked localized residual pairs and off-diagonal RMSR;
- KMO/Bartlett supporting adequacy evidence where available;
- a structured decision log.

Item-level numerical references are intentionally framed as review
prompts:

- primary loading around `.40`;
- secondary/cross-loading around `.30`;
- communality around `.40`.

Each item receives `KEEP`, `REVIEW`, or `STRONG REVIEW`. `KEEP` means no
configured numeric EFA flag fired; it is **not** a declaration that
theory, content coverage, wording, redundancy, or later validity
evidence has approved the item.

#### EFA plots

``` r
plot(efa, type = "pattern")
plot(efa, type = "items")
plot(efa, type = "residuals")
plot(efa, type = "factor_correlations")
```

The pattern heatmap preserves loading sign; the loading plot
distinguishes primary from secondary loadings and displays both teaching
references; residual and factor-correlation plots show unique matrix
information rather than duplicating symmetric cells.

#### Researcher control remains visible

A factor count may also be supplied directly:

``` r
efa2 <- nomo_efa(dat2, factors = 2)
```

Alternative common-factor extraction and rotation choices remain
explicit. An orthogonal rotation is allowed but logged as a choice
requiring substantive justification. A non-positive-definite correlation
matrix stops by default; `smooth = TRUE` makes any smoothing
intervention explicit and records it.

For numeric Likert indicators, modeling level should be declared rather
than inferred from integer storage alone:

``` r
efa_ord <- nomo_efa(
  dat_likert,
  factors = 2,
  types = c(
    q1 = "ordinal", q2 = "ordinal",
    q3 = "ordinal", q4 = "ordinal"
  )
)
```

The full Checkpoint A walkthrough is in the **“From item audit to
exploratory structure”** vignette.

### 4. `nomo_cfa()` — confirmatory measurement-model evidence

Milestone 4 adds a guided CFA layer around `lavaan::cfa()`. The
underlying `lavaan` fit is retained in `cfa$fit`; `nomologR` adds
diagnostics, literature-linked teaching references, plots, and decision
logging without silently changing the researcher-specified model.

``` r
model <- nomo_model(list(
  F1 = c("A1", "A2", "A3", "A4"),
  F2 = c("B1", "B2", "B3", "B4")
))

cfa <- nomo_cfa(
  model,
  data = dat2
)

cfa
summary(cfa)
```

The CFA layer reports convergence and captured engine warnings, cases
used, standardized loadings with uncertainty, factor correlations,
chi-square, CFI, TLI, RMSEA with confidence interval, SRMR, localized
residual correlations, and Heywood/improper-solution diagnostics.

Modification indices are available as **post-hoc diagnostics only**:

``` r
head(cfa$top_modification_indices)
```

They never free parameters or trigger automatic respecification.

#### CFA plots

``` r
plot(cfa, type = "loadings")
plot(cfa, type = "fit")
plot(cfa, type = "residuals")
plot(cfa, type = "modification_indices")
```

Fit-index values are teaching references rather than pass/fail laws. The
package deliberately asks users to interpret global fit, localized
strain, parameter estimates, estimator, sample characteristics, and
theory together.

#### Continuous, robust, and ordinal estimation

For continuous indicators, leaving `estimator = NULL` preserves lavaan’s
ordinary continuous-data default. Robust ML estimators such as `"MLR"`
remain explicit researcher choices.

Declared ordered indicators request WLSMV by default:

``` r
cfa_ord <- nomo_cfa(
  model,
  data = dat_ord,
  ordered = names(dat_ord)
)
```

Incompatible ordered-indicator ML/FIML combinations stop with an
explanation rather than being silently substituted.

#### Calibration and validation samples

`nomo_split()` supports a reproducible exploratory/confirmatory split
when the gain in independence justifies the loss of precision:

``` r
s <- nomo_split(
  dat2,
  validation_prop = .50,
  seed = 2026
)

fac_cal <- nomo_factors(s$calibration, seed = 2026)
efa_cal <- nomo_efa(s$calibration, factors = fac_cal)
cfa_val <- nomo_cfa(model, data = s$validation)
```

The split is explicit, reproducible, and logged as a design choice; no
split ratio is presented as universally optimal.

### 5. `nomo_reliability()` + `nomo_validity()` — measurement evidence beyond model fit

Milestone 5 completes the confirmatory measurement-model layer by
separating **score reliability**, **convergent evidence**, and
**construct-separation evidence** rather than collapsing them into one
validity verdict.

``` r
rel <- nomo_reliability(cfa)
val <- nomo_validity(cfa, htmt = "both")

summary(rel)
summary(val)

plot(rel)
plot(val, type = "ave")
plot(val, type = "discriminant")
```

#### Reliability is model-based and score-specific

`nomo_reliability()` uses current `semTools::compRelSEM()`
infrastructure. Model-based omega/composite reliability is primary for
the congeneric CFA workflow. Coefficient alpha is retained as a familiar
secondary statistic and is explicitly qualified by its stronger
assumptions.

For ordered indicators, the requested score scale remains visible.
Observed ordinal-score omega is supported; observed-scale alpha is not
silently replaced with a different latent-response or numeric-score
estimand.

Sampling uncertainty can be requested explicitly:

``` r
rel_ci <- nomo_reliability(
  cfa,
  ci = "bootstrap",
  ci_boot = 1000,
  ci_seed = 2026
)

summary(rel_ci)
plot(rel_ci)
```

Bootstrap intervals are optional because they require repeated CFA
refitting. Point estimates remain the original reliability estimates;
the bootstrap adds uncertainty rather than substituting a new estimand.

#### Convergent evidence is not reliability

`nomo_validity()` keeps standardized loading evidence connected to the
fitted CFA and uses average variance extracted (AVE) as convergent
evidence. AVE is deliberately **not** reported as a reliability
coefficient.

The familiar AVE reference around `.50` is a review prompt. A value
above or below it does not, by itself, declare a construct valid or
invalid.

#### Construct separation uses multiple pieces of evidence

For multi-construct models, `nomo_validity()` aligns:

- latent-factor correlations, including available CFA uncertainty;
- HTMT2 as the preferred congeneric-oriented construct-separation
  statistic;
- original HTMT as a comparison;
- optional Fornell-Larcker output as legacy/supporting evidence.

``` r
val <- nomo_validity(
  cfa,
  htmt = "both",
  fornell_larcker = TRUE
)

summary(val)
```

Values above the configured HTMT-family review reference prompt
investigation of theoretical distinctiveness, item wording/content,
cross-loadings, and construct overlap. They do **not** automatically
merge constructs or delete indicators.

The package also refuses to manufacture simple-structure evidence when
the estimand is ambiguous. Cross-loaded models, mixed indicator
composites, and multi-group/multilevel HTMT requests are surfaced with
explicit limitations rather than silently pooled or redefined.

#### Why the evidence is kept separate

A model can reproduce the covariance structure well while its indicators
still provide weak reliability and convergent evidence. Conversely, two
constructs can each show strong loadings, omega, and AVE while remaining
empirically difficult to distinguish from one another.

That is the central Checkpoint B lesson:

> Good global fit, high reliability, convergent evidence, and construct
> separation answer different measurement questions.

The full walkthrough is in the **“From CFA to a defensible measurement
model”** vignette.

### 6. `nomo_hypotheses()` + `nomo_network()` — theory specified before evidence

Milestone 6 makes the nomological network an explicit theory test rather
than a post-hoc collection of correlations. Predictions are
machine-readable before the model is interpreted:

``` r
h <- nomo_hypotheses(
  "A -> B" = positive(min = .20),
  "A <-> C" = negligible(within = c(-.15, .15)),
  "A -> criterion" = positive()
)

net <- nomo_network(
  model,
  data = calibration,
  hypotheses = h,
  validation_data = validation
)

net
nomo_table(net, "relations")
nomo_table(net, "replication")

plot(net, type = "effects")
plot(net, type = "replication")
```

`nomo_network()` keeps theory concordance, uncertainty, measurement
context, a-priori/post-hoc provenance, and replication separate. A
non-significant p-value never establishes negligibility; quantified
negligible predictions require a researcher-specified equivalence
region.

When a validation sample is supplied, the exact same prespecified fitted
model is used again. The package does not respecify the validation model
from the primary-sample results.

### 7. `nomo_invariance()` — identification-aware generalizability evidence

Milestone 7 evaluates equality constraints without treating one
fit-change cutoff as a universal law:

``` r
inv <- nomo_invariance(
  model,
  data = dat,
  group = "group",
  levels = c("configural", "metric", "scalar"),
  localize = TRUE
)

inv
nomo_table(inv, "fit")
plot(inv, type = "change")
plot(inv, type = "local_strain")
```

Continuous and ordered indicators use identification-appropriate
sequences. For categorical models, binary, three-category, and 4+
category structures are handled differently where identification
requires it.

Partial invariance remains an explicit researcher decision:

``` r
release <- nomo_partial(
  level = "metric",
  syntax = "F =~ x2",
  rationale = "Substantive and diagnostic review supports inspecting this loading."
)

inv_partial <- nomo_invariance(
  model,
  data = dat,
  group = "group",
  levels = c("configural", "metric", "scalar"),
  partial = release
)
```

Localized score diagnostics can identify where equality constraints are
strained, but `nomologR` never searches until it finds a
partial-invariance solution that “passes.”

The full walkthroughs are in the **“Nomological network”** and
**“Measurement invariance”** vignettes.

### 8. `nomo_run()` — guided orchestration without hidden decisions

Milestone 8 connects the package components into a resumable workflow
while keeping consequential choices explicit.

``` r
run <- nomo_run(
  data = dat,
  scales = list(WellBeing = c("w1", "w2", "w3", "w4"))
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
```

The guided object retains completed component results, sample roles,
stage settings, explicit researcher decisions/rationales, and component
evidence logs. Optional invariance and theory-specified network branches
can be added for future stages without recomputing completed work.

Teaching mode presents consequential pauses as **Observation / Reason /
Options / Consequence**. Research mode provides a compact view of the
same underlying analysis.

`nomo_run()` never silently deletes items, creates a CFA model from EFA,
frees invariance constraints, respecifies a model, or declares a
construct valid/invalid.

The full walkthrough is in the **“Guided workflow with nomo_run()”**
vignette.

## Development path

The detailed release specification lives in [`ROADMAP.md`](ROADMAP.md).

The complete `v0.1.0` release track is:

1.  Data & Item Audit — `nomo_screen()` **complete**
2.  Factor-Retention Evidence — `nomo_factors()` **complete**
3.  Exploratory Factor Analysis — `nomo_efa()` **complete**
4.  Confirmatory Factor Analysis — `nomo_cfa()` **complete**
5.  Reliability + convergent/discriminant evidence — **complete**
6.  Theory-Specified Nomological Network — **complete**
7.  Measurement Invariance — **complete**
8.  Guided pipeline — `nomo_run()` **complete**
9.  Reproducible report — `nomo_report()` **complete**
10. v0.1 release hardening and infrastructure — **complete**

`v0.1.0` is the first stable public release. Planned `v0.2.x` work
focuses on robustness and broader measurement models.

## Design principles

- Measurement before structure.
- Evidence accumulates; validity is not a single statistical test.
- Cutoffs are reference points, not universal laws.
- Estimator and correlation choices should respect item type.
- Modification indices do not authorize automatic model respecification.
- Nomological evidence begins with explicit theoretical predictions.
- Null predictions require evidence beyond `p > .05`.
- Consequential decisions should be visible and reproducible.
