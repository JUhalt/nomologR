# nomologR

[![R-CMD-check](https://github.com/JUhalt/nomologR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JUhalt/nomologR/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/JUhalt/nomologR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/JUhalt/nomologR/actions/workflows/test-coverage.yaml)

**Current stable release: `0.2.1`.**

`nomologR` is a guided, evidence-based workflow for **empirical scale
development and construct validation**. It coordinates established R
engines while adding transparent diagnostics, literature-linked
explanations, decision logging, and theory-aware guidance.

> **Core rule:** Flag, explain, and document. Never silently delete.

[Documentation](https://juhalt.github.io/nomologR/) ·
[Roadmap](https://github.com/JUhalt/nomologR/blob/master/ROADMAP.md) ·
[v0.3.0 milestone](https://github.com/JUhalt/nomologR/milestone/3) ·
[Open issues](https://github.com/JUhalt/nomologR/issues)

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

`nomologR` is distributed through R-universe, which builds each GitHub
release, usually within a few hours. The first CRAN submission is
targeted for `v0.3.0`
([\#39](https://github.com/JUhalt/nomologR/issues/39)); until then,
install from R-universe or GitHub.

## Who it is for

`nomologR` is written for **graduate students** (master’s and doctoral)
learning scale development and construct validation, for the **faculty
and advisors** who teach them, and for **researchers** who need a
defensible, reproducible measurement workflow. Methods are documented
from historical to contemporary practice, with literature references in
every analysis function’s help page.

## Learning resources

- [Get
  started](https://juhalt.github.io/nomologR/articles/nomologR.html) —
  the workflow at a glance and a suggested learning path.
- [Research
  basis](https://juhalt.github.io/nomologR/articles/research-basis.html)
  — where each method came from, what current literature recommends, and
  what `nomologR` implements.
- Walkthroughs that run on simulated teaching data with known answers:
  [exploratory
  structure](https://juhalt.github.io/nomologR/articles/exploratory-workflow.html),
  [measurement-model
  evidence](https://juhalt.github.io/nomologR/articles/measurement-model-evidence.html),
  [total and subscale
  scores](https://juhalt.github.io/nomologR/articles/hierarchical-models.html),
  [measurement
  invariance](https://juhalt.github.io/nomologR/articles/measurement-invariance.html),
  [nomological
  networks](https://juhalt.github.io/nomologR/articles/nomological-network.html),
  [guided
  workflow](https://juhalt.github.io/nomologR/articles/guided-workflow.html),
  and [reproducible
  reports](https://juhalt.github.io/nomologR/articles/reproducible-report.html).

The teaching datasets are `nomo_demo_continuous` (two correlated factors
with a cross-loading item, a weak item, and missing data),
`nomo_demo_ordinal` (the same structure as five-category ordered items),
and `nomo_demo_network` (three constructs, an observed outcome, and two
groups with a known source of non-invariance):

``` r

library(nomologR)

scr <- nomo_screen(nomo_demo_continuous)
fac <- nomo_factors(nomo_demo_continuous, seed = 2026)
efa <- nomo_efa(nomo_demo_continuous, factors = fac)
efa$item_summary
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

The cleanest handoff is directly from
[`nomo_factors()`](https://juhalt.github.io/nomologR/reference/nomo_factors.md):

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

Milestone 4 adds a guided CFA layer around
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html). The
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

#### Does a result depend on missing-data handling?

[`nomo_missing()`](https://juhalt.github.io/nomologR/reference/nomo_missing.md)
refits the same model under listwise deletion and FIML, or under
listwise and pairwise deletion for ordered indicators. It reports where
the estimates, reliability, or theory evidence differ:

``` r

nomo_missing(cfa, data = dat2)
```

Listwise and pairwise deletion require data missing completely at
random, and FIML requires data missing at random (Enders & Bandalos,
2001). A difference larger than half the reference standard error is
flagged, following Schafer and Graham’s (2002) rule for when bias
becomes practically important. The flag comes with the caveat that the
difference estimates bias only if the data are missing at random. That
assumption cannot in general be tested from the data at hand, so
agreement is reported as insensitivity to the choice, never as proof
that either strategy is unbiased. See **“From CFA to a defensible
measurement model”**.

#### Calibration and validation samples

[`nomo_split()`](https://juhalt.github.io/nomologR/reference/nomo_split.md)
supports a reproducible exploratory/confirmatory split when the gain in
independence justifies the loss of precision:

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

#### Comparing measurement models

[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)
places fitted CFA models side by side. When the models are nested
(checked automatically), it reports the difference test that matches the
estimator; it also reports changes in fit indices, AIC and BIC when they
are defined, and side-by-side loadings, reliability, and
construct-separation evidence. A rationale is required, and no model is
selected automatically.

``` r

full <- nomo_cfa(
  "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
  data = nomo_demo_continuous
)
no_b5 <- nomo_cfa(
  "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + 0*b5",
  data = nomo_demo_continuous
)

cmp <- nomo_compare(
  full = full,
  no_b5 = no_b5,
  rationale = "Does the weakly loading item b5 contribute to factor B?"
)
cmp
summary(cmp)
```

To test whether an item is needed, fix its loading to zero instead of
dropping the column: models with different observed variables describe
different data, so they receive descriptive evidence only.

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

[`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md)
uses current
[`semTools::compRelSEM()`](https://rdrr.io/pkg/semTools/man/compRelSEM.html)
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

[`nomo_validity()`](https://juhalt.github.io/nomologR/reference/nomo_validity.md)
keeps standardized loading evidence connected to the fitted CFA and uses
average variance extracted (AVE) as convergent evidence. AVE is
deliberately **not** reported as a reliability coefficient.

The familiar AVE reference around `.50` is a review prompt. A value
above or below it does not, by itself, declare a construct valid or
invalid.

#### Construct separation uses multiple pieces of evidence

For multi-construct models,
[`nomo_validity()`](https://juhalt.github.io/nomologR/reference/nomo_validity.md)
aligns:

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

#### Total and subscale scores

When a scale reports a total score and subscale scores,
[`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md)
writes higher-order and bifactor structures, and
[`nomo_hierarchical()`](https://juhalt.github.io/nomologR/reference/nomo_hierarchical.md)
reports how much of each score reflects the general factor:

``` r

subscales <- list(
  Focus = paste0("x", 1:4),
  Drive = paste0("x", 5:8),
  Poise = paste0("x", 9:12)
)

bifactor <- nomo_cfa(nomo_model(subscales, structure = "bifactor"), data = dat)
h <- nomo_hierarchical(bifactor)

nomo_table(h, "indices")    # omega total, omega hierarchical, ECV, PUC
nomo_table(h, "subscales")  # what each subscale adds beyond the general factor
```

The indices carry their estimands and no pass/fail thresholds. A
bifactor model usually fits at least as well as the alternatives even
when it did not generate the data, so `nomologR` compares structures
with
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)
but does not choose between them. See **“Total and subscale scores”**.

### `nomo_scores()` — scoring is a modeling decision

Once a measurement model is established,
[`nomo_scores()`](https://juhalt.github.io/nomologR/reference/nomo_scores.md)
produces sum, mean, regression, or Bartlett scores and reports what they
are and are not.

``` r

scored <- nomo_scores(fit, method = "sum")
nomo_table(scored, "diagnostics")
```

Adding items is not arithmetic: it assumes a parallel model, with equal
unstandardized loadings and equal residual variances, so for
unit-weighted scores that constrained model is fitted and compared with
the model you supplied. Every method reports Grice’s three criteria —
validity, univocality, and correlational accuracy.

The third matters before scores are used in later analyses. Correlations
among scores do not reproduce correlations among the factors, and the
direction of the discrepancy depends on the scoring method and the model
rather than being a constant that could be corrected for, so `nomologR`
reports it rather than adjusting for it. Where a question can be asked
of the latent variables instead, asking it of scores replaces an
unbiased answer with a biased one.

One design is an established exception. For a linear regression among
factors, Skrondal and Laake (2001) proved that regression-method scores
for the predictors and Bartlett scores for the outcome, each block
scored from a measurement model of its own, give consistent
coefficients. The article applies it next to the two ways of getting it
wrong. See **“Scoring a measurement model”**.

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

[`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md)
keeps theory concordance, uncertainty, measurement context,
a-priori/post-hoc provenance, and replication separate. A
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

[`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
never silently deletes items, creates a CFA model from EFA, frees
invariance constraints, respecifies a model, or declares a construct
valid/invalid.

If the measurement evidence prompts a change,
[`nomo_revise()`](https://juhalt.github.io/nomologR/reference/nomo_revise.md)
creates a child workflow that keeps the parent as its documented
ancestor:

``` r

revised <- nomo_revise(
  run,
  cfa_model = "WellBeing =~ w1 + w2 + w3 + w4\nw1 ~~ w2",
  rationale = "Item wording suggests w1 and w2 share method variance.",
  origin = "post_hoc"
)

nomo_table(revised, "lineage")
```

The revision records what changed, why, and whether the change was
prespecified or post hoc; compares the parent and revised models with
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md);
and recommends confirming a post-hoc revision in independent data.
Revisions chain, so `$lineage` and the report keep the whole history.

The full walkthrough is in the **“Guided workflow with nomo_run()”**
vignette.

### 9. `nomo_report()` — reproducible reporting

[`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
creates an archival report from a guided workflow, including methods,
evidence, researcher decisions, deviations, citations, and session
information: a self-contained HTML file, or a Word document when `file`
ends in `.docx`. See the [reproducible reporting
walkthrough](https://juhalt.github.io/nomologR/articles/reproducible-report.html)
for the documented inputs and limitations.

The report cites the methods the workflow actually used, not every
method the package offers. The same information is available directly:

``` r

nomo_methods(run)                     # methods this workflow used
nomo_methods(lineage = "historical")  # shown for recognition, and why
nomo_methods(run, references = TRUE)  # a reference list with DOIs
```

Each method records its stage, whether it is historical, contemporary,
or emerging practice, how the package uses it, its estimand and
assumptions, and DOI-verified references. The [research
basis](https://juhalt.github.io/nomologR/articles/research-basis.html)
article explains the lineage labels.

For a thesis or manuscript,
[`nomo_apa_table()`](https://juhalt.github.io/nomologR/reference/nomo_apa_table.md)
formats a result as an APA 7 table — a bold number, an italic title, no
vertical rules, and notes below — that knits directly into R Markdown or
Quarto:

``` r

nomo_apa_table(cfa, "loadings", number = 1)
nomo_apa_table(reliability, number = 2)
nomo_apa_table(network, number = 3)
```

Leading zeros follow the statistic rather than its value: reliability,
correlations, CFI, and *p* lose theirs because they cannot exceed 1,
while TLI, RMSEA, SRMR, and standardized loadings keep theirs because
they can. No table labels a result as passing or failing.

## Development path

The detailed release plan lives in
[`ROADMAP.md`](https://github.com/JUhalt/nomologR/blob/master/ROADMAP.md).

### Current release: `v0.2.1`

`v0.2.1` completes the v0.2 workflow for what happens after a
measurement model is established
([milestone](https://github.com/JUhalt/nomologR/milestone/4)):

- reports render from inside R Markdown and Quarto documents (#40);
- [`nomo_scores()`](https://juhalt.github.io/nomologR/reference/nomo_scores.md)
  produces sum, mean, regression, or Bartlett scores with Grice’s
  validity, univocality, and correlational-accuracy diagnostics, and
  tests the parallel model that unit weighting assumes (#33);
- `nomo_screen(effort = TRUE)` adds careless-responding indices,
  reported side by side because they detect different failures (#34);
- [`nomo_hierarchical()`](https://juhalt.github.io/nomologR/reference/nomo_hierarchical.md)
  reports factor determinacy and construct replicability (#56);
- [`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md)
  discloses relationships estimated between observed variables, which
  carry the bias that scoring introduces, and names the remedies the
  literature supports (#62);
- the reliability bootstrap can run on several workers (#42);
- [`nomo_apa_table()`](https://juhalt.github.io/nomologR/reference/nomo_apa_table.md)
  formats manuscript-ready tables, and
  [`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
  writes Word documents as well as HTML (#35);
- [`nomo_missing()`](https://juhalt.github.io/nomologR/reference/nomo_missing.md)
  shows whether a measurement model or network depends on how missing
  data were handled (#32);
- [`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)
  is fully covered by tests again (#54).

Certification is recorded in
[\#70](https://github.com/JUhalt/nomologR/issues/70).

### Previous release: `v0.2.0`

`v0.2.0` made the workflow research-backed from historical to
contemporary practice and more useful to graduate students and
researchers. Scope was selected in
[\#22](https://github.com/JUhalt/nomologR/issues/22). It added learning
foundations (#25) with teaching datasets, runnable vignettes, and the
research-basis article; readable invariance labels (#31); maintenance
(#36); model comparison with
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)
(#27); auditable revision lineage with
[`nomo_revise()`](https://juhalt.github.io/nomologR/reference/nomo_revise.md)
(#28); the methods registry with
[`nomo_methods()`](https://juhalt.github.io/nomologR/reference/nomo_methods.md)
(#26); interval-based replication-status language for sign changes
(#30); bifactor and higher-order models with
[`nomo_hierarchical()`](https://juhalt.github.io/nomologR/reference/nomo_hierarchical.md)
(#29); and release certification (#37).

### Next: `v0.3.0` and `v1.0.0`

`v0.3.0` carries the first CRAN submission
([\#39](https://github.com/JUhalt/nomologR/issues/39)) and a reader for
handoffs from `contentvalidR`
([\#46](https://github.com/JUhalt/nomologR/issues/46)), so items that
passed content review can be screened and modeled here. Other candidates
for that release (for example IRT/DIF, Bayesian SEM, ESEM, and
longitudinal invariance) are tracked in
[\#38](https://github.com/JUhalt/nomologR/issues/38).

`v1.0.0` is planned as a joint release with
[`contentvalidR`](https://github.com/JUhalt/contentvalidR), the
package’s content-validity counterpart, so the two show one item set
crossing from expert review into empirical evidence
([\#53](https://github.com/JUhalt/nomologR/issues/53)). A companion
article will demonstrate an item that passes content review and then
behaves badly empirically, and one the reverse
([\#60](https://github.com/JUhalt/nomologR/issues/60)).

### Completed: `v0.1.0`

The complete `v0.1.0` release track is:

1.  Data & Item Audit —
    [`nomo_screen()`](https://juhalt.github.io/nomologR/reference/nomo_screen.md)
    **complete**
2.  Factor-Retention Evidence —
    [`nomo_factors()`](https://juhalt.github.io/nomologR/reference/nomo_factors.md)
    **complete**
3.  Exploratory Factor Analysis —
    [`nomo_efa()`](https://juhalt.github.io/nomologR/reference/nomo_efa.md)
    **complete**
4.  Confirmatory Factor Analysis —
    [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
    **complete**
5.  Reliability + convergent/discriminant evidence — **complete**
6.  Theory-Specified Nomological Network — **complete**
7.  Measurement Invariance — **complete**
8.  Guided pipeline —
    [`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
    **complete**
9.  Reproducible report —
    [`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
    **complete**
10. v0.1 release hardening and infrastructure — **complete**

`v0.1.0` was the first stable public release.

## Design principles

- Measurement before structure.
- Evidence accumulates; validity is not a single statistical test.
- Cutoffs are reference points, not universal laws.
- Estimator and correlation choices should respect item type.
- Modification indices do not authorize automatic model respecification.
- Nomological evidence begins with explicit theoretical predictions.
- Null predictions require evidence beyond `p > .05`.
- Consequential decisions should be visible and reproducible.

## Citation

Use `citation("nomologR")` for the installed package version. The
default-branch
[`CITATION.cff`](https://github.com/JUhalt/nomologR/blob/master/CITATION.cff)
describes the current development source; each release tag retains the
citation metadata appropriate to that release.

## License

The current development source is licensed under the **GNU General
Public License, version 3 only (SPDX: GPL-3.0-only)**. See
[LICENSE.md](https://github.com/JUhalt/nomologR/blob/master/LICENSE.md)
and the preserved attribution in
[inst/NOTICE](https://github.com/JUhalt/nomologR/blob/master/inst/NOTICE).

Previously published releases, including `0.1.0`, retain their original
MIT license. Stable installation currently retrieves that release; the
next published release will carry GPL version 3 only. This source
transition does not relabel existing tags or release artifacts.
