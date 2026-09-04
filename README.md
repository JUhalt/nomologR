# nomologR

**Development status: `0.0.0.9000` — Milestone 2 complete; Milestone 3 (EFA) next**

`nomologR` is a guided, evidence-based workflow for **empirical scale
development and construct validation**. It coordinates established R engines
while adding transparent diagnostics, literature-linked explanations,
decision logging, and theory-aware guidance.

> **Core rule:** Flag, explain, and document. Never silently delete.

## Where nomologR fits

For new measures, [`contentvalidR`](https://github.com/JUhalt/contentvalidR)
is the natural upstream companion: it addresses conceptual/content
representation and substantive validity. `nomologR` begins when item-level
empirical data are available and follows the measure through dimensionality,
measurement modeling, reliability, construct-validity evidence, invariance,
and theory-specified nomological networks.

## What works now

### 1. `nomo_screen()` — data and item audit

The Milestone 1 module audits candidate items **without modifying the supplied
data**. Screening includes item/case missingness, response distributions,
category use, zero/near-zero variance, concentration, corrected item-rest and
inter-item relationships, reverse-key/coding review signals, optional
continuous-like shape summaries, integrated review tables, and five diagnostic
plot views.

```r
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

The default `core` workflow triangulates four pieces of retention evidence:

- common-factor parallel analysis (primary);
- Velicer original MAP / TR2;
- Velicer revised MAP / TR4;
- empirical Kaiser criterion (EKC).

Scree information, KMO/MSA, and Bartlett's test are kept as supporting evidence
rather than factor-count decision rules.

```r
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

```r
nomo_factors(dat, criterion_set = "minimal")
nomo_factors(dat, criterion_set = "core")       # default
nomo_factors(dat, criterion_set = "extended")
nomo_factors(dat, criterion_set = "all")
```

- `minimal`: parallel analysis + original MAP (TR2)
- `core`: adds revised MAP (TR4) + EKC
- `extended`: adds NEST + Hull (CAF) when their assumptions are supported
- `all`: adds comparison data + legacy Kaiser-Guttman (> 1)

The legacy Kaiser-Guttman result is displayed for historical context but is
**excluded from the synthesis**. If a requested method is incompatible with the
current indicator/correlation/missing-data setup, it is marked `skipped` with a
reason rather than being silently replaced by another analysis.

#### Parallel-analysis sensitivity

Parallel analysis itself contains analytical choices. `nomologR` computes three
rules from the same null simulations:

```r
fac$parallel$sensitivity

nomo_factors(dat, parallel_rule = "percentile")  # default
nomo_factors(dat, parallel_rule = "mean")
nomo_factors(dat, parallel_rule = "crawford")
```

The selected rule drives the primary PA suggestion; the other rules remain
visible as sensitivity evidence.

#### Retention plots

```r
plot(fac)                            # observed vs selected PA null reference
plot(fac, type = "parallel_rules")   # PA decision-rule sensitivity
plot(fac, type = "scree")            # component + common-factor scree
plot(fac, type = "map")              # original TR2 + revised TR4 MAP curves
plot(fac, type = "evidence")         # criterion-by-criterion suggestions
plot(fac, type = "concordance")      # where recommended evidence clusters
plot(fac, type = "kmo")              # item-level MSA
```

The concordance view first groups closely related variants into **criterion
families** (for example, original and revised MAP belong to one MAP family).
This avoids making two variants of the same criterion look like two independent
votes. Internally split families remain visible rather than being forced into a
single count. A synthesis may say:

> “Parallel analysis suggests 2 factors, and 4 of 5 available criterion families
> point to that same count. At the criterion-family level, MAP points to 1.
> Compare the plausible neighboring solutions in EFA.”

It should never say:

> “The scale has exactly 2 factors.”

#### Correlation choice is explicit

Under `correlation = "auto"`, continuous, binary, ordinal, and genuinely mixed
item sets are routed to Pearson, tetrachoric, polychoric, or mixed correlations
as appropriate. The selected method and modeling assumptions are also exposed
through the convenience fields `fac$correlation` and `fac$modeling_types`.

When EKC is used with a non-Pearson correlation matrix, `nomologR` keeps the
criterion available but surfaces an explicit qualification that its reference
series is approximate under that correlation model.

Numeric-discrete storage is deliberately **not** treated as proof of ordinal
measurement. For numeric Likert items, make the modeling choice explicitly:

```r
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

`types` is an explicit researcher decision, not a request for `nomologR` to guess.
A valid override is applied **before** default-type rejection, which means an
otherwise ambiguous storage format can be used when the researcher has encoded
it intentionally. The override is recorded in the decision log.

For example, an ordinary R factor is nominal by default. If its factor levels
already encode the intended response order, the researcher can declare those
items ordinal:

```r
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

- `types = "ordinal"` **does not reorder categories**. For factor-coded items,
  the existing factor-level order is used. Set that order intentionally first.
- Character/text columns are not silently converted to ordered scores. Recode
  them deliberately to numeric/factor/ordered storage before modeling.
- `types = "continuous"` requires numeric storage.
- `types = "binary"` requires exactly two observed response values.
- Constant or all-missing items fail first with a direct data-quality error; an
  override cannot manufacture variance that is not present.

This is the intended balance in `nomologR`: researchers retain control over
substantive modeling choices, while consequential assumptions remain visible,
documented, and protected from silent coercion.

## Development path

The detailed release specification lives in [`ROADMAP.md`](ROADMAP.md).
The v0.1 path is:

1. Data & Item Audit — `nomo_screen()` **complete**
2. Factor-Retention Evidence — `nomo_factors()` **complete**
3. Exploratory Factor Analysis — `nomo_efa()`
4. Confirmatory Factor Analysis — `nomo_cfa()`
5. Reliability + convergent/discriminant evidence
6. Theory-Specified Nomological Network
7. Measurement Invariance
8. Guided pipeline — `nomo_run()`
9. Reporting, documentation, and v0.1 release gate

Future-stage functions remain explicit development stubs until their milestone
is implemented and tested.

## Design principles

- Measurement before structure.
- Evidence accumulates; validity is not a single statistical test.
- Cutoffs are reference points, not universal laws.
- Estimator and correlation choices should respect item type.
- Modification indices do not authorize automatic model respecification.
- Nomological evidence begins with explicit theoretical predictions.
- Null predictions require evidence beyond `p > .05`.
- Consequential decisions should be visible and reproducible.

## Development installation

This package is still in the development series. To install the current GitHub
version after a milestone is merged to the public branch:

```r
# install.packages("remotes")
remotes::install_github("JUhalt/nomologR")
```
