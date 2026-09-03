# nomologR

**Development status: `0.0.0.9000` — Milestone 2: Factor-Retention Evidence**

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
library(nomologR)

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

`nomo_factors()` combines common-factor parallel analysis with Velicer MAP,
then adds scree, KMO/MSA, and Bartlett information as supporting evidence.

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

plot(fac)                    # observed vs null factor eigenvalues
plot(fac, type = "scree")    # observed scree information
plot(fac, type = "evidence") # parallel-analysis vs MAP factor count
plot(fac, type = "kmo")      # item-level MSA
```

Correlation choice is visible. Under `correlation = "auto"`, continuous,
binary, ordinal, and genuinely mixed item sets are routed to Pearson,
tetrachoric, polychoric, or mixed correlations as appropriate.

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

The synthesis says things such as:

> “Parallel analysis and MAP converge on 2 factors. Treat this as strong reason
> to investigate that solution, not as proof that the construct has exactly two
> dimensions.”

If retention methods disagree, `nomologR` carries the competing factor counts
forward as plausible solutions rather than silently choosing one.

## Development path

The detailed release specification lives in [`ROADMAP.md`](ROADMAP.md).
The v0.1 path is:

1. Data & Item Audit — `nomo_screen()` **complete**
2. Factor-Retention Evidence — `nomo_factors()` **active**
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

This package is still in the development series. To install the current
GitHub version:

```r
# install.packages("remotes")
remotes::install_github("JUhalt/nomologR")
```
