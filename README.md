# nomologR

**Development status: `0.0.0.9000` — Milestone 1: Data & Item Audit**

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

### `nomo_screen()`

The first production module audits candidate items **without modifying the
supplied data**. Current screening includes:

- item- and case-level missingness;
- response distributions and category use;
- zero- and near-zero-variance diagnostics;
- response concentration plus descriptive floor/ceiling evidence where meaningful;
- corrected item-rest correlations;
- inter-item correlations;
- reverse-key/coding review signals without automatic reverse scoring;
- descriptive skewness and excess kurtosis for continuous-like indicators;
- explicit decision logging with `info`, `review`, and `concern` severities.

Numerical references are teaching/screening aids rather than universal
pass/fail rules.

```r
library(nomologR)

items <- data.frame(
  item1 = c(1, 2, 3, 4, 5, 5),
  item2 = c(1, 2, 3, 4, 4, 5),
  item3 = c(5, 4, 3, 2, 1, 1),
  item4 = c(1, 2, NA, 4, 5, 5)
)

scr <- nomo_screen(items)
scr
scr$item_summary
scr$relationship_summary
scr$decision_log
```

A negative item-rest relationship is therefore treated as a reason to inspect
keying, coding, wording, or multidimensionality — not as permission to
automatically reverse-score or delete an item.

## Development path

The detailed release specification lives in [`ROADMAP.md`](ROADMAP.md).
The v0.1 path is:

1. Data & Item Audit — `nomo_screen()` **(active)**
2. Factor-Retention Evidence — `nomo_factors()`
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
