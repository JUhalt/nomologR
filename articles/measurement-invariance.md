# Measurement Invariance with nomologR

## What invariance is for

Measurement invariance asks whether a construct is measured comparably
across groups or occasions. `nomologR` treats invariance as **graded
evidence** rather than a sequence of automatic PASS/FAIL decisions.

Every fitted level retains:

- the generated
  [`semTools::measEq.syntax()`](https://rdrr.io/pkg/semTools/man/measEq.syntax.html)
  object;
- the exact lavaan syntax;
- the fitted lavaan object;
- absolute fit;
- change in fit;
- nested-model comparison evidence where available;
- localized equality-constraint diagnostics;
- researcher-specified partial-invariance provenance.

## Continuous indicators

``` r

inv <- nomo_invariance(
  "Agency =~ a1 + a2 + a3 + a4",
  data = dat,
  group = "group"
)

inv
summary(inv)
```

The conventional continuous sequence is:

``` text
configural -> metric -> scalar -> strict
```

No single delta-CFI, delta-RMSEA, delta-SRMR, or chi-square difference
rule is treated as universally decisive.

## Ordered indicators

Ordered indicators require identification-aware sequences rather than
merely substituting WLSMV for ML.

For four-or-more-category ordered indicators:

``` text
configural -> thresholds -> metric -> scalar -> strict
```

For three-category indicators, threshold equality is part of the metric
step because it is needed for identification.

For binary indicators, threshold, loading, and intercept restrictions
cannot be treated as independent sequential tests under the implemented
Wu-Estabrook approach. The sequence therefore begins:

``` text
configural -> strong -> strict
```

where `strong` imposes thresholds, loadings, and intercepts together.

## Localizing strain

``` r

inv <- nomo_invariance(
  model,
  data = dat,
  group = "group",
  levels = c("configural", "metric"),
  localize = TRUE
)

nomo_table(inv, "local_strain")
plot(inv, "local_strain")
```

Score diagnostics help researchers locate equality constraints that
deserve inspection. They **never authorize an automatic parameter
release**.

## Researcher-controlled partial invariance

``` r

partial <- nomo_partial(
  level = c("metric", "scalar"),
  syntax = c(
    "Agency =~ a2",
    "a3 ~ 1"
  ),
  rationale = c(
    "Loading difference was anticipated from prior evidence.",
    "Intercept release was theoretically justified."
  )
)

inv_partial <- nomo_invariance(
  model,
  data = dat,
  group = "group",
  partial = partial
)
```

A release is carried forward to more restrictive levels so that a
parameter freed at one level is not silently constrained again later.

`nomologR` records what was released and why. It does not search until a
model meets a preferred cutoff.

## Tables and figures

``` r

nomo_table(inv, "fit")
nomo_table(inv, "categories")
nomo_table(inv, "partial")
nomo_table(inv, "local_strain")

plot(inv, "fit")
plot(inv, "change")
plot(inv, "local_strain")
```

These outputs are intended to flow naturally into later
[`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
development while remaining ordinary tibbles and ggplot objects for
researchers who want direct control.
