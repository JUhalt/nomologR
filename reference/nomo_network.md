# Evaluate a theory-specified nomological network

`nomo_network()` combines a researcher-specified measurement/SEM model
with a machine-readable object from
[`nomo_hypotheses()`](https://juhalt.github.io/nomologR/reference/nomo_hypotheses.md).
Hypothesized relations that are not already present in `model` can be
added transparently before fitting, so the theory object itself can
define the structural portion of the network.

## Usage

``` r
nomo_network(
  model,
  data,
  hypotheses,
  validation_data = NULL,
  add_missing = TRUE,
  ordered = NULL,
  estimator = NULL,
  missing = NULL,
  std.lv = TRUE,
  control = NULL,
  equivalence_alpha = 0.05,
  guidance = nomo_defaults()
)
```

## Arguments

- model:

  One non-empty lavaan SEM/measurement-model syntax string or an object
  created by
  [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md).

- data:

  A non-empty data frame or a `nomo_split` object. For a `nomo_split`,
  the calibration subset is the primary sample and the validation subset
  is reserved for replication.

- hypotheses:

  A `nomo_hypotheses` object.

- validation_data:

  Optional independent validation data frame. Do not use this together
  with a `nomo_split` object.

- add_missing:

  Logical. If `TRUE` (default), theory-specified relations absent from
  `model` are appended transparently before estimation.

- ordered:

  Optional character vector naming ordered indicators.

- estimator:

  Optional lavaan estimator. When ordered indicators are declared and
  `estimator = NULL`, WLSMV is requested.

- missing:

  Optional lavaan missing-data option.

- std.lv:

  Logical passed to
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

- control:

  Optional optimizer-control list passed to
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

- equivalence_alpha:

  One number strictly between 0 and .5. For quantitative negligible
  predictions, the equivalence confidence level is
  `1 - 2 * equivalence_alpha`.

- guidance:

  Guidance settings returned by
  [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md).

## Value

A `nomo_network` object retaining the primary fitted SEM, optional
validation fit, measurement context, relation-level theory evidence,
replication evidence, and decision log.

## Details

Directed `A -> B` hypotheses map to lavaan regression paths `B ~ A`.
Association `A <-> B` hypotheses map to covariance paths `A ~~ B`.

For quantitative `negligible(within = ...)` predictions,
`nomo_network()` evaluates the SESOI using a normal-approximation
equivalence confidence interval. With the default
`equivalence_alpha = .05`, this is a 90 percent interval, corresponding
to the usual two one-sided tests logic. A bare
[`negligible()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
prediction remains non-confirmable from `p > .05`.

The function can also fit the same prespecified model in a validation
sample. Pass `validation_data` explicitly, or pass a `nomo_split` object
as `data` to use its calibration and validation subsets. No model
relation is added or removed on the basis of validation results.
