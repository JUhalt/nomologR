# nomologR example-data strategy

The first public example dataset will be **simulated from a known population
model**, not copied from a proprietary or substantive research dataset.

## Why simulate the first example?

A known generating model lets the test suite ask whether `nomologR` recovers
features that are actually known:

- number of factors,
- primary loadings,
- factor correlation,
- intentionally weak items,
- an intentional cross-loading item,
- and later a known structural/nomological network.

This turns the demonstration dataset into both a teaching resource and a
statistical regression test.

## Planned datasets

### `nomo_demo_continuous`

A two-factor correlated population with approximately continuous indicators.
It should contain:

- 2 correlated latent factors,
- 5 candidate indicators per factor,
- mostly strong primary loadings,
- 1 deliberately weak item,
- 1 deliberately cross-loading item,
- a small amount of reproducible missingness.

### `nomo_demo_ordinal`

An ordinal version of the same latent structure, produced by thresholding the
continuous responses. This will support tests of polychoric/WLSMV workflows.

## Reproducibility rules

1. Generation code lives in `data-raw/`.
2. A fixed seed is recorded in the generation script.
3. Population parameters are documented alongside the script.
4. Unit tests may use smaller runtime-generated fixtures, but the public example
   data should be stable across package versions unless NEWS documents a change.
5. The dissertation Religiosity/Spirituality data may later be used in a vignette
   only if a public/reusable source and clear data provenance are established;
   the core package tests must not depend on those substantive data.
