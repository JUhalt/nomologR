# Plot nomological-network evidence

Every plot marks status the same way: a filled circle for no flag, an
open circle for review, a filled square for concern, and a cross where
nothing could be decided, with a legend whenever a flag is drawn. Post
hoc hypotheses are labeled as such.

## Usage

``` r
# S3 method for class 'nomo_network'
plot(x, type = c("effects", "concordance", "fit", "replication"), ...)
```

## Arguments

- x:

  A `nomo_network` object.

- type:

  Plot type: `"effects"`, `"concordance"`, `"fit"`, or `"replication"`.

- ...:

  Unused.

## Value

A `ggplot2` object.
