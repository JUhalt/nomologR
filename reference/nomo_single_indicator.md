# The reliability of a composite modeled as a single indicator

`nomo_single_indicator()` records the reliability of an observed
composite, such as a scale mean or sum, so that
[`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md)
can model the composite as a single-indicator latent variable. The
composite's error variance is fixed at \\(1 - \rho)\sigma^2\\, where
\\\rho\\ is its reliability and \\\sigma^2\\ its variance as the model
analyzes it, so the relationships it enters are corrected for its
unreliability. The variance is computed on the cases the model analyzes
(the complete cases, under listwise deletion) and with the estimator's
denominator, so the fitted model's reliability for the composite is the
one supplied.

## Usage

``` r
nomo_single_indicator(
  reliability,
  se = NULL,
  coefficient = c("omega", "alpha", "other"),
  source = NULL,
  construct = NULL
)
```

## Arguments

- reliability:

  The composite's reliability: one number strictly between 0 and 1, or a
  [`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md)
  result, whose omega for `construct` is used.

- se:

  Optional standard error of the reliability. With a
  [`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md)
  result that has bootstrap intervals, the default is approximated from
  the omega interval, as its width divided by twice the normal quantile
  of its level.

- coefficient:

  Which coefficient `reliability` is: `"omega"` (default), `"alpha"`, or
  `"other"`.

- source:

  Optional text naming where the reliability comes from, such as a
  citation or "this sample". It is recorded in the decision log.

- construct:

  With a
  [`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md)
  result, the construct whose omega is used.

## Value

A `nomo_single_indicator` object. The fields to read are `reliability`,
`se` (`NA` when not supplied), `coefficient`, `source` (`NA` when not
supplied), and `construct` (`NA` unless taken from a
[`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md)
result). [`print()`](https://rdrr.io/r/base/print.html) shows the
reliability, its standard error, and where it comes from.

## Details

**Where the method comes from.** Spearman (1904) corrected a correlation
for the unreliability of its two measures. Structural equation modeling
carried the idea into the model itself: a composite becomes the one
indicator of a latent variable whose error variance is fixed from the
composite's reliability, as the SEM textbooks describe (Hayduk, 1987;
Bollen, 1989). Applied work in organizational research and marketing
took it up where modeling every item was impractical (Williams & Hazer,
1986), and Bagozzi and Heatherton (1994) called it the "total
aggregation" model.

**Does it work?** Savalei (2019) compared single indicators with path
analysis, which ignores measurement error, and with full
multiple-indicator SEM, in samples of 30 to 200. Path analysis and
single indicators whose reliability was fixed a priori, at a value that
slightly overestimated the true one, gave the most accurate estimates
and the most power. Single indicators whose reliability was estimated
from the same data (coefficient alpha) and full SEM performed in
between, and she recommended a fixed-reliability single indicator in
small samples. An omega estimated from the same sample, as from
[`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md)
on these data, corresponds to the data-estimated variant. Where the
items are available and the sample is large enough, modeling them as
indicators remains the fuller correction.

**Which reliability.** A coefficient corrects only the error its design
can see: internal consistency, for example, leaves transient error in,
and the coefficient chosen decides what the correction means (DeShon,
1998). Omega from the composite's own measurement model
([`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md))
is the default. Coefficient alpha understates reliability when loadings
differ, so it fixes too large an error variance and overcorrects;
[`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md)
flags it for review. The single indicator is the observed sum or mean,
so with ordered items the omega should be the observed-score one
(`ordinal_scale = TRUE`, the default); a latent-response omega
(`ordinal_scale = FALSE`) describes a hypothetical continuous composite,
and taking it brings a warning and is recorded in `source`.

**Uncertainty.** A reliability is itself an estimate. Standard errors
that treat it as known are too small, and Oberski and Satorra (2013)
derived the term to add. Supply `se`, and
[`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md)
adds it. The derivation treats the reliability as estimated
independently of the data; when it comes from the same sample, the added
term is an approximation.

## References

Bagozzi, R. P., & Heatherton, T. F. (1994). A general approach to
representing multifaceted personality constructs: Application to state
self-esteem. *Structural Equation Modeling, 1*(1), 35-67.
[doi:10.1080/10705519409539961](https://doi.org/10.1080/10705519409539961)

Bollen, K. A. (1989). *Structural equations with latent variables*.
Wiley.
[doi:10.1002/9781118619179](https://doi.org/10.1002/9781118619179)

DeShon, R. P. (1998). A cautionary note on measurement error corrections
in structural equation models. *Psychological Methods, 3*(4), 412-423.
[doi:10.1037/1082-989X.3.4.412](https://doi.org/10.1037/1082-989X.3.4.412)

Hayduk, L. A. (1987). *Structural equation modeling with LISREL:
Essentials and advances*. Johns Hopkins University Press.

Oberski, D. L., & Satorra, A. (2013). Measurement error models with
uncertainty about the error variance. *Structural Equation Modeling,
20*(3), 409-428.
[doi:10.1080/10705511.2013.797820](https://doi.org/10.1080/10705511.2013.797820)

Savalei, V. (2019). A comparison of several approaches for controlling
measurement error in small samples. *Psychological Methods, 24*(3),
352-370. [doi:10.1037/met0000181](https://doi.org/10.1037/met0000181)

Spearman, C. (1904). The proof and measurement of association between
two things. *The American Journal of Psychology, 15*(1), 72-101.
[doi:10.2307/1412159](https://doi.org/10.2307/1412159)

Williams, L. J., & Hazer, J. T. (1986). Antecedents and consequences of
satisfaction and commitment in turnover models: A reanalysis using
latent variable structural equation methods. *Journal of Applied
Psychology, 71*(2), 219-231.
[doi:10.1037/0021-9010.71.2.219](https://doi.org/10.1037/0021-9010.71.2.219)

## See also

[`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md),
whose `single_indicators` argument takes these records.

## Examples

``` r
# A published reliability, with its source.
nomo_single_indicator(.85, source = "Test manual, Table 4")
#> <nomo_single_indicator> Reliability for a single indicator
#> Reliability: .850 (omega)
#> Source: Test manual, Table 4
#> 
#> See ?nomo_network for how x in `single_indicators` fixes the composite's error
#> variance.

# Omega from the composite's own measurement model in this sample.
cfa <- nomo_cfa("Persistence =~ pe1 + pe2 + pe3 + pe4", nomo_demo_network)
nomo_single_indicator(nomo_reliability(cfa), construct = "Persistence")
#> <nomo_single_indicator> Reliability for a single indicator
#> Reliability: .816 (omega) | Construct: Persistence
#> Source: this sample (nomo_reliability())
#> 
#> See ?nomo_network for how x in `single_indicators` fixes the composite's error
#> variance.
```
