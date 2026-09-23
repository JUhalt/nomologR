# Manuscript-ready tables in APA style

Formats the evidence in a `nomologR` result as a table ready for a
thesis, dissertation, or manuscript, following APA 7 conventions: a bold
table number, an italic title, no vertical rules, and notes below the
table in the order general, specific, probability.

## Usage

``` r
nomo_apa_table(x, type = NULL, number = NULL, title = NULL, ...)
```

## Arguments

- x:

  A result object: `nomo_cfa`, `nomo_reliability`, `nomo_invariance`, or
  `nomo_network`.

- type:

  Which table to build. For `nomo_cfa`: `"loadings"`, `"fit"`, or
  `"factor_correlations"`. For `nomo_network`: `"hypotheses"` or
  `"fit"`. Other objects have one table each.

- number:

  Optional table number, printed in bold as "Table 1".

- title:

  Optional title; a descriptive default is supplied.

- ...:

  Unused.

## Value

A `nomo_apa_table` object, which prints in the console and renders as a
formatted table, with its title and notes, when knitted.

## Details

**Leading zeros follow the statistic, not the value.** APA 7 drops the
leading zero only for statistics that *cannot* exceed 1. So *p* values,
correlations, reliability coefficients, and CFI are written `.95`, while
TLI, RMSEA, SRMR, and standardized loadings keep it, `0.95`, because
each can exceed 1 in principle: TLI is not bounded above, and a
standardized loading does in an improper (Heywood) solution. Many
published tables print standardized loadings without the zero; this
follows the rule as written.

**No verdicts.** Table notes keep the package's reference-value
language. No cell reads PASS or FAIL, and fit indices are not labeled
good or poor.

The rules come from the *Publication Manual of the American
Psychological Association* (7th ed.), checked against Purdue OWL's APA 7
guides. Journal-specific templates are out of scope.

## References

American Psychological Association. (2020). *Publication manual of the
American Psychological Association* (7th ed.).
[doi:10.1037/0000165-000](https://doi.org/10.1037/0000165-000)
