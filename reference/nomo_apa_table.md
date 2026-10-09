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

  A result object: `nomo_cfa`, `nomo_reliability`, `nomo_retest`,
  `nomo_validity`, `nomo_invariance`, or `nomo_network`.

- type:

  Which table to build. For `nomo_cfa`: `"loadings"`, `"fit"`, or
  `"factor_correlations"`. For `nomo_validity`: `"discriminant"` or
  `"convergent"`. For `nomo_network`: `"hypotheses"` or `"fit"`. Other
  objects have one table each, so `type` is left `NULL` for them; any
  other value is an error.

- number:

  Optional table number, printed in bold as "Table 1". Without one, the
  number line is left out, for the document to supply.

- title:

  Optional title, one string; a descriptive default is supplied.

- ...:

  Unused.

## Value

A `nomo_apa_table` object, which prints in the console and renders as a
formatted table, with its title and notes, when knitted. The fields to
read are:

- `number`: the table number, an integer, or `NULL` when none was given.

- `title`: the table title.

- `body`: a data frame of formatted cells, all character. Its column
  names are the column headings.

- `stub`: the name of the stub column, the first column of `body`, which
  says what each row describes.

- `notes`: a list of three character vectors, `general`, `specific`, and
  `probability`, the three kinds of APA table note in the order they are
  printed. Any of them may be empty.

- `source`: the class of the result the table was built from, such as
  `"nomo_cfa"`.

Headings, cells, and notes are written in Markdown, as pandoc reads it:
italics as `*p*` and a superscript as `^a^`.
[`print()`](https://rdrr.io/r/base/print.html) shows the table as
aligned text under its number and title, with its notes, the italics
dropped, and a note marker written `(a)`; a table wider than the console
wraps its long headings and text cells, and names any column it still
cannot fit.

## Details

**Leading zeros follow the statistic, not the value.** APA 7 drops the
leading zero only for statistics that *cannot* exceed 1. So *p* values,
correlations, reliability coefficients, and CFI are written `.95`, while
TLI, RMSEA, SRMR, and standardized loadings keep it, `0.95`, because
each can exceed 1 in principle: TLI is not bounded above, and a
standardized loading does in an improper (Heywood) solution. Many
published tables print standardized loadings without the zero; this
follows the rule as written, as the output style shared with
`contentvalidR` does.

**No verdicts.** Table notes keep the package's reference-value
language. No cell reads PASS or FAIL, and fit indices are not labeled
good or poor.

**Notes say what the table shows.** The general note defines each
abbreviation, gives the estimator and the number of cases analyzed, and
says when the chi-square is a scaled test statistic and the fit indices
are robust or scaled values. For a network's hypotheses it also defines
each evidence label the table shows, such as "Concordant", and each
prediction is given with the region it names. An interval's heading
carries its level, such as "90% CI" for a reliability interval
bootstrapped at that level. A cell left empty is an em dash, explained
in the note; a cell that does not apply, such as the change in fit of
the first invariance model, is blank. Specific notes, marked with
superscript letters, name the parameters a partial invariance model
frees, the hypotheses specified post hoc, the estimates whose interval
is the equivalence interval a
[`negligible()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
prediction is judged on, and the estimates on another scale than the
rest. A correlation the model fixes, such as the zero correlations of a
bifactor model, is not tabled as an estimate. Degrees of freedom that
are not whole, as for a mean- and variance-adjusted test, are given to
two decimals.

**Discriminant evidence without the Fornell-Larcker matrix.** For a
`nomo_validity` result, the `"discriminant"` table gives each pair of
constructs one row: the latent correlation with its 95% confidence
interval (Rönkkö & Cho, 2022), and the heterotrait-monotrait ratios
HTMT2 (Roemer et al., 2021) and HTMT (Henseler et al., 2015) when they
were computed. It does not print the correlation matrix with the square
root of AVE on its diagonal, because that comparison often misses
discriminant-validity problems (Henseler et al., 2015). AVE is
convergent evidence and has its own `"convergent"` table.

**Knitting.** In an R Markdown or Quarto document the table renders as a
table with its number, title, and notes. Each column's share of the page
follows its widest entry. Knitted to PDF, the Greek letters and math
symbols are written as TeX math, so the default `pdflatex` engine
compiles them.

The rules come from the *Publication Manual of the American
Psychological Association* (7th ed.), checked against Purdue OWL's APA 7
guides. Journal-specific templates are out of scope.

**Stability.** The `type` values and the structure of the returned
object follow the stability policy in
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md).
Formatting may still be corrected where it departs from APA style, with
the correction described in NEWS.

## References

American Psychological Association. (2020). *Publication manual of the
American Psychological Association* (7th ed.).
[doi:10.1037/0000165-000](https://doi.org/10.1037/0000165-000)

Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation
models with unobservable variables and measurement error. *Journal of
Marketing Research, 18*(1), 39-50.
[doi:10.2307/3151312](https://doi.org/10.2307/3151312)

Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using
structural equation modeling: An alternative to coefficient alpha.
*Psychometrika, 74*(1), 155-167.
[doi:10.1007/s11336-008-9099-3](https://doi.org/10.1007/s11336-008-9099-3)

Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for
assessing discriminant validity in variance-based structural equation
modeling. *Journal of the Academy of Marketing Science, 43*(1), 115-135.
[doi:10.1007/s11747-014-0403-8](https://doi.org/10.1007/s11747-014-0403-8)

Roemer, E., Schuberth, F., & Henseler, J. (2021). HTMT2–An improved
criterion for assessing discriminant validity in structural equation
modeling. *Industrial Management & Data Systems, 121*(12), 2637-2650.
[doi:10.1108/IMDS-02-2021-0082](https://doi.org/10.1108/IMDS-02-2021-0082)

Rönkkö, M., & Cho, E. (2022). An updated guideline for assessing
discriminant validity. *Organizational Research Methods, 25*(1), 6-47.
[doi:10.1177/1094428120968614](https://doi.org/10.1177/1094428120968614)

## Examples

``` r
model <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)

nomo_apa_table(cfa, "loadings", number = 1)
#> <nomo_apa_table> Manuscript table in APA style
#> 
#> Table 1
#> Standardized Factor Loadings
#> ----------------------------
#> Item  visual  textual  speed
#> ----------------------------
#> x1      0.77
#> x2      0.42
#> x3      0.58
#> x4               0.85
#> x5               0.86
#> x6               0.84
#> x7                      0.57
#> x8                      0.72
#> x9                      0.67
#> ----------------------------
#> Note. Standardized loadings from a confirmatory factor analysis. Estimated
#> with maximum likelihood (ML); N = 301. Blank cells are loadings fixed to zero
#> by the model.
#> 
#> See x$body for the cells and knitr::knit_print(x) for the Markdown a knitted
#> document renders.
nomo_apa_table(cfa, "fit", number = 2)
#> <nomo_apa_table> Manuscript table in APA style
#> 
#> Table 2
#> Model Fit
#> ------------------------------------------------------------------------------
#> Model                 χ²  df       p   CFI    TLI        RMSEA [90% CI]   SRMR
#> ------------------------------------------------------------------------------
#> Measurement model  85.31  24  < .001  .931  0.896  0.092 [0.071, 0.114]  0.065
#> ------------------------------------------------------------------------------
#> Note. CFI = comparative fit index; TLI = Tucker-Lewis index; RMSEA = root mean
#> square error of approximation; CI = confidence interval; SRMR = standardized
#> root mean square residual. Estimated with maximum likelihood (ML); N = 301.
#> Fit indices are reported as evidence, not against fixed cutoffs.
#> 
#> See x$body for the cells and knitr::knit_print(x) for the Markdown a knitted
#> document renders.
nomo_apa_table(nomo_reliability(cfa), number = 3)
#> <nomo_apa_table> Manuscript table in APA style
#> 
#> Table 3
#> Reliability Estimates
#> -------------------
#> Construct    ω    α
#> -------------------
#> visual     .61  .63
#> textual    .89  .88
#> speed      .69  .69
#> -------------------
#> Note. ω = coefficient omega; α = coefficient alpha. Coefficient alpha assumes
#> equal loadings and is reported alongside omega for comparison with published
#> work.
#> 
#> See x$body for the cells and knitr::knit_print(x) for the Markdown a knitted
#> document renders.
```
