# Test-retest reliability, measurement error, and reliable change

`nomo_retest()` estimates how stable a composite's scores are when the
same people answer the same measure on two or more occasions, how large
a person's measurement error is, and which people changed by more than
that error.

## Usage

``` r
nomo_retest(data, scores, interval = NULL)
```

## Arguments

- data:

  A data frame with one row per person.

- scores:

  The columns holding the same composite on successive occasions, in
  order: a character vector of two or more column names, or a named list
  of such vectors, one per composite.

- interval:

  Optional text describing the time between occasions, such as
  `"two weeks"`. It is recorded with the results, because a test-retest
  reliability depends on it.

## Value

A `nomo_retest` object. The fields to read are:

- `icc`: one row per composite, with the number of complete cases and of
  occasions, ICC(A,1) with its 95% interval and Koo and Li's description
  of that interval (`koo_li`), ICC(C,1) with its interval, the mean
  change from the first to the last occasion with its interval, the
  pooled standard deviation, the standard error of measurement (`sem`),
  and the smallest detectable change (`sdc`).

- `reliable_change`: one row per person and composite, with the first
  and last scores, the change, the reliable change index (`rci`), and
  whether the change is a reliable increase, a reliable decrease, or
  neither.

- `interval` and `decision_log`.

Other fields record the call and the columns used (`scores`). They may
change between releases and are not part of the stable interface (see
[`?nomologR`](https://juhalt.github.io/nomologR/reference/nomologR-package.md)).

## Details

**Which intraclass correlation.** Shrout and Fleiss (1979) and McGraw
and Wong (1996) define several intraclass correlations, which differ in
their model, type, and definition. For test-retest reliability, Koo and
Li (2016) recommend a two-way mixed-effects model with absolute
agreement: the occasions are not a random sample of occasions, and
scores that do not agree across occasions are not reliable however well
they rank people. That is ICC(A,1) in McGraw and Wong's notation, for a
single measurement. The consistency form, ICC(C,1), ignores a shift in
the mean between occasions and is reported beside it, so the gap between
them shows how much of the disagreement is a systematic change. The mean
change from the first to the last occasion, with its interval, is
reported too.

**Reading the interval.** Koo and Li (2016) describe reliability as poor
below .50, moderate from .50 to .75, good from .75 to .90, and excellent
above .90, and ask that the description come from the 95% confidence
interval rather than the estimate. `koo_li` gives the description of the
interval, such as "good to excellent". These are reference values, not a
pass or a fail, and a reliability depends on the interval between
occasions and on whether the construct itself changed.

**Measurement error and change.** The standard error of measurement is
\\SD \sqrt{1 - ICC}\\ (Nunnally & Bernstein, 1994), with ICC(A,1) and
the standard deviation pooled over occasions. The smallest detectable
change is \\1.96 \sqrt{2} \\ SEM\\: a change in a person's score smaller
than that is within measurement error at 95% (Weir, 2005). Jacobson and
Truax's (1991) reliable change index divides a person's change by
\\\sqrt{2} \\ SEM\\, so it exceeds 1.96 exactly when the change exceeds
the smallest detectable change. A reliable change is not necessarily a
meaningful one.

## References

Jacobson, N. S., & Truax, P. (1991). Clinical significance: A
statistical approach to defining meaningful change in psychotherapy
research. *Journal of Consulting and Clinical Psychology, 59*(1), 12-19.
[doi:10.1037/0022-006X.59.1.12](https://doi.org/10.1037/0022-006X.59.1.12)

Koo, T. K., & Li, M. Y. (2016). A guideline of selecting and reporting
intraclass correlation coefficients for reliability research. *Journal
of Chiropractic Medicine, 15*(2), 155-163.
[doi:10.1016/j.jcm.2016.02.012](https://doi.org/10.1016/j.jcm.2016.02.012)

McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some
intraclass correlation coefficients. *Psychological Methods, 1*(1),
30-46.
[doi:10.1037/1082-989X.1.1.30](https://doi.org/10.1037/1082-989X.1.1.30)

Nunnally, J. C., & Bernstein, I. H. (1994). *Psychometric theory* (3rd
ed.). McGraw-Hill.

Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
assessing rater reliability. *Psychological Bulletin, 86*(2), 420-428.
[doi:10.1037/0033-2909.86.2.420](https://doi.org/10.1037/0033-2909.86.2.420)

Weir, J. P. (2005). Quantifying test-retest reliability using the
intraclass correlation coefficient and the SEM. *Journal of Strength and
Conditioning Research, 19*(1), 231-240.
[doi:10.1519/15184.1](https://doi.org/10.1519/15184.1)

## Examples

``` r
# Agency scores on two occasions, two weeks apart (simulated).
set.seed(2026)
true <- stats::rnorm(150)
panel <- data.frame(
  agency_t1 = 3 + true + stats::rnorm(150, sd = .45),
  agency_t2 = 3.1 + true + stats::rnorm(150, sd = .45)
)
rt <- nomo_retest(panel, scores = c("agency_t1", "agency_t2"),
                  interval = "two weeks")
rt
#> <nomo_retest> Test-retest reliability
#> Composites: 1 | Interval: two weeks
#> 
#> Reliability across occasions
#>   Composite    n  ICC(A,1) [95% CI]  Koo & Li   SEM   SDC
#>   composite  150  0.85 [0.80, 0.89]  good      0.45  1.24
#> 
#> Reliable change, first to last occasion
#>   - composite: 6 people up, 2 down, 142 within measurement error.
#> 
#> ICC(A,1): two-way mixed effects, absolute agreement, single measurement (Koo &
#> Li, 2016). SEM: standard error of measurement. SDC: smallest detectable
#> change, 1.96 x sqrt(2) x SEM (Weir, 2005). Reference ranges describe the
#> interval; they are not a pass or a fail.
nomo_table(rt, "reliable_change")
#> # A tibble: 150 × 7
#>    composite   row first_score last_score  change     rci status            
#>    <chr>     <int>       <dbl>      <dbl>   <dbl>   <dbl> <chr>             
#>  1 composite     1       3.53       3.55   0.0222  0.0350 no_reliable_change
#>  2 composite     2       2.07       1.94  -0.133  -0.209  no_reliable_change
#>  3 composite     3       3.58       2.85  -0.731  -1.15   no_reliable_change
#>  4 composite     4       3.14       3.45   0.306   0.482  no_reliable_change
#>  5 composite     5       2.59       2.06  -0.537  -0.846  no_reliable_change
#>  6 composite     6       0.998      0.242 -0.756  -1.19   no_reliable_change
#>  7 composite     7       1.95       1.97   0.0284  0.0448 no_reliable_change
#>  8 composite     8       1.70       2.83   1.12    1.77   no_reliable_change
#>  9 composite     9       3.11       3.46   0.354   0.558  no_reliable_change
#> 10 composite    10       2.68       2.90   0.218   0.344  no_reliable_change
#> # ℹ 140 more rows
```
