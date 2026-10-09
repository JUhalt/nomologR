# Teaching with nomologR

Real data never tell a class whether an analysis found the truth. The
teaching datasets in `nomologR` do: each is simulated from a population
model that is written down in its help page, with specific problems
built in. A student can run an analysis and then check it against the
answer.

This article collects exercises built on those answers. Each gives a
question, the code to run, and what the data were built to show. The
answers come from the population models, not from any one run, so they
hold however the estimates vary. The code is for students to run; it is
not run here.

## Using the package in a course

- **Teaching and research modes.** `nomo_run(mode = "teaching")`, the
  default, explains each decision it asks for: the reason, the options,
  and the consequence of each. `mode = "research"` asks for the same
  decisions without the explanations. The analyses are identical in both
  modes.
- **Decisions as assignments.** A guided run pauses at every
  consequential choice, such as the number of factors or the measurement
  model, and records the rationale a student gives. The decision log is
  a record of the student’s reasoning, which is often what an instructor
  wants to grade.
- **Reports as submissions.**
  [`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
  archives a run’s evidence and decisions in one HTML or Word document.
- **Readings.**
  [`nomo_methods()`](https://juhalt.github.io/nomologR/reference/nomo_methods.md)
  lists every method the package uses, with its references and DOIs.
  `nomo_methods(lineage = "historical")` lists the older methods
  students will meet in published work, and what now does their work.
  The research-basis article,
  [`vignette("research-basis")`](https://juhalt.github.io/nomologR/articles/research-basis.md),
  tells the same history as a timeline.

## 1. Find the weak item and the cross-loading item

`nomo_demo_continuous` has two correlated factors with five items each.
Which items are problems, and what kind?

``` r

library(nomologR)
fac <- nomo_factors(nomo_demo_continuous, seed = 2026)
efa <- nomo_efa(nomo_demo_continuous, factors = fac)
summary(efa)
```

What the data were built to show

`b5` is a weak indicator, with a population loading of 0.30, so it has a
low loading and little common variance. `a5` cross-loads, at 0.45 on A
and 0.35 on B, so its secondary loading is flagged. Neither flag is an
instruction to delete the item. Ask what each item’s content covers, and
whether the scale needs it.

## 2. How many factors?

Do the factor-retention criteria agree, and does agreement prove the
count?

``` r

fac <- nomo_factors(nomo_demo_continuous, criterion_set = "all", seed = 2026)
summary(fac)
fac <- nomo_factors(nomo_demo_continuous, criterion_set = "all",
                    missing = "complete", seed = 2026)
summary(fac)
```

What the data were built to show

The population has two factors. With the default pairwise handling of
the missing values, some criteria are skipped, and the summary says why:
they need one sample size for the whole correlation matrix. With
complete cases, every criterion runs, and all of them point to two.

Agreement among criteria is strong evidence for *investigating* a
two-factor solution. It is not proof, because every criterion reads the
same correlation matrix. The eigenvalue-greater-than-one rule is shown
as historical context; see `nomo_methods(lineage = "historical")` for
why it is not used as evidence.

## 3. Coefficient alpha or omega?

``` r

cfa <- nomo_cfa(nomo_model(list(A = paste0("a", 1:5), B = paste0("b", 1:5))),
                data = nomo_demo_continuous)
summary(nomo_reliability(cfa))
```

What the data were built to show

Alpha assumes that every item loads equally. These items don’t: `b5`‘s
loading is much weaker than the other B items’. Omega uses the estimated
loadings, so it does not rely on that assumption. Here the two
coefficients differ only a little, which is itself worth discussing: the
assumption is violated, but the consequence depends on how unequal the
loadings are. B is less reliable than A, largely because of `b5`.

## 4. Ordered categories

`nomo_demo_ordinal` has the same latent responses as
`nomo_demo_continuous`, cut into five ordered categories. What changes?

``` r

fac <- nomo_factors(nomo_demo_ordinal, seed = 2026)
fac$correlation_method
cfa <- nomo_cfa(nomo_model(list(A = paste0("a", 1:5), B = paste0("b", 1:5))),
                data = nomo_demo_ordinal, ordered = names(nomo_demo_ordinal))
cfa
```

What the data were built to show

The structure is the same, but ordered items call for polychoric
correlations and an estimator for categorical data (WLSMV). The package
chooses polychoric correlations for ordered factors and never converts
category labels to numbers silently. Compare the loadings with those
from the continuous version.

## 5. A correlation is not a path

In `nomo_demo_network`, Persistence and Performance are correlated. Does
Persistence predict Performance?

``` r

scales <- list(
  Agency = paste0("ag", 1:4),
  Persistence = paste0("pe", 1:4),
  SocialDesirability = paste0("sd", 1:3)
)
h <- nomo_hypotheses(
  "Agency -> Performance" = positive(),
  "Persistence -> Performance" = negligible(within = c(-.10, .10)),
  "Agency -> Persistence" = positive(min = .20)
)
net <- nomo_network(nomo_model(scales), data = nomo_demo_network, hypotheses = h)
summary(net)
```

What the data were built to show

Performance depends on Agency (0.40) and not on Persistence (0).
Persistence depends on Agency (0.45), so Persistence and Performance are
still correlated, about .18 in the population, through their shared
cause. The structural path answers a different question from the
correlation.

The Persistence path is near zero and not significant, yet the
[`negligible()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
prediction is not supported: its interval still reaches beyond the
region of -.10 to .10 stated in advance. A path that is not significant
is not evidence of no effect. Establishing that an effect is negligible
takes an interval inside the region, which usually needs more precision
than detecting an effect does. Ask students what sample size would be
needed.

## 6. Find the item that is not comparable across groups

The data were collected online and on paper. Can scores be compared
across the two groups?

``` r

inv <- nomo_invariance("Agency =~ ag1 + ag2 + ag3 + ag4",
                       data = nomo_demo_network, group = "group",
                       levels = c("configural", "metric", "scalar"))
summary(inv)
```

What the data were built to show

Loadings are equal across groups, so metric invariance holds. The
intercept of `ag3` is 0.50 higher on paper, so equal intercepts cost
fit, and the largest score diagnostic is `ag3`’s intercept. The latent
Agency mean is also 0.25 higher on paper. That is a real difference,
which an unequal intercept would distort. Whether to release the
intercept is the researcher’s decision, with a rationale
([`nomo_partial()`](https://juhalt.github.io/nomologR/reference/nomo_partial.md)).

## 7. When content review and the data disagree

`nomo_demo_walkthrough` is a twelve-item scale an expert panel reviewed
in `contentvalidR`. The review carried ten items. Screen them.

``` r

handoff <- readRDS(system.file("extdata", "content-handoff-walkthrough.rds",
                               package = "nomologR"))
run <- nomo_run(nomo_demo_walkthrough, scales = handoff)
run
summary(run$results$screen$EF)
```

What the data were built to show

As the data arrive, every item is flagged. `EF2` and `TF2` are
reverse-worded and not yet recoded, so they run against their scales and
pull every other item’s correlation down. The audit says so. Recode them
(6 minus the response) and screen again. `EF4` is the one Effort
Regulation item still flagged: the panel placed it, and it carries
almost no common variance. `EF3` passes the screen, but its answers pile
up at the ceiling, and the measurement model later gives it a weak
loading. It is also the only item about finishing required work.
[`vignette("content-review")`](https://juhalt.github.io/nomologR/articles/content-review.md)
works through the full example, including `TF4`, which loads on both
facets, and `TF6`, which differs by cohort.

## 8. An item-total correlation against which total?

`nomo_demo_network` measures three constructs. Screen all eleven items
as one pool, then again with the three scales declared. Which items are
flagged each time, and why?

``` r

scales <- list(Agency = paste0("ag", 1:4), Persistence = paste0("pe", 1:4),
               SocialDesirability = paste0("sd", 1:3))
items <- unlist(scales, use.names = FALSE)
summary(nomo_screen(nomo_demo_network, items = items))
summary(nomo_screen(nomo_demo_network, items = items, scales = scales))
```

What the data were built to show

In the population, social desirability is uncorrelated with Agency, and
so with Persistence too, which depends on Agency. Screened as one pool,
every item is flagged:

- The three social-desirability items have low item-rest correlations.
  Each is correlated with the sum of the other ten items, and eight of
  those ten measure something unrelated to it.
- The Agency and Persistence items have negative inter-item
  correlations, all of them with social-desirability items. Correlations
  around zero are negative about half the time.

With the scales declared, each item is correlated with the rest of its
own scale, the total it is scored on (Nunnally & Bernstein, 1994; Clark
& Watson, 2019), and negative correlations are reviewed only within a
scale. Nothing is flagged. The decision log still reports the negative
correlations between scales, as information: between constructs they are
evidence about how the constructs relate, not about keying.

The pooled value is not wrong. It answers a different question: whether
the item belongs to a single pool. That question is useful before any
scales are defined, although factor analysis answers it better. Ask
students which question their own item-total correlations answer.
