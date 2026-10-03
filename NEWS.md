# nomologR (development version)

These changes lead to 1.0.0, the stable release planned jointly with
`contentvalidR` (#53). Its first release candidate, `v1.0.0-rc.1` (version
0.99.0), is planned for 2026-10-17.

- **New methods.** The gap review (#129) added the methods social-science scale
  developers were missing, each taken from its literature.
- **Fixes.** A verified audit of every module (#145) fixed the defects that
  changed returned values, documented defaults, or the documented interface,
  before the 1.0 freeze.
- **The 1.0 contract.** The contract is now settled: the stability policy covers
  every exported function and documented field except `nomo_method_variance()`
  and `nomo_power_simulate()`, which stay experimental into 1.x.

Calls whose results or behavior change are listed first.

## Fixes from the pre-1.0 audit (#145)

The audit found about 110 distinct defects; none critical. These fixes change
results or the interface, so they land before the freeze. The remaining
corrections (validation, log rows, APA tables, and presentation) follow before
the release candidate.

### Calls that now behave differently

- `nomo_report()` requires `file`. It no longer writes `nomologR-report.html`
  to the working directory by default.
- **Columns stored as ordered factors** are treated as ordered indicators,
  whether or not `ordered` names them, because lavaan already fitted them that
  way. This applies in `nomo_cfa()`, `nomo_invariance()`,
  `nomo_invariance_longitudinal()`, `nomo_network()`, `nomo_reliability()`,
  `nomo_validity()`, and `nomo_hierarchical()`. Calls that relied on these
  columns being treated as continuous can now stop: an ML-family estimator,
  FIML, or a continuous invariance sequence.
- **Errors where calls used to be accepted silently.**
  - A partial-invariance release that matches no parameter, or that would
    break nesting, is an error.
  - `ID.cat` accepts only Wu and Estabrook's identification.
  - `nomo_efa()` no longer offers `fm = "minrank"`.
  - `nomo_hypotheses()` and `nomo_network()` refuse a directed path and an
    association for the same pair, and `nomo_network()` refuses an
    ordered-factor covariate.
  - `nomo_screen()` checks `reverse` and `scale_range` in every mode, and
    `nomo_run()` refuses an unknown `reverse` item.
  - `nomo_apa_table()` refuses a `type` for one-table results and a title that
    is not one string.
- **Results that change.**
  - `nomo_retest()`'s SEM, SDC, and reliable change under a shift between
    occasions.
  - `nomo_scores()`'s parallel-model test.
  - `nomo_network()`'s concordance values and, for some networks, the fitted
    model.
  - `nomo_validity()`'s HTMT cases under FIML.
  - `nomo_hierarchical()` under the theta parameterization.
  - The single-indicator error variance after listwise deletion.
  - The careless-responding indices on short item sets.
  - `nomo_efa(fm = "minchi")`.
  - `nomo_run()`'s downstream stages, which now inherit the CFA's settings.

### Item screening and careless responding

- `nomo_screen(effort = TRUE)` no longer produces careless-responding flags or values by arithmetic (#145).
  - **Antonym and synonym correlations.** A respondent needs at least three usable pairs, as the help page said. One who left a pair member unanswered had been given a correlation over two pairs, which is exactly +1 or -1, and could be flagged on it. `antonym_r` and `synonym_r` are now `NA` for that respondent.
  - **Even-odd consistency.** `even_odd` is bounded at -1, as `careless::evenodd()` bounds it. The Spearman-Brown correction passes -1 at a within-person correlation of -1/3 and diverges below it, and had returned values such as -482. A within-person correlation of exactly -1, which was `NA`, is now -1. Values above -1 are unchanged.
  - **Long-string.** The half-length rule is flagged only when at least `long_string_min_items` items are screened, a new setting in `nomo_defaults()` (20). With fewer items, each case's run is still reported in `long_string`, no case is flagged on it, and the decision log states the number of items and the share of cases that reached half the length. Before, the rule flagged every case at one or two items and about a third of attentive respondents at six. `effort_settings` gains `long_string_min_items` and `long_string_rule_applied`, the print reads "long-string not applied", and the report's rule column says the same. Results are unchanged at 20 or more items; set `guidance$long_string_min_items` lower to apply the rule to a shorter set.
- `nomo_screen()` checks `reverse` and `scale_range` whether or not `effort = TRUE` (#145). With `effort = FALSE`, a `reverse` naming an item that is not screened, a `reverse` that is not character, and a `scale_range` that is not `c(min, max)` with min below max were accepted silently, although both arguments feed the item audit's keying explanation. They are now errors, as with `effort = TRUE`. A non-finite `scale_range` is refused in both modes. Naming reverse-keyed items without `scale_range` is still refused with `effort = TRUE`; with `effort = FALSE` the audit runs, and a new `keying_not_used` row in the decision log says the declared keying was not used. The help page now says what each mode does, in place of "Required whenever `reverse` is supplied".

### Factor retention and EFA

- `nomo_efa()` and `nomo_factors()` report the minimum pairwise N beside the case count (#145). Under the default `missing = "pairwise"`, `n_cases` counts every row, including rows with no item data, so it can overstate the information in the correlations. Both now print "Cases: 500 (minimum pairwise N: 280)" when the two differ, and `nomo_factors()` returns `min_pairwise_n`, as `nomo_efa()` already did; `n_cases` keeps its meaning. `nomo_efa()`'s `sample_adequacy$cases_per_item` and its small-sample review row now use the minimum pairwise N, as `nomo_factors()`'s prompt already did, so the two functions agree on whether a sample is small. Nothing changes without missing item values or with `missing = "complete"`. `?nomo_efa` and `?nomo_factors` document `n_cases`, `min_pairwise_n`, and the `sample_adequacy` fields.
- Every `fm` value `nomo_efa()` and `nomo_factors()` document now runs as documented (#145). `nomo_efa()` no longer lists or accepts `"minrank"`: psych needs the `Rcsdp` package for it, which nomologR does not declare, so it failed on a default install. `nomo_efa(fm = "alpha")` with one factor stops with a clear message, because psych cannot fit a one-factor alpha solution. `nomo_factors()` now checks `fm` and lists the values that work for parallel analysis; `"alpha"` and `"minrank"` stop with that list rather than an engine error. `fm = "minchi"` now runs `minchi` in both functions: psych had quietly fitted `minres` instead, because nomologR did not pass the pairwise sample sizes `minchi` needs. Results with `fm = "minchi"` change.

### CFA, model comparison, and model syntax

- `nomo_cfa()` treats model indicators stored as ordered factors as declared, whether or not `ordered` names them (#145). lavaan already fitted such columns as categorical. The result, though, recorded no ordered indicators, gave the estimator as `DWLS`, and logged that lavaan's continuous-data default was retained. As a result `nomo_methods_used()` credited ML, `nomo_reliability()` stopped with a semTools error, and `nomo_compare()` refused the same model fitted with `ordered`. WLSMV is now requested explicitly, the indicators are recorded in `ordered` and the print, and the decision log flags them for review. On `nomo_demo_ordinal` the fit is unchanged, and a guided run without `ordered` no longer stops at the reliability stage. With an ML-family estimator or FIML, such columns now stop the fit with a message that names them, rather than lavaan's error.
- `nomo_cfa()` counts only the model's own variables in `ordered` (#145). Declared names that are not in the model are left out of `ordered`, the print and the count, and the decision log lists them.
- `nomo_cfa()`'s `fit_evidence` no longer mixes versions of a fit when lavaan cannot compute a requested scaled test statistic (#145). lavaan can then still report a scaled RMSEA, such as 0 for a model whose standard RMSEA is 0.22, and that value had been shown beside the standard chi-square and CFI. In that case every value is now the standard one, and the decision log says so for review. Otherwise the order is unchanged (robust, then scaled, then standard values), so ML with FIML still reports lavaan's robust CFI, TLI and RMSEA beside the standard chi-square. The RMSEA interval is always the one around the RMSEA reported.
- In a higher-order model, `nomo_cfa()` reports a negative first-order disturbance once, as a negative latent variance, rather than also as a negative observed residual variance (#145). A weak second-order loading now asks for the first-order factor's definition to be inspected, not item content.
- `nomo_compare()` (#145):
  - When the nesting check cannot run and nesting was not declared, `relation` is `"undetermined"`, printed "nesting not determined", and the comparison is logged for review. Before, it read "not nested" at severity info. The help lists every `relation` and `nesting_check` value.
  - `models$df` is the degrees of freedom of the chi-square beside it. For mean-and-variance adjusted tests such as `MLMVS`, it had paired the scaled chi-square with the model's degrees of freedom. Nesting and `df_difference` still count the models' degrees of freedom.
  - HTMT2 rows of `evidence` name a pair in model order ("A vs B"), as `nomo_validity()` does, rather than "B vs A".
  - The help now says what `engine_warnings` holds: the warnings from the nesting check and the difference test for each compared model. Each fit keeps its own lavaan warnings in `fits`.
- `nomo_model()` (#145): two bifactor structures it accepts are not identified without an added constraint. One is a group factor with two indicators, whose loadings enter the covariances only through their product. The other is two group factors with no more than three indicators each. The help and notes had called them weakly identified or possibly unstable. The notes now say they are not identified, at severity concern; lavaan may still report convergence, but the loadings are arbitrary. `notes` is named by severity ("review" or "concern"), and the print prefixes each note with it. These structures are still accepted, and no constraint is added.

### Reliability, validity, retest, and single indicators

- Reliability and validity now read semTools' results by construct and group (#145).
  - **Multi-group fits.** `nomo_reliability()` had given every coefficient of a multi-group CFA the block "overall". The summary then paired one group's omega with another group's alpha, and bootstrap intervals landed on the wrong group. `block` now names each coefficient's group, and `summary()`, `nomo_table()`, the bootstrap intervals and `nomo_apa_table()` follow it; the APA table has one row per construct and group.
  - **Single-indicator factors.** In a model with a single-indicator factor beside one scale, the scale's omega and alpha were filed under the invented name "construct_1". They now keep the scale's name. The single-indicator factor's `alpha_status` and a decision-log entry say that no reliability is estimated for it. The reason given for an unavailable alpha now depends on whether alpha was returned.
  - **Cross-loaded indicators.** When every factor had a cross-loaded indicator, `nomo_validity()`'s `ave` was empty and `print()`, `summary()` and `nomo_table()` failed. Each factor now keeps its row. An AVE that a cross-loading leaves undefined is `NA` with attention "unavailable", and the log records it as information rather than a concern.
- `nomo_reliability()`, `nomo_validity()` and `nomo_hierarchical()` now treat columns stored as ordered factors as ordered indicators, as lavaan fitted them, even when `ordered =` does not name them (#145). `nomo_reliability()` had treated them as continuous and failed at the alpha step.
- `nomo_validity()`'s `htmt_missing = "default"` now follows the fitted model's missing-data handling: FIML when the CFA used it, pairwise deletion when it used that, and listwise deletion otherwise (#145). Despite its help page, it had always used listwise deletion, so on a FIML CFA, HTMT rested on fewer cases than the CFA. `htmt_status` gains `missing` and `n`, and a decision-log entry records the cases HTMT used. The entry is for review only when a different handling leaves HTMT with fewer cases than the CFA.
- `nomo_retest()`'s standard error of measurement is now the square root of the residual mean square (Weir, 2005), which leaves out any shift between occasions (#145). It had been the pooled SD x sqrt(1 - ICC(A,1)), which counts a shift as error. Under a shift, `sem`, `sdc` and `rci` therefore overstated measurement error and missed reliable change. Without a shift the values are nearly unchanged.
- `nomo_hierarchical()` gives the same indices for an ordered model fitted with lavaan's theta parameterization as with delta (#145). Under theta, omega had exceeded 1, and every index except PUC was wrong. The help page now lists the `factors` and `estimand` fields, and `?nomo_table` lists the `"factors"` type.
- `nomo_network()` now fixes a single indicator's error variance from the composite's variance on the rows lavaan analyzes, with lavaan's denominator, so the fitted model's reliability is the one supplied (#145). It had used every available row, which differed whenever listwise deletion dropped cases. `single_indicators$n` is now the number of those rows.

### Measurement invariance

- `nomo_invariance()` and `nomo_invariance_longitudinal()` model indicators stored as ordered factors as ordered, whether or not `ordered` names them (#145). lavaan already fitted such columns as categorical, but nomologR recorded them as continuous and used the continuous sequence. The scalar model was then unidentified, and a population latent difference of .50 SD was reported as 0.00. They now get the threshold-aware sequence, WLSMV and the theta parameterization. `ordered` includes them, `ordered_detected` lists them, and the decision log flags them for review. Across occasions, an item whose columns are ordered factors is ordered on every occasion. Calls written for continuous indicators can therefore stop: `levels` from the continuous sequence (such as `c("configural", "metric", "scalar")`, where the ordered sequence adds `"thresholds"`), `ID.fac = "UL"`, an ML-family `estimator`, or `missing = "fiml"`. Each such error names the detected indicators and says to convert them to numeric to model them as continuous.
- A partial-invariance release must name a loading, intercept, threshold, or residual variance of an indicator in the model, and must free it (#145). semTools ignores a release it cannot match. A misspelled item (`"ag9 ~ 1"`), a factor name with the wrong case, or a column name across occasions (`"w3_t2 ~ 1"` for `"w3 ~ 1"`) had left the model fully constrained while the fit table, summary, and decision log reported the release. Such releases are now errors, as is one the generated model fixes, such as a marker loading under `ID.fac = "UL"`.
- A release applies from the level that first holds its parameter type equal (#145). Declared earlier, such as an intercept at `metric`, it is moved to that level: `partial` and the summary show the level it applies from, and the log records the declared one. Declared later, such as a loading at `scalar`, it is an error. The metric model had held that loading equal and the scalar model freed it, so the two were not nested, yet their likelihood-ratio test and fit changes were reported as usual.
- `latent_means` is decided from the fitted model (#145). It reports only factors whose reference latent mean is fixed at 0 and variance at 1, and only estimated means. For a model with higher-order factors, semTools uses unit loadings whatever `ID.fac` asks for, and the table had reported a group's own mean as a standardized difference. The table is now empty, and `ID.fac` and the log record `"ul"`. A mean fixed at 0 because every intercept of a factor was released had been reported as 0.00 [0.00, 0.00]. It is now left out, and the log says why.
- `ID.cat` accepts only Wu and Estabrook's (2016) identification, `"Wu.Estabrook.2016"`, or a semTools alias for it (#145). Under `"millsap"` (which `?nomo_invariance_longitudinal` had suggested) or `"mplus"`, the levels kept the Wu-Estabrook sequence and notes. The thresholds step then also constrained the intercepts, and the scalar step added nothing. The Millsap sentence is removed from the help.
- `completed_levels` holds only the levels that were estimated and converged (#145). A level that failed stays in `fit_evidence`. When the first level failed, `print()`, `summary()`, and the invariance line of `nomo_run()`'s key evidence say "none" instead of leaving the path blank.

### Nomological network

- `nomo_network()` and `nomo_hypotheses()` were audited before the freeze (#145).
  - **Associations are judged against the model that is fitted.** An `A <-> B` hypothesis was called "already in model" from `model` alone, where lavaan covaries exogenous factors by itself. When another hypothesis made A or B an outcome, lavaan no longer added that covariance, so the hypothesis ended "Not evaluable" while `model_relations` said the relation was in the model. The hypothesized directed paths are now put in place first, and each association is added when the model to be fitted lacks it. For such networks the fitted model changes, and a hypothesis that could not be evaluated is now estimated.
  - **A directed path or an association, not both.** `nomo_hypotheses()` refuses a set that gives the same two variables both a directed path and an association, and `nomo_network()` stops when an association hypothesis names a pair the model joins with a directed path. These calls ran before and returned a hypothesis without an estimate. `nomo_network()` also stops when a hypothesis would add a path opposite to another, because a reciprocal pair is not identified without further restrictions; before, it fitted that model. A reciprocal pair that `model` itself writes is fitted as before, and hypotheses about both directions are evaluated.
  - **Residual associations are labeled.** When the fitted model also predicts an endpoint of `A <-> B`, by a directed path or because the endpoint is an indicator of a factor, the covariance `A ~~ B` is between residuals, so the estimate is the association left after those predictors. On `nomo_demo_network`, with Agency predicting both, `Persistence <-> Performance` is estimated at -.06 where the model-implied correlation is .13. The estimate is unchanged. Its `evidence_scope` is now `"residual_association"`, its interpretation says so, and the decision log has a `residual_association` row for review.
  - **Concordance values.** New value `"direction_concordant_above_magnitude"`: an estimate with the predicted sign that is larger than the region allows had been labeled "below magnitude". An estimate whose whole interval lies outside the region is now `"inconsistent"` even when its sign is as predicted; before, a prediction with a magnitude could fail only on sign. The below- and above-magnitude values now apply only when the interval still reaches the region. An estimate without a standard error is `"not_evaluable"` rather than "directionally concordant / imprecise", and the measurement context is a concern when no standard error could be computed. In replication, the above-magnitude value counts as compatible with the prediction, as the below-magnitude value does.
  - **Ordered factors.** Variables of the fitted model stored as ordered factors, in `data` or `validation_data`, are treated as declared in `ordered`. The result's `ordered` lists them (`ordered_detected` holds the ones found), the log has an `ordered_detected` row for review, WLSMV is recorded as the estimator, and the ML-family and FIML checks apply. Estimates are unchanged when the sample's own columns are ordered factors, because lavaan already fitted such columns as categorical. When a column is an ordered factor in only one of `data` and `validation_data`, both samples are now fitted with it declared ordered, which changes the other sample's estimator and estimates. An exogenous covariate stored as an ordered factor is refused, with a message asking for a numeric or dummy-coded column: lavaan does not model a covariate as ordered-categorical. Such a call ended without convergence when the outcomes were continuous, and used the factor's codes as numbers when the model also had ordered outcomes. The result's `estimator` holds the estimator lavaan used, such as `"ML"`, when none was requested, instead of `NA`.
  - **Documentation.** `?nomo_network` defines every `concordance`, `evidence_scope`, and `replication_status` value, with its rule and the interval it uses. It spells out SESOI (smallest effect size of interest), as does the note `print()` gives for a bare `negligible()`.

### Scores, power, and missing data

- `nomo_scores()` tests the parallel model that unit weighting assumes against a model it is nested in (#145). The parallel model is now the fitted model with equal loadings and equal residual variances added and nothing else changed. It is refitted with the fit's own estimator, missing-data handling, and cases. It had been written from the loadings alone and refitted with lavaan's defaults. Results for a default correlated-factor model fitted by maximum likelihood are unchanged.
  - **Fixed factor covariances.** With uncorrelated factors (`orthogonal = TRUE` or `A ~~ 0*B`), the parallel model freed the covariances and could fit better than the fitted model. A negative chi-square difference with p = 1 was then reported as "consistent with these data". For the Holzinger and Swineford three-factor model the difference was -24.95 on 9 df; it is now 36.36 on 12 df, p < .001.
  - **Residual covariances.** A residual covariance was dropped from the parallel model, and so tested together with the parallel constraints (75.3 on 13 df with `x7 ~~ x8`). It is now kept, and only the constraints are tested (69.53 on 12 df).
  - **Robust and FIML fits.** With `estimator = "MLR"` or `missing = "ml"`, the test was reported as "could not be computed". It is now computed, as lavaan's scaled difference under a robust estimator.
  - **When there is no test.** In each of these cases `parallel_test$available` is `FALSE`: a negative or missing difference, a fitted model that is already parallel, loadings fixed at different values, or a fit without a test statistic (`test = "none"`). `parallel_test$note` gives the reason, with lavaan's message where it gave one.
- `nomo_scores()` results gain `rows`, the row of the fitted data each score belongs to (#145). After listwise deletion `scores` has fewer rows than the data, and nothing said which cases were missing from it. `scores` itself is unchanged: `data[x$rows, names(x$scores)] <- x$scores` joins the scores to the data.
- `summary()` of a `nomo_scores` object has class `summary_nomo_scores`, matching the other summary classes (#145). It was `summary.nomo_scores`, which is kept as a second class for one release and then removed.
- `nomo_table()` and `summary()` work on `nomo_power` objects (#145). `nomo_table()` had stopped with "no applicable method", and `summary()` printed base R's listing.
  - **`nomo_table()`.** It returns `"power"` for `nomo_power_rmsea()`, and `"summary"` (the default) or `"parameters"` for `nomo_power_simulate()`. `?nomo_table` lists the types.
  - **`summary()`.** For a simulation it prints every parameter at every sample size; for the RMSEA tests it prints what `print()` shows.
  - **The width hint.** When the simulation table is cut for width, the print names `nomo_table(x, "summary")` rather than `x$parameters`.
- `?nomo_missing` gives its result in the "fields to read" form the other result pages use (#145). `reference` and `fitted_as` are now documented, and so covered by the stability policy. `fits`, the refitted models, had been listed as a result field; it is now named with `call`, `object`, and `ordered` as outside the stable interface. The page also states that `pct_incomplete` and `pct_missing` are proportions. The object itself is unchanged.

### Guided workflow

- `nomo_revise()` keeps the `contentvalidR` handoff of a run whose scales came from content review (#145). The revised run was built from the plain scale list, so it lost the handoff's declared reverse keying: careless-responding indices were computed without recoding and the item audits lost their keying explanation. It also lost the content-review and held-back rows of the decision log and the report's content-review section, and relabeled item membership as researcher input. The revised run now carries all of these, and declared keying follows its items. Where a revision departs from content review, the decision log says so. A scale whose items differ from those content review carried is recorded as the researcher's definition. Each held-back item a revision reinstates, or carried item it removes, gets its own row (`reinstated:<item>`, `removed:<item>`) with the rationale of the revision that made the change. The content-review and held-back rows no longer claim that held-back items are not analyzed. Reverse keying set in `settings$screen` for an item a revision removes is dropped with the item.
- `nomo_run()` resolves `settings$screen$reverse` and `scale_range` one at a time, as `nomo_screen()` does (#145). Giving either one used to replace a `contentvalidR` handoff's declared keying entirely. Supplying the response scale a handoff had not recorded, which the run asks for, silently dropped the handoff's reverse-keyed items, and supplying `reverse` alone blocked the run for want of a range the handoff had. Now each comes from the settings if given there, else from the handoff. A new `keying` row of the decision log records a declared value replaced, or a response scale supplied, whether or not `effort = TRUE`. Results change for calls that gave only one of the two with a handoff that declares keying.
- `nomo_run()` refuses a `settings$screen$reverse` that names an item outside the run's scales, in both effort modes (#145). With `effort = FALSE`, the per-scale item audits used to drop a misspelled name without a word.
- The invariance and network stages of `nomo_run()` refit the measurement model with the CFA stage's `ordered`, `estimator`, and `missing` from `settings$cfa`, unless their own settings name them (#145). A CFA estimated with FIML, a robust estimator, or ordinal indicators used to be refitted downstream as listwise, normal-theory, continuous ML, on a different N, with nothing in the log. An `estimation_settings:<stage>` row records what was inherited and any setting of the stage's own that differs from the CFA's; naming one as `NULL` in a stage's settings asks for the default. Results change for calls that set any of the three for the CFA and request invariance or a network.
- Resuming a run with `guidance = nomo_defaults()` no longer fails because `nomo_defaults()` has gained an element since the run was saved; only a changed value stops the resume (#145).
- `?nomo_run` lists the columns of the run's own decision log (`id`, `stage`, `scope`, `observation`, `reason`, `options`, `consequence`, `decision`, `rationale`, `source`) and the values of `source`. It also says that the component logs, with the columns other decision logs have, are in `nomo_table(x, "component_log")` (#145).

### Report

- **Breaking:** `nomo_report()` now requires `file` (#145). It used to default to `"nomologR-report.html"` in the working directory, and CRAN policy asks packages not to write to the user's file space by default. A call without `file` now stops with an error that names the argument and suggests a path, before anything is written. Calls that give `file` are unchanged; to keep the old behavior, pass `file = "nomologR-report.html"`. The hint that `print()` shows for a complete `nomo_run()` now reads `nomo_report(x, file = "report.html")`, and the README and the reproducible-report article show calls with a path.

### The documented interface

- `nomo_apa_table()` now refuses invalid arguments it used to ignore (#145):
  - a `type` for a result that has one table (`nomo_reliability()`, `nomo_invariance()`, `nomo_retest()`);
  - a `title` that is not a single string, which was pasted together.
- The interface frozen at 1.0 is now written down where it was missing (#145). No value, name, or behavior changes.
  - **Conventions in returned tables.** A new section of `?nomologR` records three differences between tables that are kept as they are:
    - **Proportions.** Columns named `pct_*` (`pct_missing` in `nomo_screen()` and `nomo_missing()`, `pct_incomplete`, `pct_dropped`) hold proportions from 0 to 1, as do `*_prop` and `proportion_*` columns. `percent_unique` in `nomo_screen()`, and its reference in `nomo_defaults()`, are percentages.
    - **Flags.** Stored flags use three vocabularies:
      - `KEEP`, `REVIEW`, and `STRONG REVIEW` in the loading tables of `nomo_efa()`, `nomo_cfa()`, and `nomo_validity()`;
      - `none`, `review`, and `concern` in the item review of `summary()` for `nomo_screen()`;
      - `info`, `review`, and `concern` in decision logs and the other evidence tables, which may also mark a value that could not be computed as `unavailable`.
      The section gives the one wording that printed output, plots, and reports use for each.
    - **P-values.** A p-value is `p_value`, with two exceptions:
      - lavaan's parameter tables use `pvalue`;
      - the fit tables that keep `lavaan::fitMeasures()` names (`chisq`, `pvalue`, `cfi`, ...) do too. These are the `models` of `nomo_esem()`, `nomo_method_variance()`, and `nomo_compare()`, and the fit of `nomo_invariance()` and `nomo_network()`.
      The fit tables of `nomo_cfa()` (long by design) and `nomo_missing()` use `chi_square`, `p_value`, `CFI`, ....
  - **Help pages.** The pages of `nomo_screen()`, `nomo_cfa()`, `nomo_efa()`, `nomo_validity()`, `nomo_reliability()`, `nomo_network()`, and `nomo_missing()` name the units and flag values of their own tables. The stability policy lists the columns of each analysis's decision log.
  - **`nomo_table()`.** Its help page lists the `"factors"` type of `nomo_hierarchical`, and a new "Fit tables" section names the index columns of each fit table.
  - **`nomo_apa_table()`.** Its help page lists the fields of the returned table (`number`, `title`, `body`, `stub`, `notes`, `source`) and says that headings, cells, and notes are written in Markdown.
  - **`nomo_methods()`.** The help for `stage` lists `"scores"`.


## New methods (#129)

- Sample-size planning (#129).
  - `nomo_power_rmsea()` gives the power of MacCallum, Browne, and Sugawara's (1996) RMSEA tests of close, not-close, and exact fit, or the smallest N reaching a target power. It reproduces their sample sizes (for example, 132 for close fit and 178 for not-close fit at 100 df). The degrees of freedom come from a model string, `nomo_model()`, `nomo_cfa()`, or lavaan fit.
  - `nomo_power_simulate()` is Muthén and Muthén's (2002) Monte Carlo approach. It generates data from a population model with values, fits the analysis model at each N, and reports convergence, improper solutions (Wolf et al., 2013), parameter and standard-error bias, coverage, and power. It also gives the smallest simulated N meeting their references: biases within 10%, coverage .91 to .98, and power .80 for the focus parameters. The result records the `seed` and the call. Estimates are in the metric `lavaan::sem(std.lv = TRUE)` sets, so a latent regression is not a standardized coefficient; the help page shows how the two differ. `nomo_power_simulate()` is experimental: it may change during 1.x without a deprecation period.
- New `nomo_esem()` fits a measurement model as exploratory structural equation modeling (ESEM) beside its CFA (#129). Every item may load on every factor, so the cross-loadings a CFA fixes at zero are estimated, and the model still gives fit and standard errors (Asparouhov & Muthén, 2009).
  - **Rotation.** By default it uses the target rotation Marsh, Morin, Parker, and Kaur (2014) recommend for an a priori structure (Browne, 2001): each item's own loading is free and its cross-loadings are rotated towards zero. Geomin is available too.
  - **The comparison.** It reports both models' fit (`models`), their factor correlations and the change between them (`factor_correlations`), and their likelihood-ratio test (`comparisons`); the two lavaan fits are in `fits`. The ESEM is flagged for review when it fits better on TLI and RMSEA, which penalize its extra parameters; lower ESEM factor correlations then show that the CFA's zero cross-loadings are inflating them. Cross-loadings at or above `efa_crossloading_reference` and main loadings below `efa_loading_reference` are flagged as evidence about items, not instructions.
  - **Other outputs.** `print()`, `summary()`, `nomo_table()`, `nomo_methods()` (which credits WLSMV for ordered indicators and FIML when it was requested, as for `nomo_cfa()`), and a section in the measurement-evidence article, where ESEM finds both features built into `nomo_demo_continuous`.
- New `nomo_method_variance()` for common method variance, following Williams, Hartman, and Cavazotte's (2010) comprehensive CFA marker technique (#129). Given a measurement model and the indicators of a marker variable, it runs the three phases the authors specify:
  - **Model comparisons.** It fits the CFA, Baseline, Method-C, Method-U, and Method-R models (`models`, with the scaled chi-square and robust indices under a robust estimator, as `nomo_cfa()` reports them), and compares them (`comparisons`) to test whether marker-based method variance is present, whether its effects are equal, and whether it biases the substantive correlations.
  - **Reliability decomposition.** It splits each factor's reliability into substantive and method parts (`reliability`).
  - **Sensitivity.** It fits the Method-S(.05) and Method-S(.01) models, with the method loadings at the upper ends of their intervals. `correlations` gives each pair of factors' correlation in every model, with p-values named `retained_p_value`, `method_s_05_p_value`, and `method_s_01_p_value`.
  
  The log explains what the marker must be: theoretically unrelated to the constructs, and tapping the biases the measurement context invites. It also says what the technique cannot do. With a nonideal marker it can find method variance that is absent, and it does not recover substantive correlations accurately (Richardson, Simmering, & Sturman, 2009). The measurement-evidence article works an example. `nomo_method_variance()` is experimental: its output may change during 1.x without a deprecation period.
- New `nomo_retest()` for test-retest reliability (#129).
  - **The intraclass correlations.** For `scores`, the columns holding a composite on two or more occasions (or a named list of them, one per composite), it estimates ICC(A,1), the two-way mixed-effects, absolute-agreement, single-measurement form Koo and Li (2016) recommend for test-retest data, with its 95% interval. Beside it are the consistency form ICC(C,1) (McGraw & Wong, 1996) and the mean change between occasions. A systematic shift is flagged, because ICC(A,1) counts it as disagreement.
  - **Koo and Li's description.** The reliability is described in Koo and Li's terms (poor, moderate, good, excellent), read from the interval as they ask, and an interval that reaches "poor" is flagged.
  - **Measurement error.** It reports the standard error of measurement, SD x sqrt(1 - ICC), and the smallest detectable change, 1.96 x sqrt(2) x SEM (Weir, 2005).
  - **Reliable change.** Each person's reliable change index follows Jacobson and Truax (1991); it exceeds 1.96 exactly when the change exceeds the smallest detectable change.
  - **Other outputs.** `nomo_table()`, `nomo_apa_table()`, `nomo_methods()`, and the measurement-evidence article cover it.
- `nomo_network()` can model an observed composite, such as a scale mean, as a single-indicator latent variable, correcting the relations it enters for its unreliability (#129). Name it in `single_indicators` with its reliability, as a number or a `nomo_single_indicator()` record. The record can take omega and its bootstrap uncertainty from a `nomo_reliability()` result. The composite becomes the one indicator of a latent variable of the same name, with its error variance fixed at (1 - reliability) x its variance, so the model syntax and hypotheses are unchanged. The method goes back to Spearman's (1904) correction for attenuation and the SEM textbooks (Hayduk, 1987; Bollen, 1989), and Savalei (2019) found it the most accurate option in samples of 30 to 200 when the reliability is close to its true value. So nomologR:
  - refits each hypothesis with each reliability .05 and .10 lower and higher, records the result in `single_indicator_sensitivity`, and flags any hypothesis whose concordance changes across that range;
  - adds the reliability's uncertainty to the standard errors, intervals, and concordance when its standard error is known, as Oberski and Satorra (2013) derive; otherwise the log says the standard errors treat it as known;
  - flags coefficient alpha for review, since it understates reliability when loadings differ and so overcorrects.
  
  `hypothesis_evidence` gains `se_reliability_added`, `nomo_table()` gains the `"single_indicators"` and `"sensitivity"` types, and the APA hypotheses table notes the correction. The nomological-network article shows the correction recovering the population path (.45) from the Persistence mean (.41 uncorrected, .46 corrected). The observed-endpoint note in the log now names the option.

## Invariance across groups and occasions (#129)

- `nomo_invariance()` reports latent means, which are known-groups evidence in structured-means form (#129). At each level that holds intercepts equal, `latent_means` gives each group's latent means relative to the reference group. Under the default `ID.fac = "std.lv"` they are in the reference group's latent standard deviations (Hancock, 2001), with intervals. The summary shows them, `nomo_table(x, "latent_means")` returns them, and the log says they are comparable only with invariant intercepts, fully or partially (Byrne, Shavelson, & Muthén, 1989). On `nomo_demo_network`, holding the biased `ag3` intercept equal inflates the Agency difference between modes to .44 SD. Releasing it gives .33 [.17, .49], which covers the population's .25. The measurement-invariance article walks through this.
- New `nomo_invariance_longitudinal()` for measurement invariance across occasions (#129). It asks whether the same items mean the same thing each time the same people answer them, so that a change in scores can be read as a change in the construct (Widaman, Ferrer, & Conger, 2010).
  - **The model.** The model is written for one occasion, in the items' own names. `columns` (default `"{item}_{occasion}"`) maps each item and occasion to a column. Each item's unique factors are correlated across occasions, over all lags or up to `auto`.
  - **The sequence.** The configural, metric, scalar, and strict levels, the ordered-item sequences and identification (with Liu et al., 2017, for Millsap and Tein's conditions over time), researcher-specified partial releases, and score diagnostics are those of `nomo_invariance()`, applied across occasions through semTools' longitudinal arguments. A release names the item as in the one-occasion model, such as `"w3 ~ 1"`, and the diagnostics are labeled by item and occasions, such as `Intercept: w3 (t1 vs. t3)`.
  - **Latent change.** Once intercepts are invariant, fully or partially, `latent_means` gives each later occasion's latent mean in the first occasion's latent standard deviations, with intervals.
  - **Other outputs.** The result is also a `nomo_invariance` object, so `print()`, `summary()`, `nomo_table()`, `nomo_apa_table()`, and `plot()` work as they do across groups. `nomo_methods()` gains `longitudinal_invariance`.
- New teaching dataset `nomo_demo_longitudinal`: four Wellbeing items answered on three occasions, with a latent mean rising .30 and then .50 SD and the `w3` intercept drifting .40 after the first occasion. In the measurement-invariance article, holding that intercept equal inflates the change at the third occasion to .66 SD. Releasing it gives .55 [.43, .67], which covers the population's .50.
- `nomo_invariance()`'s option checks and level fitting are now shared with `nomo_invariance_longitudinal()`. Its results are unchanged.

## Presentation (#89)

- The output gallery in `dev/output-gallery.R` (#89) now covers the features added for 1.0 (#129): ESEM, method variance, test-retest reliability, power, latent means, longitudinal invariance, and single indicators. Reading their output as a user would led to these display changes. Computed values, decision-log text, and `nomo_table()` output are unchanged.
  - **Invariance.** A summary with a strict level had lost RMSEA and SRMR at 80 columns, pushed out by the cumulative "Constraints" column. The fit table now leaves that column out, and a line beneath it says what each level adds: "Held equal: loadings from metric; intercepts from scalar; residuals from strict." A researcher-specified release is named beside its level, as in "intercepts from scalar, except `ag3 ~ 1`", so the line does not say that the released intercept is held equal. Across occasions, the latent-change column is headed "Change". The fit and change plots angle the level names, which ran together with four levels. The local-strain plot gives each level its own panel, with the score axis from zero: in one panel, close scores at two levels had been drawn on one spot, hiding one of the levels.
  - **Method variance.** Each comparison says what it asks, such as "Method variance present?", in a short form of the question `comparisons` stores. The closing note defines the Baseline, Method-C, Method-U, Method-R, and Method-S models. The correlations are headed by the models' names, such as "Method-C" and "Method-S(.05)", rather than "Retained" and "S(.05)". The method loading's p-value is headed "Method p". The print and summary list the decision log's flagged entries, as `nomo_esem()`'s do.
  - **Test-retest reliability.** The print lists the flagged entries too. SEM, SDC, and SD have two decimals, like the intervals beside them.
  - **Alignment.** Percentages are right-aligned like the numbers beside them, in the method-variance tables and in `nomo_power_simulate()`'s print. So are the reliability and its standard error in the network summary's single-indicator table.
  - **Power.** In `nomo_power_simulate()`'s print, convergence, improper solutions, and the biases are percentages with one decimal, as the bias references are, so 499 converged replications of 500 no longer round to 100%. The note says what "Max bias" and "Max SE bias" are. `nomo_power_rmsea()`'s print gives power to two decimals, as `nomo_power_simulate()`'s does, and its target as "0.80" rather than "0.8"; the smallest N is a line of its own under the header.

## The 1.0 contract (#113)

- `nomo_missing()` and `nomo_apa_table()` leave the experimental list and are covered by the stability policy (#113). `nomo_missing()`'s flagging rule rests on Schafer and Graham (2002, p. 157): a bias beyond about half a standard error is practically important, because it degrades interval coverage. Separating sampling variability from bias would be new output, added alongside the rule. `nomo_apa_table()`'s `type` values and returned structure are covered. A table's formatting (headings, number formats, notes) may still be corrected where it departs from APA style, with the correction described in NEWS.

## The contentvalidR handoff (#53)

- The content-review reader is tested against `contentvalidR` 0.10.0 and 0.10.1 output too (#53). Their handoffs are identical to 0.9.0's apart from the producer version and date, and the reader needed no change.

## Documentation (#129, #138)

- Documentation for 1.0 (#129, #138). Get started and the research-basis article say that every analysis assumes reflective measurement, why the tools do not apply to formative measures, and where the criteria for choosing between the two are. The research-basis article places each method added in #129 between historical and contemporary practice, and gains the McGraw and Wong (1996) entry its test-retest paragraph cites. The README walks through the new functions stage by stage, lists `nomo_demo_longitudinal`, says what 1.0.0 contains, and notes that `nomo_method_variance()` and `nomo_power_simulate()` stay experimental after 1.0.0; its license note now says that releases from 0.2.0 on are GPL-3. Get started adds the new functions and dataset to its workflow and dataset tables and its learning path, and the package description names the new methods. ROADMAP lists the gap review in the 1.0.0 scope, names 0.9.0 as the current release, and records the feature freeze on 2026-10-13. The README and the exploratory-workflow article read the documented `correlation_method` and `item_types` of `nomo_factors()` rather than their undocumented aliases, and headings that pandoc had rendered as text in NEWS and the measurement-evidence article are fixed.

# nomologR 0.9.0

nomologR 0.9.0 is the last minor release before 1.0.0, the stable release planned
jointly with `contentvalidR` (#53). It carries the 1.0 scope selected so far
(#113); what is added before the release candidate on 2026-10-17 is recorded
there. It is about how the package presents itself and what it teaches:

- **Output that reads well.** Every `print()` and `summary()` is redesigned
  (#89): aligned tables with the columns that matter, explanations in full,
  APA-style numbers, and one flag wording ("review", "concern") in the console,
  plots, and reports.
- **A record of how practice changed.** `nomo_methods()` gives the year each
  method entered the literature and, for a historical method, the contemporary
  methods that took over its question. The research-basis article draws them
  into a timeline.
- **Teaching, and the bridge from content review.** "Teaching with nomologR"
  collects exercises whose answers are known from the population model. "From
  content review to empirical screening" carries a `contentvalidR` review's
  items through the screen, on the joint walkthrough's data, now shipped as
  `nomo_demo_walkthrough` (#60).
- **Manuscript tables for validity evidence.** `nomo_apa_table()` formats
  `nomo_validity()` results: each construct pair's latent correlation with its
  interval beside HTMT2, and a table of AVE.
- **The 1.0 contract.** Every help page names the fields of the object it
  returns (#114).

**No computed estimate changes.** Compared value by value with 0.3.0 across 22
analyses spanning every stage, every estimate is identical. What differs:

- With two or more declared scales, `nomo_screen()` reviews each item within
  its own scale: its item-rest correlation against the rest of that scale
  (`scale`, `scale_item_rest_r`, `scale_item_rest_n`), and its negative
  inter-item correlations only with items of the same scale
  (`scale_negative_interitem_n`). Which items are flagged can change.
  `corrected_item_rest_r` and `negative_interitem_n` are unchanged.
- In a multi-group `nomo_validity()` result, `latent_correlations$block` names
  the groups by label rather than by number.
- A CFA that is not identified (negative degrees of freedom) is logged as a
  concern rather than a review.
- Wording: counts agree with their nouns, some decision-log observations read
  differently, and the invariance summary lists the largest diagnostics rather
  than the first.

**Breaking changes.**

- `nomo_missing()` takes the fitted model as `fit` rather than `x` (#114). Calls
  that pass the model first without naming it are unaffected. The function was
  marked experimental, so there is no deprecation period.
- `nomo_screen()` checks `scales` when `effort = FALSE` too, since the
  within-scale correlations use it. A `scales` that names items not being
  screened now stops with an error instead of being ignored.
- `nomo_report(apa_tables = TRUE)` adds the validity tables, and when the run
  has a validity stage, its construct-pair table takes the place of the CFA
  factor correlations.
- An invalid choice stops with a message that names the argument, such as
  "`method` must be one of ...", instead of base R's "'arg' should be one of".
  Only code that matches the old message text would notice.

nomologR 0.3.0 is still in CRAN's queue for new submissions (#39); until CRAN
accepts it, install from R-universe or GitHub.

- With two or more declared scales, the item audit reviews negative inter-item correlations only within a scale (#113), as it does item-rest correlations. Two items of one scale should correlate positively, so a negative pair there is a keying or wording clue. Items of different constructs need not correlate at all, and a correlation near zero is negative about half the time. In `nomo_demo_network`, social desirability is uncorrelated with the other two constructs by design, and its near-zero negative correlations with their items had flagged all eight Agency and Persistence items. `relationship_summary` gains `scale_negative_interitem_n`. `negative_interitem_n` and `inter_item_correlations` are unchanged. Negative correlations between scales are logged as information (`negative_pairs_between_scales`). The log entry for negative pairs now agrees in number: "1 estimable inter-item correlation is negative".
- `nomo_apa_table()` formats a `nomo_validity()` result (#113). `type = "discriminant"`, the default, gives each pair of constructs one row: its latent correlation with a 95% confidence interval (Rönkkö & Cho, 2022), beside HTMT2 (Roemer et al., 2021) and HTMT (Henseler et al., 2015) when they were computed. `type = "convergent"` gives each construct's number of indicators and AVE. The pair table is deliberately not the Fornell-Larcker matrix with the square root of AVE on its diagonal, the comparison that `nomo_methods()` records as historical because it often misses constructs that are not distinct. Correlations and AVE lose their leading zero; the ratios, which can exceed 1, keep it. A multi-group model's tables add a Group column. `nomo_report(apa_tables = TRUE)` adds both tables after reliability, and the pair table takes the place of the CFA factor correlations, which it repeats.
- In a multi-group `nomo_validity()` result, `latent_correlations` names its groups (`block`) by their labels, as `ave` does, rather than by number. `lavaan::standardizedSolution()` numbers the groups, so the two tables of one result had disagreed, for example "1" and "2" against "Pasteur" and "Grant-White".
- When two or more scales are declared, through `scales` or a `contentvalidR` handoff, the item audit reads each item's corrected item-rest correlation within its own scale (#113). An item is scored on its scale's total, so the rest of that scale is what it has to agree with (Nunnally & Bernstein, 1994; Clark & Watson, 2019). Pooled across constructs, the items of a distinct scale look weak: in `nomo_demo_network`, the three social-desirability items correlate .15 to .17 with the rest of all eleven items and were flagged, and .52 to .57 with the rest of their own scale. The relationship summary gains `scale`, `scale_item_rest_r`, and `scale_item_rest_n`, and `corrected_item_rest_r` keeps the pooled value. The flag, the decision-log entry (which names the scale), the summary table, and the item-rest plot use the within-scale value. A guided run's per-scale audits were already within-scale; the instrument-wide audit that `settings$screen$effort = TRUE` adds now agrees with them. `scales` is checked when `effort = FALSE` too, since it now matters there.
- The item-rest plot draws its values in a column past the bars and the reference line (#89). A flagged item sits just below the reference, so its value, drawn beside the bar, had crossed the dashed line.
- `nomo_missing()` takes the fitted model as `fit`, as `nomo_reliability()`, `nomo_validity()`, `nomo_scores()`, and `nomo_hierarchical()` do (#114). It had been `x`. The function is experimental until 1.0.0, so the name changes without a deprecation period. Calls that pass the model first, without naming it, are unaffected.
- A guided run or validity result with a single construct no longer warns "Unknown or uninitialised column" when printed. With one construct there are no pairs, and the pair tables have no columns; the ordering and key-evidence code read them with `$`, which warns on a tibble.
- A new article, "Teaching with nomologR", collects exercises built on the teaching datasets' known answers. Students find the weak and cross-loading items, check whether the factor-retention criteria agree, compare alpha with omega, handle ordered categories, and tell a structural path from a correlation (including why a path that is not significant does not show that it is negligible). They also find the non-invariant intercept, see where content review and the empirical screen disagree, and see why an item-total correlation depends on which total it is computed against. Each answer is stated from the population model, not from one run. The article also suggests how to use teaching and research modes, decision logs, reports, and `nomo_methods()` in a course.
- `nomo_methods()` records how practice changed (#113). `introduced` is the year a method entered the literature, taken from the publication that introduced it, and given only when the registry cites that publication (46 of 89 methods so far). `contemporary_practice` names, for a historical method, the contemporary methods that now answer its question: parallel analysis for the eigenvalue-greater-than-one rule, omega for coefficient alpha, HTMT2 and latent-correlation intervals for the Fornell-Larcker comparison, FIML for listwise deletion. The research-basis article gains a section, "How practice changed", with a timeline from 1937 to 2022 and a table of what took over from each historical method, both drawn from the registry. The KMO entry now cites Kaiser (1970), which introduced the index. The methods table in `nomo_report()` shows the year each method was introduced, so an archived report carries the same record.
- Every function's help page now names the fields of the object it returns (#114). The stability policy covers "the documented fields of the objects they return", but the Value sections had described contents in prose without naming fields, so most fields users read, such as `standardized_loadings` and `fit_evidence` on a `nomo_cfa`, were not named in the contract. Each Value section now lists the fields to read, one line each, and says that the remaining fields (the call, settings, and engine intermediates) are not part of the stable interface. This changes documentation only. `?nomo_efa` no longer explains `factor_count` with an internal milestone label.
- A new article, "From content review to empirical screening", carries the items a `contentvalidR` review carried forward through the empirical screen, on the shared teaching data of the joint walkthrough (#60, #53). It shows the two stages disagreeing, which is the reason the pair exists. `EF4` passes content review and carries almost no common variance. `EF3` is flagged empirically but is the only item covering part of the domain. `TF4` loads on both facets, and `TF6` is not invariant across cohorts. The article builds without `contentvalidR` installed.
- New teaching data `nomo_demo_walkthrough` and `nomo_demo_walkthrough_items`: 400 simulated responses to twelve Study Persistence items in two cohorts, with each item's built-in role (#60). They are `contentvalidR`'s walkthrough files, copied unchanged from its v0.9.0 release with their git blob hashes. `contentvalidR`'s generator is in `data-raw/walkthrough/`.
- The item audit explains a negative item-rest correlation for an item declared reverse-keyed, by a `contentvalidR` handoff or by `reverse` (#60). It says the sign is what such an item shows before recoding, and gives the item-rest correlation computed on a copy recoded as declared. If the recoded correlation is still negative, it says the keying does not explain it. `nomo_run()` passes a handoff's declared keying to each scale's audit for this. The data, the returned correlation, and the flag are unchanged; nomologR never recodes data.
- The measurement-invariance summary lists the ten largest equality-constraint diagnostics across all levels. It had listed the first ten in level order, so the metric level filled the table, and a scalar-level strain such as an intercept never appeared. On `nomo_demo_network`, the known `ag3` intercept (score 61) had been hidden behind a metric loading (score 5).
- Three edge cases read better (#89). The CFA summary of a model that did not converge says "No fit index is available: the model did not converge." It had listed the reference values with nothing beside them. The item-audit summary shows a column with one observed value as "constant" rather than "binary". The returned `item_type` is unchanged. The missing-data print says why a strategy was not fitted, such as "FIML: Not fitted: no modeled variable has missing values." for complete data, and shows its N as "-" rather than `NA`.
- An invalid choice names the argument it was given for (#89): `nomo_scores(fit, method = "eap")` now stops with "`method` must be one of "sum", "mean", "regression", or "bartlett", not "eap"." instead of base R's "'arg' should be one of …". This applies to every argument with a fixed set of values, such as `type` in `plot()`, `nomo_table()`, and `nomo_apa_table()`. Partial matching (`type = "load"`) works as before.
- `nomo_cfa()` says when its fit indices cannot test the model (#89). A one-factor model with three indicators is just identified (df = 0): it reproduces the covariances by construction, and its print had shown "RMSEA 0.000 | SRMR 0.000" like a well-fitting model. A model with negative degrees of freedom, such as a factor with two indicators, is not identified. The print now reads "Fit: not testable (df = 0, just identified)", the summary explains why, and the guided run's key evidence says the same. The decision log's degrees-of-freedom entry, which was already recorded, now names the case, and a model that is not identified is a concern rather than a review. Warnings that lavaan raises while nomologR reads the results, such as "Could not compute standard errors", are kept with the model's engine warnings instead of printing at the console. The "NaNs produced" warnings that `lavaan::fitMeasures()` raises for negative degrees of freedom are no longer shown.
- Counts in printed output agree with their nouns (#89): "2 factors", "1 pair", "1 more degree of freedom", rather than "2 factor(s)". This covers the printed counts in scores, reliability, validity, hypotheses, and partial releases, and the notes in comparisons and bifactor models. Counted phrases in decision-log observations and report summaries agree too, with their verbs: "1 indicator was declared ordered", "10 of 12 reviewed items were carried".
- Plot legends use the display flag wording too (#89). The CFA loading and fit plots and the AVE and HTMT plots label their points "none", "review", or "concern" under "Flag". The loading plot had shown the recorded values `KEEP` and `REVIEW` under "Review", and the validity plots `info` and `review` under "Signal". The item evidence map calls an informational signal a "note", which is how the console shows it. Each flag level keeps the same point shape from plot to plot.
- The redesigned console output extends to the guided workflow: `print()` and `summary()` for `nomo_run()` (#89). The print opens with the run's status, mode, sample design, and scales, then lists what the run found so far as "Key evidence" (item flags, suggested factors, CFA fit, the omega range, validity and invariance results). A request repeated for several scales, such as a factor-count decision for each of three scales, is shown once with the scales it applies to, rather than once per scale. Research mode leaves out the teaching-mode reason, options, and consequences, and keeps what each scale showed and an example. The summary is about a third of its former length: a stage table, recorded decisions as bullets, the component recipe, and a count of the methods used with the primary method of each stage, pointing to `nomo_methods()` and `nomo_table()` for the full entries. The gallery in `dev/output-gallery.R` now reports no presentation faults.
- `nomo_report()` tables are easier to read (#89). Column headings are words, not code names: "Omega CI lower" for `omega_ci_lower`, "Change in CFI" for `delta_cfi`, "Corrected item-rest r" for `corrected_item_rest_r`. Flags use the console's wording ("review", "concern"). A missing or empty cell is an em dash rather than `NA` or nothing, and TRUE/FALSE is yes/no. Item, scale, and factor names are left as written. The values are otherwise unchanged, and `nomo_table()` still returns the code's column names and flag values. The citations table gives each package's reference as text. It had held R's whole `print(citation())` output, including a BibTeX entry whose `@Manual{` pandoc turned into a stray citation, and LaTeX such as `\texttt{semTools}`.
- The redesigned console output extends to comparisons, invariance, partial releases, hypotheses, networks, splits, and the notes of `nomo_apa_table()` (#89).
  - Wide tables are split: model fit apart from information criteria, difference tests apart from changes in fit, and fit by level apart from changes between levels.
  - Levels that failed or raised warnings are listed with what went wrong.
  - Measurement evidence in a comparison reads one column per model.
  - A hypothesis table states once that every relation is on the standardized scale, rather than repeating it in each row.
- The redesigned console output extends to reliability, validity, scores, missing-data sensitivity, and hierarchical models (#89). Tables replace wrapped tibbles. Severity-tagged notes print as `review:` and `concern:` bullets. Index names read as written in the literature ("omega hierarchical", "ECV"). A column that holds one value throughout, such as a block column that always says "overall", is left out. The note on correlational accuracy names the pair of factors whose score correlation strays furthest from the factor correlation. It had named one of the two, and which one depended on rounding, so it differed between platforms.
- The content-review reader is now tested against `contentvalidR` 0.8.0 and 0.9.0 output as well as 0.6.0 and 0.7.0 (#53). The added fixtures include a nine-expert panel whose carry decision changed between producers. Seven of nine experts rate item N7 relevant, which is what Lynn (1986) requires. `contentvalidR` 0.7.0 compared the I-CVI with a rounded .78 and held N7 back; 0.8.0 compares counts and carries it. The test confirms that `nomo_screen()` follows the decision the handoff records rather than inferring one from the producer version. The reader needed no change: 0.8.0 and 0.9.0 keep schema version 1 with the same fields and columns.
- The redesigned console output extends to the item audit, factor retention, and EFA (#89): `print()` and `summary()` for `nomo_screen()`, `nomo_factors()`, and `nomo_efa()`. The screen summary lists each flagged item with the decision log's own explanation rather than internal metric names. The factor-retention summary prints the reason a criterion was not run, and the Bartlett result, in full rather than cut off in a table cell. The EFA summary no longer hides the item flags behind "2 more variables".
- Plot titles, subtitles, and captions are wrapped to fit the plot (#89). ggplot2 does not wrap them, and ten plots had text long enough to run off the edge at 7 × 5 inches: four CFA plots, three EFA plots, two network plots, and the hierarchical variance plot. The CFA fit plot now draws CFI and TLI, which sit near 1, and RMSEA and SRMR, which sit near 0, in separate panels with their own scales, and a legend with one entry is no longer drawn.
- Console output begins a redesign for readability (#89), starting with `nomo_cfa()`'s `print()` and `summary()`. Tables are aligned text with the columns that matter and no type rows. A flagged loading's explanation is printed in full rather than cut off inside a table cell. Numbers have fixed decimals, p-values follow APA style, and prose wraps to the console width. Flags are shown in one wording across the package: nothing for no flag, then "review", then "concern". This is display only: returned objects, `nomo_table()` types, and decision-log values keep their values. `dev/output-gallery.R` regenerates every print, summary, and plot and reports the remaining presentation faults.
- `nomo_cfa()`, `nomo_invariance()`, and `nomo_network()` name any variable the model uses that the data lack, before fitting (#89): "`model` names variable(s) not found in `data`: agX. Check the spelling against names(data)." A misspelled item had reached lavaan, and the error named lavaan's internal step (`lavaan->lav_step02_options()`) rather than the item. The more specific argument checks, such as for `ordered`, still come first, and model syntax that lavaan cannot parse is still reported by lavaan.
- `nomo_table()` works on every measurement-stage result: `nomo_factors`, `nomo_efa`, `nomo_cfa`, `nomo_reliability`, and `nomo_validity` (#89). They had no method, so the documented way to extract a table failed for five of the package's main results. Each returns the tables its report section shows. `nomo_report()` now gets them through `nomo_table()`, so the two cannot drift apart. `?nomo_table` lists every supported object and type, including `nomo_screen` and `nomo_scores`, which were missing from it.
- `plot(<nomo_screen>, type = "responses")` draws a histogram for a continuous item (#89). It drew a bar for every distinct value, which for continuous responses means one bar per respondent: an unreadable plot, with a percentage label on each bar. Categorical items keep their bars. When the selected items mix the two, the categorical items are drawn and the caption names the continuous ones and how to plot them.
- `nomo_validity()` lists each construct pair once in its print, summary, and report section, with the latent correlation and HTMT-family values in the same row (#89). Latent correlations name a pair in model order and HTMT names it the other way round, so the two never merged. Each pair appeared twice, once with its correlation but a signal of "unavailable" and once with its HTMT values, and the print counted twice as many pairs as there are. Pairs are now shown in model order, including in the HTMT plot. The returned tables and every computed value are unchanged.
- `nomo_report()` headings are rendered for every scale of a multi-scale run (#89). A heading that followed a plot, which is every scale after the first in the item audit and factor retention, plus "Careless responding", was run into the plot's paragraph. It appeared as literal `## Scale: ...` text in HTML and Word, and the table of contents filed every scale's tables under the first. The report tests had rendered only single-scale runs or had turned plots off; a two-scale report with plots is now tested in both formats.
- The reproducible-report article's overwrite example shows the refusal message with its temporary directory as `<report_dir>`. It had printed the full temporary path of the computer that built the article, including a user name, and the path changed on every build. The message still comes from `nomo_report()`, and the example still shows that an existing report is refused unless `overwrite = TRUE` is given.

# nomologR 0.3.0

nomologR 0.3.0 is the first release submitted to CRAN (#39), and its scope was
kept deliberately lean for that reason (#38). It does three things:

- **Connects content review to empirical validation.** A handoff from
  `contentvalidR` now flows into `nomo_screen()` and `nomo_run()`, and the
  reasons for each item's carry decision travel with it (#46).
- **Folds the v0.2.1 tools into the guided workflow and its report.** These
  are careless-responding screens, scores, missing-data sensitivity, and
  manuscript tables (#73).
- **Starts the public interface's stability promise.** From this release the
  interface changes only after a deprecation period, under the policy written
  on `?nomologR` (#74).

Test coverage is back to every executable line (#72). Reviewing each uncovered
line found four defects, now fixed. Certification is recorded in #75.
Distribution is R-universe until CRAN accepts the submission. The joint 1.0.0
release with `contentvalidR` is tracked in #53.

- CRAN preparation for the first submission (#39).
  - **Spelling.** `DESCRIPTION` declares `Language: en-US`, and the package's text uses American spelling throughout. About 40 British spellings (`analysed`, `modelled`, `behaviour`) are changed in messages, help pages, articles, and NEWS; only wording changes, not code. `spelling::spell_check_package()` now finds nothing, with author names and technical terms listed in `inst/WORDLIST`.
  - **URLs.** `urlchecker::url_check()` finds all 131 URLs correct.
  - **Build.** `.git` and `.gitignore` are excluded from the build. A git worktree has a `.git` *file*, and a hidden file in the tarball is what CRAN returned `contentvalidR` 0.3.1 for.
  - **Test time.** Under CRAN conditions the tests took about 15 minutes on a slow Windows machine; they now take about 3. The 117 tests that took 2 seconds or more are skipped on CRAN: simulations, full guided runs, and rendered reports. Two stay on CRAN because they check that nomologR reproduces lavaan's own estimates, so a lavaan change that alters them is caught there. Continuous integration still runs every test, and coverage is unchanged.
  - **Examples.** `nomo_scores()` and `nomo_apa_table()` gain runnable examples, so every exported function now has one.
  - **Description.** The `DESCRIPTION` cites the two works that frame the package, Cronbach and Meehl (1955) and Flake, Pek, and Hehman (2017), with their DOIs.
  - **References.** The research-basis article's reference list gains the APA *Publication Manual*, which `?nomo_apa_table` already cited. Its "Planned" notes now say that longitudinal invariance and multiple imputation are candidates for v0.4, not v0.3.

- `nomo_report(apa_tables = TRUE)` appends a *Manuscript tables* appendix (#73, last of three parts). It holds the `nomo_apa_table()` tables for the results a run holds: CFA loadings, fit, and factor correlations; reliability; and, when present, invariance and the network's hypotheses and fit. They are numbered in the order the report presents them. A table that does not apply is left out rather than failing the report. The default, `FALSE`, leaves reports unchanged.

- `nomo_run()` can score the measurement model and compare missing-data strategies, and its report shows both (#73, second of three parts).
  - **Scores.** `settings = list(scores = list(method = "sum"))` scores the model with `nomo_scores()`. The researcher must name the method; a request without one, including `scores = list()`, is refused, because nomologR does not choose a scoring method.
  - **Missing-data sensitivity.** `settings = list(missing = list())` compares strategies with `nomo_missing()` for the measurement model, and for the network too when one is requested.
  - **When they run.** Both run after convergent and discriminant evidence, so they are in view when the researcher decides whether to carry the model forward. Each matches the standalone call.
  - **Not stages.** Neither is a stage, so the stage table keeps its shape, and a run that requests neither is unchanged.
  - **If they cannot be computed.** Requested evidence that cannot be computed is recorded in the design log and the run continues, since no later stage depends on it.
  - **Resuming.** Both can be requested when resuming before the CFA, and are locked once computed.
  - **Where they appear.** Their evidence joins the component log, their methods are credited to the run, and the report gains "Scores" and "Missing-data sensitivity" sections.

- A written stability and deprecation policy, on the package help page (`?nomologR`) and in the README (#74). From 0.3.0, the first CRAN release, the public interface changes only after a deprecation period.
  - **Covered:** exported functions, documented arguments and defaults, documented fields of returned objects, `nomo_table()` types, and decision-log columns.
  - **Breaking changes:** any change that breaks the interface, including a changed default that changes results, is deprecated for at least one minor release first.
  - **Experimental until 1.0.0:** `nomo_missing()`, whose flagging rule may be refined, and the layout of `nomo_apa_table()` tables. Both are marked in their help pages.
  - **Required by the joint release:** #53 requires this policy for the joint 1.0 release with `contentvalidR`.

- `nomo_run()` computes careless-responding indices when `settings = list(screen = list(effort = TRUE))` is given (#73, first of three parts).
  - **Once, across the instrument.** The indices describe a respondent across the whole instrument, and several cannot be computed within one scale. Even-odd consistency correlates across at least three scales, and psychometric pairs can span scales. So they are computed once, over every item in the run with the run's scales, and the per-scale item audits are unchanged. The result matches a standalone `nomo_screen(effort = TRUE)` call exactly.
  - **Keying.** `reverse`, `scale_range`, `pair_magnitude`, and `scales` can be set alongside `effort`. When the scales came from a `contentvalidR` handoff that declares keying, that keying is used unless the settings give their own, and the design log records which applied.
  - **Where it appears.** The indices' log rows join the run's component log, and the methods are credited to the run. The report gains a "Careless responding" section after the item audits: a summary, one row per index with the rule a source states for it or "none stated", and the evidence rows.
  - **Refusals.** An unreadable `effort` value is refused before any stage runs.

- `nomo_screen()` and `nomo_run()` accept a handoff from `contentvalidR`'s `content_handoff()`, so items move from content review to empirical screening with their reasons intact (#46). The interface is schema version 1, agreed with the `contentvalidR` maintainer and documented identically in both packages.
  - **What is analyzed.** `nomo_screen(data, items = handoff)` screens only the items content review carried. Every held-back item is listed in the decision log with its status and recommendation quoted in `contentvalidR`'s own words, and is never analyzed or reinstated.
  - **Guided runs.** `nomo_run(data, scales = handoff)` takes its scales from the review, and its design log records that item membership came from content review rather than from these data. A review with no construct mapping, such as an expert relevance panel, is refused by `nomo_run()` with the carried items listed, because a guided run needs scales and nomologR does not invent them. It can still be screened.
  - **Keying.** Declared keying and the response scale fill `reverse` and `scale_range` for the careless-responding indices, exactly as agreed. An undeclared item is never treated as forward keyed, a response scale is never inferred from the data, and arguments given in the call take precedence, with a note in the log.
  - **Refusals.** A carried item missing from the data is refused, never dropped. A handoff with a schema version this release does not read is refused, naming both package versions. Fields this release does not know are ignored, so `contentvalidR` can add fields within a schema version. An object `content_handoff()` could not have produced, such as keying missing for some items but not others, is refused as malformed.
  - **Report.** `nomo_report()` gains a content-review section for runs that came from a handoff, so the archive starts where the validity argument starts. Reports of other runs are unchanged.
  - **Testing.** `contentvalidR` is not a dependency. The reader is tested against genuine output from `contentvalidR` 0.6.0 and 0.7.0, stored in `tests/testthat/fixtures` with their generator and checksums. The guided-workflow article gains a runnable section built on the example handoff in `inst/extdata`.
- The research-basis article describes the careless-responding indices that shipped in 0.2.1. It had still listed them as planned.

- Test coverage is back to full executable-line coverage after v0.2.1 (#72). Reviewing each uncovered line found four defects, now fixed:
  - **Network fit in `nomo_missing()`.** For a `nomo_network`, the fit comparison listed every fit index as missing. A network's fit evidence is one wide row, while a CFA's is a long table, and only the long form was read. Every strategy's chi-square, CFI, TLI, RMSEA, and SRMR now appear.
  - **`nomo_scores()` with ordered indicators.** The parallel-model test that unit weighting implies was written for continuous indicators. With ordered indicators it was fitted by maximum likelihood against a categorical model, failed, and reported only that the comparison "could not be computed". It is now not run for ordered indicators, and the note says why.
  - **RMSEA interval in the network fit table.** The APA fit table for a `nomo_network` had an "RMSEA [90% CI]" column but printed no interval. The interval is now read from the fit, using the robust, scaled, or plain RMSEA that the evidence reports.
  - **Long-string with no usable scale.** The mean within-scale long-string returned `NaN` when no scale had two items. It now returns `NA`, as the inter-item SD already did.
- Lines that no input can reach were removed rather than tested. Examples are the empty-row checks in the careless-responding indices and the guards for fitted models that are always present. The remaining guards are tested against real fitted objects:
  - models that did not converge, have several groups, or were fitted from a covariance matrix;
  - single-indicator and cross-loaded factors;
  - small and degenerate response sets.

  Failures that only lavaan can produce are simulated.

# nomologR 0.2.1

nomologR 0.2.1 completes the six workstreams that moved out of v0.2.0 at
certification (#37). It also carries three workstreams opened during the cycle
(#54, #56, #62). Together they cover what happens after a measurement model is
established:

- scoring it, with the evidence for the scoring choice (#33);
- screening for careless responding (#34);
- checking whether results depend on missing-data handling (#32);
- judging factor-score quality (#56);
- carrying scores into a network honestly (#62);
- reporting in manuscript-ready form (#35), including from inside a thesis
  document (#40).

Where a published worked example exists, the package reproduces it exactly:
Curran's (2016) examples for the careless-responding indices, and Rodriguez,
Reise, and Haviland's (2016) MASC values for factor determinacy and construct
replicability. Skrondal and Laake's (2001) factor-score regression design and
the missing-data comparison are checked against known population values.
Every source cited is in the research-basis reference list, with its DOI
checked against the registry. Certification is recorded in #70. Distribution
remains R-universe; the first CRAN submission is targeted for v0.3.0 (#39).

- `nomo_report()` now works from inside a knitted R Markdown or Quarto
  document, such as a thesis chapter, with no workaround in the calling
  document (#40). A nested render shares knitr's state with the document that
  started it, which previously stopped the report with a duplicate chunk label,
  and which silently dropped every figure from the report when the calling
  document set a graphics device its template did not expect. The report is now
  rendered with knitr's default chunk options, which its own template then sets
  for itself, and the calling document's chunk options and duplicate-label
  setting are restored afterwards. A report therefore renders the same from the
  console, a script, or a chunk, and rendering one does not change the document
  that asked for it.

- Restored full executable-line coverage by testing every `nomo_compare()`
  failure guard (#54). These branches handle a failure inside lavaan or
  semTools rather than a researcher-facing input, and several of them produce
  the explanation a researcher reads when a check cannot be run, such as why a
  nesting check was skipped or why a difference test is unavailable. Wording
  that has never executed can be wrong without anyone noticing, so those
  sentences are now asserted against real fitted objects.

- `nomo_hierarchical()` now reports factor determinacy and construct
  replicability for the general factor and each group factor, in a new
  `factors` table reachable with `nomo_table(x, "factors")` (#56). Both
  reproduce the published values for the MASC example in Rodriguez, Reise, and
  Haviland (2016) exactly. The two indices are the same quantity only when a
  construct is unidimensional: under a bifactor model, determinacy uses the
  whole reproduced correlation matrix while H sees one factor's loadings alone,
  and Rodriguez et al. decline to prefer either, so both are reported and each
  is labeled with the question it answers. Gorsuch's (1983) and Hancock and
  Mueller's (2001) thresholds appear as their authors' recommendations where a
  value falls below them, never as rules.

- Added `nomo_scores()`, which computes sum, mean, regression, or Bartlett
  scores from a fitted measurement model and reports what those scores are and
  are not (#33). Unit weighting is treated as the model it is: McNeish and Wolf
  (2020) show that adding items assumes a parallel model, with equal
  unstandardized loadings and equal residual variances, so for `"sum"` and
  `"mean"` that constrained model is fitted and compared with the model
  supplied. Every method reports Grice's (2001) three criteria, computed from
  the fitted model: validity, univocality, and correlational accuracy. Validity
  for regression scores is the factor determinacy coefficient reported by
  `nomo_hierarchical()`, since that method maximizes it. Correlational accuracy
  is the one to read before using scores in a later analysis: correlations among
  scores do not reproduce correlations among the factors, the discrepancy is
  substantial, and its direction depends on the method and the model rather than
  being a constant that could be corrected for, so it is reported and left
  visible. Gorsuch's (1983) thresholds appear as his recommendations where a
  value falls below them, never as rules. The supplied data is never modified,
  and only the cases the model used are scored. Includes the **Scoring a
  measurement model** article.

- `nomo_screen()` gains `effort = TRUE`, which adds case-level indices of
  careless or insufficient-effort responding (#34): long-string, inter-item
  standard deviation, Mahalanobis distance, even-odd consistency, and
  psychometric antonym and synonym correlations, following Meade and Craig
  (2012), Huang et al. (2012), Curran (2016), and Marjanovic et al. (2015).
  Curran's published worked examples are reproduced exactly and used as tests.
  The indices are reported side by side rather than combined, because they
  detect different failures: inter-item standard deviation detects random
  responding and gives a respondent who answers every item identically the best
  possible score, which is exactly the case long-string flags. The decision log
  reports that disagreement when it occurs. A case is flagged only where a
  source states a rule, with the source's own qualification attached; the rest
  are reported without a flag. Inter-item standard deviation is averaged within
  scales, as Marjanovic et al. computed it, since one value across every item
  rates a respondent who is high on one construct and low on another as more
  erratic than a random responder. Within-person correlations over fewer than
  three pairs or scales are never reported, because with two every value is
  exactly +1 or -1. Reverse keying and the response range are declared, never
  inferred, and recoding happens on an internal copy only. Cases are flagged,
  never removed, and a screen that does not request these indices is unchanged.
  `nomo_table()` now supports `nomo_screen` objects.

- `nomo_network()` now discloses relationships estimated between observed
  variables (#62). When an endpoint is observed rather than latent, its
  measurement error enters unmodeled, and if it is a composite of several items
  (a sum, mean, or factor score) the relationship carries the discrepancy
  `nomo_scores()` reports as correlational accuracy. The network cannot tell a
  composite from a single measured variable, so it does not guess: it classifies
  each hypothesis by its endpoints and discloses observed ones in the decision
  log, for review when both ends are observed and for information when one is,
  saying plainly that a single measured variable is not a composite. The
  disclosure points to modeling the items as indicators, to `lavaan::sam()`
  (Rosseel & Loh, 2024), and, for a linear regression among factor scores, to
  the design Skrondal and Laake (2001) proved consistent: regression-method
  scores for the predictors and Bartlett scores for the outcome, each block
  scored from a measurement model of its own. The second condition is easy to
  miss. In reproducing their result, scoring both factors from one joint model
  biased the estimate by about +.10 in the same check that recovered the latent
  value with separate models. No correction is applied automatically.

- `nomo_scores()` and the scoring article now describe that design (#62). The
  note on correlational accuracy says that scores from one model containing
  every factor are not it, and the article applies it to its own data next to
  the two ways of getting it wrong: the same method for both blocks falls well
  short of the latent slope, and scoring both factors jointly overshoots it.

- `nomo_reliability()` gains `ci_ncpus`, which runs the bootstrap on several
  worker processes through lavaan's `snow` backend (#42). The default, `1`, keeps
  the bootstrap serial and unchanged. `snow` is used everywhere rather than
  forking, which is unavailable on Windows, so results are comparable across
  platforms. The draws depend on the worker count as well as the seed, so a
  result is reproducible for a given seed and worker count: both are recorded in
  the bootstrap status table and, newly, in the decision log that reaches
  reports, which previously carried neither. An unseeded bootstrap is flagged
  for review as not reproducible. Verified against an installed package: the
  same seed and worker count give identical intervals, and serial and two-worker
  runs differ by under .01, as the draws should. More workers are not always
  faster, since each must start and load its packages; in the evaluation
  recorded on the issue, eight workers were slower than four. Tests use at most
  two workers, following CRAN policy.

- Added `nomo_apa_table()`, which formats the evidence in a result as an APA 7
  table ready for a thesis, dissertation, or manuscript (#35): standardized
  loadings, factor correlations, and model fit from `nomo_cfa()`; reliability
  from `nomo_reliability()`; the invariance sequence from `nomo_invariance()`;
  and hypothesis evidence and model fit from `nomo_network()`. Tables carry a
  bold number, an italic title, no vertical rules, and notes below in APA's
  order, and they knit directly into R Markdown or Quarto. Leading zeros follow
  the statistic rather than its value, as APA 7 specifies: statistics that
  cannot exceed 1 (reliability, correlations, CFI, *p*) lose the zero, and those
  that can (TLI, RMSEA, SRMR, standardized loadings) keep it. A value that rounds
  to zero is never printed with a sign, and an estimate the model could not
  produce is shown as an em dash. The hypotheses table reports each estimate's
  evidence against its prediction, and marks a hypothesis specified after the
  data were seen as exploratory. No cell reads pass or fail. Word output for
  `nomo_report()` follows separately.

- `nomo_report()` now writes Word documents: a `file` ending in `.docx`
  produces one, alongside the existing HTML report (#35). Pandoc drops raw HTML
  when it writes Word, and the report template wrote its tables, notes, and its
  interpretation contract as raw HTML, so a Word report would have lost all of
  them, including the statement that the report does not turn review references
  into pass/fail rules. Each part of the template now writes HTML for an HTML
  report and markdown for a Word one, and the HTML report is unchanged. The Word
  report was checked against the HTML one: the same 37 data tables with the same
  header rows in the same order, the interpretation contract, the figures, and
  no raw HTML or escaped entities in its text. Collapsible sections are shown
  expanded, and the document uses Word's default styles. Section numbering is
  used where the installed rmarkdown supports it for Word.

- Added `nomo_missing()`, which refits a `nomo_cfa` or `nomo_network` under
  alternative missing-data strategies and reports whether the cases used, the
  estimates, fit, reliability, or theory evidence change (#32).
  - **Strategies compared.** Continuous indicators are compared under
    listwise deletion and FIML, with FIML as the reference. Ordered
    indicators, for which lavaan offers no FIML, are compared under listwise
    and pairwise deletion.
  - **Assumptions and the flag.** Each strategy is labeled with the
    mechanism it requires, following Enders and Bandalos (2001). A difference
    larger than half the reference standard error is flagged for review,
    following Schafer and Graham's (2002) rule for when bias becomes
    practically important. The flag says that the difference estimates bias
    only if the data are missing at random and the model is correct, and how
    many cases listwise deletion discarded. The count matters because, in
    checks under MCAR, differences from sampling alone grew with the share of
    cases discarded.
  - **Hypotheses.** A hypothesis whose concordance changes between strategies
    is flagged for review.
  - **What agreement means.** Whether data are missing at random cannot in
    general be tested from the data at hand, so agreement is reported as
    insensitivity to the choice, not as evidence that either strategy is
    unbiased.
  - **What lavaan actually did.** Each strategy records what lavaan
    estimated, not only what was requested. When FIML is requested with ULS,
    lavaan 0.7 runs its two-stage method instead (as it does with GLS), while
    lavaan 0.6-21 refuses. With MLM, FIML is refused. Each of these is
    reported, not hidden, and the tests accept either lavaan behavior. The data supplied must reproduce the
    fitted model when refitted with its original strategy, so a comparison
    cannot silently run on different data.
  - **Validation.** Tests reproduce lavaan's estimates for each strategy and
    use simulations with a known factor correlation of .50. Under MAR, FIML is
    within sampling error of it while listwise deletion is attenuated and
    flagged. Under MCAR, both are unbiased, and FIML's standard errors are
    smaller where it keeps cases listwise deletion discards.
  - **Not offered.** Mean substitution is explained and not implemented.
  - **Documentation.** The measurement-model article gains a section applying
    the comparison to `nomo_demo_continuous`, whose missing values are MCAR by
    construction, and to a simulated MAR sample.

# nomologR 0.2.0

nomologR 0.2.0 makes the workflow research-backed from historical to
contemporary practice and more useful to graduate students and researchers.
Every method is placed in its literature and cited in reports; researchers can
compare competing models, revise a model with its lineage recorded, and
evaluate total and subscale scores; and replication language no longer reads
noise as a finding. Scope was selected in #22.

Six Planned workstreams moved to v0.2.1 at release certification (#37), each
with its reason recorded: missing-data sensitivity (#32), score guidance (#33),
insufficient-effort responding screens (#34), manuscript-ready tables (#35),
rendering reports inside R Markdown or Quarto (#40), and optional parallel
bootstrap intervals (#42). The license is GPL-3.0-only; the published 0.1.0
release keeps its original MIT license. Distribution remains R-universe; the
first CRAN submission is targeted for v0.3.0 (#39).

## Release certification (#37)

- README links to files excluded from the built package (`ROADMAP.md`,
  `LICENSE.md`) now use absolute URLs, so they resolve for readers of the
  package tarball as well as on GitHub (#48).
- The default-run part of the `nomo_compare()` example now skips the
  side-by-side evidence, which moved to `\donttest{}`, keeping every
  default-run example within CRAN's time expectations.
- Added the R-hub v2 GitHub Actions workflow (run on demand) for multi-platform
  CRAN-style checks.
- Certification tests of every `nomo_methods()` crediting rule against real
  objects found, and fixed, methods that reports would have mis-cited: WLSMV
  comparisons were credited with the Satorra-Bentler scaled test instead of the
  scaled-and-shifted test; AIC and BIC were never credited; FIML requested as
  `missing = "ml"` or `"direct"` was not credited; a requested Fornell-Larcker
  comparison was not credited; and a `nomo_partial()` release specification was
  credited with a multiple-group fit it does not perform. A registry entry for a
  fixed change-in-CFI rule that no function displays was removed.
- `nomo_revise()` now refuses an item revision whose measurement model
  disagrees with the revised items. Previously, dropping an item without
  supplying a revised `cfa_model` screened and explored the reduced item set
  but still fitted the dropped item in the CFA, while the lineage recorded it as
  removed.

## Bifactor and higher-order models (#29)

- `nomo_model()` gains `structure = c("correlated", "higher_order",
  "bifactor")` and `general`. Bifactor syntax writes its identification and
  orthogonality explicitly, so it is identified the same way whatever `std.lv`
  is used. Higher-order models need at least three first-order factors; with
  exactly three the model is noted as fitting exactly as well as correlated
  factors. Impossible configurations are refused and fragile ones noted.
- Added `nomo_hierarchical()`, which reads a fitted bifactor or higher-order
  model and reports omega total, omega hierarchical, their ratio, explained
  common variance (ECV), the percentage of uncontaminated correlations (PUC),
  and, per subscale, omega and omega hierarchical subscale, with item-level
  general and group loadings. For higher-order models these are the exact
  Schmid-Leiman decomposition.
- Indices match analytic population values and `semTools::compRelSEM()` under
  both observed and model-implied denominators. Ordered indicators report the
  latent-response estimand, labeled as an upper bound on observed ordinal
  sum-score reliability.
- No index is treated as a pass/fail threshold, following Reise (2012), who
  notes that no benchmark value of ECV establishes unidimensionality. The output
  discloses that a bifactor model usually fits at least as well as the
  alternatives even when it did not generate the data.
- Models whose general and group factors correlate, multi-group and multilevel
  models, and non-hierarchical models are refused with an explanation. The
  higher-order refusal shared by `nomo_reliability()` and `nomo_validity()`,
  and `nomo_reliability()`'s cross-loading refusal, now point to
  `nomo_hierarchical()`.
- Added `print()`, `summary()`, `plot()`, and `nomo_table()` methods, seven
  methods-registry entries, and the **Total and subscale scores** article.

## Replication-status language (#30)

- A change in sign between primary and validation estimates is no longer
  labeled `"sign_reversal"` from the point estimates alone. Estimates scattered
  around a null relation differ in sign about half the time, and the previous
  label called that "a substantively important replication discrepancy."
- For directional predictions, a sign change is now classified from the two
  95 percent confidence intervals: `"sign_reversal"` when both exclude zero on
  opposite sides, `"direction_not_replicated"` when exactly one excludes zero,
  and `"sign_change_within_uncertainty"` when neither does or an interval is
  unavailable. The first two are recorded as concerns and the third for review.
- The rule and its rationale are documented in `?nomo_network`, the
  nomological-network vignette, and the research-basis article. It does not
  treat a non-significant path as evidence of no relation; that still requires
  a `negligible(within = ...)` prediction.

## Methods registry (#26)

- Added `nomo_methods()`, a machine-readable registry of every method the
  package implements. Each entry records its workflow stage; its lineage
  (`historical`, `contemporary`, or `emerging`); its role (`primary`,
  `supporting`, or `context`); its estimand and key assumptions; the function
  and computational engine that implement it; and its references.
- `role = "context"` marks methods displayed only so readers can recognize them
  in published work, such as the eigenvalue-greater-than-one rule and fixed
  fit-index cutoffs. They take no part in any synthesis or decision.
- The registry lists only what the package computes. Planned methods stay in
  the research-basis article and the issue tracker.
- `nomo_methods(x)` returns only the methods a result object actually used,
  read from what each component recorded: a skipped retention criterion, alpha
  that was not computed, or a comparison that was not run is not credited. A
  one-factor solution is credited with no rotation method, since none was
  applied.
- `references = TRUE` expands the result into a reference list with full
  citations and DOIs.
- The "Methods and citations" section of `nomo_report()` now lists the methods
  the run used and a deduplicated reference list for them, ahead of the
  software citations it already contained. `summary()` of a `nomo_run` and
  `nomo_table(run, "methods")` show the same methods.
- Every reference is in a single bibliography that the tests keep consistent
  with the registry in both directions. `dev/verify-method-dois.R` resolves
  each DOI and checks that the registered first author, year, and title match
  the citation; all 79 DOIs pass.

## Revision lineage (#28)

- Added `nomo_revise()`, which creates a child workflow from a parent
  `nomo_run()` with a revised measurement model, a revised item set, or both.
  The parent workflow is never modified.
- Each revision records what changed (model, items, or both), the required
  researcher rationale, and whether the change was prespecified or post hoc.
  Post-hoc revisions are labeled, and the decision log recommends confirming
  the revised model in independent data.
- Parent and revised measurement models are compared with `nomo_compare()`
  (#27), so a revision carries its own evidence. Item changes alter the
  observed variables, so those comparisons are descriptive, as documented.
- The factor-count decision is inherited from the parent unless the researcher
  supplies a new one, so a revision changes only what was intended.
- Revisions chain: `$lineage` accumulates one row per revision and is available
  through `nomo_table(run, "lineage")`, in `print()` output, and in a new
  "Revision lineage" section of `nomo_report()`.
- The `measurement_model = "revise"` decision now points to `nomo_revise()`
  instead of only advising a fresh workflow.

## Model comparison (#27)

- Added `nomo_compare()` for comparing two or more fitted `nomo_cfa()` models.
  A researcher rationale is required and recorded, the comparison can be
  labeled a priori or post hoc, and no model is ever selected automatically.
- Nested models receive the difference test that matches the estimator,
  computed by `lavaan::lavTestLRT()`: the ordinary chi-square difference test
  for ML, the scaled difference test for robust ML (Satorra & Bentler, 2001),
  and the scaled-and-shifted test for WLSMV (Satorra, 2000). A test lavaan
  cannot compute, or a negative scaled statistic, is reported as unavailable
  with lavaan's message rather than replaced by another method.
- Nesting is checked from the models' implied moments with `semTools::net()`
  (Bentler & Satorra, 2010). Researchers can declare nesting when the check
  cannot run, and a declaration the check contradicts is recorded as a concern.
- Each comparison reports changes in CFI, TLI, RMSEA, and SRMR without cutoffs,
  AIC and BIC when they are defined, and a plain-language interpretation.
- Side-by-side standardized loadings, reliability, AVE, and HTMT2 are reported
  for each model. Reliability for a model with loadings fixed to zero is
  labeled as describing a composite that still includes those items.
- Comparisons are refused, with an explanation, when models use different
  estimators, missing-data handling, cases, or data. Models with different
  observed variables receive descriptive evidence only.
- Added `print()`, `summary()`, `plot()`, and `nomo_table()` methods.

## Maintenance (#36)

- Split the guided-workflow implementation (`R/nomo_run.R`, 2,066 lines) into
  `nomo_run_state.R` (state and input validation), `nomo_run_decisions.R`
  (researcher decisions), `nomo_run_stages.R` (stage execution and blocking),
  `nomo_run_provenance.R` (provenance, recipe, and settings tables), and
  `nomo_run.R` (entry point). Every function body and signature is unchanged,
  verified by comparing the parsed functions with the previous source, and the
  generated documentation is identical.
- Redistributed the 102 tests in `tests/testthat/test-regressions.R`, which were
  consolidated during pre-v0.1 hardening, into the per-module test files. Test
  names, the number of tests (464), and expectation calls are unchanged.
- Raised the declared minimum for `EFAtools` from 0.8.0 to 1.0.0. With
  `EFAtools` 0.8.0, `nomo_factors()` could not run the empirical Kaiser
  criterion, NEST, Hull, or comparison-data criteria, and 22 factor-retention
  tests failed. The full test suite passes with the declared minimums
  `lavaan` 0.6-21, `semTools` 0.5-9, and `EFAtools` 1.0.0.
- Added a CI job that installs the minimum versions declared in `DESCRIPTION`
  Imports, confirms they are the versions installed, and runs `R CMD check`,
  so a declared minimum is never an untested promise.
- Evaluated parallel execution for bootstrap reliability intervals. Four `snow`
  workers were about 3.5 times faster than serial refitting, and results are
  reproducible for a given seed and worker count but differ across worker
  counts. Because an opt-in option adds a user-facing argument and provenance
  fields, it is tracked separately (#42); bootstrap behavior is unchanged here.
- Recorded the distribution plan: R-universe through `v0.2.x`, a
  CRAN-readiness gate in `v0.2.0` certification (#37), and the first CRAN
  submission targeted for `v0.3.0` (#39).

## Learning foundations toward v0.2.0

- Set the `v0.2.0` direction: research-backed, usable scale-development and
  construct-validation workflows for graduate students and researchers,
  spanning historical to contemporary methods. Scope, priorities, and exit
  criteria were recorded in #22 and the linked workstream issues (#25–#37);
  v0.3 candidates are tracked in #38. Distribution remains R-universe; CRAN
  submission is deferred (#39).
- Added three simulated teaching datasets with documented population models
  and fixed-seed generation code in `data-raw/nomo_demo.R`:
  `nomo_demo_continuous` (two correlated factors with a cross-loading item, a
  weak item, and MCAR missingness), `nomo_demo_ordinal` (five-category ordered
  version), and `nomo_demo_network` (three constructs, an observed outcome, and
  two administration groups with a known source of scalar non-invariance)
  (#25).
- All workflow vignettes now evaluate against the teaching datasets, so the
  guided workflow, measurement-invariance, nomological-network, and
  reproducible-report articles show real output (#25).
- Replaced the stale overview article with **Get started**, including a
  learning path for graduate students and researchers, and added a
  **Research basis** article mapping each workflow stage from historical to
  contemporary practice (#25, #26).
- Added DOI-verified references to every exported analysis function (#26).
- Replaced `\dontrun{}` examples with runnable examples and added examples to
  `nomo_network()`, `nomo_invariance()`, `nomo_table()`, and `nomo_defaults()`
  (#25).
- `nomo_table(<nomo_invariance>, "local_strain")` now adds a
  `constraint_display` column with human-readable parameter labels (for
  example `Intercept: ag3 (online vs. paper)`); the raw lavaan `constraint`
  label is retained (#31). No statistical behavior changed.
- Fixed invariance figures and constraint labels that used non-ASCII symbols
  (Greek capital delta and arrows). On the default `pdf()` device these
  symbols were dropped or mangled, and running the vignette code under
  `R CMD check` failed. Labels now read `Delta CFI`, `Delta RMSEA`,
  `Delta SRMR`, `->`, and `<->`, matching the hypothesis syntax.
- Documented the table types available from `nomo_table()`.
- Grouped the pkgdown reference index by workflow stage and articles by
  learning path, with a redirect from the former overview URL (#25).
- Archived the completed v0.1 roadmap specification in
  `dev/roadmap-v0.1-record.md` and refocused `ROADMAP.md` on v0.2 and later.

## Post-v0.1 development

- Changed the current development source to GNU GPL version 3 only
  (`GPL-3.0-only`; `GPL-3` in R metadata), retaining the historical MIT notice.
  Previously published releases retain their original licenses.
- Aligned source citation metadata with development version `0.1.0.9000`;
  release tags retain their release-specific citation metadata.
- Reconciled README, roadmap, issue navigation, historical checkpoint wording,
  and the documentation/planning workflow. Candidate methods remain proposals
  until their scope is accepted; no analytical behavior changes in this update.
- Opened development toward `v0.2.0`.
- `v0.1.0` remains the current stable public release.

# nomologR 0.1.0

## First stable public release

- First stable public release of the guided empirical scale-development and construct-validation workflow.
- Includes the complete v0.1 workflow from item screening through reproducible reporting.
- Release certification includes a clean test suite, R CMD check, coverage audit, installed-package smoke test, and public pkgdown documentation.


## Milestone 9 closeout — reproducible reporting and v0.1 release hardening

- Completed `nomo_report()` as the archival reporting layer for guided
  `nomo_run()` workflows.
- Added self-contained HTML reporting across item screening, factor retention,
  EFA, CFA, reliability, convergent/discriminant evidence, invariance, and
  theory-specified nomological networks.
- Added researcher inputs, data characteristics, decision provenance,
  deviations/post-hoc decisions, methods/citations, reproducibility information,
  and a full evidence trace linking recommendations back to their source evidence.
- Added report-render regression tests covering the core workflow and optional
  invariance/network branches.
- Completed pre-v0.1 engineering hardening with the full test suite passing,
  R CMD check at 0 errors / 0 warnings / 0 notes, executable-line coverage at
  100%, and an empty `covr::zero_coverage()` result.
- Finalized `v0.1.0` as the first stable public release after release-readiness review and certification.

## Milestone 8 closeout — guided workflow and decision provenance

- Replaced the `nomo_run()` development stub with a resumable guided workflow
  spanning screening, factor-retention evidence, EFA, CFA, reliability,
  convergent/discriminant evidence, optional invariance, and optional
  theory-specified nomological-network analysis.
- Added explicit researcher handoffs for EFA factor count, CFA model
  specification, and downstream continuation after measurement evidence.
- Preserved completed component results across resume calls; future-stage
  settings can be added without silently recomputing completed work.
- Added teaching and research presentation modes that change presentation,
  not statistical behavior.
- Added `nomo_split`-aware sample roles and exact-model network replication.
- Added stage maps, decision/rationale provenance, component evidence logs,
  reproducibility recipes, and the guided-workflow vignette.
- Optional invariance/network branches remain researcher configured; no
  automatic parameter freeing, respecification, item deletion, or one-number
  validity score is introduced.
- M8 closeout coverage reached **94.07% package-wide** and **93.16%** for
  `R/nomo_run.R`; the full test suite and R CMD check were clean.
- Milestone 8 is complete. Milestone 9 was subsequently completed in this development version; see the closeout above.



## Milestones 6–7 closeout — theory-specified networks, invariance, and Checkpoint C

- Added `positive()`, `negative()`, and `negligible()` expectation helpers plus
  `nomo_hypotheses()` for machine-readable a-priori and post-hoc theory
  specifications. Quantified negligible predictions use researcher-specified
  SESOI/equivalence regions; bare negligible predictions remain qualitative and
  are never confirmed merely because `p > .05`.
- Added `nomo_network()` for theory-specified latent SEM with transparent
  addition of missing prespecified theory paths, observed external
  criteria/outcomes, standardized or explicitly unstandardized hypothesis
  evaluation, measurement-context propagation, and calibration/validation
  replication of the exact same prespecified fitted model.
- Kept theory concordance, uncertainty, measurement quality, confirmatory
  status, and replication as distinct evidence streams rather than collapsing
  them into a single validity score.
- Added `nomo_invariance()` using current `semTools::measEq.syntax()` and
  `lavaan` infrastructure with identification-aware sequences for continuous,
  ordered 4+ category, three-category, and binary indicators.
- Added diagnostic-only localized equality-constraint score tests and
  `nomo_partial()` for explicitly researcher-specified partial-invariance
  releases with required rationale and cumulative provenance. The package never
  searches until it finds a partial-invariance model that passes.
- Added `nomo_table()` methods plus researcher-facing network and invariance
  figures, including theory-compatible regions, replication comparison,
  fit/change evidence, and human-readable localized equality constraints.
- Added the **“Nomological network”** and **“Measurement invariance”**
  vignettes plus truth simulations, edge/failure hardening, and presentation
  regression tests.
- Final Checkpoint C coverage audit reached **94.19% package-wide**. Core M6/M7
  modules: `R/nomo_hypotheses.R` 94.33%, `R/nomo_network.R` 89.95%,
  `R/nomo_network_presentation.R` 91.49%, `R/nomo_invariance.R` 92.04%,
  `R/nomo_invariance_presentation.R` 93.27%, `R/nomo_partial.R` 90.57%, and
  `R/nomo_table.R` 100.00%.
- Advanced the development version to `0.1.0.9003`, marking **Checkpoint C:
  Generalizability and Nomological Evidence Complete**. Milestone 8,
  `nomo_run()`, is the next active development target.


## Milestone 5 closeout — reliability, convergent/discriminant evidence, and Checkpoint B

- Replaced the `nomo_reliability()` development stub with a model-based
  reliability workflow using current `semTools::compRelSEM()` infrastructure.
- Made omega/composite reliability the primary coefficient for the general
  congeneric CFA workflow while retaining coefficient alpha as an explicitly
  qualified secondary statistic.
- Added ordered-indicator reliability guidance that distinguishes observed
  ordinal-score reliability from latent-response-scale coefficients and refuses
  to silently substitute a different alpha estimand.
- Added optional nonparametric bootstrap confidence intervals for reliability
  estimates through repeated refitting of the same lavaan CFA. Point estimates
  remain unchanged; uncertainty is additive and explicit.
- Replaced the `nomo_validity()` development stub with convergent and
  construct-separation evidence including standardized loadings, AVE, latent
  correlations, HTMT2, original HTMT, and optional legacy/supporting
  Fornell-Larcker output.
- Kept AVE in the validity layer rather than mislabeling it as reliability.
- Prioritized HTMT2 for congeneric measurement models while retaining original
  HTMT as comparison evidence.
- Added explicit guards for ambiguous or unsupported simple-structure
  reliability/validity estimands, including mixed ordered/continuous composites,
  cross-loaded first-order structures, and silent multi-group/multilevel HTMT
  pooling.
- Added compact `print()`/`summary()` methods and nonredundant reliability, AVE,
  and construct-separation plots. Coefficient plots use the conceptual 0-to-1
  scale by default and expand only when empirical estimates or intervals require
  it.
- User-facing audits confirmed two key teaching cases: excellent global CFA fit
  can coexist with weak reliability/AVE, and strong reliability/AVE can coexist
  with poor construct separation.
- Added the Checkpoint B vignette, **“From CFA to a defensible measurement
  model,”** integrating CFA, reliability, convergent evidence, discriminant
  evidence, uncertainty, and decision guidance.
- Added direct regression tests against `semTools::compRelSEM()`,
  `semTools::AVE()`, and `semTools::htmt()`, plus ordinal, redundant-construct,
  weak-measurement, bootstrap-uncertainty, failure-disclosure, and presentation
  hardening tests.
- Final M5 coverage audit reached **95.13% package-wide**. Key M5 modules:
  `R/nomo_reliability.R` 90.26%, `R/nomo_validity.R` 92.56%,
  `R/nomo_measurement_helpers.R` 92.70%,
  `R/nomo_reliability_uncertainty.R` 89.33%,
  `R/nomo_reliability_presentation.R` 88.93%, and
  `R/nomo_validity_presentation.R` 81.20%.
  Coverage hardening targeted consequential branches rather than artificial
  100% execution of low-value presentation/failure paths.
- Advanced the development version to `0.1.0.9002`, marking **Checkpoint B:
  Measurement Model Complete**. Milestone 6, theory-specified nomological
  networks, is next.

## Milestone 4 closeout — confirmatory factor analysis

- Replaced the `nomo_cfa()` development stub with a production CFA workflow
  around `lavaan::cfa()` while retaining the underlying lavaan fit unchanged.
- Added `nomo_model()` for simple reflective CFA syntax generation without
  automatically adding cross-loadings, residual covariances, or other post-hoc
  parameters.
- Added explicit estimator provenance and guidance for continuous ML,
  researcher-selected robust ML estimators such as MLR, and declared
  ordered-indicator WLSMV workflows.
- Added early guardrails for estimator/missing-data combinations incompatible
  with declared ordered indicators.
- Added convergence status, captured engine warnings, case-retention reporting,
  standardized loadings with uncertainty, latent-factor correlations, and
  global-fit evidence including chi-square, CFI, TLI, RMSEA with confidence
  interval, and SRMR.
- Added localized residual-correlation diagnostics and Heywood/improper-solution
  diagnostics.
- Added modification indices as quarantined post-hoc diagnostics; they never
  free parameters or trigger automatic model respecification.
- Added `print()`, `summary()`, and four CFA plot views for standardized
  loadings, global fit, localized residuals, and modification indices.
- Added `nomo_split()` for reproducible calibration/validation splitting,
  including the independence-versus-precision tradeoff and restoration of the
  caller RNG state.
- Added direct regression tests against `lavaan::cfa()` plus stress tests for
  correctly specified and misspecified CFA, omitted cross-loading, omitted
  correlated residual, ordinal CFA, robust ML, FIML, nonconvergence, improper
  solutions, modification-index quarantine, and split-sample reproducibility.
- Final M4 coverage audit reached 96.69% package-wide:
  `R/nomo_cfa.R` 97.37%, `R/nomo_cfa_presentation.R` 99.58%,
  `R/nomo_model.R` 97.73%, and `R/nomo_split.R` 98.31%.
- User-facing audit confirmed that well-specified, deliberately poor, and
  independent holdout CFA results are clearly differentiated without pass/fail
  validity language or hidden respecification.
- Local tests/checks and GitHub Actions passed; the Milestone 4 PR was squash
  merged before Milestone 5 development began.

## Milestone 3 closeout — exploratory factor analysis and Checkpoint A

- Replaced the `nomo_efa()` development stub with a production common-factor EFA
  workflow using `psych::fa()` as the statistical engine and `nomologR` as the
  guidance, diagnostics, logging, and presentation layer.
- Added researcher-controlled factor counts and direct handoff from
  `nomo_factors()`, including inherited item sets, modeling types, correlation
  models, missing-data decisions, and explicit smoothing choices.
- Preserved M2 retention ambiguity in the EFA decision log so a handoff is not
  misrepresented as proof of dimensionality.
- Added MINRES + oblimin defaults, while keeping extraction and rotation choices
  explicit and validating supported extraction methods before calling the engine.
- Added tidy pattern/structure matrices, communalities, uniquenesses, loading
  complexity, factor correlations, reproduced correlations, residual matrices,
  localized residual pairs, and off-diagonal RMSR.
- Added `KEEP`, `REVIEW`, and `STRONG REVIEW` item guidance based on configurable
  loading, cross-loading, and communality teaching references without automatic
  deletion, reverse scoring, factor-count changes, rotation hunting, or hidden
  model refitting.
- Added KMO/Bartlett supporting evidence, descriptive sample-adequacy context,
  small-sample review behavior, and explicit non-positive-definite/smoothing
  handling.
- Added neutral public factor labels (`F1`, `F2`, ...) while retaining the full
  underlying `psych` fit object for advanced inspection.
- Added `summary.nomo_efa()` and four teaching-oriented plot views for pattern
  loadings, primary/secondary loadings, unique residual pairs, and unique
  interfactor correlations.
- Corrected modeling-decision provenance so types inherited from a
  `nomo_factors()` object are distinguished from new researcher overrides made
  at the EFA stage.
- Added known-structure, cross-loading, weak-item, ordinal, redundancy,
  missingness, orthogonal-rotation, smoothing, validation, handoff, provenance,
  presentation, and failure-mode regression tests.
- Added the Checkpoint A vignette, **“From item audit to exploratory structure,”**
  demonstrating `nomo_screen()` -> `nomo_factors()` -> `nomo_efa()`.
- Milestone 3 coverage closeout reached 97.78% for `R/nomo_efa.R`, 98.70% for
  `R/nomo_efa_presentation.R`, and 96.52% package-wide in the final pre-closeout
  audit.
- Advanced the development version to `0.1.0.9001`, marking Checkpoint A:
  the exploratory measurement workflow is complete and Milestone 4 (CFA) is next.

## Milestone 2 closeout — researcher control and hardening

- Changed modeling-type override precedence so an explicit, valid `types`
  declaration can rescue otherwise ambiguous storage before default-type
  rejection, while storage-compatibility checks still block unsafe coercions.
- Added decision-log entries for researcher-specified modeling-type overrides.
- Improved error ordering so constant and all-missing items are diagnosed as
  hard data failures before modeling-type inference.
- Expanded README guidance and examples showing when and how to use `types`,
  including ordered interpretation of factor levels and the limits of overrides.
- Added ordinal one- and two-factor regression simulations plus targeted
  coverage hardening for validation, criterion, presentation, and failure paths.
- Milestone 2 core computational files exceed the v0.1 >=90% coverage gate;
  presentation coverage reached 100% in the closeout audit.

## Milestone 2B — retention triangulation and sensitivity

- Refined concordance to group closely related methods into criterion families,
  so original/revised MAP variants do not behave like independent votes.
  Internally split families remain explicit and are not forced into one bar.
- Added convenient `correlation` and `modeling_types` fields alongside the
  existing internal-detail names for easier inspection of automatic choices.
- Surface EKC's non-Pearson approximation directly in criterion status and
  summary output rather than leaving that qualification only in the decision log.
- Polished evidence/concordance plots for longer criterion sets and wrapped
  captions to remain readable at ordinary RStudio plot-device sizes.
- Expanded `nomo_factors()` from a two-criterion workflow into configurable
  retention bundles: `minimal`, `core`, `extended`, and `all`.
- Added revised Velicer MAP (TR4) alongside original MAP (TR2), with both
  minima and criterion curves retained as sensitivity evidence.
- Added the empirical Kaiser criterion (EKC) to the default `core` bundle.
- Added optional NEST and Hull (CAF) criteria for supported continuous/Pearson
  analyses, with explicit skip reasons when their reference machinery is not
  compatible with ordinal, binary, mixed, or varying-N pairwise analyses.
- Added optional comparison-data retention and a clearly labeled legacy
  Kaiser-Guttman (> 1) rule in the `all` bundle; the legacy rule is excluded
  from the evidence synthesis.
- Parallel analysis now reports percentile, mean, and Crawford stopping-rule
  suggestions from the same null simulations while preserving one explicit
  selected rule for the primary recommendation.
- Added criterion-status and concordance tables so unavailable methods are never
  silently substituted and agreement is summarized without majority-vote logic.
- Expanded `plot.nomo_factors()` with parallel-rule sensitivity, MAP curves,
  multi-method evidence, and retention-concordance views.
- Added `EFAtools (>= 0.8.0)` as the runtime engine for modern optional retention
  criteria while preserving `nomologR`'s interpretation and decision-log layer.

## Milestone 2A — factor-retention evidence

- Replaced the `nomo_factors()` development stub with a production factor-retention engine.
- Added explicit correlation-model selection for Pearson, polychoric, tetrachoric,
  and mixed indicator sets, with conservative handling of numeric-discrete items
  and user overrides for intended measurement level.
- Added reproducible common-factor parallel analysis using independently permuted
  null data and a configurable null-eigenvalue quantile.
- Added Velicer MAP as complementary retention evidence, plus observed scree information.
- Added KMO/item-level MSA and Bartlett diagnostics as supporting evidence rather
  than factor-count decision rules.
- Added explicit non-positive-definite matrix handling: no silent smoothing;
  optional smoothing must be user-requested and is recorded in the decision log.
- Added `summary.nomo_factors()` and `plot.nomo_factors()` views for retention,
  scree, method-concordance, and KMO evidence.
- Promoted `psych` to a runtime dependency because factor-retention methods now
  use its established factor/correlation engines directly.

## Milestone 1C — integrated review and visualization

- Added `summary.nomo_screen()` with an integrated item-review table that
  combines descriptive, relationship, response-category, and decision-log evidence.
- Added `plot.nomo_screen()` with evidence-map, item-rest, inter-item,
  response-profile, and missingness views.
- Added explicit visibility for declared-but-unused ordered response categories
  without silently collapsing or recoding them.
- Added an item-level attention summary (`none`, `review`, `concern`) that
  remains deliberately non-prescriptive about item retention.
- Added `ggplot2` as a core visualization dependency and expanded regression
  tests for presentation behavior.

## Milestone 1B — psychometric screening

- Added corrected item-rest and inter-item relationship diagnostics for
  explicitly scored numeric/logical candidate items.
- Added review guidance for negative and weak item-rest relationships without
  automatic reverse-scoring or deletion.
- Added configurable response-concentration and near-zero-variance screening.
- Added descriptive floor/ceiling concentration for ordered and
  numeric-discrete items where those boundaries are meaningful.
- Added descriptive skewness and excess kurtosis for continuous-like numeric
  indicators without normality pass/fail declarations.
- Expanded `nomo_screen()` printing and tests for the psychometric screening layer.
- Refocused the README on the `contentvalidR` -> `nomologR` measurement workflow
  and the active v0.1 development path.
- Added a GitHub issue form for roadmap-milestone tracking.

## Milestone 1 — item/data screening

- Implemented the first production slice of `nomo_screen()`.
- Added conservative item-type descriptions, item-level missingness and response
  summaries, response distributions, case-level completeness diagnostics, and
  explicit zero-variance/all-missing flags.
- Added an evidence-guided decision log for screening observations without
  automatically deleting items or cases.
- Added a concise `print.nomo_screen()` method.
- Added focused tests for input validation, type classification, missingness,
  response distributions, case completeness, decision logging, and the
  non-destructive workflow.
- Updated GitHub Actions checkout steps to `actions/checkout@v5` for the Node 24
  runtime.

## Foundation milestone

- Defined `nomologR` as a guided workflow for empirical scale development and
  construct-validity evidence rather than a replacement for `psych`, `lavaan`,
  or `semTools`.
- Clarified the package boundary relative to `contentvalidR` and `solomonR`.
- Standardized package naming and the planned public API on the `nomo_*` prefix.
- Reframed common numerical cutoffs as teaching/reference values rather than
  automatic pass/fail rules.
- Established the principle that the package flags, explains, and documents but
  never silently deletes items or respecifies models.
- Updated planned reliability methods to current `semTools` infrastructure such
  as `compRelSEM()`.
- Updated planned measurement-invariance methods to current `semTools`
  infrastructure such as `measEq.syntax()`.
- Separated AVE/convergent evidence from reliability.
- Added a testable internal decision-log scaffold.
- Cleaned development and continuous-integration scaffolding for the public
  GitHub repository.
