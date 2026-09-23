# nomologR 0.2.0.9000 (development)

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
  measurement error enters unmodelled, and if it is a composite of several items
  (a sum, mean, or factor score) the relationship carries the discrepancy
  `nomo_scores()` reports as correlational accuracy. The network cannot tell a
  composite from a single measured variable, so it does not guess: it classifies
  each hypothesis by its endpoints and discloses observed ones in the decision
  log, for review when both ends are observed and for information when one is,
  saying plainly that a single measured variable is not a composite. The
  disclosure points to modelling the items as indicators and to `lavaan::sam()`
  (Rosseel & Loh, 2024). No correction is applied. Skrondal and Laake's (2001)
  correction was reproduced in the course of this work, and the reproduction
  showed a condition that is easy to miss: the predictor's regression scores
  must come from its own measurement model, because scoring it jointly with the
  outcome biased the estimate by +.10 in the same check that recovered the
  latent value.

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
