# nomologR 0.1.0.9000

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
