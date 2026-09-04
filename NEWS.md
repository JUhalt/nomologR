# nomologR 0.0.0.9000

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
