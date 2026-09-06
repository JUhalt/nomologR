# nomologR Roadmap

> **Mission:** Make rigorous construct validation easier to learn, easier to execute, and easier to defend.
>
> `nomologR` is a guided, evidence-based workflow for empirical scale development and construct validation. It coordinates established R engines (primarily `psych`, `lavaan`, and `semTools`) while adding transparent diagnostics, literature-linked explanations, decision logging, and theory-aware guidance.
>
> **Core principle:** Flag, explain, and document. Never silently delete.

---

## 1. Package Boundary

The JUhalt measurement/design ecosystem should remain intentionally modular:

- **`contentvalidR`** — conceptual/content evidence before or during item development:
  construct definition, item generation, substantive/content validity, expert/sorter judgments.
- **`nomologR`** — empirical measurement and construct-validity evidence:
  item/data audit, dimensionality, EFA, CFA, reliability, convergent/discriminant evidence,
  measurement invariance, and theory-specified nomological networks.
- **`solomonR`** — Solomon four-group experimental design analysis:
  pretest sensitization, treatment effects, combination procedures, robust alternatives.

### Natural handoff

`contentvalidR` → `nomologR` → substantive/experimental research  
`solomonR` is invoked only when the substantive study uses a Solomon four-group design.

---

## 2. Design Principles

Every public function and report should follow these principles.

1. **Measurement before structure.**
2. **Evidence accumulates; validity is not a single test.**
3. **Cutoffs are reference points, not universal laws.**
4. **No automatic item deletion.**
5. **Estimator/correlation choices must respect item type.**
6. **Modification indices never authorize changes by themselves.**
7. **Nomological evidence must begin with explicit theoretical expectations.**
8. **A null prediction is not supported merely because p > .05.**
9. **Every recommendation should state why it was made.**
10. **Every consequential user decision should be recordable in a decision log.**
11. **The teaching layer should be separable from the statistical engine.**
12. **Reproducibility is a release requirement, not an optional feature.**

---

# RELEASE TRACK

## Milestone 0 — Foundation Reset
**Status:** Complete

**Target version:** `0.0.0.9000`  
**Purpose:** Stabilize the project's identity before implementing substantive methods.

### Scope
- [x] Standardize package name/casing as `nomologR`.
- [x] Replace generic `cv_*` user-facing API with package-specific `nomo_*` API.
- [x] Reset development version to `0.0.0.9000`.
- [x] Rewrite `DESCRIPTION` around the package's true niche.
- [x] Rewrite README around workflow, teaching, and decision support.
- [x] Create `NEWS.md`.
- [x] Create/commit this `ROADMAP.md`.
- [x] Update obsolete `usethis`/CI helper code.
- [x] Replace deprecated planned `semTools` APIs.
- [x] Decide the minimum supported R version.
- [x] Confirm MIT licensing metadata.
- [x] Confirm GitHub Actions R-CMD-check workflow is active.

### Proposed public API
```r
nomo_defaults()
nomo_screen()
nomo_factors()
nomo_efa()
nomo_cfa()
nomo_reliability()
nomo_validity()
nomo_invariance()
nomo_hypotheses()
nomo_network()
nomo_run()
nomo_report()
```

### Internal/support API
```r
nomo_log_new()
nomo_log_add()
nomo_check_items()
nomo_check_model()
nomo_explain()
```

### Exit gate
Do **not** begin v0.1 implementation until:

- [x] `devtools::document()` succeeds without warnings attributable to package code.
- [x] `devtools::test()` passes.
- [x] `devtools::check()` has 0 errors and no unexplained warnings.
- [x] GitHub Actions runs successfully on the default branch.
- [x] README clearly distinguishes `nomologR`, `contentvalidR`, and `solomonR`.

---

# v0.1.0 — Minimum Useful Construct-Validation Workflow

## Milestone 1 — Data & Item Audit
**Status:** Complete

**Goal:** Turn a raw item set into an interpretable diagnostic object without changing the data.

### Functions
```r
nomo_screen()
nomo_defaults()
```

### Required analyses
- [x] Missingness by item and case.
- [x] Response frequencies / floor-ceiling concentration.
- [x] Number of unique response categories.
- [x] Zero / near-zero variance flags.
- [x] Corrected item-rest (item-total) correlations.
- [x] Inter-item correlations.
- [x] Optional skew/kurtosis summaries for continuous-like indicators.
- [x] Automatic recognition or user declaration of:
  - continuous
  - ordinal
  - binary
  - mixed item sets

### Teaching behavior
Each flag must contain:
- metric
- observed value
- reference value/rationale
- severity (`info`, `review`, `concern`)
- explanation
- suggested next inspection
- **no automatic retention/deletion action**

### Benchmarks
Reference points may include:
- corrected item-rest around `.30` as a **review threshold**
- extreme missingness or response concentration as configurable flags

These values are teaching references, not pass/fail laws.

### Tests
- [x] Continuous toy data.
- [x] Ordinal toy data.
- [x] Binary toy data.
- [x] Missing data.
- [x] Constant item.
- [x] Reverse-keyed item.
- [x] Invalid column names/types.
- [x] Stable regression coverage for decision-log and presentation behavior.

### Exit gate
- [x] 100% of exported arguments documented.
- [x] >= 90% line coverage for Milestone 1 code (96.6% at milestone closeout).
- [x] No function alters supplied data unless explicitly requested.
- [x] README contains a working `nomo_screen()` example.
- [x] Milestone PR/CI review completed and merged.

---

## Milestone 2 — Factor-Retention Evidence
**Status:** Complete

**Goal:** Help users answer, “How many latent dimensions should I investigate?”

### Function
```r
nomo_factors()
```

### Required analyses
- [x] Common-factor parallel analysis as the primary retention method.
- [x] Scree information for common-factor and component eigenvalues.
- [x] Velicer original MAP (TR2) as complementary evidence.
- [x] Velicer revised MAP (TR4) as complementary sensitivity evidence.
- [x] Empirical Kaiser criterion (EKC) in the default core bundle.
- [x] Optional NEST and Hull (CAF) where assumptions are supported.
- [x] Optional comparison data in the `all` bundle.
- [x] Legacy Kaiser-Guttman (>1) displayed only as historical context and excluded from synthesis.
- [x] KMO/item MSA as supporting adequacy diagnostics.
- [x] Bartlett's test as descriptive/supporting evidence when one common N is available.
- [x] Correlation-matrix selection:
  - Pearson
  - polychoric
  - tetrachoric
  - mixed, where supported
- [x] Explicit researcher modeling-type overrides with documented storage-safety guardrails.
- [x] Explicit non-positive-definite handling; no silent smoothing.
- [x] Criterion-family synthesis so related variants are not double-counted as independent votes.

### Criterion bundles
- `minimal`: parallel analysis + original MAP (TR2)
- `core`: adds revised MAP (TR4) + EKC
- `extended`: adds NEST + Hull (CAF) when supported
- `all`: adds comparison data + legacy Kaiser-Guttman context

### Important rule
The package may recommend:
> “Evidence most strongly supports investigating 2 factors.”

It should **not** say:
> “The scale has exactly 2 factors.”

### Researcher control
`types` may override an otherwise ambiguous default when the supplied coding can
safely support the declared measurement level. Overrides are logged and do not
reorder, relabel, or silently recode categories. Constant/all-missing items and
incompatible storage remain hard failures. README examples document the intended
workflow.

### Tests
- [x] Simulated 1-factor continuous population.
- [x] Simulated 2-factor correlated continuous population.
- [x] Ordinal one-factor population.
- [x] Ordinal two-factor correlated population.
- [x] Binary and mixed indicator workflows.
- [x] Small-sample review behavior.
- [x] Pairwise vs complete missing-data behavior.
- [x] Non-positive-definite correlation-matrix behavior and explicit smoothing.
- [x] Reproducible stochastic settings and caller RNG restoration.
- [x] Ambiguous/disagreeing retention evidence with cautious synthesis.
- [x] Criterion skipping/qualification behavior.
- [x] Presentation, plotting, validation, wrapper-failure, and synthesis edge cases.

### Coverage closeout
Before the final researcher-control closeout patch, the M2 audit reported:
- `R/nomo_factors.R`: 94.31%
- `R/nomo_factors_criteria.R`: 97.07%
- `R/nomo_factors_presentation.R`: 100.00%
- package-wide: 96.21% despite future-milestone stubs remaining intentionally unimplemented

The v0.1 core-computational coverage gate is therefore satisfied. Coverage is
used alongside known-answer simulations, edge-case tests, engine comparisons,
clean checks, and CI rather than as a stand-alone correctness claim.

### Exit gate
- [x] Factor recommendation agrees with known simulated structure under ordinary conditions.
- [x] Ambiguous simulations produce appropriately cautious output.
- [x] Seed and stochastic settings are reproducible and reported.
- [x] Requested but unsupported criteria are explicitly skipped with reasons.
- [x] No retention criterion automatically deletes items or declares dimensionality proven.
- [x] Core M2 computational modules exceed the >=90% v0.1 coverage requirement.
- [x] Final Milestone 2 PR/CI review and squash merge.

---

## Milestone 3 — Exploratory Factor Analysis
**Status:** Complete

**Goal:** Provide a transparent exploratory structure without automating scale purification.

### Function
```r
nomo_efa()
```

### Required capabilities
- [x] Oblique rotation default.
- [x] Extraction method is explicit, validated, and accompanied by method guidance rather than an automatic skew/kurtosis rule.
- [x] Tidy pattern matrix.
- [x] Structure matrix when applicable.
- [x] Communalities and uniquenesses.
- [x] Cross-loading diagnostics.
- [x] Factor correlations.
- [x] Residual diagnostics, including localized residual pairs and off-diagonal RMSR.
- [x] Model/sample adequacy notes.
- [x] User-controlled factor count.
- [x] Integration with `nomo_factors()`, including inherited modeling decisions and retention ambiguity.
- [x] Explicit non-positive-definite handling and user-requested smoothing.
- [x] Neutral public factor labels without sign/order manipulation.
- [x] Decision logging with correct researcher-vs-inherited provenance.
- [x] Teaching-oriented summary and plot methods.

### Teaching references
Examples:
- primary loading around `.40` → inspect
- cross-loading around `.30` → inspect
- communality around `.40` → inspect

But decisions must combine:
- theory/content coverage
- loading magnitude
- cross-loading
- communality
- redundancy
- factor interpretability

### Decision workflow
For each item, output:
```text
KEEP / REVIEW / STRONG REVIEW
```
not:
```text
DELETE
```

`KEEP` means that no configured numerical EFA teaching-reference flag fired. It
does not certify substantive appropriateness or construct validity.

### Tests
- [x] Known simple structure.
- [x] Known cross-loading item.
- [x] Weak item.
- [x] Highly redundant item set.
- [x] Ordinal item set.
- [x] Factor-order/sign indeterminacy handled in tests.
- [x] Missing-data failure and complete-case paths.
- [x] Orthogonal rotation and one-factor behavior.
- [x] Explicit smoothing and non-positive-definite failure.
- [x] `nomo_factors()` handoff and modeling-type provenance.
- [x] User modeling-type overrides.
- [x] Presentation and plotting regression tests.
- [x] Unsupported extraction-method validation.

### Coverage closeout
Final pre-closeout audit:
- `R/nomo_efa.R`: 97.78%
- `R/nomo_efa_presentation.R`: 98.70%
- package-wide: 96.52%

Coverage is treated as supporting software-quality evidence alongside known-answer
simulation, edge-case testing, direct-engine behavior, clean `R CMD check`, and
user-facing visual review.

### Exit gate
- [x] User can reproduce the EFA table from package output.
- [x] Every flagged item has an explanation.
- [x] No hidden model refitting.
- [x] Core M3 computational modules exceed the >=90% v0.1 coverage requirement.
- [x] User-facing output/plot audit completed.
- [x] Final Milestone 3 PR/CI review and squash merge.

---

## Checkpoint A — Exploratory Workflow Complete
**Status:** Complete

At this checkpoint a new user can:

```r
x <- nomo_screen(dat, items = ...)
k <- nomo_factors(dat, items = ...)
e <- nomo_efa(dat, items = ..., factors = k)
summary(e)
```

and understand:
1. what was examined,
2. why it was examined,
3. what appears problematic,
4. what reasonable next choices exist.

### Checkpoint A release candidate
`0.1.0.9001`

- [x] `nomo_screen()` → `nomo_factors()` → `nomo_efa()` works as a coherent handoff.
- [x] A vignette walks through the exploratory workflow.
- [x] Numerical review references remain non-prescriptive.
- [x] Decision provenance is retained across stages.
- [x] Checkpoint-A core modules satisfy the >=90% coverage gate.
- [x] Local tests/checks and user-facing visual audit are clean.
- [x] GitHub Actions green and Milestone 3 squash merged.

---

## Milestone 4 — Confirmatory Factor Analysis
**Status:** Complete

**Goal:** Test a researcher-specified measurement model on fresh/holdout data when feasible.

### Functions
```r
nomo_cfa()
nomo_model()
nomo_split()
```

### Required capabilities
- [x] lavaan model syntax accepted directly.
- [x] Helper syntax generation for simple factor structures.
- [x] Continuous estimator guidance.
- [x] Ordinal/WLSMV guidance.
- [x] Robust ML support.
- [x] Standardized loadings with uncertainty.
- [x] Factor correlations.
- [x] Residuals and localized residual-pair diagnostics.
- [x] Fit indices:
  - [x] χ²
  - [x] CFI
  - [x] TLI
  - [x] RMSEA + CI
  - [x] SRMR
- [x] Modification indices available but quarantined in a diagnostic section.
- [x] Heywood/improper-solution warnings.
- [x] Identification/convergence diagnostics.
- [x] Captured engine warnings and case-retention reporting.
- [x] Underlying `lavaan` fit retained unchanged.
- [x] No automatic model respecification.

### Fit philosophy
Reference values such as CFI/TLI ≈ `.95`, RMSEA ≈ `.06`, SRMR ≈ `.08`
may appear in teaching output, but model evaluation must discuss:
- model complexity
- estimator
- sample size
- indicator type
- localized strain
- theoretical coherence

### Sample-splitting behavior
`nomologR` should:
- [x] encourage independent EFA/CFA samples when feasible,
- [x] support user-supplied calibration/validation samples,
- [x] optionally create a reproducible split with `nomo_split()`,
- [x] explain the loss of power/generalizability tradeoff,
- [x] restore caller RNG state after reproducible splitting.

### Tests
- [x] Correctly specified CFA.
- [x] Misspecified CFA.
- [x] Cross-loading omitted.
- [x] Correlated residual omitted.
- [x] Ordinal CFA.
- [x] Robust ML.
- [x] Continuous FIML.
- [x] Nonconvergence.
- [x] Heywood/improper-solution diagnostics.
- [x] Direct `lavaan::cfa()` regression comparisons.
- [x] Presentation/plotting behavior.
- [x] Split-sample reproducibility.

### Coverage closeout
Final M4 audit:
- package-wide: 96.69%
- `R/nomo_cfa.R`: 97.37%
- `R/nomo_cfa_presentation.R`: 99.58%
- `R/nomo_model.R`: 97.73%
- `R/nomo_split.R`: 98.31%

### Exit gate
- [x] Same model gives estimates consistent with direct `lavaan::cfa()`.
- [x] Package adds interpretation but does not change `lavaan` estimates.
- [x] Modification indices never trigger automatic respecification.
- [x] Well-specified and deliberately poor models are clearly differentiated.
- [x] Holdout CFA workflow operates coherently after calibration EFA.
- [x] User-facing CFA output/plot audit completed.
- [x] Local tests/checks are clean.
- [x] Final Milestone 4 PR/CI review and squash merge.

---

## Milestone 5 — Reliability & Convergent/Discriminant Evidence
**Status:** Complete

**Goal:** Separate reliability from validity and present multiple forms of
measurement evidence without turning reference values into binary verdicts.

### Functions
```r
nomo_reliability()
nomo_validity()
```

### Reliability
Primary:
- [x] omega / composite reliability via current `semTools::compRelSEM()`.
- [x] ordinal-aware reliability where applicable.
- [x] explicit observed-ordinal versus latent-response score-scale interpretation.
- [x] optional bootstrap confidence intervals without replacing point estimates.

Secondary/common:
- [x] alpha, clearly qualified.
- [x] alpha unavailable rather than silently redefined when the requested
      observed-ordinal estimand is not supported by the fitted CFA workflow.

### Convergent evidence
- [x] standardized loadings carried forward from CFA.
- [x] AVE via current `semTools::AVE()`.
- [x] uncertainty retained where available from the fitted CFA.
- [x] AVE explicitly separated from reliability.

### Discriminant / construct-separation evidence
- [x] HTMT2 as the preferred congeneric-oriented statistic.
- [x] original HTMT as comparison evidence.
- [x] latent-factor correlations with available CFA uncertainty.
- [x] optional legacy/supporting Fornell-Larcker table.
- [x] no silent pooling across groups/levels for HTMT-family evidence.
- [x] cross-loaded simple-structure limitations surfaced explicitly.

### Important rule
Output may say:
> “This evidence warrants review of construct separation.”

It should **not** say:
> “Discriminant validity = PASS/FAIL.”

Likewise, reliability and AVE reference values trigger interpretation and
inspection rather than automatic scale revision.

### Presentation
- [x] compact reliability summary with omega primary and alpha secondary.
- [x] reliability plot with optional bootstrap confidence intervals.
- [x] AVE plot without duplicating CFA item-loading graphics.
- [x] HTMT-family construct-separation plot.
- [x] conceptual 0-to-1 coefficient display range by default, expanding only
      when empirical values/intervals require it.
- [x] redundant one-level legends and repetitive captions minimized.
- [x] Checkpoint B vignette integrating CFA → reliability → validity evidence.

### Tests
- [x] high-reliability congeneric scale.
- [x] weak measurement despite acceptable/global CFA fit.
- [x] two factors with strong separation.
- [x] two nearly redundant factors.
- [x] ordinal scale.
- [x] direct `semTools` regression comparisons.
- [x] one-factor naming/status regression.
- [x] mixed ordered/continuous composite refusal.
- [x] higher-order/nonconverged model guards.
- [x] bootstrap uncertainty and failure disclosure.
- [x] HTMT/Fornell-Larcker unavailable-case disclosure.
- [x] presentation/plotting regression tests.

### Coverage closeout
Final M5 audit:
- package-wide: **95.13%**
- `R/nomo_reliability.R`: **90.26%**
- `R/nomo_validity.R`: **92.56%**
- `R/nomo_measurement_helpers.R`: **92.70%**
- `R/nomo_reliability_uncertainty.R`: **89.33%**
- `R/nomo_reliability_presentation.R`: **88.93%**
- `R/nomo_validity_presentation.R`: **81.20%**

The primary M5 analytical modules and package-wide coverage meet the v0.1
quality gate. The optional bootstrap uncertainty helper is just below 90% after
targeted hardening; remaining uncovered branches are primarily defensive or
failure-state paths. No tests were added merely to manufacture 100% coverage.

### Exit gate
- [x] Reliability results match direct engine results to tolerance.
- [x] AVE is not labeled as reliability.
- [x] HTMT-family warnings are appropriately cautious.
- [x] Ordered score-scale distinctions are explicit.
- [x] Bootstrap uncertainty does not alter point-estimate estimands.
- [x] No automatic item deletion, construct merging, or validity declaration.
- [x] User-facing tables/plots audited.
- [x] Local tests/checks clean.
- [x] Core M5 analytical coverage and package-wide coverage satisfy the v0.1 gate.

---

## Checkpoint B — Measurement Model Complete
**Status:** Complete

A user can now move from raw scale data through a defensible measurement model:

```r
scr <- nomo_screen(...)
fac <- nomo_factors(...)
efa <- nomo_efa(...)
cfa <- nomo_cfa(...)
rel <- nomo_reliability(cfa)
val <- nomo_validity(cfa)
```

and distinguish among:
1. item/data quality,
2. dimensionality,
3. exploratory structure,
4. confirmatory model fit,
5. score reliability,
6. convergent evidence, and
7. construct separation.

### Required teaching vignette
- [x] **“From CFA to a defensible measurement model”**

### Checkpoint B release candidate
`0.1.0.9002`

- [x] M1–M5 form a coherent staged measurement workflow.
- [x] Reliability is separated from validity.
- [x] Global fit is not allowed to substitute for measurement quality.
- [x] Strong reliability/AVE is not allowed to substitute for construct separation.
- [x] Uncertainty is retained or explicitly available where appropriate.
- [x] Numerical references remain review prompts rather than universal laws.
- [x] Decision logging preserves reasons and limitations.
- [x] Package-wide coverage remains above the v0.1 release minimum.
- [x] User-facing visual audit completed.

---

### v0.1 completion sequence after Checkpoint B

Milestones 6–9 are all required before the first public v0.1 release and R-Universe launch.

The planned sequence is:

1. **M6 + M7 in parallel:** theory-specified nomological evidence and measurement invariance/generalizability;
2. **M8:** one-stop guided pipeline without hidden consequential decisions;
3. **M9:** reproducible researcher-facing report with methods, evidence, decisions, deviations, citations, and session information;
4. **researcher-completeness and clean-install audit;**
5. **pkgdown, release infrastructure, R-Universe, and v0.1.0.**

Features from later roadmap stages may be pulled forward when they close a methodological gap required for a defensible v0.1 workflow, but not merely to expand scope.

For v0.1, planned pull-forwards include researcher-specified SESOI/equivalence regions for negligible nomological predictions, external criterion/predictive outcomes within the network layer, researcher-controlled partial invariance, holdout/replication support where feasible, and evidence provenance that can be carried into the guided pipeline and final report.

## Milestone 6 — Theory-Specified Nomological Network
**Status:** Complete

**Goal:** Make nomological evidence the package's signature contribution.

### Functions
```r
nomo_hypotheses()
nomo_network()
```

### Hypothesis specification
Supported expectations include:

```r
h <- nomo_hypotheses(
  "GSE -> Spirituality" = positive(),
  "GSE -> Religiosity" = negligible(within = c(-.10, .10)),
  "Religiosity -> ATLG" = positive(min = .20),
  "Spirituality -> ATLG" = negligible()
)
```

- [x] Positive and negative directional predictions.
- [x] Optional minimum/maximum magnitude requirements.
- [x] Researcher-specified negligible/SESOI regions.
- [x] Qualitative bare `negligible()` expectations that are explicitly not
      confirmable without a quantitative region.
- [x] Standardized hypothesis evaluation by default.
- [x] Explicit unstandardized expectations when raw units are substantively meaningful.
- [x] A-priori versus post-hoc provenance.

### Required output
For every theoretical relation:
- [x] predicted direction/range;
- [x] estimate and uncertainty;
- [x] standardized estimate;
- [x] p-value where relevant;
- [x] concordance classification;
- [x] measurement-context interpretation;
- [x] confirmatory/post-hoc status;
- [x] replication evidence when a validation sample is supplied.

### Null/negligible predictions
A non-significant p-value alone does **not** establish a null prediction.

For v0.1:
- [x] uncertainty is explicit;
- [x] researcher-specified SESOI/equivalence regions are supported;
- [x] `nomologR` never invents a SESOI;
- [x] observed external criteria/outcomes can enter the network while retaining
      their observed-variable estimand;
- [x] calibration/validation replication fits the exact same prespecified
      network in the validation sample.

Bayesian confirmation remains reserved for a later extension.

### Structural-model rules
- [x] Latent SEM is supported when item-level measurement is available.
- [x] Missing theory-specified paths can be added transparently to the fitted
      model and retained in provenance.
- [x] Model additions outside the a-priori theory are visibly post-hoc.
- [x] Measurement-quality review signals propagate into relation interpretation
      without replacing theory concordance.
- [x] The package does not create a one-number “nomological validity” score.
- [x] Composite/single-indicator measurement-error corrections are **not**
      inserted automatically. Observed criteria remain observed-variable
      estimands in v0.1; reliability-corrected single-indicator latent constructs
      are deferred to a later advanced extension.

### Replication
- [x] `validation_data=` is supported.
- [x] `nomo_split` objects can supply calibration and validation samples.
- [x] The exact prespecified fitted model is preserved across samples.
- [x] Relation-level replication statuses distinguish replicated concordance,
      uncertainty, instability, non-replication, replicated inconsistency, and
      sign reversal.

### Tests
- [x] Positive expected path.
- [x] Negative expected path.
- [x] Directionally correct but magnitude-insufficient prediction.
- [x] Imprecise estimate.
- [x] Quantified and unquantified negligible predictions.
- [x] Post-hoc provenance.
- [x] Measurement-quality review propagation.
- [x] Primary/validation sign reversal.
- [x] Ordered/WLSMV path.
- [x] Argument/failure validation and presentation regression tests.

### Coverage closeout
Final Checkpoint C audit:
- package-wide: **94.19%**
- `R/nomo_hypotheses.R`: **94.33%**
- `R/nomo_network.R`: **89.95%**
- `R/nomo_network_presentation.R`: **91.49%**

The network engine is effectively at the 90% core target and is backed by
known-truth simulations and consequential branch/failure tests rather than
coverage padding.

### Exit gate
- [x] A user can distinguish theory inconsistency from measurement inadequacy.
- [x] Hypotheses are machine-readable and retained in report-ready objects/tables
      for Milestone 9.
- [x] Post-hoc paths are visibly distinguished from a-priori paths.
- [x] Replication does not silently respecify the validation model.
- [x] User-facing tables and figures audited.

---

## Milestone 7 — Measurement Invariance
**Status:** Complete

**Goal:** Support defensible comparisons across groups/time.

### Functions
```r
nomo_invariance()
nomo_partial()
```

### Identification-aware sequences
Continuous indicators:
- [x] configural;
- [x] metric;
- [x] scalar;
- [x] strict.

Ordered indicators use category-aware sequences under Wu–Estabrook
identification rather than forcing a continuous-data ladder onto categorical
models:

- [x] ordered 4+ categories:
      configural → thresholds → metric → scalar → strict;
- [x] three-category indicators:
      configural → metric → scalar → strict, with threshold equality included
      where identification requires it;
- [x] binary indicators:
      configural → strong → strict, with thresholds/loadings/intercepts bundled
      where they are not separately testable;
- [x] mixed ordered structures use the most restrictive identification case present.

### Implementation
- [x] Current `semTools::measEq.syntax()` infrastructure.
- [x] Separate fitted `lavaan` models retained for each requested level.
- [x] Syntax and estimator/identification provenance retained.
- [x] WLSMV default for declared ordered indicators.
- [x] No universal ΔCFI/ΔRMSEA/χ² pass/fail rule.

### Local strain and partial invariance
- [x] Equality-constraint score diagnostics via `lavaan::lavTestScore()` are
      retained as **diagnostic only** evidence.
- [x] `nomo_partial()` requires explicit level, syntax, and researcher rationale.
- [x] Researcher-specified releases carry forward cumulatively to later
      restrictive levels.
- [x] Released constraints and rationale are retained in the decision log.
- [x] Local diagnostics never free parameters automatically.
- [x] `nomologR` never searches until it finds a partial-invariance model that
      “passes.”

### Output
- [x] fit at each level;
- [x] change in fit;
- [x] parameter constraints;
- [x] ordered-category/identification context;
- [x] localized sources of equality-constraint strain;
- [x] researcher-specified partial-invariance refits;
- [x] rationale/provenance for every explicit release;
- [x] report-ready tables and figures.

### Tests
- [x] Continuous multi-group example.
- [x] Ordered 4+ category example.
- [x] Three-category identification behavior.
- [x] Binary identification behavior.
- [x] Mixed ordered-category structure behavior.
- [x] Strong loading non-invariance worsens metric evidence.
- [x] Strong intercept non-invariance worsens scalar evidence.
- [x] Researcher-specified partial release can improve a deliberately strained
      model while preserving rationale/provenance.
- [x] Local diagnostics never create partial invariance themselves.
- [x] Argument/failure validation and presentation regression tests.

### Coverage closeout
Final Checkpoint C audit:
- `R/nomo_invariance.R`: **92.04%**
- `R/nomo_invariance_presentation.R`: **93.27%**
- `R/nomo_partial.R`: **90.57%**
- `R/nomo_table.R`: **100.00%**

### Exit gate
- [x] Continuous multi-group workflow.
- [x] Identification-aware ordinal/binary workflows.
- [x] Known non-invariance simulations localize strain.
- [x] Partial invariance remains explicitly researcher controlled.
- [x] User-facing tables and figures audited.

---

## Checkpoint C — Generalizability & Nomological Evidence Complete
**Status:** Complete

A user can now move from a defensible measurement model into two distinct
questions:

1. **Does the measurement model generalize sufficiently for the intended comparison?**
2. **Does the construct behave as theory predicted in its nomological network?**

Checkpoint C deliberately keeps those questions separate. Invariance evidence
does not establish nomological validity, and nomological concordance does not
repair weak measurement invariance.

### Checkpoint C release candidate
`0.1.0.9003`

- [x] Theory expectations are machine-readable before fitting the network.
- [x] Negligible predictions require researcher-specified equivalence regions
      for quantitative confirmation.
- [x] Measurement context, theory concordance, uncertainty, and replication
      remain distinct evidence streams.
- [x] Continuous and ordered invariance workflows are identification aware.
- [x] Partial invariance is researcher specified and rationale documented.
- [x] No automatic model respecification or parameter freeing.
- [x] Network and invariance tables/figures completed a researcher-facing visual audit.
- [x] Package-wide coverage is **94.19%** with core M6/M7 computational paths
      approximately 90% or higher and backed by truth/failure tests.

Checkpoint C is locked for PR/CI review. Milestone 8 begins from a fresh branch
after this checkpoint is merged to `master`.

---

## Milestone 8 — One-Stop Guided Pipeline
**Status:** Next — begin after Checkpoint C PR/CI merge

**Goal:** Make the package genuinely usable by non-specialists without hiding decisions.

### Function
```r
nomo_run()
```

### Modes
```r
nomo_run(..., mode = "teaching")
nomo_run(..., mode = "research")
```

### Teaching mode
Adds:
- plain-language explanations
- method rationale
- glossary links
- decision prompts
- “what next?” guidance

### Research mode
Adds:
- compact statistical output
- reproducible code snippets
- manuscript-ready tables

### Critical behavior
The pipeline pauses at consequential decisions rather than silently continuing.

Example:
```text
Three items show strong review flags.
No items have been removed.

Recommended next actions:
[1] inspect item diagnostics
[2] refit with a user-selected subset
[3] retain all items and proceed
```

### Exit gate
- [ ] Pipeline can be reproduced using individual component functions.
- [ ] Pipeline does not make hidden analytic decisions.
- [ ] Decision log fully reconstructs the workflow.

---

## Milestone 9 — Reproducible Report
**Goal:** Produce something a student can learn from and a researcher can archive.

### Function
```r
nomo_report()
```

### Required report sections
1. Researcher inputs and data characteristics
2. Item audit
3. Factor-retention evidence
4. EFA
5. CFA
6. Reliability
7. Convergent/discriminant evidence
8. Invariance, if requested
9. Nomological network
10. Decision log
11. Deviations/post-hoc decisions
12. Methods citation/reference section
13. Reproducibility/session information

### Exit gate
- [ ] HTML report renders on CI.
- [ ] Report is understandable without inspecting raw R objects.
- [ ] Every recommendation links to the evidence that produced it.

---

# v0.1.0 RELEASE GATE

`nomologR 0.1.0` is released only when all of the following are true.

## Statistical correctness
- [ ] Core estimates reproduce underlying engine results within numeric tolerance.
- [ ] Simulation tests recover known population structures.
- [ ] Ordinal and continuous workflows are both tested.
- [ ] Failure modes (nonconvergence, non-PD matrices, Heywood cases) are handled clearly.

## Software quality
- [ ] 0 R CMD check errors.
- [ ] 0 unexplained R CMD check warnings.
- [ ] GitHub Actions green across intended OS/R matrix.
- [ ] ≥ 90% coverage for core computational modules.
- [ ] No exported TODO/stub functions.
- [ ] Every exported function has examples.

## Documentation
- [ ] README quick start.
- [ ] “Measurement-first workflow” vignette.
- [ ] “Nomological network” vignette.
- [ ] Function reference complete.
- [ ] `NEWS.md`.
- [ ] `CITATION.cff`.
- [ ] Package citation via `inst/CITATION` if warranted.
- [ ] Method references linked from help pages.

## User experience
- [ ] New user can complete included example without reading source code.
- [ ] Teaching output tested for clarity.
- [ ] No recommendation uses unexplained jargon.
- [ ] Every auto-generated conclusion can be traced to a metric/rule/source.

## Release infrastructure
- [ ] GitHub release candidate tag tested.
- [ ] pkgdown site.
- [ ] R-universe setup.
- [ ] Installation instructions verified on clean R session.
- [ ] Public issue templates for bug / method question / feature request.

---

# v0.2.x — Robustness & Broader Measurement Models

Candidate modules:

- [ ] Bifactor models.
- [ ] Higher-order CFA.
- [ ] ESEM.
- [ ] Cross-validation helpers.
- [ ] Longitudinal invariance.
- [ ] Missing-data sensitivity.
- [ ] Multiple-imputation integration.
- [ ] Bootstrap stability summaries.
- [ ] CFA/SEM sample-size and power planning.
- [ ] Criterion/predictive evidence module.

---

# v0.3.x — Modern Extensions

Candidate modules:

- [ ] IRT as a complementary item-level framework.
- [ ] DIF.
- [ ] Equivalence testing for negligible structural relations.
- [ ] SESOI-aware hypothesis specifications.
- [ ] Bayesian CFA/SEM (`blavaan`) robustness module.
- [ ] Posterior predictive checking.
- [ ] Frequentist/Bayesian concordance summaries.

---

# Long-Term Research Program

Potential research contributions arising from `nomologR` itself:

1. **Decision stability**
   - How often do common scale-development heuristics lead researchers to different item sets?

2. **Cutoff sensitivity**
   - How sensitive are substantive conclusions to common loading, AVE, HTMT, and fit-index cutoffs?

3. **Nomological concordance**
   - Develop formal summaries of how well an empirical structural network matches an a priori theoretical network.

4. **Researcher degrees of freedom**
   - Quantify the decision tree generated by item removal, residual correlations, estimator choices, and model respecification.

5. **Bayesian nomological evidence**
   - Posterior probability that predicted path direction/magnitude satisfies a prespecified theoretical region.

6. **Teaching outcomes**
   - Test whether guided `nomologR` reports improve methodological understanding relative to conventional software output.

---

# Development Workflow / Checkpoint Discipline

For every milestone:

1. Open an issue defining scope and exit criteria.
2. Implement on a feature branch.
3. Add tests before or alongside substantive code.
4. Update documentation/vignette.
5. Run:
   ```r
   devtools::document()
   devtools::test()
   devtools::check()
   ```
6. Push and require GitHub Actions to pass.
7. Review API/output for teaching clarity.
8. Merge only after exit gate is satisfied.
9. Update `NEWS.md` and check off roadmap items.
10. Tag milestone release candidate when appropriate.

## Rule for scope creep

A feature may enter the current milestone only if it is necessary for:
- statistical correctness,
- reproducibility,
- documentation,
- or the milestone's stated user story.

Otherwise it goes into the next-version parking lot.

---

# Definition of Success

`nomologR` succeeds if a graduate student or applied researcher can start with a
candidate measure and finish with:

- a defensible measurement model,
- transparent evidence about reliability and construct validity,
- a theory-specified nomological network,
- a record of every important analytic decision,
- reproducible R code,
- and an explanation of **why** each step was taken.

The package should make the process cleaner without making it more automatic than the science allows.
