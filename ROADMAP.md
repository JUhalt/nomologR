# nomologR Roadmap

> **Mission:** Make rigorous construct validation easier to learn, easier to execute, and easier to defend.
>
> `nomologR` is a guided, evidence-based workflow for empirical scale development and construct validation. It coordinates established R engines (primarily `psych`, `EFAtools`, `lavaan`, and `semTools`) while adding transparent diagnostics, literature-linked explanations, decision logging, and theory-aware guidance.
>
> **Audience:** graduate students (master's and doctoral) learning these techniques, the faculty who teach and advise them, and researchers who apply them.
>
> **Core principle:** Flag, explain, and document. Never silently delete.

**Current stable release:** [0.2.0](https://github.com/JUhalt/nomologR/releases/tag/v0.2.0)
(September 19, 2026), GPL-3.0-only — research-backed, usable measurement
workflows, with scope selected in
[#22](https://github.com/JUhalt/nomologR/issues/22). The earlier
[0.1.0](https://github.com/JUhalt/nomologR/releases/tag/v0.1.0) release
(September 9, 2026) keeps its original MIT license.
**Next release:** [v0.2.1](https://github.com/JUhalt/nomologR/milestone/4) —
the Planned workstreams moved at v0.2.0 certification.
**Distribution:** [R-universe](https://juhalt.r-universe.dev), which builds each
GitHub release. The first CRAN submission is targeted for `v0.3.0`
([#39](https://github.com/JUhalt/nomologR/issues/39)).

The complete v0.1 milestone specifications, exit gates, and certification
records are preserved verbatim in
[`dev/roadmap-v0.1-record.md`](dev/roadmap-v0.1-record.md). Those records are
historical; they are not new verification results.

***
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

***
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
13. **Methods are research-backed and situated from historical to contemporary practice.**
    Historical techniques may appear as labeled context; they are never silently substituted for contemporary evidence.
14. **Teaching surfaces run on data with a known answer.**
    Examples and vignettes use the simulated teaching datasets so learners can compare evidence with the truth.

See [`VISION.md`](VISION.md) for the audience, recommendation standard, and standard for methods.

***
# v0.1.0 — Minimum Useful Construct-Validation Workflow

**Status:** Complete — released September 9, 2026
([release record](https://github.com/JUhalt/nomologR/releases/tag/v0.1.0);
closeout issues #12–#16; release PR #18; closeout PR #21).

| Milestone | Scope | Functions |
|---|---|---|
| M0 | Foundation reset | — |
| M1 | Data and item audit | `nomo_screen()`, `nomo_defaults()` |
| M2 | Factor-retention evidence | `nomo_factors()` |
| M3 | Exploratory factor analysis (Checkpoint A) | `nomo_efa()` |
| M4 | Confirmatory factor analysis | `nomo_model()`, `nomo_split()`, `nomo_cfa()` |
| M5 | Reliability and convergent/discriminant evidence (Checkpoint B) | `nomo_reliability()`, `nomo_validity()` |
| M6 | Theory-specified nomological network | `nomo_hypotheses()`, `nomo_network()` |
| M7 | Measurement invariance (Checkpoint C) | `nomo_invariance()`, `nomo_partial()`, `nomo_table()` |
| M8 | One-stop guided pipeline | `nomo_run()` |
| M9 | Reproducible report | `nomo_report()` |

Detailed scope, tests, coverage closeouts, and exit gates for each milestone are
in [`dev/roadmap-v0.1-record.md`](dev/roadmap-v0.1-record.md).

***
<a id="v02x--robustness--broader-measurement-models"></a>
# v0.2.0 — Research-Backed, Usable Measurement Workflows

**Status:** Released September 19, 2026. Scope selected in [#22](https://github.com/JUhalt/nomologR/issues/22). Every Core workstream shipped; six Planned workstreams moved to [v0.2.1](https://github.com/JUhalt/nomologR/milestone/4) at certification, each with its reason recorded on the issue.

**Goals.** Make `nomologR`:

1. **useful to graduate students and researchers** applying scale-development
   and construct-validation techniques;
2. **backed by research**, covering techniques from historical to contemporary
   practice with verifiable references; and
3. **usable and informative**, with runnable examples, walkthroughs on data with
   known answers, and output that explains its reasoning.

**Core** workstreams must ship in v0.2.0. **Planned** workstreams are in scope
but may move to a later release with a recorded rationale. Each issue records
the user problem, historical-to-contemporary lineage, estimand, assumptions and
unsupported cases, validation plan, and exit criteria.

## Core workstreams

- [x] [#25](https://github.com/JUhalt/nomologR/issues/25) Learning foundations — teaching datasets, runnable vignettes and examples, site navigation (completed in [#41](https://github.com/JUhalt/nomologR/pull/41)).
- [x] [#26](https://github.com/JUhalt/nomologR/issues/26) Research basis — historical-to-contemporary methods documentation, verified references, methods registry. References and the research-basis article shipped in [#41](https://github.com/JUhalt/nomologR/pull/41); `nomo_methods()`, report-level citations of the methods a run used, and release-time DOI verification completed the workstream.
- [x] [#27](https://github.com/JUhalt/nomologR/issues/27) Model comparison — `nomo_compare()` for nested and non-nested measurement models.
- [x] [#28](https://github.com/JUhalt/nomologR/issues/28) Revision lineage — auditable revise-and-compare cycles in `nomo_run()`.
- [x] [#29](https://github.com/JUhalt/nomologR/issues/29) Bifactor and higher-order measurement models — `nomo_model()` structures and `nomo_hierarchical()` indices.
- [x] [#30](https://github.com/JUhalt/nomologR/issues/30) Replication-status language for near-zero sign changes — a sign change is a reversal only when both confidence intervals exclude zero on opposite sides.
- [x] [#37](https://github.com/JUhalt/nomologR/issues/37) Release certification, CRAN-readiness gate, and R-universe publication.

## Planned workstreams

- [x] [#31](https://github.com/JUhalt/nomologR/issues/31) Human-readable invariance local-strain labels (completed in [#41](https://github.com/JUhalt/nomologR/pull/41)).
- [ ] [#32](https://github.com/JUhalt/nomologR/issues/32) Missing-data sensitivity across measurement stages. **Moved to v0.2.1.**
- [ ] [#33](https://github.com/JUhalt/nomologR/issues/33) Score guidance — `nomo_scores()` for sum and factor scores. **Moved to v0.2.1.**
- [ ] [#34](https://github.com/JUhalt/nomologR/issues/34) Insufficient-effort responding screen in `nomo_screen()`. **Moved to v0.2.1.**
- [ ] [#35](https://github.com/JUhalt/nomologR/issues/35) Manuscript-ready tables and Word report output. **Moved to v0.2.1.**
- [x] [#36](https://github.com/JUhalt/nomologR/issues/36) Maintenance — test organization, `nomo_run.R` modularization, minimum-dependency CI.
- [ ] [#40](https://github.com/JUhalt/nomologR/issues/40) `nomo_report()` rendering from inside R Markdown or Quarto documents. **Moved to v0.2.1.**
- [ ] [#42](https://github.com/JUhalt/nomologR/issues/42) Optional parallel bootstrap for reliability confidence intervals. **Moved to v0.2.1.**

## Suggested sequence

1. **Foundations (done):** #25 and #31, with #26 maintained alongside every later workstream.
2. **Safe structure for revision work:** #36 (done), then #27 (done), then #28 (done).
3. **Broader measurement models and interpretation:** #29 (builds on #27) and #30.
4. **Usability extensions:** #32, #33, #34, #35, #40, #42 — moved to v0.2.1 at certification.
5. **Release:** #37, including the license/version/date gate carried from #22 and the CRAN-readiness gate.

## Deferred from v0.2 scope selection

Tracked for v0.3 scope selection in [#38](https://github.com/JUhalt/nomologR/issues/38),
each with a rationale: ESEM; longitudinal invariance; multiple-imputation
integration; bootstrap stability summaries; CFA/SEM sample-size and power
planning; criterion/predictive evidence beyond network outcomes; and research
proposal [#23](https://github.com/JUhalt/nomologR/issues/23) (model-specific fit
diagnostics). None were rejected.

***
# v0.3.x — Modern Extensions

**Status:** Scope selection in [#38](https://github.com/JUhalt/nomologR/issues/38);
[v0.3.0 milestone](https://github.com/JUhalt/nomologR/milestone/3).

**Committed:** the first CRAN submission ([#39](https://github.com/JUhalt/nomologR/issues/39)); see
[Distribution](#distribution--r-universe-now-cran-with-v030) below.

Candidate modules (not commitments until selected):

- [ ] ESEM.
- [ ] Longitudinal invariance.
- [ ] Multiple-imputation integration.
- [ ] Bootstrap stability summaries.
- [ ] CFA/SEM sample-size and power planning.
- [ ] Criterion/predictive evidence beyond network outcomes.
- [ ] IRT as a complementary item-level framework, and DIF.
- [ ] Bayesian CFA/SEM (`blavaan`) robustness module, posterior predictive checking, and frequentist/Bayesian concordance summaries.
- [ ] Additional equivalence/SESOI functionality beyond v0.1's researcher-specified negligible regions. No SESOI is invented by the package.
- [ ] Model-specific fit diagnostics ([#23](https://github.com/JUhalt/nomologR/issues/23)).
- [ ] A `contentvalidR` → `nomologR` handoff that carries content-validity decisions into the decision log.

***
# Distribution — R-universe Now, CRAN with v0.3.0

- Stable releases are published as GitHub releases. [R-universe](https://juhalt.r-universe.dev) builds each release, usually within a few hours.
- `v0.2.0` certification ([#37](https://github.com/JUhalt/nomologR/issues/37)) includes a CRAN-readiness gate: `R CMD check --as-cran` clean on win-builder, the macOS builder, and R-hub; long-running tests skipped on CRAN; fast examples and vignettes.
- [ ] First CRAN submission with `v0.3.0` ([#39](https://github.com/JUhalt/nomologR/issues/39)). Decision recorded September 15, 2026: submitting after the v0.2 functions have been used on R-universe avoids repeated CRAN resubmissions while the API is still growing.

***
# Long-Term Research Program

Potential research contributions arising from `nomologR` itself:

1. **Decision stability**
   - How often do common scale-development heuristics lead researchers to different item sets?

2. **Cutoff sensitivity**
   - How sensitive are substantive conclusions to common loading, AVE, HTMT, and fit-index cutoffs?
   - [Provisional proposal #23](https://github.com/JUhalt/nomologR/issues/23): evaluate
     model-specific CFA fit diagnostics with reproducible simulation provenance,
     drawing on McNeish and Wolf (2023),
     [doi:10.1037/met0000425](https://doi.org/10.1037/met0000425).
     This extends the teaching layer; it is not an automatic validity verdict
     or a committed release feature.

3. **Nomological concordance**
   - Develop formal summaries of how well an empirical structural network matches an a priori theoretical network.

4. **Researcher degrees of freedom**
   - Quantify the decision tree generated by item removal, residual correlations, estimator choices, and model respecification.

5. **Bayesian nomological evidence**
   - Posterior probability that predicted path direction/magnitude satisfies a prespecified theoretical region.

6. **Teaching outcomes**
   - Test whether guided `nomologR` reports improve methodological understanding relative to conventional software output.

***
# Development Workflow / Checkpoint Discipline

For every workstream:

1. Open an issue defining scope, historical-to-contemporary lineage, and exit criteria.
2. Implement on a feature branch.
3. Add tests before or alongside substantive code, including known-answer checks on teaching or simulated data.
4. Add DOI-verified references and update the reference documentation, relevant vignette, and [research basis](vignettes/research-basis.Rmd) article.
5. Run:
   ```r
   devtools::document()
   devtools::test()
   devtools::check()
   ```
6. Push and require GitHub Actions to pass.
7. Review API/output for teaching clarity.
8. Merge only after exit criteria are satisfied.
9. Update `NEWS.md` and check off roadmap items.
10. Tag a release candidate when appropriate.

## Rule for scope creep

A feature may enter the current release only if it is necessary for:
- statistical correctness,
- reproducibility,
- documentation,
- or the release's stated goals.

Otherwise it goes into the next-version candidate list.

***
# Definition of Success

`nomologR` succeeds if a graduate student or applied researcher can start with a
candidate measure and finish with:

- a defensible measurement model,
- transparent evidence about reliability and construct validity,
- a theory-specified nomological network,
- a record of every important analytic decision,
- reproducible R code,
- an understanding of where each method came from and what current research recommends,
- and an explanation of **why** each step was taken.

The package should make the process cleaner without making it more automatic than the science allows.
