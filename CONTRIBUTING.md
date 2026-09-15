# Contributing to nomologR

Contributions, bug reports, methodological questions, and feature proposals are welcome.

## Project philosophy

`nomologR` is designed to make construct-validation reasoning visible without replacing researcher judgment.

Contributions should preserve the package's central principles:

- flag, explain, and document rather than silently modify data or models;
- treat statistical cutoffs as teaching references rather than universal laws;
- keep consequential researcher decisions explicit and reproducible;
- distinguish measurement problems from structural/theoretical problems;
- avoid automatic item deletion or automatic model respecification;
- preserve access to the underlying statistical-engine results where practical.

## Development workflow

1. Open or identify an issue describing the proposed change.
2. Create a feature branch from the current default branch.
3. Add or update tests alongside substantive code.
4. Update documentation and examples when user-facing behavior changes.
5. Before opening a pull request, run:

```r
devtools::document()
devtools::test()
devtools::check()
```

6. For substantive computational changes, inspect coverage and include known-answer, simulation, or direct-engine regression tests where appropriate.
7. Open a pull request and allow the GitHub Actions checks to complete.

## Statistical-method contributions

A proposed statistical feature should identify:

- the research question or decision it supports;
- the estimand or statistical quantity being reported;
- the underlying method or engine;
- assumptions and known limitations;
- how uncertainty is represented;
- how failure or unsupported cases are disclosed;
- where the method sits on the path from historical to contemporary practice,
  and which references support that placement;
- why the feature belongs in `nomologR` rather than solely in the underlying engine package.

Methodological references are expected for statistical methods. Add them to the
function's `@references` with DOIs where available, verify that each DOI
resolves to the cited work, and update the
[research basis](vignettes/research-basis.Rmd) article.

Examples and vignettes should run on the teaching datasets
(`nomo_demo_continuous`, `nomo_demo_ordinal`, `nomo_demo_network`) or other
data with a known answer. Avoid `\dontrun{}` for code that can run; use
`\donttest{}` for slow examples.

## Testing expectations

Tests should focus on correctness and consequential behavior rather than pursuing coverage percentages for their own sake.

Useful tests include:

- known-answer comparisons;
- direct comparisons with underlying engines such as `psych`, `lavaan`, or `semTools`;
- simulated populations with known structure;
- edge and failure cases;
- regression tests for interpretation and researcher-control behavior.

## Pull requests

Pull requests should describe what changed, why it changed, how it was tested, and any statistical or API decisions that deserve review.

The current development specification is maintained in [ROADMAP.md](ROADMAP.md).
Every accepted finding, code-review outcome, or research idea should have a
linked issue before implementation. Keep uncommitted proposals separate from
release scope; add a milestone only when the intended outcome and exit criteria
are accepted. Closeout should link actual validation and release evidence.

Each PR should identify its issue and documentation impact: README, roadmap,
reference/vignettes, NEWS, citation/license metadata, and published site as
applicable. Explain when a surface does not need updating.

Edit `README.Rmd` and regenerate `README.md`. `_pkgdown.yml` and source
documentation drive the website; the existing pkgdown workflow builds PRs and
publishes `master` to `gh-pages`. Generated `docs/` output is not tracked on
`master`. Verify the deployed site after merge, including its license, version,
and planning links. Do not label historical release checks as current tests.

New contributions use the current GPL version 3 only project license; preserve
required copyright and third-party notices.

## Distribution

`nomologR` is distributed through [R-universe](https://juhalt.r-universe.dev),
which builds each GitHub release, usually within a few hours. The first CRAN
submission is targeted for `v0.3.0`
([#39](https://github.com/JUhalt/nomologR/issues/39)), and CRAN-readiness checks
are part of `v0.2.0` release certification
([#37](https://github.com/JUhalt/nomologR/issues/37)).

Contributions should keep `R CMD check --as-cran` clean, keep examples and
vignettes fast, and use `skip_on_cran()` for long-running simulations.
