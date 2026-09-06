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
- why the feature belongs in `nomologR` rather than solely in the underlying engine package.

Methodological references are strongly encouraged.

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

The current development specification is maintained in `ROADMAP.md`.
