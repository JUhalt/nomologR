# Package index

## Guided workflow and reporting

Run the staged workflow with explicit researcher decisions, extract
report-ready tables, and archive evidence and decisions.

- [`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
  : Run the guided nomologR workflow
- [`nomo_revise()`](https://juhalt.github.io/nomologR/reference/nomo_revise.md)
  : Revise a guided workflow and keep its lineage
- [`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
  : Render a reproducible nomologR analysis report
- [`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md)
  : Extract report-ready evidence tables
- [`nomo_defaults()`](https://juhalt.github.io/nomologR/reference/nomo_defaults.md)
  : Default guidance settings for nomologR

## 

Item and data audit

- [`nomo_screen()`](https://juhalt.github.io/nomologR/reference/nomo_screen.md)
  : Audit item-level data before factor modeling
- [`print(`*`<nomo_screen>`*`)`](https://juhalt.github.io/nomologR/reference/print.nomo_screen.md)
  : Print a nomo_screen object
- [`summary(`*`<nomo_screen>`*`)`](https://juhalt.github.io/nomologR/reference/summary.nomo_screen.md)
  : Summarize a nomo_screen audit
- [`print(`*`<summary_nomo_screen>`*`)`](https://juhalt.github.io/nomologR/reference/print.summary_nomo_screen.md)
  : Print a summary_nomo_screen object
- [`plot(`*`<nomo_screen>`*`)`](https://juhalt.github.io/nomologR/reference/plot.nomo_screen.md)
  : Plot a nomo_screen audit

## 

Dimensionality and exploratory structure

- [`nomo_factors()`](https://juhalt.github.io/nomologR/reference/nomo_factors.md)
  : Evaluate evidence about the number of latent factors
- [`print(`*`<nomo_factors>`*`)`](https://juhalt.github.io/nomologR/reference/print.nomo_factors.md)
  : Print factor-retention evidence
- [`summary(`*`<nomo_factors>`*`)`](https://juhalt.github.io/nomologR/reference/summary.nomo_factors.md)
  : Summarize factor-retention evidence
- [`print(`*`<summary_nomo_factors>`*`)`](https://juhalt.github.io/nomologR/reference/print.summary_nomo_factors.md)
  : Print a factor-retention summary
- [`plot(`*`<nomo_factors>`*`)`](https://juhalt.github.io/nomologR/reference/plot.nomo_factors.md)
  : Plot factor-retention evidence
- [`nomo_efa()`](https://juhalt.github.io/nomologR/reference/nomo_efa.md)
  : Guided exploratory factor analysis
- [`summary(`*`<nomo_efa>`*`)`](https://juhalt.github.io/nomologR/reference/summary.nomo_efa.md)
  : Summarize a guided exploratory factor analysis
- [`plot(`*`<nomo_efa>`*`)`](https://juhalt.github.io/nomologR/reference/plot.nomo_efa.md)
  : Plot exploratory factor-analysis evidence

## 

Confirmatory measurement model

- [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md)
  : Build simple confirmatory factor-analysis syntax
- [`nomo_split()`](https://juhalt.github.io/nomologR/reference/nomo_split.md)
  : Create a reproducible calibration/validation split
- [`nomo_cfa()`](https://juhalt.github.io/nomologR/reference/nomo_cfa.md)
  : Guided confirmatory factor analysis
- [`summary(`*`<nomo_cfa>`*`)`](https://juhalt.github.io/nomologR/reference/summary.nomo_cfa.md)
  : Summarize a guided confirmatory factor analysis
- [`plot(`*`<nomo_cfa>`*`)`](https://juhalt.github.io/nomologR/reference/plot.nomo_cfa.md)
  : Plot confirmatory factor-analysis evidence
- [`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)
  : Compare confirmatory measurement models
- [`summary(`*`<nomo_compare>`*`)`](https://juhalt.github.io/nomologR/reference/summary.nomo_compare.md)
  : Summarize a measurement-model comparison
- [`plot(`*`<nomo_compare>`*`)`](https://juhalt.github.io/nomologR/reference/plot.nomo_compare.md)
  : Plot a measurement-model comparison

## 

Reliability and construct-validity evidence

- [`nomo_reliability()`](https://juhalt.github.io/nomologR/reference/nomo_reliability.md)
  : Model-based reliability evidence
- [`summary(`*`<nomo_reliability>`*`)`](https://juhalt.github.io/nomologR/reference/summary.nomo_reliability.md)
  : Summarize model-based reliability evidence
- [`plot(`*`<nomo_reliability>`*`)`](https://juhalt.github.io/nomologR/reference/plot.nomo_reliability.md)
  : Plot model-based reliability evidence
- [`nomo_validity()`](https://juhalt.github.io/nomologR/reference/nomo_validity.md)
  : Convergent and discriminant construct-validity evidence
- [`summary(`*`<nomo_validity>`*`)`](https://juhalt.github.io/nomologR/reference/summary.nomo_validity.md)
  : Summarize convergent and discriminant measurement evidence
- [`plot(`*`<nomo_validity>`*`)`](https://juhalt.github.io/nomologR/reference/plot.nomo_validity.md)
  : Plot convergent or discriminant validity evidence

## 

Measurement invariance

- [`nomo_invariance()`](https://juhalt.github.io/nomologR/reference/nomo_invariance.md)
  : Evaluate measurement invariance across groups
- [`nomo_partial()`](https://juhalt.github.io/nomologR/reference/nomo_partial.md)
  : Specify researcher-controlled partial-invariance releases
- [`plot(`*`<nomo_invariance>`*`)`](https://juhalt.github.io/nomologR/reference/plot.nomo_invariance.md)
  : Plot measurement-invariance evidence

## 

Theory-specified nomological network

- [`nomo_hypotheses()`](https://juhalt.github.io/nomologR/reference/nomo_hypotheses.md)
  : Specify a priori expectations for a nomological network
- [`positive()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
  [`negative()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
  [`negligible()`](https://juhalt.github.io/nomologR/reference/nomo_expectations.md)
  : Specify directional or negligible theoretical expectations
- [`nomo_network()`](https://juhalt.github.io/nomologR/reference/nomo_network.md)
  : Evaluate a theory-specified nomological network
- [`plot(`*`<nomo_network>`*`)`](https://juhalt.github.io/nomologR/reference/plot.nomo_network.md)
  : Plot nomological-network evidence

## Teaching datasets

Simulated data with documented population models, so learners can
compare package evidence with the known truth.

- [`nomo_demo_continuous`](https://juhalt.github.io/nomologR/reference/nomo_demo_continuous.md)
  : Simulated two-factor item data with known teaching features
- [`nomo_demo_ordinal`](https://juhalt.github.io/nomologR/reference/nomo_demo_ordinal.md)
  : Simulated five-category ordered item data
- [`nomo_demo_network`](https://juhalt.github.io/nomologR/reference/nomo_demo_network.md)
  : Simulated multi-construct validation study
