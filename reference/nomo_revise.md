# Revise a guided workflow and keep its lineage

`nomo_revise()` creates a child workflow from a parent
[`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
with a revised measurement model, a revised item set, or both. The child
records what changed, why, whether the change was prespecified or post
hoc, and a
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)
result for the parent and revised measurement models. The parent
workflow is not modified.

## Usage

``` r
nomo_revise(
  run,
  cfa_model = NULL,
  items = NULL,
  rationale,
  origin = c("post_hoc", "a_priori"),
  decisions = list(),
  compare = TRUE
)
```

## Arguments

- run:

  A parent `nomo_run` object with a fitted measurement model.

- cfa_model:

  Optional revised measurement model: a lavaan syntax string or an
  object from
  [`nomo_model()`](https://juhalt.github.io/nomologR/reference/nomo_model.md).

- items:

  Optional named list of revised item vectors, one element per revised
  scale. Scales that are not named keep the parent's items. The
  measurement model must agree with the revised items: a revision is
  refused if the model still includes an item it drops, or omits an item
  it adds. Supply `cfa_model` together with `items` when the change
  requires it; `nomo_revise()` never rewrites the model.

- rationale:

  Required character scalar recording why the workflow is revised.

- origin:

  `"post_hoc"` (default) when the revision was prompted by results, or
  `"a_priori"` when it was planned in advance.

- decisions:

  Optional named list of decisions for the child workflow, for example
  `list(factor_count = c(WellBeing = 1))`. Supplied decisions override
  inherited ones.

- compare:

  Logical. If `TRUE` (default), compare the parent and revised
  measurement models with
  [`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md).

## Value

A new `nomo_run` object for the revised workflow, carrying `$lineage`
(one row per revision), `$revision_comparison` (the
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)
result, when available), and `$parent_summary`.

## Details

Revising is always a researcher decision: a rationale is required and no
item is removed or parameter freed automatically. The child workflow
reruns the staged evidence from screening onward with the revised scales
and model, then pauses at the measurement review so the revised evidence
is inspected before any downstream branch runs.

The factor-count decision is inherited from the parent unless
`decisions` supplies a new one, so the revision changes only what the
researcher changed.

Because a revision prompted by results is evaluated on the data that
prompted it, the decision log records whether the change was
`"post_hoc"` and recommends confirming the revised model in independent
data (Simmons, Nelson, & Simonsohn, 2011; Wicherts et al., 2016; Flake &
Fried, 2020).

Removing an item changes the observed variables, so parent and revised
models are not nested and
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)
reports descriptive evidence only. To test whether an item is needed,
keep it and fix its loading to zero in the revised model; see
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md).

## References

Flake, J. K., & Fried, E. I. (2020). Measurement schmeasurement:
Questionable measurement practices and how to avoid them. *Advances in
Methods and Practices in Psychological Science, 3*(4), 456-465.
[doi:10.1177/2515245920952393](https://doi.org/10.1177/2515245920952393)

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490-504.
[doi:10.1037/0033-2909.111.3.490](https://doi.org/10.1037/0033-2909.111.3.490)

Simmons, J. P., Nelson, L. D., & Simonsohn, U. (2011). False-positive
psychology: Undisclosed flexibility in data collection and analysis
allows presenting anything as significant. *Psychological Science,
22*(11), 1359-1366.
[doi:10.1177/0956797611417632](https://doi.org/10.1177/0956797611417632)

Wicherts, J. M., Veldkamp, C. L. S., Augusteijn, H. E. M., Bakker, M.,
van Aert, R. C. M., & van Assen, M. A. L. M. (2016). Degrees of freedom
in planning, running, analyzing, and reporting psychological studies: A
checklist to avoid p-hacking. *Frontiers in Psychology, 7*, 1832.
[doi:10.3389/fpsyg.2016.01832](https://doi.org/10.3389/fpsyg.2016.01832)

## See also

[`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md),
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)

## Examples

``` r
# \donttest{
scales <- list(Agency = c("ag1", "ag2", "ag3", "ag4"))

run <- nomo_run(
  data = nomo_demo_network,
  scales = scales,
  settings = list(factors = list(n_iter = 20, seed = 2026)),
  decisions = list(
    factor_count = c(Agency = 1),
    cfa_model = "Agency =~ ag1 + ag2 + ag3 + ag4"
  )
)

revised <- nomo_revise(
  run,
  cfa_model = "Agency =~ ag1 + ag2 + ag3 + ag4\nag1 ~~ ag2",
  rationale = paste(
    "Residual diagnostics and item wording suggest ag1 and ag2 share",
    "method variance beyond the common factor."
  ),
  origin = "post_hoc"
)

nomo_table(revised, "lineage")
#> # A tibble: 1 × 10
#>   revision change_type items_removed items_added parent_model      revised_model
#>      <int> <chr>       <chr>         <chr>       <chr>             <chr>        
#> 1        1 model       ""            ""          Agency =~ ag1 + … "Agency =~ a…
#> # ℹ 4 more variables: origin <chr>, rationale <chr>, sample_design <chr>,
#> #   comparison <chr>
nomo_table(revised$revision_comparison, "comparisons")
#> # A tibble: 1 × 23
#>   model   reference relation  nested_declared nesting_check nested df_difference
#>   <chr>   <chr>     <chr>     <chr>           <chr>         <lgl>          <dbl>
#> 1 revised parent    less_con… auto            nested        TRUE              -1
#> # ℹ 16 more variables: test <chr>, method <chr>, chisq_diff <dbl>,
#> #   df_diff <dbl>, p_value <dbl>, test_available <lgl>, test_note <chr>,
#> #   delta_cfi <dbl>, delta_tli <dbl>, delta_rmsea <dbl>, delta_srmr <dbl>,
#> #   delta_aic <dbl>, delta_bic <dbl>, ic_available <lgl>, ic_note <chr>,
#> #   interpretation <chr>
# }
```
