# Revision lineage --------------------------------------------------------------

revise_parent_run <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    cache <<- nomo_run(
      data = nomo_demo_continuous,
      scales = list(A = paste0("a", 1:5), B = paste0("b", 1:5)),
      settings = list(factors = list(criterion_set = "minimal", n_iter = 10L, seed = 2026L)),
      decisions = list(
        factor_count = c(A = 1, B = 1),
        cfa_model = list(
          value = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
          rationale = "Prespecified two-construct measurement model."
        )
      )
    )
    cache
  }
})

revised_residual_model <- paste(
  "A =~ a1 + a2 + a3 + a4 + a5",
  "B =~ b1 + b2 + b3 + b4 + b5",
  "a1 ~~ a2",
  sep = "\n"
)


test_that("a model revision records lineage, rationale, and a parent comparison", {
  skip_on_cran()

  parent <- revise_parent_run()
  revised <- nomo_revise(
    parent,
    cfa_model = revised_residual_model,
    rationale = "Item wording suggests a1 and a2 share method variance beyond the common factor.",
    origin = "post_hoc"
  )

  expect_s3_class(revised, "nomo_run")
  expect_identical(revised$status, "paused")
  expect_identical(revised$next_stage, "measurement_review")

  lineage <- nomo_table(revised, "lineage")
  expect_identical(nrow(lineage), 1L)
  expect_identical(lineage$revision, 1L)
  expect_identical(lineage$change_type, "model")
  expect_identical(lineage$origin, "post_hoc")
  expect_identical(lineage$items_removed, "")
  expect_match(lineage$rationale, "method variance")
  expect_match(lineage$parent_model, "A =~ a1", fixed = TRUE)
  expect_match(lineage$revised_model, "a1 ~~ a2", fixed = TRUE)

  expect_s3_class(revised$revision_comparison, "nomo_compare")
  cmp <- revised$revision_comparison$comparisons
  expect_identical(cmp$model, "revised")
  expect_identical(cmp$reference, "parent")
  expect_true(cmp$test_available)
  expect_match(lineage$comparison, "chi-square difference")

  expect_identical(revised$parent_summary$cfa_model, parent$decisions$cfa_model$value)
  expect_null(parent$lineage)
  expect_identical(nrow(nomo_table(parent, "lineage")), 0L)
})


test_that("revision decisions and holdout advice are recorded in the decision log", {
  skip_on_cran()

  parent <- revise_parent_run()
  revised <- nomo_revise(
    parent,
    cfa_model = revised_residual_model,
    rationale = "Residual diagnostics and item content.",
    origin = "post_hoc"
  )

  log <- revised$decision_log
  revision_rows <- log[log$id == "revision", , drop = FALSE]
  expect_identical(nrow(revision_rows), 1L)
  expect_match(revision_rows$consequence, "post hoc")
  expect_match(revision_rows$consequence, "independent data")
  expect_match(revision_rows$decision, "revised measurement model")
  expect_identical(revision_rows$source, "researcher_decision")
  expect_true("revision_comparison" %in% log$id)

  planned <- nomo_revise(
    parent,
    cfa_model = revised_residual_model,
    rationale = "The correlated residual was predicted before data collection.",
    origin = "a_priori",
    compare = FALSE
  )
  planned_row <- planned$decision_log[planned$decision_log$id == "revision", , drop = FALSE]
  expect_match(planned_row$consequence, "prespecified")
  expect_null(planned$revision_comparison)
  expect_match(nomo_table(planned, "lineage")$comparison, "not requested")
})


test_that("item revisions change the scales and are compared descriptively", {
  skip_on_cran()

  parent <- revise_parent_run()
  revised <- nomo_revise(
    parent,
    items = list(B = paste0("b", 1:4)),
    cfa_model = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4",
    rationale = "b5 measures content already covered and loads weakly.",
    origin = "post_hoc"
  )

  expect_identical(revised$scales$B, paste0("b", 1:4))
  expect_identical(revised$scales$A, paste0("a", 1:5))

  lineage <- nomo_table(revised, "lineage")
  expect_identical(lineage$change_type, "model_and_items")
  expect_identical(lineage$items_removed, "b5")
  expect_identical(lineage$items_added, "")

  cmp <- revised$revision_comparison$comparisons
  expect_identical(cmp$relation, "different_variables")
  expect_false(cmp$test_available)
  expect_match(lineage$comparison, "different observed variables")
})


test_that("revisions chain and keep every earlier revision", {
  skip_on_cran()

  parent <- revise_parent_run()
  first <- nomo_revise(
    parent,
    cfa_model = revised_residual_model,
    rationale = "First revision.",
    compare = FALSE
  )
  second <- nomo_revise(
    first,
    cfa_model = paste(revised_residual_model, "b1 ~~ b2", sep = "\n"),
    rationale = "Second revision.",
    compare = FALSE
  )

  lineage <- nomo_table(second, "lineage")
  expect_identical(nrow(lineage), 2L)
  expect_identical(lineage$revision, c(1L, 2L))
  expect_identical(lineage$rationale, c("First revision.", "Second revision."))
  expect_identical(nrow(nomo_table(first, "lineage")), 1L)
})


test_that("the factor-count decision is inherited unless it is supplied", {
  skip_on_cran()

  parent <- revise_parent_run()
  revised <- nomo_revise(
    parent,
    cfa_model = revised_residual_model,
    rationale = "Inheritance.",
    compare = FALSE
  )
  expect_identical(revised$decisions$factor_count$value, parent$decisions$factor_count$value)
  expect_match(revised$decisions$factor_count$rationale, "Inherited from the parent workflow")

  supplied <- nomo_revise(
    parent,
    cfa_model = revised_residual_model,
    rationale = "Explicit factor counts.",
    decisions = list(
      factor_count = list(value = c(A = 1, B = 1), rationale = "Re-examined for the revision.")
    ),
    compare = FALSE
  )
  expect_match(supplied$decisions$factor_count$rationale, "Re-examined")
})


test_that("revisions refuse unusable inputs with explanations", {
  skip_on_cran()

  parent <- revise_parent_run()

  expect_error(nomo_revise(list(), cfa_model = "A =~ a1", rationale = "x"), "nomo_run")
  expect_error(nomo_revise(parent, cfa_model = revised_residual_model), "rationale")
  expect_error(
    nomo_revise(parent, cfa_model = revised_residual_model, rationale = "  "),
    "rationale"
  )
  expect_error(nomo_revise(parent, rationale = "No change requested."), "must change")
  expect_error(
    nomo_revise(parent, cfa_model = parent$decisions$cfa_model$value, rationale = "Same model."),
    "identical to its parent"
  )
  expect_error(
    nomo_revise(parent, items = list(Missing = "a1"), rationale = "Unknown scale."),
    "Unknown scale"
  )
  expect_error(
    nomo_revise(parent, items = list(B = character()), rationale = "Empty items."),
    "item-column names"
  )
  expect_error(
    nomo_revise(parent, cfa_model = revised_residual_model, rationale = "x", compare = NA),
    "compare"
  )

  paused <- nomo_run(
    data = nomo_demo_continuous,
    scales = list(A = paste0("a", 1:5)),
    settings = list(factors = list(criterion_set = "minimal", n_iter = 10L, seed = 2026L))
  )
  expect_error(
    nomo_revise(paused, cfa_model = "A =~ a1 + a2 + a3", rationale = "Too early."),
    "fitted measurement model"
  )
})


test_that("revision lineage reaches presentation and reporting surfaces", {
  skip_on_cran()

  parent <- revise_parent_run()
  revised <- nomo_revise(
    parent,
    cfa_model = revised_residual_model,
    rationale = "Presentation check.",
    compare = FALSE
  )

  expect_output(print(revised), "Revisions: 1")
  expect_s3_class(nomo_table(revised, "lineage"), "tbl_df")

  report_lineage <- nomologR:::nomo_report_lineage(revised)
  expect_identical(nrow(report_lineage), 1L)
  expect_true("rationale" %in% names(report_lineage))
  expect_identical(nrow(nomologR:::nomo_report_lineage(parent)), 0L)

  deviations <- nomologR:::nomo_report_deviations(revised)
  expect_true(any(grepl("revision", deviations$type, ignore.case = TRUE)))
})


test_that("an item revision must agree with the measurement model", {
  skip_on_cran()
  parent <- revise_parent_run()

  # Keeping the parent's model would still fit the dropped item in the CFA
  # while the lineage recorded it as removed.
  expect_error(
    nomo_revise(parent, items = list(B = paste0("b", 1:4)), rationale = "Drop b5."),
    "still includes b5, which this revision removes"
  )

  # An added item must appear in the model it will be fitted with. This parent
  # screens and models four B items, so b5 is absent from its model.
  four_b <- nomo_run(
    data = nomo_demo_continuous,
    scales = list(A = paste0("a", 1:5), B = paste0("b", 1:4)),
    settings = list(factors = list(criterion_set = "minimal", n_iter = 10L, seed = 2026L)),
    decisions = list(
      factor_count = c(A = 1, B = 1),
      cfa_model = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4"
    )
  )
  expect_error(
    nomo_revise(four_b, items = list(B = paste0("b", 1:5)), rationale = "Restore b5."),
    "adds b5, but the measurement model does not include it"
  )

  # The check is on presence in the model, not on factor assignment, which the
  # researcher owns: an item added to B that the model already uses elsewhere
  # is not refused.
  expect_s3_class(
    nomo_revise(
      parent,
      items = list(B = c(paste0("b", 1:5), "a5")),
      cfa_model = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5 + a5",
      rationale = "a5 cross-loads on B in the exploratory solution."
    ),
    "nomo_run"
  )
})


test_that("an item-only revision is accepted when the model already agrees", {
  skip_on_cran()

  # The parent screens five B items but its CFA already uses four.
  parent <- nomo_run(
    data = nomo_demo_continuous,
    scales = list(A = paste0("a", 1:5), B = paste0("b", 1:5)),
    settings = list(factors = list(criterion_set = "minimal", n_iter = 10L, seed = 2026L)),
    decisions = list(
      factor_count = c(A = 1, B = 1),
      cfa_model = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4"
    )
  )
  revised <- nomo_revise(
    parent,
    items = list(B = paste0("b", 1:4)),
    rationale = "b5 was already excluded from the confirmatory model."
  )

  lineage <- nomo_table(revised, "lineage")
  expect_identical(lineage$change_type, "items")
  expect_identical(lineage$items_removed, "b5")
})


test_that("an added item is recorded in the lineage", {
  skip_on_cran()
  parent <- revise_parent_run()

  revised <- nomo_revise(
    parent,
    items = list(B = c(paste0("b", 1:5), "a5")),
    cfa_model = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5 + a5",
    rationale = "a5 cross-loads on B in the exploratory solution.",
    decisions = list(factor_count = c(A = 1, B = 1))
  )
  lineage <- nomo_table(revised, "lineage")
  expect_identical(lineage$items_added, "a5")
})


test_that("parents that cannot be revised are refused", {
  skip_on_cran()
  parent <- revise_parent_run()

  expect_error(
    nomo_revise(list(), cfa_model = revised_residual_model, rationale = "x"),
    "must be a `nomo_run` object"
  )

  blocked <- parent
  blocked$status <- "blocked"
  expect_error(
    nomo_revise(blocked, cfa_model = revised_residual_model, rationale = "x"),
    "blocked by a component error"
  )

  expect_error(
    nomo_revise(parent, items = list(), rationale = "x"),
    "non-empty named list of item vectors"
  )
  expect_error(
    nomo_revise(parent, cfa_model = revised_residual_model, rationale = "x", decisions = "x"),
    "must be a named list of workflow decisions"
  )
})


test_that("an inherited factor count is checked against the revised items", {
  skip_on_cran()
  parent <- revise_parent_run()

  expect_error(
    nomo_revise(
      parent,
      items = list(B = "b1"),
      cfa_model = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1",
      rationale = "Reduce B to a single item."
    ),
    "not smaller than the revised item count for B"
  )
})


test_that("inherited factor counts are guarded against unusual parent state", {
  # nomo_run() stores factor counts as named numeric vectors and refuses a model
  # decision before a count exists, so these guards protect against state that
  # the workflow does not normally produce.
  inherited <- nomologR:::nomo_revise_inherited_factor_counts
  expect_null(inherited(list(decisions = list()), list(A = "a1")))
  expect_null(inherited(list(decisions = list(factor_count = list(value = 1L))), list(A = "a1")))
  expect_null(inherited(list(decisions = list(factor_count = list(value = c(Z = 1)))), list(A = "a1")))
})


test_that("a revised workflow without a fitted model records why it was not compared", {
  skip_on_cran()
  parent <- revise_parent_run()

  # Simulate a child workflow that stopped before fitting its measurement model.
  local_mocked_bindings(nomo_run = function(...) {
    child <- parent
    child$results$cfa <- NULL
    child
  })
  revised <- nomo_revise(
    parent,
    cfa_model = revised_residual_model,
    rationale = "Shared wording between a1 and a2."
  )
  expect_null(revised$revision_comparison)
  expect_match(
    nomo_table(revised, "lineage")$comparison,
    "did not produce a fitted measurement model"
  )
})


test_that("a failed comparison is recorded rather than stopping the revision", {
  skip_on_cran()
  parent <- revise_parent_run()

  local_mocked_bindings(
    nomo_compare = function(...) stop("simulated comparison failure")
  )
  revised <- nomo_revise(
    parent,
    cfa_model = revised_residual_model,
    rationale = "Shared wording between a1 and a2."
  )
  expect_null(revised$revision_comparison)
  expect_match(
    nomo_table(revised, "lineage")$comparison,
    "Model comparison unavailable: simulated comparison failure"
  )

  expect_identical(
    nomologR:::nomo_revise_comparison_summary(list(comparisons = tibble::tibble()), "kept"),
    "kept"
  )
})


test_that("research-mode printing shows the revision count", {
  skip_on_cran()
  parent <- revise_parent_run()
  parent$mode <- "research"
  revised <- nomo_revise(parent, cfa_model = revised_residual_model, rationale = "Shared wording.")
  expect_output(print(revised), "Revisions: 1 (post-hoc)", fixed = TRUE)
})


test_that("revision guards handle unparseable models and missing rationales", {
  # An unparseable model is left for nomo_run() to reject with lavaan's message.
  expect_null(nomologR:::nomo_revise_check_model_items(
    "this is not lavaan syntax ~~~ =~",
    list(A = "a1"),
    list(removed = "a2", added = character())
  ))

  # A stored factor-count decision without a rationale still inherits cleanly.
  inherited <- nomologR:::nomo_revise_inherited_factor_counts(
    list(decisions = list(factor_count = list(value = c(A = 1)))),
    list(A = c("a1", "a2", "a3"))
  )
  expect_equal(inherited$value, c(A = 1))
  expect_match(inherited$rationale, "Inherited from the parent workflow")
})
