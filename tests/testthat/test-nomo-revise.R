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
  # The package's one wording for a difference test (#144).
  expect_match(lineage$comparison, "^Delta chi-square\\(1\\) = [0-9.]+, p ")

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
  # A prespecified revision was not motivated by the sample (#145).
  expect_no_match(planned_row$consequence, "that motivated it", fixed = TRUE)
  expect_match(planned_row$consequence, "evaluated on the same sample as the parent model",
               fixed = TRUE)
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
  local_mocked_bindings(nomo_run_fresh = function(...) {
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
  # "post hoc" takes no hyphen (#144).
  expect_output(print(revised), "Revisions: 1 (post hoc)", fixed = TRUE)
  expect_output(print(revised), "nomo_table(x, \"lineage\") for the revisions.", fixed = TRUE)
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
  expect_identical(inherited$rationale, c(A = "Inherited from the parent workflow."))
})


test_that("each scale inherits its own factor-count rationale (#145)", {
  inherit <- function(rationale) {
    nomologR:::nomo_revise_inherited_factor_counts(
      list(decisions = list(factor_count = list(value = c(A = 1, B = 1), rationale = rationale))),
      list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3"))
    )$rationale
  }
  expect_identical(
    inherit(c(B = "B was written as one facet.", A = "A is one construct.")),
    c(A = "Inherited from the parent workflow. A is one construct.",
      B = "Inherited from the parent workflow. B was written as one facet.")
  )
  # A rationale shared by every scale is not repeated.
  expect_identical(inherit("theory"),
                   c(A = "Inherited from the parent workflow. theory",
                     B = "Inherited from the parent workflow. theory"))
})


test_that("a revision's model has one source and its decisions are checked (#145)", {
  skip_on_cran()
  parent <- revise_parent_run()
  expect_error(
    nomo_revise(parent, cfa_model = revised_residual_model, rationale = "r",
                decisions = list(cfa_model = paste(revised_residual_model, "b1 ~~ b2", sep = "\n"))),
    "Give the revised measurement model in `cfa_model`, not in `decisions`", fixed = TRUE
  )
  # An unnamed decision is refused rather than dropped.
  expect_error(
    nomo_revise(parent, cfa_model = revised_residual_model, rationale = "r",
                decisions = list(c(A = 2, B = 1))),
    "`decisions` must use unique non-empty names.", fixed = TRUE
  )
  expect_error(
    nomo_revise(parent, cfa_model = revised_residual_model, rationale = "r",
                decisions = list(factor_counts = c(A = 2, B = 1))),
    "Unsupported workflow decision name: factor_counts.", fixed = TRUE
  )
})


test_that("a supplied factor count replaces an inherited one the revised items cannot hold (#145)", {
  skip_on_cran()
  pool <- nomo_run(
    data = nomo_demo_continuous,
    scales = list(Pool = c(paste0("a", 1:5), paste0("b", 1:5))),
    settings = list(factors = list(criterion_set = "minimal", n_iter = 10L, seed = 2026L)),
    decisions = list(
      factor_count = c(Pool = 3),
      cfa_model = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
    )
  )
  short <- list(Pool = c("a1", "a2", "a3"))
  expect_error(
    nomo_revise(pool, items = short, cfa_model = "A =~ a1 + a2 + a3", rationale = "Short form."),
    "not smaller than the revised item count for Pool", fixed = TRUE
  )
  revised <- nomo_revise(
    pool, items = short, cfa_model = "A =~ a1 + a2 + a3", rationale = "Short form.",
    decisions = list(factor_count = list(value = c(Pool = 1), rationale = "One construct.")),
    compare = FALSE
  )
  expect_identical(revised$next_stage, "measurement_review")
  expect_identical(revised$decisions$factor_count$value, c(Pool = 1L))
  log <- revised$decision_log
  expect_identical(log$rationale[log$id == "factor_count:Pool"], "One construct.")
  expect_identical(
    log$observation[log$id == "revision"],
    paste("Revision 1 of the guided workflow: the measurement model was revised; items",
          "removed: a4, a5, b1, b2, b3, b4, b5.")
  )
})


test_that("the holdout advice follows the revision's origin and sample design (#145)", {
  note <- nomologR:::nomo_revise_holdout_note
  expect_match(note("same_sample", "post_hoc"), "the same sample that motivated it", fixed = TRUE)
  expect_match(note("same_sample", "a_priori"), "the same sample as the parent model", fixed = TRUE)
  expect_match(note("calibration_validation", "post_hoc"), "no longer independent", fixed = TRUE)
  expect_match(note("calibration_validation", "a_priori"),
               "evaluated on the same validation rows", fixed = TRUE)
  expect_identical(note("same_sample"), note("same_sample", "post_hoc"))
})


# Revising a run whose scales came from content review (#145) ----------------

revise_handoff_model <- function(ef = c("EF1", "EF2", "EF3", "EF4", "EF6")) {
  paste0(
    "EF =~ ", paste(ef, collapse = " + "),
    "\nTF =~ TF1 + TF2 + TF3 + TF4 + TF6"
  )
}

revise_handoff_parent <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    h <- readRDS(test_path("fixtures", "contentvalidR", "handoff-walkthrough-sort-v0.10.1.rds"))
    # Content review declares the held-back EF5 reverse-keyed as well, so a
    # revision that reinstates it has keying to keep.
    h$item_evidence$keying[h$item_evidence$item == "EF5"] <- -1
    set.seed(145)
    n <- 300
    ef <- stats::rnorm(n)
    tf <- .5 * ef + stats::rnorm(n, sd = .85)
    items <- c(paste0("EF", 1:6), paste0("TF", 1:6))
    data <- as.data.frame(stats::setNames(lapply(items, function(i) {
      f <- if (startsWith(i, "EF")) ef else tf
      as.numeric(pmin(5, pmax(1, round(3 + f + stats::rnorm(n, sd = .9)))))
    }), items))
    # Answered as worded, so the reverse-keyed items run the other way.
    data[c("EF2", "EF5", "TF2")] <- 6 - data[c("EF2", "EF5", "TF2")]
    cache <<- nomo_run(
      data,
      scales = h,
      settings = list(screen = list(effort = TRUE), factors = list(seed = 145, n_iter = 10L)),
      decisions = list(factor_count = c(EF = 1, TF = 1), cfa_model = revise_handoff_model())
    )
    cache
  }
})

revise_log_row <- function(run, id) run$decision_log[run$decision_log$id == id, ]

revise_keyed_note <- function(run, scope, item) {
  log <- run$results$screen[[scope]]$decision_log
  any(grepl("declared reverse-keyed", log$observation[log$object == item], fixed = TRUE))
}


test_that("a revision keeps the content-review handoff, its keying, and its record", {
  skip_on_cran()
  parent <- revise_handoff_parent()
  expect_identical(parent$status, "paused")

  child <- nomo_revise(
    parent,
    cfa_model = paste(revise_handoff_model(), "EF1 ~~ EF3", sep = "\n"),
    rationale = "EF1 and EF3 share wording about deadlines."
  )

  expect_identical(child$handoff, parent$handoff)
  log <- child$decision_log
  expect_true(all(c("content_review", "held_back:EF5", "held_back:TF5") %in% log$id))
  definitions <- log[startsWith(log$id, "scale_definition:"), ]
  expect_true(all(definitions$source == "content_review"))
  expect_match(definitions$observation[[1L]], "carried from content review", fixed = TRUE)
  expect_false(any(startsWith(log$id, "reinstated:") | startsWith(log$id, "removed:")))
  # With nothing reinstated or removed, content review's own wording stands.
  expect_identical(revise_log_row(child, "content_review")$consequence,
                   revise_log_row(parent, "content_review")$consequence)
  expect_match(revise_log_row(child, "content_review")$consequence,
               "Held-back items are not analyzed, and nomologR", fixed = TRUE)
  expect_identical(revise_log_row(child, "held_back:EF5")$reason,
                   "Only items carried by content review are analyzed.")

  # The careless-responding screen recodes as content review declared.
  effort <- child$results$effort$effort_settings
  expect_identical(effort$reverse, c("EF2", "TF2"))
  expect_identical(effort$scale_range, c(1, 5))
  expect_identical(revise_log_row(child, "careless_responding")$source, "content_review")

  # The item audit still explains EF2's negative item-rest correlation.
  expect_true(revise_keyed_note(child, "EF", "EF2"))

  review <- nomologR:::nomo_report_content_review(child)
  expect_match(review$summary, "only carried items were analyzed.", fixed = TRUE)
})


test_that("a revision that departs from content review records each departure", {
  skip_on_cran()
  parent <- revise_handoff_parent()
  why <- "EF5 restores the effort-after-setbacks content; EF2 repeats EF1."
  child <- nomo_revise(
    parent,
    items = list(EF = c("EF1", "EF3", "EF4", "EF5", "EF6")),
    cfa_model = revise_handoff_model(c("EF1", "EF3", "EF4", "EF5", "EF6")),
    rationale = why
  )
  expect_identical(child$status, "paused")

  # Declared keying follows the items: EF2 is gone, and the reinstated EF5 is
  # declared reverse-keyed.
  expect_identical(child$results$effort$effort_settings$reverse, c("EF5", "TF2"))
  expect_true(revise_keyed_note(child, "EF", "EF5"))

  ef <- revise_log_row(child, "scale_definition:EF")
  expect_identical(ef$source, "researcher_decision")
  expect_match(
    ef$observation,
    paste("Content review in contentvalidR 0.10.1 carried EF1, EF2, EF3, EF4, EF6 for it;",
          "researcher revisions added EF5 and removed EF2."),
    fixed = TRUE
  )
  expect_identical(revise_log_row(child, "scale_definition:TF")$source, "content_review")

  reinstated <- revise_log_row(child, "reinstated:EF5")
  expect_identical(reinstated$scope, "EF")
  expect_identical(reinstated$source, "researcher_decision")
  expect_identical(reinstated$rationale, why)
  expect_match(reinstated$observation,
               "EF5, which content review held back, is analyzed in this run: revision 1 reinstated it.",
               fixed = TRUE)
  expect_identical(revise_log_row(child, "held_back:EF5")$consequence,
                   "Revision 1 reinstated it, so it is analyzed in this run.")
  expect_match(revise_log_row(child, "held_back:EF5")$reason,
               "unless a researcher revision reinstates one.", fixed = TRUE)
  expect_identical(revise_log_row(child, "held_back:TF5")$consequence,
                   "It is not screened, modeled, or scored in this run.")
  expect_identical(revise_log_row(child, "held_back:TF5")$reason,
                   "Only items carried by content review are analyzed.")
  # The content-review row no longer says held-back items are never analyzed.
  content <- revise_log_row(child, "content_review")$consequence
  expect_match(content,
               "Held-back items are not analyzed unless a researcher revision reinstates them",
               fixed = TRUE)
  expect_match(content, "each such change has its own reinstated: or removed: row.",
               fixed = TRUE)
  expect_match(content, "nomologR neither reinstates nor drops an item", fixed = TRUE)

  removed <- revise_log_row(child, "removed:EF2")
  expect_identical(removed$scope, "EF")
  expect_identical(removed$rationale, why)
  expect_match(removed$observation, "revision 1 removed it.", fixed = TRUE)

  review <- nomologR:::nomo_report_content_review(child)
  expect_match(review$summary, "a researcher revision changed which items were analyzed",
               fixed = TRUE)

  # A later revision keeps each departure with the rationale of the revision
  # that made it.
  second <- nomo_revise(
    child,
    cfa_model = paste(revise_handoff_model(c("EF1", "EF3", "EF4", "EF5", "EF6")),
                      "EF1 ~~ EF3", sep = "\n"),
    rationale = "EF1 and EF3 share wording about deadlines.",
    compare = FALSE
  )
  again <- revise_log_row(second, "reinstated:EF5")
  expect_identical(again$rationale, why)
  expect_match(again$observation, "revision 1 reinstated it", fixed = TRUE)
  expect_identical(revise_log_row(second, "removed:EF2")$rationale, why)
  expect_identical(revise_log_row(second, "content_review")$consequence, content)
})


test_that("reverse keying set for an item a revision removes is dropped with it", {
  skip_on_cran()
  parent <- revise_parent_run()
  parent$settings$screen <- list(reverse = c("a2", "b5"), scale_range = c(1, 7))
  child <- nomo_revise(
    parent,
    items = list(B = paste0("b", 1:4)),
    cfa_model = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4",
    rationale = "b5 measures content already covered and loads weakly.",
    compare = FALSE
  )
  expect_identical(child$settings$screen$reverse, "a2")
  expect_identical(child$settings$screen$scale_range, c(1, 7))
  expect_identical(child$status, "paused")
})
