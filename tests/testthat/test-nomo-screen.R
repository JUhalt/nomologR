test_that("nomo_screen validates inputs", {
  expect_error(nomo_screen(matrix(1:4, ncol = 2)), "data frame")
  expect_error(nomo_screen(data.frame()), "at least one row")
  expect_error(nomo_screen(data.frame(x = numeric())), "at least one row")

  dat <- data.frame(x = 1:4, y = 4:1)

  expect_error(nomo_screen(dat, items = character()), "non-empty")
  expect_error(nomo_screen(dat, items = c("x", "x")), "duplicate")
  expect_error(nomo_screen(dat, items = "z"), "Unknown item column")
  expect_error(nomo_screen(dat, items = NA_character_), "missing or empty")
  expect_error(nomo_screen(dat, guidance = "teaching"), "guidance")
})


test_that("nomo_screen returns a stable structured object without modifying data", {
  dat <- data.frame(
    item1 = c(1, 2, 3, 4, 5),
    item2 = c(1, 2, NA, 4, 5),
    item3 = c(3, 3, 3, 3, 3)
  )
  original <- dat

  out <- nomo_screen(dat, items = c("item1", "item2", "item3"))

  expect_s3_class(out, "nomo_screen")
  expect_identical(dat, original)
  expect_identical(out$items, c("item1", "item2", "item3"))
  expect_equal(out$n_cases, 5)
  expect_named(
    out,
    c(
      "call", "n_cases", "items", "item_summary",
      "response_distribution", "case_summary",
      "relationship_summary", "inter_item_correlations",
      "relationship_method", "decision_log", "guidance"
    )
  )

  expect_equal(nrow(out$item_summary), 3)
  expect_equal(nrow(out$case_summary), 5)
  expect_true(all(c("item", "item_type", "pct_missing", "constant", "all_missing") %in%
    names(out$item_summary)))
})


test_that("item types are classified conservatively", {
  dat <- data.frame(
    binary_numeric = c(0, 1, 0, 1, 1),
    binary_logical = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    discrete = c(1, 2, 3, 4, 5),
    continuous = c(0.13, 1.27, 2.51, 3.92, 5.48),
    ordered_item = ordered(c("low", "mid", "high", "mid", "low"),
      levels = c("low", "mid", "high")
    ),
    nominal = factor(c("a", "b", "c", "a", "b")),
    text = c("a", "b", "c", "d", "e"),
    sparse_ordered = ordered(
      c("low", "high", "low", "high", NA),
      levels = c("low", "mid", "high")
    ),
    sparse_nominal = factor(
      c("a", "c", "a", "c", NA),
      levels = c("a", "b", "c")
    ),
    empty = rep(NA_real_, 5)
  )

  out <- nomo_screen(dat)

  observed_types <- setNames(out$item_summary$item_type, out$item_summary$item)

  expect_identical(observed_types[["binary_numeric"]], "binary")
  expect_identical(observed_types[["binary_logical"]], "binary")
  expect_identical(observed_types[["discrete"]], "numeric_discrete")
  expect_identical(observed_types[["continuous"]], "numeric_continuous")
  expect_identical(observed_types[["ordered_item"]], "ordered")
  expect_identical(observed_types[["nominal"]], "nominal")
  expect_identical(observed_types[["sparse_ordered"]], "ordered")
  expect_identical(observed_types[["sparse_nominal"]], "nominal")
  expect_identical(observed_types[["text"]], "text")
  expect_identical(observed_types[["empty"]], "empty")
})


test_that("item summaries describe missingness and zero variance", {
  dat <- data.frame(
    varying = c(1, 2, 3, 4, NA),
    constant = c(2, 2, 2, 2, 2),
    empty = rep(NA_real_, 5)
  )

  out <- nomo_screen(dat)

  varying <- out$item_summary[out$item_summary$item == "varying", ]
  constant <- out$item_summary[out$item_summary$item == "constant", ]
  empty <- out$item_summary[out$item_summary$item == "empty", ]

  expect_equal(varying$n_missing, 1)
  expect_equal(varying$pct_missing, 0.20)
  expect_false(varying$constant)
  expect_false(varying$all_missing)

  expect_true(constant$constant)
  expect_equal(constant$n_unique, 1)
  expect_equal(constant$sd, 0)

  expect_true(empty$all_missing)
  expect_equal(empty$n_observed, 0)
  expect_equal(empty$pct_missing, 1)
})


test_that("response distributions retain unused factor levels and missingness", {
  dat <- data.frame(
    item = ordered(
      c("1", "2", "2", NA, "1"),
      levels = c("1", "2", "3")
    )
  )

  out <- nomo_screen(dat, items = "item")
  dist <- out$response_distribution

  level_three <- dist[!dist$missing & dist$response == "3", ]
  missing <- dist[dist$missing, ]

  expect_equal(level_three$n, 0)
  expect_equal(level_three$proportion_total, 0)
  expect_equal(nrow(missing), 1)
  expect_equal(missing$n, 1)
  expect_equal(missing$proportion_total, 0.20)
  expect_true(is.na(missing$proportion_observed))
})


test_that("case summary reports completeness without deleting cases", {
  dat <- data.frame(
    x = c(1, NA, NA),
    y = c(2, 3, NA)
  )

  out <- nomo_screen(dat, items = c("x", "y"))

  expect_identical(out$case_summary$n_missing, c(0L, 1L, 2L))
  expect_equal(out$case_summary$pct_missing, c(0, 0.5, 1))
  expect_identical(out$case_summary$complete, c(TRUE, FALSE, FALSE))
  expect_identical(out$case_summary$all_missing, c(FALSE, FALSE, TRUE))
})


test_that("decision log flags hard data conditions without automatic deletion", {
  dat <- data.frame(
    varying = c(1, 2, 3, 4, 5),
    constant = rep(1, 5),
    empty = rep(NA_real_, 5),
    label = c("a", "b", "c", "a", "b")
  )

  out <- nomo_screen(dat)

  expect_true(any(out$decision_log$metric == "constant"))
  expect_true(any(out$decision_log$metric == "all_missing"))
  expect_true(any(out$decision_log$metric == "item_type"))
  expect_true(any(out$decision_log$severity == "concern"))

  combined_text <- paste(
    out$decision_log$observation,
    out$decision_log$recommendation,
    collapse = " "
  )

  expect_true(grepl("Do not delete", combined_text, ignore.case = TRUE))
})


test_that("items = NULL is explicitly documented in the decision log", {
  dat <- data.frame(id = 1:4, item = c(1, 2, 3, 4))

  out <- nomo_screen(dat)

  expect_true(any(out$decision_log$metric == "all_columns_selected"))
  expect_match(
    out$decision_log$recommendation[
      out$decision_log$metric == "all_columns_selected"
    ],
    "identifiers"
  )
})


test_that("print method reports audit status and returns invisibly", {
  dat <- data.frame(
    x = c(1, 2, 3),
    y = c(1, NA, 3)
  )
  out <- nomo_screen(dat)

  expect_output(returned <- print(out), "<nomo_screen>")
  expect_s3_class(returned, "nomo_screen")
  expect_output(print(out), "No rows or items were removed")
})

# ---- recovered from hygiene consolidation: test-nomo-screen.R ----
# ---- consolidated from test-nomo-screen-m1b.R ----
test_that("relationship diagnostics detect a reverse-key candidate", {
  dat <- data.frame(
    item1 = c(1, 2, 3, 4, 5, 6),
    item2 = c(1, 2, 2, 4, 5, 6),
    reverse_candidate = c(6, 5, 4, 3, 2, 1)
  )

  out <- nomo_screen(dat)

  expect_equal(nrow(out$inter_item_correlations), 3)
  expect_true(all(out$relationship_summary$relationship_eligible))

  reverse_row <- out$relationship_summary[
    out$relationship_summary$item == "reverse_candidate",
    ,
    drop = FALSE
  ]

  expect_lt(reverse_row$corrected_item_rest_r, 0)

  log_row <- out$decision_log[
    out$decision_log$object == "reverse_candidate" &
      out$decision_log$metric == "corrected_item_rest",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(log_row), 1)
  expect_identical(log_row$severity, "review")
  expect_match(log_row$recommendation, "reverse", ignore.case = TRUE)
  expect_match(log_row$recommendation, "Do not", ignore.case = TRUE)
})


test_that("factor labels are not silently scored for relationship diagnostics", {
  dat <- data.frame(
    item1 = 1:6,
    item2 = c(1, 2, 3, 3, 5, 6),
    ordered_item = ordered(
      c("low", "mid", "high", "mid", "low", "high"),
      levels = c("low", "mid", "high")
    )
  )

  out <- nomo_screen(dat)

  row <- out$relationship_summary[
    out$relationship_summary$item == "ordered_item",
    ,
    drop = FALSE
  ]

  expect_false(row$relationship_eligible)
  expect_identical(
    row$relationship_reason,
    "not_explicitly_scored_numeric"
  )

  expect_true(
    any(out$decision_log$metric == "unscored_items_skipped")
  )
})


test_that("item-rest and inter-item diagnostics report their sample sizes", {
  dat <- data.frame(
    item1 = c(1, 2, 3, 4, 5, 6),
    item2 = c(1, 2, 3, 4, NA, 6),
    item3 = c(1, 2, 3, 4, 5, 6)
  )

  out <- nomo_screen(dat)

  expect_true(
    all(out$relationship_summary$item_rest_n == 5L)
  )

  pair_13 <- out$inter_item_correlations[
    out$inter_item_correlations$item1 == "item1" &
      out$inter_item_correlations$item2 == "item3",
    ,
    drop = FALSE
  ]

  pair_12 <- out$inter_item_correlations[
    out$inter_item_correlations$item1 == "item1" &
      out$inter_item_correlations$item2 == "item2",
    ,
    drop = FALSE
  ]

  expect_equal(pair_13$n_pair, 6L)
  expect_equal(pair_12$n_pair, 5L)
})


test_that("hard data problems are excluded from relationship diagnostics", {
  dat <- data.frame(
    item1 = 1:6,
    item2 = c(1, 2, 3, 3, 5, 6),
    constant = rep(2, 6),
    empty = rep(NA_real_, 6)
  )

  out <- nomo_screen(dat)
  rel <- out$relationship_summary

  expect_identical(
    rel$relationship_reason[rel$item == "constant"],
    "constant"
  )

  expect_identical(
    rel$relationship_reason[rel$item == "empty"],
    "all_missing"
  )
})

# ---- consolidated from test-nomo-screen-m1b2.R ----
test_that("response concentration and ordered floor/ceiling are summarized", {
  dat <- data.frame(
    ordered_item = ordered(
      c(rep("low", 8), rep("mid", 2)),
      levels = c("low", "mid", "high")
    ),
    numeric_item = c(rep(1, 8), 2, 3)
  )

  out <- nomo_screen(dat)

  ordered_row <- out$item_summary[
    out$item_summary$item == "ordered_item",
    ,
    drop = FALSE
  ]

  expect_equal(ordered_row$floor_prop, 0.80)
  expect_equal(ordered_row$ceiling_prop, 0)
  expect_equal(ordered_row$mode_prop, 0.80)

  numeric_row <- out$item_summary[
    out$item_summary$item == "numeric_item",
    ,
    drop = FALSE
  ]

  expect_equal(numeric_row$floor_prop, 0.80)
  expect_equal(numeric_row$ceiling_prop, 0.10)

  expect_true(any(out$decision_log$metric == "floor_concentration"))
})


test_that("near-zero variance is flagged as a configurable screening heuristic", {
  dat <- data.frame(
    concentrated = c(rep(0, 96), rep(1, 4)),
    comparison = rep(c(0, 1), 50)
  )

  out <- nomo_screen(dat)

  row <- out$item_summary[
    out$item_summary$item == "concentrated",
    ,
    drop = FALSE
  ]

  expect_true(row$near_zero_variance)
  expect_equal(row$frequency_ratio, 24)
  expect_equal(row$percent_unique, 2)

  log_row <- out$decision_log[
    out$decision_log$object == "concentrated" &
      out$decision_log$metric == "near_zero_variance",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(log_row), 1)
  expect_identical(log_row$severity, "review")
  expect_match(log_row$reference, "not a psychometric law")
  expect_match(log_row$recommendation, "rather than an automatic", ignore.case = TRUE)
})


test_that("response-concentration reference can be changed through guidance", {
  dat <- data.frame(
    item1 = c(rep(1, 8), 2, 3),
    item2 = 1:10
  )

  guidance <- nomo_defaults()
  guidance$response_concentration_reference <- 0.95

  out <- nomo_screen(dat, guidance = guidance)

  expect_false(any(
    out$decision_log$object == "item1" &
      out$decision_log$metric %in% c(
        "response_concentration",
        "floor_concentration",
        "ceiling_concentration"
      )
  ))
})


test_that("continuous-like indicators receive descriptive shape summaries", {
  dat <- data.frame(
    continuous = c(-2.1, -1.2, -0.4, 0, 0.4, 1.2, 2.1),
    second = c(-1.8, -1, -0.3, 0.1, 0.5, 1.1, 1.9)
  )

  out <- nomo_screen(dat)

  row <- out$item_summary[
    out$item_summary$item == "continuous",
    ,
    drop = FALSE
  ]

  expect_true(is.finite(row$skewness))
  expect_true(is.finite(row$excess_kurtosis))
  expect_lt(abs(row$skewness), 0.01)
})


test_that("binary items do not receive misleading floor/ceiling summaries", {
  dat <- data.frame(
    binary = c(0, 0, 1, 1, 1, 0),
    comparison = c(0, 1, 0, 1, 0, 1)
  )

  out <- nomo_screen(dat)

  row <- out$item_summary[
    out$item_summary$item == "binary",
    ,
    drop = FALSE
  ]

  expect_true(is.na(row$floor_prop))
  expect_true(is.na(row$ceiling_prop))
})


test_that("M1B descriptive additions remain non-destructive", {
  dat <- data.frame(
    item1 = c(1, 1, 1, 1, 2, 3),
    item2 = c(1, 2, 3, 4, 5, 6)
  )
  original <- dat

  out <- nomo_screen(dat)

  expect_identical(dat, original)
  expect_s3_class(out, "nomo_screen")
  expect_true(all(c(
    "percent_unique",
    "frequency_ratio",
    "near_zero_variance",
    "floor_prop",
    "ceiling_prop",
    "skewness",
    "excess_kurtosis"
  ) %in% names(out$item_summary)))
})


test_that("print method includes the expanded screening status", {
  dat <- data.frame(
    item1 = c(rep(1, 8), 2, 3),
    item2 = 1:10
  )

  out <- nomo_screen(dat)

  expect_output(
    print(out),
    "Response concentration flags:"
  )
  expect_output(
    print(out),
    "Near-zero variance:"
  )
})

# ---- consolidated from test-nomo-screen-presentation.R ----
test_that("summary.nomo_screen integrates item-level evidence", {
  dat <- data.frame(
    item1 = 1:6,
    item2 = c(1, 2, 2, 4, 5, 6),
    reverse_candidate = 6:1
  )

  out <- nomo_screen(dat)
  s <- summary(out)

  expect_s3_class(s, "summary_nomo_screen")
  expect_true(all(c(
    "overview",
    "item_review",
    "decision_log",
    "relationship_method",
    "guidance"
  ) %in% names(s)))

  expect_true(all(c(
    "attention",
    "review_metrics",
    "n_unused_response_categories",
    "corrected_item_rest_r"
  ) %in% names(s$item_review)))

  reverse_row <- s$item_review[
    s$item_review$item == "reverse_candidate",
    ,
    drop = FALSE
  ]

  expect_identical(as.character(reverse_row$attention), "review")
  expect_match(reverse_row$review_metrics, "corrected_item_rest")
  expect_match(reverse_row$review_metrics, "negative_interitem_pairs")
})


test_that("unused declared ordered categories become integrated review evidence", {
  dat <- data.frame(
    ordered_item = ordered(
      c("low", "high", "low", "high", "low", "high"),
      levels = c("low", "mid", "high")
    ),
    item2 = 1:6
  )

  out <- nomo_screen(dat)
  s <- summary(out)

  row <- s$item_review[
    s$item_review$item == "ordered_item",
    ,
    drop = FALSE
  ]

  expect_equal(row$n_response_categories_listed, 3L)
  expect_equal(row$n_response_categories_used, 2L)
  expect_equal(row$n_unused_response_categories, 1L)
  expect_identical(as.character(row$attention), "review")
  expect_match(row$review_metrics, "unused_response_categories")
})


test_that("summary print is concise and non-prescriptive", {
  dat <- data.frame(
    item1 = 1:6,
    item2 = c(1, 2, 2, 4, 5, 6)
  )

  s <- summary(nomo_screen(dat))

  expect_output(returned <- print(s), "<nomo_screen summary>")
  expect_s3_class(returned, "summary_nomo_screen")
  expect_output(
    print(s),
    "not decisions to keep or delete an item"
  )
})


test_that("plot.nomo_screen returns ggplot objects for all display types", {
  dat <- data.frame(
    item1 = c(1, 2, 3, 4, 5, 6, NA),
    item2 = c(1, 2, 2, 4, 5, 6, 6),
    reverse_candidate = c(6, 5, 4, 3, 2, 1, 1)
  )

  out <- nomo_screen(dat)

  expect_s3_class(plot(out), "ggplot")
  expect_s3_class(plot(out, type = "item_rest"), "ggplot")
  expect_s3_class(plot(out, type = "interitem"), "ggplot")
  expect_s3_class(plot(out, type = "responses"), "ggplot")
  expect_s3_class(plot(out, type = "missingness"), "ggplot")
})


test_that("response plot preserves declared zero-count ordered categories", {
  dat <- data.frame(
    ordered_item = ordered(
      c("low", "high", "low", "high", "low", "high"),
      levels = c("low", "mid", "high")
    ),
    item2 = 1:6
  )

  out <- nomo_screen(dat)
  p <- plot(
    out,
    type = "responses",
    items = "ordered_item"
  )

  middle <- p$data[
    p$data$response == "mid",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(middle), 1L)
  expect_equal(middle$n, 0L)
  expect_equal(middle$proportion_observed, 0)
})


test_that("evidence map represents derived negative-pair review evidence", {
  dat <- data.frame(
    item1 = 1:6,
    item2 = 1:6,
    reverse_candidate = 6:1
  )

  out <- nomo_screen(dat)
  p <- plot(out, type = "evidence")

  cell <- p$data[
    as.character(p$data$item) == "reverse_candidate" &
      as.character(p$data$metric_label) == "Negative inter-item",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(cell), 1L)
  expect_identical(as.character(cell$severity), "review")
})


test_that("plot item filters are validated", {
  dat <- data.frame(
    item1 = 1:6,
    item2 = 1:6
  )
  out <- nomo_screen(dat)

  expect_error(
    plot(out, items = "not_an_item"),
    "Unknown item"
  )

  expect_error(
    plot(out, items = character()),
    "non-empty"
  )
})


test_that("item-rest plot fails informatively when no estimates are available", {
  dat <- data.frame(
    ordered_item = ordered(
      c("low", "mid", "high", "mid", "low", "high"),
      levels = c("low", "mid", "high")
    )
  )

  out <- nomo_screen(dat)

  expect_error(
    plot(out, type = "item_rest"),
    "No corrected item-rest correlations"
  )
})


test_that("summary and plotting leave the screening object unchanged", {
  dat <- data.frame(
    item1 = 1:6,
    item2 = c(1, 2, 2, 4, 5, 6)
  )

  out <- nomo_screen(dat)
  original <- out

  summary(out)
  plot(out, type = "evidence")

  expect_identical(out, original)
})


test_that("summary print says what triggered each flag instead of hiding it (#89)", {
  dat <- data.frame(
    item1 = 1:6,
    item2 = 1:6,
    reverse_candidate = 6:1
  )

  s <- summary(nomo_screen(dat))

  # Each flagged item is listed with the decision log's own explanation.
  expect_output(print(s), "Flagged items")
  expect_output(print(s), "item-rest correlation")
})


test_that("item-rest plot colors item-rest evidence rather than overall item attention", {
  dat <- data.frame(
    item1 = c(1, 2, 3, 4, 5, 6),
    item2 = c(1, 2, 2, 4, 5, 6),
    reverse_candidate = c(6, 5, 4, 3, 2, 1),
    item4 = c(1, 1, 2, 3, 4, 5)
  )

  out <- nomo_screen(dat)
  p <- plot(out, type = "item_rest")

  good_row <- p$data[
    p$data$item == "item2",
    ,
    drop = FALSE
  ]
  reverse_row <- p$data[
    p$data$item == "reverse_candidate",
    ,
    drop = FALSE
  ]

  expect_identical(
    as.character(good_row$item_rest_attention),
    "none"
  )
  expect_identical(
    as.character(reverse_row$item_rest_attention),
    "review"
  )
})


test_that("continuous items are drawn as histograms, not a bar per value (#89)", {
  out <- nomo_screen(nomo_demo_continuous)
  p <- plot(out, type = "responses")
  expect_true(inherits(p$layers[[1]]$stat, "StatBin"))

  # Twenty bins per item, and each item's proportions sum to one.
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_identical(length(unique(built$PANEL)), 10L)
  expect_true(all(table(built$PANEL) == 20L))
  expect_equal(as.numeric(tapply(built$y, built$PANEL, sum)), rep(1, 10), tolerance = 1e-8)

  # A mixed selection draws the categorical items and names the others.
  mixed <- nomo_screen(data.frame(cont = nomo_demo_continuous$a1, disc = rep(1:5, 100)))
  p_mixed <- plot(mixed, type = "responses")
  expect_identical(levels(p_mixed$data$item), "disc")
  expect_match(plot_text(p_mixed$labels$caption), "Continuous items are not shown here: cont", fixed = TRUE)
  expect_match(plot_text(p_mixed$labels$caption), 'items = c("cont")', fixed = TRUE)
  p_cont <- plot(mixed, type = "responses", items = "cont")
  expect_true(inherits(p_cont$layers[[1]]$stat, "StatBin"))

  empty <- out
  empty$response_distribution <- empty$response_distribution[0, , drop = FALSE]
  expect_error(plot(empty, type = "responses"), "No observed responses", fixed = TRUE)
})


test_that("response plot preserves declared ordered-category order visually", {
  dat <- data.frame(
    ordered_item = ordered(
      c("low", "high", "low", "high", "low", "high"),
      levels = c("low", "mid", "high")
    )
  )

  out <- nomo_screen(dat)
  p <- plot(out, type = "responses", items = "ordered_item")

  expect_true(is.factor(p$data$response_key))
  expect_identical(
    as.character(p$data$response),
    c("low", "mid", "high")
  )
  expect_identical(
    as.integer(p$data$response_key),
    c(1L, 2L, 3L)
  )
})


test_that("missingness labels are nudged away from the zero axis", {
  dat <- data.frame(
    item1 = c(1, 2, 3, 4, 5, NA),
    item2 = 1:6
  )

  out <- nomo_screen(dat)
  p <- plot(out, type = "missingness")

  zero_row <- p$data[
    p$data$item == "item2",
    ,
    drop = FALSE
  ]

  expect_gt(zero_row$missingness_label_position, 0)
})


test_that("plot legends show only attention levels that are actually present", {
  dat <- data.frame(
    item1 = 1:6,
    item2 = c(1, 2, 2, 4, 5, 6),
    reverse_candidate = 6:1
  )

  out <- nomo_screen(dat)

  evidence <- plot(out, type = "evidence")
  evidence_built <- ggplot2::ggplot_build(evidence)
  evidence_scale <- evidence_built$plot$scales$get_scales("fill")
  evidence_breaks <- as.character(evidence_scale$get_breaks())

  expect_true(all(c("none", "info", "review") %in% evidence_breaks))
  expect_false("concern" %in% evidence_breaks)

  item_rest <- plot(out, type = "item_rest")
  item_rest_built <- ggplot2::ggplot_build(item_rest)
  item_rest_scale <- item_rest_built$plot$scales$get_scales("fill")
  item_rest_breaks <- as.character(item_rest_scale$get_breaks())

  # In this toy data, every estimable item-rest value is itself a review
  # signal, so "none" is correctly absent from the legend.
  expect_true("review" %in% item_rest_breaks)
  expect_true(all(item_rest_breaks %in% c("none", "review")))
  expect_false("concern" %in% item_rest_breaks)
})


test_that("evidence legend restores concern when concern-level evidence exists", {
  dat <- data.frame(
    item1 = 1:6,
    constant = rep(1, 6)
  )

  out <- nomo_screen(dat)
  evidence <- plot(out, type = "evidence")

  built <- ggplot2::ggplot_build(evidence)
  scale <- built$plot$scales$get_scales("fill")
  breaks <- as.character(scale$get_breaks())

  expect_true("concern" %in% breaks)
})


test_that("item-rest legend includes none when no-review item-rest evidence is present", {
  dat <- data.frame(
    item1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    item2 = c(1, 2, 3, 4, 5, 6, 7, 8),
    item3 = c(1, 2, 3, 4, 5, 6, 8, 7)
  )

  out <- nomo_screen(dat)
  p <- plot(out, type = "item_rest")

  built <- ggplot2::ggplot_build(p)
  scale <- built$plot$scales$get_scales("fill")
  breaks <- as.character(scale$get_breaks())

  expect_true("none" %in% breaks)
  expect_false("concern" %in% breaks)
})


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("closeout: screen and split cover empty, binary, other, and RNG cleanup paths", {
  empty_cols <- data.frame(row.names = 1:3)
  expect_error(nomo_screen(empty_cols), "at least one column")

  expect_equal(
    nomologR:::nomo_screen_item_type(c(TRUE, FALSE, TRUE)),
    "binary"
  )
  expect_equal(
    nomologR:::nomo_screen_item_type(as.Date("2026-01-01") + 0:2),
    "other"
  )

  scr <- nomo_screen(data.frame(x = c(1, 2, 3, 4, 5)), items = "x")
  scr$decision_log <- nomologR:::nomo_log_new()
  txt <- paste(capture.output(print(scr)), collapse = "\n")
  expect_match(txt, "Decision log: no entries", fixed = TRUE)

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  invisible(nomo_split(data.frame(x = 1:10), seed = 2026L))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})


test_that("closeout: screening relationship diagnostics cover non-finite scores and no numeric reference", {
  selected <- data.frame(
    a = c(1, Inf, 3, 4, 5),
    b = c(1, 2, 3, 4, 5)
  )
  item_summary <- dplyr::bind_rows(
    nomologR:::nomo_screen_item_summary(selected$a, "a"),
    nomologR:::nomo_screen_item_summary(selected$b, "b")
  )
  guidance <- nomo_defaults()
  rel <- nomologR:::nomo_screen_relationships(
    selected = selected,
    item_summary = item_summary,
    guidance = guidance
  )
  expect_true(any(rel$decision_log$metric == "non_finite_scores"))

  selected2 <- data.frame(
    a = 1:8,
    b = 1:8,
    c = 8:1
  )
  item_summary2 <- dplyr::bind_rows(
    nomologR:::nomo_screen_item_summary(selected2$a, "a"),
    nomologR:::nomo_screen_item_summary(selected2$b, "b"),
    nomologR:::nomo_screen_item_summary(selected2$c, "c")
  )
  guidance2 <- nomo_defaults()
  guidance2$item_total_reference <- NA_real_
  rel2 <- nomologR:::nomo_screen_relationships(
    selected = selected2,
    item_summary = item_summary2,
    guidance = guidance2
  )
  neg <- rel2$decision_log[
    rel2$decision_log$metric == "corrected_item_rest" &
      rel2$decision_log$value < 0,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(neg), 0L)
  expect_true(any(grepl(
    "Negative sign requires coding/structure review",
    neg$reference,
    fixed = TRUE
  )))
})


test_that("closeout: screening descriptives cover guidance fallback and ceiling concentration", {
  x <- c(1, 2, 3, 4, rep(5, 16))
  selected <- data.frame(item = x)
  item_summary <- nomologR:::nomo_screen_item_summary(x, "item")

  guidance <- nomo_defaults()
  guidance$response_concentration_reference <- "bad"
  guidance$nzv_frequency_ratio_reference <- NULL
  guidance$nzv_percent_unique_reference <- Inf

  out <- nomologR:::nomo_screen_descriptives(
    selected = selected,
    item_summary = item_summary,
    guidance = guidance
  )
  expect_true(any(
    out$decision_log$metric %in%
      c("response_concentration", "ceiling_concentration")
  ))

  guidance$response_concentration_reference <- .70
  out2 <- nomologR:::nomo_screen_descriptives(
    selected = selected,
    item_summary = item_summary,
    guidance = guidance
  )
  expect_true(any(out2$decision_log$metric == "ceiling_concentration"))
})


test_that("closeout: screen presentation guards and rare evidence states are exercised", {
  expect_error(
    nomologR:::summary.nomo_screen(list()),
    "inherit from `nomo_screen`"
  )
  expect_error(
    nomologR:::plot.nomo_screen(list()),
    "inherit from `nomo_screen`"
  )

  ord <- ordered(
    c("low", "low", "high", "high"),
    levels = c("low", "mid", "high")
  )
  scr <- nomo_screen(data.frame(ord = ord, x = 1:4))
  ev <- nomologR:::nomo_screen_evidence_data(scr, scr$items)
  expect_true(any(
    ev$metric == "unused_response_categories" &
      as.character(ev$severity) == "review"
  ))

  extra <- nomologR:::nomo_log_add(
    scr$decision_log,
    stage = "screen",
    object = "x",
    metric = "does_not_map_to_evidence_grid",
    severity = "review"
  )
  scr$decision_log <- extra
  expect_s3_class(
    nomologR:::nomo_screen_evidence_data(scr, scr$items),
    "data.frame"
  )

  one <- nomo_screen(data.frame(x = 1:6), items = "x")
  expect_error(
    plot(one, type = "interitem"),
    "At least two relationship-eligible"
  )

  no_resp <- scr
  no_resp$response_distribution <- no_resp$response_distribution[0, , drop = FALSE]
  expect_error(
    plot(no_resp, type = "responses"),
    "No observed/declared response categories"
  )

  concern <- nomo_screen(data.frame(a = 1:8, b = 1:8, c = 8:1))
  concern$decision_log <- nomologR:::nomo_log_add(
    concern$decision_log,
    stage = "screen",
    object = "c",
    metric = "corrected_item_rest",
    severity = "concern"
  )
  p <- plot(concern, type = "item_rest")
  expect_s3_class(p, "ggplot")
})


test_that("closeout B: model and screening type helpers cover missing factor names and ordered binary items", {
  bad <- list(c("x1", "x2"), c("x3", "x4"))
  names(bad) <- c("F1", NA_character_)
  expect_error(
    nomo_model(bad),
    "unique, non-empty factor names"
  )

  binary_ordered <- ordered(
    c("no", "yes", "no", "yes"),
    levels = c("no", "yes")
  )
  expect_identical(
    nomologR:::nomo_screen_item_type(binary_ordered),
    "binary"
  )
})


test_that("closeout B: screen evidence aliases floor and ceiling concentration to one presentation metric", {
  scr <- nomo_screen(
    data.frame(x = c(1, 1, 1, 1, 2, 3)),
    items = "x"
  )

  scr$decision_log <- nomologR:::nomo_log_add(
    scr$decision_log,
    stage = "screen",
    object = "x",
    metric = "floor_concentration",
    severity = "review"
  )

  dat <- nomologR:::nomo_screen_evidence_data(scr, "x")
  row <- dat[
    dat$metric == "response_concentration",
    ,
    drop = FALSE
  ]
  expect_identical(as.character(row$severity[[1L]]), "review")
})


test_that("a constant column reads as constant in the summary (#89)", {
  d <- nomo_demo_continuous[, 1:4]
  d$same <- 3
  out <- nomo_screen(d)
  printed <- capture.output(print(summary(out)))
  expect_true(any(grepl("^\\s+same\\s+constant\\s", printed)))
  # Display only: the returned type keeps its stored value.
  expect_identical(out$item_summary$item_type[out$item_summary$item == "same"], "binary")
})


test_that("a negative item-rest correlation the declared keying does not explain says so (#60)", {
  set.seed(60)
  f <- stats::rnorm(200)
  data <- data.frame(
    x = pmin(5, pmax(1, round(3 + f + stats::rnorm(200, sd = .8)))),
    y = pmin(5, pmax(1, round(3 + f + stats::rnorm(200, sd = .8)))),
    z = pmin(5, pmax(1, round(3 - f + stats::rnorm(200, sd = .8))))
  )
  # Declaring every item reverse-keyed recodes them all, which leaves z
  # running against the other two.
  out <- nomo_screen(data, reverse = c("x", "y", "z"), scale_range = c(1, 5))
  z <- out$decision_log[out$decision_log$object == "z" &
                          out$decision_log$metric == "corrected_item_rest", ]
  expect_match(z$observation, "so the keying does not explain the sign", fixed = TRUE)

  # Without usable keying, the entry reads as before.
  plain <- nomo_screen(data, reverse = "z")
  z <- plain$decision_log[plain$decision_log$object == "z" &
                            plain$decision_log$metric == "corrected_item_rest", ]
  expect_false(grepl("declared reverse-keyed", z$observation, fixed = TRUE))
  expect_match(z$recommendation, "Inspect intended keying", fixed = TRUE)
})


test_that("declared scales give each item its within-scale item-rest correlation (#113)", {
  handoff <- readRDS(system.file("extdata", "content-handoff-walkthrough.rds",
                                 package = "nomologR"))
  walk <- nomo_demo_walkthrough
  walk$EF2 <- 6L - walk$EF2
  walk$TF2 <- 6L - walk$TF2

  out <- nomo_screen(walk, items = handoff)
  rel <- out$relationship_summary
  expect_true(all(c("scale", "scale_item_rest_r", "scale_item_rest_n") %in% names(rel)))

  # Each value is the item against the rest of its own scale, the total it
  # is scored on.
  ef <- c("EF1", "EF2", "EF3", "EF4", "EF6")
  tf <- c("TF1", "TF2", "TF3", "TF4", "TF6")
  rest <- function(item, facet) {
    stats::cor(walk[[item]], rowSums(walk[setdiff(facet, item)]))
  }
  expected <- c(vapply(ef, rest, numeric(1), facet = ef),
                vapply(tf, rest, numeric(1), facet = tf))
  expect_equal(rel$scale_item_rest_r, unname(expected[rel$item]))
  expect_identical(rel$scale, rep(c("EF", "TF"), each = 5L))
  expect_identical(rel$scale_item_rest_n, rep(400L, 10L))
  # The pooled value is unchanged: TF4 shares variance with Effort
  # Regulation, which the pool counts in its favor.
  expect_gt(rel$corrected_item_rest_r[rel$item == "TF4"],
            rel$scale_item_rest_r[rel$item == "TF4"])

  # The review reads the within-scale value: only EF4 is below the reference.
  log <- out$decision_log[out$decision_log$metric == "corrected_item_rest", ]
  expect_identical(log$object, "EF4")
  expect_equal(log$value, rel$scale_item_rest_r[rel$item == "EF4"])
  expect_match(log$observation, "within its scale `EF` of r = 0.23", fixed = TRUE)

  local_reproducible_output(width = 80)
  printed <- capture.output(print(summary(out)))
  expect_true(any(grepl("Scale", printed, fixed = TRUE)))
  expect_true(any(grepl("Item-rest r is within the item's scale", printed, fixed = TRUE)))
  p <- plot(out, type = "item_rest")
  expect_equal(p$data$item_rest, rel$scale_item_rest_r[match(p$data$item, rel$item)])
})


test_that("item-rest values are drawn clear of the bars and the reference line", {
  handoff <- readRDS(system.file("extdata", "content-handoff-walkthrough.rds",
                                 package = "nomologR"))
  out <- nomo_screen(nomo_demo_walkthrough, items = handoff)
  label_y <- function(p) {
    text <- vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))
    p$layers[[which(text)]]$aes_params$y
  }
  values <- out$relationship_summary$scale_item_rest_r
  expect_gt(label_y(plot(out, type = "item_rest")),
            max(values, out$guidance$item_total_reference))

  # A reference past every bar moves the column past the line.
  out$guidance$item_total_reference <- 0.90
  expect_gt(label_y(plot(out, type = "item_rest")), 0.90)

  # Without a reference line, the column sits just past the longest bar.
  out$guidance$item_total_reference <- NA_real_
  p <- plot(out, type = "item_rest")
  expect_false(any(vapply(p$layers, function(l) {
    inherits(l$geom, "GeomHline") && identical(l$aes_params$linetype, 2)
  }, logical(1))))
  expect_gt(label_y(p), max(values))
  expect_lt(label_y(p), 0.90)
})


test_that("keying notes on a declared scale use the within-scale value (#113)", {
  handoff <- readRDS(system.file("extdata", "content-handoff-walkthrough.rds",
                                 package = "nomologR"))
  # As collected, EF2 and TF2 are not yet recoded.
  out <- nomo_screen(nomo_demo_walkthrough, items = handoff)
  log <- out$decision_log[out$decision_log$metric == "corrected_item_rest", ]
  expect_true(all(c("EF2", "TF2") %in% log$object))

  walk <- nomo_demo_walkthrough
  walk$EF2 <- 6L - walk$EF2
  recoded <- nomo_screen(walk, items = handoff)$relationship_summary
  ef2 <- log$observation[log$object == "EF2"]
  expect_match(ef2, "within its scale `EF`", fixed = TRUE)
  expect_match(
    ef2,
    sprintf("item-rest correlation is r = %.2f",
            recoded$scale_item_rest_r[recoded$item == "EF2"]),
    fixed = TRUE
  )
})


test_that("one declared scale, or none, leaves the item-rest review pooled (#113)", {
  walk <- nomo_demo_walkthrough
  ef <- c("EF1", "EF2", "EF3", "EF4", "EF6")
  one <- nomo_screen(walk, items = ef, scales = list(EF = ef))
  none <- nomo_screen(walk, items = ef)
  expect_true(all(is.na(one$relationship_summary$scale_item_rest_r)))
  expect_true(all(is.na(one$relationship_summary$scale)))
  expect_identical(one$decision_log, none$decision_log)

  # Unnamed scales are named by position; an item counts in the first scale
  # that lists it.
  two <- nomo_screen(walk, items = c(ef, "TF1", "TF3"),
                     scales = list(ef, c("EF6", "TF1", "TF3")))
  rel <- two$relationship_summary
  expect_identical(rel$scale, c(rep("Scale 1", 5L), "Scale 2", "Scale 2"))

  # `scales` is checked even without the effort indices.
  expect_error(nomo_screen(walk, items = ef, scales = list(A = "TF1")),
               "not being screened", fixed = TRUE)
})


test_that("a scale with fewer than three complete cases has no within-scale value", {
  dat <- data.frame(
    a1 = c(1, 2, 3, 4, 5, 2), a2 = c(2, 2, 3, 5, 4, 1), a3 = c(1, 3, 3, 4, 5, 2),
    b1 = c(1, 5, NA, NA, NA, NA), b2 = c(2, 4, NA, NA, NA, 3)
  )
  out <- nomo_screen(dat, scales = list(A = c("a1", "a2", "a3"), B = c("b1", "b2")))
  rel <- out$relationship_summary
  expect_true(all(is.finite(rel$scale_item_rest_r[rel$scale == "A"])))
  expect_true(all(is.na(rel$scale_item_rest_r[rel$scale == "B"])))
  expect_identical(rel$scale_item_rest_n[rel$scale == "B"], c(2L, 2L))
})


test_that("screens saved before within-scale values still summarize and plot", {
  out <- nomo_screen(nomo_demo_walkthrough, items = c("EF1", "EF3", "EF4", "EF6"))
  out$relationship_summary$scale_item_rest_r <- NULL
  local_reproducible_output(width = 80)
  expect_no_warning(capture.output(print(summary(out))))
  expect_no_warning(p <- plot(out, type = "item_rest"))
  expect_equal(p$data$item_rest,
               out$relationship_summary$corrected_item_rest_r[
                 match(p$data$item, out$relationship_summary$item)])
})
