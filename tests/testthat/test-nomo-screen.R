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

  expect_output(returned <- print(s), "<summary_nomo_screen>")
  expect_s3_class(returned, "summary_nomo_screen")
  expect_output(
    print(s),
    "not an automatic retention/deletion decision"
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


test_that("summary print exposes the review metrics instead of hiding columns", {
  dat <- data.frame(
    item1 = 1:6,
    item2 = 1:6,
    reverse_candidate = 6:1
  )

  s <- summary(nomo_screen(dat))

  expect_output(print(s), "review_metrics")
  expect_output(print(s), "corrected_item_rest")
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
