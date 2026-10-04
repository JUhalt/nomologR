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
  expect_output(print(s), "Flagged")
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
  # On six responses, shares are whole percentages, rounded half up.
  expect_identical(p$data$value_label, c("50%", "0%", "50%"))
  labels <- ggplot2::ggplot_build(p)$data[[2]]$label
  expect_identical(labels, c("50%", "0%", "50%"))
})


test_that("response-profile shares take one decimal on a base of 100 or more (#145)", {
  handoff <- readRDS(system.file("extdata", "content-handoff-walkthrough.rds",
                                 package = "nomologR"))
  out <- nomo_screen(nomo_demo_walkthrough, items = handoff)
  p <- plot(out, type = "responses")
  labels <- ggplot2::ggplot_build(p)$data[[2]]$label
  expect_true(all(grepl("^[0-9]+\\.[0-9]%$", labels)))
  ef1 <- out$response_distribution[out$response_distribution$item == "EF1" &
                                      !out$response_distribution$missing, ]
  expect_identical(p$data$value_label[p$data$item == "EF1"],
                   sprintf("%.1f%%", floor(1000 * ef1$n / 400 + 0.5) / 10))
  # 0.125 of 8 responses is 13%, half up, where sprintf("%.0f") gave 12%.
  eight <- nomo_screen(data.frame(x = c(1, 2, 2, 2, 3, 3, 3, 3), y = c(1:7, 7)))
  expect_identical(plot(eight, type = "responses", items = "x")$data$value_label,
                   c("13%", "38%", "50%"))
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

  # Status is drawn by shape, with color repeating it, from the shared status
  # scales; an informational entry is not a flag (#145).
  evidence <- plot(out, type = "evidence")
  evidence_built <- ggplot2::ggplot_build(evidence)
  evidence_scale <- evidence_built$plot$scales$get_scales("shape")
  evidence_breaks <- as.character(evidence_scale$get_breaks())

  expect_identical(evidence_breaks, c("none", "review"))
  expect_null(evidence_built$plot$scales$get_scales("fill"))

  item_rest <- plot(out, type = "item_rest")
  item_rest_built <- ggplot2::ggplot_build(item_rest)
  item_rest_scale <- item_rest_built$plot$scales$get_scales("shape")
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
  scale <- built$plot$scales$get_scales("shape")
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
  scale <- built$plot$scales$get_scales("shape")
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

  # Without a response scale the keying cannot be used, so the entry reads as
  # before, and the log says why rather than leaving the explanation absent
  # (#145).
  plain <- nomo_screen(data, reverse = "z")
  z <- plain$decision_log[plain$decision_log$object == "z" &
                            plain$decision_log$metric == "corrected_item_rest", ]
  expect_false(grepl("declared reverse-keyed", z$observation, fixed = TRUE))
  expect_match(z$recommendation, "Inspect intended keying", fixed = TRUE)
  unused <- plain$decision_log[plain$decision_log$metric == "keying_not_used", ]
  expect_identical(nrow(unused), 1L)
  expect_identical(unused$severity, "info")
  expect_identical(unused$value, 1)
  expect_match(unused$observation,
               "Reverse-keyed item z was declared without `scale_range`", fixed = TRUE)
  expect_match(unused$recommendation, "scale_range = c(min, max)", fixed = TRUE)

  # Usable keying, no keying, and keying declared with nothing reversed leave
  # no such row.
  expect_false("keying_not_used" %in% out$decision_log$metric)
  expect_false("keying_not_used" %in% nomo_screen(data)$decision_log$metric)
  expect_false("keying_not_used" %in%
                 nomo_screen(data, reverse = character(0))$decision_log$metric)
})


test_that("`reverse` and `scale_range` are checked without the effort indices (#145)", {
  data <- data.frame(x = c(1, 2, 3, 4, 5), y = c(2, 1, 4, 3, 5), z = c(5, 4, 2, 3, 1))

  # Each of these was accepted silently when `effort = FALSE`, although both
  # arguments still reach the item audit. They are refused in either mode.
  for (effort in c(FALSE, TRUE)) {
    expect_error(nomo_screen(data, effort = effort, reverse = "nope", scale_range = c(1, 5)),
                 "`reverse` names item(s) not being screened: nope.", fixed = TRUE)
    expect_error(nomo_screen(data, effort = effort, reverse = 3),
                 "`reverse` must be a character vector of item names.", fixed = TRUE)
    expect_error(nomo_screen(data, effort = effort, scale_range = c(5, 1)),
                 "min below max", fixed = TRUE)
    expect_error(nomo_screen(data, effort = effort, scale_range = "a"),
                 "min below max", fixed = TRUE)
    expect_error(nomo_screen(data, effort = effort, scale_range = c(1, 3, 5)),
                 "min below max", fixed = TRUE)
    expect_error(nomo_screen(data, effort = effort, reverse = "z", scale_range = c(1, Inf)),
                 "min below max", fixed = TRUE)
    expect_error(nomo_screen(data, effort = effort, reverse = "z", scale_range = c(1, NA)),
                 "min below max", fixed = TRUE)
  }

  # An item outside `items` is not being screened, whatever `data` holds.
  expect_error(nomo_screen(data, items = c("x", "y"), reverse = "z", scale_range = c(1, 5)),
               "not being screened: z", fixed = TRUE)

  # Reverse-keyed items with no range are refused only where they would have
  # to be recoded for an index.
  expect_error(nomo_screen(data, effort = TRUE, reverse = "z"), "not inferred", fixed = TRUE)
  expect_s3_class(nomo_screen(data, reverse = "z"), "nomo_screen")
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
  expect_match(log$observation, "within its scale `EF` of r = .23", fixed = TRUE)

  local_reproducible_output(width = 80)
  printed <- capture.output(print(summary(out)))
  expect_true(any(grepl("Scale", printed, fixed = TRUE)))
  expect_true(any(grepl("sum of the other items of", printed, fixed = TRUE)))
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
    sprintf("item-rest correlation is r = %s",
            nomologR:::nomo_present_stat(recoded$scale_item_rest_r[recoded$item == "EF2"], "r")),
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


test_that("negative inter-item pairs are reviewed within a declared scale (#113)", {
  scales <- list(Agency = paste0("ag", 1:4), Persistence = paste0("pe", 1:4),
                 SocialDesirability = paste0("sd", 1:3))
  items <- unlist(scales, use.names = FALSE)
  pooled <- nomo_screen(nomo_demo_network, items = items)
  declared <- nomo_screen(nomo_demo_network, items = items, scales = scales)

  # Social desirability is uncorrelated with the other constructs in the
  # population, so its correlations with their items straddle zero.
  iic <- declared$inter_item_correlations
  scale_of <- stats::setNames(rep(names(scales), lengths(scales)), items)
  between <- scale_of[iic$item1] != scale_of[iic$item2]
  expect_gt(sum(iic$r[between] < 0), 1L)
  expect_identical(sum(iic$r[!between] < 0), 0L)

  # Pooled, those pairs flag the Agency and Persistence items.
  expect_true(all(pooled$relationship_summary$negative_interitem_n[1:8] > 0L))
  expect_true(all(is.na(pooled$relationship_summary$scale_negative_interitem_n)))
  expect_true(all(summary(pooled)$item_review$attention == "review"))
  pooled_log <- pooled$decision_log[pooled$decision_log$metric == "negative_pairs", ]
  expect_identical(pooled_log$severity, "review")
  expect_match(pooled_log$observation, "correlations are negative.", fixed = TRUE)

  # Declared, they are reported as information and flag nothing.
  rel <- declared$relationship_summary
  expect_identical(rel$negative_interitem_n, pooled$relationship_summary$negative_interitem_n)
  expect_identical(rel$scale_negative_interitem_n, rep(0L, 11L))
  expect_true(all(summary(declared)$item_review$attention == "none"))
  log <- declared$decision_log
  expect_false("negative_pairs" %in% log$metric)
  info <- log[log$metric == "negative_pairs_between_scales", ]
  expect_identical(info$severity, "info")
  expect_equal(info$value, sum(iic$r[between] < 0))
  expect_match(info$observation, "declared scales are negative.", fixed = TRUE)
  expect_true(all(plot(declared)$data$severity[
    plot(declared)$data$metric == "negative_interitem_pairs"] == "none"))
})


test_that("one negative pair within a scale is still reviewed, and says so", {
  dat <- data.frame(
    a1 = c(1L, 4L, 1L, 2L, 5L, 3L, 2L, 3L, 3L, 1L),
    a2 = c(5L, 5L, 2L, 2L, 1L, 5L, 5L, 1L, 1L, 5L),
    b1 = c(5L, 2L, 2L, 1L, 4L, 1L, 4L, 3L, 2L, 2L),
    b2 = c(4L, 4L, 4L, 2L, 4L, 1L, 1L, 4L, 1L, 2L)
  )
  out <- nomo_screen(dat, scales = list(A = c("a1", "a2"), B = c("b1", "b2")))
  log <- out$decision_log
  within <- log[log$metric == "negative_pairs", ]
  expect_identical(within$observation,
                   "1 estimable inter-item correlation within a declared scale is negative.")
  between <- log[log$metric == "negative_pairs_between_scales", ]
  expect_match(between$observation,
               "1 correlation between items of different declared scales is negative.",
               fixed = TRUE)
  rel <- out$relationship_summary
  expect_identical(rel$scale_negative_interitem_n[rel$item %in% c("a1", "a2")], c(1L, 1L))
  expect_identical(rel$scale_negative_interitem_n[rel$item %in% c("b1", "b2")], c(0L, 0L))
  review <- summary(out)$item_review
  expect_true(all(grepl("negative_interitem_pairs", review$review_metrics[review$item %in% c("a1", "a2")])))
  expect_false(any(grepl("negative_interitem_pairs", review$review_metrics[review$item %in% c("b1", "b2")])))
})


test_that("screens saved before within-scale pair counts still review negative pairs", {
  out <- nomo_screen(nomo_demo_walkthrough, items = paste0("EF", 1:4))
  out$relationship_summary$scale_negative_interitem_n <- NULL
  expect_no_warning(review <- summary(out)$item_review)
  expect_true(all(grepl("negative_interitem_pairs", review$review_metrics[review$item != "EF2"])))
})


# The pre-1.0 audit (#145) -----------------------------------------------------

# Two correlated factors and an item of a third construct, on 300 cases.
audit_scales_data <- function(seed = 1) {
  set.seed(seed)
  f1 <- stats::rnorm(300)
  f2 <- stats::rnorm(300)
  data.frame(
    a1 = f1 + stats::rnorm(300), a2 = f1 + stats::rnorm(300), a3 = f1 + stats::rnorm(300),
    b1 = f2 + stats::rnorm(300), b2 = f2 + stats::rnorm(300), b3 = f2 + stats::rnorm(300),
    c1 = -0.2 * f1 + stats::rnorm(300)
  )
}


test_that("a guidance list that asks for automatic deletion is refused", {
  dat <- data.frame(x = 1:5, y = c(2, 1, 4, 3, 5))
  for (field in c("auto_delete", "auto_respecify")) {
    guidance <- nomo_defaults()
    guidance[[field]] <- TRUE
    expect_error(nomo_screen(dat, guidance = guidance),
                 sprintf("`guidance$%s` cannot be `TRUE`", field), fixed = TRUE)
  }
})


test_that("responses outside the declared scale_range are a concern (#145)", {
  dat <- data.frame(
    x = c(0, 1, 2, 3, 4, 4), y = c(1, 2, 3, 4, 5, 9), z = c(5, 4, 3, 2, 1, 1),
    label = factor(c("a", "b", "c", "a", "b", "c"))
  )
  out <- nomo_screen(dat, reverse = "x", scale_range = c(1, 5))
  rows <- out$decision_log[out$decision_log$metric == "out_of_range", ]
  expect_identical(rows$object, c("x", "y"))
  expect_identical(rows$value, c(1, 1))
  expect_identical(rows$reference, rep("Declared response scale 1 to 5", 2L))
  expect_identical(
    rows$observation,
    c("`x` has 1 response outside the declared response scale of 1 to 5 (observed 0 to 4).",
      "`y` has 1 response outside the declared response scale of 1 to 5 (observed 1 to 9).")
  )
  expect_match(rows$recommendation[[1L]], "this item is declared reverse-keyed", fixed = TRUE)
  expect_match(rows$recommendation[[2L]], "value computed from the scale.$")
  review <- summary(out)$item_review
  expect_identical(as.character(review$attention[review$item %in% c("x", "y")]),
                   c("concern", "concern"))

  # A range that holds every response, or no range, adds nothing.
  expect_false("out_of_range" %in% nomo_screen(dat, scale_range = c(0, 9))$decision_log$metric)
  expect_false("out_of_range" %in% nomo_screen(dat)$decision_log$metric)
  expect_identical(nrow(nomologR:::nomo_screen_out_of_range(dat[0], c(1, 5))), 0L)

  # The careless-responding indices would recode x on the wrong scale.
  numeric_items <- c("x", "y", "z")
  expect_error(
    nomo_screen(dat, items = numeric_items, effort = TRUE, reverse = "x", scale_range = c(1, 5)),
    "Outside it: x (observed 0 to 4).", fixed = TRUE
  )
  # A forward-keyed item outside the range is not recoded, so it is logged
  # rather than refused.
  ok <- nomo_screen(dat, items = numeric_items, effort = TRUE, reverse = "z",
                    scale_range = c(1, 5))
  expect_identical(ok$decision_log$object[ok$decision_log$metric == "out_of_range"], c("x", "y"))
  expect_identical(nomologR:::nomo_screen_range_text(c(0, 1.5), c(4, 7)),
                   c("0 to 4", "1.5 to 7"))
})


test_that("an item alone in its declared scale is not reviewed on the pooled value (#145)", {
  dat <- audit_scales_data()
  out <- nomo_screen(dat, scales = list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3"),
                                        C = "c1"))
  rel <- out$relationship_summary
  expect_lt(rel$corrected_item_rest_r[rel$item == "c1"], 0)
  expect_true(is.na(rel$scale_item_rest_r[rel$item == "c1"]))

  log <- out$decision_log[out$decision_log$object == "c1", ]
  expect_false("corrected_item_rest" %in% log$metric)
  row <- log[log$metric == "item_rest_not_computed", ]
  expect_identical(row$severity, "info")
  expect_match(row$observation,
               "`c1` is the only item of its scale `C` in the relationship diagnostics",
               fixed = TRUE)
  expect_match(row$recommendation, "not reviewed on the pooled item-rest value", fixed = TRUE)

  # The summary shows no value for it, and the plots leave it out.
  review <- summary(out)$item_review
  expect_identical(as.character(review$attention[review$item == "c1"]), "none")
  local_reproducible_output(width = 80)
  printed <- capture.output(print(summary(out)))
  expect_true(any(grepl("^  c1\\s+C\\s+continuous\\s+0\\.0%\\s+0\\.3%\\s+--\\s*$", printed)))
  expect_true(any(grepl("-- marks a value that was not computed", printed, fixed = TRUE)))
  expect_false("c1" %in% as.character(plot(out, type = "item_rest")$data$item))
  ev <- nomologR:::nomo_screen_evidence_data(out, out$items)
  expect_identical(as.character(ev$status[ev$item == "c1" & ev$metric == "corrected_item_rest"]),
                   "not computed")

  # A scale whose cases are too few, or that does not vary, says so too.
  few <- data.frame(
    a1 = c(1, 2, 3, 4, 5, 2), a2 = c(2, 2, 3, 5, 4, 1), a3 = c(1, 3, 3, 4, 5, 2),
    b1 = c(1, 5, NA, NA, NA, NA), b2 = c(2, 4, NA, NA, NA, 3)
  )
  few_out <- nomo_screen(few, scales = list(A = c("a1", "a2", "a3"), B = c("b1", "b2")))
  few_rows <- few_out$decision_log[few_out$decision_log$metric == "item_rest_not_computed", ]
  expect_identical(few_rows$object, c("b1", "b2"))
  expect_match(few_rows$observation[[1L]], "2 cases are complete on the scale's items",
               fixed = TRUE)
  flat <- data.frame(
    a1 = c(1, 2, 3, 4, 5, 2), a2 = c(2, 2, 3, 5, 4, 1),
    b1 = c(1, 1, 1, 1, 2, NA), b2 = c(2, 2, 2, 2, NA, 3)
  )
  flat_out <- nomo_screen(flat, scales = list(A = c("a1", "a2"), B = c("b1", "b2")))
  flat_rows <- flat_out$decision_log[flat_out$decision_log$metric == "item_rest_not_computed", ]
  expect_match(flat_rows$observation[[1L]], "does not vary among the 4 cases complete",
               fixed = TRUE)
})


test_that("negative pairs with an item outside every scale are named as such (#145)", {
  set.seed(5)
  f1 <- stats::rnorm(300)
  f2 <- stats::rnorm(300)
  dat <- data.frame(
    a1 = f1 + stats::rnorm(300), a2 = f1 + stats::rnorm(300), a3 = f1 + stats::rnorm(300),
    b1 = f2 + stats::rnorm(300), b2 = f2 + stats::rnorm(300), b3 = f2 + stats::rnorm(300),
    u1 = -f1 - f2 + stats::rnorm(300)
  )
  scales <- list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3"))
  out <- nomo_screen(dat, scales = scales)
  row <- out$decision_log[out$decision_log$metric == "negative_pairs", ]
  expect_identical(
    row$observation,
    "6 estimable inter-item correlations involving an item with no declared scale are negative."
  )
  expect_identical(out$relationship_summary$scale_negative_interitem_n[1:6], rep(1L, 6L))

  # Within a scale and with an unscaled item, each kind is counted.
  dat$a3 <- -dat$a3
  mixed <- nomo_screen(dat, scales = scales)
  row <- mixed$decision_log[mixed$decision_log$metric == "negative_pairs", ]
  expect_match(row$observation,
               "^2 estimable inter-item correlations within a declared scale and [0-9]+ involving an item with no declared scale are negative\\.$")

  # The summary says that u1's value is the pooled one and what its -- means.
  local_reproducible_output(width = 80)
  printed <- capture.output(print(summary(out)))
  expect_true(any(grepl("^  u1\\s+--\\s+continuous", printed)))
  notes <- paste(trimws(printed), collapse = " ")
  expect_match(notes, paste(
    "Item-rest r is the correlation of an item with the sum of the other items of",
    "its scale, or of all the other items for an item in no declared scale; the",
    "pooled value is kept as corrected_item_rest_r. In Scale, -- marks an item in",
    "no declared scale."
  ), fixed = TRUE)
  expect_false(any(grepl("not computed", printed, fixed = TRUE)))
  # So does the item-rest plot, which draws u1 with that value.
  p <- plot(out, type = "item_rest")
  expect_true("u1" %in% as.character(p$data$item))
  expect_match(plot_text(p$labels$subtitle),
               "or of all the other items for an item in no declared scale.", fixed = TRUE)
})


test_that("the summary notes say what each item-rest value and each -- is (#145)", {
  dat <- audit_scales_data()
  dat$c2 <- dat$c1 + stats::rnorm(300)
  dat$o <- ordered(sample(1:3, 300, TRUE))
  # A and C are single-item scales here, so only the items in no declared scale
  # have a value, and the ordered item is left out of the correlations.
  out <- nomo_screen(dat, scales = list(A = "a1", C = "c1", O = "o"))
  review <- summary(out)$item_review
  review$item_rest <- nomologR:::nomo_screen_review_item_rest(review)
  expect_identical(nomologR:::nomo_screen_review_notes(review)[-1], c(
    "Item-rest r is the correlation of an item in no declared scale with the sum of all the other items.",
    "In Scale, -- marks an item in no declared scale or left out of the correlation diagnostics.",
    "In other columns, it marks a value that was not computed; the decision log says why."
  ))

  # Without scales, the value is the pooled one; with only a missing value,
  # the -- note stands alone.
  review <- summary(nomo_screen(dat[c("a1", "a2", "a3")]))$item_review
  review$item_rest <- nomologR:::nomo_screen_review_item_rest(review)
  expect_identical(nomologR:::nomo_screen_review_notes(review)[-1],
                   "Item-rest r is the correlation of an item with the sum of the other items.")
  review$item_rest[[1L]] <- NA
  expect_identical(nomologR:::nomo_screen_review_notes(review)[[3L]],
                   "-- marks a value that was not computed; the decision log says why.")
})


test_that("forward items flagged by an unrecoded reverse-keyed item are told so (#145)", {
  handoff <- readRDS(system.file("extdata", "content-handoff-walkthrough.rds",
                                 package = "nomologR"))
  out <- nomo_screen(nomo_demo_walkthrough, items = handoff)
  log <- out$decision_log
  ef1 <- log[log$object == "EF1" & log$metric == "corrected_item_rest", ]
  expect_match(ef1$observation,
               "Its rest score includes EF2, declared reverse-keyed and not yet recoded in the data; with EF2 recoded as declared, its item-rest correlation is r = .",
               fixed = TRUE)
  expect_match(ef1$recommendation, "Recode the declared reverse-keyed items in the data first",
               fixed = TRUE)
  # The value given is the item's own value with EF2 recoded.
  walk <- nomo_demo_walkthrough
  walk$EF2 <- 6L - walk$EF2
  recoded <- nomo_screen(walk, items = handoff)$relationship_summary
  expect_match(ef1$observation, sprintf(
    "is r = %s.", nomologR:::nomo_present_stat(recoded$scale_item_rest_r[recoded$item == "EF1"], "r")
  ), fixed = TRUE)

  pairs <- log[log$metric == "negative_pairs", ]
  expect_match(pairs$observation,
               "All involve EF2 or TF2, declared reverse-keyed and not yet recoded in the data.",
               fixed = TRUE)
  expect_match(pairs$recommendation, "^Recode the declared reverse-keyed items")

  # The summary names the item each forward item correlates with negatively,
  # and says when that item is declared and not yet recoded (#145). EF1's own
  # log row already names EF2 in its rest score, so it is not told twice.
  local_reproducible_output(width = 80)
  lines <- capture.output(print(summary(out)))
  printed <- paste(trimws(lines), collapse = " ")
  expect_match(printed, paste(
    "- TF1, TF3, TF4, TF6 (Review): Its correlation with TF2 is negative. TF2 is",
    "declared reverse-keyed and not yet recoded in the data, so recode it first",
    "and read this correlation again."
  ), fixed = TRUE)
  expect_match(printed, "r = .56. Its correlation with EF2 is negative. - EF2 (Review)",
               fixed = TRUE)
  expect_match(printed, "- Inter-item correlations (Review): 8 estimable", fixed = TRUE)
  expect_identical(nomologR:::nomo_screen_unrecoded_items(log), c("EF2", "TF2"))
})


test_that("a negative forward item and a lone unrecoded pair are explained (#145)", {
  set.seed(145)
  f <- stats::rnorm(300)
  item <- function(sign = 1) pmin(5, pmax(1, round(3 + sign * f + stats::rnorm(300, sd = .9))))
  dat <- data.frame(x1 = item(), x2 = item(), x3 = item(-1), x4 = item(-1))
  # x3 is declared and not yet recoded; x4 is reversed but undeclared.
  out <- nomo_screen(dat, reverse = "x3", scale_range = c(1, 5))
  log <- out$decision_log
  x4 <- log[log$object == "x4" & log$metric == "corrected_item_rest", ]
  expect_match(x4$observation, "Its rest score includes x3", fixed = TRUE)
  expect_identical(
    x4$recommendation,
    paste("Recode the declared reverse-keyed items in the data first, then read",
          "this item again; do not reverse-score or delete it from this value.")
  )
  pairs <- log[log$metric == "negative_pairs", ]
  expect_match(pairs$observation,
               "4 estimable inter-item correlations are negative. 2 of them involve x3, declared reverse-keyed",
               fixed = TRUE)
  # x1's own row names x3 in its rest score, every other item when no scale
  # was declared, so the summary does not explain its pair with x3 again.
  local_reproducible_output(width = 80)
  printed <- paste(trimws(capture.output(print(summary(out)))), collapse = " ")
  expect_match(printed, "Its correlations with x3 and x4 are negative. - x2 (Review)",
               fixed = TRUE)

  # One negative pair, involving the unrecoded item: y3 runs against y1 and
  # shares its error with y2.
  set.seed(3)
  g <- stats::rnorm(300)
  e <- stats::rnorm(300)
  one <- data.frame(y1 = g, y2 = e + 0.3 * g, y3 = -g + e)
  scr <- nomo_screen(one, reverse = "y3", scale_range = c(-100, 100))
  pair <- scr$decision_log[scr$decision_log$metric == "negative_pairs", ]
  expect_identical(
    pair$observation,
    "1 estimable inter-item correlation is negative. It involves y3, declared reverse-keyed and not yet recoded in the data."
  )
  others <- nomo_screen(one, reverse = "y2", scale_range = c(-100, 100))
  expect_match(others$decision_log$observation[others$decision_log$metric == "negative_pairs"],
               "^1 estimable inter-item correlation is negative\\.$")
  expect_identical(nomologR:::nomo_screen_unrecoded_note(character(0), NA_real_), "")
})


test_that("two declared items not yet recoded in one scale are each described once (#145)", {
  set.seed(11)
  f <- stats::rnorm(400)
  g <- stats::rnorm(400)
  item <- function(z, s = 1) pmin(5, pmax(1, round(3 + s * z + stats::rnorm(400, sd = .9))))
  # A2 and A3 run against their scale as collected.
  dat <- data.frame(A1 = item(f), A2 = item(f, -1), A3 = item(f, -1), A4 = item(f),
                    B1 = item(g), B2 = item(g), B3 = item(g))
  scales <- list(A = paste0("A", 1:4), B = paste0("B", 1:3))
  within_r <- function(data, it) {
    rel <- nomo_screen(data, scales = scales)$relationship_summary
    nomologR:::nomo_present_stat(rel$scale_item_rest_r[rel$item == it], "r")
  }
  recoded <- dat
  recoded[c("A2", "A3")] <- 6 - recoded[c("A2", "A3")]

  out <- nomo_screen(dat, scales = scales, reverse = c("A2", "A3"), scale_range = c(1, 5))
  obs <- function(x, it) {
    x$decision_log$observation[x$decision_log$object == it &
                                 x$decision_log$metric == "corrected_item_rest"]
  }
  # A declared item's value recodes it and the other declared item, and says
  # so; it gets no second sentence about its partner.
  a2 <- obs(out, "A2")
  expect_match(a2, sprintf(
    "with it and A3 recoded on the declared 1 to 5 scale, its item-rest correlation is r = %s. The data were not recoded.",
    within_r(recoded, "A2")
  ), fixed = TRUE)
  expect_no_match(a2, "Its rest score includes", fixed = TRUE)
  expect_no_match(obs(out, "A3"), "Its rest score includes", fixed = TRUE)
  # A forward item's value recodes both.
  expect_match(obs(out, "A1"), sprintf(
    "Its rest score includes A2 and A3, declared reverse-keyed and not yet recoded in the data; with them recoded as declared, its item-rest correlation is r = %s.",
    within_r(recoded, "A1")
  ), fixed = TRUE)

  # A4 is declared too, but the data already run its way: recoding it as
  # declared leaves the sign, and recoding only A2 and A3 gives its value.
  mixed <- nomo_screen(dat, scales = scales, reverse = c("A2", "A3", "A4"),
                       scale_range = c(1, 5))
  all_three <- recoded
  all_three$A4 <- 6 - all_three$A4
  a4 <- obs(mixed, "A4")
  expect_match(a4, sprintf(
    "but with it, A2, and A3 recoded as declared, its item-rest correlation is still r = %s,",
    within_r(all_three, "A4")
  ), fixed = TRUE)
  expect_match(a4, sprintf(
    "with them recoded as declared, its item-rest correlation is r = %s.",
    within_r(recoded, "A4")
  ), fixed = TRUE)
  expect_match(obs(mixed, "A2"), sprintf(
    "with it, A3, and A4 recoded on the declared 1 to 5 scale, its item-rest correlation is r = %s.",
    within_r(all_three, "A2")
  ), fixed = TRUE)
})


test_that("an item with non-finite values is flagged in the item review (#145)", {
  dat <- audit_scales_data()
  dat$a1[1] <- Inf
  dat$b1[1:2] <- -Inf
  out <- nomo_screen(dat)
  rows <- out$decision_log[out$decision_log$metric == "non_finite_scores", ]
  expect_identical(rows$object, c("a1", "b1"))
  expect_identical(rows$value, c(1, 2))
  expect_identical(rows$severity, c("concern", "concern"))
  expect_match(rows$observation[[2L]], "`b1` contains 2 non-finite values (Inf or -Inf)",
               fixed = TRUE)
  review <- summary(out)$item_review
  expect_identical(as.character(review$attention[review$item %in% c("a1", "b1")]),
                   c("concern", "concern"))
  ev <- nomologR:::nomo_screen_evidence_data(out, out$items)
  expect_identical(as.character(ev$severity[ev$item == "a1" & ev$metric == "coding"]), "concern")
  expect_identical(as.character(ev$status[ev$item == "a1" & ev$metric == "corrected_item_rest"]),
                   "not computed")
})


test_that("floor and ceiling use the declared scale, or say they are observed (#145)", {
  set.seed(3)
  dat <- data.frame(a1 = sample(3:5, 300, TRUE, prob = c(.9, .05, .05)),
                    a2 = sample(1:5, 300, TRUE), a3 = sample(1:5, 300, TRUE))
  observed <- nomo_screen(dat)
  row <- observed$decision_log[observed$decision_log$object == "a1" &
                                 observed$decision_log$metric == "floor_concentration", ]
  expect_match(row$observation, "in one category at the lowest observed value.", fixed = TRUE)
  expect_match(row$recommendation, "supply `scale_range`", fixed = TRUE)
  expect_equal(observed$item_summary$floor_prop[[1L]], mean(dat$a1 == 3))

  # On the declared 1 to 5 scale, a pile-up at 3 is concentration, not a floor.
  declared <- nomo_screen(dat, scale_range = c(1, 5))
  expect_identical(declared$item_summary$floor_prop[[1L]], 0)
  expect_equal(declared$item_summary$ceiling_prop[[1L]], mean(dat$a1 == 5))
  row <- declared$decision_log[declared$decision_log$object == "a1", ]
  expect_true("response_concentration" %in% row$metric)
  expect_false(any(c("floor_concentration", "ceiling_concentration") %in% row$metric))

  dat$a1 <- sample(c(1, 4, 5), 300, TRUE, prob = c(.9, .05, .05))
  top <- nomo_screen(dat, scale_range = c(1, 5))
  row <- top$decision_log[top$decision_log$object == "a1" &
                            top$decision_log$metric == "floor_concentration", ]
  expect_match(row$observation, "at the lowest category of the declared response scale.",
               fixed = TRUE)
  expect_no_match(row$recommendation, "supply `scale_range`", fixed = TRUE)
})


test_that("only a numeric-discrete item is told to supply scale_range (#145)", {
  set.seed(2)
  dat <- data.frame(
    g = factor(sample(c("x", "y", "z"), 200, TRUE, prob = c(.92, .04, .04))),
    lg = sample(c(TRUE, FALSE), 200, TRUE, prob = c(.93, .07)),
    o = ordered(sample(c("lo", "mid", "hi"), 200, TRUE, prob = c(.05, .9, .05)),
                levels = c("lo", "mid", "hi")),
    cont = c(rep(0, 180), stats::rnorm(20)),
    n1 = sample(1:5, 200, TRUE), n2 = sample(1:5, 200, TRUE)
  )
  out <- nomo_screen(dat)
  rows <- out$decision_log[out$decision_log$metric == "response_concentration", ]
  expect_setequal(rows$object, c("g", "lg", "o", "cont"))
  # Factor, logical, ordered, and continuous items have no ends taken from
  # scale_range, so none is asked for it.
  expect_false(any(grepl("scale_range", rows$recommendation, fixed = TRUE)))
  expect_false(any(grepl("numeric item shows only", rows$recommendation, fixed = TRUE)))
  expect_identical(out$item_summary$item_type[1:4],
                   c("nominal", "binary", "ordered", "numeric_continuous"))
})


test_that("response distributions count numeric values, not their labels (#145)", {
  dist <- nomologR:::nomo_screen_distribution(c(0.3, 0.1 + 0.2, 1, 2, 2.5, 3.7, NA), "x")
  observed <- dist[!dist$missing, ]
  expect_identical(sum(observed$n), 6L)
  expect_equal(sum(observed$proportion_observed), 1)
  expect_identical(observed$n, rep(1L, 6L))
})


test_that("duplicated column names are refused, whether or not items are named (#145)", {
  dat <- data.frame(a1 = 1:5, a1 = c(2, 1, 4, 3, 5), a3 = c(1, 3, 2, 5, 4),
                    check.names = FALSE)
  expect_error(nomo_screen(dat), "`data` has more than one column named `a1`", fixed = TRUE)
  expect_error(nomo_screen(dat, items = c("a1", "a3")), "more than one column named `a1`",
               fixed = TRUE)
  # A duplicate outside the items is no obstacle.
  expect_s3_class(nomo_screen(dat, items = "a3"), "nomo_screen")
})


test_that("log rows and printed counts agree in number (#145)", {
  dat <- data.frame(a = c(1, 2, 3, NA), b = factor(c("x", "y", "z", NA)), d = c(1, Inf, 2, NA))
  out <- nomo_screen(dat)
  obs <- out$decision_log$observation
  expect_true("1 case has no observed responses on the selected candidate items." %in% obs)
  expect_true("1 candidate item was not included in item-rest/inter-item diagnostics: b." %in% obs)
  expect_true("Only 1 candidate item is eligible for relationship diagnostics." %in% obs)
  expect_true("`a` has 1 missing response (25%)." %in% obs)
  none <- nomo_screen(data.frame(b = factor(c("x", "y", "z"))))
  expect_true("No candidate item is eligible for relationship diagnostics." %in%
                none$decision_log$observation)
  two <- nomo_screen(data.frame(a = c(1, 2, NA, NA), b = c(2, 1, NA, NA)))
  expect_true("2 cases have no observed responses on the selected candidate items." %in%
                two$decision_log$observation)

  local_reproducible_output(width = 80)
  printed <- capture.output(print(out))
  expect_true("Items in correlation diagnostics: 1 of 3 | Item-rest correlations: 0" %in% printed)
  expect_true("Decision log: 10 entries | Flagged: 1 review, 2 concern" %in% printed)
})


test_that("correlations in the log are written without a leading zero (#145)", {
  out <- nomo_screen(nomo_demo_continuous)
  row <- out$decision_log[out$decision_log$metric == "corrected_item_rest", ]
  expect_identical(
    row$observation,
    "`b5` has a corrected item-rest correlation of r = .28 (n = 473), below the teaching reference of .30."
  )
  expect_identical(row$reference, "Teaching reference .30; not a retention rule")
})


test_that("the print says why category items have no correlations (#145)", {
  local_reproducible_output(width = 80)
  printed <- capture.output(print(nomo_screen(nomo_demo_ordinal)))
  expect_true("Items in correlation diagnostics: 0 of 10 | Item-rest correlations: 0" %in% printed)
  expect_true(any(grepl(
    "Correlations were not computed for 10 ordered items: nomologR does not turn",
    printed, fixed = TRUE
  )))
  text <- paste(printed, collapse = " ")
  expect_match(text, "Supply numeric scores to include them.", fixed = TRUE)
  expect_match(text, "Decision log: 3 entries | Flagged: none", fixed = TRUE)
  mixed <- nomo_screen(data.frame(x = 1:6, y = c(2, 1, 4, 3, 6, 5),
                                  t = c("a", "b", "c", "d", "e", "f"),
                                  o = ordered(c(1, 2, 3, 1, 2, 3))))
  text <- paste(capture.output(print(mixed)), collapse = " ")
  expect_match(text, "not computed for 1 text and 1 ordered items", fixed = TRUE)
  one <- nomo_screen(data.frame(x = 1:6, y = c(2, 1, 4, 3, 6, 5), t = letters[1:6]))
  text <- paste(capture.output(print(one)), collapse = " ")
  expect_match(text, "for 1 text item: nomologR", fixed = TRUE)
  expect_match(text, "Supply numeric scores to include it.", fixed = TRUE)

  # The summary says the same, and shows no item-rest column.
  printed <- capture.output(print(summary(nomo_screen(nomo_demo_ordinal))))
  expect_true("Items in correlation diagnostics: 0 of 10" %in% printed)
  expect_false(any(grepl("Item-rest r", printed, fixed = TRUE)))
})


test_that("print and summary follow the shared output style (#144)", {
  local_reproducible_output(width = 80)
  scr <- nomo_screen(nomo_demo_continuous)
  printed <- capture.output(print(scr))
  expect_identical(printed[[1L]], "<nomo_screen> Item and data audit")
  expect_match(paste(printed, collapse = " "),
               "See summary(x) for each item's review and nomo_table(x, \"decision_log\") for every log entry.",
               fixed = TRUE)
  summary_printed <- capture.output(print(summary(scr)))
  expect_identical(summary_printed[[1L]], "<nomo_screen summary> Item and data audit")
  expect_true("Cases: 500 | Items: 10 | Item flags: 1 review, 0 concern" %in% summary_printed)
  expect_true(any(grepl("^  b5\\s+continuous\\s+0\\.0%\\s+1\\.4%\\s+\\.28  Review$", summary_printed)))
  expect_true("Flagged" %in% summary_printed)
  expect_match(paste(summary_printed, collapse = " "),
               "See nomo_table(x, \"decision_log\") for every log entry and plot(x) for the item evidence map.",
               fixed = TRUE)
  for (width in c(40, 80)) {
    local_reproducible_output(width = width)
    lines <- c(capture.output(print(scr)), capture.output(print(summary(scr))))
    expect_true(all(nchar(lines) <= width))
  }

  # A small sample gives whole percentages; no item is flagged.
  local_reproducible_output(width = 80)
  small <- capture.output(print(summary(nomo_screen(data.frame(
    x = c(1, 2, 3, NA, 5), y = c(1, 2, 3, 4, 5), z = c(2, 2, 3, 4, 5)
  )))))
  expect_true(any(grepl("^  x\\s+discrete\\s+20%", small)))
  expect_false("Flagged" %in% small)

  # With the careless-responding indices, the pointer names their table.
  effort <- capture.output(print(nomo_screen(nomo_demo_continuous, effort = TRUE)))
  expect_match(paste(effort, collapse = " "), "nomo_table(x, \"effort\") for each case's indices",
               fixed = TRUE)
  expect_true(any(grepl("Long-string: not applied | Antonym: ", effort, fixed = TRUE)))
})


test_that("the summary explains every flag in a sentence (#145)", {
  pairs <- data.frame(item1 = c("a", "a", "a", "a", "a"),
                      item2 = c("p", "q", "r", "s", "t"), r = -0.1)
  expect_identical(nomologR:::nomo_screen_pair_text("a", pairs),
                   "Its correlations with p, q, r, and 2 other items are negative.")
  expect_identical(nomologR:::nomo_screen_pair_text("a", pairs[1:2, ]),
                   "Its correlations with p and q are negative.")
  expect_identical(
    nomologR:::nomo_screen_pair_text("a", pairs[1:2, ], unrecoded = c("p", "q")),
    paste("Its correlations with p and q are negative. p and q are declared reverse-keyed",
          "and not yet recoded in the data, so recode them first and read these",
          "correlations again.")
  )
  expect_match(nomologR:::nomo_screen_pair_text("a", pairs, unrecoded = "t"),
               "2 other items are negative. t is declared reverse-keyed", fixed = TRUE)
  expect_match(nomologR:::nomo_screen_pair_text("a", NULL), "see x$inter_item_correlations",
               fixed = TRUE)
  expect_match(nomologR:::nomo_screen_pair_text("z", pairs), "Some of its inter-item",
               fixed = TRUE)
  expect_identical(nomologR:::nomo_screen_unit_label(c("cases", "item_keying")),
                   c("Cases", "Item keying"))

  # Unused categories of an ordered item.
  local_reproducible_output(width = 80)
  ord <- ordered(c("low", "high", "low", "high", "low", "high"), levels = c("low", "mid", "high"))
  text <- paste(capture.output(print(summary(nomo_screen(data.frame(ord = ord, x = 1:6))))),
                collapse = " ")
  expect_match(text, "- ord (Review): It leaves 1 declared response category unused.",
               fixed = TRUE)
})


test_that("the review's item-rest value reads old and new screens alike", {
  review <- data.frame(corrected_item_rest_r = c(.5, .4, -.1),
                       scale_item_rest_r = c(.6, NA, NA),
                       scale = c("A", "C", NA))
  expect_identical(nomologR:::nomo_screen_review_item_rest(review), c(.6, NA, -.1))
  review$scale <- NULL
  expect_identical(nomologR:::nomo_screen_review_item_rest(review), c(.6, .4, -.1))
})


test_that("plots draw status by shape and color, with plain titles (#145)", {
  out <- nomo_screen(data.frame(item1 = 1:6, item2 = c(1, 2, 2, 4, 5, 6),
                                reverse_candidate = 6:1))
  evidence <- plot(out)
  expect_identical(evidence$labels$title, "Item evidence map")
  expect_true(all(c("status", "point_size") %in% names(evidence$data)))
  expect_identical(levels(evidence$data$status),
                   c("none", "review", "concern", "not computed"))
  expect_true("Coding" %in% levels(evidence$data$metric_label))

  item_rest <- plot(out, type = "item_rest")
  expect_true(any(vapply(item_rest$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
  expect_identical(item_rest$labels$subtitle, "Each item against the sum of the other items.")
  expect_match(plot_text(item_rest$labels$caption), "dashed line marks the .30 teaching reference",
               fixed = TRUE)
  bare <- out
  bare$guidance$item_total_reference <- NULL
  expect_null(plot(bare, type = "item_rest")$labels$caption)

  inter <- plot(out, type = "interitem")
  expect_identical(inter$labels$fill, "Pearson r")
})


test_that("a screen saved before declared scales still names its negative pairs", {
  out <- nomo_screen(data.frame(item1 = 1:6, item2 = c(1, 2, 2, 4, 5, 6),
                                reverse_candidate = 6:1))
  out$relationship_summary$scale <- NULL
  out$relationship_summary$scale_item_rest_r <- NULL
  pairs <- summary(out)$negative_pairs
  expect_identical(nrow(pairs), 2L)
  expect_true(all(pairs$r < 0))
  # Its summary still prints, with the pooled note and no Scale note.
  local_reproducible_output(width = 80)
  printed <- capture.output(print(summary(out)))
  expect_true(
    "  Item-rest r is the correlation of an item with the sum of the other items." %in% printed
  )
  expect_false(any(grepl("In Scale", printed, fixed = TRUE)))
  expect_match(paste(printed, collapse = " "), "reverse_candidate (Review)", fixed = TRUE)
})
