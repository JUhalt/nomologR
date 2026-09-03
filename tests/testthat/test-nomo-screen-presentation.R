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
