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
