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
      "decision_log", "guidance"
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
