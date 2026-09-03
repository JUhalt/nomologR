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
