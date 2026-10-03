test_that("nomo_defaults returns stable teaching guidance", {
  x <- nomo_defaults()

  expect_identical(x$profile, "teaching")
  expect_equal(x$item_total_reference, 0.30)
  # The fewest screened items at which long-string becomes a flag (#145).
  expect_identical(x$long_string_min_items, 20L)
  expect_equal(x$efa_loading_reference, 0.40)
  expect_equal(x$fit_reference$cfi, 0.95)
  expect_false(x$auto_delete)
  expect_false(x$auto_respecify)
  expect_match(x$interpretation, "do not automatically")
})

test_that("unknown guidance profiles are rejected", {
  expect_error(nomo_defaults("automatic-delete"))
})

test_that("auto_delete and auto_respecify cannot be switched on (#145)", {
  # They record a design rule; setting either to TRUE was silently ignored.
  check <- nomologR:::nomo_defaults_check_safeguards
  expect_identical(check(nomo_defaults()), nomo_defaults())
  expect_silent(check(list()))

  guidance <- nomo_defaults()
  guidance$auto_delete <- TRUE
  expect_error(check(guidance), "`guidance$auto_delete` cannot be `TRUE`", fixed = TRUE)
  guidance$auto_respecify <- TRUE
  expect_error(
    check(guidance),
    "`guidance$auto_delete` or `guidance$auto_respecify` cannot be `TRUE`: nomologR never",
    fixed = TRUE
  )

  dat <- nomo_demo_continuous[1:150, paste0("a", 1:5)]
  expect_error(nomo_efa(dat, factors = 1, guidance = guidance), "never deletes an item")
  expect_error(nomo_factors(dat, n_iter = 10, guidance = guidance), "never deletes an item")
})
