test_that("nomo_defaults returns stable teaching guidance", {
  x <- nomo_defaults()

  expect_identical(x$profile, "teaching")
  expect_equal(x$item_total_reference, 0.30)
  expect_equal(x$efa_loading_reference, 0.40)
  expect_equal(x$fit_reference$cfi, 0.95)
  expect_false(x$auto_delete)
  expect_false(x$auto_respecify)
  expect_match(x$interpretation, "do not automatically")
})

test_that("unknown guidance profiles are rejected", {
  expect_error(nomo_defaults("automatic-delete"))
})
