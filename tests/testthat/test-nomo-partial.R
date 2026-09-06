test_that("nomo_partial records explicit releases and rationales", {
  p <- nomo_partial(
    level = c("metric", "scalar"),
    syntax = c("F =~ x2", "x3 ~ 1"),
    rationale = c(
      "Loading release was prespecified.",
      "Intercept release was justified by prior evidence."
    )
  )

  expect_s3_class(p, "nomo_partial")
  expect_equal(p$n, 2L)
  expect_equal(p$releases$level, c("metric", "scalar"))
  expect_output(print(p), "No release was selected automatically")
})


test_that("nomo_partial validates researcher specifications", {
  expect_error(
    nomo_partial("configural", "F =~ x2", "reason"),
    "Unknown"
  )
  expect_error(
    nomo_partial("metric", "", "reason"),
    "blank"
  )
  expect_error(
    nomo_partial(
      c("metric", "metric"),
      c("F =~ x2", "F =~ x2"),
      "reason"
    ),
    "specified more than once"
  )
})
