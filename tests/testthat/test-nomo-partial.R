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


test_that("as.data.frame() of a nomo_partial object returns its releases (#145)", {
  p <- nomo_partial(
    level = c("metric", "scalar"),
    syntax = c("F =~ x2", "x3 ~ 1"),
    rationale = "Prespecified."
  )
  # Base R's method for a list evaluated the stored call again, and again for
  # the object that returned, until "node stack overflow". The call is swapped
  # for one that stops, so this test fails at once without the method.
  expect_true(is.call(p$call))
  p$call <- quote(stop("the stored call was evaluated"))

  out <- as.data.frame(p)
  expect_identical(class(out), "data.frame")
  expect_identical(out, as.data.frame(p$releases))
  expect_identical(names(out), c("release_id", "level", "syntax", "rationale"))
  expect_identical(out$syntax, c("F =~ x2", "x3 ~ 1"))
  expect_output(print(out), "x3 ~ 1", fixed = TRUE)
  expect_identical(data.frame(p)$level, c("metric", "scalar"))

  # The object is still a list with its documented fields.
  expect_true(is.list(p))
  expect_identical(p$n, 2L)
  expect_true(grepl("\\code{as.data.frame()} returns \\code{releases} as a data frame",
                    nomo_test_rd_text("nomo_partial", "\\value"), fixed = TRUE))
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


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("partial invariance always requires rationale", {
  expect_error(
    nomo_partial(
      level = "metric",
      syntax = "F =~ x2",
      rationale = ""
    ),
    "blank"
  )
})
