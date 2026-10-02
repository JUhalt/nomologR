# Ordered factors that `ordered` does not name (#145) ---------------------------------

test_that("model indicators stored as ordered factors are treated as declared", {
  dat <- nomo_demo_ordinal
  model <- "A =~ a1 + a2 + a3 + a4"

  found <- nomo_ordered_indicators(model, dat)
  expect_identical(found$ordered, c("a1", "a2", "a3", "a4"))
  expect_identical(found$detected, c("a1", "a2", "a3", "a4"))

  # Declared columns stay first and are not reported as detected; columns the
  # model does not use are ignored.
  partly <- nomo_ordered_indicators(model, dat, ordered = c("a2", "a1"))
  expect_identical(partly$ordered, c("a2", "a1", "a3", "a4"))
  expect_identical(partly$detected, c("a3", "a4"))

  # Numeric columns are left alone, and a model that does not parse or names
  # absent columns detects nothing.
  expect_identical(
    nomo_ordered_indicators(model, nomo_demo_continuous),
    list(ordered = character(), detected = character())
  )
  expect_identical(nomo_ordered_indicators("A =~", dat)$detected, character())
  expect_identical(nomo_ordered_indicators("A =~ z1 + z2", dat)$detected, character())
})


test_that("the detection is logged for review, in the singular and the plural", {
  log <- nomo_ordered_detected_log(nomo_log_new(), character(), "cfa")
  expect_identical(nrow(log), 0L)

  one <- nomo_ordered_detected_log(nomo_log_new(), "a1", "cfa")
  expect_identical(one$metric, "ordered_detected")
  expect_identical(one$severity, "review")
  expect_identical(one$stage, "cfa")
  expect_identical(one$value, 1)
  expect_match(one$observation, "1 indicator is stored as an ordered factor", fixed = TRUE)
  expect_match(one$observation, "It is modeled as ordered, as lavaan fits it.", fixed = TRUE)

  two <- nomo_ordered_detected_log(nomo_log_new(), c("a1", "a2"), "invariance")
  expect_identical(two$object, "a1, a2")
  expect_match(two$observation, "2 indicators are stored as ordered factors", fixed = TRUE)
  expect_match(two$observation, "They are modeled as ordered, as lavaan fits them.", fixed = TRUE)
  expect_match(two$recommendation, "Name these columns in `ordered`", fixed = TRUE)
})
