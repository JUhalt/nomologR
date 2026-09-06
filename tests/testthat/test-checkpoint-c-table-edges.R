test_that("nomo_table generic covers hypothesis and unsupported-object behavior", {
  h <- nomo_hypotheses(
    "A -> B" = positive(),
    "A <-> C" = negligible(within = c(-.10, .10))
  )

  expect_identical(
    nomo_table(h),
    h$hypotheses
  )

  expect_error(
    nomo_table(list(a = 1)),
    "no applicable method",
    ignore.case = TRUE
  )
})
