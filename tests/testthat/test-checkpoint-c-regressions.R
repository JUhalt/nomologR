test_that("M6 and M7 exported entry points are implemented", {
  expect_true(is.function(nomo_hypotheses))
  expect_true(is.function(nomo_network))
  expect_true(is.function(nomo_invariance))
  expect_true(is.function(nomo_partial))
  expect_true(is.function(nomo_table))
})


test_that("hypothesis language preserves researcher provenance", {
  h <- nomo_hypotheses(
    "A -> B" = positive(origin = "a_priori"),
    "A <-> C" = negligible(
      within = c(-.10, .10),
      origin = "post_hoc"
    )
  )

  tab <- nomo_table(h)

  expect_equal(tab$origin, c("a_priori", "post_hoc"))
  expect_equal(tab$confirmable, c(TRUE, TRUE))
})


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


test_that("report-ready table generic refuses unsupported classes normally", {
  expect_error(
    nomo_table(list(a = 1)),
    "no applicable method",
    ignore.case = TRUE
  )
})
