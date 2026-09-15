# Report-ready tables ------------------------------------------------------------

test_that("invariance local-strain tables add readable constraint labels", {
  skip_on_cran()

  inv <- nomo_invariance(
    "Agency =~ ag1 + ag2 + ag3 + ag4",
    data = nomo_demo_network,
    group = "group",
    levels = c("configural", "metric", "scalar")
  )
  strain <- nomo_table(inv, "local_strain")

  expect_s3_class(strain, "tbl_df")
  expect_true(all(c("constraint", "constraint_display") %in% names(strain)))
  expect_identical(strain$constraint, inv$local_strain$constraint)
  expect_false(any(grepl("^\\.p[0-9]+\\.", strain$constraint_display)))
})


test_that("empty invariance local-strain tables are returned unchanged", {
  empty <- structure(
    list(local_strain = tibble::tibble(), fits = list()),
    class = c("nomo_invariance", "list")
  )

  expect_identical(nomo_table(empty, "local_strain"), tibble::tibble())
})


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("report-ready table generic refuses unsupported classes normally", {
  expect_error(
    nomo_table(list(a = 1)),
    "no applicable method",
    ignore.case = TRUE
  )
})


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
