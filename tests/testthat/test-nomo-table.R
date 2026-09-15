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
