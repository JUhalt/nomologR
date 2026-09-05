test_that("nomo_split is reproducible, exhaustive, and non-overlapping", {
  dat <- data.frame(id = 1:200, x = rnorm(200))

  a <- nomo_split(dat, validation_prop = .30, seed = 4101)
  b <- nomo_split(dat, validation_prop = .30, seed = 4101)

  expect_s3_class(a, "nomo_split")
  expect_equal(a$validation_rows, b$validation_rows)
  expect_equal(a$n_calibration, 140)
  expect_equal(a$n_validation, 60)
  expect_equal(nrow(a$assignment), 200)
  expect_equal(sort(c(a$calibration_rows, a$validation_rows)), 1:200)
  expect_length(intersect(a$calibration_rows, a$validation_rows), 0)
  expect_equal(a$calibration$id, dat$id[a$calibration_rows])
  expect_equal(a$validation$id, dat$id[a$validation_rows])
})


test_that("nomo_split restores caller RNG state", {
  set.seed(9123)
  before <- .Random.seed
  invisible(nomo_split(data.frame(x = 1:120), seed = 99))
  expect_identical(.Random.seed, before)
})


test_that("nomo_split logs the precision tradeoff for small subsets", {
  out <- nomo_split(data.frame(x = 1:120), validation_prop = .50, seed = 1)

  expect_true(any(out$decision_log$metric == "validation_proportion"))
  expect_true(any(out$decision_log$metric == "split_sample_size"))
  expect_equal(
    out$decision_log$severity[out$decision_log$metric == "split_sample_size"],
    "review"
  )
})


test_that("nomo_split validates arguments", {
  expect_error(nomo_split(data.frame(x = 1)), "at least two rows")
  expect_error(nomo_split(data.frame(x = 1:10), 0), "strictly between")
  expect_error(nomo_split(data.frame(x = 1:10), 1), "strictly between")
  expect_error(nomo_split(data.frame(x = 1:10), seed = 1.5), "finite integer")
  expect_error(
    nomo_split(data.frame(x = 1:10), guidance = list()),
    "factor_small_n_reference"
  )
})


test_that("print.nomo_split communicates the design tradeoff", {
  out <- nomo_split(data.frame(x = 1:200), seed = 7)
  txt <- capture.output(print(out))

  expect_true(any(grepl("calibration", txt)))
  expect_true(any(grepl("validation", txt)))
  expect_true(any(grepl("loss of precision", txt)))
})
