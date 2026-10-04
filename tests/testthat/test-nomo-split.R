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


test_that("nomo_split leaves no seed behind when the caller had none", {
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit(if (had) assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
  if (had) rm(".Random.seed", envir = .GlobalEnv)
  invisible(nomo_split(data.frame(x = 1:20), seed = 1))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
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
  local_reproducible_output(width = 80)
  txt <- capture.output(print(out))

  expect_true("Rows: 200 | Calibration: 100 | Validation: 100" %in% txt)
  # Proportions without the leading zero (#144).
  expect_true(any(grepl("Validation proportion: .50 requested, .50 realized", txt, fixed = TRUE)))
  # The sentence wraps at the console width, so search the joined text.
  expect_true(grepl("loss of precision", paste(txt, collapse = " ")))
  expect_identical(txt[[length(txt)]], "See x$assignment for the sample each row went to.")

  # A realized proportion that rounds to the requested one shows the decimals
  # that tell them apart.
  odd <- nomo_split(data.frame(x = 1:201), validation_prop = .50, seed = 7)
  expect_output(print(odd), ".50 requested, .498 realized", fixed = TRUE)
})


test_that("a seed beyond the integer range gets the argument's own message (#145)", {
  for (seed in list(1e10, 2^31, -2^31, Inf, NA_real_, "1", c(1, 2))) {
    expect_error(nomo_split(data.frame(x = 1:10), seed = seed),
                 "`seed` must be one finite integer.", fixed = TRUE)
  }
  expect_s3_class(nomo_split(data.frame(x = 1:10), seed = .Machine$integer.max), "nomo_split")
})


test_that("the split records the random-number generator its seed depends on (#145)", {
  dat <- data.frame(x = 1:120)
  default <- nomo_split(dat, seed = 7)
  # The three documented kinds, on every R version (R-devel adds a fourth).
  expect_identical(default$rng_kind, RNGkind()[1:3])
  expect_match(default$decision_log$observation[[1L]], "using seed 7 with RNGkind() Mersenne-Twister",
               fixed = TRUE)
  expect_output(print(default), "Generator: Mersenne-Twister, Inversion, Rejection", fixed = TRUE)

  # Under another sampler the same seed gives another split, which the
  # recorded kinds reproduce.
  old <- RNGkind()
  on.exit(suppressWarnings(RNGkind(old[[1L]], old[[2L]], old[[3L]])), add = TRUE)
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  rounding <- nomo_split(dat, seed = 7)
  expect_identical(rounding$rng_kind[[3L]], "Rounding")
  expect_false(identical(rounding$validation_rows, default$validation_rows))
  suppressWarnings(RNGkind(default$rng_kind[[1L]], default$rng_kind[[2L]], default$rng_kind[[3L]]))
  expect_identical(nomo_split(dat, seed = 7)$validation_rows, default$validation_rows)
})
