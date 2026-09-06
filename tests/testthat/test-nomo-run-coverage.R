test_that("nomo_run internal validators cover consequential failure branches", {
  dat <- make_m8_full_data(n = 120L, seed = 8601L)
  roles <- nomologR:::nomo_run_data_roles(dat)
  scales <- list(WellBeing = c("i1", "i2", "i3", "i4"))

  expect_error(
    nomologR:::nomo_run_data_roles(data.frame()),
    "non-empty"
  )

  expect_error(
    nomologR:::nomo_run_validate_scales(list(), roles),
    "non-empty"
  )
  expect_error(
    nomologR:::nomo_run_validate_scales(
      list(WellBeing = c("i1", "i1")),
      roles
    ),
    "unique"
  )

  expect_error(
    nomologR:::nomo_run_validate_settings(
      list(unknown = list()),
      scales
    ),
    "Unknown"
  )
  expect_error(
    nomologR:::nomo_run_validate_settings(
      list(factors = 1),
      scales
    ),
    "must be a list"
  )
  expect_error(
    nomologR:::nomo_run_validate_settings(
      list(factors = list(types = c(nope = "ordinal"))),
      scales
    ),
    "outside"
  )

  expect_error(
    nomologR:::nomo_run_validate_decisions(list(bad = 1)),
    "Unsupported"
  )

  expect_error(
    nomologR:::nomo_run_normalize_factor_counts(0, scales),
    "positive integer"
  )
  expect_error(
    nomologR:::nomo_run_normalize_factor_counts(4, scales),
    "smaller"
  )

  expect_error(
    nomologR:::nomo_run_normalize_cfa_model(1),
    "measurement-model"
  )
  expect_error(
    nomologR:::nomo_run_normalize_measurement_decision("maybe"),
    "proceed"
  )
})


test_that("structured decision and rationale validation branches are covered", {
  expect_error(
    nomologR:::nomo_run_unpack_decision(
      list(value = 1L, rationale = "x", extra = TRUE)
    ),
    "only"
  )

  expect_error(
    nomologR:::nomo_run_normalize_rationale(
      c(A = "x"),
      c("A", "B")
    ),
    "matching"
  )

  expect_equal(
    nomologR:::nomo_run_normalize_measurement_decision(" PROCEED "),
    "proceed"
  )
})


test_that("overlapping supplied scale membership is retained and logged", {
  set.seed(8602L)
  f <- rnorm(180)
  dat <- data.frame(
    i1 = f + rnorm(180),
    i2 = f + rnorm(180),
    i3 = f + rnorm(180),
    i4 = f + rnorm(180),
    i5 = f + rnorm(180)
  )

  run <- nomo_run(
    data = dat,
    scales = list(
      A = c("i1", "i2", "i3"),
      B = c("i3", "i4", "i5")
    ),
    settings = list(
      factors = list(
        criterion_set = "minimal",
        n_iter = 10L,
        seed = 2026L
      )
    )
  )

  expect_true(any(run$decision_log$id == "overlapping_items"))
  expect_equal(run$scales$A, c("i1", "i2", "i3"))
  expect_equal(run$scales$B, c("i3", "i4", "i5"))
})


test_that("resume protects source data scales and guidance", {
  dat <- make_m8_full_data(n = 180L, seed = 8603L)

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings()
  )

  expect_error(
    nomo_run(
      resume = run,
      data = dat
    ),
    "Do not supply"
  )

  g <- nomo_defaults()
  g$factor_small_n_reference <- g$factor_small_n_reference + 1

  expect_error(
    nomo_run(
      resume = run,
      guidance = g
    ),
    "Guidance cannot"
  )

  expect_error(
    nomo_run(
      resume = list()
    ),
    "nomo_run"
  )
})


test_that("blocked result presentation and table branches are retained", {
  dat <- make_m8_full_data(n = 180L, seed = 8604L)
  dat$i4 <- 1

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings()
  )

  expect_equal(run$status, "blocked")
  expect_output(print(run), "blocked", ignore.case = TRUE)
  expect_s3_class(nomo_table(run, "requests"), "data.frame")
  expect_s3_class(nomo_table(run, "scales"), "data.frame")
})
