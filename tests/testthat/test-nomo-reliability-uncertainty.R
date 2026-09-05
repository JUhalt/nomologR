test_that("bootstrap reliability intervals add uncertainty without changing point estimates", {
  set.seed(5401)
  n <- 450
  f <- rnorm(n)
  dat <- data.frame(
    i1 = .82 * f + rnorm(n, sd = .58),
    i2 = .78 * f + rnorm(n, sd = .62),
    i3 = .80 * f + rnorm(n, sd = .60),
    i4 = .76 * f + rnorm(n, sd = .65)
  )

  fit <- lavaan::cfa('F =~ i1 + i2 + i3 + i4', data = dat)
  point <- nomo_reliability(fit, ci = "none")
  boot <- nomo_reliability(
    fit,
    ci = "bootstrap",
    ci_boot = 20,
    ci_seed = 5401
  )

  expect_equal(boot$evidence$estimate, point$evidence$estimate, tolerance = 1e-10)
  expect_true(all(c("ci_lower", "ci_upper", "ci_n_success") %in% names(boot$evidence)))
  expect_true(any(is.finite(boot$evidence$ci_lower)))
  expect_true(any(is.finite(boot$evidence$ci_upper)))
  expect_equal(boot$ci_status$method, "bootstrap")
  expect_equal(boot$ci_status$requested_draws, 20L)
  expect_s3_class(plot(boot), "ggplot")
})


test_that("reliability CI arguments are validated", {
  fit <- lavaan::cfa(
    'F =~ x1 + x2 + x3',
    data = lavaan::HolzingerSwineford1939
  )
  expect_error(nomo_reliability(fit, ci_level = 1), "strictly between")
  expect_error(nomo_reliability(fit, ci_boot = 10), "at least 20")
  expect_error(nomo_reliability(fit, ci_seed = Inf), "finite integer")
})
