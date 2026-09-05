# Milestone 4D: CFA stress tests --------------------------------------------

nomo_test_two_factor_data <- function(n = 800L, seed = 4201L,
                                      cross_loading = 0,
                                      residual_pair = 0) {
  set.seed(seed)
  f1 <- rnorm(n)
  f2 <- 0.35 * f1 + sqrt(1 - 0.35^2) * rnorm(n)

  shared_residual <- rnorm(n)
  e <- replicate(8, rnorm(n))

  data.frame(
    A1 = .82 * f1 + .55 * e[, 1],
    A2 = .78 * f1 + residual_pair * shared_residual + .55 * e[, 2],
    A3 = .75 * f1 + residual_pair * shared_residual + .60 * e[, 3],
    A4 = .78 * f1 + cross_loading * f2 + .55 * e[, 4],
    B1 = .82 * f2 + .55 * e[, 5],
    B2 = .78 * f2 + .60 * e[, 6],
    B3 = .75 * f2 + .62 * e[, 7],
    B4 = .80 * f2 + .58 * e[, 8]
  )
}


test_that("correctly specified continuous CFA remains well behaved", {
  dat <- nomo_test_two_factor_data(n = 900, seed = 4202)
  model <- '
    F1 =~ A1 + A2 + A3 + A4
    F2 =~ B1 + B2 + B3 + B4
  '

  out <- nomo_cfa(model, dat, modification_indices = TRUE)

  expect_true(out$converged)
  expect_false(out$heywood_detected)
  expect_true(all(out$standardized_loadings$attention == "KEEP"))

  core <- out$fit_evidence[out$fit_evidence$metric %in% c(
    "CFI", "TLI", "RMSEA", "SRMR"
  ), , drop = FALSE]
  expect_true(sum(core$attention == "review", na.rm = TRUE) <= 1)
  expect_equal(
    out$decision_log$value[
      out$decision_log$metric == "automatic_respecification"
    ],
    0
  )
})


test_that("omitted cross-loading produces visible strain without automatic refit", {
  dat <- nomo_test_two_factor_data(
    n = 1200,
    seed = 4203,
    cross_loading = .45
  )
  model <- '
    F1 =~ A1 + A2 + A3 + A4
    F2 =~ B1 + B2 + B3 + B4
  '

  out <- nomo_cfa(model, dat, modification_indices = TRUE, mi_top = 50)

  expect_true(out$converged)

  mi <- out$modification_indices
  candidate <- mi[
    mi$lhs == "F2" & mi$op == "=~" & mi$rhs == "A4",
    , drop = FALSE
  ]

  expect_true(nrow(candidate) >= 1)
  expect_true(max(candidate$mi, na.rm = TRUE) > 10)
  expect_true(any(out$fit_evidence$attention == "review"))
  expect_equal(
    out$decision_log$value[
      out$decision_log$metric == "automatic_respecification"
    ],
    0
  )
})


test_that("omitted correlated residual is localized but not automatically freed", {
  dat <- nomo_test_two_factor_data(
    n = 1200,
    seed = 4204,
    residual_pair = .70
  )
  model <- '
    F1 =~ A1 + A2 + A3 + A4
    F2 =~ B1 + B2 + B3 + B4
  '

  out <- nomo_cfa(model, dat, modification_indices = TRUE, mi_top = 50)

  expect_true(out$converged)

  mi <- out$modification_indices
  candidate <- mi[
    mi$lhs == "A2" & mi$op == "~~" & mi$rhs == "A3" |
      mi$lhs == "A3" & mi$op == "~~" & mi$rhs == "A2",
    , drop = FALSE
  ]

  expect_true(nrow(candidate) >= 1)
  expect_true(max(candidate$mi, na.rm = TRUE) > 10)

  residual_pair <- out$residual_pairs[
    out$residual_pairs$item1 %in% c("A2", "A3") &
      out$residual_pairs$item2 %in% c("A2", "A3"),
    , drop = FALSE
  ]
  expect_true(nrow(residual_pair) >= 1)
  expect_true(all(is.finite(residual_pair$abs_residual)))

  # Residual-correlation magnitude is not sample-size invariant and the fitted
  # factor can absorb some shared residual covariance. The stronger, more
  # reproducible localization test is therefore that the target pair is more
  # discrepant than a typical residual pair while its MI is clearly elevated.
  expect_true(
    max(residual_pair$abs_residual, na.rm = TRUE) >
      stats::median(out$residual_pairs$abs_residual, na.rm = TRUE)
  )
  expect_equal(
    out$decision_log$value[
      out$decision_log$metric == "automatic_respecification"
    ],
    0
  )
})


test_that("gross one-factor misspecification triggers global and local review", {
  dat <- nomo_test_two_factor_data(n = 900, seed = 4205)
  model <- 'General =~ A1 + A2 + A3 + A4 + B1 + B2 + B3 + B4'

  out <- nomo_cfa(model, dat, modification_indices = TRUE)

  expect_true(out$converged)
  core <- out$fit_evidence[out$fit_evidence$metric %in% c(
    "CFI", "TLI", "RMSEA", "SRMR"
  ), , drop = FALSE]

  expect_true(sum(core$attention == "review", na.rm = TRUE) >= 2)
  expect_true(nrow(out$residual_pairs) > 0)
  expect_true(out$residual_pairs$abs_residual[[1]] > .10)
  expect_true(any(grepl(
    "fit_",
    out$decision_log$metric,
    fixed = TRUE
  )))
})


test_that("ordinal WLSMV wrapper reproduces direct lavaan standardized loadings", {
  set.seed(4206)
  n <- 700
  f1 <- rnorm(n)
  f2 <- .30 * f1 + sqrt(1 - .30^2) * rnorm(n)

  latent <- data.frame(
    q1 = .85 * f1 + rnorm(n, sd = .55),
    q2 = .80 * f1 + rnorm(n, sd = .60),
    q3 = .75 * f1 + rnorm(n, sd = .65),
    q4 = .85 * f2 + rnorm(n, sd = .55),
    q5 = .80 * f2 + rnorm(n, sd = .60),
    q6 = .75 * f2 + rnorm(n, sd = .65)
  )

  dat <- as.data.frame(lapply(latent, function(z) {
    ordered(cut(
      z,
      breaks = c(-Inf, -.75, 0, .75, Inf),
      labels = c("1", "2", "3", "4")
    ))
  }))

  model <- '
    F1 =~ q1 + q2 + q3
    F2 =~ q4 + q5 + q6
  '

  out <- nomo_cfa(
    model,
    dat,
    ordered = names(dat),
    modification_indices = FALSE
  )
  direct <- lavaan::cfa(
    model,
    data = dat,
    ordered = names(dat),
    estimator = "WLSMV"
  )

  expect_true(out$converged)
  expect_equal(out$estimator, "WLSMV")

  direct_std <- lavaan::standardizedSolution(direct, type = "std.all")
  direct_load <- direct_std[direct_std$op == "=~", "est.std"]
  expect_equal(
    out$standardized_loadings$loading,
    direct_load,
    tolerance = 1e-7
  )
})


test_that("advanced optimizer control makes nonconvergence reproducible and visible", {
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '

  out <- nomo_cfa(
    model,
    data = lavaan::HolzingerSwineford1939,
    control = list(iter.max = 1L),
    modification_indices = TRUE
  )

  expect_false(out$converged)
  expect_identical(out$control$iter.max, 1L)
  expect_true(length(out$engine_warnings) >= 1)
  expect_equal(nrow(out$modification_indices), 0)

  convergence <- out$decision_log[
    out$decision_log$metric == "convergence",
    , drop = FALSE
  ]
  expect_equal(convergence$severity, "concern")

  optimizer <- out$decision_log[
    out$decision_log$metric == "optimizer_control",
    , drop = FALSE
  ]
  expect_equal(nrow(optimizer), 1)
  expect_equal(optimizer$severity, "review")
})


test_that("optimizer control input is validated", {
  dat <- lavaan::HolzingerSwineford1939
  model <- 'visual =~ x1 + x2 + x3'

  expect_error(
    nomo_cfa(model, dat, control = 1),
    "named list"
  )
  expect_error(
    nomo_cfa(model, dat, control = list(1)),
    "named list"
  )
  expect_error(
    nomo_cfa(model, dat, control = stats::setNames(list(1), "")),
    "named list"
  )
})
