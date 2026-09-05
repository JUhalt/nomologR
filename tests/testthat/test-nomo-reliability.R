test_that("nomo_reliability matches direct semTools omega and alpha", {
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '
  fit <- lavaan::cfa(model, data = lavaan::HolzingerSwineford1939)

  out <- nomo_reliability(fit)

  direct_omega <- semTools::compRelSEM(
    fit,
    obs.var = TRUE,
    tau.eq = FALSE,
    ord.scale = TRUE,
    simplify = TRUE
  )
  direct_alpha <- semTools::compRelSEM(
    fit,
    obs.var = TRUE,
    tau.eq = TRUE,
    ord.scale = TRUE,
    simplify = TRUE
  )

  expect_s3_class(out, "nomo_reliability")
  expect_equal(out$omega_engine, direct_omega)
  expect_equal(out$alpha_engine, direct_alpha)
  expect_true(all(c("omega", "alpha") %in% unique(out$evidence$metric)))
  expect_false("ave" %in% names(out))
  expect_true(any(out$decision_log$metric == "coefficient_choice"))
  expect_true(any(out$decision_log$metric == "alpha_assumptions"))
})


test_that("nomo_reliability accepts nomo_cfa without changing engine estimates", {
  model <- '
    F1 =~ x1 + x2 + x3
    F2 =~ x4 + x5 + x6
  '
  wrapped <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
  out <- nomo_reliability(wrapped)

  direct <- semTools::compRelSEM(
    wrapped$fit,
    obs.var = TRUE,
    tau.eq = FALSE,
    ord.scale = TRUE,
    simplify = TRUE
  )

  expect_identical(out$source, "nomo_cfa")
  expect_equal(out$omega_engine, direct)
})


test_that("ordinal reliability matches semTools on the observed response scale", {
  set.seed(5101)
  n <- 700
  f <- rnorm(n)
  z <- data.frame(
    i1 = .82 * f + rnorm(n, sd = .60),
    i2 = .78 * f + rnorm(n, sd = .65),
    i3 = .74 * f + rnorm(n, sd = .68),
    i4 = .80 * f + rnorm(n, sd = .62)
  )

  dat <- as.data.frame(lapply(z, function(x) {
    ordered(cut(
      x,
      breaks = c(-Inf, -.7, 0, .7, Inf),
      labels = FALSE
    ))
  }))

  model <- 'F =~ i1 + i2 + i3 + i4'
  fit <- lavaan::cfa(model, data = dat, ordered = names(dat))

  out <- nomo_reliability(fit, ordinal_scale = TRUE)
  direct <- semTools::compRelSEM(
    fit,
    obs.var = TRUE,
    tau.eq = FALSE,
    ord.scale = TRUE,
    simplify = TRUE
  )

  expect_equal(out$omega_engine, direct)
  expect_setequal(out$ordered, names(dat))
  expect_true(any(out$decision_log$metric == "ordinal_scale"))
  expect_match(
    out$decision_log$observation[out$decision_log$metric == "ordinal_scale"],
    "actual ordinal-score scale"
  )
  expect_null(out$alpha_engine)
  expect_false(any(out$evidence$metric == "alpha"))
  expect_true(any(out$alpha_status$indicator_type == "ordered"))
  expect_true(all(!out$alpha_status$available[out$alpha_status$indicator_type == "ordered"]))
  expect_true(any(out$decision_log$metric == "alpha_availability"))
  expect_match(
    out$alpha_status$reason[out$alpha_status$indicator_type == "ordered"],
    "not computed from an ordered-indicator CFA"
  )
})


test_that("latent-response ordinal reliability is explicitly distinguished", {
  set.seed(5102)
  n <- 600
  f <- rnorm(n)
  z <- data.frame(
    i1 = .80 * f + rnorm(n, sd = .60),
    i2 = .78 * f + rnorm(n, sd = .62),
    i3 = .76 * f + rnorm(n, sd = .64),
    i4 = .74 * f + rnorm(n, sd = .66)
  )
  dat <- as.data.frame(lapply(z, function(x) ordered(cut(x, 4, labels = FALSE))))
  fit <- lavaan::cfa('F =~ i1 + i2 + i3 + i4', data = dat, ordered = names(dat))

  out <- nomo_reliability(fit, ordinal_scale = FALSE, include_alpha = FALSE)

  expect_null(out$alpha_engine)
  expect_false(any(out$evidence$metric == "alpha"))
  expect_match(
    out$decision_log$observation[out$decision_log$metric == "ordinal_scale"],
    "hypothetical continuous response composite"
  )

  out_alpha <- nomo_reliability(fit, ordinal_scale = FALSE, include_alpha = TRUE)
  direct_alpha <- semTools::compRelSEM(
    fit,
    obs.var = TRUE,
    tau.eq = TRUE,
    ord.scale = FALSE,
    simplify = TRUE
  )
  expect_equal(out_alpha$alpha_engine, direct_alpha)
  expect_true(any(out_alpha$evidence$metric == "alpha"))
  expect_true(all(out_alpha$alpha_status$score_scale == "latent_response"))
  expect_match(out_alpha$alpha_status$reason, "polychoric/latent-response scale")
})


test_that("weak reliability triggers review rather than a pass-fail verdict", {
  set.seed(5103)
  n <- 800
  f <- rnorm(n)
  dat <- data.frame(
    i1 = .30 * f + rnorm(n),
    i2 = .30 * f + rnorm(n),
    i3 = .30 * f + rnorm(n),
    i4 = .30 * f + rnorm(n)
  )
  fit <- lavaan::cfa('F =~ i1 + i2 + i3 + i4', data = dat)
  out <- nomo_reliability(fit)

  expect_true(any(out$evidence$attention == "review"))
  expect_false(any(grepl("pass|fail", out$evidence$interpretation, ignore.case = TRUE)))
  expect_true(any(grepl("intended use", out$evidence$interpretation, ignore.case = TRUE)))
})


test_that("model strain is carried into reliability interpretation", {
  set.seed(5104)
  n <- 1000
  f1 <- rnorm(n)
  f2 <- .35 * f1 + sqrt(1 - .35^2) * rnorm(n)
  dat <- data.frame(
    A1 = .8 * f1 + rnorm(n, sd = .55),
    A2 = .8 * f1 + rnorm(n, sd = .55),
    A3 = .8 * f1 + rnorm(n, sd = .55),
    B1 = .8 * f2 + rnorm(n, sd = .55),
    B2 = .8 * f2 + rnorm(n, sd = .55),
    B3 = .8 * f2 + rnorm(n, sd = .55)
  )

  poor <- lavaan::cfa('ONE =~ A1 + A2 + A3 + B1 + B2 + B3', data = dat)
  out <- nomo_reliability(poor)

  expect_true(out$model_strain)
  expect_true(any(out$decision_log$metric == "model_dependence"))
  expect_true(any(out$decision_log$severity[out$decision_log$metric == "model_dependence"] == "review"))
})


test_that("advanced and ambiguous models stop with an explanation", {
  fit_structural <- lavaan::sem(
    '
      F1 =~ x1 + x2 + x3
      F2 =~ x4 + x5 + x6
      F2 ~ F1
    ',
    data = lavaan::HolzingerSwineford1939
  )
  expect_error(nomo_reliability(fit_structural), "structural regressions")

  set.seed(5105)
  n <- 1200
  f1 <- rnorm(n)
  f2 <- .30 * f1 + sqrt(1 - .30^2) * rnorm(n)
  dat_cross <- data.frame(
    A1 = .82 * f1 + rnorm(n, sd = .55),
    A2 = .78 * f1 + rnorm(n, sd = .60),
    A3 = .76 * f1 + rnorm(n, sd = .62),
    X  = .60 * f1 + .40 * f2 + rnorm(n, sd = .55),
    B1 = .82 * f2 + rnorm(n, sd = .55),
    B2 = .78 * f2 + rnorm(n, sd = .60),
    B3 = .76 * f2 + rnorm(n, sd = .62)
  )
  fit_cross <- lavaan::cfa(
    '
      F1 =~ A1 + A2 + A3 + X
      F2 =~ X + B1 + B2 + B3
    ',
    data = dat_cross
  )
  expect_true(lavaan::lavInspect(fit_cross, "converged"))
  expect_error(nomo_reliability(fit_cross), "cross-loaded")
})


test_that("reliability input validation is explicit", {
  expect_error(nomo_reliability(data.frame(x = 1:3)), "nomo_cfa.*lavaan")
  expect_error(nomo_reliability(NULL, obs.var = NA), "obs.var")
  expect_error(nomo_reliability(NULL, ordinal_scale = NA), "ordinal_scale")
  expect_error(nomo_reliability(NULL, include_alpha = NA), "include_alpha")
})

test_that("reliability core is free of tidyselect deprecation warnings", {
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '
  fit <- lavaan::cfa(model, data = lavaan::HolzingerSwineford1939)

  expect_no_warning(nomo_reliability(fit))
})



test_that("single-factor reliability retains the CFA construct name and alpha availability", {
  set.seed(5106)
  n <- 900
  f <- rnorm(n)
  dat <- data.frame(
    W1 = .35 * f + rnorm(n),
    W2 = .38 * f + rnorm(n),
    W3 = .33 * f + rnorm(n),
    W4 = .36 * f + rnorm(n),
    W5 = .34 * f + rnorm(n)
  )
  fit <- lavaan::cfa('Weak =~ W1 + W2 + W3 + W4 + W5', data = dat)
  out <- nomo_reliability(fit)

  expect_setequal(unique(out$evidence$construct), "Weak")
  expect_setequal(unique(out$omega$construct), "Weak")
  expect_setequal(unique(out$alpha$construct), "Weak")
  expect_identical(out$alpha_status$construct, "Weak")
  expect_true(out$alpha_status$available)
  expect_match(out$alpha_status$reason, "Alpha was computed")
})
