test_that("nomo_validity matches direct semTools AVE, HTMT2, and HTMT", {
  set.seed(5201)
  n <- 1000
  f1 <- rnorm(n)
  f2 <- .35 * f1 + sqrt(1 - .35^2) * rnorm(n)
  dat <- data.frame(
    A1 = .82 * f1 + rnorm(n, sd = .55),
    A2 = .78 * f1 + rnorm(n, sd = .60),
    A3 = .75 * f1 + rnorm(n, sd = .62),
    A4 = .80 * f1 + rnorm(n, sd = .58),
    B1 = .82 * f2 + rnorm(n, sd = .55),
    B2 = .78 * f2 + rnorm(n, sd = .60),
    B3 = .75 * f2 + rnorm(n, sd = .62),
    B4 = .80 * f2 + rnorm(n, sd = .58)
  )
  model <- '
    F1 =~ A1 + A2 + A3 + A4
    F2 =~ B1 + B2 + B3 + B4
  '
  fit <- lavaan::cfa(model, data = dat)

  out <- nomo_validity(fit, htmt = "both")
  direct_ave <- semTools::AVE(fit, obs.var = TRUE, return.df = TRUE)
  direct_htmt2 <- semTools::htmt(
    model = model,
    data = dat,
    missing = "default",
    absolute = TRUE,
    htmt2 = TRUE
  )
  direct_htmt <- semTools::htmt(
    model = model,
    data = dat,
    missing = "default",
    absolute = TRUE,
    htmt2 = FALSE
  )

  expect_s3_class(out, "nomo_validity")
  expect_equal(out$ave_engine, direct_ave)
  expect_equal(out$htmt2_matrix, direct_htmt2, tolerance = 1e-8)
  expect_equal(out$htmt_matrix, direct_htmt, tolerance = 1e-8)
  expect_equal(nrow(out$standardized_loadings), 8L)
  expect_equal(nrow(out$latent_correlations), 1L)
  expect_true(any(out$references$topic == "HTMT2"))
})


test_that("nomo_validity accepts nomo_cfa and keeps AVE conceptually separate", {
  model <- '
    F1 =~ x1 + x2 + x3
    F2 =~ x4 + x5 + x6
  '
  wrapped <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
  out <- nomo_validity(wrapped, htmt = "none")

  expect_identical(out$source, "nomo_cfa")
  expect_true(nrow(out$ave) >= 2L)
  expect_true(any(out$decision_log$metric == "evidence_strategy"))
  expect_true(all(out$htmt_status$reason == "Not requested."))
})


test_that("weak convergent structure is reviewed without automatic item deletion", {
  set.seed(5202)
  n <- 1000
  f <- rnorm(n)
  dat <- data.frame(
    i1 = .35 * f + rnorm(n),
    i2 = .38 * f + rnorm(n),
    i3 = .33 * f + rnorm(n),
    i4 = .36 * f + rnorm(n)
  )
  fit <- lavaan::cfa('F =~ i1 + i2 + i3 + i4', data = dat)
  out <- nomo_validity(fit, htmt = "none")

  expect_true(any(out$ave$attention == "review"))
  expect_true(any(out$standardized_loadings$attention == "REVIEW"))
  expect_false(any(grepl("automatically delete", out$decision_log$decision, ignore.case = TRUE)))
})


test_that("nearly redundant factors trigger HTMT-family review rather than a validity verdict", {
  set.seed(5203)
  n <- 1800
  f1 <- rnorm(n)
  f2 <- .95 * f1 + sqrt(1 - .95^2) * rnorm(n)
  dat <- data.frame(
    A1 = .88 * f1 + rnorm(n, sd = .45),
    A2 = .85 * f1 + rnorm(n, sd = .48),
    A3 = .86 * f1 + rnorm(n, sd = .47),
    B1 = .88 * f2 + rnorm(n, sd = .45),
    B2 = .85 * f2 + rnorm(n, sd = .48),
    B3 = .86 * f2 + rnorm(n, sd = .47)
  )
  model <- '
    F1 =~ A1 + A2 + A3
    F2 =~ B1 + B2 + B3
  '
  fit <- lavaan::cfa(model, data = dat)
  out <- nomo_validity(fit, htmt = "both")

  expect_true(any(out$discriminant$estimate > out$htmt_reference))
  expect_true(any(out$discriminant$attention == "review"))
  expect_false(any(grepl("validity (passed|failed)|valid|invalid", out$discriminant$interpretation, ignore.case = TRUE)))
})


test_that("legacy Fornell-Larcker evidence is opt-in and labeled supporting", {
  model <- '
    F1 =~ x1 + x2 + x3
    F2 =~ x4 + x5 + x6
  '
  fit <- lavaan::cfa(model, data = lavaan::HolzingerSwineford1939)

  default <- nomo_validity(fit, htmt = "none")
  legacy <- nomo_validity(fit, htmt = "none", fornell_larcker = TRUE)

  expect_null(default$fornell_larcker)
  expect_true(is.matrix(legacy$fornell_larcker))
  expect_true(nrow(legacy$fornell_larcker_pairs) >= 1L)
  expect_true(any(legacy$decision_log$metric == "Fornell-Larcker"))
  expect_true(any(grepl("legacy|supporting", legacy$decision_log$observation, ignore.case = TRUE)))
})


test_that("cross-loaded indicators remain inspectable but HTMT is not forced", {
  set.seed(5204)
  n <- 1400
  f1 <- rnorm(n)
  f2 <- .30 * f1 + sqrt(1 - .30^2) * rnorm(n)
  dat <- data.frame(
    A1 = .82 * f1 + rnorm(n, sd = .55),
    A2 = .80 * f1 + rnorm(n, sd = .58),
    X  = .50 * f1 + .45 * f2 + rnorm(n, sd = .60),
    B1 = .82 * f2 + rnorm(n, sd = .55),
    B2 = .80 * f2 + rnorm(n, sd = .58)
  )
  model <- '
    F1 =~ A1 + A2 + X
    F2 =~ X + B1 + B2
  '
  fit <- lavaan::cfa(model, data = dat)
  expect_true(lavaan::lavInspect(fit, "converged"))

  out <- suppressWarnings(nomo_validity(fit, htmt = "both"))

  expect_true("X" %in% out$cross_loaded_items)
  expect_true(all(!out$htmt_status$available))
  expect_true(any(grepl("cross-loaded", out$htmt_status$reason)))
  expect_true(any(out$decision_log$metric == "cross_loading"))
})


test_that("ordinal validity evidence matches semTools engines", {
  set.seed(5205)
  n <- 1200
  f1 <- rnorm(n)
  f2 <- .30 * f1 + sqrt(1 - .30^2) * rnorm(n)
  z <- data.frame(
    A1 = .85 * f1 + rnorm(n, sd = .55),
    A2 = .80 * f1 + rnorm(n, sd = .60),
    A3 = .78 * f1 + rnorm(n, sd = .62),
    B1 = .85 * f2 + rnorm(n, sd = .55),
    B2 = .80 * f2 + rnorm(n, sd = .60),
    B3 = .78 * f2 + rnorm(n, sd = .62)
  )
  dat <- as.data.frame(lapply(z, function(x) {
    ordered(cut(x, breaks = c(-Inf, -.75, 0, .75, Inf), labels = FALSE))
  }))
  model <- '
    F1 =~ A1 + A2 + A3
    F2 =~ B1 + B2 + B3
  '
  fit <- lavaan::cfa(model, data = dat, ordered = names(dat))
  out <- nomo_validity(fit, htmt = "htmt2")

  direct_ave <- semTools::AVE(fit, obs.var = TRUE, return.df = TRUE)
  direct_htmt2 <- semTools::htmt(
    model = model,
    data = dat,
    missing = "default",
    ordered = names(dat),
    absolute = TRUE,
    htmt2 = TRUE
  )

  expect_equal(out$ave_engine, direct_ave)
  expect_equal(out$htmt2_matrix, direct_htmt2, tolerance = 1e-8)
  expect_setequal(out$ordered, names(dat))
})


test_that("multi-group validity does not silently pool HTMT", {
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  '
  fit <- lavaan::cfa(
    model,
    data = lavaan::HolzingerSwineford1939,
    group = "school"
  )
  out <- nomo_validity(fit, htmt = "both")

  expect_equal(out$ngroups, 2L)
  expect_true(nrow(out$ave) >= 4L)
  expect_true(all(!out$htmt_status$available))
  expect_true(any(grepl("not silently pooled", out$htmt_status$reason)))
})


test_that("validity argument checks are explicit", {
  expect_error(nomo_validity(NULL, ave_obs_var = NA), "ave_obs_var")
  expect_error(nomo_validity(NULL, htmt = "wrong"), "arg")
  expect_error(nomo_validity(NULL, htmt_missing = ""), "htmt_missing")
  expect_error(nomo_validity(NULL, htmt_missing = "magic"), "must be one of")
  expect_error(nomo_validity(NULL, fornell_larcker = NA), "fornell_larcker")
})


test_that("single-factor AVE retains the CFA construct name", {
  set.seed(5211)
  n <- 850
  f <- rnorm(n)
  dat <- data.frame(
    W1 = .45 * f + rnorm(n),
    W2 = .48 * f + rnorm(n),
    W3 = .43 * f + rnorm(n),
    W4 = .46 * f + rnorm(n)
  )
  fit <- lavaan::cfa('Weak =~ W1 + W2 + W3 + W4', data = dat)
  out <- nomo_validity(fit, htmt = "none")

  expect_identical(out$ave$construct, "Weak")
})
