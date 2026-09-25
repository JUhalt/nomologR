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

# ---- recovered from hygiene consolidation: test-nomo-reliability.R ----
# ---- consolidated from test-nomo-reliability-uncertainty.R ----
test_that("bootstrap reliability intervals add uncertainty without changing point estimates", {
  skip_on_cran()
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

# ---- consolidated from test-coverage-sprint-reliability.R ----
# Pre-v0.1 coverage sprint: reliability presentation and uncertainty ----------

test_that("reliability summary table covers empty and missing-coefficient branches", {
  run <- make_m9_report_run()
  rel <- run$results$reliability

  empty <- rel
  empty$item_type_context <- empty$item_type_context[0, , drop = FALSE]
  expect_equal(nrow(nomologR:::nomo_reliability_summary_table(empty)), 0L)

  no_omega <- rel
  no_omega$evidence <- no_omega$evidence[
    no_omega$evidence$metric != "omega", , drop = FALSE
  ]
  out <- nomologR:::nomo_reliability_summary_table(no_omega)
  expect_gt(nrow(out), 0L)
  expect_true(all(is.na(out$omega)))
  expect_true(all(out$signal == "info"))

  no_alpha <- rel
  no_alpha$evidence <- no_alpha$evidence[
    no_alpha$evidence$metric != "alpha", , drop = FALSE
  ]
  out2 <- nomologR:::nomo_reliability_summary_table(no_alpha)
  expect_gt(nrow(out2), 0L)
  expect_true(all(is.na(out2$alpha)))
})


test_that("reliability summary signals and score scales cover ordered branches", {
  run <- make_m9_report_run()
  rel <- run$results$reliability

  ordered <- rel
  ordered$item_type_context$indicator_type[] <- "ordered"
  ordered$ordinal_scale <- TRUE
  ordered$evidence$attention[ordered$evidence$metric == "omega"] <- "concern"
  out <- nomologR:::nomo_reliability_summary_table(ordered)
  expect_true(all(out$omega_scale == "observed_ordinal"))
  expect_true(all(out$signal == "concern"))

  ordered$ordinal_scale <- FALSE
  ordered$evidence$attention[ordered$evidence$metric == "omega"] <- "review"
  out2 <- nomologR:::nomo_reliability_summary_table(ordered)
  expect_true(all(out2$omega_scale == "latent_response"))
  expect_true(all(out2$signal == "review"))

  ordered$evidence$attention[ordered$evidence$metric == "omega"] <- NA_character_
  out3 <- nomologR:::nomo_reliability_summary_table(ordered)
  expect_true(all(out3$signal == "info"))
})


test_that("reliability CI formatting and plot limits cover edge branches", {
  expect_true(is.na(
    nomologR:::nomo_reliability_ci_string(NA_real_, .1, .2)
  ))
  expect_equal(
    nomologR:::nomo_reliability_ci_string(.75, NA_real_, NA_real_),
    "0.750"
  )
  expect_equal(
    nomologR:::nomo_reliability_ci_string(.75, .60, .85),
    "0.750 [0.600, 0.850]"
  )

  expect_equal(nomologR:::nomo_plot_x_limits(numeric()), c(0, 1))
  expect_equal(
    nomologR:::nomo_plot_x_limits(c(-.12, 1.08)),
    c(-.15, 1.10)
  )
})


test_that("reliability print methods cover point, bootstrap, strain, and empty branches", {
  run <- make_m9_report_run()
  rel <- run$results$reliability

  point <- rel
  point$ci_status$method[] <- "none"
  txt <- paste(capture.output(print(point)), collapse = "\n")
  expect_match(txt, "point estimates only", fixed = TRUE)

  boot <- rel
  boot$ci_status <- tibble::tibble(
    method = "bootstrap",
    level = .95,
    requested_draws = 20L,
    min_successful_draws = 15L,
    available = TRUE,
    seed = 42L,
    reason = "Synthetic bootstrap qualification."
  )
  boot$model_strain <- TRUE
  txt2 <- paste(capture.output(print(boot)), collapse = "\n")
  expect_match(txt2, "percentile bootstrap CI", fixed = TRUE)
  expect_match(txt2, "minimum successful draws", fixed = TRUE)
  expect_match(txt2, "Bootstrap note", fixed = TRUE)
  expect_match(txt2, "Measurement-model context", fixed = TRUE)

  s <- summary(rel)
  empty <- s
  empty$table <- empty$table[0, , drop = FALSE]
  empty$ci_status$method[] <- "none"
  empty$model_strain <- TRUE
  empty$alpha_status$requested[] <- TRUE
  empty$alpha_status$available[] <- FALSE
  empty$alpha_status$reason[] <- "Synthetic unavailable alpha."

  txt3 <- paste(capture.output(print(empty)), collapse = "\n")
  expect_match(txt3, "No reliability coefficients", fixed = TRUE)
  expect_match(txt3, "Secondary alpha unavailable", fixed = TRUE)
  expect_match(txt3, "Sampling uncertainty was not bootstrapped", fixed = TRUE)
  expect_match(txt3, "requires review", fixed = TRUE)
})


test_that("reliability summary print reports bootstrap interval notation", {
  run <- make_m9_report_run()
  rel <- run$results$reliability
  s <- summary(rel)

  s$table$omega_ci_lower <- s$table$omega - .05
  s$table$omega_ci_upper <- s$table$omega + .05

  txt <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(txt, "Bracketed values are bootstrap confidence intervals", fixed = TRUE)
})


test_that("reliability plot covers errors, one-coefficient guides, CIs, and facets", {
  run <- make_m9_report_run()
  rel <- run$results$reliability

  empty <- rel
  empty$evidence$estimate[] <- NA_real_
  expect_error(plot(empty), "No finite reliability coefficients")

  one <- rel
  one$evidence <- one$evidence[one$evidence$metric == "omega", , drop = FALSE]
  one$evidence$ci_lower <- NULL
  one$evidence$ci_upper <- NULL
  p <- plot(one)
  expect_s3_class(p, "ggplot")

  ci <- rel
  ci$evidence$ci_lower <- ci$evidence$estimate - .05
  ci$evidence$ci_upper <- ci$evidence$estimate + .05
  ci$ci_level <- .95
  ci$evidence <- dplyr::bind_rows(
    ci$evidence,
    dplyr::mutate(ci$evidence, block = "second")
  )
  p2 <- plot(ci)
  expect_s3_class(p2, "ggplot")
  expect_match(p2$labels$subtitle, "bootstrap CIs", fixed = TRUE)
  expect_true(length(p2$facet$params$facets) >= 1L)
})


test_that("reliability bootstrap helper returns aligned point statistics for a fitted CFA", {
  run <- make_m9_report_run()
  rel <- run$results$reliability
  cfa <- run$results$cfa

  fit <- cfa$fit
  type_context <- rel$item_type_context
  keys <- paste(
    rel$evidence$metric,
    rel$evidence$construct,
    rel$evidence$block,
    sep = "::"
  )

  stat <- nomologR:::nomo_reliability_boot_stat(
    fit = fit,
    expected_keys = keys,
    construct_names = unique(rel$evidence$construct),
    type_context = type_context,
    obs.var = rel$obs.var,
    ordinal_scale = rel$ordinal_scale,
    include_alpha = rel$include_alpha
  )

  expect_equal(names(stat), keys)
  expect_true(any(is.finite(stat)))
})


test_that("reliability bootstrap CI helper fails closed for an invalid fitted model", {
  run <- make_m9_report_run()
  rel <- run$results$reliability

  fit_info <- list(
    fit = NULL,
    latent_names = unique(rel$evidence$construct)
  )

  ans <- nomologR:::nomo_reliability_bootstrap_ci(
    fit_info = fit_info,
    evidence = rel$evidence,
    type_context = rel$item_type_context,
    obs.var = rel$obs.var,
    ordinal_scale = rel$ordinal_scale,
    include_alpha = rel$include_alpha,
    level = .95,
    R = 20L,
    seed = 2026L
  )

  expect_false(ans$status$available[[1L]])
  expect_match(ans$status$reason[[1L]], "Bootstrap failed", fixed = TRUE)
  expect_true(all(is.na(ans$intervals$ci_lower)))
  expect_true(all(ans$intervals$n_success == 0L))
})


test_that("reliability print covers alpha-not-requested and no-finite-omega branches", {
  run <- make_m9_report_run()
  rel <- run$results$reliability

  no_alpha <- rel
  no_alpha$include_alpha <- FALSE
  no_alpha$alpha_status$requested[] <- FALSE
  no_alpha$alpha_status$available[] <- FALSE
  no_alpha$evidence$estimate[no_alpha$evidence$metric == "omega"] <- NA_real_

  txt <- paste(capture.output(print(no_alpha)), collapse = "\n")
  expect_false(grepl("Omega range:", txt, fixed = TRUE))
  expect_false(grepl("Alpha:", txt, fixed = TRUE))
})


test_that("reliability plot labels point-estimate subtitle without intervals", {
  run <- make_m9_report_run()
  rel <- run$results$reliability

  rel$evidence$ci_lower <- NA_real_
  rel$evidence$ci_upper <- NA_real_

  p <- plot(rel)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "bootstrap CIs are optional", fixed = TRUE)
})


test_that("reliability summary table fills missing CI-success columns", {
  run <- make_m9_report_run()
  rel <- run$results$reliability

  rel$evidence$ci_lower <- NULL
  rel$evidence$ci_upper <- NULL
  rel$evidence$ci_n_success <- NULL

  out <- nomologR:::nomo_reliability_summary_table(rel)
  expect_true("omega_ci_n_success" %in% names(out))
  expect_true("alpha_ci_n_success" %in% names(out))
  expect_true(all(is.na(out$omega_ci_n_success)))
  expect_true(all(is.na(out$alpha_ci_n_success)))
})


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("closeout: reliability bootstrap helper covers alignment and finite-draw qualifications", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  evidence <- tibble::tibble(
    construct = c("F1", "F2"),
    block = c("overall", "overall"),
    metric = c("omega", "omega"),
    estimate = c(.8, .8)
  )
  type_context <- tibble::tibble(
    construct = c("F1", "F2"),
    indicator_type = c("continuous", "continuous")
  )
  fit_info <- list(fit = structure(list(), class = "lavaan"), latent_names = c("F1", "F2"))

  testthat::local_mocked_bindings(
    bootstrapLavaan = function(...) matrix(numeric(), 0L, 0L),
    .package = "lavaan"
  )
  bad_align <- nomologR:::nomo_reliability_bootstrap_ci(
    fit_info, evidence, type_context,
    obs.var = TRUE, ordinal_scale = TRUE, include_alpha = FALSE,
    level = .95, R = 20L, seed = 1L
  )
  expect_false(bad_align$status$available[[1L]])
  expect_match(bad_align$status$reason[[1L]], "could not be aligned", fixed = TRUE)

  draws <- matrix(.80, nrow = 20L, ncol = 2L)
  testthat::local_mocked_bindings(
    bootstrapLavaan = function(...) draws,
    .package = "lavaan"
  )
  ok <- nomologR:::nomo_reliability_bootstrap_ci(
    fit_info, evidence, type_context,
    obs.var = TRUE, ordinal_scale = TRUE, include_alpha = FALSE,
    level = .95, R = 20L, seed = 1L
  )
  expect_true(ok$status$available[[1L]])
  expect_identical(colnames(draws), NULL)

  few <- matrix(NA_real_, nrow = 20L, ncol = 2L)
  few[1:9, ] <- .80
  colnames(few) <- c("omega::F1::overall", "omega::F2::overall")
  testthat::local_mocked_bindings(
    bootstrapLavaan = function(...) few,
    .package = "lavaan"
  )
  too_few <- nomologR:::nomo_reliability_bootstrap_ci(
    fit_info, evidence, type_context,
    obs.var = TRUE, ordinal_scale = TRUE, include_alpha = FALSE,
    level = .95, R = 20L, seed = 1L
  )
  expect_false(too_few$status$available[[1L]])
  expect_match(too_few$status$reason[[1L]], "too few finite", fixed = TRUE)

  qualified <- matrix(NA_real_, nrow = 20L, ncol = 2L)
  qualified[1:15, ] <- seq(.70, .84, length.out = 15L)
  colnames(qualified) <- c("omega::F1::overall", "omega::F2::overall")
  testthat::local_mocked_bindings(
    bootstrapLavaan = function(...) qualified,
    .package = "lavaan"
  )
  q <- nomologR:::nomo_reliability_bootstrap_ci(
    fit_info, evidence, type_context,
    obs.var = TRUE, ordinal_scale = TRUE, include_alpha = FALSE,
    level = .95, R = 20L, seed = 1L
  )
  expect_true(q$status$available[[1L]])
  expect_match(q$status$reason[[1L]], "only 15 of 20", fixed = TRUE)
})


test_that("closeout: reliability bootstrap statistic returns all-NA when engines fail", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  testthat::local_mocked_bindings(
    compRelSEM = function(...) stop("synthetic engine failure"),
    .package = "semTools"
  )

  out <- nomologR:::nomo_reliability_boot_stat(
    fit = structure(list(), class = "lavaan"),
    expected_keys = "omega::F::overall",
    construct_names = "F",
    type_context = tibble::tibble(
      construct = "F",
      indicator_type = "continuous"
    ),
    obs.var = TRUE,
    ordinal_scale = TRUE,
    include_alpha = FALSE
  )
  expect_true(is.na(out[[1L]]))
})


test_that("closeout: reliability engine errors and inadmissible coefficient interpretation are explicit", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  cfa <- make_m9_report_run()$results$cfa

  testthat::local_mocked_bindings(
    compRelSEM = function(...) stop("synthetic omega failure"),
    .package = "semTools"
  )
  expect_error(
    nomo_reliability(cfa),
    "Omega/composite-reliability estimation failed"
  )

  testthat::local_mocked_bindings(
    compRelSEM = function(...) c(WellBeing = 1.20),
    .package = "semTools"
  )
  bad <- nomo_reliability(cfa, include_alpha = FALSE)
  expect_identical(bad$evidence$attention[[1L]], "concern")
  expect_match(
    bad$evidence$interpretation[[1L]],
    "outside the conventional 0-1",
    fixed = TRUE
  )

  testthat::local_mocked_bindings(
    nomo_reliability_tidy = function(...) tibble::tibble(),
    .package = "nomologR"
  )
  expect_error(
    nomo_reliability(cfa, include_alpha = FALSE),
    "could not be converted"
  )
})


test_that("closeout: mixed ordered/continuous constructs compute alpha only for continuous composites", {
  set.seed(5608)
  n <- 500
  f1 <- rnorm(n)
  f2 <- .25 * f1 + sqrt(1 - .25^2) * rnorm(n)

  a1 <- .8 * f1 + rnorm(n, sd = .6)
  a2 <- .75 * f1 + rnorm(n, sd = .65)
  a3 <- .7 * f1 + rnorm(n, sd = .7)

  dat <- data.frame(
    A1 = ordered(cut(a1, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    A2 = ordered(cut(a2, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    A3 = ordered(cut(a3, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    B1 = .8 * f2 + rnorm(n, sd = .6),
    B2 = .75 * f2 + rnorm(n, sd = .65),
    B3 = .7 * f2 + rnorm(n, sd = .7)
  )

  fit <- lavaan::cfa(
    "
      F1 =~ A1 + A2 + A3
      F2 =~ B1 + B2 + B3
    ",
    data = dat,
    ordered = c("A1", "A2", "A3"),
    estimator = "WLSMV"
  )
  expect_true(lavaan::lavInspect(fit, "converged"))

  rel <- nomo_reliability(
    fit,
    ordinal_scale = TRUE,
    include_alpha = TRUE
  )
  expect_true(any(rel$alpha_status$construct == "F2"))
  expect_false(rel$alpha_status$available[rel$alpha_status$construct == "F1"])
})


test_that("closeout B: reliability alpha engine errors remain specific to the requested estimand", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  cfa <- make_m9_report_run()$results$cfa
  original_comp <- semTools::compRelSEM
  calls <- 0L

  testthat::local_mocked_bindings(
    compRelSEM = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(original_comp(...))
      }
      stop("synthetic alpha failure")
    },
    .package = "semTools"
  )

  expect_error(
    nomo_reliability(cfa, include_alpha = TRUE),
    "Coefficient-alpha estimation failed"
  )
})


test_that("closeout B: continuous-alpha failure is explicit in a model containing separate ordered and continuous composites", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  set.seed(6203)
  n <- 320
  f1 <- rnorm(n)
  f2 <- .25 * f1 + sqrt(1 - .25^2) * rnorm(n)

  a1 <- .80 * f1 + rnorm(n, sd = .60)
  a2 <- .75 * f1 + rnorm(n, sd = .65)
  a3 <- .70 * f1 + rnorm(n, sd = .70)

  dat <- data.frame(
    A1 = ordered(cut(a1, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    A2 = ordered(cut(a2, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    A3 = ordered(cut(a3, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    B1 = .80 * f2 + rnorm(n, sd = .60),
    B2 = .75 * f2 + rnorm(n, sd = .65),
    B3 = .70 * f2 + rnorm(n, sd = .70)
  )

  fit <- lavaan::cfa(
    "
      F1 =~ A1 + A2 + A3
      F2 =~ B1 + B2 + B3
    ",
    data = dat,
    ordered = c("A1", "A2", "A3"),
    estimator = "WLSMV"
  )
  expect_true(lavaan::lavInspect(fit, "converged"))

  original_comp <- semTools::compRelSEM
  testthat::local_mocked_bindings(
    compRelSEM = function(..., tau.eq = FALSE) {
      if (is.character(tau.eq)) {
        stop("synthetic continuous-alpha failure")
      }
      original_comp(..., tau.eq = tau.eq)
    },
    .package = "semTools"
  )

  expect_error(
    nomo_reliability(
      fit,
      ordinal_scale = TRUE,
      include_alpha = TRUE
    ),
    "Coefficient-alpha estimation failed for continuous composites"
  )
})


test_that("closeout B: reliability marks an improper measurement model as a model-dependence concern", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  cfa <- make_m9_report_run()$results$cfa
  original_measurement_fit <- nomologR:::nomo_measurement_fit

  testthat::local_mocked_bindings(
    nomo_measurement_fit = function(...) {
      z <- original_measurement_fit(...)
      z$post_check <- FALSE
      z
    },
    .package = "nomologR"
  )

  rel <- nomo_reliability(cfa, include_alpha = FALSE)
  dep <- rel$decision_log[
    rel$decision_log$metric == "model_dependence",
    ,
    drop = FALSE
  ]
  expect_gt(nrow(dep), 0L)
  expect_identical(dep$severity[[1L]], "concern")
  expect_match(dep$observation[[1L]], "admissibility", fixed = TRUE)
})


# Parallel bootstrap (#42) -----------------------------------------------------

reliability_boot_fit <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    cache <<- nomo_cfa(
      "A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4",
      nomo_demo_continuous
    )
    cache
  }
})


test_that("the bootstrap stays serial by default and records it", {
  skip_on_cran()
  rel <- nomo_reliability(reliability_boot_fit(), ci = "bootstrap",
                          ci_boot = 20, ci_seed = 2026)

  expect_identical(rel$ci_ncpus, 1L)
  expect_identical(rel$ci_status$workers, 1L)

  entry <- rel$decision_log[rel$decision_log$metric == "bootstrap_reproducibility", ]
  expect_identical(nrow(entry), 1L)
  expect_identical(entry$severity, "info")
  expect_match(entry$observation, "seed 2026, serially", fixed = TRUE)
  expect_match(entry$recommendation, "same number of workers", fixed = TRUE)
})


test_that("the printed status names the worker count when there was more than one", {
  skip_on_cran()
  rel <- nomo_reliability(reliability_boot_fit(), ci = "bootstrap",
                          ci_boot = 20, ci_seed = 2026)
  expect_false(any(grepl("workers", utils::capture.output(print(rel)), fixed = TRUE)))

  # The print reads the recorded count, so a two-worker status prints it
  # without running a parallel bootstrap here.
  rel$ci_status$workers <- 2L
  expect_output(print(rel), "| 2 workers", fixed = TRUE)
})


test_that("an unseeded bootstrap is flagged as not reproducible", {
  skip_on_cran()
  rel <- nomo_reliability(reliability_boot_fit(), ci = "bootstrap", ci_boot = 20)
  entry <- rel$decision_log[rel$decision_log$metric == "bootstrap_reproducibility", ]

  expect_identical(entry$severity, "review")
  expect_match(entry$recommendation, "cannot be reproduced exactly", fixed = TRUE)
})


test_that("point estimates record no workers and log no bootstrap", {
  rel <- nomo_reliability(reliability_boot_fit())
  expect_true(is.na(rel$ci_status$workers))
  expect_false("bootstrap_reproducibility" %in% rel$decision_log$metric)
})


test_that("ci_ncpus must be a whole number of at least one", {
  fit <- reliability_boot_fit()
  for (bad in list(0, 1.5, -2, NA, "2", c(1, 2))) {
    expect_error(nomo_reliability(fit, ci_ncpus = bad), "ci_ncpus")
  }
})


test_that("a parallel bootstrap is reproducible for a seed and a worker count", {
  skip_on_cran()

  # Workers are separate R processes that must load an installed nomologR, which
  # is not the case under devtools::load_all(). Probe rather than assume.
  cl <- tryCatch(parallel::makePSOCKcluster(1L), error = function(e) NULL)
  if (is.null(cl)) skip("could not start a worker process")
  can_load <- tryCatch(
    isTRUE(parallel::clusterEvalQ(
      cl, requireNamespace("nomologR", quietly = TRUE)
    )[[1L]]),
    error = function(e) FALSE
  )
  parallel::stopCluster(cl)
  if (!can_load) skip("worker processes cannot load an installed nomologR")

  # CRAN policy: never more than two cores at once.
  run <- function() {
    nomo_reliability(reliability_boot_fit(), ci = "bootstrap",
                     ci_boot = 30, ci_seed = 2026, ci_ncpus = 2)
  }
  first <- run()
  second <- run()

  expect_true(first$ci_status$available)
  expect_identical(first$ci_status$workers, 2L)
  expect_identical(first$omega$ci_lower, second$omega$ci_lower)
  expect_identical(first$omega$ci_upper, second$omega$ci_upper)

  entry <- first$decision_log[first$decision_log$metric == "bootstrap_reproducibility", ]
  expect_match(entry$observation, "on 2 snow workers", fixed = TRUE)
})
