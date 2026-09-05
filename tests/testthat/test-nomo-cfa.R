test_that("nomo_model builds simple reflective CFA syntax", {
  model <- nomo_model(list(
    F1 = c("x1", "x2", "x3"),
    F2 = c("x4", "x5", "x6")
  ))

  expect_s3_class(model, "nomo_model")
  expect_match(as.character(model), "F1 =~ x1 \\+ x2 \\+ x3")
  expect_match(as.character(model), "F2 =~ x4 \\+ x5 \\+ x6")
  expect_error(nomo_model(list()), "non-empty named list")
  expect_error(nomo_model(list(F1 = c("x1", "x1"))), "duplicated within a factor")
})


test_that("continuous nomo_cfa reproduces direct lavaan estimates", {
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '
  dat <- lavaan::HolzingerSwineford1939

  out <- nomo_cfa(model, data = dat, modification_indices = FALSE)
  direct <- lavaan::cfa(model, data = dat)

  expect_s3_class(out, "nomo_cfa")
  expect_s4_class(out$fit, "lavaan")
  expect_true(out$converged)
  expect_equal(out$estimator, "ML")
  expect_equal(out$estimator_engine, "ML")
  expect_equal(out$n_used, 301)

  direct_fit <- lavaan::fitMeasures(direct, c("cfi", "tli", "rmsea", "srmr"))
  ours <- out$fit_measures_all[c("cfi", "tli", "rmsea", "srmr")]
  expect_equal(as.numeric(ours), as.numeric(direct_fit), tolerance = 1e-8)

  direct_std <- lavaan::standardizedSolution(direct, type = "std.all")
  direct_loadings <- direct_std[direct_std$op == "=~", "est.std"]
  expect_equal(out$standardized_loadings$loading, direct_loadings, tolerance = 1e-8)
  expect_true(nrow(out$residual_pairs) > 0)
  expect_true(any(out$decision_log$metric == "automatic_respecification"))
})


test_that("robust ML remains an explicit researcher estimator choice", {
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '
  dat <- lavaan::HolzingerSwineford1939

  out <- nomo_cfa(model, data = dat, estimator = "MLR", modification_indices = FALSE)
  direct <- lavaan::cfa(model, data = dat, estimator = "MLR")

  expect_true(out$converged)
  expect_equal(out$estimator, "MLR")
  expect_equal(out$estimator_source, "researcher")

  expected <- lavaan::fitMeasures(direct, "cfi.robust")
  actual <- out$fit_evidence$value[out$fit_evidence$metric == "CFI"]
  if (is.finite(expected)) expect_equal(actual, unname(expected), tolerance = 1e-8)
})


test_that("declared ordinal indicators use WLSMV by default", {
  set.seed(4101)
  n <- 500
  f1 <- rnorm(n)
  f2 <- .35 * f1 + sqrt(1 - .35^2) * rnorm(n)
  latent <- data.frame(
    q1 = .85 * f1 + rnorm(n, sd = .55),
    q2 = .80 * f1 + rnorm(n, sd = .60),
    q3 = .75 * f1 + rnorm(n, sd = .65),
    q4 = .85 * f2 + rnorm(n, sd = .55),
    q5 = .80 * f2 + rnorm(n, sd = .60),
    q6 = .75 * f2 + rnorm(n, sd = .65)
  )
  dat <- as.data.frame(lapply(
    latent,
    function(z) as.integer(cut(z, breaks = c(-Inf, -.75, 0, .75, Inf), labels = FALSE))
  ))
  model <- '
    F1 =~ q1 + q2 + q3
    F2 =~ q4 + q5 + q6
  '

  out <- nomo_cfa(
    model, data = dat, ordered = names(dat), modification_indices = FALSE
  )

  expect_true(out$converged)
  expect_equal(out$estimator, "WLSMV")
  expect_equal(out$estimator_source, "ordered_default")
  expect_true(out$estimator_engine %in% c("DWLS", "WLSMV"))
  expect_equal(sort(out$ordered), sort(names(dat)))
  expect_equal(nrow(out$standardized_loadings), 6)
})


test_that("incompatible ordered-data estimator and FIML choices stop early", {
  dat <- data.frame(
    q1 = rep(1:4, 50),
    q2 = rep(4:1, 50),
    q3 = rep(c(1, 2, 3, 4), 50)
  )
  model <- "F1 =~ q1 + q2 + q3"

  expect_error(
    nomo_cfa(model, data = dat, ordered = names(dat), estimator = "MLR"),
    "ML-family estimators"
  )
  expect_error(
    nomo_cfa(model, data = dat, ordered = names(dat), missing = "fiml"),
    "FIML is not supported"
  )
})


test_that("fit references are review prompts, not pass/fail verdicts", {
  measures <- c(
    chisq = 100, df = 50, pvalue = .001, cfi = .90, tli = .89,
    rmsea = .09, rmsea.ci.lower = .08, rmsea.ci.upper = .10, srmr = .10
  )
  tab <- nomo_cfa_fit_evidence(measures, nomo_defaults())
  expect_equal(
    tab$attention[tab$metric %in% c("CFI", "TLI", "RMSEA", "SRMR")],
    rep("review", 4)
  )
  expect_false(any(grepl("pass|fail", tab$attention, ignore.case = TRUE)))
})


test_that("loading helper identifies weak and extreme standardized loadings", {
  std <- data.frame(
    lhs = c("F1", "F1", "F1"), op = rep("=~", 3), rhs = c("x1", "x2", "x3"),
    est.std = c(.80, .35, 1.05), se = c(.05, .06, .07), z = c(16, 5.8, 15),
    pvalue = c(0, 0, 0), ci.lower = c(.70, .23, .91), ci.upper = c(.90, .47, 1.19)
  )
  tab <- nomo_cfa_loadings(std, nomo_defaults())
  expect_equal(tab$attention, c("KEEP", "REVIEW", "STRONG REVIEW"))
})


test_that("Heywood helper detects negative variances and >1 loadings", {
  pe <- data.frame(
    lhs = c("x1", "F1"), op = c("~~", "~~"), rhs = c("x1", "F1"), est = c(-.10, -.20)
  )
  std <- data.frame(lhs = "F1", op = "=~", rhs = "x1", est.std = 1.10)
  out <- nomo_cfa_heywood(
    parameter_estimates = pe,
    standardized_solution = std,
    latent_names = "F1",
    observed_names = "x1"
  )
  expect_true(all(c(
    "negative_observed_residual_variance",
    "negative_latent_variance",
    "standardized_loading_beyond_one"
  ) %in% out$issue))
  expect_true(all(out$severity == "concern"))
})


test_that("residual helper returns unique ranked pairs", {
  mat <- matrix(
    c(0, .10, -.30, .10, 0, .20, -.30, .20, 0),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("x1", "x2", "x3"), c("x1", "x2", "x3"))
  )
  out <- nomo_cfa_residual_pairs(mat)
  expect_equal(nrow(out), 3)
  expect_equal(out$abs_residual[[1]], .30)
  expect_equal(out$item1[[1]], "x3")
  expect_equal(out$item2[[1]], "x1")
})


test_that("modification indices are retained but never acted on automatically", {
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '
  out <- nomo_cfa(
    model, data = lavaan::HolzingerSwineford1939,
    modification_indices = TRUE, mi_top = 5
  )
  expect_true(nrow(out$modification_indices) > 0)
  expect_lte(nrow(out$top_modification_indices), 5)
  expect_true(any(out$decision_log$metric == "modification_indices"))
  auto <- out$decision_log[
    out$decision_log$metric == "automatic_respecification", , drop = FALSE
  ]
  expect_equal(auto$value, 0)
})


test_that("CFA presentation methods return stable classes", {
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '
  out <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
  expect_s3_class(summary(out), "summary_nomo_cfa")
  expect_s3_class(plot(out, type = "loadings"), "ggplot")
  expect_s3_class(plot(out, type = "fit"), "ggplot")
  expect_s3_class(plot(out, type = "residuals"), "ggplot")
  expect_s3_class(plot(out, type = "modification_indices"), "ggplot")
})


test_that("nomo_cfa validates key inputs", {
  dat <- data.frame(x1 = rnorm(50), x2 = rnorm(50), x3 = rnorm(50))
  model <- "F1 =~ x1 + x2 + x3"
  expect_error(nomo_cfa("", dat), "non-empty")
  expect_error(nomo_cfa(model, numeric()), "data frame")
  expect_error(nomo_cfa(model, dat, ordered = "missing_item"), "not found")
  expect_error(nomo_cfa(model, dat, std.lv = NA), "TRUE or FALSE")
  expect_error(nomo_cfa(model, dat, mi_top = -1), "non-negative integer")
  expect_error(nomo_cfa(model, dat, guidance = list()), "missing required")
})


test_that("lavaan estimation errors are surfaced as CFA estimation failures", {
  dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
  expect_error(
    nomo_cfa("this is not valid lavaan syntax", dat),
    "CFA estimation failed"
  )
})


test_that("nomo_model normalizes names before duplicate checks", {
  model <- nomo_model(stats::setNames(
    list(c(" x1 ", "x2", "x3")),
    " F1 "
  ))

  expect_identical(as.character(model), "F1 =~ x1 + x2 + x3")
  expect_identical(names(attr(model, "factors")), "F1")
  expect_identical(attr(model, "factors")[[1]], c("x1", "x2", "x3"))
  expect_output(print(model), "F1 =~ x1 \\+ x2 \\+ x3")

  expect_error(
    nomo_model(stats::setNames(
      list(c("x1", "x2"), c("x3", "x4")),
      c("F1", " F1 ")
    )),
    "unique, non-empty factor names"
  )
  expect_error(
    nomo_model(list(F1 = c("x1", " x1 "))),
    "duplicated within a factor"
  )
  expect_error(
    nomo_model(list(F1 = c("x1", "  "))),
    "non-missing indicator names"
  )
  expect_error(
    nomo_model(list(F1 = c("x1", NA_character_))),
    "non-missing indicator names"
  )
  expect_error(
    nomo_model(stats::setNames(list(c("x1", "x2")), "  ")),
    "unique, non-empty factor names"
  )
})


test_that("fit evidence prioritizes robust and scaled variants transparently", {
  measures <- c(
    chisq = 99,
    chisq.scaled = 88,
    df = 40,
    df.scaled = 39,
    pvalue = .01,
    pvalue.scaled = .02,
    cfi = .91,
    cfi.scaled = .92,
    cfi.robust = .93,
    tli = .90,
    tli.scaled = .91,
    tli.robust = .92,
    rmsea = .08,
    rmsea.scaled = .075,
    rmsea.robust = .07,
    rmsea.ci.lower = .06,
    rmsea.ci.lower.scaled = .055,
    rmsea.ci.lower.robust = .05,
    rmsea.ci.upper = .10,
    rmsea.ci.upper.scaled = .095,
    rmsea.ci.upper.robust = .09,
    srmr = .07
  )

  tab <- nomo_cfa_fit_evidence(measures, nomo_defaults())

  expect_equal(tab$value[tab$metric == "chi_square"], 88)
  expect_equal(tab$variant[tab$metric == "chi_square"], "chisq.scaled")
  expect_equal(tab$value[tab$metric == "CFI"], .93)
  expect_equal(tab$variant[tab$metric == "CFI"], "cfi.robust")
  expect_equal(tab$value[tab$metric == "RMSEA"], .07)
  expect_equal(tab$variant[tab$metric == "RMSEA"], "rmsea.robust")

  absent <- nomo_cfa_first_measure(numeric(), c("cfi.robust", "cfi"))
  expect_true(is.na(absent$value))
  expect_true(is.na(absent$variant))

  fallback <- nomo_cfa_first_measure(
    c(cfi.robust = NA_real_, cfi = .94),
    c("cfi.robust", "cfi")
  )
  expect_equal(fallback$value, .94)
  expect_equal(fallback$variant, "cfi")
})


test_that("factor-correlation helper isolates latent covariance rows", {
  std <- data.frame(
    lhs = c("F1", "F1", "F1", "x1"),
    op = c("~~", "=~", "~~", "~~"),
    rhs = c("F2", "x1", "F1", "x2"),
    est.std = c(.45, .80, 1, .10),
    se = c(.06, .04, .00, .03),
    z = c(7.5, 20, NA, 3.3),
    pvalue = c(0, 0, NA, .001),
    ci.lower = c(.33, .72, 1, .04),
    ci.upper = c(.57, .88, 1, .16)
  )

  out <- nomo_cfa_factor_correlations(std, c("F1", "F2"))
  expect_equal(nrow(out), 1)
  expect_equal(out$factor1, "F1")
  expect_equal(out$factor2, "F2")
  expect_equal(out$correlation, .45)

  empty <- nomo_cfa_factor_correlations(tibble::tibble(), c("F1", "F2"))
  expect_equal(nrow(empty), 0)
})


test_that("Heywood helper detects inadmissible latent correlations", {
  pe <- data.frame(
    lhs = character(), op = character(), rhs = character(), est = numeric()
  )
  std <- data.frame(
    lhs = c("F1", "F1"),
    op = c("=~", "~~"),
    rhs = c("x1", "F2"),
    est.std = c(.8, 1.04)
  )

  out <- nomo_cfa_heywood(
    parameter_estimates = pe,
    standardized_solution = std,
    latent_names = c("F1", "F2"),
    observed_names = "x1"
  )

  expect_true("latent_correlation_beyond_one" %in% out$issue)
  expect_equal(
    out$severity[out$issue == "latent_correlation_beyond_one"],
    "concern"
  )
})


test_that("residual helper supports direct and nested lavaan-style objects", {
  mat <- matrix(
    c(0, .1, .1, 0),
    nrow = 2,
    dimnames = list(c("x1", "x2"), c("x1", "x2"))
  )

  expect_equal(nomo_cfa_residual_matrix(list(cov = mat)), mat)
  expect_equal(nomo_cfa_residual_matrix(list(list(cov = mat))), mat)
  expect_equal(nrow(nomo_cfa_residual_matrix(NULL)), 0)
  expect_equal(nrow(nomo_cfa_residual_matrix(list(foo = mat))), 0)

  unnamed <- unname(mat)
  pairs <- nomo_cfa_residual_pairs(unnamed)
  expect_equal(pairs$item1, "V2")
  expect_equal(pairs$item2, "V1")
  expect_equal(nrow(nomo_cfa_residual_pairs(matrix(1, 1, 1))), 0)
})


test_that("continuous missing-data handling makes case retention visible", {
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '

  dat <- lavaan::HolzingerSwineford1939
  dat$x1[1:15] <- NA_real_

  listwise <- nomo_cfa(
    model,
    data = dat,
    modification_indices = FALSE
  )
  fiml <- nomo_cfa(
    model,
    data = dat,
    missing = "fiml",
    modification_indices = FALSE
  )
  direct_fiml <- lavaan::cfa(model, data = dat, missing = "fiml")

  expect_equal(listwise$data_n, 301)
  expect_equal(listwise$n_used, 286)
  expect_equal(listwise$n_dropped, 15)
  expect_equal(listwise$pct_dropped, 15 / 301)
  expect_equal(fiml$n_used, 301)
  expect_equal(fiml$n_dropped, 0)

  listwise_cases <- listwise$decision_log[
    listwise$decision_log$metric == "cases_used", , drop = FALSE
  ]
  fiml_cases <- fiml$decision_log[
    fiml$decision_log$metric == "cases_used", , drop = FALSE
  ]
  expect_equal(listwise_cases$severity, "review")
  expect_equal(fiml_cases$severity, "info")
  expect_true(any(fiml$decision_log$metric == "missing_data_option"))

  expect_equal(
    as.numeric(fiml$fit_measures_all[c("cfi", "rmsea", "srmr")]),
    as.numeric(lavaan::fitMeasures(direct_fiml, c("cfi", "rmsea", "srmr"))),
    tolerance = 1e-8
  )
})


test_that("a deliberately misspecified CFA surfaces strain without changing model", {
  model <- '
    general =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
  '

  out <- nomo_cfa(
    model,
    data = lavaan::HolzingerSwineford1939,
    modification_indices = TRUE,
    mi_top = 8
  )

  fit_reviews <- out$fit_evidence[
    out$fit_evidence$metric %in% c("CFI", "TLI", "RMSEA", "SRMR") &
      out$fit_evidence$attention == "review",
    , drop = FALSE
  ]

  expect_true(out$converged)
  expect_gt(nrow(fit_reviews), 0)
  expect_gt(nrow(out$residual_pairs), 0)
  expect_gt(nrow(out$modification_indices), 0)
  expect_lte(nrow(out$top_modification_indices), 8)

  auto <- out$decision_log[
    out$decision_log$metric == "automatic_respecification",
    , drop = FALSE
  ]
  expect_equal(auto$value, 0)
  expect_identical(out$model, model)
})


test_that("decision log retains warnings, nonconvergence, flags, and MI quarantine", {
  fit_evidence <- tibble::tibble(
    metric = c("df", "CFI"),
    value = c(0, .80),
    variant = c("df", "cfi"),
    reference = c(NA_real_, .95),
    direction = c("information", "higher"),
    attention = c("info", "review"),
    explanation = c("df info", "poor fit")
  )
  loadings <- tibble::tibble(
    item = "x1",
    loading = .30,
    attention = "REVIEW",
    explanation = "weak loading"
  )
  heywood <- tibble::tibble(
    object = "x2",
    issue = "negative_observed_residual_variance",
    value = -.1,
    severity = "concern",
    explanation = "negative residual variance"
  )
  residual_pairs <- tibble::tibble(
    item1 = "x1",
    item2 = "x2",
    residual = .20,
    abs_residual = .20
  )
  mis <- tibble::tibble(lhs = "x1", op = "~~", rhs = "x2", mi = 10)

  log <- nomo_cfa_decision_log(
    estimator_label = "ML",
    engine_estimator = "ML",
    estimator_source = "researcher",
    ordered = character(),
    missing = NULL,
    data_n = 100,
    n_used = 95,
    converged = FALSE,
    warnings = "synthetic engine warning",
    fit_evidence = fit_evidence,
    loadings = loadings,
    heywood = heywood,
    residual_pairs = residual_pairs,
    modification_indices = mis,
    modification_indices_requested = TRUE,
    mi_error = NULL
  )

  expect_true(all(c(
    "cases_used",
    "convergence",
    "engine_warning",
    "fit_cfi",
    "degrees_of_freedom",
    "standardized_loading",
    "negative_observed_residual_variance",
    "largest_residual_correlation",
    "modification_indices",
    "automatic_respecification"
  ) %in% log$metric))
  expect_equal(log$severity[log$metric == "convergence"], "concern")
  expect_equal(log$severity[log$metric == "cases_used"], "review")
})


test_that("additional CFA validation branches are explicit", {
  dat <- data.frame(x1 = rnorm(60), x2 = rnorm(60), x3 = rnorm(60))
  model <- "F1 =~ x1 + x2 + x3"

  expect_error(nomo_cfa(model, dat, ordered = 1), "character vector")
  expect_error(nomo_cfa(model, dat, estimator = character()), "one non-empty")
  expect_error(nomo_cfa(model, dat, missing = character()), "one non-empty")
  expect_error(
    nomo_cfa(model, dat, modification_indices = NA),
    "TRUE or FALSE"
  )
  expect_error(
    nomo_cfa(
      model,
      dat,
      guidance = list(
        cfa_loading_reference = .5,
        fit_reference = list(cfi = .95)
      )
    ),
    "must contain cfi, tli, rmsea, and srmr"
  )
})
