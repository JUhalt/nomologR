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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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


test_that("a model naming a variable the data lack is refused with its name (#89)", {
  set.seed(8905)
  dat <- data.frame(x1 = rnorm(60), x2 = rnorm(60), x3 = rnorm(60),
                    g = rep(c("a", "b"), 30))

  expect_error(
    nomo_cfa("F =~ x1 + x2 + xx3", dat),
    "`model` names variable(s) not found in `data`: xx3. Check the spelling against names(data).",
    fixed = TRUE
  )
  expect_error(nomo_cfa("F =~ x1 + y2 + y3", dat), "not found in `data`: y2, y3.", fixed = TRUE)
  expect_error(
    nomo_invariance("F =~ x1 + x2 + xx3", data = dat, group = "g"),
    "not found in `data`: xx3.", fixed = TRUE
  )

  h <- nomo_hypotheses("F -> x1" = positive())
  expect_error(
    nomo_network("F =~ x1 + x2 + xx3", data = dat, hypotheses = h),
    "not found in `data`: xx3.", fixed = TRUE
  )
  expect_error(
    nomo_network("F =~ x1 + x2 + x3", data = dat, validation_data = dat[, c("x1", "x2")],
                 hypotheses = h),
    "not found in `validation_data`: x3.", fixed = TRUE
  )
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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

# ---- recovered from hygiene consolidation: test-nomo-cfa.R ----
# ---- consolidated from test-nomo-cfa-presentation.R ----
nomo_test_cfa <- function(modification_indices = TRUE) {
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '
  nomo_cfa(
    model,
    data = lavaan::HolzingerSwineford1939,
    modification_indices = modification_indices,
    mi_top = 5
  )
}


test_that("CFA print method exposes the central guardrails", {
  skip_on_cran()
  out <- nomo_test_cfa()
  txt <- capture.output(print(out))

  expect_true(any(grepl("<nomo_cfa>", txt, fixed = TRUE)))
  expect_true(any(grepl("Converged: yes", txt, fixed = TRUE)))
  expect_true(any(grepl("Fit: CFI", txt, fixed = TRUE)))
  expect_true(any(grepl("Flags:", txt, fixed = TRUE)))
  expect_true(any(grepl("No parameter was freed and no model was refit automatically",
                        txt, fixed = TRUE)))
})


test_that("CFA print method handles engine labels, warnings, and unavailable values", {
  skip_on_cran()
  out <- nomo_test_cfa(modification_indices = FALSE)
  out$n_used <- NA_real_
  out$estimator <- "WLSMV"
  out$estimator_engine <- "DWLS"
  out$converged <- FALSE
  out$engine_warnings <- "synthetic warning"
  out$fit_evidence$value[out$fit_evidence$metric %in% c("CFI", "TLI", "RMSEA", "SRMR")] <- NA_real_

  txt <- capture.output(print(out))
  expect_true(any(grepl("Cases: -- of 301 used", txt, fixed = TRUE)))
  expect_true(any(grepl("engine: DWLS", txt, fixed = TRUE)))
  # No status is shown in capitals (#144).
  expect_true(any(grepl("Converged: no", txt, fixed = TRUE)))
  expect_true(any(grepl("Engine warnings: 1", txt, fixed = TRUE)))
  expect_false(any(grepl("Fit:", txt, fixed = TRUE)))
  # The flags count what did not converge and lavaan's warning, not loadings
  # computed from an unconverged fit.
  expect_true(any(grepl("Flags: 1 review, 1 concern", txt, fixed = TRUE)))

  out$estimator <- NA_character_
  out$estimator_engine <- NA_character_
  txt <- capture.output(print(out))
  expect_true(any(grepl("Estimator: -- |", txt, fixed = TRUE)))
  # With no abbreviation shown, no definitions are printed.
  expect_false(any(grepl(" = ", txt, fixed = TRUE)))
})


test_that("summary printer covers flagged and diagnostic sections", {
  skip_on_cran()
  out <- nomo_test_cfa()
  s <- summary(out)

  s$n_dropped <- 10
  s$pct_dropped <- 10 / s$data_n
  s$standardized_loadings$attention[[1]] <- "REVIEW"
  s$standardized_loadings$explanation[[1]] <- "synthetic loading review"
  s$heywood <- tibble::tibble(
    object = "x1",
    issue = "synthetic_heywood",
    value = -0.1,
    severity = "concern",
    explanation = "synthetic improper solution"
  )
  s$engine_warnings <- "synthetic engine warning"

  s$ordered <- "x1"
  s$fit_evidence$variant[s$fit_evidence$metric == "CFI"] <- "cfi.robust"

  txt <- capture.output(print(s))
  expect_true(any(grepl("(10 not used, 3.3%)", txt, fixed = TRUE)))
  expect_true(any(grepl("Ordered indicators: 1", txt, fixed = TRUE)))
  expect_true(any(grepl("CFI is a robust value.", txt, fixed = TRUE)))
  # One "Flagged" section, concern before review (#144).
  expect_true("Flagged" %in% txt)
  flagged <- txt[(which(txt == "Flagged") + 1L):length(txt)]
  expect_match(flagged[[1L]], "^  - x1 \\(Concern\\): synthetic improper solution\\.")
  expect_true(any(grepl("x1 on visual (Review): synthetic loading review.", txt, fixed = TRUE)))
  expect_true(any(grepl("lavaan (Review): synthetic engine warning.", txt, fixed = TRUE)))
  expect_true(any(grepl("Factor correlations", txt, fixed = TRUE)))
  # An improper-solution signal of an unknown kind is named in words.
  expect_true(any(grepl("^  x1 +Synthetic heywood +-0.10$", txt)))
  expect_true(any(grepl("Largest residual correlations", txt, fixed = TRUE)))
  expect_true(any(grepl("Modification indices (diagnostic only)", txt, fixed = TRUE)))
  expect_true(any(grepl("no single cutoff establishes model validity", txt, fixed = TRUE)))
})


test_that("summary printer handles clean optional sections", {
  skip_on_cran()
  out <- nomo_test_cfa(modification_indices = FALSE)
  s <- summary(out)
  s$n_dropped <- 0
  s$pct_dropped <- 0
  s$standardized_loadings$attention <- "KEEP"
  s$factor_correlations <- s$factor_correlations[0, , drop = FALSE]
  s$heywood <- s$heywood[0, , drop = FALSE]
  s$largest_residuals <- s$largest_residuals[0, , drop = FALSE]
  s$top_modification_indices <- tibble::tibble()
  s$engine_warnings <- character()

  txt <- capture.output(print(s))
  expect_true(any(grepl("No loading was flagged for review.", txt, fixed = TRUE)))
  expect_true(any(grepl("No improper-solution signal", txt, fixed = TRUE)))
  expect_false(any(grepl("not used,", txt, fixed = TRUE)))
  expect_false(any(grepl("Modification indices", txt, fixed = TRUE)))

  # Without a chi-square, fit indices, or loadings, each section says so.
  s$fit_evidence <- s$fit_evidence[0, , drop = FALSE]
  s$standardized_loadings <- s$standardized_loadings[0, , drop = FALSE]
  txt <- capture.output(print(s))
  expect_false(any(grepl("chi-square", txt, fixed = TRUE)))
  expect_false(any(grepl("Index", txt, fixed = TRUE)))
  expect_true(any(grepl("No standardized loadings are available.", txt, fixed = TRUE)))
})


test_that("all CFA plot views contain interpretable data", {
  skip_on_cran()
  out <- nomo_test_cfa()

  p_load <- plot(out, type = "loadings")
  p_fit <- plot(out, type = "fit")
  p_res <- plot(out, type = "residuals")
  p_mi <- plot(out, type = "modification_indices")

  expect_s3_class(p_load, "ggplot")
  expect_s3_class(p_fit, "ggplot")
  expect_s3_class(p_res, "ggplot")
  expect_s3_class(p_mi, "ggplot")

  expect_equal(nrow(p_load$data), nrow(out$standardized_loadings))
  expect_true(all(as.character(p_fit$data$metric) %in% c("CFI", "TLI", "RMSEA", "SRMR")))
  # Indices near 1 and near 0 get their own panels, and a legend with one
  # entry is not drawn (#89).
  fit_layout <- ggplot2::ggplot_build(p_fit)$layout$layout
  expect_identical(nrow(fit_layout), 2L)
  one_flag <- out
  one_flag$fit_evidence$attention <- "info"
  expect_null(ggplot2::get_guide_data(plot(one_flag, type = "fit"), "shape"))
  two_flags <- one_flag
  two_flags$fit_evidence$attention[two_flags$fit_evidence$metric == "SRMR"] <- "review"
  expect_false(is.null(ggplot2::get_guide_data(plot(two_flags, type = "fit"), "shape")))
  expect_equal(nrow(p_res$data), choose(nrow(out$residual_matrix), 2))
  expect_lte(nrow(p_mi$data), 5)
})


test_that("CFA plot methods fail informatively when evidence is unavailable", {
  skip_on_cran()
  out <- nomo_test_cfa(modification_indices = FALSE)

  no_load <- out
  no_load$standardized_loadings <- no_load$standardized_loadings[0, , drop = FALSE]
  expect_error(plot(no_load, type = "loadings"), "No standardized loadings")

  no_fit <- out
  no_fit$fit_evidence$value[no_fit$fit_evidence$metric %in% c("CFI", "TLI", "RMSEA", "SRMR")] <- NA_real_
  expect_error(plot(no_fit, type = "fit"), "No global fit evidence")

  no_res <- out
  no_res$residual_matrix <- matrix(numeric(), 0, 0)
  expect_error(plot(no_res, type = "residuals"), "No residual-correlation matrix")

  expect_error(
    plot(out, type = "modification_indices"),
    "No modification indices"
  )
})


test_that("residual plot handles an all-zero matrix without infinite scale limits", {
  skip_on_cran()
  out <- nomo_test_cfa(modification_indices = FALSE)
  nm <- colnames(out$residual_matrix)[1:3]
  out$residual_matrix <- matrix(
    0,
    3,
    3,
    dimnames = list(nm, nm)
  )

  p <- plot(out, type = "residuals")
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 3)
})

# ---- consolidated from test-nomo-cfa-stress.R ----
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("closeout: CFA accepts nomo_model and rejects non-list guidance", {
  dat <- lavaan::HolzingerSwineford1939
  nm <- nomo_model(list(visual = c("x1", "x2", "x3")))
  out <- nomo_cfa(nm, dat, modification_indices = FALSE)
  expect_s3_class(out, "nomo_cfa")

  expect_error(
    nomo_cfa(
      "visual =~ x1 + x2 + x3",
      dat,
      guidance = 1
    ),
    "`guidance`"
  )
})


test_that("closeout: CFA captures residual and modification-index warnings and MI errors", {
  skip_on_cran()
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- lavaan::HolzingerSwineford1939
  model <- "visual =~ x1 + x2 + x3"
  original_residuals <- lavaan::lavResiduals
  original_mi <- lavaan::modificationIndices

  testthat::local_mocked_bindings(
    lavResiduals = function(...) {
      warning("synthetic residual warning")
      original_residuals(...)
    },
    modificationIndices = function(...) {
      warning("synthetic MI warning")
      original_mi(...)
    },
    .package = "lavaan"
  )
  warned <- nomo_cfa(model, dat)
  expect_true(any(grepl("synthetic", warned$engine_warnings, fixed = TRUE)))

  testthat::local_mocked_bindings(
    modificationIndices = function(...) stop("synthetic MI error"),
    .package = "lavaan"
  )
  failed_mi <- nomo_cfa(model, dat)
  expect_equal(nrow(failed_mi$modification_indices), 0L)
  mi_log <- failed_mi$decision_log[
    failed_mi$decision_log$metric == "modification_indices",
    ,
    drop = FALSE
  ]
  expect_gt(nrow(mi_log), 0L)
  expect_true(any(grepl(
    "synthetic MI error",
    mi_log$observation,
    fixed = TRUE
  )))
})


test_that("closeout B: CFA fallbacks recover names when standardized output is unavailable and nobs is unusable", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- lavaan::HolzingerSwineford1939
  original_inspect <- lavaan::lavInspect

  testthat::local_mocked_bindings(
    lavInspect = function(object, what, ...) {
      if (identical(what, "options")) return(list())
      if (identical(what, "nobs")) return(0)
      original_inspect(object, what, ...)
    },
    standardizedSolution = function(...) data.frame(),
    .package = "lavaan"
  )

  out <- nomo_cfa(
    "visual =~ x1 + x2 + x3",
    dat,
    estimator = "MLR",
    modification_indices = FALSE
  )

  expect_true(is.na(out$n_used))
  expect_true(is.na(out$n_dropped))
  expect_true(length(lavaan::lavNames(out$fit, type = "lv")) >= 1L)
})


test_that("closeout B: CFA loading and factor-correlation helpers fill optional statistics with NA", {
  loads <- nomologR:::nomo_cfa_loadings(
    data.frame(
      lhs = "F",
      op = "=~",
      rhs = "x1",
      est.std = .70
    ),
    guidance = nomo_defaults()
  )
  expect_true(is.na(loads$se[[1L]]))
  expect_true(is.na(loads$z[[1L]]))
  expect_true(is.na(loads$p_value[[1L]]))
  expect_true(is.na(loads$ci_lower[[1L]]))
  expect_true(is.na(loads$ci_upper[[1L]]))

  cors <- nomologR:::nomo_cfa_factor_correlations(
    data.frame(
      lhs = "F1",
      op = "~~",
      rhs = "F2",
      est.std = .30
    ),
    latent_names = c("F1", "F2")
  )
  expect_true(is.na(cors$se[[1L]]))
})


test_that("closeout B: CFA summary displays engine/requested estimator differences", {
  skip_on_cran()
  cfa <- make_m9_report_run()$results$cfa
  s <- summary(cfa)
  s$estimator <- "MLR"
  s$estimator_engine <- "ML"

  txt <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(txt, "engine: ML", fixed = TRUE)
})


test_that("closeout C: CFA loading helper returns its stable empty schema when no loading rows exist", {
  standardized <- data.frame(
    lhs = "F",
    op = "~~",
    rhs = "F",
    est.std = 1
  )

  out <- nomologR:::nomo_cfa_loadings(
    standardized,
    guidance = nomo_defaults()
  )

  expect_equal(nrow(out), 0L)
  expect_identical(
    names(out),
    c(
      "factor", "item", "loading", "se", "z", "p_value",
      "ci_lower", "ci_upper", "attention", "explanation"
    )
  )
})


test_that("a model its fit indices cannot test says so, and keeps its warnings (#89)", {
  cont <- nomo_demo_continuous

  # One factor with three indicators is just identified: its fit is perfect
  # by construction, which is not evidence of fit.
  just <- nomo_cfa("A =~ a1 + a2 + a3", data = cont)
  expect_output(print(just), "Fit: not testable (df = 0, just identified)", fixed = TRUE)
  expect_output(print(summary(just)), "The model is just identified (df = 0)", fixed = TRUE)
  log <- just$decision_log
  expect_identical(log$severity[log$metric == "degrees_of_freedom"], "review")
  expect_match(
    nomologR:::nomo_run_key_evidence(list(results = list(cfa = just))),
    "CFA: fit not testable (just identified)", fixed = TRUE
  )

  # Two indicators: lavaan fits it, but it is not identified. Nothing reaches
  # the console as a loose warning; lavaan's warning stays with the model.
  expect_no_warning(under <- nomo_cfa("A =~ a1 + a2", data = cont))
  expect_output(print(under), "Fit: not testable (df = -1, not identified)", fixed = TRUE)
  expect_true(any(grepl("not identified", under$engine_warnings, fixed = TRUE)))
  log <- under$decision_log
  expect_identical(log$severity[log$metric == "degrees_of_freedom"], "concern")
  expect_match(log$observation[log$metric == "degrees_of_freedom"],
               "The model is not identified (df = -1)", fixed = TRUE)
})


test_that("a summary without fit indices says why rather than listing references (#89)", {
  s <- summary(nomo_cfa("A =~ a1 + a2 + a3 + a4", data = nomo_demo_continuous))
  s$fit_evidence$value <- NA_real_
  expect_output(print(s), "No fit index is available.", fixed = TRUE)
})


test_that("an unconverged CFA shows no loadings, fit, or reassurance as results (#145)", {
  stuck <- nomo_cfa(nomo_model(list(A = paste0("a", 1:5), B = paste0("b", 1:5))),
                    data = nomo_demo_continuous, control = list(iter.max = 2))
  expect_false(stuck$converged)
  local_reproducible_output(width = 80)
  txt <- capture.output(print(summary(stuck)))
  # The optimizer's last values are not estimates, so neither they nor the
  # sentences that would reassure about them are printed.
  expect_true("Not converged" %in% txt)
  expect_false(any(c("Standardized loadings", "Factor correlations", "Global fit") %in% txt))
  expect_false(any(grepl("No loading was flagged", txt, fixed = TRUE)))
  expect_false(any(grepl("No improper-solution signal", txt, fixed = TRUE)))
  expect_true(any(grepl("Convergence (Concern): lavaan did not report convergence", txt,
                        fixed = TRUE)))
  expect_true(any(grepl("Optimizer (Review): Researcher-supplied", txt, fixed = TRUE)))

  printed <- capture.output(print(stuck))
  expect_true(any(grepl("did not converge, so its loadings and fit are not estimates",
                        printed, fixed = TRUE)))
  expect_false(any(grepl("Flags: none", printed, fixed = TRUE)))
  expect_match(printed, "1 concern", all = FALSE)
})


# Audit fixes (#145) ------------------------------------------------------------

ordinal_two_factor <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"


test_that("ordered-factor indicators not named in `ordered` are recorded as fitted (#145)", {
  skip_on_cran()
  undeclared <- nomo_cfa(ordinal_two_factor, nomo_demo_ordinal, modification_indices = FALSE)
  declared <- nomo_cfa(ordinal_two_factor, nomo_demo_ordinal, ordered = names(nomo_demo_ordinal),
                       modification_indices = FALSE)

  # lavaan fits these columns as categorical either way; the result now says so.
  expect_identical(undeclared$ordered, names(nomo_demo_ordinal))
  expect_identical(undeclared$estimator, "WLSMV")
  expect_identical(undeclared$estimator_engine, "DWLS")
  expect_identical(undeclared$estimator_source, "ordered_default")
  expect_equal(undeclared$fit_measures_all[["chisq.scaled"]],
               declared$fit_measures_all[["chisq.scaled"]], tolerance = 1e-8)
  expect_output(print(undeclared), "Ordered indicators: 10", fixed = TRUE)

  log <- undeclared$decision_log
  detected <- log[log$metric == "ordered_detected", , drop = FALSE]
  expect_identical(detected$severity, "review")
  expect_identical(detected$value, 10)
  expect_match(log$observation[log$metric == "ordered_indicators"],
               "10 indicators are modeled as ordered: 0 named in `ordered` and 10 stored as ordered factors.",
               fixed = TRUE)
  expect_match(log$observation[log$metric == "estimator"],
               "declared or stored as ordered factors, so WLSMV was requested", fixed = TRUE)
  expect_false(any(grepl("continuous-data default", log$observation, fixed = TRUE)))

  # What follows from the fit treats the indicators as ordered too.
  expect_true(all(c("wlsmv_cfa", "categorical_correlations") %in% nomo_methods_used(undeclared)))
  expect_false("ml_cfa" %in% nomo_methods_used(undeclared))
  rel <- nomo_reliability(undeclared)
  expect_true(all(nomo_reliability_item_types(nomo_measurement_fit(undeclared))$indicator_type ==
                    "ordered"))
  expect_s3_class(rel, "nomo_reliability")

  # Declared and detected indicators are counted separately in the log.
  partly <- nomo_cfa(ordinal_two_factor, nomo_demo_ordinal, ordered = c("a1", "a2"),
                     modification_indices = FALSE)
  expect_identical(partly$ordered, c("a1", "a2", "a3", "a4", "a5", paste0("b", 1:5)))
  expect_match(partly$decision_log$observation[partly$decision_log$metric == "ordered_indicators"],
               "2 named in `ordered` and 8 stored as ordered factors", fixed = TRUE)
  expect_identical(
    partly$decision_log$object[partly$decision_log$metric == "ordered_detected"],
    "a3, a4, a5, b1, b2, b3, b4, b5"
  )
})


test_that("undeclared ordered factors meet the ordered-data estimator checks (#145)", {
  expect_error(
    nomo_cfa(ordinal_two_factor, nomo_demo_ordinal, estimator = "MLR"),
    "These indicators are stored as ordered factors and count as declared: a1, a2",
    fixed = TRUE
  )
  expect_error(
    nomo_cfa(ordinal_two_factor, nomo_demo_ordinal, missing = "fiml"),
    "FIML is not supported by lavaan for declared ordered indicators. These indicators",
    fixed = TRUE
  )

  one <- nomo_demo_continuous
  one$a1 <- ordered(cut(one$a1, c(-Inf, 3, 4, 5, Inf)))
  expect_error(
    nomo_cfa(ordinal_two_factor, one, estimator = "ML"),
    "This indicator is stored as an ordered factor and counts as declared: a1.",
    fixed = TRUE
  )
})


test_that("declared ordered names outside the model are not counted (#145)", {
  skip_on_cran()
  numeric_items <- as.data.frame(lapply(nomo_demo_ordinal, as.numeric))
  out <- nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5", numeric_items,
                  ordered = names(numeric_items), modification_indices = FALSE)

  expect_identical(out$ordered, paste0("a", 1:5))
  expect_output(print(out), "Ordered indicators: 5", fixed = TRUE)
  row <- out$decision_log[out$decision_log$metric == "ordered_indicators", , drop = FALSE]
  expect_identical(row$object, "a1, a2, a3, a4, a5")
  expect_identical(row$value, 5)
  expect_match(row$observation, paste(
    "5 indicators were declared ordered. `ordered` named 5 variables not in the",
    "model, which were not used: b1, b2, b3, b4, b5."
  ), fixed = TRUE)
  expect_false("ordered_detected" %in% out$decision_log$metric)

  # Only names outside the model: nothing is ordered, and the log says why.
  none <- nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5", numeric_items, ordered = "b1",
                   modification_indices = FALSE)
  expect_identical(none$ordered, character())
  expect_identical(none$estimator, "ML")
  expect_match(
    none$decision_log$observation[none$decision_log$metric == "ordered_indicators"],
    "`ordered` named 1 variable not in the model, which was not used: b1.", fixed = TRUE
  )
})


test_that("fit evidence takes every value from one version of the fit (#145)", {
  # lavaan reported no scaled test statistic but a scaled RMSEA of 0 for a
  # model whose standard RMSEA is 0.22: every value is the standard one.
  no_scaled <- c(
    chisq = 754.7, chisq.scaled = NA, df = 19, df.scaled = 19, pvalue = 0,
    pvalue.scaled = NA, cfi = .702, cfi.scaled = NA, cfi.robust = NA,
    tli = .561, tli.scaled = NA, tli.robust = NA, rmsea = .220,
    rmsea.scaled = 0, rmsea.robust = NA, rmsea.ci.lower = .207,
    rmsea.ci.lower.scaled = 0, rmsea.ci.upper = .234, rmsea.ci.upper.scaled = 0,
    srmr = .144
  )
  tab <- nomo_cfa_fit_evidence(no_scaled, nomo_defaults())
  expect_identical(
    tab$variant,
    c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")
  )
  expect_equal(tab$value[tab$metric == "RMSEA"], .220)
  expect_identical(tab$attention[tab$metric == "RMSEA"], "review")
  expect_true(nomo_cfa_scaled_unavailable(no_scaled))
  expect_false(nomo_cfa_scaled_unavailable(c(chisq = 75.8, cfi = .97)))
  expect_false(nomo_cfa_scaled_unavailable(c(chisq = 75.8, chisq.scaled = 74.9)))

  # The interval is the one around the RMSEA reported, or none at all.
  robust_point <- c(
    chisq.scaled = 80, rmsea.robust = .05, rmsea.scaled = .06,
    rmsea.ci.lower.robust = NA, rmsea.ci.lower.scaled = .04,
    rmsea.ci.upper.robust = NA, rmsea.ci.upper.scaled = .08
  )
  tab <- nomo_cfa_fit_evidence(robust_point, nomo_defaults())
  expect_identical(tab$variant[tab$metric == "RMSEA"], "rmsea.robust")
  expect_true(all(is.na(tab$value[tab$metric %in% c("RMSEA_CI_lower", "RMSEA_CI_upper")])))
  tab <- nomo_cfa_fit_evidence(c(chisq = 10, rmsea.ci.lower = .01), nomo_defaults())
  expect_true(is.na(tab$value[tab$metric == "RMSEA_CI_lower"]))

  # Within the scaled version, robust indices come first and scaled ones follow.
  scaled_only <- c(chisq = 75.8, chisq.scaled = 65.8, df = 34, df.scaled = 29.9,
                   cfi = .973, cfi.scaled = .874)
  tab <- nomo_cfa_fit_evidence(scaled_only, nomo_defaults())
  expect_identical(tab$variant[tab$metric %in% c("chi_square", "df", "CFI")],
                   c("chisq.scaled", "df.scaled", "cfi.scaled"))

  log <- nomo_cfa_decision_log(
    estimator_label = "MLR", engine_estimator = "ML", estimator_source = "researcher",
    ordered = character(), missing = NULL, data_n = 100, n_used = 100, converged = TRUE,
    warnings = character(), fit_evidence = tibble::tibble(), loadings = tibble::tibble(),
    heywood = tibble::tibble(), residual_pairs = tibble::tibble(),
    modification_indices = tibble::tibble(), modification_indices_requested = FALSE,
    mi_error = NULL, scaled_unavailable = TRUE
  )
  row <- log[log$metric == "scaled_test_unavailable", , drop = FALSE]
  expect_identical(row$severity, "review")
  expect_match(row$observation, "every fit index reported are the standard", fixed = TRUE)
})


test_that("a fit without a scaled test statistic reports standard values throughout (#145)", {
  skip_on_cran()
  out <- nomo_cfa("Agency =~ ag1 + ag2 + ag3 + pe1; Persistence =~ ag4 + pe2 + pe3 + pe4",
                  nomo_demo_network, estimator = "MLR", modification_indices = FALSE)
  fe <- out$fit_evidence
  if (nomo_cfa_scaled_unavailable(out$fit_measures_all)) {
    expect_false(any(grepl("scaled|robust", fe$variant)))
    expect_equal(fe$value[fe$metric == "RMSEA"], unname(out$fit_measures_all[["rmsea"]]))
    expect_true("scaled_test_unavailable" %in% out$decision_log$metric)
  } else {
    expect_identical(fe$variant[fe$metric == "chi_square"], "chisq.scaled")
    expect_false("scaled_test_unavailable" %in% out$decision_log$metric)
  }
})


test_that("ML with FIML keeps lavaan's robust fit indices beside the standard chi-square (#145)", {
  # No scaled test was requested, so there is no failed scaled statistic: the
  # robust (missing-data corrected) indices are used, with their own interval.
  fiml_like <- c(
    chisq = 81.2, df = 34, pvalue = 1e-5, cfi = .9716, cfi.robust = .9717,
    tli = .9624, tli.robust = .9625, rmsea = .0527, rmsea.robust = .0527,
    rmsea.ci.lower = .0380, rmsea.ci.lower.robust = .0380,
    rmsea.ci.upper = .0675, rmsea.ci.upper.robust = .0677, srmr = .0486
  )
  expect_false(nomo_cfa_scaled_unavailable(fiml_like))
  expected_variants <- c(
    "chisq", "df", "pvalue", "cfi.robust", "tli.robust", "rmsea.robust",
    "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "srmr"
  )
  tab <- nomo_cfa_fit_evidence(fiml_like, nomo_defaults())
  expect_identical(tab$variant, expected_variants)
  expect_equal(tab$value[tab$metric == "RMSEA_CI_upper"], .0677)

  skip_on_cran()
  model <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
  out <- nomo_cfa(model, nomo_demo_continuous, missing = "fiml", modification_indices = FALSE)
  direct <- lavaan::cfa(model, data = nomo_demo_continuous, missing = "fiml")
  fe <- out$fit_evidence
  expect_identical(fe$variant, expected_variants)
  expect_equal(
    fe$value,
    as.numeric(lavaan::fitMeasures(direct, expected_variants)),
    tolerance = 1e-8
  )
  expect_false("scaled_test_unavailable" %in% out$decision_log$metric)
})


test_that("a higher-order disturbance is a latent variance, not an observed residual (#145)", {
  skip_on_cran()
  # The covariance matrix implied by a higher-order model whose first-order
  # factor F1 has a negative disturbance variance. With data reproducing it
  # exactly, the ML estimate is that population value.
  lambda <- kronecker(diag(4), matrix(.7, 3, 1))
  gamma <- c(1, .6, .6, .45)
  phi <- tcrossprod(gamma) + diag(c(-.05, .64, .64, .64))
  sigma <- lambda %*% phi %*% t(lambda) + diag(.51, 12)
  set.seed(14501)
  z <- scale(matrix(rnorm(300 * 12), 300, 12), scale = FALSE)
  dat <- as.data.frame(z %*% solve(chol(stats::cov(z))) %*% chol(sigma))
  names(dat) <- paste0("x", 1:12)

  model <- nomo_model(
    list(F1 = paste0("x", 1:3), F2 = paste0("x", 4:6), F3 = paste0("x", 7:9),
         F4 = paste0("x", 10:12)),
    structure = "higher_order"
  )
  out <- nomo_cfa(model, dat, modification_indices = FALSE)

  f1 <- out$heywood[out$heywood$object == "F1", , drop = FALSE]
  expect_identical(f1$issue, "negative_latent_variance")
  expect_false("negative_observed_residual_variance" %in% out$heywood$issue)
  expect_identical(sum(out$decision_log$object == "F1" &
                         out$decision_log$metric == "negative_latent_variance"), 1L)

  # A weak second-order loading points to the factor, not to item content.
  weak <- out$standardized_loadings[out$standardized_loadings$factor == "G" &
                                      out$standardized_loadings$attention == "REVIEW", ]
  expect_identical(weak$item, "F4")
  expect_match(weak$explanation, "inspect the first-order factor's definition", fixed = TRUE)
  items <- out$standardized_loadings[out$standardized_loadings$factor == "F1", ]
  expect_true(all(items$attention == "KEEP"))
})


test_that("the loading helper words first-order factors and items apart (#145)", {
  std <- data.frame(
    lhs = c("F1", "G"), op = c("=~", "=~"), rhs = c("x1", "F1"),
    est.std = c(.30, .30)
  )
  tab <- nomo_cfa_loadings(std, nomo_defaults())
  expect_match(tab$explanation[[1L]], "inspect item content", fixed = TRUE)
  expect_match(tab$explanation[[2L]], "inspect the first-order factor's definition", fixed = TRUE)
})


# Pre-RC fixes and the shared output style (#144, #145) ----------------------------

hs_three <- "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9"


test_that("a whole-number argument beyond the integer range gets its own message (#145)", {
  expect_error(nomo_cfa(hs_three, lavaan::HolzingerSwineford1939, mi_top = 1e10),
               "`mi_top` must be a non-negative integer.", fixed = TRUE)
  expect_error(nomo_cfa(hs_three, lavaan::HolzingerSwineford1939, mi_top = 2.5),
               "`mi_top` must be a non-negative integer.", fixed = TRUE)
  expect_error(nomo_cfa(hs_three, lavaan::HolzingerSwineford1939, mi_top = -1),
               "`mi_top` must be a non-negative integer.", fixed = TRUE)
})


test_that("references are written as the values they are compared with (#144)", {
  skip_on_cran()
  out <- nomo_cfa(hs_three, lavaan::HolzingerSwineford1939, estimator = "MLR",
                  modification_indices = FALSE)
  weak <- out$standardized_loadings$explanation[out$standardized_loadings$attention == "REVIEW"]
  expect_match(weak, "teaching reference of 0.50;", fixed = TRUE)
  log <- out$decision_log
  expect_identical(log$reference[log$metric == "fit_cfi"], "Configured teaching reference: .950")
  expect_identical(log$reference[log$metric == "fit_rmsea"], "Configured teaching reference: 0.060")
  expect_match(log$observation[log$metric == "largest_residual_correlation"],
               "^Largest absolute residual correlation: 0\\.[0-9]{2}\\.$")
})


test_that("the CFA says which chi-square and which index versions it shows (#145)", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  hs <- lavaan::HolzingerSwineford1939
  mlr <- nomo_cfa(hs_three, hs, estimator = "MLR", modification_indices = FALSE)
  printed <- capture.output(print(mlr))
  expect_true("Fit: CFI .930 | TLI 0.895 | RMSEA 0.092 | SRMR 0.065" %in% printed)
  expect_true("CFI, TLI, and RMSEA are robust values." %in% printed)
  summarized <- paste(capture.output(print(summary(mlr))), collapse = " ")
  expect_match(summarized, "chi-square(24) = 87.13, p < .001   The chi-square is the Yuan-Bentler",
               fixed = TRUE)
  # A flagged value is shown beside its reference, in the same format.
  expect_match(summarized, "CFI (Review): The value, .930, is below the teaching reference of .950",
               fixed = TRUE)
  expect_match(summarized, "RMSEA (Review): The value, 0.092, is above", fixed = TRUE)

  # A fractional df prints with two decimals (#145).
  mlmvs <- nomo_cfa(hs_three, hs, estimator = "MLMVS", modification_indices = FALSE)
  expect_output(print(summary(mlmvs)), "chi-square(19.95) = 67.23, p < .001", fixed = TRUE)
  expect_output(print(summary(mlmvs)), "the mean- and variance-adjusted test statistic",
                fixed = TRUE)

  # For the guided run's key evidence, a value can carry its version.
  expect_identical(
    unname(nomologR:::nomo_cfa_fit_parts(mlr$fit_evidence, c("CFI", "SRMR"), tag = TRUE)),
    c("CFI .930 (robust)", "SRMR 0.065")
  )
})


test_that("the versions note covers each combination of variants (#145)", {
  note <- nomologR:::nomo_cfa_versions_note
  expect_identical(note(c(chi_square = "chisq", CFI = "cfi", TLI = "tli", RMSEA = "rmsea")), "")
  expect_identical(
    note(c(chi_square = "chisq.scaled", CFI = "cfi", TLI = "tli", RMSEA = "rmsea")),
    "The chi-square is the scaled test statistic."
  )
  expect_identical(
    note(c(chi_square = "chisq.scaled", CFI = "cfi.robust", TLI = "tli.robust",
           RMSEA = "rmsea.scaled"), "Yuan-Bentler scaled"),
    paste("The chi-square is the Yuan-Bentler scaled test statistic, CFI and TLI are",
          "robust values, and RMSEA is a scaled value.")
  )

  # A scaled test without a label here is named as lavaan names it.
  fit <- nomo_cfa(hs_three, lavaan::HolzingerSwineford1939, modification_indices = FALSE)$fit
  real <- lavaan::lavInspect
  label <- testthat::with_mocked_bindings(
    nomologR:::nomo_cfa_test_label(fit),
    lavInspect = function(object, what, ...) {
      if (identical(what, "options")) return(list(test = c("standard", "browne.residual.adf")))
      real(object, what, ...)
    },
    .package = "lavaan"
  )
  expect_identical(label, "browne residual adf")
  expect_identical(nomologR:::nomo_cfa_test_label(NULL), "")
})


test_that("correlations a model fixes are not shown as estimates (#145)", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  hs <- lavaan::HolzingerSwineford1939
  factors <- list(visual = c("x1", "x2", "x3"), textual = c("x4", "x5", "x6"),
                  speed = c("x7", "x8", "x9"))
  bifactor <- nomo_cfa(nomo_model(factors, structure = "bifactor"), hs,
                       modification_indices = FALSE)
  txt <- capture.output(print(summary(bifactor)))
  expect_false(any(grepl("Factor 1", txt, fixed = TRUE)))
  expect_match(gsub("\\s+", " ", paste(txt, collapse = " ")),
               "Every factor correlation is fixed at 0 by the model, so not estimated: G with visual",
               fixed = TRUE)
  # The stored table is unchanged.
  expect_identical(nrow(bifactor$factor_correlations), 6L)
  # The improper solution of this fit is listed, with its signal in words.
  expect_true(any(grepl("^  x1 +Negative residual variance +-1\\.14$", txt)))

  # One correlation fixed at zero beside estimated ones, then at another value.
  orth <- nomo_cfa(paste(hs_three, "visual ~~ 0*textual", sep = "\n"), hs,
                   modification_indices = FALSE)
  txt <- paste(capture.output(print(summary(orth))), collapse = " ")
  expect_match(txt, "Fixed at 0 by the model, so not estimated: visual with textual.", fixed = TRUE)
  expect_match(txt, "visual    speed", fixed = TRUE)
  fixed <- nomo_cfa(paste(hs_three, "visual ~~ 0.3*textual", sep = "\n"), hs,
                    modification_indices = FALSE)
  expect_output(print(summary(fixed)),
                "Fixed by the model, so not estimated: visual with textual.", fixed = TRUE)

  # Without a parameter table, nothing is taken to be fixed.
  expect_identical(
    nomologR:::nomo_cfa_fixed_correlations(
      tibble::tibble(factor1 = "A", factor2 = "B"), fit = NULL
    ),
    FALSE
  )
})


test_that("an improper solution is tabled once, with the loading flagged once (#144)", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  heywood <- nomo_cfa("visual =~ x1+x2+x3\ntextual =~ x4+x5+x6\nspeed =~ x7+x8",
                      lavaan::HolzingerSwineford1939)
  txt <- capture.output(print(summary(heywood)))
  expect_true(any(grepl("^  speed =~ x8 +Loading above 1 +1\\.25$", txt)))
  flagged <- txt[(which(txt == "Flagged") + 1L):length(txt)]
  expect_match(flagged[[1L]], "^  - x8 on speed \\(Concern\\): Absolute standardized loading exceeds 1")
  expect_false(any(grepl("speed =~ x8 (Concern)", txt, fixed = TRUE)))
  expect_match(paste(capture.output(print(heywood)), collapse = " "),
               "Improper-solution signals: 2", fixed = TRUE)
})


test_that("second-order loadings sit under an Indicator column (#145)", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  factors <- list(visual = c("x1", "x2", "x3"), textual = c("x4", "x5", "x6"),
                  speed = c("x7", "x8", "x9"))
  ho <- nomo_cfa(nomo_model(factors, structure = "higher_order"),
                 lavaan::HolzingerSwineford1939, modification_indices = FALSE)
  txt <- capture.output(print(summary(ho)))
  expect_match(txt, "^  Factor +Indicator +Loading", all = FALSE)
  expect_match(txt, "^  G +visual +0\\.", all = FALSE)
})


test_that("CFA plots draw flags with the shared status shapes and name the references (#144)", {
  skip_on_cran()
  out <- nomo_cfa(hs_three, lavaan::HolzingerSwineford1939, modification_indices = FALSE)
  out$standardized_loadings$attention[1:3] <- c("KEEP", "REVIEW", "STRONG REVIEW")
  p <- plot(out, type = "loadings")
  guide <- ggplot2::get_guide_data(p, "shape")
  expect_identical(guide$.label, c("No flag", "Review", "Concern"))
  expect_identical(unname(guide$shape), c(16, 1, 15))
  expect_match(p$labels$subtitle, "0.50", fixed = TRUE)

  fit <- plot(out, type = "fit")
  # The reference is a dashed mark, never the cross that means "not computed".
  geoms <- vapply(fit$layers, function(l) class(l$geom)[[1L]], character(1))
  expect_true("GeomErrorbar" %in% geoms)
  expect_false(any(vapply(fit$layers, function(l) identical(l$aes_params$shape, 4), logical(1))))
  expect_match(gsub("\n", " ", fit$labels$caption),
               "References: CFI .950, TLI 0.950, RMSEA 0.060, SRMR 0.080", fixed = TRUE)
  with_mi <- nomo_cfa(hs_three, lavaan::HolzingerSwineford1939, mi_top = 3)
  expect_identical(plot(with_mi, type = "modification_indices")$labels$subtitle,
                   "Post hoc diagnostic evidence only")
})


test_that("nomo_model() refuses names lavaan would rename or cannot read (#145)", {
  expect_error(
    nomo_model(list(visual = c("x1", "x2", "x3"), "self-efficacy" = c("x4", "x5", "x6"))),
    'Factor name cannot be read by lavaan: "self-efficacy". Use letters, digits, `.`, and `_`, starting with a letter, for example "self.efficacy".',
    fixed = TRUE
  )
  expect_error(nomo_model(list("1st" = c("x1", "x2"), "my factor" = c("x3", "x4"))),
               'Factor names cannot be read by lavaan: "1st", "my factor".', fixed = TRUE)
  expect_error(nomo_model(list(A = c("item 1", "item2"))),
               'Indicator name cannot be read by lavaan: "item 1".', fixed = TRUE)
  expect_error(nomo_model(list(A = c("x1", "x2", "x3"), B = c("x4", "x5", "x6"),
                               C = c("x7", "x8", "x9")),
                          structure = "higher_order", general = "g-factor"),
               'The `general` factor name cannot be read by lavaan: "g-factor".', fixed = TRUE)
  # Dots and underscores are names lavaan reads as written.
  model <- nomo_model(list(self.efficacy = c("x1", "x2", "x3"), self_worth = c("x4", "x5", "x6")))
  fitted <- nomo_cfa(model, lavaan::HolzingerSwineford1939, modification_indices = FALSE)
  expect_identical(unique(fitted$standardized_loadings$factor), c("self.efficacy", "self_worth"))
})
