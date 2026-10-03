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


test_that("each construct pair is one row, with its correlation and HTMT together (#89)", {
  cfa <- nomo_cfa(
    "F1 =~ x1 + x2 + x3\nF2 =~ x4 + x5 + x6\nF3 =~ x7 + x8 + x9",
    data = lavaan::HolzingerSwineford1939
  )
  out <- nomo_validity(cfa)
  tab <- summary(out)$discriminant

  # Three constructs have three pairs, listed in model order.
  expect_identical(nrow(tab), 3L)
  expect_identical(paste(tab$construct_1, tab$construct_2),
                   c("F1 F2", "F1 F3", "F2 F3"))
  expect_true(all(is.finite(tab$latent_r)))
  expect_true(all(is.finite(tab$HTMT2)))
  expect_true(all(tab$signal %in% c("info", "review")))

  # Reorienting a pair keeps each value with its pair.
  lat <- out$latent_correlations
  h2 <- out$htmt2
  for (i in seq_len(nrow(tab))) {
    pair <- c(tab$construct_1[[i]], tab$construct_2[[i]])
    in_pair <- function(d) d$construct_1 %in% pair & d$construct_2 %in% pair
    expect_equal(tab$latent_r[[i]], lat$correlation[in_pair(lat)])
    expect_equal(tab$HTMT2[[i]], h2$estimate[in_pair(h2)])
  }

  printed <- utils::capture.output(print(out))
  expect_true(any(grepl("across 3 pairs", printed, fixed = TRUE)))

  p <- plot(out, type = "discriminant")
  expect_identical(levels(p$data$pair), rev(c("F1 vs F2", "F1 vs F3", "F2 vs F3")))
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

  # Every factor here has the cross-loaded indicator, so semTools returns AVE
  # as NA for both. The object still prints, summarizes, and tabulates (#145).
  expect_identical(out$ave$construct, c("F1", "F2"))
  expect_true(all(is.na(out$ave$estimate)))
  expect_identical(out$ave$attention, c("unavailable", "unavailable"))
  local_reproducible_output(width = 80)
  expect_no_error(capture.output(print(out)))
  expect_no_error(capture.output(print(summary(out))))
  convergent <- nomo_table(out, "convergent")
  expect_identical(convergent$construct, c("F1", "F2"))
  expect_true(all(is.na(convergent$AVE)))
  expect_identical(nrow(nomo_table(out, "discriminant")), 1L)
  seen <- character()
  legacy <- withCallingHandlers(
    nomo_validity(fit, fornell_larcker = TRUE),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("block", seen, fixed = TRUE)))
  expect_true(all(legacy$fornell_larcker_pairs$attention == "unavailable"))

  # The expected NA is not reported as an inadmissible value.
  ave_log <- out$decision_log[out$decision_log$metric == "AVE", ]
  expect_identical(ave_log$severity, c("info", "info"))
  expect_match(ave_log$observation, "not computed for a factor with a cross-loaded indicator",
               fixed = TRUE)
  expect_match(ave_log$recommendation, "do not change indicator membership", fixed = TRUE)
})


test_that("only the factors that share a cross-loaded indicator lose their AVE", {
  fit <- lavaan::cfa(
    "visual =~ x1 + x2 + x3 + x9\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9",
    data = lavaan::HolzingerSwineford1939
  )
  out <- nomo_validity(fit)
  ave <- out$ave
  expect_identical(ave$attention[ave$construct != "textual"], c("unavailable", "unavailable"))
  expect_identical(ave$attention[ave$construct == "textual"], "info")
  log <- out$decision_log[out$decision_log$metric == "AVE", ]
  expect_false(any(log$severity == "concern"))
})


test_that("a convergent table is built when no AVE table could be made", {
  fit <- lavaan::cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
                     data = nomo_demo_continuous)
  out <- nomo_validity(fit, htmt = "none")
  out$ave <- tibble::tibble()
  convergent <- nomo_validity_convergent_table(out)
  expect_identical(convergent$construct, c("A", "B"))
  expect_true(all(is.na(convergent$AVE)))
})


test_that("HTMT follows the CFA's missing-data handling by default and records its cases", {
  skip_on_cran()
  model <- nomo_model(list(A = paste0("a", 1:5), B = paste0("b", 1:5)))
  fiml <- nomo_cfa(model, data = nomo_demo_continuous, missing = "fiml")

  # The default uses the cases the FIML CFA used, as htmt_missing = "fiml" does.
  default <- nomo_validity(fiml)
  explicit <- nomo_validity(fiml, htmt_missing = "fiml")
  expect_equal(default$htmt2$estimate, explicit$htmt2$estimate)
  expect_equal(default$htmt$estimate, explicit$htmt$estimate)
  expect_identical(default$htmt_status$missing, c("fiml", "fiml"))
  expect_identical(default$htmt_status$n, c(500L, 500L))
  entry <- default$decision_log[default$decision_log$metric == "htmt_missing_data", ]
  expect_identical(entry$severity, "info")
  expect_identical(entry$value, 500)
  expect_match(entry$observation, "the fitted model's own missing-data handling", fixed = TRUE)
  expect_match(entry$recommendation, "use the same missing-data handling", fixed = TRUE)
  # "direct" names the same full-information handling as the FIML CFA's.
  direct <- nomo_validity(fiml, htmt_missing = "direct", htmt = "htmt2")
  entry <- direct$decision_log[direct$decision_log$metric == "htmt_missing_data", ]
  expect_identical(entry$severity, "info")
  expect_match(entry$observation, "(as requested, the fitted model's own", fixed = TRUE)

  # A pairwise CFA rests on pairwise counts too: HTMT under the same handling
  # is not flagged, whether by default or on request (#145).
  pairwise_cfa <- nomo_cfa(model, data = nomo_demo_continuous, missing = "pairwise")
  for (requested in c("default", "pairwise")) {
    same <- nomo_validity(pairwise_cfa, htmt_missing = requested, htmt = "htmt2")
    expect_identical(same$htmt_status$missing, "pairwise")
    expect_identical(same$htmt_status$n, 473L)
    entry <- same$decision_log[same$decision_log$metric == "htmt_missing_data", ]
    expect_identical(entry$severity, "info")
    expect_match(entry$observation, "the smallest number of cases for any pair", fixed = TRUE)
    expect_match(entry$recommendation, "use the same missing-data handling", fixed = TRUE)
    expect_no_match(entry$recommendation, "set `htmt_missing`", fixed = TRUE)
  }
  # Listwise deletion on request after a pairwise CFA drops cases, for review.
  dropped <- nomo_validity(pairwise_cfa, htmt_missing = "listwise", htmt = "htmt2")
  entry <- dropped$decision_log[dropped$decision_log$metric == "htmt_missing_data", ]
  expect_identical(entry$severity, "review")
  expect_match(entry$recommendation, "rests on fewer cases than the CFA", fixed = TRUE)

  # Listwise deletion on request drops cases the CFA used, and the log says so.
  listwise <- nomo_validity(fiml, htmt_missing = "listwise")
  expect_identical(listwise$htmt_status$n, c(473L, 473L))
  expect_false(isTRUE(all.equal(listwise$htmt2$estimate, default$htmt2$estimate)))
  entry <- listwise$decision_log[listwise$decision_log$metric == "htmt_missing_data", ]
  expect_identical(entry$severity, "review")
  expect_match(entry$observation, "(as requested): n = 473, the complete cases, of the 500",
               fixed = TRUE)
  expect_match(entry$recommendation, "rests on fewer cases than the CFA", fixed = TRUE)

  # A listwise CFA keeps listwise deletion, on the same cases.
  plain_cfa <- nomo_cfa(model, data = nomo_demo_continuous)
  plain <- nomo_validity(plain_cfa)
  expect_identical(plain$htmt_status$missing, c("listwise", "listwise"))
  expect_identical(plain$htmt_status$n, c(473L, 473L))
  expect_equal(plain$htmt2$estimate, listwise$htmt2$estimate)
  # A different handling on as many cases as the CFA is not flagged.
  other <- nomo_validity(plain_cfa, htmt_missing = "fiml", htmt = "htmt2")
  entry <- other$decision_log[other$decision_log$metric == "htmt_missing_data", ]
  expect_identical(entry$severity, "info")
  expect_match(entry$observation, "(as requested): n = 473", fixed = TRUE)
  expect_match(entry$recommendation, "rest on the same cases", fixed = TRUE)
  pairwise <- nomo_validity(fiml, htmt_missing = "pairwise", htmt = "htmt2")
  expect_match(
    pairwise$decision_log$observation[pairwise$decision_log$metric == "htmt_missing_data"],
    "the smallest number of cases for any pair", fixed = TRUE
  )

  # Not computed: no handling or count is recorded, and nothing is logged.
  none <- nomo_validity(fiml, htmt = "none")
  expect_true(all(is.na(none$htmt_status$n)))
  expect_false("htmt_missing_data" %in% none$decision_log$metric)
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
  # Latent correlations name their groups as the AVE table does, not by number.
  expect_setequal(out$latent_correlations$block, c("Pasteur", "Grant-White"))
  expect_setequal(out$latent_correlations$block, out$ave$block)
})


test_that("validity argument checks are explicit", {
  expect_error(nomo_validity(NULL, ave_obs_var = NA), "ave_obs_var")
  expect_error(nomo_validity(NULL, htmt = "wrong"), "`htmt` must be one of", fixed = TRUE)
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


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("closeout: validity AVE and HTMT engine failure paths are retained as evidence", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  cfa <- make_m9_report_run()$results$cfa

  testthat::local_mocked_bindings(
    AVE = function(...) stop("synthetic AVE failure"),
    .package = "semTools"
  )
  expect_error(
    nomo_validity(cfa, htmt = "none"),
    "AVE estimation failed"
  )

  testthat::local_mocked_bindings(
    AVE = function(...) {
      warning("synthetic AVE warning")
      c(WellBeing = 1.20)
    },
    .package = "semTools"
  )
  out <- nomo_validity(cfa, htmt = "none")
  expect_identical(out$ave$attention[[1L]], "concern")
  expect_true(length(out$ave_warnings) >= 1L)
})


test_that("closeout: validity HTMT failures and inadmissible values are explicit", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa(
    "
      visual =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    ",
    data = dat
  )
  expect_true(lavaan::lavInspect(fit, "converged"))

  testthat::local_mocked_bindings(
    htmt = function(...) stop("synthetic HTMT failure"),
    .package = "semTools"
  )
  failed <- nomo_validity(fit, htmt = "both")
  expect_true(all(failed$htmt_status$requested))
  expect_true(all(!failed$htmt_status$available))

  bad_mat <- matrix(
    c(1, -.20, -.20, 1),
    2, 2,
    dimnames = list(c("visual", "textual"), c("visual", "textual"))
  )
  testthat::local_mocked_bindings(
    htmt = function(...) bad_mat,
    .package = "semTools"
  )
  bad <- nomo_validity(fit, htmt = "both")
  expect_true(any(bad$discriminant$attention == "concern"))
  expect_true(any(grepl(
    "unavailable or inadmissible",
    bad$discriminant$interpretation,
    fixed = TRUE
  )))
})


test_that("closeout: validity grouped Fornell-Larcker and post-check concern paths are explicit", {
  dat <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa(
    "
      visual =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    ",
    data = dat,
    group = "school"
  )
  expect_true(lavaan::lavInspect(fit, "converged"))

  grouped <- nomo_validity(
    fit,
    htmt = "none",
    fornell_larcker = TRUE
  )
  expect_match(
    grouped$fornell_larcker_reason,
    "restricted to a single-group",
    fixed = TRUE
  )

  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )
  base_cfa <- make_m9_report_run()$results$cfa
  original_measurement_fit <- nomologR:::nomo_measurement_fit
  testthat::local_mocked_bindings(
    nomo_measurement_fit = function(...) {
      z <- original_measurement_fit(...)
      z$post_check <- FALSE
      z
    },
    .package = "nomologR"
  )
  strained <- nomo_validity(base_cfa, htmt = "none")
  expect_true(any(
    strained$decision_log$metric == "lavaan_post_check" &
      strained$decision_log$severity == "concern"
  ))
})


test_that("closeout B: validity handles an empty standardized-loading evidence table", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  cfa <- make_m9_report_run()$results$cfa

  testthat::local_mocked_bindings(
    nomo_validity_standardized_loadings = function(...) tibble::tibble(),
    .package = "nomologR"
  )

  out <- nomo_validity(cfa, htmt = "none")
  expect_equal(nrow(out$standardized_loadings), 0L)
  expect_false(any(out$decision_log$metric == "standardized_loading"))
})

test_that("a single construct has no pairs, and its output raises no warnings", {
  v <- nomo_validity(nomo_cfa("A =~ a1 + a2 + a3 + a4", data = nomo_demo_continuous))
  expect_identical(nrow(nomologR:::nomo_validity_discriminant_table(v)), 0L)
  expect_no_warning(print(v))
  expect_no_warning(print(summary(v)))
  expect_no_warning(evidence <- nomologR:::nomo_run_key_evidence(list(results = list(validity = v))))
  expect_match(evidence, "separation flags none", fixed = TRUE)
})
