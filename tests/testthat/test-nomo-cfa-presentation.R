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
  out <- nomo_test_cfa()
  txt <- capture.output(print(out))

  expect_true(any(grepl("<nomo_cfa>", txt, fixed = TRUE)))
  expect_true(any(grepl("Converged: yes", txt, fixed = TRUE)))
  expect_true(any(grepl("Global fit:", txt, fixed = TRUE)))
  expect_true(any(grepl("Loading review:", txt, fixed = TRUE)))
  expect_true(any(grepl("No parameters were automatically freed", txt, fixed = TRUE)))
})


test_that("CFA print method handles engine labels, warnings, and unavailable values", {
  out <- nomo_test_cfa(modification_indices = FALSE)
  out$n_used <- NA_real_
  out$estimator <- "WLSMV"
  out$estimator_engine <- "DWLS"
  out$converged <- FALSE
  out$engine_warnings <- "synthetic warning"
  out$fit_evidence$value[out$fit_evidence$metric %in% c("CFI", "TLI", "RMSEA", "SRMR")] <- NA_real_

  txt <- capture.output(print(out))
  expect_true(any(grepl("unknown used", txt, fixed = TRUE)))
  expect_true(any(grepl("engine: DWLS", txt, fixed = TRUE)))
  expect_true(any(grepl("Converged: NO", txt, fixed = TRUE)))
  expect_true(any(grepl("Captured engine warnings: 1", txt, fixed = TRUE)))
  expect_false(any(grepl("Global fit:", txt, fixed = TRUE)))
})


test_that("summary printer covers flagged and diagnostic sections", {
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

  txt <- capture.output(print(s))
  expect_true(any(grepl("Case use:", txt, fixed = TRUE)))
  expect_true(any(grepl("Loading flags requiring inspection", txt, fixed = TRUE)))
  expect_true(any(grepl("Factor correlations", txt, fixed = TRUE)))
  expect_true(any(grepl("Improper-solution / Heywood signals", txt, fixed = TRUE)))
  expect_true(any(grepl("Largest localized residual correlations", txt, fixed = TRUE)))
  expect_true(any(grepl("Top modification indices - diagnostic only", txt, fixed = TRUE)))
  expect_true(any(grepl("Captured engine warnings", txt, fixed = TRUE)))
  expect_true(any(grepl("no single cutoff establishes model validity", txt, fixed = TRUE)))
})


test_that("summary printer handles clean optional sections", {
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
  expect_true(any(grepl("No configured standardized-loading review flags", txt, fixed = TRUE)))
  expect_true(any(grepl("No configured Heywood/improper-solution signal", txt, fixed = TRUE)))
  expect_false(any(grepl("Case use:", txt, fixed = TRUE)))
  expect_false(any(grepl("Top modification indices", txt, fixed = TRUE)))
})


test_that("all CFA plot views contain interpretable data", {
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
  expect_equal(nrow(p_res$data), choose(nrow(out$residual_matrix), 2))
  expect_lte(nrow(p_mi$data), 5)
})


test_that("CFA plot methods fail informatively when evidence is unavailable", {
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
