# Exploratory structural equation modeling (#129) -----------------------------------

esem_data <- function(cross = TRUE, n = 800, seed = 2009) {
  population <- if (cross) {
    paste(
      "A =~ 0.7*a1 + 0.7*a2 + 0.6*a3 + 0.6*a4 + 0.25*b1",
      "B =~ 0.7*b1 + 0.6*b2 + 0.6*b3 + 0.3*b4 + 0.45*a3",
      "A ~~ 0.4*B",
      sep = "\n"
    )
  } else {
    paste(
      "A =~ 0.7*a1 + 0.7*a2 + 0.6*a3 + 0.6*a4",
      "B =~ 0.7*b1 + 0.6*b2 + 0.6*b3 + 0.5*b4",
      "A ~~ 0.4*B",
      sep = "\n"
    )
  }
  set.seed(seed)
  lavaan::simulateData(population, sample.nobs = n, standardized = TRUE)
}
esem_model <- "A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4"

esem_cross <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- nomo_esem(esem_model, esem_data())
    cache
  }
})


test_that("ESEM recovers cross-loadings the CFA inflates the correlation with", {
  skip_on_cran()
  es <- esem_cross()
  expect_s3_class(es, "nomo_esem")
  expect_identical(es$rotation, "target")
  expect_identical(es$n, 800L)

  ld <- es$loadings
  expect_identical(nrow(ld), 16L)
  expect_identical(ld$item[1:2], c("a1", "a1"))
  expect_identical(ld$role[1:2], c("main", "cross"))
  expect_true(all(is.na(ld$cfa_loading[ld$role == "cross"])))
  expect_true(all(is.finite(ld$se)))
  a3_on_b <- ld$loading[ld$item == "a3" & ld$factor == "B"]
  expect_gt(a3_on_b, .30)

  # The CFA forces the cross-loadings into the factor correlation.
  fc <- es$factor_correlations
  expect_identical(nrow(fc), 1L)
  expect_lt(fc$esem, fc$cfa)
  expect_equal(fc$difference, fc$esem - fc$cfa)

  # The CFA is nested in the ESEM: its extra df are the fixed cross-loadings
  # less the rotation's m(m - 1) constraints.
  expect_identical(es$fit$model, c("ESEM", "CFA"))
  expect_identical(es$comparison$delta_df, es$fit$df[[2L]] - es$fit$df[[1L]])
  expect_identical(es$comparison$delta_df, 8L - 2L)
  expect_lt(es$comparison$p_value, .001)

  log <- es$decision_log
  expect_identical(
    log$metric,
    c("esem_rotation", "esem_comparison", "esem_cross_loading", "esem_weak_main_loading")
  )
  expect_identical(log$severity[log$metric == "esem_comparison"], "review")
  expect_match(log$recommendation[log$metric == "esem_comparison"], "prefer the ESEM")
  expect_match(log$observation[log$metric == "esem_cross_loading"], "a3 on B")
  expect_match(log$observation[log$metric == "esem_weak_main_loading"], "b4 on B")

  expect_identical(nomo_methods_used(es), c("esem", "ml_cfa"))
  expect_true("esem" %in% nomo_methods(es)$id)
  expect_identical(nomo_table(es), es$loadings)
  expect_identical(nomo_table(es, "factor_correlations"), es$factor_correlations)
  expect_identical(nomo_table(es, "comparison"), es$comparison)
})


test_that("without cross-loadings the CFA is the more parsimonious account", {
  skip_on_cran()
  es <- nomo_esem(esem_model, esem_data(cross = FALSE), rotation = "geomin")
  expect_identical(es$rotation, "geomin")
  log <- es$decision_log
  expect_identical(log$metric, c("esem_rotation", "esem_comparison"))
  expect_match(log$observation[[1L]], "Geomin")
  expect_identical(log$severity[[2L]], "info")
  expect_match(log$recommendation[[2L]], "more parsimonious")
  expect_lt(abs(es$factor_correlations$difference), .05)

  local_reproducible_output(width = 80)
  printed <- capture.output(print(es))
  expect_false(any(grepl("Flagged", printed)))
  summarized <- capture.output(print(summary(es)))
  expect_false(any(grepl("Flagged", summarized)))
})


test_that("a nomo_model, a robust estimator, FIML, and ordered items are passed on", {
  skip_on_cran()
  d <- esem_data(cross = FALSE, n = 500)
  d$a1[1:20] <- NA
  model <- nomo_model(list(A = paste0("a", 1:4), B = paste0("b", 1:4)))
  es <- nomo_esem(model, d, estimator = "MLR", missing = "fiml")
  expect_identical(lavaan::lavInspect(es$esem_fit, "options")$estimator, "ML")
  expect_identical(lavaan::lavInspect(es$cfa_fit, "options")$missing, "ml")
  expect_true("yuan.bentler.mplus" %in% lavaan::lavInspect(es$esem_fit, "options")$test)
  expect_true(is.finite(es$comparison$p_value))

  cut_items <- d
  for (v in names(cut_items)) cut_items[[v]] <- as.integer(cut(cut_items[[v]], c(-Inf, -1, 0, 1, Inf)))
  cut_items <- stats::na.omit(cut_items)
  es_ord <- nomo_esem(esem_model, cut_items, ordered = names(cut_items))
  expect_identical(lavaan::lavInspect(es_ord$esem_fit, "options")$estimator, "DWLS")
  expect_true(all(is.finite(es_ord$loadings$loading)))
  expect_true(is.finite(es_ord$fit$cfi[[1L]]))
})


test_that("the results print and summarize within 80 columns", {
  skip_on_cran()
  es <- esem_cross()
  local_reproducible_output(width = 80)
  printed <- capture.output(print(es))
  expect_true(all(nchar(printed) <= 80))
  expect_true(any(grepl("Flagged", printed)))
  expect_true(any(grepl("A with B", printed)))
  summarized <- capture.output(print(summary(es)))
  expect_true(all(nchar(summarized) <= 80))
  expect_true(any(grepl("ESEM loadings", summarized)))
  expect_true(any(grepl("^  a3 ", summarized)))
  expect_s3_class(summary(es), "summary_nomo_esem")
})


test_that("arguments and models are checked", {
  d <- esem_data(cross = FALSE, n = 200)
  expect_error(nomo_esem(1, d), "one non-empty lavaan model string")
  expect_error(nomo_esem(c(esem_model, esem_model), d), "one non-empty")
  expect_error(nomo_esem("  ", d), "one non-empty")
  expect_error(nomo_esem(esem_model, d[0, ]), "non-empty data frame")
  expect_error(nomo_esem(esem_model, as.list(d)), "non-empty data frame")
  expect_error(nomo_esem("A =~ a1 + a2 + a3 + a4", d), "two or more factors")
  expect_error(nomo_esem(paste(esem_model, "A ~ B", sep = "\n"), d), "measurement model")
  expect_error(nomo_esem(paste(esem_model, "a1 ~~ a2", sep = "\n"), d), "measurement model")
  expect_error(nomo_esem(paste(esem_model, "G =~ A + B", sep = "\n"), d), "measurement model")
  expect_error(nomo_esem("A =~ 1*a1 + a2 + a3\nB =~ b1 + b2 + b3", d), "cannot fix or label")
  expect_error(nomo_esem("A =~ a1 + a2 + a3\nB =~ a3 + b2 + b3", d), "load on one factor")
  expect_error(nomo_esem("A =~ a1 + a2 + zz\nB =~ b1 + b2 + b3", d), "zz")
  expect_error(nomo_esem(esem_model, d, rotation = "varimax"), "rotation")
  expect_error(
    nomo_esem(esem_model, d, estimator = "NOT_AN_ESTIMATOR"),
    "The ESEM could not be fitted"
  )
})
