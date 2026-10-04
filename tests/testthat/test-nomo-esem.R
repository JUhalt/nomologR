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
  expect_identical(c(fc$factor1, fc$factor2), c("A", "B"))
  expect_lt(fc$esem, fc$cfa)
  expect_equal(fc$difference, fc$esem - fc$cfa)

  # The CFA is nested in the ESEM: its extra df are the fixed cross-loadings
  # less the rotation's m(m - 1) constraints.
  expect_identical(es$models$model, c("ESEM", "CFA"))
  expect_identical(names(es$comparisons), c("chisq_diff", "df_diff", "p_value"))
  expect_identical(es$comparisons$df_diff, es$models$df[[2L]] - es$models$df[[1L]])
  expect_identical(es$comparisons$df_diff, 8L - 2L)
  expect_lt(es$comparisons$p_value, .001)
  expect_identical(names(es$fits), c("ESEM", "CFA"))
  expect_s4_class(es$fits$ESEM, "lavaan")
  expect_equal(es$models$chisq[[2L]], unname(lavaan::fitMeasures(es$fits$CFA, "chisq")))
  expect_identical(es$ordered, character())
  expect_identical(es$estimator, NA_character_)
  expect_identical(es$missing, NA_character_)

  log <- es$decision_log
  expect_identical(
    log$metric,
    c("esem_rotation", "cases_used", "esem_comparison", "esem_cross_loading",
      "esem_weak_main_loading")
  )
  expect_identical(log$severity[log$metric == "cases_used"], "info")
  expect_identical(es$data_n, 800L)
  expect_identical(es$engine_warnings, list(ESEM = character(), CFA = character(),
                                            comparison = character()))
  expect_identical(log$severity[log$metric == "esem_comparison"], "review")
  expect_match(log$recommendation[log$metric == "esem_comparison"], "prefer the ESEM")
  expect_match(log$observation[log$metric == "esem_cross_loading"], "a3 on B")
  expect_match(log$observation[log$metric == "esem_weak_main_loading"], "b4 on B")

  expect_identical(nomo_methods_used(es), c("esem", "ml_cfa"))
  expect_true("esem" %in% nomo_methods(es)$id)
  expect_identical(nomo_table(es), es$loadings)
  expect_identical(nomo_table(es, "factor_correlations"), es$factor_correlations)
  expect_identical(nomo_table(es, "models"), es$models)
  expect_identical(nomo_table(es, "comparisons"), es$comparisons)
  expect_error(nomo_table(es, "fit"), "`type` must be one of")
})


test_that("without cross-loadings the CFA is the more parsimonious account", {
  skip_on_cran()
  es <- nomo_esem(esem_model, esem_data(cross = FALSE), rotation = "geomin")
  expect_identical(es$rotation, "geomin")
  log <- es$decision_log
  expect_identical(log$metric, c("esem_rotation", "cases_used", "esem_comparison"))
  expect_match(log$observation[[1L]], "Geomin")
  expect_identical(log$severity[log$metric == "esem_comparison"], "info")
  expect_match(log$recommendation[log$metric == "esem_comparison"], "more parsimonious")
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
  expect_identical(lavaan::lavInspect(es$fits$ESEM, "options")$estimator, "ML")
  expect_identical(lavaan::lavInspect(es$fits$CFA, "options")$missing, "ml")
  expect_true("yuan.bentler.mplus" %in% lavaan::lavInspect(es$fits$ESEM, "options")$test)
  expect_true(is.finite(es$comparisons$p_value))
  # The models table reports the scaled chi-square, as nomo_cfa() does.
  expect_equal(es$models$chisq[[1L]],
               unname(lavaan::fitMeasures(es$fits$ESEM, "chisq.scaled")))
  expect_identical(es$estimator, "MLR")
  expect_identical(es$missing, "fiml")
  expect_identical(nomo_methods_used(es), c("esem", "ml_cfa", "fiml"))

  cut_items <- d
  for (v in names(cut_items)) cut_items[[v]] <- as.integer(cut(cut_items[[v]], c(-Inf, -1, 0, 1, Inf)))
  cut_items <- stats::na.omit(cut_items)
  es_ord <- nomo_esem(esem_model, cut_items, ordered = names(cut_items))
  expect_identical(lavaan::lavInspect(es_ord$fits$ESEM, "options")$estimator, "DWLS")
  expect_true(all(is.finite(es_ord$loadings$loading)))
  expect_true(is.finite(es_ord$models$cfi[[1L]]))
  expect_identical(es_ord$ordered, names(cut_items))
  # Ordered indicators are credited with WLSMV and polychoric correlations,
  # not maximum likelihood.
  expect_identical(nomo_methods_used(es_ord),
                   c("esem", "wlsmv_cfa", "categorical_correlations"))
  expect_true("wlsmv_cfa" %in% nomo_methods(es_ord)$id)
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


# Pre-RC fixes and the shared output style (#144, #145) ----------------------------

esem_two <- nomo_model(list(Agency = paste0("ag", 1:4), Persistence = paste0("pe", 1:4)))


test_that("guidance is checked before any model is fitted (#145)", {
  expect_error(nomo_esem(esem_model, esem_data(cross = FALSE, n = 200), guidance = list()),
               "`guidance` is missing required setting `efa_crossloading_reference`.",
               fixed = TRUE)
  bad <- nomo_defaults()
  bad$efa_loading_reference <- "high"
  expect_error(nomo_esem(esem_model, esem_data(cross = FALSE, n = 200), guidance = bad),
               "`guidance$efa_loading_reference` must be one finite numeric value.",
               fixed = TRUE)
})


test_that("an improper ESEM keeps lavaan's warnings and is flagged as a concern (#145)", {
  skip_on_cran()
  set.seed(5)
  small <- nomo_demo_network[sample(800, 14), ]
  expect_no_warning(es <- nomo_esem(esem_two, small))
  expect_true(any(grepl("variances are negative", es$engine_warnings$ESEM, fixed = TRUE)))
  log <- es$decision_log
  expect_true(all(c("engine_warning", "negative_observed_residual_variance",
                    "standardized_loading_beyond_one") %in% log$metric))
  concern <- log[log$severity == "concern", , drop = FALSE]
  expect_true("ESEM ag1" %in% concern$object)
  expect_match(concern$observation[concern$metric == "negative_observed_residual_variance"][[1L]],
               "The value in the ESEM is -0.03.", fixed = TRUE)
  local_reproducible_output(width = 80)
  printed <- capture.output(print(es))
  flagged <- printed[(which(printed == "Flagged") + 1L):length(printed)]
  expect_match(flagged[[1L]], "\\(Concern\\)")
  expect_match(printed, "ESEM ag1", all = FALSE, fixed = TRUE)
  # lavaan's message has no full stop; the summary adds one before the
  # recommendation that follows it, and print() gives the message alone.
  expect_false(grepl("[.]$", es$engine_warnings$ESEM[[1L]]))
  summarized <- paste(trimws(capture.output(print(summary(es)))), collapse = " ")
  expect_match(summarized, "variances are negative. Inspect the warning in context",
               fixed = TRUE)
  expect_match(paste(trimws(printed), collapse = " "), "variances are negative. ",
               fixed = TRUE)
})


test_that("an ESEM or CFA that does not converge is named, with what lavaan said (#145)", {
  skip_on_cran()
  set.seed(204)
  tiny <- nomo_demo_network[sample(800, 10), ]
  expect_error(nomo_esem(esem_two, tiny),
               "The ESEM did not converge, so the ESEM and the CFA cannot be compared.",
               fixed = TRUE)
  expect_error(nomo_esem(esem_two, tiny), "lavaan reported: ", fixed = TRUE)

  # Without a warning from lavaan, the message says only what failed.
  real <- lavaan::lavInspect
  err <- tryCatch(
    testthat::with_mocked_bindings(
      nomo_esem(esem_model, esem_data(cross = FALSE, n = 300)),
      lavInspect = function(object, what, ...) {
        if (identical(what, "converged")) return(FALSE)
        real(object, what, ...)
      },
      .package = "lavaan"
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "well defined.$")
})


test_that("cases lavaan did not use are reported in print, summary, and the log (#145)", {
  skip_on_cran()
  d <- nomo_demo_network
  set.seed(1)
  d$ag1[sample(800, 200)] <- NA
  es <- nomo_esem(esem_two, d)
  expect_identical(es$n, 600L)
  expect_identical(es$data_n, 800L)
  row <- es$decision_log[es$decision_log$metric == "cases_used", , drop = FALSE]
  expect_identical(row$severity, "review")
  expect_match(row$observation, "600 of 800 input cases were used; 200 cases were not used",
               fixed = TRUE)
  local_reproducible_output(width = 80)
  expect_output(print(es), "Cases: 600 of 800 used", fixed = TRUE)
  expect_output(print(es), "Cases (Review): 600 of 800 input cases", fixed = TRUE)
  expect_output(print(summary(es)), "Cases: 600 of 800 used", fixed = TRUE)
})


test_that("ESEM output names its sources, test, and versions in the shared style (#144, #145)", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  es <- esem_cross()
  printed <- capture.output(print(es))
  expect_identical(printed[[1L]], "<nomo_esem> ESEM beside its CFA")
  expect_match(printed[[2L]], "^Asparouhov and Muth.n \\(2009\\); Marsh et al\\. \\(2014\\)\\.$")
  expect_match(printed, "^  CFA vs\\. ESEM: Delta chi-square\\(6\\) = [0-9.]+, p < \\.001\\.$",
               all = FALSE)
  expect_match(printed, "^  Model +Chi-square +df +p +CFI", all = FALSE)
  expect_match(printed, "^  A with B +\\.[0-9]{2} +\\.[0-9]{2} +-\\.[0-9]{2}$", all = FALSE)
  expect_match(printed, "ESEM = exploratory structural equation modeling", all = FALSE,
               fixed = TRUE)
  expect_match(printed[[length(printed)]], "for every fit index\\.$")
  summarized <- capture.output(print(summary(es)))
  expect_true("What these columns mean" %in% summarized)
  expect_match(summarized, "^  a3 +0\\.[0-9]{2} +0\\.[0-9]{2} +0\\.[0-9]{2}$", all = FALSE)

  # A robust estimator: the scaled chi-square and its difference test are named.
  mlr <- nomo_esem(esem_model, esem_data(cross = FALSE, n = 300), estimator = "MLR")
  text <- gsub("\\s+", " ", paste(capture.output(print(mlr)), collapse = " "))
  expect_match(text, "The chi-square is the Yuan-Bentler scaled test statistic", fixed = TRUE)
  expect_match(text, "scaled difference test (Satorra & Bentler, 2001).", fixed = TRUE)
  expect_identical(mlr$comparison_method, "satorra.bentler.2001")

  # A difference test lavaan could not give is said to be missing.
  no_test <- es
  no_test$comparisons$chisq_diff <- NA_real_
  expect_output(print(no_test), "CFA vs. ESEM: no difference test is available.", fixed = TRUE)

  # A warning from the difference test is logged under the comparison, and a
  # log without a sample count has no row for it.
  log <- nomologR:::nomo_esem_log(es$loadings, es$factor_correlations, es$models,
                                  es$comparisons, "target", c(cross = .30, main = .40),
                                  engine_warnings = list(comparison = "mocked LRT warning"))
  row <- log[log$metric == "engine_warning", , drop = FALSE]
  expect_identical(row$object, "ESEM vs. CFA")
  expect_identical(row$observation, "mocked LRT warning")
  expect_false("cases_used" %in% log$metric)
})
