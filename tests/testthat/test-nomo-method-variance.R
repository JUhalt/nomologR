# Marker-based method variance (#129) ----------------------------------------------

mv_population <- function(method = rep(.3, 11), r = .4) {
  items <- c(paste0("a", 1:4), paste0("b", 1:4), paste0("m", 1:3))
  paste(
    "A =~ 0.7*a1 + 0.7*a2 + 0.6*a3 + 0.6*a4",
    "B =~ 0.7*b1 + 0.6*b2 + 0.6*b3 + 0.5*b4",
    "M =~ 0.7*m1 + 0.7*m2 + 0.6*m3",
    paste0("CMV =~ ", paste0(method, "*", items, collapse = " + ")),
    sprintf("A ~~ %s*B", r),
    "A ~~ 0*M", "B ~~ 0*M", "CMV ~~ 0*A + 0*B + 0*M",
    sep = "\n"
  )
}
mv_data <- function(population, n = 600, seed = 2010) {
  set.seed(seed)
  lavaan::simulateData(population, sample.nobs = n, standardized = TRUE)
}
mv_model <- "A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4"
mv_marker <- c("m1", "m2", "m3")

mv_equal <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- nomo_method_variance(mv_model, mv_data(mv_population()), marker = mv_marker)
    }
    cache
  }
})


test_that("equal method effects are detected and Method-C is retained", {
  skip_on_cran()
  mv <- mv_equal()
  expect_s3_class(mv, "nomo_method_variance")
  expect_identical(mv$models$model, c("CFA", "Baseline", "Method-C", "Method-U", "Method-R",
                                      "Method-S(.05)", "Method-S(.01)"))
  expect_identical(names(mv$models),
                   c("model", "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
  expect_equal(mv$models$tli[[1L]], unname(lavaan::fitMeasures(mv$fits$CFA, "tli")))
  cmp <- mv$comparisons
  expect_identical(cmp$comparison, c("Baseline vs. Method-C", "Method-C vs. Method-U",
                                     "Method-C vs. Method-R"))
  expect_identical(names(cmp), c("comparison", "question", "chisq_diff", "df_diff", "p_value"))
  expect_identical(cmp$question, c(
    "Is marker-based method variance present?", "Are the method effects equal?",
    "Does the method variance bias the substantive correlations?"
  ))
  expect_identical(cmp$df_diff, c(1L, 7L, 1L))
  expect_lt(cmp$p_value[[1L]], .05)
  expect_gt(cmp$p_value[[2L]], .05)
  expect_identical(mv$retained, "Method-C")
  # Each comparison's degrees of freedom are the difference of its models'.
  df <- stats::setNames(mv$models$df, mv$models$model)
  expect_identical(unname(df[["Baseline"]] - df[["Method-C"]]), 1L)
  expect_identical(unname(df[["Method-C"]] - df[["Method-U"]]), 7L)

  # The marker's measurement parameters are fixed at their CFA values.
  pe_cfa <- lavaan::parameterEstimates(mv$fits$CFA)
  pe_base <- lavaan::parameterEstimates(mv$fits$Baseline)
  marker_rows <- function(pe) pe[pe$op == "=~" & pe$lhs == "Marker", "est"]
  expect_equal(marker_rows(pe_base), marker_rows(pe_cfa), tolerance = 1e-8)
  expect_true(all(pe_base$se[pe_base$op == "=~" & pe_base$lhs == "Marker"] == 0))

  # Equations 1-3: the method share is the method part over the Baseline total.
  rel <- mv$reliability
  expect_identical(rel$factor, c("A", "B"))
  expect_equal(rel$method_share, rel$reliability_method / rel$reliability_total)
  expect_true(all(rel$reliability_substantive < rel$reliability_total))
  expect_true(all(rel$reliability_method > 0))

  loadings <- nomo_table(mv, "loadings")
  expect_identical(loadings$item, c(paste0("a", 1:4), paste0("b", 1:4)))
  expect_equal(loadings$method_variance, loadings$method_loading^2)
  expect_true(all(loadings$p_value >= 0 & loadings$p_value <= 1))

  cor <- nomo_table(mv, "correlations")
  expect_identical(c(cor$factor1, cor$factor2), c("A", "B"))
  expect_identical(
    grep("p_value", names(cor), value = TRUE),
    c("retained_p_value", "method_s_05_p_value", "method_s_01_p_value")
  )
  expect_equal(cor$baseline, cor$cfa, tolerance = .01)
  expect_identical(mv$marker_correlations$factor1, c("A", "B"))
  expect_identical(mv$marker_correlations$factor2, c("Marker", "Marker"))
  expect_identical(nomo_table(mv), mv$comparisons)
  expect_identical(nomo_table(mv, "reliability"), mv$reliability)
  expect_true("cfa_marker_technique" %in% nomo_methods(mv)$id)
  expect_identical(mv$estimator, NA_character_)
  expect_identical(mv$missing, NA_character_)
  expect_identical(nomo_methods_used(mv), c("cfa_marker_technique", "ml_cfa"))

  log <- mv$decision_log
  expect_identical(log$severity[log$metric == "method_variance_presence"], "review")
  expect_match(log$observation[log$metric == "method_variance_equality"],
               "consistent with equality", fixed = TRUE)
  expect_identical(log$severity[log$metric == "method_variance_bias"], "info")
  expect_match(log$recommendation[log$metric == "marker_assumption"],
               "Richardson, Simmering, & Sturman, 2009", fixed = TRUE)
})


test_that("unequal method effects retain Method-U, and strong ones bias the correlation", {
  skip_on_cran()
  unequal <- c(.1, .2, .5, .6, .1, .2, .5, .6, .3, .3, .3)
  mv <- nomo_method_variance(
    mv_model, mv_data(mv_population(method = unequal, r = .2), n = 1500),
    marker = mv_marker
  )
  expect_identical(mv$retained, "Method-U")
  expect_identical(mv$comparisons$comparison[[3L]], "Method-U vs. Method-R")
  expect_lt(mv$comparisons$p_value[[2L]], .05)
  log <- mv$decision_log
  expect_match(log$recommendation[log$metric == "method_variance_equality"],
               "contradict", fixed = TRUE)
  # The method loadings follow the unequal population effects.
  method <- mv$method_loadings$method_loading
  expect_gt(mean(method[c(3, 4, 7, 8)]), mean(method[c(1, 2, 5, 6)]))
})


test_that("without method variance, none is reported", {
  skip_on_cran()
  mv <- nomo_method_variance(
    mv_model, mv_data(mv_population(method = rep(0, 11)), seed = 3), marker = mv_marker
  )
  log <- mv$decision_log
  presence <- log[log$metric == "method_variance_presence", ]
  expect_identical(presence$severity, "info")
  expect_match(presence$observation, "not detected", fixed = TRUE)
  expect_match(presence$recommendation, "Absence of evidence", fixed = TRUE)
})


test_that("a one-factor model has no correlations to bias", {
  skip_on_cran()
  dat <- mv_data(mv_population())
  mv <- nomo_method_variance("A =~ a1 + a2 + a3 + a4", dat, marker = c("m1", "m2"))
  expect_true(is.na(mv$comparisons$p_value[[3L]]))
  expect_identical(nrow(mv$correlations), 0L)
  expect_false("Method-R" %in% mv$models$model)
  expect_false(any(c("method_variance_bias", "method_variance_sensitivity") %in%
                     mv$decision_log$metric))
  expect_match(mv$decision_log$recommendation[mv$decision_log$metric == "marker_assumption"],
               "three or more marker indicators", fixed = TRUE)
  local_reproducible_output(width = 80)
  printed <- capture.output(print(mv))
  expect_false(any(grepl("Substantive correlations", printed, fixed = TRUE)))
})


test_that("a robust estimator and FIML are passed to lavaan, and fit is reported scaled", {
  skip_on_cran()
  dat <- mv_data(mv_population())
  dat$a1[1:20] <- NA
  mv <- nomo_method_variance(mv_model, dat, marker = mv_marker,
                             estimator = "MLR", missing = "fiml")
  expect_identical(lavaan::lavInspect(mv$fits$`Method-C`, "options")$estimator, "ML")
  expect_identical(lavaan::lavInspect(mv$fits$`Method-C`, "options")$missing, "ml")
  expect_identical(lavaan::lavInspect(mv$fits$`Method-C`, "options")$se, "robust.huber.white")
  expect_identical(mv$estimator, "MLR")
  expect_identical(mv$missing, "fiml")
  expect_identical(nomo_methods_used(mv), c("cfa_marker_technique", "ml_cfa", "fiml"))

  # The models table reports the scaled chi-square and the robust indices, as
  # nomo_cfa() does, rather than the ML statistics.
  measures <- lavaan::fitMeasures(
    mv$fits$`Method-C`,
    c("chisq", "chisq.scaled", "pvalue.scaled", "cfi.robust", "tli.robust", "rmsea.robust")
  )
  row <- mv$models[mv$models$model == "Method-C", ]
  expect_equal(row$chisq, unname(measures[["chisq.scaled"]]))
  expect_false(isTRUE(all.equal(row$chisq, unname(measures[["chisq"]]))))
  expect_equal(row$pvalue, unname(measures[["pvalue.scaled"]]))
  expect_equal(row$cfi, unname(measures[["cfi.robust"]]))
  expect_equal(row$tli, unname(measures[["tli.robust"]]))
  expect_equal(row$rmsea, unname(measures[["rmsea.robust"]]))
})


test_that("the results print and summarize within 80 columns", {
  skip_on_cran()
  mv <- mv_equal()
  local_reproducible_output(width = 80)
  printed <- capture.output(print(mv))
  expect_match(printed, "Retained: Method-C", fixed = TRUE, all = FALSE)
  expect_match(printed, "Baseline vs. Method-C", fixed = TRUE, all = FALSE)
  # Each comparison says what it asks, the correlations are headed by the
  # models' own names, and the flagged log entries are listed (#89).
  expect_match(printed, "Baseline vs. Method-C  Method variance present?", fixed = TRUE,
               all = FALSE)
  expect_match(printed, "Baseline  Method-C  Method-S(.05)  Method-S(.01)", fixed = TRUE,
               all = FALSE)
  expect_match(printed, "(Review): Marker-based method variance is present", fixed = TRUE,
               all = FALSE)
  # The closing note defines the models the tables name.
  note <- paste(printed, collapse = " ")
  expect_match(note, "Method-C: Baseline plus equal marker loadings", fixed = TRUE)
  expect_match(note, "Method-S(.01): the method loadings fixed at the upper ends",
               fixed = TRUE)
  expect_false(any(nchar(printed) > 80L))
  summarized <- capture.output(print(summary(mv)))
  expect_match(summarized, "Loadings in Method-C (completely standardized)",
               fixed = TRUE, all = FALSE)
  expect_match(summarized, "Method-S(.01)", fixed = TRUE, all = FALSE)
  # Percentages are right-aligned with the numbers beside them.
  expect_match(summarized, "Method p  Method variance$", all = FALSE)
  expect_match(summarized, "^  A +0\\.[0-9]{3} +0\\.[0-9]{3} +0\\.[0-9]{3} +[0-9.]+%$",
               all = FALSE)
  expect_false(any(nchar(summarized) > 80L))
})


test_that("each comparison's short question comes from its stored question (#89)", {
  questions <- nomologR:::nomo_method_variance_questions
  # Out of order, and one question with no short form, which is shown as stored.
  comparisons <- tibble::tibble(
    comparison = c("Method-C vs. Method-R", "Baseline vs. Method-C", "Other"),
    question = c(questions[["bias", "question"]], questions[["presence", "question"]],
                 "Asked?"),
    chisq_diff = c(1, 2, 3), df_diff = 1L, p_value = .5
  )
  local_reproducible_output(width = 80)
  shown <- capture.output(nomologR:::nomo_method_variance_present_comparisons(comparisons))
  expect_match(shown, "Method-C vs. Method-R  Correlations biased?", fixed = TRUE,
               all = FALSE)
  expect_match(shown, "Baseline vs. Method-C  Method variance present?", fixed = TRUE,
               all = FALSE)
  expect_match(shown, "Other                  Asked?", fixed = TRUE, all = FALSE)
})


test_that("arguments and data are checked", {
  dat <- data.frame(a1 = 1:5, a2 = 1:5, m1 = 1:5, m2 = 1:5)
  model <- "A =~ a1 + a2"
  expect_error(nomo_method_variance("", dat, c("m1", "m2")), "non-empty lavaan model")
  expect_error(nomo_method_variance(model, list(), c("m1", "m2")), "non-empty data frame")
  expect_error(nomo_method_variance(model, dat, c("m1", "m2"), alpha = .7), "`alpha` must be")
  expect_error(nomo_method_variance(model, dat, c("m1", "m2"), marker_name = ""),
               "`marker_name` must be")
  expect_error(nomo_method_variance(model, dat, c("m1", "m2"), marker_name = "A"),
               "already a factor or column")
  expect_error(nomo_method_variance(model, dat, "m1"), "two or more distinct columns")
  expect_error(
    nomo_method_variance(nomo_model(list(A = c("a1", "a2"))), dat, "m1"),
    "two or more distinct columns"
  )
  expect_error(nomo_method_variance(model, dat, c("a1", "m1")), "cannot also be substantive")
  expect_error(nomo_method_variance(model, dat, c("m1", "m9")), "Columns not in `data`: m9.",
               fixed = TRUE)
  words <- dat
  words$m2 <- letters[1:5]
  expect_error(nomo_method_variance(model, words, c("m1", "m2")), "Not numeric: m2")
  expect_error(nomo_method_variance("A ~ a1", dat, c("m1", "m2")), "at least one factor")
  expect_error(nomo_method_variance("A =~ a1 + a2 + m1", dat, c("m1", "m2")),
               "cannot also be substantive")
  expect_error(nomo_method_variance("A =~ a1 + a2\nB =~ a2", dat, c("m1", "m2")),
               "load on one factor")
  expect_error(nomo_method_variance(model, dat, c("m1", "m2"), estimator = "NOPE"),
               "The CFA model could not be fitted")
  expect_identical(nomologR:::nomo_present_p_text(.5), "= .500")
  expect_identical(nomologR:::nomo_present_p_text(.0001), "< .001")
})


test_that("a biased correlation, and one whose significance the sensitivity changes, are flagged", {
  comparisons <- tibble::tibble(
    comparison = c("Baseline vs. Method-C", "Method-C vs. Method-U", "Method-U vs. Method-R"),
    question = "q", chisq_diff = c(20, 30, 9), df_diff = c(1L, 7L, 1L),
    p_value = c(.00001, .0001, .003)
  )
  correlations <- tibble::tibble(
    factor1 = c("A", "A"), factor2 = c("B", "C"),
    cfa = .4, baseline = .4, retained = c(.3, .1),
    retained_p_value = c(.001, .04), method_s_05 = c(.3, .08),
    method_s_05_p_value = c(.001, .06), method_s_01 = c(.3, .07),
    method_s_01_p_value = c(.001, .09)
  )
  reliability <- tibble::tibble(factor = "A", reliability_total = .8,
                                reliability_substantive = .7, reliability_method = .1,
                                method_share = .125)
  log <- nomologR:::nomo_method_variance_log(
    c("m1", "m2", "m3"), comparisons, "Method-U", reliability, correlations, .05
  )
  bias <- log[log$metric == "method_variance_bias", ]
  expect_identical(bias$severity, "review")
  expect_match(bias$observation, "The method variance biases", fixed = TRUE)
  expect_match(bias$recommendation, "Compare the Baseline and Method-U", fixed = TRUE)
  sensitivity <- log[log$metric == "method_variance_sensitivity", ]
  expect_identical(sensitivity$severity, "review")
  expect_match(sensitivity$observation, "the significance of A with C changes", fixed = TRUE)
  expect_match(log$observation[log$metric == "method_variance_reliability"], "A 12.5%",
               fixed = TRUE)
})
