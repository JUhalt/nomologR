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
  # The comparison that was not run is left out and said to be, rather than
  # shown as a row of "--" and "NA" (#145).
  expect_false(any(grepl("NA", printed, fixed = TRUE)))
  expect_false(any(grepl("Correlations biased?", printed, fixed = TRUE)))
  expect_match(gsub("[[:space:]]+", " ", paste(printed, collapse = " ")), "so Method-R is not fitted.",
               fixed = TRUE)
  expect_false(any(grepl("Method-R --", printed, fixed = TRUE)))
  # A two-indicator marker is flagged for review, with the reason (#145).
  marker <- mv$decision_log[mv$decision_log$metric == "marker_assumption", ]
  expect_identical(marker$severity, "review")
  expect_match(marker$observation, "identified only through its correlations", fixed = TRUE)
  expect_match(printed, "Marker (Review): The marker is measured by m1, m2", fixed = TRUE,
               all = FALSE)
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

  # The summary says which versions it shows (#145).
  local_reproducible_output(width = 80)
  summarized <- gsub("[[:space:]]+", " ", paste(capture.output(print(summary(mv))), collapse = " "))
  expect_match(summarized, paste(
    "The chi-square is the Yuan-Bentler scaled test statistic, and the model comparisons",
    "are scaled difference tests; CFI, TLI, and RMSEA are robust values."
  ), fixed = TRUE)
  expect_match(summarized, "MLR -- Maximum likelihood with robust standard errors", fixed = TRUE)
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
  # A key defines the models the tables name (#144).
  note <- gsub("\\s+", " ", paste(printed, collapse = " "))
  expect_match(note, "Method-C -- Baseline plus equal marker loadings", fixed = TRUE)
  expect_match(note, "Method-S(.05), Method-S(.01) -- The retained method loadings fixed at the ends",
               fixed = TRUE)
  # The header cites the technique, the cases are counted, the numbers follow
  # their kinds, and the print ends with a pointer (#144).
  expect_identical(printed[1:2], c("<nomo_method_variance> Marker-based method variance",
                                   "Williams, Hartman, and Cavazotte (2010)."))
  expect_match(printed, "Cases: 600 | Estimator: ML", fixed = TRUE, all = FALSE)
  expect_match(printed, "^  A +\\.[0-9]{2} +\\.[0-9]{2} +\\.[0-9]{2} +[0-9.]+%$", all = FALSE)
  expect_match(printed, "^  A with B +\\.[0-9]{3} +\\.[0-9]{3}", all = FALSE)
  expect_match(note, "df = degrees of freedom; ML = maximum likelihood.", fixed = TRUE)
  expect_match(printed[[length(printed)]], "for every recorded decision.", fixed = TRUE)
  expect_false(any(grepl("p-value|NA", printed)))
  expect_false(any(nchar(printed) > 80L))
  summarized <- capture.output(print(summary(mv)))
  expect_match(summarized, "Loadings in Method-C (completely standardized)",
               fixed = TRUE, all = FALSE)
  expect_match(summarized, "Method-S(.01)", fixed = TRUE, all = FALSE)
  # Percentages are right-aligned with the numbers beside them.
  expect_match(summarized, "Method p  Method variance$", all = FALSE)
  expect_match(summarized, "^  A +\\.[0-9]{2} +\\.[0-9]{2} +\\.[0-9]{2} +[0-9.]+%$",
               all = FALSE)
  expect_match(summarized, "^  A +a1 +0\\.[0-9]{2} +0\\.[0-9]{2} +< \\.001", all = FALSE)
  expect_match(summarized, "TLI -- Tucker-Lewis index.", fixed = TRUE, all = FALSE)
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


test_that("a missing degrees of freedom prints as missing, never NA (#145)", {
  comparisons <- tibble::tibble(
    comparison = "Method-C vs. Method-R", question = "Asked?",
    chisq_diff = NA_real_, df_diff = NA_integer_, p_value = NA_real_
  )
  local_reproducible_output(width = 80)
  shown <- capture.output(nomologR:::nomo_method_variance_present_comparisons(comparisons))
  expect_false(any(grepl("NA", shown, fixed = TRUE)))
})


test_that("opposite-signed method effects are not reported as absent (#145)", {
  skip_on_cran()
  # The marker loads +.35 on the A items and -.35 on the B items: under
  # Method-C's equality constraint they cancel, and Method-U is retained.
  mixed <- c(rep(.35, 4), rep(-.35, 4), .35, .35, .35)
  mv <- nomo_method_variance(mv_model, mv_data(mv_population(method = mixed), n = 800, seed = 11),
                             marker = mv_marker)
  expect_identical(mv$retained, "Method-U")
  expect_gt(mv$comparisons$p_value[[1L]], .05)
  presence <- mv$decision_log[mv$decision_log$metric == "method_variance_presence", ]
  expect_identical(presence$severity, "review")
  expect_match(presence$observation, "not detected under equal effects", fixed = TRUE)
  expect_match(presence$observation, "Method-U is retained", fixed = TRUE)
  expect_match(presence$observation, "Delta chi-square(8) = ", fixed = TRUE)
  expect_match(presence$recommendation, "opposite signs cancel", fixed = TRUE)
  # The published sequence of comparisons is unchanged.
  expect_identical(nrow(mv$comparisons), 3L)
})


test_that("the sensitivity models move negative method loadings away from zero (#145)", {
  skip_on_cran()
  negative <- c(rep(-.3, 8), .3, .3, .3)
  mv <- nomo_method_variance(mv_model, mv_data(mv_population(method = negative), n = 800, seed = 12),
                             marker = mv_marker)
  items <- c(paste0("a", 1:4), paste0("b", 1:4))
  marker_loadings <- function(fit) {
    pe <- lavaan::parameterEstimates(fit)
    pe$est[pe$op == "=~" & pe$lhs == "Marker" & pe$rhs %in% items]
  }
  retained <- marker_loadings(mv$fits[[mv$retained]])
  expect_true(all(retained < 0))
  s05 <- marker_loadings(mv$fits$`Method-S(.05)`)
  s01 <- marker_loadings(mv$fits$`Method-S(.01)`)
  expect_true(all(s05 < retained))
  expect_true(all(s01 < s05))
  sensitivity <- mv$decision_log[mv$decision_log$metric == "method_variance_sensitivity", ]
  expect_match(sensitivity$reference, "farther from zero", fixed = TRUE)
})


test_that("an improper or unconverged CFA stops the technique with its reason (#145)", {
  skip_on_cran()
  # A two-indicator marker unrelated to the substantive items: the CFA gives m1
  # a negative error variance, which had been fixed into every later model.
  two <- mv_data(paste(
    "A =~ 0.7*a1 + 0.7*a2 + 0.6*a3 + 0.6*a4", "B =~ 0.7*b1 + 0.6*b2 + 0.6*b3 + 0.5*b4",
    "M =~ 0.7*m1 + 0.7*m2", "A ~~ 0.4*B", "A ~~ 0*M", "B ~~ 0*M", sep = "
"
  ), n = 300, seed = 68)
  # Some platforms' optimizers (macOS) stop short of convergence on these data
  # instead of reaching the negative variance; either way the technique stops
  # with its reason.
  expect_error(
    nomo_method_variance(mv_model, two, marker = c("m1", "m2")),
    "The CFA model (gives the marker a negative error variance for m1|did not converge)"
  )
  # The negative-variance message itself, on fixed estimates.
  negative <- data.frame(lhs = c("Marker", "Marker", "m1", "m2"), op = c("=~", "=~", "~~", "~~"),
                         rhs = c("m1", "m2", "m1", "m2"), est = c(.9, .4, -.05, .5), se = .1)
  expect_error(nomologR:::nomo_method_variance_check_cfa(negative, c("m1", "m2"), "Marker"),
               "The CFA model gives the marker a negative error variance for m1", fixed = TRUE)
  expect_error(nomologR:::nomo_method_variance_check_cfa(negative, c("m1", "m2"), "Marker"),
               "identified only through its correlations", fixed = TRUE)
  # Forty cases: the CFA does not converge, and that is said.
  small <- mv_data(mv_population(), n = 40, seed = 7)
  expect_error(
    nomo_method_variance(mv_model, small, marker = mv_marker),
    "The CFA model did not converge", fixed = TRUE
  )
  # Standard errors that cannot be computed are reported too.
  pe <- data.frame(lhs = c("Marker", "Marker", "m1", "m2", "m3"), op = c("=~", "=~", "~~", "~~", "~~"),
                   rhs = c("m1", "m2", "m1", "m2", "m3"), est = c(.7, .6, .5, .5, .5),
                   se = c(NA, .1, .1, .1, .1))
  expect_error(nomologR:::nomo_method_variance_check_cfa(pe, c("m1", "m2", "m3"), "Marker"),
               "The CFA model gives the marker no standard errors for its parameters", fixed = TRUE)
  pe$se <- .1
  expect_invisible(nomologR:::nomo_method_variance_check_cfa(pe, c("m1", "m2", "m3"), "Marker"))
})


test_that("lavaan warnings reach the log, and an improper model is a concern (#145)", {
  skip_on_cran()
  dat <- mv_data(mv_population())
  # A variance 2500 times the others makes lavaan warn at every model, and
  # ten incomplete cases are left out.
  dat$a1 <- dat$a1 * 50
  dat$b1[1:10] <- NA
  mv <- nomo_method_variance(mv_model, dat, marker = mv_marker)
  expect_true(length(mv$engine_warnings$CFA) > 0L)
  warned <- mv$decision_log[mv$decision_log$metric == "engine_warning", ]
  expect_true(nrow(warned) > 0L)
  expect_true(all(warned$severity == "review"))
  expect_match(warned$observation[[1L]], "lavaan warned when fitting the CFA model:", fixed = TRUE)
  local_reproducible_output(width = 80)
  printed <- capture.output(print(mv))
  expect_match(printed, "CFA (Review): lavaan warned", fixed = TRUE, all = FALSE)
  expect_match(printed, "Cases: 590 of 600 used", fixed = TRUE, all = FALSE)
  cases <- mv$decision_log[mv$decision_log$metric == "cases_used", ]
  expect_identical(cases$severity, "review")
  expect_identical(cases$stage, "method_variance")
  expect_match(printed, "Cases (Review): 590 of 600 input cases were used", fixed = TRUE,
               all = FALSE)

  # A model whose solution fails lavaan's check is a concern, with the check's
  # own words; other warnings stay review rows.
  log <- nomologR:::nomo_method_variance_engine_log(
    list(
      Baseline = c("lavaan->lav_object_post_check():\n   some estimated ov variances are negative",
                   "another warning"),
      `Method-U` = character(),
      `Baseline vs. Method-C` = "a comparison warning"
    ),
    c(Baseline = FALSE, `Method-U` = FALSE)
  )
  expect_identical(log$severity, c("concern", "review", "concern", "review"))
  # In plain words, each a sentence of its own (#145).
  expect_identical(
    log$observation[[1L]],
    "The Baseline solution is improper: some estimated observed-variable variances are negative."
  )
  expect_identical(log$observation[[2L]],
                   "lavaan warned when fitting the Baseline model: another warning.")
  expect_match(log$observation[[3L]], "admissibility check failed", fixed = TRUE)
  expect_match(log$observation[[4L]], "fitting the comparison Baseline vs. Method-C", fixed = TRUE)
  # Two failed checks make one sentence.
  both <- nomologR:::nomo_method_variance_engine_log(
    list(CFA = c("lavaan->lav_object_post_check():\n   some estimated ov variances are negative",
                 "lavaan->lav_object_post_check():\n   some estimated lv variances are negative")),
    c(CFA = FALSE)
  )
  expect_identical(both$observation, paste(
    "The CFA solution is improper: some estimated observed-variable variances are negative;",
    "some estimated latent-variable variances are negative."
  ))
})


test_that("the marker name must be a syntactic name (#145)", {
  dat <- data.frame(a1 = 1:5, a2 = 1:5, m1 = 1:5, m2 = 1:5)
  expect_error(
    nomo_method_variance("A =~ a1 + a2", dat, c("m1", "m2"), marker_name = "Social desirability"),
    "`marker_name` must be a syntactic name, such as \"Socialdesirability\"", fixed = TRUE
  )
})


test_that("the method-variance display helpers cover their edge cases (#144)", {
  local_reproducible_output(width = 80)
  facts <- capture.output(nomologR:::nomo_method_variance_present_facts(list(
    marker = c("m1", "m2", "m3"), n = 600, data_n = 600, estimator_shown = NA_character_,
    retained = "Method-C", alpha = .01
  )))
  expect_identical(facts, c(
    "<nomo_method_variance> Marker-based method variance",
    "Williams, Hartman, and Cavazotte (2010).",
    "Marker: m1, m2, m3 | Cases: 600",
    "Retained: Method-C | Comparisons at alpha = .01"
  ))
  expect_null(nomologR:::nomo_method_variance_present_flagged(tibble::tibble()))
  captured <- nomologR:::nomo_method_variance_capture({
    warning("first")
    warning("first")
    "value"
  })
  expect_identical(captured, list(value = "value", warnings = "first"))
})
