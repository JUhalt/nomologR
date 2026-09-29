# Longitudinal measurement invariance (#129) -----------------------------------------

long_model <- "Wellbeing =~ w1 + w2 + w3 + w4"
long_occasions <- c("t1", "t2", "t3")

long_default <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- nomo_invariance_longitudinal(
        long_model, nomo_demo_longitudinal, occasions = long_occasions
      )
    }
    cache
  }
})


test_that("the drifting w3 intercept shows up at the scalar level", {
  skip_on_cran()
  long <- long_default()
  expect_s3_class(long, c("nomo_invariance_longitudinal", "nomo_invariance"))
  expect_identical(long$occasions, long_occasions)
  expect_identical(long$completed_levels, c("configural", "metric", "scalar", "strict"))
  expect_identical(long$long_items$w3, c("w3_t1", "w3_t2", "w3_t3"))
  expect_identical(long$long_factors$Wellbeing, c("Wellbeing_t1", "Wellbeing_t2", "Wellbeing_t3"))
  expect_match(long$longitudinal_model, "Wellbeing_t2 =~ w1_t2 + w2_t2 + w3_t2 + w4_t2", fixed = TRUE)

  # Every pair of occasions carries a residual covariance for each item.
  pe <- lavaan::parameterEstimates(long$fits$configural)
  auto <- pe[pe$op == "~~" & pe$lhs != pe$rhs & grepl("^w", pe$lhs), , drop = FALSE]
  expect_identical(nrow(auto), 12L)
  expect_identical(long$fit_evidence$df[[1L]], 39)

  fit <- long$fit_evidence
  expect_lt(fit$lrt_p[fit$level == "scalar"], .001)
  expect_lt(fit$delta_cfi[fit$level == "scalar"], -.02)

  strain <- nomo_table(long, "local_strain")
  top <- strain[order(-strain$score_x2), , drop = FALSE]
  expect_identical(top$constraint_display[[1L]], "Intercept: w3 (t1 vs. t3)")
  expect_true("Loading: Wellbeing -> w4 (t1 vs. t3)" %in% strain$constraint_display)

  # Holding the drifting intercept equal inflates the latent change.
  means <- nomo_table(long, "latent_means")
  expect_identical(unique(means$level), c("scalar", "strict"))
  expect_identical(means$occasion[1:2], c("t2", "t3"))
  expect_identical(means$reference_occasion[[1L]], "t1")
  expect_gt(means$estimate[[2L]], .60)

  log <- long$decision_log
  expect_identical(log$metric[1:2], c("occasions", "residual_autocorrelation"))
  expect_match(log$observation[[1L]], "3 occasions")
  expect_match(log$observation[[2L]], "every other occasion")
  expect_identical(log$metric[[nrow(log)]], "latent_change")
  expect_match(log$observation[[nrow(log)]], "Wellbeing at t3")

  expect_identical(
    nomo_methods_used(long),
    c("longitudinal_invariance", "invariance_hierarchy", "invariance_delta_fit",
      "score_diagnostics")
  )
  expect_true("longitudinal_invariance" %in% nomo_methods(long)$id)
  apa <- nomo_apa_table(long)
  expect_identical(apa$title, "Measurement Invariance Across Occasions")
  expect_match(apa$notes$general, "Occasions: t1, t2, t3", fixed = TRUE)
  expect_s3_class(plot(long), "ggplot")
})


test_that("releasing the w3 intercept recovers the population change", {
  skip_on_cran()
  release <- nomo_partial(
    level = "scalar", syntax = "w3 ~ 1",
    rationale = "The score diagnostics point to the w3 intercept."
  )
  long <- nomo_invariance_longitudinal(
    nomo_model(list(Wellbeing = paste0("w", 1:4))), nomo_demo_longitudinal,
    occasions = long_occasions, levels = c("configural", "metric", "scalar"),
    partial = release
  )
  fit <- long$fit_evidence
  # The release frees w3's intercept at t2 and t3.
  expect_identical(fit$df[fit$level == "scalar"] - fit$df[fit$level == "metric"], 4)
  expect_gt(fit$lrt_p[fit$level == "scalar"], .05)
  means <- long$latent_means
  expect_identical(means$level, c("scalar", "scalar"))
  # Population change: .30 and .50 in t1 standard deviations.
  expect_true(means$ci_lower[[1L]] < .30 && .30 < means$ci_upper[[1L]])
  expect_true(means$ci_lower[[2L]] < .50 && .50 < means$ci_upper[[2L]])
  expect_true("researcher_requested_release" %in% long$decision_log$metric)
  expect_true("partial_invariance" %in% nomo_methods_used(long))
})


test_that("the lags, the column pattern, and the identification are options", {
  skip_on_cran()
  renamed <- nomo_demo_longitudinal
  names(renamed) <- sub("^(w[0-9])_(t[0-9])$", "\\2.\\1", names(renamed))
  long <- nomo_invariance_longitudinal(
    long_model, renamed, occasions = long_occasions, columns = "{occasion}.{item}",
    auto = 1, levels = c("configural", "metric")
  )
  expect_identical(long$long_items$w1, c("t1.w1", "t2.w1", "t3.w1"))
  # Adjacent occasions only: the t1-t3 covariances are gone.
  expect_identical(long$fit_evidence$df[[1L]], 43)
  expect_match(long$decision_log$observation[[2L]], "up to 1 apart")
  expect_identical(nrow(long$latent_means), 0L)
  expect_false("latent_change" %in% long$decision_log$metric)

  ul <- nomo_invariance_longitudinal(
    long_model, nomo_demo_longitudinal, occasions = long_occasions,
    levels = c("configural", "metric", "scalar"), ID.fac = "UL", localize = FALSE
  )
  expect_identical(ul$completed_levels, c("configural", "metric", "scalar"))
  expect_identical(nrow(ul$latent_means), 0L)
})


test_that("ordered items are expanded to every occasion", {
  skip_on_cran()
  cut_items <- as.data.frame(lapply(nomo_demo_longitudinal, function(x) {
    as.integer(cut(x, c(-Inf, 3, 3.75, 4.5, 5.25, Inf)))
  }))
  long <- nomo_invariance_longitudinal(
    long_model, cut_items, occasions = long_occasions,
    ordered = c("w1", "w2", "w3", "w4"), levels = c("configural", "thresholds")
  )
  expect_identical(length(long$ordered), 12L)
  expect_identical(long$indicator_type, "ordered_polytomous")
  expect_identical(long$estimator, "WLSMV")
  expect_identical(long$ID.cat, "Wu.Estabrook.2016")
  expect_true(all(long$fit_evidence$converged))
  expect_true(all(c("categorical_invariance", "longitudinal_invariance") %in%
                    nomo_methods_used(long)))
})


test_that("the results print and summarize within 80 columns", {
  skip_on_cran()
  long <- long_default()
  local_reproducible_output(width = 80)
  printed <- capture.output(print(long))
  expect_true(all(nchar(printed) <= 80))
  expect_match(printed[[1L]], "<nomo_invariance_longitudinal> Measurement invariance across occasions")
  expect_true(any(grepl("Occasions: t1, t2, t3", printed)))
  summarized <- capture.output(print(summary(long)))
  expect_true(all(nchar(summarized) <= 80))
  expect_match(summarized[[1L]], "<nomo_invariance_longitudinal summary>")
  expect_true(any(grepl("Latent change from t1", summarized)))
  expect_true(any(grepl("Intercept: w3 (t1 vs. t3)", summarized, fixed = TRUE)))
  # With a strict level, the fit table still has room for RMSEA and SRMR, and
  # the constraints each level adds are named beneath it (#89).
  expect_true(any(grepl("^  Level +Chi-square +df +p +CFI +RMSEA +SRMR$", summarized)))
  expect_false(any(grepl("Not shown for width", summarized, fixed = TRUE)))
  expect_true(any(grepl("Held equal: loadings from metric; intercepts from scalar;",
                        summarized, fixed = TRUE)))
  expect_true(any(grepl("^  Level +Occasion +Factor +Change ", summarized)))
})


test_that("an occasion label falls back when a name is not in the map", {
  pt <- data.frame(lhs = "F", op = "~~", rhs = "F", group = 1L, stringsAsFactors = FALSE)
  map <- tibble::tibble(name = "w1_t1", generic = "w1", occasion = "t1")
  label <- nomo_invariance_parameter_label(pt, 1L, character(), map)
  expect_identical(label, list(base = "Residual variance: F", group = ""))
})


test_that("arguments and models are checked", {
  d <- nomo_demo_longitudinal
  expect_error(nomo_invariance_longitudinal(1, d, long_occasions), "one non-empty")
  expect_error(nomo_invariance_longitudinal(long_model, d[0, ], long_occasions), "non-empty data frame")
  expect_error(nomo_invariance_longitudinal(long_model, d, "t1"), "two or more distinct")
  expect_error(nomo_invariance_longitudinal(long_model, d, c("t1", "t1")), "two or more distinct")
  expect_error(nomo_invariance_longitudinal(long_model, d, c("t1", NA)), "two or more distinct")
  expect_error(nomo_invariance_longitudinal(long_model, d, list("t1", "t2")), "two or more distinct")
  expect_error(
    nomo_invariance_longitudinal(long_model, d, long_occasions, columns = "{item}"),
    "\\{occasion\\}"
  )
  expect_error(nomo_invariance_longitudinal(long_model, d, long_occasions, auto = 0), "`auto`")
  expect_error(nomo_invariance_longitudinal(long_model, d, long_occasions, auto = 1.5), "`auto`")
  expect_error(nomo_invariance_longitudinal(long_model, d, long_occasions, auto = "some"), "`auto`")
  expect_error(
    nomo_invariance_longitudinal("F =~ w1 + w2\nG =~ w3 + w4\nF ~ G", d, long_occasions),
    "for one occasion"
  )
  expect_error(
    nomo_invariance_longitudinal("F =~ w1 + w2\nw1 ~~ w2", d, long_occasions),
    "for one occasion"
  )
  expect_error(
    nomo_invariance_longitudinal("F =~ 1*w1 + w2 + w3", d, long_occasions),
    "cannot fix or label"
  )
  expect_error(
    nomo_invariance_longitudinal(long_model, d, long_occasions, ordered = 1),
    "`ordered` must be NULL"
  )
  expect_error(
    nomo_invariance_longitudinal(long_model, d, long_occasions, ordered = "w9"),
    "not in `model`: w9"
  )
  expect_error(
    nomo_invariance_longitudinal(
      "F =~ w1 + w11", d, c("1", "11"), columns = "{item}{occasion}"
    ),
    "its own name"
  )
  expect_error(
    nomo_invariance_longitudinal("w1 =~ w2 + w3 + w4", d, long_occasions),
    "Rename the factors"
  )
  expect_error(
    nomo_invariance_longitudinal(long_model, d, c("t1", "t2", "t4")),
    "w1_t4"
  )
})
