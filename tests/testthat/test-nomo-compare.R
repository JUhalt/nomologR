# Measurement-model comparison -------------------------------------------------

compare_syntax <- list(
  full = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
  zero_b5 = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + 0*b5",
  drop_b5 = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4",
  cross_a5 = "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5 + a5",
  a5_on_b = "A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4 + b5 + a5",
  one = "F =~ a1 + a2 + a3 + a4 + a5 + b1 + b2 + b3 + b4 + b5"
)

lrt_row <- function(...) {
  tab <- suppressWarnings(as.data.frame(lavaan::lavTestLRT(...)))
  tab[nrow(tab), , drop = FALSE]
}


test_that("nested ML comparison reproduces lavaan tests, fit changes, and information criteria", {
  full <- nomo_cfa(compare_syntax$full, nomo_demo_continuous)
  zero <- nomo_cfa(compare_syntax$zero_b5, nomo_demo_continuous)

  out <- nomo_compare(full = full, zero_b5 = zero, rationale = "Does b5 contribute to factor B?")
  expect_s3_class(out, "nomo_compare")

  cmp <- out$comparisons
  expect_identical(nrow(cmp), 1L)
  expect_identical(cmp$model, "zero_b5")
  expect_identical(cmp$reference, "full")
  expect_identical(cmp$relation, "more_constrained")
  expect_identical(cmp$nesting_check, "nested")
  expect_true(cmp$nested)
  expect_true(cmp$test_available)
  expect_identical(cmp$df_difference, 1)

  lav <- lrt_row(full$fit, zero$fit)
  expect_equal(cmp$chisq_diff, lav[["Chisq diff"]], tolerance = 1e-8)
  expect_equal(cmp$df_diff, lav[["Df diff"]])
  expect_equal(cmp$p_value, lav[["Pr(>Chisq)"]], tolerance = 1e-8)
  expect_identical(cmp$method, "standard")

  fm_full <- lavaan::fitMeasures(full$fit, c("cfi", "rmsea", "aic", "bic"))
  fm_zero <- lavaan::fitMeasures(zero$fit, c("cfi", "rmsea", "aic", "bic"))
  expect_equal(cmp$delta_cfi, unname(fm_zero[["cfi"]] - fm_full[["cfi"]]), tolerance = 1e-8)
  expect_equal(cmp$delta_rmsea, unname(fm_zero[["rmsea"]] - fm_full[["rmsea"]]), tolerance = 1e-8)
  expect_equal(cmp$delta_aic, unname(fm_zero[["aic"]] - fm_full[["aic"]]), tolerance = 1e-8)
  expect_equal(cmp$delta_bic, unname(fm_zero[["bic"]] - fm_full[["bic"]]), tolerance = 1e-8)
  expect_true(cmp$ic_available)

  expect_false(any(grepl("winner|best model|preferred model", tolower(cmp$interpretation))))
  expect_match(cmp$interpretation, "No model is selected automatically")
  expect_true("Does b5 contribute to factor B?" %in% out$decision_log$rationale)
  expect_true("no model selected automatically" %in% out$decision_log$decision)
})


test_that("zero-loading evidence is labeled and loadings are aligned side by side", {
  full <- nomo_cfa(compare_syntax$full, nomo_demo_continuous)
  zero <- nomo_cfa(compare_syntax$zero_b5, nomo_demo_continuous)
  out <- nomo_compare(full = full, zero_b5 = zero, rationale = "Item b5.", reference = "zero_b5")

  expect_identical(out$reference, "zero_b5")
  expect_identical(out$comparisons$relation, "less_constrained")
  expect_identical(out$models$fixed_zero_loadings, c(0L, 1L))

  omega_b <- out$evidence[out$evidence$model == "zero_b5" & out$evidence$construct == "B" &
                            out$evidence$metric == "omega", , drop = FALSE]
  expect_identical(nrow(omega_b), 1L)
  expect_match(omega_b$note, "fixed to zero for b5")

  expect_true(all(c("factor", "item", "full", "zero_b5") %in% names(out$loadings)))
  expect_equal(out$loadings$zero_b5[out$loadings$item == "b5"], 0)
  expect_gt(out$loadings$full[out$loadings$item == "b5"], 0)
})


test_that("models with different observed variables receive descriptive evidence only", {
  full <- nomo_cfa(compare_syntax$full, nomo_demo_continuous)
  drop <- nomo_cfa(compare_syntax$drop_b5, nomo_demo_continuous)
  out <- nomo_compare(full = full, drop_b5 = drop, rationale = "Shortened scale.")

  cmp <- out$comparisons
  expect_identical(cmp$relation, "different_variables")
  expect_identical(cmp$nesting_check, "not_applicable")
  expect_false(cmp$test_available)
  expect_false(cmp$ic_available)
  expect_true(is.na(cmp$chisq_diff))
  expect_true(is.na(cmp$delta_aic))
  expect_match(cmp$test_note, "fix its loading to zero")
  expect_match(cmp$ic_note, "different observed variables")
  expect_true(is.na(out$loadings$drop_b5[out$loadings$item == "b5"]))
  expect_true(any(out$evidence$model == "drop_b5"))
})


test_that("nesting declarations are checked, and non-nested models keep information criteria", {
  full <- nomo_cfa(compare_syntax$full, nomo_demo_continuous)
  moved <- nomo_cfa(compare_syntax$a5_on_b, nomo_demo_continuous)

  auto <- nomo_compare(full = full, moved = moved, rationale = "Where does a5 belong?", evidence = FALSE)
  expect_identical(auto$comparisons$relation, "non_nested")
  expect_identical(auto$comparisons$nesting_check, "not_nested")
  expect_false(auto$comparisons$test_available)
  expect_true(auto$comparisons$ic_available)
  expect_true(is.finite(auto$comparisons$delta_aic))

  declared <- nomo_compare(
    full = full, moved = moved, rationale = "Where does a5 belong?",
    nested = "yes", evidence = FALSE
  )
  expect_false(declared$comparisons$test_available)
  expect_match(declared$comparisons$test_note, "declared nested")
  expect_true(any(declared$decision_log$severity == "concern"))

  skipped <- nomo_compare(
    full = full, moved = moved, rationale = "Where does a5 belong?",
    nested = "no", evidence = FALSE
  )
  expect_identical(skipped$comparisons$nesting_check, "not_run")
  expect_false(skipped$comparisons$test_available)
})


test_that("equivalent parameterizations are identified without a difference test", {
  marker <- nomo_cfa(compare_syntax$full, nomo_demo_continuous)
  std_lv <- nomo_cfa(compare_syntax$full, nomo_demo_continuous, std.lv = TRUE)
  out <- nomo_compare(marker = marker, std_lv = std_lv, rationale = "Identification choice.", evidence = FALSE)

  expect_identical(out$comparisons$relation, "equivalent")
  expect_false(out$comparisons$test_available)
  expect_match(out$comparisons$test_note, "equivalent")
  expect_equal(out$comparisons$delta_aic, 0, tolerance = 1e-6)
})


test_that("comparisons refuse incompatible inputs with explanations", {
  full <- nomo_cfa(compare_syntax$full, nomo_demo_continuous)
  zero <- nomo_cfa(compare_syntax$zero_b5, nomo_demo_continuous)

  expect_error(nomo_compare(full = full, rationale = "x"), "at least two")
  expect_error(nomo_compare(full = full, other = list(), rationale = "x"), "nomo_cfa")
  expect_error(nomo_compare(full = full, zero = zero), "rationale")
  expect_error(nomo_compare(full = full, zero = zero, rationale = "   "), "rationale")
  expect_error(nomo_compare(full = full, full = zero, rationale = "x"), "unique")
  expect_error(nomo_compare(full = full, zero = zero, rationale = "x", reference = "missing"), "reference")
  expect_error(nomo_compare(full = full, zero = zero, rationale = "x", evidence = NA), "evidence")

  mlr <- nomo_cfa(compare_syntax$full, nomo_demo_continuous, estimator = "MLR")
  expect_error(nomo_compare(full = full, mlr = mlr, rationale = "x"), "same estimator")

  fiml <- nomo_cfa(compare_syntax$full, nomo_demo_continuous, missing = "fiml")
  expect_error(nomo_compare(full = full, fiml = fiml, rationale = "x"), "missing-data handling")

  subset <- nomo_cfa(compare_syntax$full, nomo_demo_continuous[1:400, ])
  expect_error(nomo_compare(full = full, subset = subset, rationale = "x"), "same cases")

  altered <- nomo_demo_continuous
  altered$a1 <- rev(altered$a1)
  other <- nomo_cfa(compare_syntax$full, altered)
  expect_error(nomo_compare(full = full, other = other, rationale = "x"), "same data")
})


test_that("unnamed models are labeled from their expressions", {
  full <- nomo_cfa(compare_syntax$full, nomo_demo_continuous)
  zero <- nomo_cfa(compare_syntax$zero_b5, nomo_demo_continuous)
  out <- nomo_compare(full, zero, rationale = "Labels.", evidence = FALSE)
  expect_identical(out$models$model, c("full", "zero"))
})


test_that("robust ML comparisons use the scaled difference test and flag post-hoc origin", {
  skip_on_cran()

  simple <- nomo_cfa(compare_syntax$full, nomo_demo_continuous, estimator = "MLR")
  cross <- nomo_cfa(compare_syntax$cross_a5, nomo_demo_continuous, estimator = "MLR")
  out <- nomo_compare(
    simple = simple, cross = cross,
    rationale = "The largest modification index suggested B =~ a5.",
    origin = "post_hoc", evidence = FALSE
  )

  cmp <- out$comparisons
  lav <- lrt_row(simple$fit, cross$fit)
  expect_identical(cmp$relation, "less_constrained")
  if (is.finite(lav[["Chisq diff"]]) && lav[["Chisq diff"]] >= 0) {
    expect_true(cmp$test_available)
    expect_equal(cmp$chisq_diff, lav[["Chisq diff"]], tolerance = 1e-8)
    expect_equal(cmp$p_value, lav[["Pr(>Chisq)"]], tolerance = 1e-8)
    expect_match(cmp$method, "satorra")
  } else {
    expect_false(cmp$test_available)
  }
  expect_true("comparison_origin" %in% out$decision_log$metric)
})


test_that("undefined scaled difference tests are reported as unavailable, matching lavaan", {
  skip_on_cran()

  two <- nomo_cfa(compare_syntax$full, nomo_demo_continuous, estimator = "MLR")
  one <- nomo_cfa(compare_syntax$one, nomo_demo_continuous, estimator = "MLR")
  out <- nomo_compare(two = two, one = one, rationale = "One or two factors?", evidence = FALSE)

  cmp <- out$comparisons
  lav <- lrt_row(two$fit, one$fit)
  if (is.finite(lav[["Chisq diff"]]) && lav[["Chisq diff"]] >= 0) {
    expect_true(cmp$test_available)
    expect_equal(cmp$chisq_diff, lav[["Chisq diff"]], tolerance = 1e-8)
  } else {
    expect_false(cmp$test_available)
    expect_true(is.na(cmp$p_value))
    expect_match(cmp$test_note, "unavailable")
    expect_match(cmp$test_note, "satorra.bentler.2010", fixed = TRUE)
  }
})


test_that("WLSMV comparisons use lavaan's categorical difference test without information criteria", {
  skip_on_cran()

  ordered_items <- names(nomo_demo_ordinal)
  full <- nomo_cfa(compare_syntax$full, nomo_demo_ordinal, ordered = ordered_items)
  zero <- nomo_cfa(compare_syntax$zero_b5, nomo_demo_ordinal, ordered = ordered_items)

  out <- nomo_compare(full = full, zero_b5 = zero, rationale = "Item b5.", nested = "yes", evidence = FALSE)
  cmp <- out$comparisons
  lav <- lrt_row(full$fit, zero$fit)

  expect_true(cmp$nesting_check %in% c("nested", "unavailable"))
  expect_true(cmp$test_available)
  expect_equal(cmp$chisq_diff, lav[["Chisq diff"]], tolerance = 1e-8)
  expect_equal(cmp$p_value, lav[["Pr(>Chisq)"]], tolerance = 1e-8)
  expect_false(cmp$ic_available)
  expect_true(is.na(cmp$delta_aic))
  expect_match(cmp$ic_note, "not defined")
})


test_that("comparison presentation methods and tables work", {
  full <- nomo_cfa(compare_syntax$full, nomo_demo_continuous)
  zero <- nomo_cfa(compare_syntax$zero_b5, nomo_demo_continuous)
  out <- nomo_compare(full = full, zero_b5 = zero, rationale = "Item b5.")

  expect_output(print(out), "No model was selected automatically")
  expect_output(print(summary(out)), "Measurement evidence by model")
  expect_s3_class(plot(out, type = "fit"), "ggplot")
  expect_s3_class(plot(out, type = "loadings"), "ggplot")

  for (type in c("comparisons", "models", "loadings", "evidence", "decision_log")) {
    expect_s3_class(nomo_table(out, type), "data.frame")
  }
  expect_identical(nomo_table(out), out$comparisons)
})
