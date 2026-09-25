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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  marker <- nomo_cfa(compare_syntax$full, nomo_demo_continuous)
  std_lv <- nomo_cfa(compare_syntax$full, nomo_demo_continuous, std.lv = TRUE)
  out <- nomo_compare(marker = marker, std_lv = std_lv, rationale = "Identification choice.", evidence = FALSE)

  expect_identical(out$comparisons$relation, "equivalent")
  expect_false(out$comparisons$test_available)
  expect_match(out$comparisons$test_note, "equivalent")
  expect_equal(out$comparisons$delta_aic, 0, tolerance = 1e-6)
})


test_that("comparisons refuse incompatible inputs with explanations", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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


test_that("unconverged, disjoint, and inconsistently typed models are refused", {
  full <- nomo_cfa(compare_syntax$full, nomo_demo_continuous)

  unconverged <- suppressWarnings(nomo_cfa(
    compare_syntax$zero_b5,
    nomo_demo_continuous,
    control = list(iter.max = 1L)
  ))
  skip_if(isTRUE(unconverged$converged))
  expect_error(
    nomo_compare(full = full, unconverged = unconverged, rationale = "x"),
    "must converge before they are compared. Not converged: unconverged"
  )
})


test_that("models sharing no observed variables are refused", {
  # Complete data, so both models use the same cases and the variable check is
  # the one that applies. (With missing data, listwise deletion would already
  # give the two models different cases.)
  agency <- nomo_cfa("Agency =~ ag1 + ag2 + ag3 + ag4", nomo_demo_network)
  persistence <- nomo_cfa("Persistence =~ pe1 + pe2 + pe3 + pe4", nomo_demo_network)
  expect_error(
    nomo_compare(agency = agency, persistence = persistence, rationale = "x"),
    "Models `agency` and `persistence` share no observed variables."
  )
})


test_that("models treating shared indicators differently are refused", {
  skip_on_cran()
  numeric_items <- as.data.frame(lapply(nomo_demo_ordinal, as.numeric))
  a_items <- paste0("a", 1:5)
  all_items <- c(a_items, paste0("b", 1:5))

  all_ordered <- nomo_cfa(compare_syntax$full, numeric_items, ordered = all_items)
  a_ordered <- nomo_cfa(
    compare_syntax$zero_b5, numeric_items,
    ordered = a_items, estimator = "WLSMV"
  )
  expect_error(
    nomo_compare(all_ordered = all_ordered, a_ordered = a_ordered, rationale = "x"),
    "treat shared indicators differently"
  )
})


test_that("the difference-test method is validated and can be chosen", {
  skip_on_cran()
  full <- nomo_cfa(compare_syntax$full, nomo_demo_continuous, estimator = "MLR")
  zero <- nomo_cfa(compare_syntax$zero_b5, nomo_demo_continuous, estimator = "MLR")

  expect_error(
    nomo_compare(full = full, zero = zero, rationale = "x", method = ""),
    "one non-empty character value"
  )

  cmp <- nomo_compare(
    full = full, zero = zero,
    rationale = "Request the 2010 scaled difference test.",
    method = "satorra.bentler.2010",
    evidence = FALSE
  )
  expect_identical(cmp$comparisons$method, "satorra.bentler.2010")
  expect_true("lrt_scaled" %in% nomo_methods(cmp)$id)
})


test_that("presentation handles comparisons without tests or plottable values", {
  full <- nomo_cfa(compare_syntax$full, nomo_demo_continuous)
  one <- nomo_cfa(compare_syntax$one, nomo_demo_continuous)
  cmp <- nomo_compare(
    full = full, one = one,
    rationale = "Compare against a single general factor.",
    nested = "no",
    evidence = FALSE
  )
  expect_output(print(cmp), "no difference test")

  no_fit <- cmp
  no_fit$comparisons$delta_cfi <- NA_real_
  expect_output(print(no_fit), "change in fit unavailable")

  no_fit$models[, c("cfi", "tli", "rmsea", "srmr")] <- NA_real_
  expect_error(plot(no_fit, type = "fit"), "No finite fit indices")

  no_loadings <- cmp
  no_loadings$loadings <- no_loadings$loadings[0, ]
  expect_error(plot(no_loadings, type = "loadings"), "No standardized loadings")

  na_loadings <- cmp
  for (nm in setdiff(names(na_loadings$loadings), c("factor", "item"))) {
    na_loadings$loadings[[nm]] <- NA_real_
  }
  expect_error(plot(na_loadings, type = "loadings"), "No finite standardized loadings")
})


# Failure guards (#54) ---------------------------------------------------------
#
# Every branch below guards a failure inside lavaan or semTools rather than a
# researcher-facing input, so each is reached by mocking the one call that
# fails and passing everything else through to the real function. Several of
# them produce the sentence a researcher reads when a check cannot be run, and
# wording that has never executed can be wrong without anyone noticing.

compare_fitted_pair <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) return(cache)

    cache <<- list(
      full = nomo_cfa(compare_syntax$full, nomo_demo_continuous),
      zero = nomo_cfa(compare_syntax$zero_b5, nomo_demo_continuous)
    )

    cache
  }
})


test_that("difference-test helpers report unusable values rather than guessing", {
  expect_identical(nomologR:::nomo_compare_format_p(NA_real_), "p unavailable")
  expect_identical(nomologR:::nomo_compare_format_p(Inf), "p unavailable")
  expect_identical(nomologR:::nomo_compare_format_p(0.0004), "p < .001")
  expect_identical(nomologR:::nomo_compare_format_p(0.0321), "p = .032")

  # A lavaan difference-test table without the expected column.
  row <- data.frame(`Chisq diff` = 3.2, check.names = FALSE)
  expect_equal(nomologR:::nomo_compare_lrt_value(row, "Chisq diff"), 3.2)
  expect_identical(nomologR:::nomo_compare_lrt_value(row, "Df diff"), NA_real_)

  # A column that is present but not usable.
  na_row <- data.frame(`Df diff` = NA_real_, check.names = FALSE)
  expect_identical(nomologR:::nomo_compare_lrt_value(na_row, "Df diff"), NA_real_)
})


test_that("nomo_compare refuses when lavaan cannot report the cases or data used", {
  pair <- compare_fitted_pair()
  real_lav_inspect <- lavaan::lavInspect

  expect_error(
    testthat::with_mocked_bindings(
      nomo_compare(
        full = pair$full,
        zero_b5 = pair$zero,
        rationale = "Cases cannot be confirmed."
      ),
      lavInspect = function(object, what, ...) {
        if (identical(what, "case.idx")) stop("mocked case failure")
        real_lav_inspect(object, what, ...)
      },
      .package = "lavaan"
    ),
    "Could not confirm which cases"
  )

  expect_error(
    testthat::with_mocked_bindings(
      nomo_compare(
        full = pair$full,
        zero_b5 = pair$zero,
        rationale = "Data cannot be retrieved."
      ),
      lavInspect = function(object, what, ...) {
        if (identical(what, "data")) stop("mocked data failure")
        real_lav_inspect(object, what, ...)
      },
      .package = "lavaan"
    ),
    "Could not retrieve the data"
  )
})


test_that("model data is recovered when lavaan returns it per group", {
  fit <- compare_fitted_pair()$full$fit
  real_lav_inspect <- lavaan::lavInspect

  out <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_data(fit),
    lavInspect = function(object, what, ...) {
      if (identical(what, "data")) {
        return(list(real_lav_inspect(object, "data")))
      }
      real_lav_inspect(object, what, ...)
    },
    .package = "lavaan"
  )

  # Unwrapping the per-group list yields the data lavaan actually used, which
  # is not the whole dataset: these models drop incomplete cases.
  expect_s3_class(out, "data.frame")
  expect_identical(out, as.data.frame(real_lav_inspect(fit, "data")))
  expect_lt(nrow(out), nrow(nomo_demo_continuous))

  # Nothing recoverable at all is reported as nothing, not as an empty frame.
  expect_null(testthat::with_mocked_bindings(
    nomologR:::nomo_compare_data(fit),
    lavInspect = function(object, what, ...) {
      if (identical(what, "data")) return(NULL)
      real_lav_inspect(object, what, ...)
    },
    .package = "lavaan"
  ))
})


test_that("fixed-zero loadings are empty rather than an error when parTable fails", {
  fit <- compare_fitted_pair()$full$fit

  out <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_fixed_zero_loadings(fit),
    parTable = function(...) stop("mocked parameter-table failure"),
    .package = "lavaan"
  )

  expect_s3_class(out, "tbl_df")
  expect_identical(nrow(out), 0L)
  expect_identical(names(out), c("factor", "item"))

  # A table that returns rows but not the columns this needs.
  out_short <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_fixed_zero_loadings(fit),
    parTable = function(...) data.frame(lhs = "A", rhs = "a1"),
    .package = "lavaan"
  )

  expect_identical(nrow(out_short), 0L)
})


test_that("the nesting check explains itself when semTools cannot answer", {
  pair <- compare_fitted_pair()
  reference <- pair$full$fit
  other <- pair$zero$fit
  real_net <- semTools::net(reference, other)

  nesting <- function(declared, net_value) {
    testthat::with_mocked_bindings(
      nomologR:::nomo_compare_nesting(
        reference_fit = reference,
        other_fit = other,
        same_variables = TRUE,
        df_reference = 34,
        df_other = 35,
        declared = declared
      ),
      net = function(...) if (inherits(net_value, "condition")) stop(net_value) else net_value,
      .package = "semTools"
    )
  }

  # semTools errors: the researcher is told what failed, and what the two
  # declarations mean for the difference test.
  failure <- simpleError("mocked nesting failure")

  declared_yes <- nesting("yes", failure)
  expect_identical(declared_yes$check, "unavailable")
  expect_true(declared_yes$nested)
  expect_match(declared_yes$note, "Nesting could not be checked automatically")
  expect_match(declared_yes$note, "mocked nesting failure", fixed = TRUE)
  expect_match(declared_yes$note, "treated as nested because the researcher declared them nested")

  declared_auto <- nesting("auto", failure)
  expect_identical(declared_auto$check, "unavailable")
  expect_false(declared_auto$nested)
  expect_match(declared_auto$note, "Nesting could not be checked automatically")
  expect_match(declared_auto$note, 'set `nested = "yes"`', fixed = TRUE)

  # A result whose shape is not the one this code reads.
  unexpected <- nesting("auto", methods::new("Net", test = matrix(TRUE), df = 1))
  expect_identical(unexpected$check, "unavailable")
  expect_match(unexpected$note, "returned an unexpected result", fixed = TRUE)

  # A model that did not converge on the other's implied moments.
  na_net <- real_net
  na_test <- methods::slot(na_net, "test")
  na_test[2L, 1L] <- NA
  methods::slot(na_net, "test") <- na_test

  na_result <- nesting("auto", na_net)
  expect_identical(na_result$check, "unavailable")
  expect_match(na_result$note, "did not converge when fitted to the other model's implied moments", fixed = TRUE)

  # Equal degrees of freedom: the models are equivalent and the data cannot
  # distinguish them.
  equivalent_net <- real_net
  methods::slot(equivalent_net, "df") <- c(34, 34)

  equivalent <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_nesting(
      reference_fit = reference,
      other_fit = other,
      same_variables = TRUE,
      df_reference = 34,
      df_other = 34,
      declared = "auto"
    ),
    net = function(...) equivalent_net,
    .package = "semTools"
  )

  expect_identical(equivalent$check, "equivalent")
  expect_identical(equivalent$relation, "equivalent")
  expect_match(equivalent$note, "the data cannot distinguish", fixed = TRUE)
})


test_that("warnings raised by the nesting check are carried, not swallowed", {
  pair <- compare_fitted_pair()
  reference <- pair$full$fit
  other <- pair$zero$fit
  real_net <- semTools::net(reference, other)

  out <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_nesting(
      reference_fit = reference,
      other_fit = other,
      same_variables = TRUE,
      df_reference = 34,
      df_other = 35,
      declared = "auto"
    ),
    net = function(...) {
      warning("mocked nesting warning")
      real_net
    },
    .package = "semTools"
  )

  expect_identical(out$check, "nested")
  expect_true("mocked nesting warning" %in% out$warnings)
})


test_that("a difference test that lavaan cannot compute is reported, not assumed", {
  pair <- compare_fitted_pair()

  failed <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_difference_test(
      pair$full$fit, pair$zero$fit,
      method = "default", estimator = "ML"
    ),
    lavTestLRT = function(...) stop("mocked difference-test failure"),
    .package = "lavaan"
  )

  expect_false(failed$available)
  expect_identical(failed$statistic, NA_real_)
  expect_match(failed$note, "lavaan could not compute the difference test", fixed = TRUE)
  expect_match(failed$note, "mocked difference-test failure", fixed = TRUE)
})


test_that("a negative scaled difference statistic is named as the reason", {
  pair <- compare_fitted_pair()
  real_lrt <- lavaan::lavTestLRT

  negative <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_difference_test(
      pair$full$fit, pair$zero$fit,
      method = "default", estimator = "MLR"
    ),
    lavTestLRT = function(...) {
      tab <- real_lrt(...)
      tab[nrow(tab), "Chisq diff"] <- -1.5
      tab
    },
    .package = "lavaan"
  )

  expect_false(negative$available)
  expect_identical(negative$statistic, NA_real_)
  expect_match(negative$note, "the scaled difference statistic is negative", fixed = TRUE)

  # A robust estimator with the default method gets the Satorra-Bentler (2010)
  # pointer; the same failure under ML does not, because it does not apply.
  expect_match(negative$note, 'method = \\"satorra.bentler.2010\\"')

  under_ml <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_difference_test(
      pair$full$fit, pair$zero$fit,
      method = "default", estimator = "ML"
    ),
    lavTestLRT = function(...) {
      tab <- real_lrt(...)
      tab[nrow(tab), "Chisq diff"] <- -1.5
      tab
    },
    .package = "lavaan"
  )

  expect_false(under_ml$available)
  expect_no_match(under_ml$note, "satorra.bentler.2010", fixed = TRUE)
})


test_that("warnings from lavaan are carried into the difference-test note", {
  pair <- compare_fitted_pair()
  real_lrt <- lavaan::lavTestLRT

  warned <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_difference_test(
      pair$full$fit, pair$zero$fit,
      method = "default", estimator = "ML"
    ),
    lavTestLRT = function(...) {
      warning("mocked lavaan warning")
      tab <- real_lrt(...)
      tab[nrow(tab), "Chisq diff"] <- NA_real_
      tab
    },
    .package = "lavaan"
  )

  expect_false(warned$available)
  expect_match(warned$note, "lavaan did not return a finite difference statistic", fixed = TRUE)
  expect_match(warned$note, "lavaan reported: mocked lavaan warning", fixed = TRUE)
})


test_that("side-by-side evidence reports a failed component instead of dropping it", {
  pair <- compare_fitted_pair()

  out <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_evidence("full", pair$full, nomo_defaults()),
    nomo_reliability = function(...) stop("mocked reliability failure"),
    nomo_validity = function(...) stop("mocked validity failure")
  )

  expect_s3_class(out, "tbl_df")
  expect_setequal(out$metric, c("reliability", "validity"))
  expect_true(all(is.na(out$estimate)))
  expect_match(out$note[out$metric == "reliability"], "Unavailable: mocked reliability failure", fixed = TRUE)
  expect_match(out$note[out$metric == "validity"], "Unavailable: mocked validity failure", fixed = TRUE)
})


test_that("evidence and loadings tables keep their schema when there is nothing to show", {
  pair <- compare_fitted_pair()

  # A model that yields neither reliability nor validity rows.
  empty_evidence <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_evidence("full", pair$full, nomo_defaults()),
    nomo_reliability = function(...) list(evidence = tibble::tibble()),
    nomo_validity = function(...) list(ave = tibble::tibble(), htmt2 = tibble::tibble())
  )

  expect_identical(nrow(empty_evidence), 0L)
  expect_identical(
    names(empty_evidence),
    c("model", "construct", "metric", "estimate", "note")
  )

  # Models that report no standardized loadings at all.
  stripped <- lapply(list(full = pair$full, zero_b5 = pair$zero), function(m) {
    m$standardized_loadings <- m$standardized_loadings[0, ]
    m
  })

  expect_identical(nrow(nomologR:::nomo_compare_loadings(stripped)), 0L)
})


test_that("the interpretation omits sections that have nothing to report", {
  row <- tibble::tibble(
    model = "zero_b5",
    reference = "full",
    relation = "non_nested",
    df_difference = 1,
    test_available = FALSE,
    test = NA_character_,
    chisq_diff = NA_real_,
    df_diff = NA_real_,
    p_value = NA_real_,
    test_note = "",
    delta_cfi = NA_real_,
    delta_tli = NA_real_,
    delta_rmsea = NA_real_,
    delta_srmr = NA_real_,
    ic_available = FALSE,
    delta_aic = NA_real_,
    delta_bic = NA_real_,
    ic_note = ""
  )

  out <- nomologR:::nomo_compare_interpretation(row)

  # The relation is still stated; the empty sections simply do not appear.
  expect_match(out, "are not nested", fixed = TRUE)
  expect_no_match(out, "chi-square difference", fixed = TRUE)
  expect_no_match(out, "Change in fit", fixed = TRUE)
  expect_no_match(out, "AIC", fixed = TRUE)
})


test_that("equal degrees of freedom read as equivalent even when the check could not run", {
  pair <- compare_fitted_pair()

  # Declared nested, the automatic check unavailable, and the two models have
  # the same degrees of freedom: neither is more constrained than the other.
  out <- testthat::with_mocked_bindings(
    nomologR:::nomo_compare_nesting(
      reference_fit = pair$full$fit,
      other_fit = pair$zero$fit,
      same_variables = TRUE,
      df_reference = 34,
      df_other = 34,
      declared = "yes"
    ),
    net = function(...) stop("mocked nesting failure"),
    .package = "semTools"
  )

  expect_identical(out$check, "unavailable")
  expect_true(out$nested)
  expect_identical(out$relation, "equivalent")
})


test_that("warnings are collected once while evidence is computed quietly", {
  out <- nomologR:::nomo_compare_quietly({
    warning("mocked evidence warning")
    warning("mocked evidence warning")
    warning("a second warning")
    42L
  })

  expect_identical(out$value, 42L)
  expect_identical(out$warnings, c("mocked evidence warning", "a second warning"))

  # An error is returned rather than thrown, so the caller can report it.
  failed <- nomologR:::nomo_compare_quietly(stop("mocked evidence failure"))
  expect_s3_class(failed$value, "error")
  expect_identical(conditionMessage(failed$value), "mocked evidence failure")
})
