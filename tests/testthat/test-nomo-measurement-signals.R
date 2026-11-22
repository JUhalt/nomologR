# Signals the measurement-model stages raise about the data and the solution
# (1.0.1 audit): improper solutions beyond negative variances, indicators with
# few categories fitted as continuous, loadings that run against their
# factor, and flags that name their index or coefficient.


# Fixtures --------------------------------------------------------------------

three_factors <- "F1 =~ x1 + x2 + x3\nF2 =~ x4 + x5 + x6\nF3 =~ x7 + x8 + x9"

# Three factors whose correlations, .95, .95, and .20, no real factors could
# have, measured by items whose covariance matrix is still positive definite.
# With exact data the CFA recovers those correlations.
nonpd_latent_data <- function(n = 300) {
  lambda <- matrix(0, 9, 3, dimnames = list(paste0("x", 1:9), c("F1", "F2", "F3")))
  lambda[1:3, 1] <- 0.7
  lambda[4:6, 2] <- 0.7
  lambda[7:9, 3] <- 0.7
  phi <- matrix(c(1, .95, .95, .95, 1, .2, .95, .2, 1), 3)
  sigma <- lambda %*% phi %*% t(lambda)
  diag(sigma) <- 1
  nomo_test_simulate(sigma, n = n, seed = 1, exact = TRUE)
}

big_five <- list(
  A = paste0("A", 1:5), C = paste0("C", 1:5), E = paste0("E", 1:5),
  N = paste0("N", 1:5), O = paste0("O", 1:5)
)

# psych::bfi as answered, and with its reverse-keyed items recoded, as it is
# meant to be used.
bfi_raw <- function() psych::bfi
bfi_recoded <- function() {
  out <- psych::bfi
  reversed <- c("A1", "C4", "C5", "E1", "E2", "O2", "O5")
  out[reversed] <- 7L - out[reversed]
  out
}

walkthrough_scales <- list(EF = paste0("EF", c(1:4, 6)), TF = paste0("TF", c(1:4, 6)))
walkthrough_recoded <- function() {
  out <- nomo_demo_walkthrough
  out[c("EF2", "TF2")] <- 6L - out[c("EF2", "TF2")]
  out
}

# The audit's Big Five items collapsed to three categories (usefulness-04).
three_category_bfi <- function() {
  items <- unlist(big_five[c("N", "C")])
  as.data.frame(lapply(bfi_recoded()[1:500, items], function(v) as.integer(cut(v, c(0, 2, 4, 6)))))
}

new_metrics <- c(
  "few_categories", "cases_below_parameters", "loading_signs",
  "latent_covariance_not_positive_definite", "residual_covariance_not_positive_definite"
)


# Fit flags name their index (usefulness-15) -------------------------------------

test_that("each fit index's explanation names the index, its value, and its review reference", {
  measures <- c(
    chisq = 100, df = 50, pvalue = .001, cfi = .90, tli = .97,
    rmsea = .09, rmsea.ci.lower = .08, rmsea.ci.upper = .10, srmr = .04
  )
  tab <- nomo_cfa_fit_evidence(measures, nomo_defaults())
  explanation <- stats::setNames(tab$explanation, tab$metric)
  expect_identical(
    explanation[["CFI"]],
    "CFI .900 is below the review reference .950; inspect global and localized model strain."
  )
  expect_identical(
    explanation[["TLI"]],
    "TLI 0.970 is at or above the review reference 0.950; this does not establish model correctness."
  )
  expect_identical(
    explanation[["RMSEA"]],
    "RMSEA 0.090 is above the review reference 0.060; inspect global and localized model strain."
  )
  expect_identical(
    explanation[["SRMR"]],
    "SRMR 0.040 is at or below the review reference 0.080; this does not establish model correctness."
  )
  # A value that would print as its reference shows the decimals that differ.
  close <- nomo_cfa_fit_evidence(replace(measures, "cfi", .9496), nomo_defaults())
  expect_match(close$explanation[close$metric == "CFI"], "^CFI \\.9496 is below the review reference \\.950;")
  unavailable <- nomo_cfa_fit_evidence(measures[names(measures) != "srmr"], nomo_defaults())
  expect_identical(unavailable$explanation[unavailable$metric == "SRMR"], "SRMR is unavailable.")
})


test_that("a fitted CFA's log rows name each flagged index, as a guided run lists them", {
  skip_on_cran()
  out <- nomo_cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9",
                  lavaan::HolzingerSwineford1939, modification_indices = FALSE)
  log <- out$decision_log
  expect_match(log$observation[log$metric == "fit_cfi"],
               "^CFI \\.9[0-9]{2} is below the review reference \\.950; inspect global")
  expect_match(log$observation[log$metric == "fit_rmsea"],
               "^RMSEA 0\\.0[0-9]{2} is above the review reference 0\\.060; inspect global")
  expect_false(any(grepl("teaching reference", log$observation, fixed = TRUE)))
})


# Improper solutions beyond a negative variance (usefulness-03) -------------------

test_that("a latent covariance matrix that is not positive definite is an improper solution", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  out <- nomo_cfa(three_factors, nonpd_latent_data(), modification_indices = FALSE)
  # Every correlation is within 1 and every variance is positive.
  expect_true(all(abs(out$factor_correlations$correlation) < 1))
  expect_false(any(c("negative_latent_variance", "latent_correlation_beyond_one") %in%
                     out$heywood$issue))
  row <- out$heywood[out$heywood$issue == "latent_covariance_not_positive_definite", ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$object, "Latent covariances")
  expect_identical(row$severity, "concern")
  expect_equal(row$value, min(eigen(lavaan::lavInspect(out$fit, "cor.lv"))$values),
               tolerance = 1e-6)
  expect_lt(row$value, 0)
  expect_match(row$explanation, paste(
    "The latent covariance matrix of F1, F2, and F3 is not positive definite",
    "(smallest eigenvalue -0.25), so no real factors could have these correlations:"
  ), fixed = TRUE)
  expect_true(out$heywood_detected)
  expect_identical(
    out$decision_log$severity[out$decision_log$metric == "latent_covariance_not_positive_definite"],
    "concern"
  )
  # The test is lavaan's own: it warns of the same matrix.
  expect_true(any(grepl("covariance matrix of latent variables is not positive definite",
                        out$engine_warnings, fixed = TRUE)))

  printed <- capture.output(print(out))
  expect_true("Loadings: 9 | Flags: 1 concern, 1 review | Improper-solution signals: 1" %in% printed)
  summarized <- capture.output(print(summary(out)))
  expect_false(any(grepl("No improper-solution signal", summarized, fixed = TRUE)))
  expect_true("  Latent covariances  Negative eigenvalue  -0.25" %in% summarized)
  expect_match(paste(summarized, collapse = " "),
               "Latent covariances (Concern): The latent covariance matrix of F1, F2,",
               fixed = TRUE)
})


test_that("a residual covariance matrix that is not positive definite is an improper solution", {
  skip_on_cran()
  # x1 and x4 load on different factors and share a residual covariance of .60
  # where each residual variance is .51, so their residuals would correlate
  # 1.18, while the items' covariance matrix stays positive definite.
  lambda <- matrix(0, 6, 2, dimnames = list(paste0("x", 1:6), c("F1", "F2")))
  lambda[1:3, 1] <- 0.7
  lambda[4:6, 2] <- 0.7
  sigma <- lambda %*% matrix(c(1, .3, .3, 1), 2) %*% t(lambda)
  diag(sigma) <- 1
  sigma["x1", "x4"] <- sigma["x4", "x1"] <- sigma["x1", "x4"] + 0.6
  data <- nomo_test_simulate(sigma, n = 300, seed = 2, exact = TRUE)
  out <- nomo_cfa("F1 =~ x1 + x2 + x3\nF2 =~ x4 + x5 + x6\nx1 ~~ x4", data,
                  modification_indices = FALSE)
  row <- out$heywood[out$heywood$issue == "residual_covariance_not_positive_definite", ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$object, "Residual covariances")
  expect_equal(row$value, min(eigen(stats::cov2cor(lavaan::lavInspect(out$fit, "theta")))$values),
               tolerance = 1e-6)
  expect_match(row$explanation, paste(
    "^The residual covariance matrix \\(theta\\) is not positive definite \\(smallest",
    "eigenvalue -0\\.[0-9]+\\), so no real residuals could have these correlations"
  ))
  expect_false("latent_covariance_not_positive_definite" %in% out$heywood$issue)
  expect_identical(nomo_cfa_issue_label(row$issue), "Negative eigenvalue")
})


test_that("a signal already reported is not reported again as a matrix", {
  skip_on_cran()
  two <- "F1 =~ x1 + x2 + x3\nF2 =~ x4 + x5 + x6"
  # A loading of 1.05 leaves x1 a negative residual variance by construction.
  improper <- nomo_test_simulate(
    "F1 =~ 1.05*x1 + 0.7*x2 + 0.6*x3\nF2 =~ 0.7*x4 + 0.6*x5 + 0.6*x6\nF1 ~~ 0.4*F2",
    n = 200, seed = 5, exact = TRUE
  )
  out <- nomo_cfa(two, improper, modification_indices = FALSE)
  expect_true("negative_observed_residual_variance" %in% out$heywood$issue)
  expect_false("residual_covariance_not_positive_definite" %in% out$heywood$issue)
  # A latent correlation of 1.10 already shows that the latent matrix is
  # improper.
  beyond <- nomo_test_simulate(
    "F1 =~ 0.7*x1 + 0.7*x2 + 0.7*x3\nF2 =~ 0.7*x4 + 0.7*x5 + 0.7*x6\nF1 ~~ 1.1*F2",
    n = 200, seed = 6, exact = TRUE
  )
  out <- nomo_cfa(two, beyond, modification_indices = FALSE)
  expect_true("latent_correlation_beyond_one" %in% out$heywood$issue)
  expect_false("latent_covariance_not_positive_definite" %in% out$heywood$issue)
})


test_that("a model fitted to fewer cases than it has free parameters is flagged for review", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  population <- paste(
    "F1 =~ 0.7*x1 + 0.7*x2 + 0.7*x3", "F2 =~ 0.7*x4 + 0.7*x5 + 0.7*x6",
    "F3 =~ 0.7*x7 + 0.7*x8 + 0.7*x9", "F1 ~~ 0.3*F2", "F1 ~~ 0.3*F3", "F2 ~~ 0.3*F3",
    sep = "\n"
  )
  small <- nomo_cfa(three_factors, nomo_test_simulate(population, n = 20, seed = 3, exact = TRUE),
                    modification_indices = FALSE)
  row <- small$decision_log[small$decision_log$metric == "cases_below_parameters", ]
  expect_identical(row$severity, "review")
  expect_identical(row$value, 21)
  expect_identical(row$observation, paste(
    "The model has 21 free parameters and 20 cases were used. With fewer cases",
    "than free parameters the estimates are unstable, and an improper solution is",
    "more likely."
  ))
  summarized <- paste(trimws(capture.output(print(summary(small)))), collapse = " ")
  expect_match(summarized, "Free parameters (Review): The model has 21 free parameters", fixed = TRUE)
  expect_match(summarized, "Fit the model to more cases, or fit a model with fewer parameters",
               fixed = TRUE)
  # At 21 cases there are as many cases as free parameters.
  even <- nomo_cfa(three_factors, nomo_test_simulate(population, n = 21, seed = 3, exact = TRUE),
                   modification_indices = FALSE)
  expect_false("cases_below_parameters" %in% even$decision_log$metric)
})


test_that("the audit's 60-case Big Five CFA is reported as improper and too small", {
  skip_on_cran()
  data <- bfi_recoded()
  small <- withr::with_seed(7, data[sample(nrow(data), 60), ])
  out <- nomo_cfa(nomo_model(big_five), small, modification_indices = FALSE)
  # lavaan warns that the latent covariance matrix is not positive definite,
  # and the improper-solution table now says so too.
  expect_identical(
    "latent_covariance_not_positive_definite" %in% out$heywood$issue,
    any(grepl("covariance matrix of latent variables is not positive definite",
              out$engine_warnings, fixed = TRUE))
  )
  expect_true("latent_covariance_not_positive_definite" %in% out$heywood$issue)
  expect_true("cases_below_parameters" %in% out$decision_log$metric)
})


test_that("an ESEM reports a matrix that is not positive definite in either model", {
  skip_on_cran()
  es <- nomo_esem(three_factors, nonpd_latent_data())
  log <- es$decision_log
  row <- log[log$metric == "latent_covariance_not_positive_definite" & log$object == "CFA latent covariances", ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$severity, "concern")
  expect_match(row$observation, "The latent covariance matrix of F1, F2, and F3 is not positive definite",
               fixed = TRUE)
  # The eigenvalue is in the explanation, so no second value sentence follows.
  expect_false(grepl("The value in the CFA", row$observation, fixed = TRUE))
})


# ESEM with ordered-factor columns (helppages-7) -----------------------------------

test_that("an ESEM records ordered-factor columns as ordered, with the estimator lavaan used", {
  skip_on_cran()
  model <- "A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4"
  items <- c(paste0("a", 1:4), paste0("b", 1:4))
  es <- nomo_esem(model, nomo_demo_ordinal)
  expect_identical(es$ordered, items)
  expect_identical(es$ordered_detected, items)
  expect_identical(es$estimator, "WLSMV")
  expect_identical(lavaan::lavInspect(es$fits$ESEM, "options")$estimator, "DWLS")
  row <- es$decision_log[es$decision_log$metric == "ordered_detected", ]
  expect_identical(row$severity, "review")
  expect_identical(row$stage, "esem")
  expect_identical(nomo_methods_used(es), c("esem", "wlsmv_cfa", "categorical_correlations"))
  printed <- paste(trimws(capture.output(print(es))), collapse = " ")
  expect_match(printed, "8 indicators are stored as ordered factors and not named in `ordered`",
               fixed = TRUE)

  # Named in `ordered`, the same models are fitted and nothing is flagged.
  named <- nomo_esem(model, nomo_demo_ordinal, ordered = items)
  expect_equal(named$models, es$models)
  expect_identical(named$ordered_detected, character())
  expect_false("ordered_detected" %in% named$decision_log$metric)

  # Continuous indicators are fitted with ML, which is now recorded.
  continuous <- nomo_esem(model, nomo_demo_continuous)
  expect_identical(continuous$estimator, "ML")
  expect_identical(continuous$ordered, character())
  expect_identical(nomo_methods_used(continuous), c("esem", "ml_cfa"))
})


# Few categories fitted as continuous (usefulness-04) ------------------------------

test_that("indicators with few categories fitted as continuous are flagged, with the remedy", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  data <- three_category_bfi()
  model <- nomo_model(big_five[c("N", "C")])
  out <- nomo_cfa(model, data, modification_indices = FALSE)
  row <- out$decision_log[out$decision_log$metric == "few_categories", ]
  expect_identical(row$severity, "review")
  expect_identical(row$value, 10)
  expect_identical(row$observation, paste(
    "10 indicators have 4 or fewer observed categories and were fitted as continuous",
    "with ML: N1, N2, N3, N4, N5, C1, C2, C3, C4, C5."
  ))
  expect_match(row$recommendation, "(Rhemtulla et al., 2012)", fixed = TRUE)
  expect_match(row$recommendation, "Name these indicators in `ordered`", fixed = TRUE)
  printed <- paste(capture.output(print(out)), collapse = " ")
  expect_match(printed, paste(
    "10 indicators have 4 or fewer observed categories and were fitted as",
    "continuous. Name them in `ordered` to fit them as ordered categories with",
    "WLSMV."
  ), fixed = TRUE)
  expect_match(printed, "WLSMV = diagonally weighted least squares", fixed = TRUE)
  summarized <- paste(trimws(capture.output(print(summary(out)))), collapse = " ")
  expect_match(summarized, "Categories (Review): 10 indicators have 4 or fewer", fixed = TRUE)
  expect_match(summarized, "and compare the loadings.", fixed = TRUE)

  # Declared ordered, the indicators are categorical and nothing is said.
  ordered <- nomo_cfa(model, data, ordered = names(data), modification_indices = FALSE)
  expect_false(any(c("few_categories", "moderate_categories") %in% ordered$decision_log$metric))
})


test_that("category rows say one indicator in the singular and suggest MLR only under ML", {
  one <- nomo_cfa_categories_log(nomo_log_new(), c(a1 = 3L, b1 = 6L), "ML")
  expect_identical(one$metric, c("few_categories", "moderate_categories"))
  expect_identical(one$severity, c("review", "info"))
  expect_identical(
    one$observation[[1L]],
    "1 indicator has 4 or fewer observed categories and was fitted as continuous with ML: a1."
  )
  expect_identical(
    one$observation[[2L]],
    paste("1 indicator was fitted as continuous with ML; its factor's indicators use",
          "5 to 7 observed categories between them.")
  )
  two <- nomo_cfa_categories_log(nomo_log_new(), c(a1 = 5L, b1 = 6L), "ML")
  expect_match(two$observation, "^2 indicators were fitted as continuous with ML; their factors'")
  expect_match(one$recommendation[[2L]], "`estimator = \"MLR\"`", fixed = TRUE)
  robust <- nomo_cfa_categories_log(nomo_log_new(), c(a1 = 6L, b1 = 9L), "MLR")
  expect_identical(nrow(robust), 0L)
})


test_that("categories are counted over a factor's indicators, so a ceiling item is not flagged", {
  skip_on_cran()
  walk <- walkthrough_recoded()
  # EF3's answers fall in three of the five categories.
  expect_identical(length(unique(walk$EF3)), 3L)
  out <- nomo_cfa(nomo_model(walkthrough_scales), walk, modification_indices = FALSE)
  counts <- nomo_cfa_category_counts(out$fit, walk)
  expect_identical(counts[["EF3"]], 5L)
  expect_false("few_categories" %in% out$decision_log$metric)
  # Five-point items under ML get information about MLR, which is not a flag.
  info <- out$decision_log[out$decision_log$metric == "moderate_categories", ]
  expect_identical(info$severity, "info")
  expect_identical(info$value, 10)
  # A model with no numeric indicator has nothing to count.
  expect_identical(nomo_cfa_category_counts(out$fit, walk[0, "cohort", drop = FALSE]), integer())
})


test_that("an EFA flags numeric items with few categories modeled as continuous", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  data <- three_category_bfi()
  out <- nomo_efa(data, factors = 2)
  row <- out$decision_log[out$decision_log$metric == "few_categories", ]
  expect_identical(row$severity, "review")
  expect_identical(row$observation, paste(
    "10 numeric-discrete items with 4 or fewer observed categories were modeled as",
    "continuous: N1, N2, N3, N4, N5, C1, C2, C3, C4, C5."
  ))
  printed <- paste(trimws(capture.output(print(out))), collapse = " ")
  expect_match(printed, "Categories (Review): 10 numeric-discrete items", fixed = TRUE)
  expect_match(printed, "rerun with `types` marking them as ordinal", fixed = TRUE)
  summarized <- paste(trimws(capture.output(print(summary(out)))), collapse = " ")
  expect_match(summarized, "Categories (Review): 10 numeric-discrete items", fixed = TRUE)

  # Declared continuous, the items are the researcher's choice.
  declared <- nomo_efa(data, factors = 2,
                       types = stats::setNames(rep("continuous", ncol(data)), names(data)))
  expect_false("few_categories" %in% declared$decision_log$metric)
  # Five-point items, one of which uses three categories, are not flagged.
  walk <- walkthrough_recoded()
  expect_identical(nomo_efa_few_categories(walk[unlist(walkthrough_scales)],
                                           nomo_efa(walk[unlist(walkthrough_scales)],
                                                    factors = 2)$item_types),
                   character())
})


# Loadings that run against their factor (usefulness-02) ---------------------------

test_that("a loading against most of its factor's loadings is flagged for review", {
  std <- data.frame(lhs = "F1", op = "=~", rhs = paste0("x", 1:5),
                    est.std = c(.7, .6, .65, -.75, -.3))
  tab <- nomo_cfa_loadings(std, nomo_defaults())
  # x4 has the largest loading, but most loadings set the direction.
  expect_identical(tab$attention, c("KEEP", "KEEP", "KEEP", "REVIEW", "REVIEW"))
  expect_identical(tab$explanation[[4L]], paste(
    "Standardized loading -0.75 is opposite in sign to the loadings of x1, x2, and x3",
    "on F1. If x4 is reverse-keyed, recode it and refit: as it stands, a summed score",
    "and omega count it against the factor."
  ))
  expect_match(tab$explanation[[5L]], paste0(
    "^Standardized loading -0.30 is below the review reference 0.50 in absolute value; ",
    "inspect item content, precision, and model specification. It is also opposite in ",
    "sign to the loadings of x1, x2, and x3 on F1. If x5"
  ))

  # When the signs split evenly, the largest loading sets the direction.
  tie <- nomo_cfa_sign_split(rep("F", 4), paste0("y", 1:4), c(.6, .5, -.8, -.55))
  expect_identical(tie$against, c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(tie$others[1:2], c("y3 and y4", "y3 and y4"))
  # Items declared reverse-keyed do not set the direction, so the declared
  # pair is against its factor whatever its loadings' size.
  keyed <- nomo_cfa_sign_split(rep("F", 4), paste0("y", 1:4), c(.6, .5, -.8, -.55),
                               reference = c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(keyed$against, c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(keyed$others[3:4], c("y1 and y2", "y1 and y2"))
  # A factor whose items are all declared has nothing to compare them with.
  all_keyed <- nomo_cfa_sign_split(rep("F", 3), paste0("y", 1:3), c(.6, -.5, -.4),
                                   reference = FALSE)
  expect_false(any(all_keyed$against))
  expect_identical(all_keyed$lead, c(0, 0, 0))
  # Cross-loaded items, and factors loading on a higher-order factor, are not
  # compared.
  split <- nomo_cfa_sign_split(
    c("F", "F", "F", "G", "G", "G", "H", "H"), c("a", "b", "c", "c", "d", "e", "F", "G"),
    c(.7, .6, .5, -.4, .7, .6, .8, -.6), observed = c(rep(TRUE, 6), FALSE, FALSE)
  )
  expect_false(any(split$against))
  # A loading above 1 keeps its own flag and explanation.
  extreme <- nomo_cfa_loadings(
    data.frame(lhs = "F", op = "=~", rhs = c("a", "b", "c"), est.std = c(.7, .6, -1.2)),
    nomo_defaults()
  )
  expect_identical(extreme$attention[[3L]], "STRONG REVIEW")
  expect_false(grepl("opposite in sign", extreme$explanation[[3L]], fixed = TRUE))
})


test_that("unrecoded reverse-keyed items are flagged by the CFA, reliability, and validity", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  model <- nomo_model(big_five[c("A", "C", "E")])
  raw <- nomo_cfa(model, bfi_raw(), modification_indices = FALSE)
  loads <- raw$standardized_loadings
  against <- loads$item[grepl("opposite in sign", loads$explanation, fixed = TRUE)]
  # A's marker is A1, so A2 to A5 load negatively and A1 is the item against.
  expect_setequal(against, c("A1", "C4", "C5", "E1", "E2"))
  expect_true(all(loads$attention[loads$item %in% against] == "REVIEW"))
  against <- nomo_cfa_against_items(raw$fit)
  expect_named(against, c("A", "C", "E"))
  expect_identical(against[c("A", "C", "E")], list(A = "A1", C = c("C4", "C5"), E = c("E1", "E2")))
  # Most of A's and E's loadings are negative: those factors run opposite to
  # most of their items.
  expect_identical(attr(against, "reversed"), c("A", "E"))

  rel <- nomo_reliability(raw)
  signs <- rel$decision_log[rel$decision_log$metric == "loading_signs", ]
  expect_identical(signs$object, c("A", "C", "E"))
  expect_identical(unique(signs$severity), "concern")
  expect_identical(signs$observation[[2L]], paste(
    "C4 and C5 load on C with the opposite sign to its other indicators. Omega and",
    "alpha describe the unit-weighted sum, in which these items count against the",
    "factor."
  ))
  expect_match(signs$observation[[1L]], "in which this item counts against", fixed = TRUE)
  printed <- paste(trimws(capture.output(print(rel))), collapse = " ")
  expect_match(printed, "C (Concern): C4 and C5 load on C with the opposite sign", fixed = TRUE)
  summarized <- paste(trimws(capture.output(print(summary(rel)))), collapse = " ")
  expect_match(summarized, "Recode reverse-keyed items, refit the CFA, and estimate reliability again.",
               fixed = TRUE)
  # The out-of-range alpha names itself.
  alpha_c <- rel$decision_log[rel$decision_log$object == "C" & rel$decision_log$metric == "alpha", ]
  expect_match(alpha_c$observation, "^Alpha \\(-\\.[0-9]{2}\\) is outside the 0 to 1 range of a reliability coefficient\\.")

  val <- nomo_validity(raw)
  vsigns <- val$decision_log[val$decision_log$metric == "loading_signs", ]
  expect_identical(vsigns$object, c("A", "C", "E"))
  # A and E are oriented against most of their items; C is not.
  expect_match(vsigns$observation[[1L]], "Most of A's loadings are negative, so A runs opposite",
               fixed = TRUE)
  expect_false(grepl("Most of", vsigns$observation[[2L]], fixed = TRUE))
  # A strong loading flagged only for its sign keeps the CFA's explanation.
  c4 <- val$decision_log[val$decision_log$object == "C4", ]
  expect_match(c4$observation, "^Standardized loading -0\\.[0-9]{2} is opposite in sign")
  printed <- paste(trimws(capture.output(print(val))), collapse = " ")
  expect_match(printed, "C (Concern): C4 and C5 load on C with the opposite sign", fixed = TRUE)
  # AVE squares the loadings, so a loading flagged only for its sign is not
  # counted beside it; a weak one still is.
  conv <- nomo_validity_convergent_table(val)
  expect_identical(conv$n_loading_review[conv$construct %in% c("C", "E")], c(0L, 0L))
  expect_false(grepl("loadings are flagged (C4", printed, fixed = TRUE))
  weak <- val
  weak$standardized_loadings$loading[weak$standardized_loadings$item == "C4"] <- -0.2
  conv <- nomo_validity_convergent_table(weak)
  expect_identical(conv$n_loading_review[conv$construct == "C"], 1L)
})


test_that("a guided run says when declared reverse-keyed items still load against their factor", {
  skip_on_cran()
  local_reproducible_output(width = 80)
  decisions <- list(factor_count = c(EF = 1, TF = 1), cfa_model = nomo_model(walkthrough_scales))
  settings <- list(screen = list(reverse = c("EF2", "TF2"), scale_range = c(1, 5)),
                   factors = list(n_iter = 20, seed = 2026))
  raw <- nomo_run(nomo_demo_walkthrough, walkthrough_scales, decisions = decisions,
                  settings = settings)
  expect_identical(raw$next_stage, "measurement_review")
  expect_identical(nomo_run_unrecoded_items(raw), c("EF2", "TF2"))
  expect_match(raw$decision_requests$observation, paste(
    "EF2 and TF2 are declared reverse-keyed and still load on their factors in the",
    "CFA with the opposite sign to the items not declared, so the data were not",
    "recoded; the CFA, reliability, and validity evidence describe them as answered."
  ), fixed = TRUE)
  log <- nomo_table(raw, "component_log")
  row <- log[log$metric %in% "declared_reverse_keyed", , drop = FALSE]
  expect_identical(row$severity, "concern")
  expect_identical(row$pipeline_component, "cfa")
  # Listed under the CFA, the row does not name it again.
  expect_identical(row$observation, paste(
    "EF2 and TF2 are declared reverse-keyed and still load on their factors with the",
    "opposite sign to the items not declared, so the data were not recoded; the CFA,",
    "reliability, and validity evidence describe them as answered."
  ))
  expect_match(row$recommendation, paste(
    "Recode the declared items in the data, as 6 minus the response on the declared",
    "1 to 5 scale, and start a new nomo_run()"
  ), fixed = TRUE)
  printed <- paste(trimws(capture.output(print(raw))), collapse = " ")
  expect_match(printed, "EF2 and TF2 are declared reverse-keyed and still load", fixed = TRUE)
  summarized <- paste(trimws(capture.output(print(summary(raw)))), collapse = " ")
  expect_match(summarized, "The CFA (Concern): EF2 and TF2 are declared reverse-keyed",
               fixed = TRUE)

  # Recoded as declared, the items load with their factors and nothing is said.
  recoded <- nomo_run(walkthrough_recoded(), walkthrough_scales, decisions = decisions,
                      settings = settings)
  expect_identical(nomo_run_unrecoded_items(recoded), character())
  expect_false(grepl("reverse-keyed", recoded$decision_requests$observation, fixed = TRUE))
  expect_false("declared_reverse_keyed" %in% nomo_table(recoded, "component_log")$metric)
  # Before the CFA there is nothing to check.
  expect_identical(nomo_run_unrecoded_items(list(results = list(cfa = NULL))), character())
})


test_that("declared items are found when they are half or more of a factor's items", {
  skip_on_cran()
  # The run's state as nomo_run_unrecoded_items() reads it.
  run_state <- function(data, scales, reverse, scale_range = NULL) {
    cfa <- nomo_cfa(nomo_model(scales), data, modification_indices = FALSE)
    list(results = list(cfa = cfa), scales = scales, handoff = NULL,
         settings = list(screen = list(reverse = reverse, scale_range = scale_range)))
  }
  # C1, C2, and C3 declared and left as answered: most of C's loadings run
  # with them, so the CFA names C4 and C5, and the declared items are judged
  # against the items not declared.
  majority <- bfi_recoded()[1:800, ]
  majority[c("C1", "C2", "C3")] <- 7L - majority[c("C1", "C2", "C3")]
  x <- run_state(majority, big_five[c("C", "E")], c("C1", "C2", "C3"), c(1, 6))
  expect_identical(unlist(nomo_cfa_against_items(x$results$cfa$fit), use.names = FALSE),
                   c("C4", "C5"))
  expect_identical(nomo_run_unrecoded_items(x), c("C1", "C2", "C3"))
  row <- nomo_run_unrecoded_log(x)
  expect_identical(row$object, "C1, C2, C3")
  expect_match(row$observation, "^C1, C2, and C3 are declared reverse-keyed and still load")
  # The recode follows the declared 1 to 6 scale.
  expect_match(row$recommendation, "as 7 minus the response on the declared 1 to 6 scale",
               fixed = TRUE)
  x$settings$screen$scale_range <- NULL
  expect_match(nomo_run_unrecoded_log(x)$recommendation,
               "for example as 6 minus the response on a 1 to 5 scale", fixed = TRUE)

  # Two of four declared, with the largest loading among them: an even split
  # that the largest loading would decide against E3 and E4.
  balanced <- bfi_recoded()[1:800, ]
  balanced[c("E1", "E2")] <- 7L - balanced[c("E1", "E2")]
  x <- run_state(balanced, list(E = paste0("E", 1:4), C = paste0("C", 1:5)), c("E1", "E2"))
  expect_identical(unlist(nomo_cfa_against_items(x$results$cfa$fit), use.names = FALSE),
                   c("E3", "E4"))
  expect_identical(nomo_run_unrecoded_items(x), c("E1", "E2"))

  # Recoded as declared, the same declarations find nothing.
  x <- run_state(bfi_recoded()[1:800, ], list(E = paste0("E", 1:4), C = paste0("C", 1:5)),
                 c("E1", "E2"))
  expect_identical(nomo_run_unrecoded_items(x), character())
  expect_identical(nrow(nomo_run_unrecoded_log(x)), 0L)
})


# Silence on clean data -------------------------------------------------------------

test_that("the new signals stay silent on the packaged demo data and recoded Big Five items", {
  skip_on_cran()
  two <- nomo_model(list(A = paste0("a", 1:5), B = paste0("b", 1:5)))
  fits <- list(
    continuous = nomo_cfa(two, nomo_demo_continuous, modification_indices = FALSE),
    ordinal = nomo_cfa(two, nomo_demo_ordinal, modification_indices = FALSE),
    network = nomo_cfa(nomo_model(list(AG = paste0("ag", 1:4), PE = paste0("pe", 1:4),
                                       SD = paste0("sd", 1:3))),
                       nomo_demo_network, modification_indices = FALSE),
    walkthrough = nomo_cfa(nomo_model(walkthrough_scales), walkthrough_recoded(),
                           modification_indices = FALSE),
    longitudinal = nomo_cfa(nomo_model(list(T1 = paste0("w", 1:4, "_t1"),
                                            T2 = paste0("w", 1:4, "_t2"))),
                            nomo_demo_longitudinal, modification_indices = FALSE),
    bfi = nomo_cfa(nomo_model(big_five), bfi_recoded(), modification_indices = FALSE)
  )
  for (name in names(fits)) {
    fit <- fits[[name]]
    expect_false(any(new_metrics %in% c(fit$decision_log$metric, fit$heywood$issue)), label = name)
    expect_false(any(grepl("opposite in sign", fit$standardized_loadings$explanation)), label = name)
  }
  for (name in c("continuous", "walkthrough", "bfi")) {
    expect_false("loading_signs" %in% nomo_reliability(fits[[name]])$decision_log$metric,
                 label = name)
    expect_false("loading_signs" %in% nomo_validity(fits[[name]])$decision_log$metric,
                 label = name)
  }
  efa <- nomo_efa(bfi_recoded()[unlist(big_five)], factors = 5)
  expect_false("few_categories" %in% efa$decision_log$metric)
})
