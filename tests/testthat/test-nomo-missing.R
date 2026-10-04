# Fixtures ---------------------------------------------------------------------

missing_model <- "A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4"
missing_b <- paste0("b", 1:4)


# Two factors correlating .50, four items loading .70 on each.
missing_population <- function(n, seed, r = .50) {
  set.seed(seed)
  lam <- .70
  f <- matrix(stats::rnorm(n * 2), n, 2) %*% chol(matrix(c(1, r, r, 1), 2))
  x <- cbind(
    f[, 1] %o% rep(lam, 4) + matrix(stats::rnorm(n * 4, sd = sqrt(1 - lam^2)), n),
    f[, 2] %o% rep(lam, 4) + matrix(stats::rnorm(n * 4, sd = sqrt(1 - lam^2)), n)
  )
  d <- as.data.frame(x)
  names(d) <- c(paste0("a", 1:4), missing_b)
  d
}


# MAR: the B items are missing for most cases whose observed A items are high,
# so missingness depends on observed values only. MCAR: the same number of
# cases lose their B items at random.
missing_samples <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    d <- missing_population(4000, seed = 32)
    high <- rowMeans(d[, paste0("a", 1:4)]) > 0
    drop <- high & stats::runif(nrow(d)) < .90
    mar <- d
    mar[drop, missing_b] <- NA
    mcar <- d
    mcar[sample.int(nrow(d), sum(drop)), missing_b] <- NA
    cache <<- list(mar = mar, mcar = mcar, n_dropped = sum(drop))
    cache
  }
})


# Each sensitivity refits the model several times, so the two used throughout
# are computed once.
missing_results <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    samples <- missing_samples()
    cache <<- list(
      mar = nomo_missing(nomo_cfa(missing_model, data = samples$mar), data = samples$mar),
      mcar = nomo_missing(nomo_cfa(missing_model, data = samples$mcar), data = samples$mcar)
    )
    cache
  }
})


missing_correlation <- function(x, strategy) {
  e <- x$estimates
  e$estimate[e$type == "factor_correlation" & e$strategy == strategy]
}


# The strategies reproduce lavaan ----------------------------------------------

test_that("each strategy reproduces lavaan's own estimates", {
  skip_on_cran()
  mar <- missing_samples()$mar
  out <- missing_results()$mar

  for (strategy in c("listwise", "ml")) {
    engine <- lavaan::standardizedSolution(
      lavaan::cfa(missing_model, data = mar, missing = strategy)
    )
    loadings <- engine[engine$op == "=~", ]
    ours <- out$estimates[out$estimates$type == "loading" & out$estimates$strategy == strategy, ]
    expect_equal(ours$estimate, loadings$est.std, tolerance = 1e-8)
    expect_equal(ours$se, loadings$se, tolerance = 1e-8)
  }

  strategies <- out$strategies
  expect_identical(strategies$strategy, c("listwise", "ml"))
  expect_identical(strategies$role, c("comparison", "reference"))
  expect_identical(strategies$requires, c("MCAR", "MAR"))
  expect_identical(strategies$n_used, c(4000 - missing_samples()$n_dropped, 4000))
})


# Known parameters: MAR and MCAR -----------------------------------------------

test_that("under MAR, FIML recovers the population value and listwise deletion does not", {
  skip_on_cran()
  out <- missing_results()$mar

  fiml <- missing_correlation(out, "ml")
  listwise <- missing_correlation(out, "listwise")
  fiml_se <- out$estimates$se[out$estimates$type == "factor_correlation" &
                                out$estimates$strategy == "ml"]

  # Enders and Bandalos (2001): FIML unbiased under MAR, listwise deletion
  # biased. Selecting cases on high A restricts its range among complete cases,
  # which attenuates the correlation.
  expect_lt(abs(fiml - .50), 2 * fiml_se)
  expect_lt(listwise, fiml - fiml_se)

  entry <- out$decision_log[out$decision_log$metric == "estimate_difference", ]
  expect_identical(nrow(entry), 1L)
  expect_identical(entry$severity, "review")
  expect_match(entry$reference, "Schafer & Graham (2002)", fixed = TRUE)
  expect_match(entry$recommendation, "If the data are MAR and the model is correct", fixed = TRUE)
  expect_match(entry$recommendation, "fewer cases than the fullest strategy", fixed = TRUE)
})


test_that("under MCAR, both strategies recover the population value and FIML is more efficient", {
  skip_on_cran()
  out <- missing_results()$mcar
  e <- out$estimates

  for (strategy in c("listwise", "ml")) {
    se <- e$se[e$type == "factor_correlation" & e$strategy == strategy]
    expect_lt(abs(missing_correlation(out, strategy) - .50), 3 * se)
  }

  # The A items are fully observed, so FIML keeps every case that listwise
  # deletion discards for them, and its standard errors are smaller.
  a <- e$type == "loading" & grepl("^A =~", e$parameter)
  expect_true(all(e$se[a & e$strategy == "ml"] < e$se[a & e$strategy == "listwise"]))
})


# What the log says ------------------------------------------------------------

test_that("the log states what each strategy assumes and that MAR is untestable", {
  skip_on_cran()
  out <- missing_results()$mar
  log <- out$decision_log

  mechanism <- log[log$metric == "missing_mechanism", ]
  expect_identical(mechanism$severity, "info")
  expect_match(mechanism$recommendation, "cannot in general be tested", fixed = TRUE)
  expect_match(mechanism$recommendation, "missing completely at random (MCAR)", fixed = TRUE)

  discarded <- log[log$metric == "cases_discarded", ]
  expect_identical(discarded$value, as.numeric(missing_samples()$n_dropped))

  expect_true("fiml_normality" %in% log$metric)
})


test_that("agreement is reported without implying that either strategy is unbiased", {
  skip_on_cran()
  out <- nomo_missing(
    nomo_cfa(
      "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
      data = nomo_demo_continuous
    ),
    data = nomo_demo_continuous
  )

  entry <- out$decision_log[out$decision_log$metric == "estimates_agree", ]
  expect_identical(nrow(entry), 1L)
  expect_identical(entry$severity, "info")
  expect_match(entry$recommendation, "missing not at random", fixed = TRUE)
  expect_false("estimate_difference" %in% out$decision_log$metric)
})


test_that("data with nothing missing is not refitted and says why", {
  complete <- stats::na.omit(nomo_demo_continuous)
  out <- nomo_missing(
    nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5", data = complete),
    data = complete
  )

  expect_identical(out$decision_log$metric, "no_missing")
  expect_identical(out$strategies$available, c(TRUE, FALSE))
  expect_error(nomo_missing(out, data = complete), "supports `nomo_cfa`")
  expect_error(
    nomo_missing(nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5", data = complete),
                 data = complete, reliability = NA),
    "TRUE or FALSE"
  )
  expect_match(out$strategies$note[[2L]], "no modeled variable has missing values", fixed = TRUE)

  # The print says why too, and an unfitted strategy's N is a dash (#89).
  printed <- capture.output(print(out))
  expect_true(any(grepl("FIML: Not fitted: no modeled variable has missing values.",
                        printed, fixed = TRUE)))
  expect_false(any(grepl("\\bNA\\b", printed)))
})


# What lavaan actually did ------------------------------------------------------

test_that("a strategy lavaan substitutes or refuses is recorded as such", {
  skip_on_cran()
  model <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"

  uls <- nomo_missing(
    nomo_cfa(model, data = nomo_demo_continuous, estimator = "ULS"),
    data = nomo_demo_continuous,
    reliability = FALSE
  )
  row <- uls$strategies[uls$strategies$strategy == "ml", ]
  # What lavaan does here depends on its version: 0.7 runs its two-stage
  # method in place of FIML, while 0.6-21, the declared minimum, refuses. Either
  # way it is not FIML, and the table and log must say which happened.
  if (isTRUE(row$available)) {
    expect_false(row$lavaan_missing %in% c("ml", "ml.x"))
    expect_true(is.na(row$requires))
    expect_true("strategy_substituted" %in% uls$decision_log$metric)
  } else {
    expect_true(nzchar(row$note))
    expect_true("strategy_unavailable" %in% uls$decision_log$metric)
  }
  # Credited by what lavaan estimated, so FIML is never credited here.
  expect_false("fiml" %in% nomo_methods(uls)$id)

  mlm <- nomo_missing(
    nomo_cfa(model, data = nomo_demo_continuous, estimator = "MLM"),
    data = nomo_demo_continuous,
    reliability = FALSE
  )
  row <- mlm$strategies[mlm$strategies$strategy == "ml", ]
  expect_false(row$available)
  expect_match(row$note, "MLM", fixed = TRUE)
  expect_true("strategy_unavailable" %in% mlm$decision_log$metric)

  # With FIML refused, the strategy that was fitted becomes the reference, and
  # the log says that differences no longer estimate bias.
  expect_identical(mlm$reference, "listwise")
  substituted <- mlm$decision_log[mlm$decision_log$metric == "reference_substituted", ]
  expect_identical(nrow(substituted), 1L)
  expect_match(substituted$recommendation, "no longer", fixed = TRUE)
})


test_that("ordered indicators compare listwise with pairwise deletion", {
  skip_on_cran()
  items <- c(paste0("a", 1:5), paste0("b", 1:5))
  ordinal <- nomo_demo_ordinal
  set.seed(32)
  ordinal$a2[sample.int(nrow(ordinal), 40)] <- NA

  out <- nomo_missing(
    nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
             data = ordinal, ordered = items),
    data = ordinal,
    reliability = FALSE
  )

  expect_identical(out$reference, "pairwise")
  expect_identical(out$strategies$strategy, c("listwise", "pairwise"))
  expect_identical(out$strategies$requires, c("MCAR", "MCAR"))
  expect_true(all(out$strategies$available))
  expect_identical(sort(nomo_methods(out)$id),
                   sort(c("listwise_deletion", "pairwise_deletion", "missing_sensitivity")))
})


# The data must be the data the model was fitted to ----------------------------

test_that("data that does not reproduce the fitted model is refused", {
  model <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
  fit <- nomo_cfa(model, data = nomo_demo_continuous)

  expect_error(nomo_missing(fit, data = nomo_demo_continuous[-1, ]), "has 499 rows")

  altered <- nomo_demo_continuous
  altered$a1 <- altered$a1 + 0.01
  altered$a1[1] <- altered$a1[1] + 1
  expect_error(nomo_missing(fit, data = altered), "does not reproduce the fitted model")

  expect_error(nomo_missing(fit, data = "not data"), "non-empty data frame")
  # The fitted model is `fit`, as in nomo_reliability() and nomo_validity() (#114).
  expect_error(nomo_missing(fit = fit, data = "not data"), "non-empty data frame")
  expect_identical(names(formals(nomo_missing))[[1L]], "fit")
  expect_error(nomo_missing(list(), data = nomo_demo_continuous), "supports `nomo_cfa`")
})


test_that("strategies can be named with lavaan's aliases, and are validated", {
  skip_on_cran()
  model <- "A =~ a1 + a2 + a3 + a4 + a5
B =~ b1 + b2 + b3 + b4 + b5"
  fit <- nomo_cfa(model, data = nomo_demo_continuous)

  out <- nomo_missing(fit, data = nomo_demo_continuous, strategies = "fiml",
                      reliability = FALSE)
  expect_identical(out$strategies$strategy, c("ml", "listwise"))
  expect_identical(out$reference, "ml")

  expect_error(nomo_missing(fit, data = nomo_demo_continuous, strategies = NA_character_), "character vector")
  expect_error(nomo_missing(fit, data = nomo_demo_continuous, strategies = 1), "character vector")
})


# Networks ---------------------------------------------------------------------

test_that("a hypothesis whose concordance depends on the strategy is flagged", {
  skip_on_cran()
  mar <- missing_samples()$mar
  cfa <- missing_results()$mar

  # A magnitude boundary between the two estimates: FIML meets it, listwise
  # deletion does not.
  boundary <- round(mean(c(missing_correlation(cfa, "listwise"), missing_correlation(cfa, "ml"))), 3)
  net <- nomo_network(
    paste(missing_model, "B ~ A", sep = "\n"),
    data = mar,
    hypotheses = nomo_hypotheses("A -> B" = positive(min = boundary)),
    std.lv = TRUE
  )
  out <- nomo_missing(net, data = mar)

  h <- out$estimates
  expect_identical(h$type, c("hypothesis", "hypothesis"))
  expect_true(h$concordance_changed[h$strategy == "listwise"])

  entry <- out$decision_log[out$decision_log$metric == "concordance_changed", ]
  expect_identical(nrow(entry), 1L)
  expect_identical(entry$severity, "review")
  expect_match(entry$observation, "H1 (A -> B)", fixed = TRUE)
  expect_null(out$reliability)
  expect_error(nomo_table(out, "reliability"), "nomo_cfa")
})


test_that("a network fitted to a nomo_split is compared on its calibration sample", {
  skip_on_cran()
  d <- nomo_demo_network
  set.seed(32)
  d$ag2[sample.int(nrow(d), 60)] <- NA
  split <- nomo_split(d, validation_prop = .40, seed = 2026)

  net <- nomo_network(
    "Agency =~ ag1 + ag2 + ag3 + ag4\nPersistence =~ pe1 + pe2 + pe3 + pe4\nPersistence ~ Agency",
    data = split,
    hypotheses = nomo_hypotheses("Agency -> Persistence" = positive())
  )
  out <- nomo_missing(net, data = split)
  expect_identical(out$pattern$n_cases, nrow(split$calibration))
  expect_true(all(out$strategies$available))
})


# Presentation -----------------------------------------------------------------

test_that("nomo_missing prints and tabulates its evidence", {
  skip_on_cran()
  out <- missing_results()$mar

  expect_output(print(out), "Reference: FIML")
  expect_output(print(out), "Largest differences from the reference")
  expect_output(print(out), "cannot be tested")

  for (type in c("strategies", "estimates", "fit", "reliability", "pattern",
                 "variables", "decision_log")) {
    expect_s3_class(nomo_table(out, type), "data.frame")
  }
  expect_identical(nomo_table(out, "fit")$strategy, c("listwise", "ml"))

  rel <- nomo_table(out, "reliability")
  expect_true(all(c("omega", "alpha") %in% rel$metric))
  expect_true(all(is.na(rel$difference[rel$role == "reference"])))
})


test_that("the object holds the fields its help page gives, and no others (#145)", {
  skip_on_cran()
  out <- missing_results()$mar

  # The fields ?nomo_missing gives as the ones to read, which the stability
  # policy covers. `reference` and `fitted_as` are among them: print() reads
  # both.
  stable <- c("reference", "fitted_as", "pattern", "variables", "strategies",
              "fit", "estimates", "reliability", "decision_log")
  # The fields it names as not part of the stable interface. A field added to
  # the object belongs in one list or the other on that page.
  other <- c("call", "object", "ordered", "fits")
  expect_setequal(names(out), c(stable, other))
  expect_identical(out$reference, "ml")
  expect_identical(out$fitted_as, "listwise")
  # The unit the page states: proportions, not percentages.
  expect_true(out$pattern$pct_incomplete > 0 && out$pattern$pct_incomplete < 1)
  expect_true(all(out$variables$pct_missing >= 0 & out$variables$pct_missing < 1))
})


# Remaining paths (#72) --------------------------------------------------------

missing_demo_model <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"


test_that("a network's fit comparison carries each strategy's fit indices", {
  skip_on_cran()
  d <- nomo_demo_network
  set.seed(32)
  d$ag2[sample.int(nrow(d), 60)] <- NA
  net <- nomo_network(
    "Agency =~ ag1 + ag2 + ag3 + ag4\nPersistence =~ pe1 + pe2 + pe3 + pe4\nPersistence ~ Agency",
    data = d,
    hypotheses = nomo_hypotheses("Agency -> Persistence" = positive())
  )
  out <- nomo_missing(net, data = d)
  fit <- nomo_table(out, "fit")

  expect_identical(fit$strategy, c("listwise", "ml"))
  expect_true(all(is.finite(as.matrix(fit[, c("chi_square", "df", "CFI", "TLI", "RMSEA", "SRMR")]))))
  # The network was fitted with listwise deletion, so that row is its own fit.
  expect_equal(fit$chi_square[fit$strategy == "listwise"], net$fit_evidence$chisq)
  expect_equal(fit$SRMR[fit$strategy == "listwise"], net$fit_evidence$srmr)
})


test_that("data without a modeled variable, or that cannot be refitted, is refused", {
  fit <- nomo_cfa(missing_demo_model, data = nomo_demo_continuous)

  without_a1 <- nomo_demo_continuous[, setdiff(names(nomo_demo_continuous), "a1")]
  expect_error(nomo_missing(fit, data = without_a1), "modeled variable(s): a1", fixed = TRUE)

  all_missing <- nomo_demo_continuous
  all_missing$a1 <- NA_real_
  # lavaan prints its variable table before refusing; only the refusal matters.
  expect_error(
    utils::capture.output(nomo_missing(fit, data = all_missing)),
    "could not be confirmed", fixed = TRUE
  )
})


test_that("FIML is credited when it was fitted", {
  out <- nomo_missing(nomo_cfa(missing_demo_model, data = nomo_demo_continuous),
                      data = nomo_demo_continuous, reliability = FALSE)
  expect_true(all(c("missing_sensitivity", "listwise_deletion", "fiml") %in% nomo_methods(out)$id))
})


test_that("lavaan's missing-data aliases are read as the strategy they name", {
  normalize <- nomologR:::nomo_missing_normalize
  expect_identical(normalize(NULL), "listwise")
  expect_identical(normalize(NA_character_), "listwise")
  expect_identical(normalize(" "), "listwise")
  expect_identical(normalize("FIML"), "ml")
  expect_identical(normalize("direct"), "ml")
  expect_identical(normalize("fiml.x"), "ml.x")
  expect_identical(normalize("pairwise"), "pairwise")
})


test_that("a substituted reference says whether differences still estimate bias", {
  new_log <- nomologR:::nomo_log_new()

  # FIML replaced by an MCAR strategy: differences no longer estimate bias.
  log <- nomologR:::nomo_missing_reference_log(new_log, "ml", "listwise", "nomo_cfa")
  expect_identical(log$metric, "reference_substituted")
  expect_identical(log$object, "cfa")
  expect_match(log$recommendation, "no longer", fixed = TRUE)

  # Pairwise replaced by listwise: both require MCAR, so nothing changes in kind.
  log <- nomologR:::nomo_missing_reference_log(new_log, "pairwise", "listwise", "nomo_network")
  expect_identical(log$object, "network")
  expect_identical(log$recommendation, "Differences are measured from this strategy instead.")
})


test_that("a strategy that did not converge, or is inadmissible, is flagged for review", {
  skip_on_cran()
  out <- nomo_missing(nomo_cfa(missing_demo_model, data = nomo_demo_continuous),
                      data = nomo_demo_continuous, reliability = FALSE)
  pattern <- list(summary = out$pattern, variables = out$variables)
  write_log <- function(strategies) {
    nomologR:::nomo_missing_log(
      pattern = pattern, strategies = strategies, estimates = out$estimates,
      reference = out$reference, ordered = FALSE, kind = "nomo_cfa"
    )
  }

  unconverged <- out$strategies
  unconverged$converged[unconverged$strategy == "listwise"] <- FALSE
  entry <- write_log(unconverged)
  entry <- entry[entry$metric == "nonconvergence", ]
  expect_identical(entry$severity, "review")
  expect_match(entry$observation, "did not converge", fixed = TRUE)

  inadmissible <- out$strategies
  inadmissible$admissible[inadmissible$strategy == "listwise"] <- FALSE
  entry <- write_log(inadmissible)
  entry <- entry[entry$metric == "inadmissible_solution", ]
  expect_match(entry$observation, "negative variance", fixed = TRUE)
})


test_that("each difference is attributed only as far as the strategies allow", {
  out <- nomo_missing(nomo_cfa(missing_demo_model, data = nomo_demo_continuous),
                      data = nomo_demo_continuous, reliability = FALSE)
  difference_log <- function(estimates, strategies, reference) {
    nomologR:::nomo_missing_difference_log(
      nomologR:::nomo_log_new(), estimates, strategies, reference,
      nomologR:::nomo_missing_label_inline(reference), "cfa"
    )
  }
  flag_all <- function(e) {
    comparison <- e$role == "comparison"
    e$difference_in_se[comparison] <- 2
    e$beyond_half_se[comparison] <- TRUE
    e
  }

  # No comparison with a finite difference: nothing to report.
  none <- out$estimates
  none$difference_in_se <- NA_real_
  expect_identical(nrow(difference_log(none, out$strategies, "ml")), 0L)

  # Strategies that analyze the same number of cases: the sampling caveat is
  # stated without a count.
  same_n <- out$strategies
  same_n$n_used <- 500
  entry <- difference_log(flag_all(out$estimates), same_n, "ml")
  expect_match(entry$recommendation, "analyze different cases", fixed = TRUE)

  # A reference that is neither FIML nor pairwise-against-listwise: the
  # difference shows dependence on the strategy and is attributed to neither.
  renamed <- flag_all(out$estimates)
  renamed$strategy[renamed$strategy == "ml"] <- "pairwise"
  renamed$strategy[renamed$strategy == "listwise"] <- "two.stage"
  strategies <- out$strategies
  strategies$strategy <- c("two.stage", "pairwise")
  strategies$lavaan_missing <- strategies$strategy
  strategies$label <- nomologR:::nomo_missing_label(strategies$strategy)
  entry <- difference_log(renamed, strategies, "pairwise")
  expect_match(entry$recommendation, "depends on the choice of strategy", fixed = TRUE)
})


test_that("reliability that cannot be computed leaves an empty comparison, not a wrong one", {
  skip_on_cran()
  fit <- nomo_cfa(missing_demo_model, data = nomo_demo_continuous)

  local({
    local_mocked_bindings(nomo_reliability = function(...) stop("simulated failure"))
    out <- nomo_missing(fit, data = nomo_demo_continuous)
    expect_identical(nrow(out$reliability), 0L)
  })

  local({
    local_mocked_bindings(
      nomo_reliability = function(...) list(omega = tibble::tibble(), alpha = tibble::tibble())
    )
    out <- nomo_missing(fit, data = nomo_demo_continuous)
    expect_identical(nrow(out$reliability), 0L)
  })
})


# Pre-RC fixes (#145) ----------------------------------------------------------

# Text as printed, with line breaks and runs of spaces read as one space.
flat_text <- function(x) gsub("[[:space:]]+", " ", paste(x, collapse = " "))


test_that("a strategy lavaan substituted is labeled and attributed by what ran (#145)", {
  skip_on_cran()
  uls <- nomo_missing(
    nomo_cfa(missing_demo_model, data = nomo_demo_continuous, estimator = "ULS"),
    data = nomo_demo_continuous, reliability = FALSE
  )
  row <- uls$strategies[uls$strategies$strategy == "ml", ]
  skip_if_not(isTRUE(row$available), "this lavaan refuses FIML with ULS")
  expect_identical(row$lavaan_missing, "two.stage")
  expect_identical(row$label, "Two-stage ML (requested FIML)")

  local_reproducible_output(width = 80)
  printed <- capture.output(print(uls))
  expect_match(printed, "Reference: Two-stage ML (requested FIML)", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("the FIML estimate", printed, fixed = TRUE)))
  expect_true(any(grepl("ML -- Maximum likelihood.", printed, fixed = TRUE)))

  # A difference from it is attributed to no strategy, whether or not it is
  # flagged: FIML was never fitted.
  log <- uls$decision_log
  difference <- log[log$metric %in% c("estimate_difference", "estimates_agree"), ]
  expect_match(difference$observation, "two-stage ML (requested FIML) estimate", fixed = TRUE)
  expect_false(grepl("FIML is consistent", difference$recommendation, fixed = TRUE))
})


test_that("each difference is attributed by the method lavaan used for the reference (#145)", {
  out <- nomo_missing(nomo_cfa(missing_demo_model, data = nomo_demo_continuous),
                      data = nomo_demo_continuous, reliability = FALSE)
  flagged <- out$estimates
  comparison <- flagged$role == "comparison"
  flagged$difference_in_se[comparison] <- 2
  flagged$beyond_half_se[comparison] <- TRUE
  difference_log <- function(strategies) {
    nomologR:::nomo_missing_difference_log(
      nomologR:::nomo_log_new(), flagged, strategies, "ml",
      nomologR:::nomo_missing_inline(nomologR:::nomo_missing_table_label("ml", strategies)), "cfa"
    )
  }

  # FIML ran: the difference estimates listwise deletion's bias, with the
  # half-SE rule worded as the adaptation it is.
  entry <- difference_log(out$strategies)
  expect_match(entry$recommendation, "FIML is consistent", fixed = TRUE)
  expect_match(entry$recommendation, "sampling variability alone can exceed when data are MCAR",
               fixed = TRUE)
  expect_match(entry$reference, "adapted from Schafer & Graham (2002)", fixed = TRUE)
  expect_match(entry$observation, "11 of 11 estimates under listwise deletion differ from",
               fixed = TRUE)

  # Two-stage ML ran in its place: no attribution to FIML.
  substituted <- out$strategies
  substituted$lavaan_missing[substituted$strategy == "ml"] <- "two.stage"
  substituted$label <- nomologR:::nomo_missing_engine_label(substituted$strategy,
                                                            substituted$lavaan_missing)
  entry <- difference_log(substituted)
  expect_match(entry$recommendation, "depends on the choice of strategy", fixed = TRUE)
  expect_match(entry$observation, "the two-stage ML (requested FIML) estimate", fixed = TRUE)

  # One estimate that differs takes the singular.
  one <- flagged[flagged$parameter == "A ~~ B", ]
  entry <- nomologR:::nomo_missing_difference_log(
    nomologR:::nomo_log_new(), one, out$strategies, "ml", "FIML", "cfa"
  )
  expect_match(entry$observation, "1 of 1 estimate under listwise deletion differs from",
               fixed = TRUE)

  # A substituted reference is named by what lavaan used.
  log <- nomologR:::nomo_missing_reference_log(
    nomologR:::nomo_log_new(), "ml", "pairwise", "nomo_cfa", engine = "listwise"
  )
  expect_match(log$observation, "listwise deletion (requested pairwise deletion) is the reference",
               fixed = TRUE)
  expect_match(log$recommendation, "no longer", fixed = TRUE)
})


test_that("the print defines its terms and puts flags in one Flagged section (#145)", {
  skip_on_cran()
  out <- missing_results()$mar
  local_reproducible_output(width = 80)
  printed <- capture.output(print(out))
  text <- gsub("[[:space:]]+", " ", paste(printed, collapse = " "))

  expect_identical(printed[[2L]], "Model: CFA | Reference: FIML | Fitted with: Listwise deletion")
  expect_match(text, "Largest differences from the reference (standardized estimates)",
               fixed = TRUE)
  for (term in c("MCAR, missing completely at random", "MAR, missing at random",
                 "FIML -- Full-information maximum likelihood.",
                 "Covariance coverage -- Proportion of cases with both variables",
                 "Difference (SE) -- The estimate minus")) {
    expect_match(text, term, fixed = TRUE)
  }
  expect_false(grepl("For review", text, fixed = TRUE))
  expect_match(printed, "^  - Estimates \\(Review\\): [0-9]+ of [0-9]+ estimates under", all = FALSE)
  # A correlation has no leading zero, a loading keeps it, and both have three
  # decimals, as the log gives them.
  expect_match(printed, "^  A ~~ B +Listwise deletion +[.][0-9]{3} +[.][0-9]{3} +[-+][0-9]",
               all = FALSE)
  expect_match(printed, "^  [AB] =~ [ab][0-9] +Listwise deletion +0[.][0-9]{3} +0[.][0-9]{3} ",
               all = FALSE)
  expect_false(any(nchar(printed) > 80L))
  expect_match(printed[[length(printed)]], "for every recorded decision.", fixed = TRUE)
})


test_that("summary() shows every comparison and each recommendation (#145)", {
  skip_on_cran()
  out <- missing_results()$mar
  s <- summary(out)
  expect_identical(class(s), c("summary_nomo_missing", "list"))
  expect_identical(s$estimates, out$estimates)
  expect_null(s$fits)

  local_reproducible_output(width = 80)
  printed <- capture.output(returned <- print(s))
  expect_identical(returned, s)
  expect_identical(printed[[1L]], "<nomo_missing summary> Missing-data sensitivity")
  for (section in c("Missing values by variable", "Strategies", "Fit by strategy",
                    "Differences from the reference (standardized estimates)",
                    "Reliability by strategy", "Flagged", "Notes")) {
    expect_true(section %in% printed, label = section)
  }
  # Every comparison, not only the five largest.
  compared <- sum(out$estimates$role == "comparison")
  table_rows <- grep("^  [AB] (=~|~~) ", printed, value = TRUE)
  expect_identical(length(table_rows), compared)
  # The flag's recommendation, which print() leaves out.
  expect_match(gsub("[[:space:]]+", " ", paste(printed, collapse = " ")),
               "Report which strategy the results rest on", fixed = TRUE)
  expect_true(any(grepl("CFI -- Comparative fit index.", printed, fixed = TRUE)))
  expect_false(any(nchar(printed) > 80L))

  # Nothing missing: the summary says so, with no comparison to show.
  complete <- stats::na.omit(nomo_demo_continuous)
  none <- summary(nomo_missing(nomo_cfa(missing_demo_model, data = complete), data = complete,
                               reliability = FALSE))
  shown <- capture.output(print(none))
  expect_true(any(grepl("Lowest covariance coverage: 1.00$", shown)))
  expect_false("Missing values by variable" %in% shown)
  expect_false("Flagged" %in% shown)
  expect_true(any(grepl("None of the 473 cases is missing a modeled variable.", shown,
                        fixed = TRUE)))

  # A network has no reliability and is named as one.
  mar <- missing_samples()$mar
  net <- nomo_network(paste(missing_model, "B ~ A", sep = "\n"), data = mar,
                      hypotheses = nomo_hypotheses("A -> B" = positive()), std.lv = TRUE)
  network <- capture.output(print(summary(nomo_missing(net, data = mar))))
  expect_match(network[[2L]], "^Model: Network", all = FALSE)
  expect_false("Reliability by strategy" %in% network)
  expect_false(any(grepl("CFA -- ", network, fixed = TRUE)))
})


test_that("with several comparison strategies, each has a table of its own at any width (#145)", {
  skip_on_cran()
  fit <- nomo_cfa(missing_demo_model, data = nomo_demo_continuous)
  out <- nomo_missing(fit, data = nomo_demo_continuous,
                      strategies = c("listwise", "pairwise", "fiml"))
  differences <- out$estimates[out$estimates$role == "comparison", ]
  reliability <- out$reliability[out$reliability$role == "comparison", ]

  for (width in c(80L, 40L)) {
    local_reproducible_output(width = width)
    printed <- capture.output(print(out))
    detail <- capture.output(print(summary(out)))
    expect_false(any(nchar(c(printed, detail)) > width), label = paste("width", width))

    # Each strategy heads its own rows, in the order of the strategies table,
    # and no table needs a Strategy column to tell them apart.
    for (shown in list(printed, detail)) {
      expect_identical(grep("^  (Listwise|Pairwise) deletion$", shown, value = TRUE)[1:2],
                       c("  Listwise deletion", "  Pairwise deletion"))
      expect_false(any(grepl("Strategy +Estimate", shown)))
    }
    # In the summary, every difference and every coefficient sits under its
    # strategy: listwise rows before the pairwise heading, pairwise rows after.
    part <- function(from, to) {
      detail[seq(grep(from, detail)[[1L]], grep(to, detail)[[1L]])]
    }
    for (table in list(
      list(lines = part("^Differences from the reference", "^Reliability by strategy$"),
           row = "^    [AB] (=~|~~) ", strategy = differences$strategy),
      list(lines = part("^Reliability by strategy$", "^What these terms mean$"),
           row = "^    [AB] +(omega|alpha) ", strategy = reliability$strategy)
    )) {
      at <- grep("^  (Listwise|Pairwise) deletion$", table$lines)
      rows <- grep(table$row, table$lines)
      expect_length(at, 2L)
      expect_identical(length(rows), length(table$strategy))
      expect_identical(sum(rows > at[[1L]] & rows < at[[2L]]),
                       sum(table$strategy == "listwise"))
      expect_identical(sum(rows > at[[2L]]), sum(table$strategy == "pairwise"))
    }
  }

  # At 40 columns the difference is still shown for each strategy's rows.
  local_reproducible_output(width = 40)
  printed <- capture.output(print(out))
  expect_match(printed, "^    Parameter +Estimate +Difference \\(SE\\)$", all = FALSE)
  expect_match(printed, "^    B =~ b3 +0[.]772 +[+]0[.]41$", all = FALSE)

  # With one comparison strategy, one table names it in a column.
  local_reproducible_output(width = 80)
  one <- capture.output(print(missing_results()$mar))
  expect_false(any(grepl("^  Listwise deletion$", one)))
  expect_match(one, "^  Parameter +Strategy +Estimate +Reference +Difference \\(SE\\)$",
               all = FALSE)
})


test_that("plot() draws each difference against half a reference standard error (#145)", {
  out <- nomo_missing(nomo_cfa(missing_demo_model, data = nomo_demo_continuous),
                      data = nomo_demo_continuous, reliability = FALSE)
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  expect_identical(nrow(p$data), sum(out$estimates$role == "comparison"))
  # Nothing is flagged, so every point has no flag and no legend is needed.
  expect_true(all(as.character(p$data$status) == "none"))
  built <- ggplot2::ggplot_build(p)
  expect_equal(sort(built$data[[2L]]$xintercept), c(-.5, .5))
  expect_match(flat_text(p$labels$subtitle), "compared with FIML.", fixed = TRUE)
  expect_match(flat_text(p$labels$caption), "adapted from Schafer and Graham (2002)", fixed = TRUE)
  expect_error(plot(out, type = "estimates"), "`type` must be one of", fixed = TRUE)

  # Two comparison strategies are drawn in a panel each, with a flag marked.
  two <- out
  extra <- out$estimates[out$estimates$role == "comparison", ]
  extra$strategy <- "pairwise"
  extra$difference_in_se <- extra$difference_in_se * 3
  extra$beyond_half_se <- abs(extra$difference_in_se) > .5
  two$estimates <- rbind(out$estimates, extra)
  added <- out$strategies[out$strategies$strategy == "listwise", ]
  added$strategy <- "pairwise"
  added$label <- "Pairwise deletion"
  two$strategies <- rbind(out$strategies, added)
  p <- plot(two)
  expect_s3_class(p$facet, "FacetWrap")
  expect_true("review" %in% as.character(p$data$status))

  # With nothing compared there is nothing to draw.
  complete <- stats::na.omit(nomo_demo_continuous)
  none <- nomo_missing(nomo_cfa(missing_demo_model, data = complete), data = complete,
                       reliability = FALSE)
  expect_error(plot(none), "No difference from the reference is available to plot",
               fixed = TRUE)
})
