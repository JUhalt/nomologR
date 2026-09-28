# Single-indicator latent variables (#129) ---------------------------------------

si_data <- local({
  dat <- nomo_demo_network
  dat$persistence <- rowMeans(dat[c("pe1", "pe2", "pe3", "pe4")])
  dat$sd_score <- rowMeans(dat[c("sd1", "sd2", "sd3")])
  dat
})
si_model <- "Agency =~ ag1 + ag2 + ag3 + ag4"
si_hypotheses <- nomo_hypotheses("Agency -> persistence" = positive(min = .20))

si_reliability <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- nomo_reliability(
        nomo_cfa("Persistence =~ pe1 + pe2 + pe3 + pe4", si_data),
        ci = "bootstrap", ci_boot = 100L, ci_seed = 1L
      )
    }
    cache
  }
})


# The record ----------------------------------------------------------------------

test_that("a reliability is recorded with its coefficient, standard error, and source", {
  si <- nomo_single_indicator(.85, se = .02, source = "Test manual, Table 4")
  expect_s3_class(si, "nomo_single_indicator")
  expect_identical(si$reliability, .85)
  expect_identical(si$se, .02)
  expect_identical(si$coefficient, "omega")
  expect_identical(si$source, "Test manual, Table 4")
  expect_true(is.na(si$construct))

  bare <- nomo_single_indicator(.7, coefficient = "alpha")
  expect_true(is.na(bare$se))
  expect_true(is.na(bare$source))
  expect_identical(bare$coefficient, "alpha")

  local_reproducible_output(width = 80)
  printed <- capture.output(print(si))
  expect_match(printed, "Reliability: 0.850 (omega) | SE: 0.020", fixed = TRUE, all = FALSE)
  expect_match(printed, "Source: Test manual, Table 4", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("Source", capture.output(print(bare)), fixed = TRUE)))
})


test_that("a reliability must be a proportion, and its companions well formed", {
  for (bad in list(0, 1, 1.2, -0.1, NA_real_, c(.8, .9), "0.8")) {
    expect_error(nomo_single_indicator(bad), "strictly between 0 and 1")
  }
  expect_error(nomo_single_indicator(.8, se = 0), "`se` must be NULL or one positive number")
  expect_error(nomo_single_indicator(.8, se = c(.01, .02)), "`se` must be NULL")
  expect_error(nomo_single_indicator(.8, source = ""), "`source` must be NULL")
  expect_error(nomo_single_indicator(.8, construct = "A"), "used only when `reliability`")
  expect_error(nomo_single_indicator(.8, coefficient = "kr20"), "`coefficient` must be one of")
})


test_that("omega and its bootstrap uncertainty come from a reliability result", {
  rel <- si_reliability()
  si <- nomo_single_indicator(rel)
  omega <- rel$omega[rel$omega$construct == "Persistence", ]
  expect_identical(si$reliability, omega$estimate)
  expect_equal(si$se, (omega$ci_upper - omega$ci_lower) / (2 * stats::qnorm(.975)))
  expect_identical(si$construct, "Persistence")
  expect_identical(si$source, "this sample (nomo_reliability())")
  expect_identical(nomo_single_indicator(rel, se = .05)$se, .05)
  expect_identical(nomo_single_indicator(rel, source = "Own CFA")$source, "Own CFA")
  local_reproducible_output(width = 80)
  expect_match(capture.output(print(si)), "Construct: Persistence", fixed = TRUE, all = FALSE)

  # Without a level recorded, the interval is read as 95%.
  no_level <- rel
  no_level$ci_level <- NULL
  expect_equal(nomo_single_indicator(no_level)$se, si$se)

  # Without an interval, there is no standard error to take.
  point <- nomo_reliability(nomo_cfa("Persistence =~ pe1 + pe2 + pe3 + pe4", si_data))
  expect_true(is.na(nomo_single_indicator(point)$se))

  expect_error(nomo_single_indicator(rel, coefficient = "alpha"), "supplies omega")
})


test_that("a reliability result with several constructs needs one named", {
  skip_on_cran()
  two <- nomo_reliability(nomo_cfa(
    "Agency =~ ag1 + ag2 + ag3 + ag4\nPersistence =~ pe1 + pe2 + pe3 + pe4", si_data
  ))
  expect_error(nomo_single_indicator(two), "Name the `construct`")
  expect_error(nomo_single_indicator(two, construct = "Nope"), "must be one of")
  expect_identical(nomo_single_indicator(two, construct = "Agency")$construct, "Agency")

  broken <- two
  broken$omega$estimate[broken$omega$construct == "Agency"] <- NA_real_
  expect_error(nomo_single_indicator(broken, construct = "Agency"), "not a single estimate")
})


# In the network --------------------------------------------------------------------

test_that("a composite modeled as a single indicator is corrected for its unreliability", {
  skip_on_cran()
  plain <- nomo_network(si_model, si_data, si_hypotheses)
  rho <- .8
  net <- nomo_network(si_model, si_data, si_hypotheses,
                      single_indicators = c(persistence = rho))

  # A latent predictor and a single-indicator outcome: the standardized path is
  # the composite's correlation over the square root of its reliability.
  expect_equal(net$hypothesis_evidence$estimate,
               plain$hypothesis_evidence$estimate / sqrt(rho), tolerance = 2e-3)
  expect_gt(net$hypothesis_evidence$estimate, plain$hypothesis_evidence$estimate)
  expect_identical(net$hypothesis_evidence$target_type, "latent")
  expect_identical(net$hypothesis_evidence$se_reliability_added, 0)

  si <- net$single_indicators
  expect_identical(si$variable, "persistence")
  expect_identical(si$indicator, "persistence_si")
  expect_identical(si$coefficient, "unspecified")
  expect_equal(si$variance, stats::var(si_data$persistence))
  expect_equal(si$error_variance, (1 - rho) * si$variance)
  expect_match(net$model_fitted, "persistence =~ persistence_si", fixed = TRUE)
  expect_identical(net$model_original, si_model)

  log <- net$decision_log
  entry <- log[log$metric == "single_indicator", ]
  expect_identical(entry$severity, "info")
  expect_match(entry$observation, "coefficient not stated; source not stated", fixed = TRUE)
  known <- log[log$metric == "single_indicator_uncertainty", ]
  expect_match(known$observation, "treat the reliability of `persistence` as known", fixed = TRUE)
  # The composite is latent now, so no observed endpoint is disclosed for it.
  expect_false("mixed_endpoints" %in% log$metric)
  expect_true("single_indicator_reliability" %in% nomo_methods(net)$id)
})


test_that("each hypothesis is refitted across the reliability, and a change is flagged", {
  skip_on_cran()
  h <- nomo_hypotheses("Agency -> persistence" = positive(min = .40))
  net <- nomo_network(si_model, si_data, h, single_indicators = c(persistence = .8))

  s <- nomo_table(net, "sensitivity")
  expect_identical(s$shift, c(-0.10, -0.05, 0, 0.05, 0.10))
  expect_equal(s$reliability, .8 + s$shift)
  # A higher reliability corrects less.
  expect_true(all(diff(s$estimate) < 0))
  expect_gt(length(unique(s$concordance)), 1L)

  log <- net$decision_log
  flagged <- log[log$metric == "single_indicator_sensitivity", ]
  expect_identical(flagged$severity, "review")
  expect_match(flagged$observation, "The evidence depends on the reliability assumed for `persistence`: H1 is",
               fixed = TRUE)
  local_reproducible_output(width = 80)
  summarized <- capture.output(print(summary(net)))
  expect_match(summarized, "changes$", all = FALSE)
})


test_that("a reliability near 1 is shifted only within (0, 1)", {
  skip_on_cran()
  net <- nomo_network(si_model, si_data, si_hypotheses,
                      single_indicators = list(persistence = nomo_single_indicator(.97, se = .01)))
  s <- net$single_indicator_sensitivity
  expect_identical(s$shift, c(-0.10, -0.05, 0))
  # One side of the reliability is enough for the rate of change.
  expect_gt(net$hypothesis_evidence$se_reliability_added, 0)
  flagged <- net$decision_log[net$decision_log$metric == "single_indicator_sensitivity", ]
  expect_match(flagged$observation, "Across reliabilities from 0.870 to 0.970", fixed = TRUE)
  # Shifts past 1 are left empty in the summary.
  wide <- nomologR:::nomo_network_sensitivity_wide(s)
  expect_true(is.na(wide$plus_05) && is.na(wide$plus_10))
  expect_identical(wide$concordance, "unchanged")
})


test_that("a reliability's standard error widens the interval by Oberski and Satorra's term", {
  skip_on_cran()
  rho <- .8
  se <- .03
  known <- nomo_network(si_model, si_data, si_hypotheses, single_indicators = c(persistence = rho))
  uncertain <- nomo_network(si_model, si_data, si_hypotheses,
                            single_indicators = list(persistence = nomo_single_indicator(rho, se = se)))
  k <- known$hypothesis_evidence
  u <- uncertain$hypothesis_evidence

  expect_equal(u$estimate, k$estimate)
  # The standardized path is r / sqrt(rho), whose rate of change in rho is
  # -estimate / (2 rho).
  expected <- abs(k$estimate / (2 * rho)) * se
  expect_equal(u$se_reliability_added, expected, tolerance = .02)
  expect_equal(u$se, sqrt(k$se^2 + u$se_reliability_added^2))
  expect_equal(u$ci_upper - u$ci_lower, 2 * stats::qnorm(.975) * u$se)
  expect_gt(u$ci_upper - u$ci_lower, k$ci_upper - k$ci_lower)

  log <- uncertain$decision_log
  added <- log[log$metric == "single_indicator_uncertainty", ]
  expect_match(added$observation, "add the uncertainty in the reliability of `persistence` (standard error 0.030)",
               fixed = TRUE)
})


test_that("alpha is flagged, since it overcorrects when loadings differ", {
  skip_on_cran()
  net <- nomo_network(si_model, si_data, si_hypotheses, single_indicators = list(
    persistence = nomo_single_indicator(.75, coefficient = "alpha", source = "Pilot study")
  ))
  log <- net$decision_log
  alpha <- log[log$metric == "single_indicator_alpha", ]
  expect_identical(alpha$severity, "review")
  expect_match(alpha$observation, "overcorrected", fixed = TRUE)
  entry <- log[log$metric == "single_indicator", ]
  expect_match(entry$observation, "(alpha; source: Pilot study)", fixed = TRUE)
})


test_that("the validation sample is corrected with its own variance and uncertainty", {
  skip_on_cran()
  s <- nomo_split(si_data, validation_prop = .5, seed = 11)
  uncertain <- nomo_network(si_model, s, si_hypotheses,
                            single_indicators = list(persistence = nomo_single_indicator(.8, se = .03)))
  v <- uncertain$validation$hypothesis_evidence
  expect_gt(v$se_reliability_added, 0)
  expect_identical(nrow(uncertain$replication_evidence), 1L)

  known <- nomo_network(si_model, s, si_hypotheses, single_indicators = c(persistence = .8))
  expect_identical(known$validation$hypothesis_evidence$se_reliability_added, 0)
  expect_equal(known$validation$hypothesis_evidence$estimate, v$estimate)
})


test_that("the indicator's name never collides with a column", {
  skip_on_cran()
  dat <- si_data
  dat$persistence_si <- 0
  net <- nomo_network(si_model, dat, si_hypotheses, single_indicators = c(persistence = .8))
  expect_identical(net$single_indicators$indicator, "persistence_si2")
})


test_that("single indicators are checked against the model and the data", {
  skip_on_cran()
  fit <- function(single, data = si_data, ...) {
    nomo_network(si_model, data, si_hypotheses, single_indicators = single, ...)
  }
  expect_error(fit(list(.8)), "must be a named list")
  expect_error(fit(list()), "must be a named list")
  expect_error(fit(list(persistence = .8, persistence = .7)), "must be a named list")
  expect_error(fit(c(Agency = .8)), "a latent variable in `model`")
  expect_error(fit(c(sd_score = .8)), "which `model` does not use")
  expect_error(fit(list(persistence = "high")), "must be a reliability or a `nomo_single_indicator()` record",
               fixed = TRUE)
  expect_error(fit(c(persistence = 1.1)), "strictly between 0 and 1")

  words <- si_data
  words$persistence <- as.character(words$persistence)
  expect_error(fit(c(persistence = .8), data = words), "must be numeric")

  flat <- si_data
  flat$persistence <- 3
  expect_error(fit(c(persistence = .8), data = flat), "does not vary")

  s <- nomo_split(si_data, validation_prop = .5, seed = 11)
  s$validation$persistence <- as.character(s$validation$persistence)
  expect_error(fit(c(persistence = .8), data = s), "must be numeric")

  ordinal <- si_data
  ordinal$persistence <- as.integer(cut(ordinal$persistence, 4))
  expect_error(
    nomo_network(si_model, ordinal, si_hypotheses, ordered = "persistence",
                 single_indicators = c(persistence = .8)),
    "is declared ordered"
  )
})


test_that("a refit that fails is recorded as not evaluable, and costs no rate of change", {
  base <- list(hypothesis_evidence = tibble::tibble(
    id = "H1", estimate = .4, ci_lower = .3, ci_upper = .5, concordance = "concordant"
  ))
  spec <- tibble::tibble(variable = "x", reliability = .8, se = .02,
                         coefficient = "omega", source = NA_character_)
  out <- nomologR:::nomo_network_single_sensitivity(
    spec, base, refit = function(spec) stop("no fit")
  )
  expect_identical(out$table$concordance, c(rep("not_evaluable", 2L), "concordant",
                                            rep("not_evaluable", 2L)))
  expect_identical(out$extra[["H1"]], 0)

  log <- nomologR:::nomo_network_single_log(
    tibble::tibble(variable = "x", indicator = "x_si", reliability = .8, se = .02,
                   coefficient = "omega", source = NA_character_, n = 10L,
                   variance = 1, error_variance = .2),
    out$table
  )
  flagged <- log[log$metric == "single_indicator_sensitivity", ]
  expect_match(flagged$observation, "not evaluable at 0.700, 0.750, 0.850, 0.900", fixed = TRUE)
})


test_that("single indicators are shown in print, summary, tables, and the APA note", {
  skip_on_cran()
  h <- nomo_hypotheses(
    "Agency -> persistence" = positive(min = .20),
    "Agency <-> sd_score" = negligible(within = c(-.15, .15))
  )
  net <- nomo_network(si_model, si_data, h, single_indicators = list(
    persistence = nomo_single_indicator(.8, se = .03),
    sd_score = .62
  ))
  local_reproducible_output(width = 80)
  printed <- capture.output(print(net))
  expect_match(printed, "Single indicator: persistence (reliability 0.800, omega)",
               fixed = TRUE, all = FALSE)
  summarized <- capture.output(print(summary(net)))
  expect_true(any(grepl("Sensitivity to the reliability", summarized, fixed = TRUE)))
  expect_true(any(grepl("Composite    ID   -.10", summarized, fixed = TRUE)))
  expect_false(any(nchar(summarized) > 80L))

  expect_identical(nomo_table(net, "single_indicators"), net$single_indicators)
  expect_identical(nomo_table(net, "sensitivity"), net$single_indicator_sensitivity)

  note <- paste(nomo_apa_table(net)$notes$general, collapse = " ")
  expect_match(note, "persistence and sd_score were modeled as single-indicator latent variables",
               fixed = TRUE)
  expect_match(note, "persistence = .80, omega; sd_score = .62)", fixed = TRUE)

  one <- nomo_network(si_model, si_data, si_hypotheses, single_indicators = c(persistence = .8))
  note <- paste(nomo_apa_table(one)$notes$general, collapse = " ")
  expect_match(note, "persistence was modeled as a single-indicator latent variable, with its error variance",
               fixed = TRUE)
})


test_that("networks saved before single indicators still print and tabulate", {
  skip_on_cran()
  net <- nomo_network(si_model, si_data, si_hypotheses)
  expect_identical(nrow(net$single_indicators), 0L)
  expect_identical(nrow(net$single_indicator_sensitivity), 0L)
  old <- net
  old$single_indicators <- NULL
  old$single_indicator_sensitivity <- NULL
  local_reproducible_output(width = 80)
  expect_no_warning(capture.output(print(old), print(summary(old))))
  expect_null(nomo_table(old, "single_indicators"))
  expect_false(grepl("single-indicator", paste(nomo_apa_table(old)$notes$general, collapse = " ")))
})


test_that("prose lists join with and as well as or", {
  expect_identical(nomologR:::nomo_present_or(c("a", "b"), "and"), "a and b")
  expect_identical(nomologR:::nomo_present_or(c("a", "b", "c"), "and"), "a, b, and c")
  expect_identical(nomologR:::nomo_present_or(c("a", "b", "c")), "a, b, or c")
})
