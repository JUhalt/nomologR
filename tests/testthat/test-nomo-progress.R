# Long steps say that they have started (#183) ---------------------------------
#
# Polychoric parallel analysis and a guided run with robust estimation took
# tens of minutes with no output, which reads as a stalled session. These are
# ordinary messages: suppressMessages() silences them.

# The Big Five items the audit used, as psych ships them.
psych_bfi <- function() {
  shipped <- utils::data(package = "psych")$results[, "Item"]
  testthat::skip_if_not("bfi" %in% shipped, "psych no longer ships bfi")
  env <- new.env()
  utils::data("bfi", package = "psych", envir = env)
  env$bfi
}


test_that("polychoric parallel analysis says what it will compute (#183)", {
  skip_on_cran()
  # Five of the demo's ordered items, which nomo_factors() models as ordinal.
  small <- nomo_demo_ordinal[1:200, paste0("a", 1:5)]
  expect_message(
    out <- nomo_factors(small, n_iter = 20, seed = 1),
    paste(
      "Parallel analysis with polychoric correlations: 20 null data sets for 5 items",
      "and 200 cases; this can take several minutes. Lower `n_iter` to shorten it."
    ),
    fixed = TRUE
  )
  expect_identical(out$correlation_method, "polychoric")

  # At the fewest null data sets `n_iter` allows there is nothing to lower.
  shown <- testthat::capture_messages(nomo_factors(small, n_iter = 10, seed = 1))
  expect_identical(shown, paste(
    "Parallel analysis with polychoric correlations: 10 null data sets for 5 items",
    "and 200 cases; this can take several minutes.\n"
  ))

  # Mixed items are named as such.
  mixed <- small
  mixed$a1 <- as.numeric(mixed$a1) + seq_len(nrow(mixed)) / 1000
  expect_message(
    nomo_factors(mixed, types = c(a1 = "continuous"), n_iter = 10, seed = 1),
    "Parallel analysis with mixed correlations: 10 null data sets",
    fixed = TRUE
  )

  # suppressMessages() silences it.
  expect_no_message(suppressMessages(nomo_factors(small, n_iter = 10, seed = 1)))
})


test_that("the parallel-analysis message fires on the audit's ordinal bfi items (#183)", {
  bfi <- psych_bfi()
  items <- c("A1", "A2", "A3", "A4", "A5")
  types <- stats::setNames(rep("ordinal", 5), items)
  expect_message(
    nomo_factors(bfi[1:300, ], items = items, types = types, n_iter = 10, seed = 1),
    "Parallel analysis with polychoric correlations: 10 null data sets for 5 items",
    fixed = TRUE
  )
})


test_that("Pearson and tetrachoric parallel analysis stay silent (#183)", {
  skip_on_cran()
  expect_no_message(nomo_factors(nomo_demo_continuous, n_iter = 10, seed = 1))
  expect_no_message(nomo_factors(nomo_demo_network, items = paste0("ag", 1:4),
                                 n_iter = 10, seed = 1))
  expect_no_message(nomo_factors(nomo_demo_walkthrough, items = paste0("EF", 1:6),
                                 n_iter = 10, seed = 1))
  expect_no_message(nomo_factors(nomo_demo_longitudinal, items = paste0("w", 1:4, "_t1"),
                                 n_iter = 10, seed = 1))
  # Binary items get tetrachoric correlations.
  binary <- as.data.frame(lapply(nomo_demo_ordinal, function(v) as.integer(as.integer(v) > 2L)))
  expect_no_message(out <- nomo_factors(binary, n_iter = 10, seed = 1))
  expect_identical(out$correlation_method, "tetrachoric")
  # Ordinal items stored as numbers are Pearson unless declared, as the bfi
  # items are used.
  bfi <- psych_bfi()
  expect_no_message(nomo_factors(bfi, items = names(bfi)[1:10], n_iter = 10, seed = 1))
})


test_that("a guided run says when each long step starts (#183)", {
  skip_on_cran()
  scales <- list(Agency = paste0("ag", 1:4), Persistence = paste0("pe", 1:4))
  shown <- testthat::capture_messages(
    run <- nomo_run(nomo_demo_network, scales,
                    settings = list(factors = list(n_iter = 10, seed = 2026)))
  )
  expect_identical(shown, c(
    "Estimating factor-retention evidence for `Agency`...\n",
    "Estimating factor-retention evidence for `Persistence`...\n"
  ))

  h <- nomo_hypotheses("Agency -> Persistence" = positive())
  shown <- testthat::capture_messages(
    run <- nomo_run(
      resume = run,
      decisions = list(
        factor_count = c(Agency = 1, Persistence = 1),
        cfa_model = nomo_model(scales),
        measurement_model = "proceed"
      ),
      settings = list(
        reliability = list(ci = "boot", ci_boot = 20, ci_seed = 1),
        invariance = list(group = "group", levels = c("configural", "metric")),
        network = list(hypotheses = h),
        missing = list()
      )
    )
  )
  expect_identical(run$status, "complete")
  expect_identical(shown, c(
    "Fitting the CFA model...\n",
    "Computing bootstrap intervals for reliability (20 resamples)...\n",
    "Refitting the measurement model under each missing-data strategy...\n",
    "Fitting the invariance models across `group` (2 groups)...\n",
    "Fitting the nomological network model...\n",
    "Refitting the network under each missing-data strategy...\n"
  ))

  # Reliability without bootstrap intervals takes moments and says nothing.
  shown <- testthat::capture_messages(
    nomo_run(nomo_demo_network, scales,
             decisions = list(factor_count = c(Agency = 1, Persistence = 1),
                              cfa_model = nomo_model(scales)),
             settings = list(factors = list(n_iter = 10, seed = 2026)))
  )
  expect_false(any(grepl("bootstrap", shown, fixed = TRUE)))
  expect_true("Fitting the CFA model...\n" %in% shown)
  expect_no_message(suppressMessages(
    nomo_run(nomo_demo_network, scales, settings = list(factors = list(n_iter = 10, seed = 2026)))
  ))
})
