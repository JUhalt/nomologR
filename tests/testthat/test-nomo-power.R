# Power and sample size (#129) ----------------------------------------------------

test_that("RMSEA power reproduces MacCallum, Browne, and Sugawara's sample sizes", {
  # Their Table 4, for power .80 at alpha .05: 132 for close fit and 178 for
  # not-close fit at 100 degrees of freedom.
  expect_identical(nomo_power_rmsea(df = 100)$n_required, 132L)
  expect_identical(nomo_power_rmsea(df = 100, test = "not_close")$n_required, 178L)

  close <- nomo_power_rmsea(df = 20, n = c(100, 400, 200))
  expect_identical(close$power$n, c(100L, 200L, 400L))
  expect_true(all(diff(close$power$power) > 0))
  # The power at the required N reaches the target, and one fewer does not.
  req <- nomo_power_rmsea(df = 20)
  expect_gte(req$power$power, .80)
  expect_lt(nomo_power_rmsea(df = 20, n = req$n_required - 1L)$power$power, .80)

  local_reproducible_output(width = 80)
  printed <- capture.output(print(close))
  # The smallest N is a line of its own under the header, and power has two
  # decimals, as in nomo_power_simulate()'s print (#89).
  expect_identical(printed[[3L]], sprintf("Smallest N for power 0.80: %d", req$n_required))
  expect_true(any(grepl("Power by sample size", printed, fixed = TRUE)))
  expect_true(any(grepl("^  100 +0[.][0-9]{2}$", printed)))
  expect_true(all(nchar(printed) <= 80))

  exact <- nomo_power_rmsea(df = 20, test = "exact")
  expect_identical(c(exact$rmsea_null, exact$rmsea_alt), c(0, .05))
  expect_true("rmsea_power" %in% nomo_methods(exact)$id)
})


test_that("RMSEA power has the table and summary every result object has (#145)", {
  close <- nomo_power_rmsea(df = 20, n = c(100, 200, 400))

  # One table, so one type, which is also the default.
  expect_identical(nomo_table(close), close$power)
  expect_identical(nomo_table(close, "power"), close$power)
  expect_error(nomo_table(close, "parameters"),
               '`type` must be one of "power", not "parameters".', fixed = TRUE)

  s <- summary(close)
  expect_identical(class(s), c("summary_nomo_power", "list"))
  expect_identical(s$power, close$power)
  local_reproducible_output(width = 80)
  printed <- capture.output(returned <- print(s))
  expect_identical(returned, s)
  expect_identical(printed[[1L]], "<nomo_power summary> Power of the test of close fit")
  # A summary of the RMSEA tests has nothing to add to what print() shows.
  expect_identical(printed[-1L], capture.output(print(close))[-1L])
})


test_that("the degrees of freedom come from the model however it is given", {
  model <- nomo_model(list(A = paste0("a", 1:4), B = paste0("b", 1:4)))
  expect_identical(nomo_power_rmsea(model)$df, 19L)
  expect_identical(nomo_power_rmsea("A =~ a1 + a2 + a3\nB =~ b1 + b2 + b3\nA ~~ 0*B")$df, 9L)
  # An exogenous observed predictor, which lavaan treats as fixed.
  expect_identical(
    nomo_power_rmsea("A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3\nB ~ A + x1")$df, 19L
  )
  skip_on_cran()
  cfa <- nomo_cfa("Agency =~ ag1 + ag2 + ag3 + ag4", nomo_demo_network)
  expect_identical(nomo_power_rmsea(cfa)$df, 2L)
  expect_identical(nomo_power_rmsea(cfa$fit)$df, 2L)
})


test_that("the search reports small, unreachable, and immediate targets", {
  # A negligible difference in RMSEA is not detectable with a million cases.
  never <- nomo_power_rmsea(df = 1, rmsea_null = .05, rmsea_alt = .05001)
  expect_true(is.na(never$n_required))
  expect_identical(nrow(never$power), 0L)
  local_reproducible_output(width = 80)
  expect_match(capture.output(print(never)),
               "Smallest N for power 0.80: none up to one million", fixed = TRUE,
               all = FALSE)
  # A target below alpha is met by the smallest sample.
  expect_identical(nomo_power_rmsea(df = 10, power = .01)$n_required, 2L)
})


test_that("RMSEA power arguments are checked", {
  expect_error(nomo_power_rmsea(), "Give `model` or `df`")
  expect_error(nomo_power_rmsea("A =~ a1 + a2 + a3", df = 5), "Give `model` or `df`")
  expect_error(nomo_power_rmsea(df = 0), "positive whole number")
  expect_error(nomo_power_rmsea(df = 2.5), "positive whole number")
  expect_error(nomo_power_rmsea(model = list()), "must be a lavaan model string")
  expect_error(nomo_power_rmsea(df = 10, power = 1), "`power` must be")
  expect_error(nomo_power_rmsea(df = 10, alpha = 0), "`alpha` must be")
  expect_error(nomo_power_rmsea(df = 10, rmsea_null = -1), "non-negative")
  expect_error(nomo_power_rmsea(df = 10, rmsea_alt = .03), "above the null")
  expect_error(nomo_power_rmsea(df = 10, test = "not_close", rmsea_alt = .08), "below it")
  expect_error(nomo_power_rmsea(df = 10, n = 1), "at least 2")
  expect_error(nomo_power_rmsea(df = 10, test = "loose"), "`test` must be one of")
})


pw_population <- "
A =~ 0.7*a1 + 0.7*a2 + 0.6*a3 + 0.5*a4
B =~ 0.7*b1 + 0.6*b2 + 0.6*b3 + 0.5*b4
A ~~ 0.3*B
"


test_that("a Monte Carlo study reports recovery and power at each sample size", {
  skip_on_cran()
  pw <- nomo_power_simulate(pw_population, n = c(300, 60), reps = 12,
                            focus = "B ~~ A", seed = 11)
  expect_s3_class(pw, "nomo_power")
  expect_identical(pw$focus, "A~~B")
  expect_identical(pw$summary$n, c(60L, 300L))
  expect_identical(pw$analysis,
                   "\nA =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4\nA ~~ B\n")
  par <- pw$parameters
  expect_identical(nrow(par), 2L * 9L)
  ab <- par[par$parameter == "A~~B", ]
  expect_identical(ab$population, c(.3, .3))
  expect_equal(ab$relative_bias, (ab$mean_estimate - .3) / .3)
  expect_true(all(ab$power >= 0 & ab$power <= 1))
  expect_gt(ab$power[[2L]], ab$power[[1L]])
  expect_identical(pw$summary$converged, c(1, 1))
  expect_true("monte_carlo_power" %in% nomo_methods(pw)$id)
  # The seed and the call are recorded, so the study can be rerun.
  expect_identical(pw$seed, 11L)
  expect_true(is.call(pw$call))
  expect_identical(pw$call$seed, 11)

  # The same seed gives the same study, and the session's random state is kept.
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  again <- nomo_power_simulate(pw_population, n = c(300, 60), reps = 12,
                               focus = "A~~B", seed = 11)
  expect_identical(again$parameters, pw$parameters)
  expect_identical(stats::runif(1), before)

  local_reproducible_output(width = 80)
  printed <- capture.output(print(pw))
  expect_match(printed, "Replications: 12 per N", fixed = TRUE, all = FALSE)
  # Convergence, improper solutions, and biases are right-aligned percentages,
  # as the references for bias are (#89).
  expect_match(printed, "^ +60 +[0-9.]+% +[0-9.]+% +[0-9.]+ +[0-9.]+% +[0-9.]+% ", all = FALSE)
  expect_match(printed, "Max bias and Max SE bias are the largest absolute relative biases",
               fixed = TRUE, all = FALSE)
  expect_false(any(nchar(printed) > 80L))

  # The tables are reached through nomo_table(), by sample size by default
  # (#145), and a table cut for width names the call that shows the rest.
  expect_identical(nomo_table(pw), pw$summary)
  expect_identical(nomo_table(pw, "summary"), pw$summary)
  expect_identical(nomo_table(pw, "parameters"), pw$parameters)
  expect_error(nomo_table(pw, "power"),
               '`type` must be one of "summary" or "parameters", not "power".',
               fixed = TRUE)
  local_reproducible_output(width = 50)
  narrow <- gsub("[[:space:]]+", " ", paste(capture.output(print(pw)), collapse = " "))
  expect_match(narrow, "Not shown for width: ", fixed = TRUE)
  expect_match(narrow, "See nomo_table(x, \"summary\").", fixed = TRUE)
  expect_false(grepl("x$parameters", narrow, fixed = TRUE))

  # summary() adds the table print() leaves out: every parameter at every N.
  s <- summary(pw)
  expect_identical(class(s), c("summary_nomo_power", "list"))
  expect_identical(s$parameters, pw$parameters)
  local_reproducible_output(width = 80)
  detail <- capture.output(print(s))
  expect_identical(detail[[1L]], "<nomo_power summary> Monte Carlo power and sample size")
  expect_match(detail, "By sample size and parameter", fixed = TRUE, all = FALSE)
  expect_identical(sum(grepl("^ +[0-9]+ +A~~B ", detail)), 2L)
  # Biases are percentages, as they are by sample size.
  expect_match(detail, "^ +60 +A~~B +0[.]300 +-?[0-9.]+ +-?[0-9.]+% +-?[0-9.]+% +[0-9.]+ +[0-9.]+$",
               all = FALSE)
  expect_false(any(grepl("By sample size and parameter", printed, fixed = TRUE)))
  expect_false(any(nchar(detail) > 80L))
})


test_that("the simulation handles zero values, missing parameters, and failed fits", {
  skip_on_cran()
  population <- paste(pw_population, "A ~~ 0*C\nC =~ 0.6*c1 + 0.6*c2 + 0.6*c3", sep = "\n")
  pw <- nomo_power_simulate(population, n = 40, reps = 3, seed = 5,
                            analysis = "A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4\nA ~~ B")
  # Every nonzero parameter is in focus by default; a zero one has no
  # relative bias.
  expect_false("A~~C" %in% pw$focus)
  par <- pw$parameters
  expect_true(is.na(par$relative_bias[par$parameter == "A~~C"]))
  # Parameters the analysis model leaves out have no estimates.
  expect_true(all(is.nan(par$mean_estimate[par$parameter %in% c("C=~c1", "A~~C")])))
  expect_false(pw$summary$meets_references)

  failing <- nomo_power_simulate(pw_population, n = 20, reps = 2, seed = 5,
                                 analysis = "A =~ zz1 + zz2 + zz3")
  expect_identical(failing$summary$converged, 0)
  expect_true(is.na(failing$summary$improper))
  expect_true(is.na(failing$n_required))
  local_reproducible_output(width = 80)
  expect_match(capture.output(print(failing)), "No simulated N meets the references",
               fixed = TRUE, all = FALSE)

  # Without a seed, the session's random stream is used as it stands; without
  # any random state, one is created and removed again.
  old <- if (exists(".Random.seed", envir = .GlobalEnv)) get(".Random.seed", envir = .GlobalEnv)
  if (!is.null(old)) rm(".Random.seed", envir = .GlobalEnv)
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
  nomo_power_simulate(pw_population, n = 30, reps = 2, seed = 1)
  expect_false(exists(".Random.seed", envir = .GlobalEnv))
  unseeded <- nomo_power_simulate(pw_population, n = 30, reps = 2)
  expect_identical(unseeded$reps, 2L)
  expect_identical(unseeded$seed, NA_integer_)
})


test_that("a smallest N is reported when one meets the references", {
  skip_on_cran()
  pw <- nomo_power_simulate(pw_population, n = 40, reps = 3, seed = 3, focus = "A=~a1")
  pw$summary$meets_references <- TRUE
  pw$n_required <- 40L
  local_reproducible_output(width = 80)
  expect_match(capture.output(print(pw)), "Smallest simulated N meeting the references: 40.",
               fixed = TRUE, all = FALSE)
})


test_that("simulation arguments are checked", {
  expect_error(nomo_power_simulate("", n = 50), "one lavaan model string")
  expect_error(nomo_power_simulate(pw_population, n = 5), "at least 10")
  expect_error(nomo_power_simulate(pw_population, n = 50, reps = 1), "at least 2")
  expect_error(nomo_power_simulate(pw_population, n = 50, alpha = 2), "`alpha` must be")
  expect_error(nomo_power_simulate(pw_population, n = 50, standardized = NA), "TRUE or FALSE")
  expect_error(nomo_power_simulate(pw_population, n = 50, seed = 1.5), "one whole number")
  expect_error(nomo_power_simulate(pw_population, n = 50, focus = "A~~Z"), "without population values")
  expect_error(nomo_power_simulate(pw_population, n = 50, focus = NA_character_), "character vector")
  expect_error(nomo_power_simulate("A =~ a1 + a2 + a3", n = 50), "gives no parameter a value")
  expect_error(nomo_power_simulate("A =~~ a1", n = 50), "Could not parse `population`")
})
