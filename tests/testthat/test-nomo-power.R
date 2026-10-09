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
  # The smallest N is a line of its own under the header and its source, and
  # power has two decimals without a leading zero, as in
  # nomo_power_simulate()'s print (#89, #144).
  expect_identical(printed[[2L]], "MacCallum, Browne, and Sugawara (1996).")
  expect_identical(printed[[4L]], sprintf("Smallest N for power .80: %d", req$n_required))
  expect_true(any(grepl("Power by sample size", printed, fixed = TRUE)))
  expect_true(any(grepl("^  100 +[.][0-9]{2}$", printed)))
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
               "Smallest N for power .80: none up to one million", fixed = TRUE,
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
  # The samples of this test are drawn with base R (helper-fixtures.R), because
  # lavaan's generator gives different samples for one seed from one lavaan
  # version to the next. What is asserted below about power, convergence, and
  # coverage holds for these samples under every lavaan version. The tests
  # after this one draw with lavaan::simulateData() itself.
  local_base_r_simulate_data()
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

  # The same seed gives the same study, and the session's random-number state
  # is afterwards as it was, so the caller's next draw is the one it would
  # have had.
  expected <- withr::with_seed(99, stats::runif(1))
  withr::with_seed(99, {
    before <- nomo_test_rng_state()
    again <- nomo_power_simulate(pw_population, n = c(300, 60), reps = 12,
                                 focus = "A~~B", seed = 11)
    expect_identical(nomo_test_rng_state(), before)
    expect_identical(stats::runif(1), expected)
  })
  expect_identical(again$parameters, pw$parameters)

  local_reproducible_output(width = 80)
  printed <- capture.output(print(pw))
  expect_match(printed, "Replications: 12 per N", fixed = TRUE, all = FALSE)
  # Whether an N meets the references follows the stub, and convergence,
  # improper solutions, and biases are right-aligned percentages, as the
  # references for bias are (#89, #144).
  expect_match(printed, "^ +60 +(yes|no) +[0-9.]+% +[0-9.]+% +[0-9.]+ +[0-9.]+% +[0-9.]+% ",
               all = FALSE)
  expect_match(printed, "Max bias, Max SE bias -- Largest absolute relative bias",
               fixed = TRUE, all = FALSE)
  expect_false(any(nchar(printed) > 80L))
  # Coverage is a range, and a missing bound leaves the whole cell missing,
  # never half a range such as "---0.95" (#145).
  expect_match(printed, "^ +300 +(yes|no) .* [.][0-9]{2} to (1[.]00|[.][0-9]{2})$", all = FALSE)
  half <- pw
  half$summary$min_coverage[[1L]] <- NA_real_
  shown <- capture.output(print(half))
  expect_match(shown, "^ +60 +(yes|no) .* --$", all = FALSE)
  expect_false(any(grepl("---", shown, fixed = TRUE)))

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
  expect_match(detail, "^ +60 +A~~B +0[.]30 +-?[0-9.]+ +-?[0-9.]+% +-?[0-9.]+% +[0-9.]+ +[0-9.]+$",
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
  failed <- capture.output(print(failing))
  expect_match(failed, "No simulated N meets the references", fixed = TRUE, all = FALSE)
  # Without a converged replication there is no coverage to show (#145).
  expect_false(any(grepl("---", failed, fixed = TRUE)))

  # With a seed, the session's random-number state is afterwards as it was;
  # without one, the session's stream is used as it stands, so it moves on.
  # test-nomo-global-state.R covers a session that has no state.
  withr::with_seed(8103, {
    before <- nomo_test_rng_state()
    nomo_power_simulate(pw_population, n = 30, reps = 2, seed = 1)
    expect_identical(nomo_test_rng_state(), before)
    unseeded <- nomo_power_simulate(pw_population, n = 30, reps = 2)
    expect_false(identical(nomo_test_rng_state(), before))
  })
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


# Pre-RC fixes (#145) -------------------------------------------------------------

# Text as printed, with line breaks and runs of spaces read as one space.
flat_text <- function(x) gsub("[[:space:]]+", " ", paste(x, collapse = " "))


test_that("a multi-group fit is refused by nomo_power_rmsea() (#145)", {
  skip_on_cran()
  model <- "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9"
  grouped <- lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, group = "school")
  expect_error(nomo_power_rmsea(grouped),
               "`model` is a multi-group fit (2 groups)", fixed = TRUE)
  single <- lavaan::cfa(model, data = lavaan::HolzingerSwineford1939)
  expect_identical(nomo_power_rmsea(single)$df, 24L)
})


test_that("numbers beyond R's integer range get the package's own message (#145)", {
  expect_error(nomo_power_rmsea(df = 20, n = 3e9),
               "`n` must hold whole numbers of at least 2, within R's integer range.",
               fixed = TRUE)
  expect_error(nomo_power_rmsea(df = 3e9), "`df` must be one positive whole number",
               fixed = TRUE)
  expect_error(nomo_power_simulate(pw_population, n = 100, reps = 2, seed = 1e10),
               "`seed` must be NULL or one whole number within R's integer range.",
               fixed = TRUE)
  expect_error(nomo_power_simulate(pw_population, n = 3e9, reps = 2),
               "`n` must hold whole numbers of at least 10", fixed = TRUE)
  expect_error(nomo_power_simulate(pw_population, n = 100, reps = 1e10),
               "`reps` must be one whole number of at least 2", fixed = TRUE)
})


test_that("a null RMSEA must be the one the test is named for (#145)", {
  expect_error(nomo_power_rmsea(df = 20, test = "close", rmsea_null = 0, rmsea_alt = .05),
               "The test of close fit needs a null RMSEA above 0", fixed = TRUE)
  expect_error(nomo_power_rmsea(df = 20, test = "not_close", rmsea_null = 0, rmsea_alt = .05),
               "The test of not-close fit needs a null RMSEA above 0", fixed = TRUE)
  expect_error(nomo_power_rmsea(df = 20, test = "exact", rmsea_null = .03),
               "The test of exact fit has a null RMSEA of 0, not 0.030", fixed = TRUE)
  expect_identical(nomo_power_rmsea(df = 20, test = "close", rmsea_null = .03,
                                    rmsea_alt = .08)$rmsea_null, .03)
})


test_that("the RMSEA print formats its values as the style gives them (#144)", {
  never <- nomo_power_rmsea(df = 1, rmsea_null = .05, rmsea_alt = .05001)
  local_reproducible_output(width = 80)
  printed <- capture.output(print(never))
  # An alternative just off the null shows the digits that tell them apart.
  expect_identical(printed[[3L]], "df: 1 | RMSEA: null 0.050, alternative 0.05001 | alpha = .05")
  expect_true(any(grepl("RMSEA = root mean square error of approximation; df = degrees of",
                        printed, fixed = TRUE)))
  expect_identical(printed[[length(printed)]],
                   "See nomo_table(x, \"power\") for the power at each sample size.")
  # A target given to three decimals keeps them, and so does a power that
  # would otherwise print as the target.
  target <- nomo_power_rmsea(df = 20, test = "exact", power = .825)
  printed <- capture.output(print(target))
  expect_match(printed, "Smallest N for power .825: ", fixed = TRUE, all = FALSE)
  expect_match(printed, "^ +[0-9]+ +[.]8[0-9]{2,}$", all = FALSE)
})


test_that("an infeasible standardized population is named, not simulated silently (#145)", {
  skip_on_cran()
  infeasible <- "A =~ 0.9*a1 + 0.9*a2 + 0.9*a3\nB =~ 0.9*b1 + 0.9*b2 + 0.9*b3\nB ~ 0.8*A"
  expect_warning(
    nomo_power_simulate(infeasible, n = 50, reps = 2, seed = 1),
    "implies variances other than 1 (b1 = 2.33, b2 = 2.33, and b3 = 2.33)", fixed = TRUE
  )
  feasible <- "A =~ 0.7*a1 + 0.7*a2 + 0.7*a3\nB =~ 0.7*b1 + 0.7*b2 + 0.7*b3\nB ~ 0.4*A"
  expect_no_warning(nomo_power_simulate(feasible, n = 50, reps = 2, seed = 1))
  # Unstandardized data are generated as written, with nothing to check.
  expect_no_warning(nomo_power_simulate(infeasible, n = 50, reps = 2, seed = 1,
                                        standardized = FALSE))

  # The check draws no random numbers the simulation would have drawn: a
  # seeded study is the one it was before the check existed.
  set.seed(5)
  before <- stats::runif(1)
  set.seed(5)
  nomologR:::nomo_power_check_unit_variance(feasible)
  expect_identical(stats::runif(1), before)
})


test_that("the simulation print names what its references cover (#145)", {
  skip_on_cran()
  pw <- nomo_power_simulate(pw_population, n = 40, reps = 3, seed = 3, focus = "A~~B")
  local_reproducible_output(width = 80)
  printed <- capture.output(print(pw))
  text <- gsub("[[:space:]]+", " ", paste(printed, collapse = " "))
  expect_identical(printed[[2L]], "Muth\u00e9n and Muth\u00e9n (2002).")
  expect_match(text, "within 10% for every parameter given a population value", fixed = TRUE)
  expect_match(text, "coverage .91 to .98, and power .80", fixed = TRUE)
  expect_match(text, "alpha = .05", fixed = TRUE)
  expect_match(text, "N -- Sample size.", fixed = TRUE)
  expect_false(grepl("0.91", text, fixed = TRUE))
  expect_match(text, "nomo_table(x, \"summary\") for the table by sample size.", fixed = TRUE)
})


test_that("plot() draws the power curve of an RMSEA test (#145)", {
  close <- nomo_power_rmsea(df = 20, n = c(100, 200, 400))
  p <- plot(close)
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$title, "Power of the test of close fit")
  expect_match(flat_text(p$labels$caption), "Dashed line: target power .80.", fixed = TRUE)
  expect_match(flat_text(p$labels$caption),
               sprintf("the smallest N that reaches it, %d.", close$n_required),
               fixed = TRUE)
  expect_identical(nrow(p$layers[[4L]]$data), 3L)
  expect_error(plot(close, type = "bias"), "`type` must be one of", fixed = TRUE)

  never <- plot(nomo_power_rmsea(df = 1, rmsea_null = .05, rmsea_alt = .05001))
  expect_match(flat_text(never$labels$caption), "No N up to one million reaches it.", fixed = TRUE)
  expect_match(flat_text(never$labels$subtitle), "alternative 0.05001", fixed = TRUE)
})


test_that("plot() draws a simulation's power by sample size for each focus parameter (#145)", {
  skip_on_cran()
  pw <- nomo_power_simulate(pw_population, n = c(60, 300), reps = 12, focus = "A~~B", seed = 11)
  p <- plot(pw)
  expect_s3_class(p, "ggplot")
  expect_identical(p$data$parameter, c("A~~B", "A~~B"))
  meets <- pw$summary$meets_references[match(p$data$n, pw$summary$n)]
  expect_identical(as.character(p$data$status), ifelse(meets, "none", "review"))
  expect_match(flat_text(p$labels$caption), "Muth\u00e9n and Muth\u00e9n (2002)", fixed = TRUE)

  failing <- nomo_power_simulate(pw_population, n = 20, reps = 2, seed = 5,
                                 analysis = "A =~ zz1 + zz2 + zz3")
  expect_error(plot(failing), "No focus parameter has an estimated power to plot.", fixed = TRUE)
})
