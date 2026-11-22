# Console presentation helpers (#89) --------------------------------------------

test_that("numbers, p-values, and intervals follow one format", {
  expect_identical(nomologR:::nomo_present_number(c(0.12345, NA, Inf)),
                   c("0.123", "--", "--"))
  expect_identical(nomologR:::nomo_present_number(2, 1L), "2.0")
  expect_identical(nomologR:::nomo_present_signed(c(0.0216, -0.4, NA)),
                   c("+0.022", "-0.400", "--"))
  expect_identical(nomologR:::nomo_present_p(c(0.00004, 0.0431, 0.5, NA)),
                   c("< .001", ".043", ".500", "--"))
  expect_identical(nomologR:::nomo_present_p_clause(c(0.0002, 0.2, NA)),
                   c("p < .001", "p = .200", ""))
  expect_identical(nomologR:::nomo_present_ci(c(0.1, NA), c(0.3, NA)),
                   c("[0.100, 0.300]", "--"))
})


test_that("a count takes its noun in the right number", {
  count <- nomologR:::nomo_present_count
  expect_identical(count(c(0, 1, 2), "factor"), c("0 factors", "1 factor", "2 factors"))
  expect_identical(count(1:2, "analysis", "analyses"), c("1 analysis", "2 analyses"))
  expect_identical(nomologR:::nomo_present_noun(3, "degree"), "degrees")
})


test_that("alternatives read as prose, and a bad choice names its argument (#89)", {
  or <- nomologR:::nomo_present_or
  expect_identical(or("a"), "a")
  expect_identical(or(c("a", "b")), "a or b")
  expect_identical(or(c("a", "b", "c")), "a, b, or c")

  pick <- function(size = c("small", "large")) nomologR:::nomo_match_arg(size)
  expect_identical(pick(), "small")
  expect_identical(pick("lar"), "large")
  expect_error(pick("medium"), '`size` must be one of "small" or "large", not "medium".',
               fixed = TRUE)
  expect_error(nomologR:::nomo_match_arg("x", choices = "teaching"),
               'must be one of "teaching", not "x".', fixed = TRUE)

  # The shared whole-number check sits with the other argument helpers
  # (#145, R-0b follow-up): one finite whole number in R's integer range.
  whole <- nomologR:::nomo_is_whole_number
  expect_identical(vapply(list(3, -2L, 3.5, 1e10, NA_real_, Inf, c(1, 2), "3", numeric()),
                          whole, logical(1)),
                   c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  helpers <- testthat::test_path("..", "..", "R", "utils-args.R")
  skip_if_not(file.exists(helpers))
  expect_true(any(grepl("^nomo_is_whole_number <- function", readLines(helpers))))
})


test_that("every flag vocabulary is shown in one wording", {
  flag <- nomologR:::nomo_present_flag
  expect_identical(flag(c("KEEP", "REVIEW", "STRONG REVIEW")), c("", "review", "concern"))
  expect_identical(flag(c("info", "review", "concern")), c("", "review", "concern"))
  expect_identical(flag(c("none", "INFO", "unavailable")), c("", "", "not computed"))
  expect_identical(flag(c("Something Else", NA)), c("something else", ""))
  expect_identical(nomologR:::nomo_present_flag_counts(c("KEEP", "KEEP")), "none")
  expect_identical(nomologR:::nomo_present_flag_counts(c("REVIEW", "STRONG REVIEW", "KEEP")),
                   "1 concern, 1 review")
})


test_that("plot legends use the same flag wording (#89)", {
  expect_identical(
    as.character(nomologR:::nomo_present_flag_legend(
      c("KEEP", "REVIEW", "STRONG REVIEW", "info", "concern")
    )),
    c("none", "review", "concern", "none", "concern")
  )

  skip_on_cran()
  legend <- function(p, aesthetic) ggplot2::get_guide_data(p, aesthetic)$.label
  cfa <- nomo_cfa(
    nomo_model(list(A = paste0("a", 1:5), B = paste0("b", 1:5))),
    data = nomo_demo_continuous
  )
  val <- nomo_validity(cfa)

  # The CFA plots use the shared status scales (#144).
  cfa$standardized_loadings$attention[1:3] <- c("KEEP", "REVIEW", "STRONG REVIEW")
  p <- plot(cfa, type = "loadings")
  expect_identical(p$labels$shape, "Flag")
  expect_identical(legend(p, "shape"), c("No flag", "Review", "Concern"))
  cfa$fit_evidence$attention <- ifelse(cfa$fit_evidence$metric == "SRMR", "review", "info")
  expect_identical(legend(plot(cfa, type = "fit"), "shape"), c("No flag", "Review"))

  val$ave$attention <- c("info", "concern")
  expect_identical(legend(plot(val, type = "ave"), "shape"), c("No flag", "Concern"))

  p <- plot(nomo_screen(nomo_demo_continuous), type = "evidence")
  expect_identical(p$scales$get_scales("shape")$name, "Flag")
  expect_identical(legend(p, "shape"), c("No flag", "Review"))
})


test_that("text, facts, and bullets wrap to the console width", {
  local_reproducible_output(width = 40)
  long <- paste(rep("word", 30), collapse = " ")

  text <- utils::capture.output(nomologR:::nomo_present_text(long, indent = 2L))
  expect_true(all(nchar(text) <= 40L))
  expect_true(all(startsWith(text, "  ")))

  facts <- utils::capture.output(nomologR:::nomo_present_facts(
    c("First part here", "", "Second part is longer", "Third part")
  ))
  expect_gt(length(facts), 1L)
  expect_true(all(nchar(facts) <= 40L))
  expect_false(any(grepl("|  |", facts, fixed = TRUE)))
  expect_identical(utils::capture.output(nomologR:::nomo_present_facts(character())),
                   character())

  bullets <- utils::capture.output(nomologR:::nomo_present_bullets(c(long, NA, "")))
  expect_true(startsWith(bullets[[1L]], "  - "))
  expect_true(all(startsWith(bullets[-1L], "    ")))
  expect_true(all(nchar(bullets) <= 40L))
})


test_that("tables align, drop empty columns, and name what does not fit", {
  local_reproducible_output(width = 60)
  d <- data.frame(
    item = c("a1", "a2"), value = c(0.5, -0.25), n = c(10L, 200L),
    ok = c(TRUE, NA), empty = c(NA, NA), p = c(0.2, 0.0001),
    stringsAsFactors = FALSE
  )
  out <- utils::capture.output(nomologR:::nomo_present_table(
    d,
    c("Item" = "item", "Value" = "value", "N" = "n", "OK" = "ok",
      "Empty" = "empty", "p" = "p", "Missing" = "not_a_column"),
    formats = list(p = nomologR:::nomo_present_p)
  ))
  expect_identical(out[[1L]], "  Item   Value    N  OK        p")
  expect_identical(out[[2L]], "  a1     0.500   10  yes    .200")
  expect_identical(out[[3L]], "  a2    -0.250  200  --   < .001")

  wide <- data.frame(a = "x", b = strrep("y", 30), c = strrep("z", 30))
  narrow <- utils::capture.output(nomologR:::nomo_present_table(
    wide, c("A" = "a", "B" = "b", "C" = "c"), more = "nomo_table(x)"
  ))
  expect_false(any(grepl("zzz", narrow)))
  expect_match(narrow[[length(narrow)]], "Not shown for width: C. See nomo_table(x).",
               fixed = TRUE)
  bare <- utils::capture.output(nomologR:::nomo_present_table(
    wide, c("A" = "a", "B" = "b", "C" = "c")
  ))
  expect_match(bare[[length(bare)]], "Not shown for width: C.$")

  expect_identical(utils::capture.output(nomologR:::nomo_present_table(d[0, ], c("Item" = "item"))),
                   character())
  expect_identical(utils::capture.output(nomologR:::nomo_present_table(d, c("E" = "empty"))),
                   character())
})


test_that("notes carry their flag, and a column with one value is left out", {
  notes <- nomologR:::nomo_present_notes
  expect_identical(capture.output(notes(NULL)), character())
  expect_identical(capture.output(notes(data.frame(severity = character(), note = character()))),
                   character())
  expect_identical(
    capture.output(notes(data.frame(severity = c("info", "review"),
                                    note = c("A note.", "Look again.")))),
    c("  - A note.", "  - Review: Look again.")
  )

  drop <- nomologR:::nomo_present_drop_constant
  columns <- c(Construct = "construct", Block = "block")
  expect_identical(drop(columns, "Block", c("overall", "overall")), c(Construct = "construct"))
  expect_identical(drop(columns, "Block", c("overall", "group")), columns)
})


test_that("flagged log entries and percentages are shown one way (#89)", {
  flagged <- nomologR:::nomo_present_flagged
  log <- tibble::tibble(
    severity = c("info", "review", "concern"),
    observation = c("Fine.", "Look again.", "A problem."),
    recommendation = c("", "Check it.", "Fix it.")
  )
  expect_identical(capture.output(flagged(log[1L, ])), character())
  expect_identical(capture.output(flagged(log)),
                   c("", "Flagged", "  - Concern: A problem.", "  - Review: Look again."))
  expect_identical(capture.output(flagged(log, recommendation = TRUE))[3:4],
                   c("  - Concern: A problem. Fix it.", "  - Review: Look again. Check it."))

  # One decimal, so a single failure in 500 is not rounded away.
  expect_identical(nomologR:::nomo_present_percent(c(0.998, 0.0432, NA)),
                   c("99.8%", "4.3%", "--"))
})


test_that("invariance summaries say what each level holds equal (#89)", {
  held <- function(level, constraints) {
    capture.output(nomologR:::nomo_invariance_present_constraints(
      tibble::tibble(level = level, constraints = constraints)
    ))
  }
  local_reproducible_output(width = 80)
  expect_identical(
    held(c("configural", "metric", "scalar", "strict"),
         c("none", "loadings", "loadings, intercepts", "loadings, intercepts, residuals")),
    c("  Held equal: loadings from metric; intercepts from scalar; residuals from",
      "  strict.")
  )
  expect_identical(
    held(c("configural", "strong"), c("none", "thresholds, loadings, intercepts")),
    "  Held equal: thresholds, loadings, and intercepts from strong."
  )
  expect_identical(held("configural", "none"), character())

  # A researcher-specified release is named at the level it is first made; the
  # releases carry over to later levels as fitted.
  expect_identical(
    capture.output(nomologR:::nomo_invariance_present_constraints(tibble::tibble(
      level = c("configural", "metric", "scalar", "strict"),
      constraints = c("none", "loadings", "loadings, intercepts",
                      "loadings, intercepts, residuals"),
      partial_requested = c("", "", "ag3 ~ 1; ag2 ~ 1", "ag3 ~ 1; ag2 ~ 1")
    ))),
    c("  Held equal: loadings from metric; intercepts from scalar, except ag3 ~ 1 and",
      "  ag2 ~ 1; residuals from strict.")
  )
})


# `digits` changes a printout that shows statistics and leaves the default
# as it was; a printout with none says so and prints as usual (#145,
# clarity-22).
expect_digits_apply <- function(x) {
  default <- utils::capture.output(print(x))
  expect_identical(utils::capture.output(print(x, digits = NULL)), default)
  expect_false(identical(utils::capture.output(print(x, digits = 4)), default))
}

expect_digits_unused <- function(x) {
  default <- utils::capture.output(print(x))
  expect_message(given <- utils::capture.output(print(x, digits = 4)),
                 "`digits` changes nothing here", fixed = TRUE)
  expect_identical(given, default)
}


test_that("the CFA print and summary read as designed (#89)", {
  cfa <- nomo_cfa(
    nomo_model(list(A = paste0("a", 1:5), B = paste0("b", 1:5))),
    data = nomo_demo_continuous
  )
  expect_snapshot(print(cfa))
  expect_snapshot(print(summary(cfa)))
  expect_digits_apply(cfa)
  expect_digits_apply(summary(cfa))
  # Fewer decimals round the fit values but never the references: a CFI
  # reference of .95 had read "1.0" (#145, clarity-22).
  fewer <- utils::capture.output(print(summary(cfa), digits = 1))
  expect_true("  CFI      .97       .950" %in% fewer)
  expect_true("  RMSEA   0.05      0.060  [0.0, 0.1]" %in% fewer)
})


test_that("the item audit, factor retention, and EFA read as designed (#89)", {
  skip_on_cran()
  scr <- nomo_screen(nomo_demo_continuous)
  fac <- nomo_factors(nomo_demo_continuous, seed = 2026)
  efa <- nomo_efa(nomo_demo_continuous, factors = fac)
  expect_snapshot(print(scr))
  expect_snapshot(print(summary(scr)))
  expect_snapshot(print(fac))
  expect_snapshot(print(summary(fac)))
  expect_snapshot(print(efa))
  expect_snapshot(print(summary(efa)))
  expect_digits_unused(scr)
  for (x in list(summary(scr), fac, summary(fac), efa, summary(efa))) expect_digits_apply(x)
})


test_that("reliability, validity, scores, and missing-data output read as designed (#89)", {
  skip_on_cran()
  cfa <- nomo_cfa(
    nomo_model(list(A = paste0("a", 1:5), B = paste0("b", 1:5))),
    data = nomo_demo_continuous
  )
  rel <- nomo_reliability(cfa)
  val <- nomo_validity(cfa)
  sc <- nomo_scores(cfa, method = "sum")
  expect_snapshot(print(rel))
  expect_snapshot(print(summary(rel)))
  expect_snapshot(print(val))
  expect_snapshot(print(summary(val)))
  expect_snapshot(print(sc))
  expect_snapshot(print(summary(sc)))
  mis <- nomo_missing(cfa, data = nomo_demo_continuous)
  expect_snapshot(print(mis))
  for (x in list(rel, summary(rel), val, summary(val), sc, summary(sc), mis, summary(mis))) {
    expect_digits_apply(x)
  }
  # References keep at least their own decimals (#145, clarity-22).
  expect_true("Primary coefficient: model-based omega | Review reference: .70" %in%
                utils::capture.output(print(rel, digits = 1)))
  fewer <- utils::capture.output(print(val, digits = 1))
  expect_true("AVE reference: .50 | HTMT reference: 0.85" %in% fewer)
  expect_true("  - A (Review): AVE .497 is below the review reference .50." %in% fewer)
})


test_that("hierarchical output reads as designed (#89)", {
  skip_on_cran()
  set.seed(2026)
  n <- 500
  g <- rnorm(n)
  s <- matrix(rnorm(n * 3), n, 3)
  dat <- as.data.frame(sapply(1:9, function(i) {
    .6 * g + .45 * s[, ceiling(i / 3)] + rnorm(n, sd = .65)
  }))
  names(dat) <- paste0("x", 1:9)
  factors <- list(A = c("x1", "x2", "x3"), B = c("x4", "x5", "x6"), C = c("x7", "x8", "x9"))
  hier <- nomo_hierarchical(nomo_cfa(nomo_model(factors, structure = "bifactor"), data = dat))
  expect_snapshot(print(hier))
  expect_snapshot(print(summary(hier)))
  expect_digits_apply(hier)
  expect_digits_apply(summary(hier))
  # An index and the share in its interpretation have one precision (#145).
  more <- paste(utils::capture.output(print(summary(hier), digits = 4)), collapse = "\n")
  expect_match(more, "Omega total = .8962: Common sources explain .8962 of", fixed = TRUE)
  expect_match(more, "PUC = .7500: Of the item correlations, .7500 are", fixed = TRUE)
})


test_that("comparison, invariance, and network output read as designed (#89)", {
  skip_on_cran()
  cont <- nomo_demo_continuous
  full <- nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5", data = cont)
  no_b5 <- nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + 0*b5", data = cont)
  cmp <- nomo_compare(full = full, no_b5 = no_b5, rationale = "Is b5 needed?")
  expect_snapshot(print(cmp))
  expect_snapshot(print(summary(cmp)))

  inv <- nomo_invariance("Agency =~ ag1 + ag2 + ag3 + ag4", data = nomo_demo_network,
                         group = "group", levels = c("configural", "metric", "scalar"))
  expect_snapshot(print(inv))
  expect_snapshot(print(summary(inv)))
  expect_snapshot(print(nomo_partial(level = "scalar", syntax = "ag3 ~ 1",
                                     rationale = "Anticipated mode difference.")))

  h <- nomo_hypotheses(
    "Agency -> Persistence" = positive(min = .20),
    "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15)),
    "Agency -> Performance" = positive()
  )
  expect_snapshot(print(h))
  expect_snapshot(print(summary(h)))
  scales <- list(Agency = paste0("ag", 1:4), Persistence = paste0("pe", 1:4),
                 SocialDesirability = paste0("sd", 1:3))
  net <- nomo_network(nomo_model(scales), data = nomo_demo_network, hypotheses = h)
  expect_snapshot(print(net))
  expect_snapshot(print(summary(net)))
  split <- nomo_split(nomo_demo_network, validation_prop = 0.4, seed = 2026)
  expect_snapshot(print(split))
  for (x in list(cmp, summary(cmp), inv, summary(inv), h, summary(h), net, summary(net), split)) {
    expect_digits_apply(x)
  }
  # The requested proportion was given, and the realized one equals it, so
  # both keep their decimals (#145, clarity-22).
  expect_true("Validation proportion: .40 requested, .40 realized | Seed: 2026" %in%
                utils::capture.output(print(split, digits = 1)))
  expect_digits_unused(nomo_partial(level = "scalar", syntax = "ag3 ~ 1",
                                    rationale = "Anticipated mode difference."))
})


test_that("the other printouts take `digits` too (#145, clarity-22)", {
  skip_on_cran()
  model <- nomo_model(list(Agency = paste0("ag", 1:4), Persistence = paste0("pe", 1:4)))
  expect_digits_unused(model)
  esem <- nomo_esem(model, nomo_demo_network)
  method <- nomo_method_variance(model, data = nomo_demo_network, marker = c("sd1", "sd2", "sd3"))
  rmsea <- nomo_power_rmsea(model)
  simulated <- nomo_power_simulate("F =~ .7*y1 + .7*y2 + .7*y3 + .7*y4", n = 100, reps = 20,
                                   seed = 1)
  set.seed(2026)
  true <- stats::rnorm(150)
  panel <- data.frame(agency_t1 = 3 + true + stats::rnorm(150, sd = .45),
                      agency_t2 = 3.1 + true + stats::rnorm(150, sd = .45))
  retest <- nomo_retest(panel, scores = c("agency_t1", "agency_t2"), interval = "two weeks")
  single <- nomo_single_indicator(.85, source = "Test manual, Table 4")
  for (x in list(esem, summary(esem), method, summary(method), rmsea, summary(rmsea),
                 simulated, summary(simulated), retest, summary(retest), single)) {
    expect_digits_apply(x)
  }
  # Values the researcher gave never print with fewer decimals than by
  # default, and the references of a power analysis keep theirs, as its target
  # does (#145, clarity-22).
  expect_true("df: 19 | RMSEA: null 0.050, alternative 0.080 | alpha = .05" %in%
                utils::capture.output(print(rmsea, digits = 1)))
  expect_true("Reliability: .850 (omega)" %in% utils::capture.output(print(single, digits = 1)))
  fewer <- utils::capture.output(print(summary(simulated), digits = 1))
  expect_true(any(grepl("^  100  F=~y1 +0\\.70 +0\\.7 ", fewer)))
  more <- paste(utils::capture.output(print(simulated, digits = 4)), collapse = " ")
  expect_match(more, ".91 to .98, and power .80 for", fixed = TRUE)
  cfa <- nomo_cfa(model, data = nomo_demo_network)
  expect_digits_unused(nomo_apa_table(cfa, "loadings", number = 1))
})


test_that("invariance flags the levels with what went wrong (#89, #144)", {
  # A level that failed or raised a warning is a flag in the decision log, and
  # the Flagged section names it with what went wrong.
  log <- tibble::tibble(
    object = c("metric", "metric", "scalar"),
    metric = c("invariance_level", "engine_warning", "invariance_level"),
    severity = c("info", "review", "concern"),
    observation = c("Metric model estimated.", "lavaan warned when fitting the metric model: a lavaan warning",
                    "The scalar model could not be fitted: model could not be identified"),
    recommendation = "Inspect it."
  )
  local_reproducible_output(width = 80)
  txt <- utils::capture.output(nomologR:::nomo_invariance_present_flagged(log))
  expect_identical(txt, c(
    "", "Flagged",
    "  - scalar (Concern): The scalar model could not be fitted: model could not be",
    "    identified.",
    "  - metric (Review): lavaan warned when fitting the metric model: a lavaan",
    "    warning."
  ))
  expect_identical(utils::capture.output(nomologR:::nomo_invariance_present_flagged(log[1L, ])),
                   character())
})


test_that("guided runs say what they found, and group repeated requests (#89)", {
  skip_on_cran()
  scales <- list(Agency = paste0("ag", 1:4), Persistence = paste0("pe", 1:4),
                 SocialDesirability = paste0("sd", 1:3))
  factors <- list(criterion_set = "minimal", n_iter = 20, seed = 2026)
  paused <- nomo_run(nomo_demo_network, scales = scales, settings = list(factors = factors))
  expect_snapshot(print(paused))

  h <- nomo_hypotheses("Agency -> Persistence" = positive(min = .20))
  complete <- nomo_run(
    nomo_demo_network, scales = scales,
    decisions = list(factor_count = c(Agency = 1, Persistence = 1, SocialDesirability = 1),
                     cfa_model = nomo_model(scales), measurement_model = "proceed"),
    settings = list(factors = factors, network = list(hypotheses = h),
                    invariance = list(group = "group", levels = c("configural", "metric")),
                    screen = list(effort = TRUE), scores = list(method = "sum"),
                    missing = list(reliability = FALSE))
  )
  expect_snapshot(print(complete))
  expect_snapshot(print(summary(complete)))
  expect_digits_apply(complete)
  expect_digits_unused(summary(complete))
  # A network flag about a relation reads as the network's own summary gives
  # it: the relation is the unit, and no bullet puts it in backticks (#145,
  # out-11).
  summarized <- utils::capture.output(print(summary(complete)))
  flagged <- summarized[seq(match("Flagged", summarized), length(summarized))]
  expect_true(any(startsWith(flagged, "  - Persistence <-> SocialDesirability in the network (Review): The")))
  expect_false(any(grepl("`[^`]* (<->|->) [^`]*`", flagged)))

  # The closing hint is a call that runs as written: nomo_report() has no
  # default path, so the hint names `file`.
  expect_match(
    paste(utils::capture.output(print(complete)), collapse = " "),
    "nomo_report(x, file = \"report.html\") for an archived report.",
    fixed = TRUE
  )

  # Evidence that is unavailable is described as such rather than left out.
  thin <- complete
  thin$results$factors$Agency$parallel$n_factors <- NULL
  thin$results$cfa$fit_evidence$value <- NA_real_
  thin$results$reliability$evidence$estimate <- NA_real_
  evidence <- nomologR:::nomo_run_key_evidence(thin)
  expect_true(any(grepl("Agency -", evidence, fixed = TRUE)))
  expect_true(any(grepl("CFA: fit unavailable", evidence, fixed = TRUE)))
  expect_false(any(grepl("Reliability:", evidence, fixed = TRUE)))
})


test_that("a flag without a log row of its own still gives a reason (#89)", {
  s <- summary(nomo_screen(data.frame(a = c(1, 2, 3, 4, 5, 6), b = c(2, 1, 4, 3, 6, 5),
                                      c = c(1, 3, 2, 5, 4, 6))))
  s$item_review$attention[[1L]] <- "review"
  s$item_review$review_metrics[[1L]] <- "negative_interitem_pairs"
  s$item_review$attention[[2L]] <- "concern"
  s$item_review$review_metrics[[2L]] <- ""
  s$decision_log <- s$decision_log[0, , drop = FALSE]
  txt <- paste(utils::capture.output(print(s)), collapse = " ")
  expect_match(txt, "a (Review): Some of its inter-item correlations are negative;",
               fixed = TRUE)
  expect_match(txt, "b (Concern): The decision log has the details.", fixed = TRUE)
})


test_that("factor-retention summaries without rule sensitivity leave that section out (#89)", {
  s <- summary(nomo_factors(nomo_demo_continuous, criterion_set = "minimal", n_iter = 10,
                            seed = 1))
  s$parallel_sensitivity <- NULL
  txt <- utils::capture.output(print(s))
  expect_false(any(grepl("rule sensitivity", txt, fixed = TRUE)))
  expect_true(any(grepl("Retention evidence", txt, fixed = TRUE)))
})
