# Conventions in returned tables (#145) -----------------------------------------
#
# ?nomologR records the names and values that differ from one table to another:
# proportions in `pct_*` columns, three stored flag vocabularies, and `pvalue`
# beside `p_value`. These tests hold the record to what the tables contain.

conventions_text <- function() {
  nomo_test_rd_text("nomologR-package", "Conventions in returned tables")
}

rd_names <- function(text, name) grepl(sprintf("\\code{%s}", name), text, fixed = TRUE)


test_that("?nomologR records the units, flag vocabularies, and p-value names", {
  text <- conventions_text()

  for (column in c("pct_missing", "pct_incomplete", "pct_dropped", "percent_unique",
                   "nzv_percent_unique_reference")) {
    expect_true(rd_names(text, column), label = column)
  }
  for (value in c("KEEP", "REVIEW", "STRONG REVIEW", "none", "info", "review",
                  "concern", "unavailable")) {
    expect_true(rd_names(text, sprintf('"%s"', value)), label = value)
  }
  for (column in c("p_value", "pvalue", "lrt_p", "chi_square", "chisq")) {
    expect_true(rd_names(text, column), label = column)
  }
})


test_that("each help page names the units and flags of its own tables", {
  for (topic in c("nomo_efa", "nomo_cfa", "nomo_validity")) {
    value <- nomo_test_rd_text(topic, "\\value")
    expect_true(rd_names(value, '"STRONG REVIEW"'), label = topic)
    expect_true(grepl("Conventions in returned tables", value, fixed = TRUE), label = topic)
  }
  expect_true(rd_names(nomo_test_rd_text("nomo_cfa", "\\value"), "pct_dropped"))

  # Pages whose tables use only the evidence vocabulary name it too.
  for (topic in c("nomo_reliability", "nomo_network")) {
    value <- nomo_test_rd_text(topic, "\\value")
    for (flag in c("info", "review", "concern")) {
      expect_true(rd_names(value, sprintf('"%s"', flag)), label = paste(topic, flag))
    }
    expect_true(grepl("Conventions in returned tables", value, fixed = TRUE), label = topic)
  }
  expect_true(rd_names(nomo_test_rd_text("nomo_network", "\\value"), "measurement_attention"))
  expect_true(rd_names(nomo_test_rd_text("nomo_reliability", "\\value"), "signal"))

  # The Fornell-Larcker comparison can be "unavailable", which the page says.
  validity <- nomo_test_rd_text("nomo_validity", "\\value")
  expect_true(rd_names(validity, "fornell_larcker_pairs"))
  expect_true(rd_names(validity, '"unavailable"'))

  screen <- nomo_test_rd_text("nomo_screen", "\\value")
  expect_true(rd_names(screen, "pct_missing"))
  expect_true(rd_names(screen, "percent_unique"))

  missing <- nomo_test_rd_text("nomo_missing", "\\value")
  expect_true(rd_names(missing, "pct_missing"))
  expect_true(rd_names(missing, "pct_incomplete"))
  expect_true(grepl("proportions", missing, fixed = TRUE))
  expect_true(grepl("Conventions in returned tables", missing, fixed = TRUE))
})


test_that("flags display in the one wording ?nomologR describes", {
  expect_identical(nomo_present_flag(c("KEEP", "none", "info")), c("", "", ""))
  expect_identical(nomo_present_flag(c("REVIEW", "review")), c("review", "review"))
  expect_identical(nomo_present_flag(c("STRONG REVIEW", "concern")), c("concern", "concern"))
  expect_identical(nomo_present_flag("unavailable"), "not computed")
  # A decision log can hold only the evidence vocabulary.
  expect_identical(eval(formals(nomo_log_add)$severity), c("info", "review", "concern"))
})


test_that("pct_* columns hold proportions and percent_unique a percentage", {
  skip_on_cran()
  dat <- nomo_demo_continuous
  dat$a1[seq_len(50L)] <- NA
  missing_a1 <- mean(is.na(dat$a1))
  scr <- nomo_screen(dat)
  a1 <- scr$item_summary[scr$item_summary$item == "a1", , drop = FALSE]
  expect_equal(a1$pct_missing, missing_a1)
  # A percentage, 0 to 100, which a proportion could not exceed.
  expect_equal(a1$percent_unique, 100 * a1$n_unique / a1$n_observed)
  expect_gt(max(scr$item_summary$percent_unique, na.rm = TRUE), 1)

  hs <- lavaan::HolzingerSwineford1939
  hs$x1[seq_len(30L)] <- NA
  cfa <- nomo_cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6", data = hs)
  expect_equal(cfa$sample_summary$pct_dropped, 30 / nrow(hs))

  model <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
  sensitivity <- nomo_missing(nomo_cfa(model, data = dat), data = dat, reliability = FALSE)
  expect_equal(
    unname(sensitivity$variables$pct_missing[sensitivity$variables$variable == "a1"]),
    missing_a1
  )

  tables <- list(
    scr$item_summary, scr$case_summary, cfa$sample_summary,
    sensitivity$pattern, sensitivity$variables
  )
  for (tb in tables) {
    for (column in grep("^pct_", names(tb), value = TRUE)) {
      expect_true(all(tb[[column]] >= 0 & tb[[column]] <= 1, na.rm = TRUE), label = column)
    }
  }
})


test_that("stored flags stay within their documented vocabularies", {
  skip_on_cran()
  loading <- c("KEEP", "REVIEW", "STRONG REVIEW")
  evidence <- c("info", "review", "concern")

  scr <- nomo_screen(nomo_demo_continuous)
  efa <- nomo_efa(nomo_demo_continuous, factors = 2)
  cfa <- nomo_cfa(
    "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9",
    data = lavaan::HolzingerSwineford1939
  )
  val <- nomo_validity(cfa, fornell_larcker = TRUE)
  rel <- nomo_reliability(cfa)
  net <- nomo_network(
    "A =~ ag1 + ag2 + ag3 + ag4\nP =~ pe1 + pe2 + pe3 + pe4",
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses("A -> P" = positive())
  )

  expect_true(all(efa$item_summary$attention %in% loading))
  expect_true(all(cfa$standardized_loadings$attention %in% loading))
  expect_true(all(val$standardized_loadings$attention %in% loading))
  expect_true(all(summary(scr)$item_review$attention %in% c("none", "review", "concern")))
  expect_true(all(cfa$fit_evidence$attention %in% c(evidence, "unavailable")))
  expect_gt(nrow(val$fornell_larcker_pairs), 0L)
  expect_true(all(val$fornell_larcker_pairs$attention %in% c("info", "review", "unavailable")))
  expect_true(all(c(
    val$ave$attention, val$discriminant$attention,
    rel$evidence$attention, nomo_table(rel)$signal,
    net$hypothesis_evidence$measurement_attention,
    net$measurement_context$summary$attention,
    net$measurement_context$loadings$attention
  ) %in% evidence))
  expect_true(all(c(
    scr$decision_log$severity, efa$decision_log$severity,
    cfa$decision_log$severity, val$decision_log$severity,
    rel$decision_log$severity, net$decision_log$severity
  ) %in% evidence))
})


test_that("a Fornell-Larcker pair is unavailable when an AVE has no square root", {
  skip_on_cran()
  cfa <- nomo_cfa(
    "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9",
    data = lavaan::HolzingerSwineford1939
  )
  ave <- nomo_validity(cfa)$ave
  ave$estimate[ave$construct == "visual"] <- -0.1
  # sqrt() of the negative AVE warns; the pair's flag is what is checked here.
  pairs <- suppressWarnings(nomo_validity_fornell_larcker(cfa$fit, ave))$pairs
  with_visual <- pairs$construct_1 == "visual" | pairs$construct_2 == "visual"
  expect_true(all(pairs$attention[with_visual] == "unavailable"))
  expect_true(all(pairs$attention[!with_visual] %in% c("info", "review")))
})


test_that("?nomologR sets out the status words shared with contentvalidR (#144)", {
  text <- nomo_test_rd_text("nomologR-package", "Status words shared with contentvalidR")

  # Guide point 19: each contentvalidR status beside its nomologR word.
  rows <- c(
    "Supported" = "no flag", "Review" = "review", "(no counterpart)" = "concern",
    "Insufficient data" = "not computed", "Descriptive only" = "note"
  )
  cells <- trimws(strsplit(text, "\\\\cr|\\\\tab|\\\\tabular\\{ll\\}\\{")[[1L]])
  for (status in names(rows)) {
    at <- match(status, cells)
    expect_false(is.na(at), label = status)
    expect_identical(cells[[at + 1L]], rows[[status]], label = status)
  }
  # Guide point 33: what `recommendation` means in each package.
  expect_true(grepl("\\code{recommendation}", text, fixed = TRUE))
  expect_true(grepl("prose advice", text, fixed = TRUE))
  expect_true(grepl("decision word", text, fixed = TRUE))
  expect_true(grepl("never delete", text, fixed = TRUE))
})
