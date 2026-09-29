# Test-retest reliability (#129) --------------------------------------------------

retest_data <- function(n = 150, shift = .1, error = .45, seed = 2026) {
  set.seed(seed)
  true <- stats::rnorm(n)
  data.frame(
    agency_t1 = 3 + true + stats::rnorm(n, sd = error),
    agency_t2 = 3 + shift + true + stats::rnorm(n, sd = error),
    agency_t3 = 3 + 2 * shift + true + stats::rnorm(n, sd = error)
  )
}


test_that("the agreement and consistency ICCs are McGraw and Wong's A,1 and C,1", {
  dat <- retest_data()
  rt <- nomo_retest(dat, scores = c("agency_t1", "agency_t2"), interval = "two weeks")
  expect_s3_class(rt, "nomo_retest")
  expect_identical(rt$scores, list(composite = c("agency_t1", "agency_t2")))
  icc <- rt$icc
  expect_identical(icc$composite, "composite")
  expect_identical(icc$n, 150L)
  expect_identical(icc$n_occasions, 2L)

  # Two-way mean squares, computed directly.
  x <- as.matrix(dat[c("agency_t1", "agency_t2")])
  n <- nrow(x)
  k <- ncol(x)
  grand <- mean(x)
  msr <- k * sum((rowMeans(x) - grand)^2) / (n - 1)
  msc <- n * sum((colMeans(x) - grand)^2) / (k - 1)
  mse <- sum((x - outer(rowMeans(x), rep(1, k)) - outer(rep(1, n), colMeans(x)) + grand)^2) /
    ((n - 1) * (k - 1))
  expect_equal(icc$icc_agreement, (msr - mse) / (msr + (k - 1) * mse + k * (msc - mse) / n))
  expect_equal(icc$icc_consistency, (msr - mse) / (msr + (k - 1) * mse))
  expect_lt(icc$agreement_ci_lower, icc$icc_agreement)
  expect_gt(icc$agreement_ci_upper, icc$icc_agreement)

  sd_pooled <- sqrt(mean(c(stats::var(dat$agency_t1), stats::var(dat$agency_t2))))
  expect_equal(icc$sd, sd_pooled)
  expect_equal(icc$sem, sd_pooled * sqrt(1 - icc$icc_agreement))
  expect_equal(icc$sdc, stats::qnorm(.975) * sqrt(2) * icc$sem)
  change <- dat$agency_t2 - dat$agency_t1
  expect_equal(icc$mean_change, mean(change))
  expect_equal(c(icc$change_ci_lower, icc$change_ci_upper),
               as.numeric(stats::t.test(change)$conf.int))
  expect_identical(rt$interval, "two weeks")
})


test_that("reliable change exceeds the smallest detectable change, and nothing less", {
  rt <- nomo_retest(retest_data(), c("agency_t1", "agency_t2"))
  rc <- rt$reliable_change
  sdc <- rt$icc$sdc
  expect_identical(nrow(rc), 150L)
  expect_equal(rc$rci, rc$change / (sqrt(2) * rt$icc$sem))
  expect_identical(rc$status == "reliable_increase", rc$change > sdc)
  expect_identical(rc$status == "reliable_decrease", rc$change < -sdc)
  expect_true(any(rc$status == "reliable_increase"))
  expect_true(any(rc$status == "reliable_decrease"))
  expect_identical(nomo_table(rt, "reliable_change"), rc)
  expect_identical(nomo_table(rt), rt$icc)
})


test_that("Koo and Li's description is read from the interval", {
  describe <- nomologR:::nomo_retest_koo_li
  expect_identical(describe(.2, .45), "poor")
  expect_identical(describe(.45, .6), "poor to moderate")
  expect_identical(describe(.76, .9), "good")
  expect_identical(describe(.8, .95), "good to excellent")
  expect_identical(describe(.92, .97), "excellent")
  expect_true(is.na(describe(NA_real_, .9)))
})


test_that("several composites, missing responses, and three occasions are handled", {
  dat <- retest_data()
  dat$other_t1 <- dat$agency_t1 + stats::rnorm(150, sd = 2)
  dat$other_t2 <- dat$agency_t2 + stats::rnorm(150, sd = 2)
  dat$agency_t2[1:10] <- NA
  rt <- nomo_retest(dat, scores = list(
    Agency = c("agency_t1", "agency_t2", "agency_t3"),
    Other = c("other_t1", "other_t2")
  ))
  expect_identical(names(rt$scores), c("Agency", "Other"))
  expect_identical(rt$icc$composite, c("Agency", "Other"))
  expect_identical(rt$icc$n, c(140L, 150L))
  expect_identical(rt$icc$last, c("agency_t3", "other_t2"))
  expect_identical(unique(rt$reliable_change$row[rt$reliable_change$composite == "Agency"]),
                   11:150)
  # A noisy composite can be poorly reliable; the interval then says so.
  expect_match(rt$icc$koo_li[[2L]], "poor")
  log <- rt$decision_log
  poor <- log[log$metric == "icc_agreement" & log$object == "Other", ]
  expect_identical(poor$severity, "review")
  expect_match(poor$recommendation, "reaches the range described as poor", fixed = TRUE)
  good <- log[log$metric == "icc_agreement" & log$object == "Agency", ]
  expect_identical(good$severity, "info")
  expect_identical("retest_interval" %in% log$metric, TRUE)
})


test_that("a systematic shift between occasions is flagged, and its absence noted", {
  shifted <- nomo_retest(retest_data(shift = .5), c("agency_t1", "agency_t2"))
  log <- shifted$decision_log
  change <- log[log$metric == "mean_change", ]
  expect_identical(change$severity, "review")
  expect_match(change$recommendation, "ICC(A,1) counts the shift as disagreement", fixed = TRUE)
  expect_lt(shifted$icc$icc_agreement, shifted$icc$icc_consistency)

  steady <- nomo_retest(retest_data(shift = 0), c("agency_t1", "agency_t2"))
  change <- steady$decision_log[steady$decision_log$metric == "mean_change", ]
  expect_identical(change$severity, "info")
  expect_match(change$recommendation, "includes no change", fixed = TRUE)
})


test_that("scores and data are checked", {
  dat <- retest_data()
  expect_error(nomo_retest(list(), "a"), "non-empty data frame")
  expect_error(nomo_retest(dat[0, ], c("agency_t1", "agency_t2")), "non-empty data frame")
  expect_error(nomo_retest(dat, "agency_t1"), "two or more distinct columns")
  expect_error(nomo_retest(dat, c("agency_t1", "agency_t1")), "two or more distinct columns")
  expect_error(nomo_retest(dat, list(c("agency_t1", "agency_t2"))),
               "`scores` must be a character vector of column names, or a named", fixed = TRUE)
  expect_error(nomo_retest(dat, list()), "named")
  expect_error(nomo_retest(dat, c("agency_t1", "nope")), "`scores` column not in `data`: nope.",
               fixed = TRUE)
  expect_error(nomo_retest(dat, c("agency_t1", "nope", "nada")), "`scores` columns not in",
               fixed = TRUE)
  words <- dat
  words$agency_t2 <- as.character(words$agency_t2)
  expect_error(nomo_retest(words, c("agency_t1", "agency_t2")),
               "`scores` columns must be numeric. Not numeric: agency_t2", fixed = TRUE)
  expect_error(nomo_retest(dat, c("agency_t1", "agency_t2"), interval = ""), "`interval` must be")

  few <- dat[1:2, ]
  expect_error(nomo_retest(few, c("agency_t1", "agency_t2")), "2 complete cases")
  one <- dat[1, ]
  expect_error(nomo_retest(one, c("agency_t1", "agency_t2")), "1 complete case across")
  flat <- data.frame(a = rep(1, 5), b = rep(1, 5))
  expect_error(nomo_retest(flat, c("a", "b")), "does not vary")
})


test_that("the results print, summarize, report methods, and form an APA table", {
  dat <- retest_data(shift = .5)
  rt <- nomo_retest(dat, c("agency_t1", "agency_t2"), interval = "two weeks")
  local_reproducible_output(width = 80)
  printed <- capture.output(print(rt))
  expect_match(printed, "Interval: two weeks", fixed = TRUE, all = FALSE)
  expect_match(printed, "ICC(A,1) [95% CI]", fixed = TRUE, all = FALSE)
  expect_match(printed, "composite: ", fixed = TRUE, all = FALSE)
  expect_false(any(nchar(printed) > 80L))

  summarized <- capture.output(print(summary(rt)))
  expect_match(summarized, "Consistency and change", fixed = TRUE, all = FALSE)
  expect_match(summarized, "Flagged", fixed = TRUE, all = FALSE)
  expect_false(any(nchar(summarized) > 80L))

  steady <- nomo_retest(retest_data(shift = 0), c("agency_t1", "agency_t2"))
  expect_match(capture.output(print(steady)), "Interval: not recorded", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("Flagged", capture.output(print(summary(steady))), fixed = TRUE)))

  expect_setequal(nomo_methods(rt)$id, c("icc_retest", "sem_sdc", "reliable_change_index"))

  apa <- nomo_apa_table(rt, number = 2)
  expect_identical(apa$source, "nomo_retest")
  expect_identical(names(apa$body)[3:4], c("ICC(A,1) [95% CI]", "ICC(C,1) [95% CI]"))
  expect_true(startsWith(apa$body[["ICC(A,1) [95% CI]"]], "."))
  expect_match(paste(apa$notes$general, collapse = " "), "Interval between occasions: two weeks.",
               fixed = TRUE)
  expect_false(grepl("Interval between",
                     paste(nomo_apa_table(steady)$notes$general, collapse = " "), fixed = TRUE))
})
