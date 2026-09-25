# Fixtures ---------------------------------------------------------------------

# Four scales of six items, two reverse keyed per scale, with 15 injected
# straight-liners and 15 injected random responders. Four scales give even-odd
# consistency a correlation over four points, and the items yield enough
# antonym and synonym pairs for the within-person correlations to exist.
effort_data <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    set.seed(34)
    n <- 400
    fs <- replicate(4, stats::rnorm(n))
    make <- function(f) pmin(5, pmax(1, round(3 + f + stats::rnorm(n, sd = .7))))
    cols <- list()
    reverse <- character()
    scales <- list()
    for (s in 1:4) {
      nm <- paste0("s", s, "_", 1:6)
      for (j in 1:6) {
        v <- make(fs[, s])
        if (j %in% c(2, 5)) {
          v <- 6 - v
          reverse <- c(reverse, nm[j])
        }
        cols[[nm[j]]] <- v
      }
      scales[[paste0("S", s)]] <- nm
    }
    dat <- as.data.frame(cols)
    dat[1:15, ] <- 3
    dat[16:30, ] <- matrix(sample(1:5, 15 * ncol(dat), TRUE), 15, ncol(dat))
    cache <<- list(
      data = dat, scales = scales, reverse = reverse,
      group = c(rep("straight", 15), rep("random", 15), rep("attentive", n - 30))
    )
    cache
  }
})


effort_screen <- function() {
  fx <- effort_data()
  nomo_screen(
    fx$data, effort = TRUE, scales = fx$scales, reverse = fx$reverse,
    scale_range = c(1, 5)
  )
}


# Known answers from the source ------------------------------------------------

test_that("inter-item standard deviation reproduces Curran's worked example", {
  # Curran (2016, p. 11): three ten-item response strings on a five-point scale,
  # with inter-item standard deviations of 0, about 1.49, and 2.11.
  strings <- rbind(
    c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
    c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    c(1, 1, 1, 1, 1, 5, 5, 5, 5, 5)
  )
  expect_equal(
    round(nomologR:::nomo_effort_inter_item_sd(strings), 2),
    c(0, 1.49, 2.11)
  )
})


test_that("the psychometric antonym coefficient reproduces Curran's worked example", {
  # Curran (2016, p. 11): antonym vectors [3, 4, 5] and [2, 2, 1] correlate
  # -.866. Each pair gets its own independent factor, so the only strong
  # negative correlations in the data are within the three intended pairs and
  # the data-driven pairing recovers exactly those. A single shared factor
  # would make cross pairs such as (x1, y2) just as strongly negative.
  set.seed(2)
  n <- 200
  f <- replicate(3, stats::rnorm(n))
  base <- data.frame(
    x1 = f[, 1] + stats::rnorm(n, sd = .2), x2 = -f[, 1] + stats::rnorm(n, sd = .2),
    y1 = f[, 2] + stats::rnorm(n, sd = .2), y2 = -f[, 2] + stats::rnorm(n, sd = .2),
    z1 = f[, 3] + stats::rnorm(n, sd = .2), z2 = -f[, 3] + stats::rnorm(n, sd = .2)
  )
  base[1, ] <- c(3, 2, 4, 2, 5, 1)

  out <- nomologR:::nomo_effort_pairs(as.matrix(base), "antonym")
  expect_identical(nrow(out$pairs), 3L)
  expect_setequal(
    paste(out$pairs$first, out$pairs$second),
    c("x1 x2", "y1 y2", "z1 z2")
  )
  expect_equal(round(out$values[[1L]], 3), -0.866)
})


test_that("long-string is the longest run of identical consecutive responses", {
  strings <- rbind(
    c(4, 4, 4, 4, 4, 4, 4, 4),
    c(1, 2, 3, 4, 5, 4, 3, 2),
    c(2, 2, 2, 2, 2, 5, 1, 3)
  )
  expect_equal(nomologR:::nomo_effort_long_string(strings), c(8, 1, 5))
})


test_that("inter-item standard deviation is averaged within scales, as Marjanovic et al. computed it", {
  # A respondent high on one scale and low on another is consistent within
  # each, yet looks more variable than a random responder if one standard
  # deviation is taken across every item.
  strings <- rbind(
    contrast = c(5, 5, 5, 5, 1, 1, 1, 1),
    random = c(1, 5, 2, 4, 5, 1, 4, 2)
  )
  colnames(strings) <- paste0("x", 1:8)
  scales <- list(A = paste0("x", 1:4), B = paste0("x", 5:8))

  overall <- nomologR:::nomo_effort_inter_item_sd(strings)
  within <- nomologR:::nomo_effort_inter_item_sd_mean(strings, scales)

  expect_gt(overall[["contrast"]], overall[["random"]])
  expect_equal(within[[1L]], 0)
  expect_gt(within[[2L]], within[[1L]])
})


# Degenerate correlations ------------------------------------------------------

test_that("a within-person correlation over two points is never reported", {
  # With two antonym pairs every respondent's correlation is exactly +1 or -1,
  # which would flag attentive respondents by arithmetic alone.
  set.seed(3)
  n <- 150
  f <- stats::rnorm(n)
  two_pairs <- cbind(
    a1 = f + stats::rnorm(n, sd = .3), a2 = -f + stats::rnorm(n, sd = .3),
    b1 = f + stats::rnorm(n, sd = .3), b2 = -f + stats::rnorm(n, sd = .3)
  )
  out <- nomologR:::nomo_effort_pairs(two_pairs, "antonym")
  expect_identical(nrow(out$pairs), 2L)
  expect_true(all(is.na(out$values)))

  # The same holds for even-odd consistency across two scales, where the
  # Spearman-Brown correction would otherwise divide by zero.
  two_scales <- list(A = c("a1", "a2"), B = c("b1", "b2"))
  expect_true(all(is.na(nomologR:::nomo_effort_even_odd(two_pairs, two_scales))))
})


test_that("even-odd consistency never returns a divergent correction", {
  skip_on_cran()
  out <- effort_screen()
  finite <- out$effort$even_odd[is.finite(out$effort$even_odd)]
  expect_gt(length(finite), 0L)
  expect_true(all(abs(finite) < 100))
})


# Behavior on injected cases ---------------------------------------------------

test_that("the indices separate straight-lining, random, and attentive responding", {
  skip_on_cran()
  fx <- effort_data()
  e <- effort_screen()$effort
  mean_of <- function(x) mean(x[is.finite(x)])

  straight <- e[fx$group == "straight", ]
  random <- e[fx$group == "random", ]
  attentive <- e[fx$group == "attentive", ]

  expect_true(all(straight$flag_long_string))
  expect_false(any(attentive$flag_long_string))

  # Inter-item standard deviation detects random responding, and rates the
  # straight-liners as perfectly consistent.
  expect_gt(mean_of(random$inter_item_sd_mean), mean_of(attentive$inter_item_sd_mean))
  expect_true(all(straight$inter_item_sd == 0))

  expect_gt(mean_of(attentive$even_odd), mean_of(random$even_odd))
  expect_lt(mean_of(attentive$antonym_r), mean_of(random$antonym_r))
  expect_gt(mean_of(attentive$synonym_r), mean_of(random$synonym_r))
})


test_that("the disagreement between long-string and inter-item SD is reported", {
  skip_on_cran()
  log <- effort_screen()$decision_log
  entry <- log[log$metric == "index_disagreement", ]

  expect_identical(nrow(entry), 1L)
  expect_identical(entry$value[[1L]], 15)
  expect_match(entry$recommendation, "best possible score", fixed = TRUE)
})


test_that("few pairs are disclosed as making the flags coarse", {
  skip_on_cran()
  log <- effort_screen()$decision_log
  pairs <- log[log$metric %in% c("psychometric_antonym", "psychometric_synonym"), ]

  expect_identical(nrow(pairs), 2L)
  expect_true(all(grepl("computed here over", pairs$reference, fixed = TRUE)))
  expect_true(any(grepl("arithmetic rather than behavior", pairs$recommendation,
                        fixed = TRUE)))
})


# Contract ---------------------------------------------------------------------

test_that("nomo_screen never modifies data or recodes what it returns", {
  skip_on_cran()
  fx <- effort_data()
  before <- fx$data
  out <- effort_screen()

  expect_identical(fx$data, before)
  expect_identical(nrow(out$effort), nrow(before))
  expect_identical(out$effort_settings$reverse, fx$reverse)
})


test_that("existing output is unchanged when effort is not requested", {
  skip_on_cran()
  fx <- effort_data()
  plain <- nomo_screen(fx$data)

  expect_null(plain$effort)
  expect_null(plain$effort_pairs)
  expect_false(any(grepl("psychometric|long_string|even_odd",
                         plain$decision_log$metric)))
})


test_that("keying and range are never inferred", {
  fx <- effort_data()

  expect_error(
    nomo_screen(fx$data, effort = TRUE, reverse = fx$reverse),
    "not inferred"
  )
  expect_error(
    nomo_screen(fx$data, effort = TRUE, reverse = "nope", scale_range = c(1, 5)),
    "not being screened"
  )
  expect_error(
    nomo_screen(fx$data, effort = TRUE, scale_range = c(5, 1)),
    "min below max"
  )
  expect_error(nomo_screen(fx$data, effort = NA), "TRUE or FALSE")
  expect_error(nomo_screen(fx$data, effort = TRUE, pair_magnitude = 1.5),
               "between 0 and 1")
  expect_error(
    nomo_screen(fx$data, effort = TRUE, scales = list(A = "missing_item")),
    "not being screened"
  )

  characters <- fx$data
  characters$s1_1 <- as.character(characters$s1_1)
  expect_error(nomo_screen(characters, effort = TRUE), "numeric responses")
})


test_that("an undeclared keying is disclosed where it would mislead", {
  skip_on_cran()
  fx <- effort_data()
  out <- nomo_screen(fx$data, effort = TRUE, scales = fx$scales)
  entry <- out$decision_log[out$decision_log$metric == "even_odd", ]

  expect_match(entry$recommendation, "No reverse keying was declared", fixed = TRUE)
})


# Crediting --------------------------------------------------------------------

test_that("careless-responding indices are credited only when they were computed", {
  skip_on_cran()
  fx <- effort_data()

  full <- nomo_methods_used(effort_screen())
  expect_true(all(c(
    "long_string", "inter_item_sd", "mahalanobis_screen",
    "even_odd_consistency", "psychometric_antonyms", "psychometric_synonyms"
  ) %in% full))

  # No screen requested, no citation for one.
  plain <- nomo_methods_used(nomo_screen(fx$data))
  expect_false(any(c("long_string", "inter_item_sd") %in% plain))

  # Without scales, even-odd consistency is never computed and so never cited.
  unscaled <- nomo_methods_used(nomo_screen(fx$data, effort = TRUE))
  expect_true("long_string" %in% unscaled)
  expect_false("even_odd_consistency" %in% unscaled)

  expect_true(all(full %in% nomo_methods()$id))
})


test_that("the printed screen summarizes careless-responding flags", {
  skip_on_cran()
  expect_output(print(effort_screen()), "Careless-responding flags")
  expect_output(print(effort_screen()), "never removed")
  expect_false(any(grepl(
    "Careless-responding",
    utils::capture.output(print(nomo_screen(effort_data()$data)))
  )))
})


test_that("nomo_table returns each part of a screen", {
  skip_on_cran()
  out <- effort_screen()

  expect_identical(nomo_table(out, "items"), out$item_summary)
  expect_identical(nomo_table(out, "cases"), out$case_summary)
  expect_identical(nomo_table(out, "effort"), out$effort)
  expect_identical(nomo_table(out, "decision_log"), out$decision_log)

  # Asking for indices that were never computed says how to get them.
  expect_error(
    nomo_table(nomo_screen(effort_data()$data), "effort"),
    "effort = TRUE"
  )
})


test_that("declaring no reverse-keyed items differs from not declaring keying", {
  skip_on_cran()
  fx <- effort_data()
  metric <- function(out) {
    out$decision_log$recommendation[out$decision_log$metric == "even_odd"]
  }

  # NULL: nobody said, so the log warns that the index may be wrong.
  undeclared <- nomo_screen(fx$data, effort = TRUE, scales = fx$scales)
  expect_match(metric(undeclared), "No reverse keying was declared", fixed = TRUE)

  # character(0): someone checked and nothing is reversed. No range is needed,
  # because nothing is recoded, and the log must not claim keying was missing.
  checked <- nomo_screen(fx$data, effort = TRUE, scales = fx$scales,
                         reverse = character(0))
  expect_match(metric(checked), "declared with no reverse-keyed items", fixed = TRUE)
  expect_no_match(metric(checked), "No reverse keying was declared", fixed = TRUE)
})


# Edge cases (#72) -------------------------------------------------------------

test_that("per-scale indices use only scales with at least two items", {
  resp <- cbind(a1 = c(1, 2, 3), a2 = c(1, 3, 5), b1 = c(2, 2, 2))
  scales <- list(A = c("a1", "a2"), B = "b1")

  # Only A contributes: runs of (1, 1), (2, 3), (3, 5).
  expect_equal(nomologR:::nomo_effort_long_string_mean(resp, scales), c(2, 1, 1))
  expect_equal(
    nomologR:::nomo_effort_inter_item_sd_mean(resp, scales),
    c(0, stats::sd(c(2, 3)), stats::sd(c(3, 5)))
  )

  # A single respondent still gets one value per person.
  one <- resp[1, , drop = FALSE]
  expect_equal(nomologR:::nomo_effort_long_string_mean(one, scales), 2)
  expect_equal(nomologR:::nomo_effort_inter_item_sd_mean(one, scales), 0)

  # With no scale of two items there is no value, never a mean over nothing.
  single <- list(B = "b1")
  expect_true(all(is.na(nomologR:::nomo_effort_long_string_mean(resp, single))))
  expect_true(all(is.na(nomologR:::nomo_effort_inter_item_sd_mean(resp, single))))
})


test_that("a respondent with fewer than two answers has no inter-item SD", {
  resp <- rbind(c(1, NA, NA), c(1, 2, 3))
  expect_equal(nomologR:::nomo_effort_inter_item_sd(resp), c(NA, 1))
})


test_that("Mahalanobis distance is refused rather than guessed", {
  # No more complete cases than items: the covariance cannot be estimated.
  few <- matrix(c(1, 2, 3, 2, 3, 4, 3, 1, 2), 3, 3)
  expect_true(all(is.na(nomologR:::nomo_effort_mahalanobis(few))))

  # A singular covariance has no inverse.
  set.seed(72)
  x <- stats::rnorm(20)
  collinear <- cbind(x, 2 * x, stats::rnorm(20))
  expect_true(all(is.na(nomologR:::nomo_effort_mahalanobis(collinear))))
})


test_that("psychometric pairs need four items, variance, and a pair past the threshold", {
  set.seed(72)
  three <- matrix(stats::rnorm(60), 20, 3, dimnames = list(NULL, c("a", "b", "c")))
  expect_identical(nrow(nomologR:::nomo_effort_pairs(three, "antonym")$pairs), 0L)

  independent <- matrix(stats::rnorm(400), 100, 4, dimnames = list(NULL, letters[1:4]))
  out <- nomologR:::nomo_effort_pairs(independent, "synonym")
  expect_identical(nrow(out$pairs), 0L)
  expect_true(all(is.na(out$values)))

  # A constant item has no correlation; it is never paired, and the pairs among
  # the other items are still found.
  f <- stats::rnorm(100)
  with_constant <- cbind(
    a1 = f + stats::rnorm(100, sd = .2), a2 = f + stats::rnorm(100, sd = .2),
    b1 = f + stats::rnorm(100, sd = .2), b2 = f + stats::rnorm(100, sd = .2),
    c1 = f + stats::rnorm(100, sd = .2), c2 = f + stats::rnorm(100, sd = .2),
    k = 3
  )
  out <- nomologR:::nomo_effort_pairs(with_constant, "synonym")
  expect_identical(nrow(out$pairs), 3L)
  expect_false("k" %in% c(out$pairs$first, out$pairs$second))

  # A respondent who answered too few of the paired items gets no value.
  with_constant[1, c("a1", "b1", "c1")] <- NA
  out <- nomologR:::nomo_effort_pairs(with_constant, "synonym")
  expect_true(is.na(out$values[[1L]]))
  expect_true(is.finite(out$values[[2L]]))
})


test_that("even-odd consistency needs three scales with both halves answered", {
  set.seed(72)
  f <- stats::rnorm(50)
  resp <- cbind(
    a1 = f + stats::rnorm(50), a2 = f + stats::rnorm(50),
    b1 = f + stats::rnorm(50), b2 = f + stats::rnorm(50),
    c1 = f + stats::rnorm(50), c2 = f + stats::rnorm(50)
  )
  scales <- list(A = c("a1", "a2"), B = c("b1", "b2"), C = c("c1", "c2"))

  # Two scales missing leave one point; one scale missing leaves two, and a
  # correlation over two points is always +1 or -1, so neither is reported.
  resp[1, c("a1", "a2", "b1", "b2")] <- NA
  resp[2, c("a1", "a2")] <- NA
  out <- nomologR:::nomo_effort_even_odd(resp, scales)
  expect_true(is.na(out[[1L]]))
  expect_true(is.na(out[[2L]]))
  expect_true(any(is.finite(out[-(1:2)])))
})


test_that("the log says when too few pairs exist, and when enough make a flag firm", {
  # Items that all correlate positively offer no antonym pairs.
  few <- nomo_screen(nomo_demo_continuous, effort = TRUE)$decision_log
  antonym <- few[few$metric == "psychometric_antonym", ]
  expect_identical(nrow(antonym), 1L)
  expect_match(antonym$observation, "so the index was not computed", fixed = TRUE)
  expect_match(antonym$recommendation, "at least three", fixed = TRUE)

  # Six strong synonym pairs: past Meade and Craig's five, so the note is
  # Curran's, not the caution about few pairs.
  set.seed(72)
  n <- 300
  pairs <- lapply(1:6, function(k) {
    f <- stats::rnorm(n)
    cbind(round(3 + f + stats::rnorm(n, sd = .3)), round(3 + f + stats::rnorm(n, sd = .3)))
  })
  many <- as.data.frame(do.call(cbind, pairs))
  names(many) <- paste0("i", seq_len(ncol(many)))
  log <- nomo_screen(many, effort = TRUE)$decision_log
  synonym <- log[log$metric == "psychometric_synonym", ]
  expect_identical(nrow(synonym), 1L)
  expect_match(synonym$recommendation, "close to certain evidence", fixed = TRUE)
})


test_that("effort arguments are validated, and every screen table is returned", {
  fx <- effort_data()
  expect_error(nomo_screen(fx$data, effort = TRUE, scales = "s1_1"),
               "must be a list of character vectors", fixed = TRUE)
  expect_error(nomo_screen(fx$data, effort = TRUE, scales = list(A = 1:3)),
               "must be a list of character vectors", fixed = TRUE)
  expect_error(nomo_screen(fx$data, effort = TRUE, reverse = 1),
               "`reverse` must be a character vector", fixed = TRUE)

  out <- nomo_screen(fx$data)
  expect_identical(nomo_table(out, "distribution"), out$response_distribution)
  expect_identical(nomo_table(out, "relationships"), out$relationship_summary)
})
