# Teaching datasets -------------------------------------------------------------
#
# The documented population features of the teaching datasets are relied on by
# examples and vignettes. These tests fail if regenerated data no longer show
# them; any intentional change must be recorded in NEWS.md.

test_that("teaching datasets have the documented structure", {
  expect_identical(dim(nomo_demo_continuous), c(500L, 10L))
  expect_identical(
    names(nomo_demo_continuous),
    c(paste0("a", 1:5), paste0("b", 1:5))
  )
  expect_true(all(vapply(nomo_demo_continuous, is.numeric, logical(1))))
  expect_identical(sum(is.na(nomo_demo_continuous)), 27L)
  expect_identical(sum(is.na(nomo_demo_continuous$a2)), 15L)
  expect_identical(sum(is.na(nomo_demo_continuous$b3)), 12L)

  expect_identical(dim(nomo_demo_ordinal), c(500L, 10L))
  expect_identical(names(nomo_demo_ordinal), names(nomo_demo_continuous))
  expect_true(all(vapply(nomo_demo_ordinal, is.ordered, logical(1))))
  expect_true(all(vapply(
    nomo_demo_ordinal,
    function(x) identical(levels(x), as.character(1:5)),
    logical(1)
  )))
  expect_false(anyNA(nomo_demo_ordinal))

  expect_identical(dim(nomo_demo_network), c(800L, 13L))
  expect_identical(
    names(nomo_demo_network),
    c(paste0("ag", 1:4), paste0("pe", 1:4), paste0("sd", 1:3), "Performance", "group")
  )
  expect_identical(levels(nomo_demo_network$group), c("online", "paper"))
  expect_identical(as.vector(table(nomo_demo_network$group)), c(400L, 400L))
  expect_false(anyNA(nomo_demo_network))

  expect_identical(dim(nomo_demo_longitudinal), c(500L, 12L))
  expect_identical(
    names(nomo_demo_longitudinal),
    paste0(rep(paste0("w", 1:4), 3L), "_", rep(c("t1", "t2", "t3"), each = 4L))
  )
  expect_false(anyNA(nomo_demo_longitudinal))
})


test_that("continuous teaching data show the documented structure and item flags", {
  skip_on_cran()
  fac <- nomo_factors(
    nomo_demo_continuous,
    criterion_set = "minimal",
    n_iter = 20,
    seed = 2026
  )
  efa <- nomo_efa(nomo_demo_continuous, factors = fac)
  items <- efa$item_summary

  expect_identical(ncol(efa$pattern_matrix), 2L)

  a_factor <- unique(items$primary_factor[items$item %in% paste0("a", 1:4)])
  b_factor <- unique(items$primary_factor[items$item %in% paste0("b", 1:5)])
  expect_length(a_factor, 1L)
  expect_length(b_factor, 1L)
  expect_false(identical(a_factor, b_factor))

  expect_true(items$cross_loading[items$item == "a5"])
  expect_false(any(items$cross_loading[items$item != "a5"]))
  expect_true(items$weak_primary[items$item == "b5"])
  expect_identical(items$attention[items$item == "b5"], "STRONG REVIEW")
})


test_that("ordinal teaching data use polychoric evidence for two factors", {
  skip_on_cran()

  fac <- nomo_factors(
    nomo_demo_ordinal,
    criterion_set = "minimal",
    n_iter = 20,
    seed = 2026
  )
  efa <- nomo_efa(nomo_demo_ordinal, factors = fac)

  expect_identical(fac$correlation, "polychoric")
  expect_identical(ncol(efa$pattern_matrix), 2L)
})


test_that("network teaching data reproduce the documented theory evidence", {
  skip_on_cran()

  model <- nomo_model(list(
    Agency = paste0("ag", 1:4),
    Persistence = paste0("pe", 1:4),
    SocialDesirability = paste0("sd", 1:3)
  ))
  h <- nomo_hypotheses(
    "Agency -> Persistence" = positive(min = .20),
    "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15)),
    "Agency -> Performance" = positive(),
    "Persistence -> Performance" = positive(min = .20)
  )

  net <- nomo_network(model, data = nomo_demo_network, hypotheses = h)
  evidence <- nomo_table(net, "hypotheses")

  expect_identical(
    evidence$concordance,
    c("concordant", "concordant", "concordant", "inconsistent")
  )
  expect_true(evidence$equivalence_supported[evidence$id == "H2"])
  expect_equal(evidence$estimate[evidence$id == "H1"], .45, tolerance = .10)
  expect_equal(evidence$estimate[evidence$id == "H3"], .40, tolerance = .10)
})


test_that("network teaching data localize the known scalar non-invariance", {
  skip_on_cran()

  inv <- nomo_invariance(
    "Agency =~ ag1 + ag2 + ag3 + ag4",
    data = nomo_demo_network,
    group = "group",
    levels = c("configural", "metric", "scalar")
  )
  strain <- nomo_table(inv, "local_strain")
  scalar <- strain[strain$level == "scalar", , drop = FALSE]

  expect_gt(nrow(scalar), 0L)
  expect_match(scalar$constraint_display[[1L]], "Intercept: ag3", fixed = TRUE)

  # The summary's "largest" diagnostics are the largest across all levels,
  # so the scalar intercept leads rather than the metric rows that come first
  # in the table (#60).
  top <- summary(inv)$top_local_strain
  expect_false(is.unsorted(rev(top$score_x2)))
  expect_match(top$constraint_display[[1L]], "Intercept: ag3", fixed = TRUE)
})


test_that("longitudinal teaching data localize the drifting intercept and recover the change", {
  skip_on_cran()
  release <- nomo_partial(
    level = "scalar", syntax = "w3 ~ 1",
    rationale = "The documented drift in the w3 intercept."
  )
  long <- nomo_invariance_longitudinal(
    "Wellbeing =~ w1 + w2 + w3 + w4",
    data = nomo_demo_longitudinal,
    occasions = c("t1", "t2", "t3"),
    levels = c("configural", "metric", "scalar"),
    partial = release
  )
  expect_gt(long$fit_evidence$lrt_p[long$fit_evidence$level == "scalar"], .05)
  means <- long$latent_means
  expect_equal(means$estimate, c(.30, .50), tolerance = .10)

  loadings <- lavaan::standardizedSolution(long$fits$configural)
  t1 <- loadings[loadings$op == "=~" & loadings$lhs == "Wellbeing_t1", "est.std"]
  expect_equal(t1, c(.80, .75, .70, .65), tolerance = .10)
})


test_that("walkthrough teaching data have the documented structure and features", {
  expect_identical(dim(nomo_demo_walkthrough), c(400L, 14L))
  items <- c(paste0("EF", 1:6), paste0("TF", 1:6))
  expect_identical(names(nomo_demo_walkthrough), c("respondent", "cohort", items))
  expect_identical(levels(nomo_demo_walkthrough$cohort), c("A", "B"))
  expect_identical(as.vector(table(nomo_demo_walkthrough$cohort)), c(200L, 200L))
  expect_true(all(vapply(nomo_demo_walkthrough[items], function(x) {
    is.integer(x) && all(x %in% 1:5)
  }, logical(1))))
  expect_identical(nomo_demo_walkthrough_items$item, items)
  expect_identical(nomo_demo_walkthrough_items$item[nomo_demo_walkthrough_items$reverse_worded],
                   c("EF2", "TF2"))

  # The handoff stored with the package is for these items.
  handoff <- readRDS(system.file("extdata", "content-handoff-walkthrough.rds",
                                 package = "nomologR"))
  expect_identical(handoff$item_evidence$item, items)

  walk <- nomo_demo_walkthrough
  # Answered as written, a reverse-worded item runs against its facet.
  expect_lt(stats::cor(walk$EF1, walk$EF2), 0)
  walk$EF2 <- 6L - walk$EF2
  walk$TF2 <- 6L - walk$TF2
  expect_gt(stats::cor(walk$EF1, walk$EF2), 0)

  rest <- function(item, facet) {
    others <- setdiff(facet, item)
    stats::cor(walk[[item]], rowSums(walk[others]))
  }
  ef <- c("EF1", "EF2", "EF3", "EF4", "EF6")
  # EF4 carries the least common variance of the carried EF items.
  expect_identical(names(which.min(vapply(ef, rest, numeric(1), facet = ef))), "EF4")
  # EF3's answers pile up at the top of the scale.
  expect_identical(names(which.min(vapply(walk[items], stats::sd, numeric(1)))), "EF3")
  expect_gt(mean(walk$EF3 == 5), .70)
  # TF6 is answered higher in cohort B; TF1 is not.
  shift <- function(item) diff(tapply(walk[[item]], walk$cohort, mean))[[1L]]
  expect_gt(shift("TF6"), .25)
  expect_lt(abs(shift("TF1")), .20)
  # TF4 shares more with Effort Regulation than any other TF item does.
  with_ef <- vapply(c("TF1", "TF2", "TF3", "TF4", "TF6"), function(item) {
    stats::cor(walk[[item]], rowSums(walk[ef]))
  }, numeric(1))
  expect_identical(names(which.max(with_ef)), "TF4")
})


test_that("dataset help pages write population values as the output prints them (#144)", {
  # A standardized loading, a path coefficient, and a difference in means or
  # intercepts can exceed 1, so they keep the leading zero; a correlation and a
  # probability cannot, so they drop it (guide point 8).
  page <- function(topic) {
    paste(vapply(c("\\description", "\\format", "\\details"),
                 function(section) nomo_test_rd_text(topic, section), character(1)),
          collapse = " ")
  }

  continuous <- page("nomo_demo_continuous")
  expect_match(continuous, "(population loadings 0.45 and 0.35)", fixed = TRUE)
  expect_match(continuous, "loadings are 0.80, 0.75, 0.70, 0.72, and 0.45", fixed = TRUE)
  expect_match(continuous, "with correlation .40", fixed = TRUE)

  network <- page("nomo_demo_network")
  expect_match(network, "standardized coefficient of 0.45", fixed = TRUE)
  expect_match(network, "regressed on Agency (0.40)", fixed = TRUE)
  expect_match(network, "mean is 0.25 SD higher", fixed = TRUE)
  expect_match(network, "is 0.50 higher", fixed = TRUE)
  expect_match(network, "(about .18 in the population)", fixed = TRUE)

  longitudinal <- page("nomo_demo_longitudinal")
  expect_match(longitudinal, "are 0.80, 0.75, 0.70, and 0.65", fixed = TRUE)
  expect_match(longitudinal, "to 0.30 at", fixed = TRUE)
  expect_match(longitudinal, "correlates .60 between adjacent occasions", fixed = TRUE)

  walkthrough <- page("nomo_demo_walkthrough")
  expect_match(walkthrough, "\\code{EF1} 0.72", fixed = TRUE)
  expect_match(walkthrough, "quantiles .10, .30, .60, and .85", fixed = TRUE)

  # No loading is left without its leading zero on any of the pages.
  pages <- c(continuous, network, longitudinal, walkthrough)
  expect_false(any(grepl("loadings?( are)? [.][0-9]", pages)))
  expect_false(any(grepl("[A-Z]{2}[0-9][}] [.][0-9]", pages)))
})
