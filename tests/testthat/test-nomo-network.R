make_network_fixture <- function(n = 350L, seed = 6201L) {
  set.seed(seed)

  A <- rnorm(n)
  B <- .45 * A + rnorm(n, sd = sqrt(1 - .45^2))
  criterion <- .50 * A + rnorm(n, sd = .85)

  data.frame(
    a1 = .82 * A + rnorm(n, sd = .55),
    a2 = .78 * A + rnorm(n, sd = .62),
    a3 = .75 * A + rnorm(n, sd = .66),
    b1 = .82 * B + rnorm(n, sd = .55),
    b2 = .77 * B + rnorm(n, sd = .63),
    b3 = .74 * B + rnorm(n, sd = .67),
    criterion = criterion
  )
}


test_that("nomo_network transparently adds theory paths and matches estimates", {
  dat <- make_network_fixture()

  measurement <- "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
  "

  h <- nomo_hypotheses(
    "A -> B" = positive(min = .20),
    "A -> criterion" = positive()
  )

  out <- nomo_network(
    measurement,
    data = dat,
    hypotheses = h
  )

  expect_s3_class(out, "nomo_network")
  expect_true(out$converged)
  expect_true(inherits(out$fit, "lavaan"))

  expect_true(all(out$model_relations$added_from_hypothesis))
  expect_match(out$model_fitted, "B ~ A", fixed = TRUE)
  expect_match(out$model_fitted, "criterion ~ A", fixed = TRUE)

  expect_equal(nrow(out$hypothesis_evidence), 2L)
  expect_true(all(is.finite(out$hypothesis_evidence$estimate)))
  expect_true(all(is.finite(out$hypothesis_evidence$estimate_standardized)))
  expect_true(all(out$hypothesis_evidence$estimate > 0))

  expect_true(all(out$hypothesis_evidence$concordance %in% c(
    "concordant",
    "directionally_concordant_imprecise"
  )))
})


test_that("nomo_network does not duplicate paths already in model", {
  dat <- make_network_fixture(seed = 6202L)

  model <- "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
    B ~ A
  "

  h <- nomo_hypotheses(
    "A -> B" = positive()
  )

  out <- nomo_network(
    model,
    data = dat,
    hypotheses = h
  )

  expect_false(out$model_relations$added_from_hypothesis)
  expect_true(out$model_relations$already_in_model)
})


test_that("association predictions map to covariance parameters", {
  set.seed(6203)
  n <- 350
  common <- rnorm(n)
  A <- .7 * common + rnorm(n, sd = .7)
  C <- .6 * common + rnorm(n, sd = .8)

  dat <- data.frame(
    a1 = .8 * A + rnorm(n, sd = .6),
    a2 = .8 * A + rnorm(n, sd = .6),
    a3 = .7 * A + rnorm(n, sd = .7),
    c1 = .8 * C + rnorm(n, sd = .6),
    c2 = .8 * C + rnorm(n, sd = .6),
    c3 = .7 * C + rnorm(n, sd = .7)
  )

  model <- "
    A =~ a1 + a2 + a3
    C =~ c1 + c2 + c3
  "

  h <- nomo_hypotheses(
    "A <-> C" = positive()
  )

  out <- nomo_network(
    model,
    data = dat,
    hypotheses = h
  )

  expect_true(is.finite(out$hypothesis_evidence$estimate))
  expect_gt(out$hypothesis_evidence$estimate, 0)
  expect_equal(out$hypothesis_evidence$relation_type, "association")
})


test_that("negligible predictions require a quantitative region for confirmation", {
  set.seed(6204)
  n <- 500

  A <- rnorm(n)
  C <- rnorm(n)

  dat <- data.frame(
    a1 = .8 * A + rnorm(n, sd = .6),
    a2 = .8 * A + rnorm(n, sd = .6),
    a3 = .8 * A + rnorm(n, sd = .6),
    c1 = .8 * C + rnorm(n, sd = .6),
    c2 = .8 * C + rnorm(n, sd = .6),
    c3 = .8 * C + rnorm(n, sd = .6)
  )

  model <- "
    A =~ a1 + a2 + a3
    C =~ c1 + c2 + c3
  "

  bare <- nomo_network(
    model,
    data = dat,
    hypotheses = nomo_hypotheses(
      "A <-> C" = negligible()
    )
  )

  expect_equal(
    bare$hypothesis_evidence$concordance,
    "not_confirmable_without_sesoi"
  )

  bounded <- nomo_network(
    model,
    data = dat,
    hypotheses = nomo_hypotheses(
      "A <-> C" = negligible(within = c(-.20, .20))
    )
  )

  expect_true(bounded$hypothesis_evidence$concordance %in% c(
    "concordant",
    "directionally_concordant_imprecise"
  ))
})


test_that("post-hoc hypotheses remain explicitly exploratory", {
  dat <- make_network_fixture(seed = 6205L)

  model <- "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
  "

  h <- nomo_hypotheses(
    "A -> B" = positive(origin = "post_hoc")
  )

  out <- nomo_network(
    model,
    data = dat,
    hypotheses = h
  )

  expect_equal(
    out$hypothesis_evidence$confirmatory_status,
    "post_hoc_exploratory"
  )
  expect_match(
    out$hypothesis_evidence$interpretation,
    "exploratory"
  )
})


test_that("nomo_network validates unknown nodes and optional model augmentation", {
  dat <- make_network_fixture(seed = 6206L)

  model <- "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
  "

  expect_error(
    nomo_network(
      model,
      data = dat,
      hypotheses = nomo_hypotheses(
        "A -> MissingThing" = positive()
      )
    ),
    "neither latent variables"
  )

  out <- nomo_network(
    model,
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    ),
    add_missing = FALSE
  )

  expect_false(out$model_relations$added_from_hypothesis)
})


test_that("network print and summary expose theory evidence without validity verdicts", {
  dat <- make_network_fixture(seed = 6207L)

  model <- "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
  "

  out <- nomo_network(
    model,
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  expect_output(print(out), "Statistical significance alone")
  s <- summary(out)
  expect_s3_class(s, "summary_nomo_network")
  expect_output(print(s), "Hypothesis evidence")
})
