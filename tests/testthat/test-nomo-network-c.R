make_network_fixture_c <- function(n = 420L, seed = 6801L) {
  set.seed(seed)

  A <- rnorm(n)
  B <- .45 * A + rnorm(n, sd = sqrt(1 - .45^2))
  C <- .04 * A + rnorm(n, sd = .999)
  criterion <- .45 * A + rnorm(n, sd = .85)

  data.frame(
    a1 = .82 * A + rnorm(n, sd = .55),
    a2 = .78 * A + rnorm(n, sd = .62),
    a3 = .75 * A + rnorm(n, sd = .66),
    b1 = .82 * B + rnorm(n, sd = .55),
    b2 = .77 * B + rnorm(n, sd = .63),
    b3 = .74 * B + rnorm(n, sd = .67),
    c1 = .82 * C + rnorm(n, sd = .55),
    c2 = .77 * C + rnorm(n, sd = .63),
    c3 = .74 * C + rnorm(n, sd = .67),
    criterion = criterion
  )
}


test_that("M6C propagates measurement context without replacing theory evidence", {
  dat <- make_network_fixture_c()
  model <- "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
  "

  out <- nomo_network(
    model,
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive(min = .20)
    )
  )

  expect_s3_class(out, "nomo_network")
  expect_true(is.list(out$measurement_context))
  expect_true(all(c(
    "summary", "loadings", "variances", "fit_reference_flags"
  ) %in% names(out$measurement_context)))
  expect_true(out$hypothesis_evidence$measurement_attention %in%
                c("info", "review", "concern"))
  expect_true("concordance" %in% names(out$hypothesis_evidence))
})


test_that("latent-to-observed criterion paths are first-class network evidence", {
  dat <- make_network_fixture_c(seed = 6802L)
  model <- "A =~ a1 + a2 + a3"

  out <- nomo_network(
    model,
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> criterion" = positive()
    )
  )

  expect_equal(
    out$hypothesis_evidence$evidence_scope,
    "latent_to_observed_outcome"
  )
  expect_equal(out$hypothesis_evidence$source_type, "latent")
  expect_equal(out$hypothesis_evidence$target_type, "observed")
  expect_gt(out$hypothesis_evidence$estimate, 0)
})


test_that("quantified negligible predictions use equivalence CI rather than p > .05", {
  dat <- make_network_fixture_c(n = 700L, seed = 6803L)

  model <- "
    A =~ a1 + a2 + a3
    C =~ c1 + c2 + c3
  "

  out <- nomo_network(
    model,
    data = dat,
    hypotheses = nomo_hypotheses(
      "A <-> C" = negligible(within = c(-.20, .20))
    ),
    equivalence_alpha = .05
  )

  row <- out$hypothesis_evidence

  expect_equal(row$equivalence_alpha, .05)
  expect_true(is.finite(row$equivalence_ci_lower))
  expect_true(is.finite(row$equivalence_ci_upper))
  expect_lt(
    row$equivalence_ci_upper - row$equivalence_ci_lower,
    row$ci_upper - row$ci_lower
  )
  expect_type(row$equivalence_supported, "logical")
  expect_match(row$interpretation, "not by p > .05", fixed = TRUE)
})


test_that("nomo_split feeds calibration and validation samples to the same network", {
  dat <- make_network_fixture_c(n = 700L, seed = 6804L)
  split <- nomo_split(dat, validation_prop = .40, seed = 2026)

  model <- "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
  "

  out <- nomo_network(
    model,
    data = split,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  expect_equal(out$sample_role, "calibration")
  expect_equal(out$split_source, "nomo_split")
  expect_equal(out$data_n, split$n_calibration)
  expect_equal(out$validation_n, split$n_validation)
  expect_true(is.list(out$validation))
  expect_true(inherits(out$validation$fit, "lavaan"))
  expect_equal(nrow(out$replication_evidence), 1L)
  expect_true(out$replication_evidence$replication_status %in% c(
    "replicated_concordance",
    "direction_replicated_but_uncertain",
    "mixed_or_inconclusive"
  ))
})


test_that("explicit validation data are supported without changing the model", {
  dat1 <- make_network_fixture_c(n = 400L, seed = 6805L)
  dat2 <- make_network_fixture_c(n = 400L, seed = 6806L)

  model <- "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
  "

  out <- nomo_network(
    model,
    data = dat1,
    validation_data = dat2,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  expect_equal(out$data_n, 400L)
  expect_equal(out$validation_n, 400L)
  expect_identical(
    out$model_fitted,
    out$validation$model_fitted
  )
  expect_true(out$model_relations$added_from_hypothesis)
})


test_that("report-ready network tables expose the intended evidence streams", {
  dat <- make_network_fixture_c(seed = 6807L)

  out <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
    ",
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  htab <- nomo_table(out, "hypotheses")
  expect_s3_class(htab, "tbl_df")
  expect_true(all(c(
    "relation", "estimate", "ci_lower", "ci_upper",
    "concordance", "measurement_attention"
  ) %in% names(htab)))

  mtab <- nomo_table(out, "measurement")
  expect_s3_class(mtab, "tbl_df")
  expect_true(all(c("attention", "observation") %in% names(mtab)))

  expect_s3_class(nomo_table(out$hypotheses), "tbl_df")
})


test_that("network presentation methods return stable ggplot objects", {
  dat1 <- make_network_fixture_c(n = 400L, seed = 6808L)
  dat2 <- make_network_fixture_c(n = 400L, seed = 6809L)

  out <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
    ",
    data = dat1,
    validation_data = dat2,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  p_effects <- plot(out, type = "effects")
  p_fit <- plot(out, type = "fit")
  p_rep <- plot(out, type = "replication")

  expect_s3_class(p_effects, "ggplot")
  expect_s3_class(p_fit, "ggplot")
  expect_s3_class(p_rep, "ggplot")

  expect_match(p_effects$labels$title, "Theory-specified")
  expect_match(p_rep$labels$title, "validation")
})
