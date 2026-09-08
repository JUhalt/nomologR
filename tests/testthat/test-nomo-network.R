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

# ---- recovered from hygiene consolidation: test-nomo-network.R ----
# ---- consolidated from test-checkpoint-c-network-edges.R ----
make_checkpoint_network_data <- function(n = 260L, seed = 7301L) {
  set.seed(seed)

  A <- rnorm(n)
  B <- .40 * A + rnorm(n, sd = .90)
  criterion <- .50 * A + rnorm(n, sd = .80)

  data.frame(
    a1 = .82 * A + rnorm(n, sd = .55),
    a2 = .78 * A + rnorm(n, sd = .62),
    a3 = .74 * A + rnorm(n, sd = .66),
    b1 = .82 * B + rnorm(n, sd = .55),
    b2 = .78 * B + rnorm(n, sd = .62),
    b3 = .74 * B + rnorm(n, sd = .66),
    criterion = criterion
  )
}


test_that("network helper regions cover inclusive, exclusive, and infinite bounds", {
  expect_true(nomologR:::nomo_network_in_region(
    .20, .20, Inf, TRUE, FALSE
  ))
  expect_false(nomologR:::nomo_network_in_region(
    .20, .20, Inf, FALSE, FALSE
  ))
  expect_true(nomologR:::nomo_network_in_region(
    -.20, -Inf, -.20, FALSE, TRUE
  ))
  expect_false(nomologR:::nomo_network_in_region(
    -.20, -Inf, -.20, FALSE, FALSE
  ))
  expect_true(is.na(nomologR:::nomo_network_in_region(
    NA_real_, 0, 1, TRUE, TRUE
  )))

  expect_true(nomologR:::nomo_network_interval_within_region(
    -.10, .10, -.10, .10, TRUE, TRUE
  ))
  expect_false(nomologR:::nomo_network_interval_within_region(
    -.10, .10, -.10, .10, FALSE, FALSE
  ))
  expect_true(nomologR:::nomo_network_interval_overlaps_region(
    -.30, .05, -.10, .10
  ))
  expect_false(nomologR:::nomo_network_interval_overlaps_region(
    .20, .30, -.10, .10
  ))
})


test_that("network direction and scope helpers distinguish supported estimands", {
  expect_true(nomologR:::nomo_network_direction_correct(.2, "positive"))
  expect_false(nomologR:::nomo_network_direction_correct(-.2, "positive"))
  expect_true(nomologR:::nomo_network_direction_correct(-.2, "negative"))
  expect_true(is.na(nomologR:::nomo_network_direction_correct(
    .2, "negligible"
  )))

  dat <- data.frame(y = 1:3)
  expect_equal(
    nomologR:::nomo_network_node_type("F", "F", dat),
    "latent"
  )
  expect_equal(
    nomologR:::nomo_network_node_type("y", "F", dat),
    "observed"
  )
  expect_equal(
    nomologR:::nomo_network_node_type("z", "F", dat),
    "unknown"
  )

  expect_equal(
    nomologR:::nomo_network_scope("latent", "latent", "directed"),
    "latent_structural"
  )
  expect_equal(
    nomologR:::nomo_network_scope("latent", "observed", "directed"),
    "latent_to_observed_outcome"
  )
  expect_equal(
    nomologR:::nomo_network_scope("observed", "latent", "directed"),
    "observed_to_latent"
  )
  expect_equal(
    nomologR:::nomo_network_scope("observed", "observed", "directed"),
    "observed_structural"
  )
  expect_equal(
    nomologR:::nomo_network_scope("latent", "latent", "association"),
    "latent_association"
  )
  expect_equal(
    nomologR:::nomo_network_scope("latent", "observed", "association"),
    "latent_observed_association"
  )
  expect_equal(
    nomologR:::nomo_network_scope("observed", "observed", "association"),
    "observed_association"
  )
})


test_that("equivalence CI helper handles valid and invalid uncertainty", {
  ci <- nomologR:::nomo_network_equivalence_ci(
    estimate = .05,
    se = .04,
    alpha = .05
  )

  expect_true(all(is.finite(ci)))
  expect_lt(ci[["lower"]], .05)
  expect_gt(ci[["upper"]], .05)

  expect_true(all(is.na(
    nomologR:::nomo_network_equivalence_ci(
      estimate = NA_real_,
      se = .04,
      alpha = .05
    )
  )))
  expect_true(all(is.na(
    nomologR:::nomo_network_equivalence_ci(
      estimate = .05,
      se = -.01,
      alpha = .05
    )
  )))
})


test_that("replication helper covers major discrepancy classifications", {
  make_fit <- function(
      estimate,
      concordance,
      id = "H1",
      relation = "A -> B",
      prediction = "positive") {
    list(
      hypothesis_evidence = tibble::tibble(
        id = id,
        relation = relation,
        prediction = prediction,
        estimate = estimate,
        concordance = concordance
      )
    )
  }

  expect_equal(
    nomologR:::nomo_network_replication_evidence(
      make_fit(.4, "concordant"),
      make_fit(.3, "concordant")
    )$replication_status,
    "replicated_concordance"
  )

  expect_equal(
    nomologR:::nomo_network_replication_evidence(
      make_fit(.4, "concordant"),
      make_fit(-.3, "inconsistent")
    )$replication_status,
    "sign_reversal"
  )

  expect_equal(
    nomologR:::nomo_network_replication_evidence(
      make_fit(.4, "concordant"),
      make_fit(.2, "inconsistent")
    )$replication_status,
    "not_replicated"
  )

  expect_equal(
    nomologR:::nomo_network_replication_evidence(
      make_fit(.2, "inconsistent"),
      make_fit(.3, "concordant")
    )$replication_status,
    "unstable"
  )

  expect_equal(
    nomologR:::nomo_network_replication_evidence(
      make_fit(.2, "inconsistent"),
      make_fit(.3, "inconsistent")
    )$replication_status,
    "replicated_inconsistency"
  )

  expect_equal(
    nomologR:::nomo_network_replication_evidence(
      make_fit(.2, "directionally_concordant_imprecise"),
      make_fit(.1, "inconclusive")
    )$replication_status,
    "direction_replicated_but_uncertain"
  )

  missing_validation <- list(
    hypothesis_evidence = tibble::tibble(
      id = "H2",
      relation = "A -> C",
      prediction = "positive",
      estimate = .2,
      concordance = "concordant"
    )
  )

  expect_equal(
    nomologR:::nomo_network_replication_evidence(
      make_fit(.2, "concordant"),
      missing_validation
    )$replication_status,
    "not_evaluable"
  )
})


test_that("network argument validation exercises researcher-control errors", {
  dat <- make_checkpoint_network_data()
  model <- "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
  "
  h <- nomo_hypotheses("A -> B" = positive())

  expect_error(
    nomo_network("", dat, h),
    "non-empty"
  )
  expect_error(
    nomo_network(model, list(), h),
    "non-empty data frame"
  )
  expect_error(
    nomo_network(model, dat, list()),
    "nomo_hypotheses"
  )
  expect_error(
    nomo_network(model, dat, h, add_missing = NA),
    "TRUE or FALSE"
  )
  expect_error(
    nomo_network(model, dat, h, std.lv = NA),
    "TRUE or FALSE"
  )
  expect_error(
    nomo_network(model, dat, h, equivalence_alpha = .5),
    "strictly between"
  )
  expect_error(
    nomo_network(
      model,
      dat,
      h,
      ordered = "missing_item"
    ),
    "not found"
  )
  expect_error(
    nomo_network(
      model,
      dat,
      h,
      estimator = ""
    ),
    "non-empty"
  )
  expect_error(
    nomo_network(
      model,
      dat,
      h,
      missing = ""
    ),
    "non-empty"
  )
})


test_that("nomo_split cannot silently accept a second validation sample", {
  dat <- make_checkpoint_network_data(n = 300L, seed = 7302L)
  split <- nomo_split(dat, validation_prop = .40, seed = 2026)

  model <- "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
  "

  expect_error(
    nomo_network(
      model,
      data = split,
      validation_data = dat,
      hypotheses = nomo_hypotheses(
        "A -> B" = positive()
      )
    ),
    "Do not supply"
  )
})


test_that("unstandardized theory expectations use unstandardized estimates", {
  dat <- make_checkpoint_network_data(n = 420L, seed = 7303L)

  out <- nomo_network(
    "A =~ a1 + a2 + a3",
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> criterion" = positive(
        min = .10,
        scale = "unstandardized"
      )
    )
  )

  row <- out$hypothesis_evidence

  expect_equal(row$scale, "unstandardized")
  expect_equal(row$estimate, row$estimate_unstandardized)
  expect_true(is.finite(row$estimate_standardized))
})


test_that("ordered network path uses explicit categorical estimator defaults", {
  set.seed(7304L)
  n <- 320L
  A <- rnorm(n)
  B <- .35 * A + rnorm(n, sd = .90)

  make_ord <- function(x) {
    cut(
      x,
      breaks = c(-Inf, -1, -.35, .35, 1, Inf),
      labels = FALSE
    )
  }

  dat <- data.frame(
    a1 = make_ord(.8 * A + rnorm(n, sd = .6)),
    a2 = make_ord(.8 * A + rnorm(n, sd = .6)),
    a3 = make_ord(.7 * A + rnorm(n, sd = .7)),
    b1 = make_ord(.8 * B + rnorm(n, sd = .6)),
    b2 = make_ord(.8 * B + rnorm(n, sd = .6)),
    b3 = make_ord(.7 * B + rnorm(n, sd = .7))
  )

  out <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
    ",
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    ),
    ordered = names(dat)
  )

  expect_equal(out$estimator, "WLSMV")
  expect_equal(out$estimator_source, "ordered_default")
  expect_equal(length(out$ordered), 6L)
})


test_that("network table method covers every evidence stream", {
  dat1 <- make_checkpoint_network_data(n = 360L, seed = 7305L)
  dat2 <- make_checkpoint_network_data(n = 360L, seed = 7306L)

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

  for (type in c(
    "hypotheses",
    "fit",
    "measurement",
    "relations",
    "replication",
    "decision_log"
  )) {
    expect_s3_class(nomo_table(out, type), "data.frame")
  }

  no_rep <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
    ",
    data = dat1,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  expect_equal(nrow(nomo_table(no_rep, "replication")), 0L)
})


test_that("network presentation covers concordance and missing-replication branches", {
  dat <- make_checkpoint_network_data(n = 340L, seed = 7307L)

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

  expect_s3_class(plot(out, type = "concordance"), "ggplot")

  expect_error(
    plot(out, type = "replication"),
    "No validation sample"
  )

  expect_output(print(summary(out)), "Model fit evidence")
})

# ---- consolidated from test-nomo-network-c.R ----
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

# ---- consolidated from test-nomo-network-hardening.R ----
make_network_truth_fixture <- function(n = 900L,
                                       beta = .50,
                                       weak_loading = FALSE,
                                       seed = 7101L) {
  set.seed(seed)

  A <- rnorm(n)
  B <- beta * A + rnorm(n, sd = 1)

  a3_loading <- if (isTRUE(weak_loading)) .15 else .75

  data.frame(
    a1 = .84 * A + rnorm(n, sd = .55),
    a2 = .80 * A + rnorm(n, sd = .60),
    a3 = a3_loading * A + rnorm(n, sd = .98),
    b1 = .84 * B + rnorm(n, sd = .55),
    b2 = .80 * B + rnorm(n, sd = .60),
    b3 = .75 * B + rnorm(n, sd = .65)
  )
}


network_truth_model <- function() {
  "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
  "
}


test_that("known opposite-direction population is classified as inconsistent", {
  dat <- make_network_truth_fixture(
    n = 1000L,
    beta = -.55,
    seed = 7102L
  )

  out <- nomo_network(
    network_truth_model(),
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  row <- out$hypothesis_evidence

  expect_true(out$converged)
  expect_lt(row$estimate, 0)
  expect_lt(row$ci_upper, 0)
  expect_equal(row$concordance, "inconsistent")
})


test_that("correct direction below a prespecified magnitude remains distinct from concordance", {
  dat <- make_network_truth_fixture(
    n = 1400L,
    beta = .08,
    seed = 7103L
  )

  out <- nomo_network(
    network_truth_model(),
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive(min = .30)
    )
  )

  row <- out$hypothesis_evidence

  expect_gt(row$estimate, 0)
  expect_lt(row$estimate, .30)
  expect_equal(
    row$concordance,
    "direction_concordant_below_magnitude"
  )
})


test_that("weak measurement propagates a review flag without rewriting theory evidence", {
  dat <- make_network_truth_fixture(
    n = 1100L,
    beta = .45,
    weak_loading = TRUE,
    seed = 7104L
  )

  out <- nomo_network(
    network_truth_model(),
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  expect_true(out$converged)
  expect_true(
    out$measurement_context$summary$attention %in% c("review", "concern")
  )
  expect_gte(
    out$measurement_context$summary$loading_review_flags,
    1L
  )
  expect_true(
    out$hypothesis_evidence$measurement_attention %in% c("review", "concern")
  )
  expect_match(
    out$hypothesis_evidence$interpretation,
    "Measurement context also requires review"
  )
})


test_that("primary-validation sign reversal is surfaced as a replication discrepancy", {
  primary <- make_network_truth_fixture(
    n = 900L,
    beta = .55,
    seed = 7105L
  )
  validation <- make_network_truth_fixture(
    n = 900L,
    beta = -.55,
    seed = 7106L
  )

  out <- nomo_network(
    network_truth_model(),
    data = primary,
    validation_data = validation,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  rep <- out$replication_evidence

  expect_gt(rep$primary_estimate, 0)
  expect_lt(rep$validation_estimate, 0)
  expect_equal(rep$replication_status, "sign_reversal")
  expect_match(rep$interpretation, "changes sign")
})


test_that("validation fit preserves the exact prespecified fitted model", {
  primary <- make_network_truth_fixture(
    n = 700L,
    beta = .45,
    seed = 7107L
  )
  validation <- make_network_truth_fixture(
    n = 700L,
    beta = .45,
    seed = 7108L
  )

  out <- nomo_network(
    network_truth_model(),
    data = primary,
    validation_data = validation,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  expect_identical(
    out$model_fitted,
    out$validation$model_fitted
  )
  expect_true(out$model_relations$added_from_hypothesis)
})


test_that("hardening keeps relation evidence report-ready and non-binary", {
  dat <- make_network_truth_fixture(
    n = 800L,
    beta = .45,
    seed = 7109L
  )

  out <- nomo_network(
    network_truth_model(),
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive(min = .20)
    )
  )

  tab <- nomo_table(out, "hypotheses")

  expect_true(all(c(
    "estimate",
    "se",
    "ci_lower",
    "ci_upper",
    "p_value",
    "concordance",
    "measurement_attention",
    "confirmatory_status"
  ) %in% names(tab)))

  expect_false(any(c(
    "valid",
    "invalid",
    "pass",
    "fail"
  ) %in% names(tab)))
})
