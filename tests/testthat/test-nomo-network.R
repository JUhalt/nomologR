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
      prediction = "positive",
      ci_lower = estimate - .1,
      ci_upper = estimate + .1) {
    list(
      hypothesis_evidence = tibble::tibble(
        id = id,
        relation = relation,
        prediction = prediction,
        estimate = estimate,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
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

  # A sign change is a reversal only when both intervals exclude zero on
  # opposite sides (#30).
  reversal <- nomologR:::nomo_network_replication_evidence(
    make_fit(.40, "concordant", ci_lower = .30, ci_upper = .50),
    make_fit(-.30, "inconsistent", ci_lower = -.40, ci_upper = -.20)
  )
  expect_equal(reversal$replication_status, "sign_reversal")
  expect_match(reversal$interpretation, "both confidence intervals exclude zero")

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


test_that("a sign change near zero is not labeled a reversal (#30)", {
  fit_with <- function(estimate, ci_lower, ci_upper, concordance,
                       prediction = "positive") {
    list(
      hypothesis_evidence = tibble::tibble(
        id = "H1",
        relation = "A -> B",
        prediction = prediction,
        estimate = estimate,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        concordance = concordance
      )
    )
  }
  classify <- function(a, b) {
    nomologR:::nomo_network_replication_evidence(a, b)
  }

  # The pattern reported in #30: both estimates near zero, both intervals
  # include zero.
  near_zero <- classify(
    fit_with(-.101, -.211, .008, "inconsistent"),
    fit_with(.004, -.127, .134, "direction_concordant_below_magnitude")
  )
  expect_equal(near_zero$replication_status, "sign_change_within_uncertainty")
  expect_match(near_zero$interpretation, "neither sample's confidence interval excludes zero")
  expect_match(near_zero$interpretation, "not evidence of a reversal")
  expect_match(near_zero$interpretation, "negligible()", fixed = TRUE)
  expect_false(grepl("substantively important", near_zero$interpretation))

  # Only the primary interval excludes zero.
  primary_only <- classify(
    fit_with(.30, .15, .45, "concordant"),
    fit_with(-.05, -.20, .10, "inconclusive")
  )
  expect_equal(primary_only$replication_status, "direction_not_replicated")
  expect_match(primary_only$interpretation, "only the primary sample's confidence interval")
  expect_match(primary_only$interpretation, "not the same as a reversal")

  # Only the validation interval excludes zero; the wording follows the sample.
  validation_only <- classify(
    fit_with(-.05, -.20, .10, "inconclusive"),
    fit_with(.30, .15, .45, "concordant")
  )
  expect_equal(validation_only$replication_status, "direction_not_replicated")
  expect_match(validation_only$interpretation, "only the validation sample's confidence interval")

  # Negative predictions follow the same rule.
  negative_reversal <- classify(
    fit_with(-.40, -.50, -.30, "concordant", prediction = "negative"),
    fit_with(.30, .20, .40, "inconsistent", prediction = "negative")
  )
  expect_equal(negative_reversal$replication_status, "sign_reversal")

  # An interval that touches zero does not exclude it.
  touching <- classify(
    fit_with(.20, 0, .40, "directionally_concordant_imprecise"),
    fit_with(-.20, -.40, 0, "inconsistent")
  )
  expect_equal(touching$replication_status, "sign_change_within_uncertainty")
})


test_that("missing intervals can never establish a reversal (#30)", {
  no_ci <- function(estimate, concordance) {
    list(
      hypothesis_evidence = tibble::tibble(
        id = "H1",
        relation = "A -> B",
        prediction = "positive",
        estimate = estimate,
        concordance = concordance
      )
    )
  }

  out <- nomologR:::nomo_network_replication_evidence(
    no_ci(.40, "concordant"),
    no_ci(-.30, "inconsistent")
  )
  expect_equal(out$replication_status, "sign_change_within_uncertainty")
  expect_match(out$interpretation, "Confidence intervals were unavailable")

  expect_true(is.na(nomologR:::nomo_network_interval_side(NA_real_, .5)))
  expect_true(is.na(nomologR:::nomo_network_interval_side(-.1, .1)))
  expect_equal(nomologR:::nomo_network_interval_side(.1, .5), "positive")
  expect_equal(nomologR:::nomo_network_interval_side(-.5, -.1), "negative")
})


test_that("new replication statuses have readable labels", {
  labels <- nomologR:::nomo_network_pretty_status(c(
    "sign_reversal",
    "direction_not_replicated",
    "sign_change_within_uncertainty"
  ))
  expect_equal(labels, c(
    "Sign reversal",
    "Direction not replicated",
    "Sign change within uncertainty"
  ))
})


test_that("the #30 example is no longer reported as a sign reversal", {
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
  s <- nomo_split(nomo_demo_network, validation_prop = .40, seed = 2026)
  net <- nomo_network(model, data = s, hypotheses = h)

  rep <- nomo_table(net, "replication")
  h4 <- rep[rep$id == "H4", ]
  validation_h4 <- net$validation$hypothesis_evidence
  validation_h4 <- validation_h4[validation_h4$id == "H4", ]

  # The population path is zero. The estimates differ in sign, but the
  # validation interval clearly includes zero, so no reversal can be claimed.
  expect_true(sign(h4$primary_estimate) != sign(h4$validation_estimate))
  expect_lt(validation_h4$ci_lower, 0)
  expect_gt(validation_h4$ci_upper, 0)
  expect_false(identical(h4$replication_status, "sign_reversal"))
  expect_true(h4$replication_status %in% c(
    "sign_change_within_uncertainty",
    "direction_not_replicated"
  ))

  log_row <- net$decision_log[
    net$decision_log$stage == "network_replication" &
      net$decision_log$object == "Persistence -> Performance",
  ]
  expected_severity <- if (identical(h4$replication_status, "direction_not_replicated")) {
    "concern"
  } else {
    "review"
  }
  expect_equal(log_row$severity, expected_severity)
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

  expect_output(print(summary(out)), "Model fit")
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

  expect_match(plot_text(p_effects$labels$title), "Theory-specified")
  expect_match(plot_text(p_rep$labels$title), "validation")
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

  # The whole interval is short of the magnitude: the direction is right and
  # the predicted magnitude is excluded, which is inconsistent with it (#145).
  out <- nomo_network(
    network_truth_model(),
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive(min = .30)
    )
  )

  row <- out$hypothesis_evidence

  expect_gt(row$estimate, 0)
  expect_lt(row$ci_upper, .30)
  expect_equal(row$concordance, "inconsistent")
  expect_match(row$interpretation, "has the predicted direction", fixed = TRUE)
  expect_match(row$interpretation, "smaller in magnitude", fixed = TRUE)

  # An interval that still reaches the magnitude leaves the question open.
  reaching <- nomo_network(
    network_truth_model(),
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive(min = row$estimate + (row$ci_upper - row$estimate) / 2)
    )
  )$hypothesis_evidence

  expect_equal(reaching$concordance, "direction_concordant_below_magnitude")
  expect_match(reaching$interpretation, "still overlaps that region", fixed = TRUE)
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


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("closeout: network low-level classifiers cover non-evaluable and inconclusive states", {
  expect_error(
    nomologR:::nomo_network_model_table("F =~"),
    "Could not parse"
  )
  expect_false(
    nomologR:::nomo_network_relation_present(
      data.frame(),
      list(relation_type = "directed", source = "A", target = "B")
    )
  )
  expect_true(is.na(
    nomologR:::nomo_network_fit_measure(numeric(), "cfi")
  ))
  expect_length(
    nomologR:::nomo_network_match_index(
      data.frame(),
      list(relation_type = "directed", source = "A", target = "B")
    ),
    0L
  )

  expect_true(is.na(
    nomologR:::nomo_network_interval_within_region(
      NA_real_, .2, 0, Inf, FALSE, FALSE
    )
  ))
  expect_true(
    nomologR:::nomo_network_interval_within_region(
      -.2, .2, -Inf, .5, FALSE, TRUE
    )
  )
  expect_true(is.na(
    nomologR:::nomo_network_interval_overlaps_region(
      NA_real_, .2, 0, 1
    )
  ))
  expect_true(is.na(
    nomologR:::nomo_network_direction_correct(NA_real_, "positive")
  ))

  h <- as.list(
    nomo_hypotheses("A -> B" = positive(min = .20))$hypotheses[1, , drop = FALSE]
  )
  ne <- nomologR:::nomo_network_classify(
    h, estimate = .3, ci_lower = .2, ci_upper = .4, converged = FALSE
  )
  expect_identical(ne$concordance, "not_evaluable")

  h_inconclusive <- as.list(
    nomo_hypotheses("A -> B" = positive())$hypotheses[1, , drop = FALSE]
  )
  inc <- nomologR:::nomo_network_classify(
    h_inconclusive,
    estimate = -.10,
    ci_lower = -.20,
    ci_upper = .20,
    converged = TRUE
  )
  expect_identical(inc$concordance, "inconclusive")
})


test_that("closeout: network measurement context covers empty and strained measurement evidence", {
  fit <- make_m9_report_run()$results$cfa$fit
  g <- nomo_defaults()

  empty <- nomologR:::nomo_network_measurement_context(
    fit = fit,
    standardized_solution = data.frame(
      lhs = character(), op = character(), rhs = character(),
      est.std = numeric()
    ),
    parameter_estimates = data.frame(
      lhs = character(), op = character(), rhs = character(), est = numeric()
    ),
    fit_evidence = tibble::tibble(
      cfi = NA_real_, tli = NA_real_, rmsea = NA_real_, srmr = NA_real_
    ),
    converged = TRUE,
    warnings = character(),
    guidance = g
  )
  expect_equal(nrow(empty$loadings), 0L)
  expect_equal(nrow(empty$variances), 0L)

  g$cfa_loading_reference <- NA_real_
  strained <- nomologR:::nomo_network_measurement_context(
    fit = fit,
    standardized_solution = data.frame(
      lhs = "WellBeing", op = "=~", rhs = "i1", est.std = .30
    ),
    parameter_estimates = data.frame(
      lhs = "i1", op = "~~", rhs = "i1", est = -.10
    ),
    fit_evidence = tibble::tibble(
      cfi = .50, tli = .50, rmsea = .20, srmr = .20
    ),
    converged = FALSE,
    warnings = "synthetic engine warning",
    guidance = g
  )
  expect_identical(strained$summary$attention[[1L]], "concern")
  expect_match(strained$summary$observation[[1L]], "negative variance", fixed = TRUE)
})


test_that("closeout: network replication and log handle non-evaluable evidence and warnings", {
  a <- tibble::tibble(
    id = "H1",
    relation = "A -> B",
    prediction = "positive",
    estimate = NA_real_,
    concordance = "not_evaluable"
  )
  b <- tibble::tibble(
    id = "H1",
    relation = "A -> B",
    prediction = "positive",
    estimate = .2,
    concordance = "concordant"
  )
  rep <- nomologR:::nomo_network_replication_evidence(
    list(hypothesis_evidence = a),
    list(hypothesis_evidence = b)
  )
  expect_identical(rep$replication_status[[1L]], "not_evaluable")

  measurement <- list(
    summary = tibble::tibble(
      loading_review_flags = 0L,
      attention = "info",
      observation = "Synthetic measurement context."
    )
  )
  additions <- tibble::tibble(
    relation = "A -> B",
    syntax = "B ~ A",
    added_from_hypothesis = FALSE
  )
  evidence <- tibble::tibble(
    relation = "A -> B",
    concordance = "not_evaluable",
    estimate = NA_real_,
    theoretical_region = "(0, +Inf)",
    interpretation = "Not evaluable."
  )
  log <- nomologR:::nomo_network_decision_log(
    model_additions = additions,
    hypotheses_evidence = evidence,
    converged = FALSE,
    warnings = "synthetic warning",
    estimator = "ML",
    ordered = character(),
    measurement_context = measurement,
    sample_role = "primary"
  )
  expect_true(any(log$metric == "engine_warnings"))
  expect_true(any(log$severity == "concern"))
})


test_that("closeout: network public validation covers ordered validation and ML/FIML guards", {
  dat <- data.frame(
    i1 = rnorm(30),
    i2 = rnorm(30),
    criterion = rnorm(30)
  )
  val <- dat
  val$i1 <- NULL
  h <- nomo_hypotheses("F -> criterion" = positive())
  model <- "F =~ i1 + i2"

  expect_error(
    nomo_network(model, dat, h, guidance = 1),
    "`guidance` must be a list"
  )
  expect_error(
    nomo_network(model, dat, h, ordered = 1),
    "`ordered`"
  )
  expect_error(
    nomo_network(
      model, dat, h,
      validation_data = val,
      ordered = "i1"
    ),
    "not found in validation data"
  )
  expect_error(
    nomo_network(
      model, dat, h,
      ordered = "i1",
      estimator = "ml"
    ),
    "ML-family"
  )
  expect_error(
    nomo_network(
      model, dat, h,
      ordered = "i1",
      missing = "fiml"
    ),
    "FIML"
  )

  nm <- nomo_model(list(F = c("i1", "i2")))
  expect_error(
    nomo_network(
      nm,
      dat,
      nomo_hypotheses("F -> missing_node" = positive())
    ),
    "neither latent variables"
  )
})


test_that("closeout: network fit wrapper exposes missing/control arguments before engine failure", {
  h <- nomo_hypotheses("F -> criterion" = positive())
  rels <- tibble::tibble(
    id = "H1",
    relation = "F -> criterion",
    syntax = "criterion ~ F",
    already_in_model = FALSE,
    added_from_hypothesis = TRUE,
    origin = "a_priori"
  )
  expect_error(
    nomologR:::nomo_network_fit_once(
      model_fitted = "F =~ missing_item",
      model_relations = rels,
      hypotheses = h,
      data = data.frame(x = 1:10),
      ordered = character(),
      estimator_requested = NULL,
      estimator_source = "lavaan_default",
      missing = "fiml",
      std.lv = TRUE,
      control = list(iter.max = 1L),
      guidance = nomo_defaults(),
      equivalence_alpha = .05,
      sample_role = "primary"
    ),
    "Nomological-network estimation failed"
  )
})


test_that("closeout: network presentation covers empty evidence and replication branches", {
  skip_on_cran()
  net <- make_m9_full_report_run()$results$network

  if (nrow(net$replication_evidence)) {
    s <- summary(net)
    expect_gt(nrow(s$replication_counts), 0L)
    txt <- paste(capture.output(print(s)), collapse = "\n")
    expect_match(txt, "Replication evidence", fixed = TRUE)
  }

  empty_effects <- net
  empty_effects$hypothesis_evidence$estimate[] <- NA_real_
  empty_effects$hypothesis_evidence$ci_lower[] <- NA_real_
  empty_effects$hypothesis_evidence$ci_upper[] <- NA_real_
  expect_error(
    plot(empty_effects, type = "effects"),
    "No finite hypothesis estimates"
  )

  empty_con <- net
  empty_con$hypothesis_evidence <- empty_con$hypothesis_evidence[0, , drop = FALSE]
  expect_error(
    plot(empty_con, type = "concordance"),
    "No hypothesis evidence"
  )

  empty_fit <- net
  for (nm in c("cfi", "tli", "rmsea", "srmr")) {
    empty_fit$fit_evidence[[nm]] <- NA_real_
  }
  expect_error(
    plot(empty_fit, type = "fit"),
    "No finite global fit evidence"
  )

  no_rep <- net
  no_rep$replication_evidence <- no_rep$replication_evidence[0, , drop = FALSE]
  expect_error(
    plot(no_rep, type = "replication"),
    "No validation sample"
  )

  if (nrow(net$replication_evidence)) {
    bad_rep <- net
    bad_rep$replication_evidence$primary_estimate[] <- NA_real_
    bad_rep$replication_evidence$validation_estimate[] <- NA_real_
    expect_error(
      plot(bad_rep, type = "replication"),
      "No finite replication estimates"
    )
  }
})


test_that("closeout B: network fitting captures warnings and explicit researcher estimator selection", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- lavaan::HolzingerSwineford1939
  model <- "visual =~ x1 + x2 + x3"
  h <- nomo_hypotheses("visual -> x4" = positive())
  original_sem <- lavaan::sem

  testthat::local_mocked_bindings(
    sem = function(...) {
      warning("synthetic SEM warning")
      original_sem(...)
    },
    .package = "lavaan"
  )

  out <- nomo_network(
    model,
    dat,
    h,
    estimator = "MLR"
  )

  expect_identical(out$estimator_source, "researcher")
  expect_true(any(grepl(
    "synthetic SEM warning",
    out$engine_warnings,
    fixed = TRUE
  )))
})


test_that("closeout B: network decision log classifies supportive replication as informational", {
  measurement <- list(
    summary = tibble::tibble(
      loading_review_flags = 0L,
      attention = "info",
      observation = "Synthetic measurement context."
    )
  )
  additions <- tibble::tibble(
    relation = "A -> B",
    syntax = "B ~ A",
    added_from_hypothesis = FALSE
  )
  evidence <- tibble::tibble(
    relation = "A -> B",
    concordance = "concordant",
    estimate = .30,
    theoretical_region = "(0, +Inf)",
    interpretation = "Synthetic concordance."
  )
  replication <- tibble::tibble(
    relation = "A -> B",
    replication_status = "replicated_concordance",
    estimate_shift = .01,
    interpretation = "Synthetic replication."
  )

  log <- nomologR:::nomo_network_decision_log(
    model_additions = additions,
    hypotheses_evidence = evidence,
    converged = TRUE,
    warnings = character(),
    estimator = NULL,
    ordered = character(),
    measurement_context = measurement,
    replication_evidence = replication,
    sample_role = "primary"
  )

  row <- log[
    log$stage == "network_replication",
    ,
    drop = FALSE
  ]
  expect_gt(nrow(row), 0L)
  expect_identical(row$severity[[1L]], "info")
})


test_that("closeout B: network presentation covers zero-span theory regions and populated replication summaries", {
  net <- make_m9_full_report_run()$results$network

  zero_net <- net
  zero_net$hypothesis_evidence$estimate[] <- 0
  zero_net$hypothesis_evidence$ci_lower[] <- 0
  zero_net$hypothesis_evidence$ci_upper[] <- 0
  zero_net$hypotheses$hypotheses$lower[] <- -Inf
  zero_net$hypotheses$hypotheses$upper[] <- Inf

  prepared <- nomologR:::nomo_network_theory_plot_data(zero_net)
  expect_equal(prepared$limits, c(-.1, .1))

  net$validation_n <- 100L
  net$replication_evidence <- tibble::tibble(
    id = "H1",
    relation = net$hypothesis_evidence$relation[[1L]],
    prediction = net$hypothesis_evidence$prediction[[1L]],
    primary_estimate = .30,
    validation_estimate = .28,
    estimate_shift = -.02,
    primary_concordance = "concordant",
    validation_concordance = "concordant",
    replication_status = "replicated_concordance",
    interpretation = "Synthetic replicated concordance."
  )

  s <- summary(net)
  expect_gt(nrow(s$replication_counts), 0L)

  txt <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(txt, "Validation sample: N = 100", fixed = TRUE)
  expect_match(txt, "Replication evidence", fixed = TRUE)

  expect_s3_class(plot(net, type = "replication"), "ggplot")
})


test_that("closeout C: network replication decision log marks inconclusive replication for review", {
  measurement <- list(
    summary = tibble::tibble(
      loading_review_flags = 0L,
      attention = "info",
      observation = "Synthetic measurement context."
    )
  )

  additions <- tibble::tibble(
    relation = "A -> B",
    syntax = "B ~ A",
    added_from_hypothesis = FALSE
  )

  evidence <- tibble::tibble(
    relation = "A -> B",
    concordance = "concordant",
    estimate = .30,
    theoretical_region = "(0, +Inf)",
    interpretation = "Synthetic concordance."
  )

  replication <- tibble::tibble(
    relation = "A -> B",
    replication_status = "mixed_or_inconclusive",
    estimate_shift = .01,
    interpretation = "Synthetic mixed replication evidence."
  )

  log <- nomologR:::nomo_network_decision_log(
    model_additions = additions,
    hypotheses_evidence = evidence,
    converged = TRUE,
    warnings = character(),
    estimator = NULL,
    ordered = character(),
    measurement_context = measurement,
    replication_evidence = replication,
    sample_role = "primary"
  )

  row <- log[
    log$stage == "network_replication",
    ,
    drop = FALSE
  ]

  expect_gt(nrow(row), 0L)
  expect_identical(row$severity[[1L]], "review")
})


test_that("closeout C: replication plots reject rows with no finite paired estimates", {
  net <- make_m9_full_report_run()$results$network

  net$replication_evidence <- tibble::tibble(
    primary_estimate = NA_real_,
    validation_estimate = NA_real_
  )

  expect_error(
    plot(net, type = "replication"),
    "No finite replication estimates are available to plot"
  )
})


# Observed endpoints (#62) -----------------------------------------------------

test_that("a network of latent variables raises no endpoint disclosure", {
  out <- nomo_network(
    "Agency =~ ag1 + ag2 + ag3 + ag4\nPersistence =~ pe1 + pe2 + pe3 + pe4",
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses("Agency -> Persistence" = positive())
  )
  expect_false(any(out$decision_log$metric %in%
                     c("observed_endpoints", "mixed_endpoints")))
})


test_that("relationships between observed composites are disclosed for review", {
  d <- nomo_demo_network
  d$agency_sum <- rowSums(d[, paste0("ag", 1:4)])
  d$persist_sum <- rowSums(d[, paste0("pe", 1:4)])

  out <- nomo_network(
    "persist_sum ~ agency_sum",
    data = d,
    hypotheses = nomo_hypotheses("agency_sum -> persist_sum" = positive())
  )
  entry <- out$decision_log[out$decision_log$metric == "observed_endpoints", ]

  expect_identical(nrow(entry), 1L)
  expect_identical(entry$severity, "review")
  expect_match(entry$observation, "agency_sum, persist_sum", fixed = TRUE)
  expect_match(entry$recommendation, "correlational accuracy", fixed = TRUE)
  expect_match(entry$recommendation, "lavaan::sam()", fixed = TRUE)

  # For factor scores, the one design shown to give consistent regression
  # coefficients, with both of its conditions.
  expect_match(entry$recommendation, "Skrondal and Laake (2001)", fixed = TRUE)
  expect_match(entry$recommendation, "Bartlett scores for the outcome", fixed = TRUE)
  expect_match(entry$recommendation, "measurement model of its own", fixed = TRUE)
})


test_that("a latent-to-observed relationship is disclosed, and a single measure is excused", {
  out <- nomo_network(
    paste(
      "Agency =~ ag1 + ag2 + ag3 + ag4",
      "Persistence =~ pe1 + pe2 + pe3 + pe4",
      "Performance ~ Agency",
      sep = "\n"
    ),
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses("Agency -> Performance" = positive())
  )
  entry <- out$decision_log[out$decision_log$metric == "mixed_endpoints", ]

  expect_identical(nrow(entry), 1L)
  expect_identical(entry$severity, "info")
  expect_match(entry$observation, "Performance", fixed = TRUE)

  # The network cannot tell a composite from a single measured variable, so the
  # disclosure must say plainly that it does not apply to the latter.
  expect_match(entry$recommendation, "is not a composite", fixed = TRUE)
})


# Audit of the network stage (#145) --------------------------------------------

demo_network_model <- function(factors = c("Agency", "Persistence")) {
  items <- list(
    Agency = c("ag1", "ag2", "ag3", "ag4"),
    Persistence = c("pe1", "pe2", "pe3", "pe4"),
    SocialDesirability = c("sd1", "sd2", "sd3")
  )
  as.character(nomo_model(items[factors]))
}

classify_against <- function(expectation, estimate, ci_lower, ci_upper) {
  hypothesis <- as.list(
    nomo_hypotheses("A -> B" = expectation)$hypotheses[1, , drop = FALSE]
  )
  nomo_network_classify(hypothesis, estimate, ci_lower, ci_upper, converged = TRUE)
}


test_that("an association is judged against the model that will be fitted (#145)", {
  model <- demo_network_model(c("Agency", "Persistence", "SocialDesirability"))

  # lavaan covaries exogenous factors by itself, but not once a hypothesized
  # path makes Persistence an outcome. The association is then added.
  out <- nomo_network(
    model,
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses(
      "Agency -> Persistence" = positive(),
      "Persistence <-> SocialDesirability" = negligible(within = c(-.15, .15)),
      "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15))
    )
  )
  relations <- out$model_relations

  expect_identical(relations$already_in_model, c(FALSE, FALSE, TRUE))
  expect_identical(relations$added_from_hypothesis, c(TRUE, TRUE, FALSE))
  expect_match(out$model_fitted, "Persistence ~~ SocialDesirability", fixed = TRUE)
  expect_no_match(out$model_fitted, "Agency ~~ SocialDesirability", fixed = TRUE)
  expect_true(all(is.finite(out$hypothesis_evidence$estimate)))
  expect_false(any(out$hypothesis_evidence$concordance == "not_evaluable"))

  # Every relation the result calls present has a parameter in the fit.
  fitted <- out$parameter_estimates
  expect_true(any(
    fitted$op == "~~" & fitted$lhs == "Agency" &
      fitted$rhs == "SocialDesirability"
  ))

  # Without a directed path, lavaan's own covariance is the association.
  kept <- nomo_network(
    model,
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses(
      "Persistence <-> SocialDesirability" = negligible(within = c(-.15, .15))
    ),
    add_missing = FALSE
  )
  expect_true(kept$model_relations$already_in_model)
  expect_true(is.finite(kept$hypothesis_evidence$estimate))

  # With one, and no additions allowed, it is absent and reported as absent.
  absent <- nomo_network(
    paste(model, "Persistence ~ Agency", sep = "\n"),
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses(
      "Persistence <-> SocialDesirability" = negligible(within = c(-.15, .15))
    ),
    add_missing = FALSE
  )
  expect_false(absent$model_relations$already_in_model)
  expect_false(absent$model_relations$added_from_hypothesis)
  expect_identical(absent$hypothesis_evidence$concordance, "not_evaluable")
})


test_that("the covariance of two fixed covariates is added, so it is estimated (#145)", {
  set.seed(14501)
  dat <- nomo_demo_network
  dat$x1 <- rnorm(nrow(dat))
  dat$x2 <- .3 * dat$x1 + rnorm(nrow(dat))

  out <- nomo_network(
    demo_network_model("Agency"),
    data = dat,
    hypotheses = nomo_hypotheses(
      "x1 -> Agency" = negligible(within = c(-.2, .2)),
      "x2 -> Agency" = negligible(within = c(-.2, .2)),
      "x1 <-> x2" = positive()
    )
  )

  expect_true(out$model_relations$added_from_hypothesis[[3L]])
  expect_gt(out$hypothesis_evidence$se[[3L]], 0)
  expect_identical(out$hypothesis_evidence$concordance[[3L]], "concordant")
})


test_that("a pair of variables carries one relation in the fitted model (#145)", {
  model <- paste(demo_network_model(), "Persistence ~ Agency", sep = "\n")

  expect_error(
    nomo_network(
      model, nomo_demo_network,
      nomo_hypotheses("Agency <-> Persistence" = positive())
    ),
    "can carry a directed path or an association, not both"
  )
  expect_error(
    nomo_network(
      model, nomo_demo_network,
      nomo_hypotheses("Persistence -> Agency" = positive())
    ),
    "would add a path opposite to one the model already has"
  )

  # A hypothesis set built before nomo_hypotheses() refused such pairs.
  legacy <- nomo_hypotheses(
    "Agency -> Persistence" = positive(),
    "Agency <-> Performance" = positive()
  )
  legacy$hypotheses$target[[2L]] <- "Persistence"
  legacy$hypotheses$relation[[2L]] <- "Agency <-> Persistence"
  expect_error(
    nomo_network(demo_network_model(), nomo_demo_network, legacy),
    "Hypothesis `Agency <-> Persistence` is an association"
  )
})


test_that("an association with a predicted endpoint is labeled a residual association (#145)", {
  out <- nomo_network(
    demo_network_model(),
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses(
      "Agency -> Persistence" = positive(),
      "Agency -> Performance" = positive(),
      "Persistence <-> Performance" = positive()
    )
  )
  evidence <- out$hypothesis_evidence

  expect_identical(
    evidence$evidence_scope,
    c("latent_structural", "latent_to_observed_outcome", "residual_association")
  )
  expect_match(evidence$interpretation[[3L]], "residual association", fixed = TRUE)
  expect_match(
    evidence$interpretation[[3L]], "`Persistence` and `Performance`", fixed = TRUE
  )
  expect_no_match(evidence$interpretation[[1L]], "residual association", fixed = TRUE)

  # The quantity differs from the overall association the two share, here
  # even in sign, which is why it is labeled.
  implied <- stats::cov2cor(lavaan::lavInspect(out$fit, "cov.all"))
  expect_lt(evidence$estimate[[3L]], 0)
  expect_gt(implied["Persistence", "Performance"], 0)

  entry <- out$decision_log[out$decision_log$metric == "residual_association", ]
  expect_identical(nrow(entry), 1L)
  expect_identical(entry$severity, "review")
  expect_identical(entry$object, "Persistence <-> Performance")
  expect_equal(entry$value, evidence$estimate[[3L]])

  expect_output(print(summary(out)), "residual association", fixed = TRUE)

  # Associations among variables nothing predicts keep their scope.
  plain <- nomo_network(
    demo_network_model(c("Agency", "SocialDesirability")),
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses(
      "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15))
    )
  )
  expect_identical(plain$hypothesis_evidence$evidence_scope, "latent_association")
  expect_false(any(plain$decision_log$metric == "residual_association"))

  expect_identical(
    nomo_network_scope("latent", "observed", "association", residual = TRUE),
    "residual_association"
  )
})


test_that("an estimate that misses a magnitude is classed by side and by interval (#145)", {
  above <- classify_against(positive(max = .30), .35, .25, .45)
  expect_identical(above$concordance, "direction_concordant_above_magnitude")
  expect_match(above$interpretation, "larger in magnitude", fixed = TRUE)
  expect_match(above$interpretation, "(0, 0.3]", fixed = TRUE)

  below <- classify_against(positive(min = .30), .25, .15, .35)
  expect_identical(below$concordance, "direction_concordant_below_magnitude")
  expect_match(below$interpretation, "smaller in magnitude", fixed = TRUE)

  expect_identical(
    classify_against(negative(min = -.30), -.35, -.45, -.25)$concordance,
    "direction_concordant_above_magnitude"
  )
  expect_identical(
    classify_against(negative(max = -.30), -.25, -.35, -.15)$concordance,
    "direction_concordant_below_magnitude"
  )

  # An interval wholly outside the region is inconsistent with the predicted
  # magnitude, on either side, although the direction is as predicted.
  beyond <- classify_against(positive(min = .10, max = .20), .39, .32, .45)
  expect_identical(beyond$concordance, "inconsistent")
  expect_match(beyond$interpretation, "has the predicted direction", fixed = TRUE)
  expect_match(beyond$interpretation, "larger in magnitude", fixed = TRUE)

  short <- classify_against(positive(min = .20), .05, .01, .09)
  expect_identical(short$concordance, "inconsistent")
  expect_match(short$interpretation, "smaller in magnitude", fixed = TRUE)

  wrong_sign <- classify_against(positive(min = .20), -.25, -.35, -.15)
  expect_identical(wrong_sign$concordance, "inconsistent")
  expect_no_match(wrong_sign$interpretation, "predicted direction", fixed = TRUE)

  expect_identical(
    classify_against(negligible(within = c(-.1, .1)), .15, .05, .30)$concordance,
    "inconclusive"
  )
})


test_that("the above-magnitude value reaches the output, the log and replication (#145)", {
  out <- nomo_network(
    demo_network_model(),
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses(
      "Agency -> Persistence" = positive(max = .40),
      "Agency -> Performance" = positive(max = .30)
    )
  )
  evidence <- out$hypothesis_evidence

  expect_identical(
    evidence$concordance,
    c("direction_concordant_above_magnitude", "inconsistent")
  )
  log <- out$decision_log[out$decision_log$metric == "theory_concordance", ]
  expect_identical(log$severity, c("review", "concern"))

  expect_identical(
    nomo_network_pretty_status("direction_concordant_above_magnitude"),
    "Direction concordant / above magnitude"
  )
  expect_true(
    "Direction concordant / above magnitude" %in% nomo_network_concordance_levels()
  )
  expect_false(anyNA(plot(out, type = "concordance")$data$concordance_display))
  expect_identical(
    nomo_apa_concordance("direction_concordant_above_magnitude"),
    "Direction concordant, above predicted magnitude"
  )

  sample <- function(estimate, concordance) {
    list(hypothesis_evidence = tibble::tibble(
      id = "H1", relation = "A -> B", prediction = "positive",
      estimate = estimate, ci_lower = estimate - .1, ci_upper = estimate + .1,
      concordance = concordance
    ))
  }
  expect_identical(
    nomo_network_replication_evidence(
      sample(.45, "direction_concordant_above_magnitude"),
      sample(.90, "inconsistent")
    )$replication_status,
    "not_replicated"
  )
})


test_that("an estimate without a standard error is not evaluable (#145)", {
  no_interval <- classify_against(positive(), .25, NA_real_, NA_real_)
  expect_identical(no_interval$concordance, "not_evaluable")
  expect_match(no_interval$interpretation, "standard error was unavailable", fixed = TRUE)

  # A reciprocal pair written in the model is not identified, so lavaan
  # converges without standard errors.
  model <- paste(
    demo_network_model(), "Persistence ~ Agency", "Agency ~ Persistence",
    sep = "\n"
  )
  out <- suppressWarnings(nomo_network(
    model,
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses("Agency -> Persistence" = positive())
  ))
  evidence <- out$hypothesis_evidence

  expect_true(out$converged)
  expect_true(is.finite(evidence$estimate))
  expect_true(is.na(evidence$se))
  expect_identical(evidence$concordance, "not_evaluable")
  expect_match(evidence$interpretation, "standard error was unavailable", fixed = TRUE)
  expect_no_match(evidence$interpretation, "confidence interval extends", fixed = TRUE)

  context <- out$measurement_context$summary
  expect_identical(context$attention, "concern")
  expect_match(context$observation, "standard errors could not be computed", fixed = TRUE)

  log <- out$decision_log
  expect_identical(log$severity[log$metric == "measurement_context"], "concern")
  expect_identical(log$severity[log$metric == "theory_concordance"], "concern")
})


test_that("columns stored as ordered factors are treated as declared (#145)", {
  dat <- nomo_demo_ordinal
  model <- as.character(nomo_model(list(
    A = paste0("a", 1:5), B = paste0("b", 1:5)
  )))
  h <- nomo_hypotheses("A -> B" = positive())

  found <- nomo_network(model, dat, h)
  named <- nomo_network(model, dat, h, ordered = names(dat))

  expect_identical(found$estimator, "WLSMV")
  expect_identical(found$estimator_source, "ordered_default")
  expect_setequal(found$ordered, names(dat))
  expect_setequal(found$ordered_detected, names(dat))
  expect_identical(named$ordered_detected, character())
  expect_equal(found$hypothesis_evidence$estimate, named$hypothesis_evidence$estimate)
  expect_equal(found$fit_evidence, named$fit_evidence)

  metrics <- found$decision_log$metric
  expect_true(all(c("ordered_indicators", "ordered_detected", "estimator") %in% metrics))
  detected <- found$decision_log[metrics == "ordered_detected", ]
  expect_identical(detected$stage, "network")
  expect_identical(detected$severity, "review")
  expect_equal(detected$value, 10)
  expect_false("ordered_detected" %in% named$decision_log$metric)

  # Naming some columns leaves the rest to be found.
  partly <- nomo_network(model, dat, h, ordered = c("a1", "a2"))
  expect_setequal(partly$ordered_detected, setdiff(names(dat), c("a1", "a2")))

  # The ordered-data guards apply to columns found this way, and say so.
  expect_error(
    nomo_network(model, dat, h, estimator = "MLR"),
    "ML-family estimators are not supported.*Stored as ordered factors and treated as declared: a1"
  )
  expect_error(
    nomo_network(model, dat, h, missing = "fiml"),
    "FIML is not supported.*Stored as ordered factors"
  )
})


test_that("an ordered column in either sample is treated as declared in both (#145)", {
  ordinal <- nomo_demo_ordinal
  half <- seq_len(nrow(ordinal)) <= nrow(ordinal) / 2
  primary <- as.data.frame(lapply(ordinal[half, ], as.integer))
  validation <- ordinal[!half, ]

  out <- nomo_network(
    as.character(nomo_model(list(A = paste0("a", 1:5), B = paste0("b", 1:5)))),
    data = primary,
    validation_data = validation,
    hypotheses = nomo_hypotheses("A -> B" = positive())
  )

  expect_setequal(out$ordered_detected, names(ordinal))
  expect_identical(out$estimator, "WLSMV")
  expect_identical(out$validation$estimator, "WLSMV")
  expect_identical(sum(out$decision_log$metric == "ordered_detected"), 1L)
})


test_that("the estimator a fit used is recorded when none was requested (#145)", {
  out <- nomo_network(
    demo_network_model(),
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses("Agency -> Persistence" = positive())
  )
  expect_identical(out$estimator, "ML")
  expect_identical(out$estimator_source, "lavaan_default")
  expect_false("estimator" %in% out$decision_log$metric)

  categorical <- lavaan::cfa(
    "A =~ a1 + a2 + a3 + a4 + a5", data = nomo_demo_ordinal
  )
  expect_identical(nomo_network_fitted_estimator(categorical), "WLSMV")
  expect_true(is.na(nomo_network_fitted_estimator("not a fit")))
})
