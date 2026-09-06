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
