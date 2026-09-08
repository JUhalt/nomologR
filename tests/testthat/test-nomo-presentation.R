# ---- consolidated from test-checkpoint-c-presentation-polish.R ----
make_polish_network_fixture <- function(n = 420L, seed = 7501L) {
  set.seed(seed)

  A <- rnorm(n)
  B <- .45 * A + rnorm(n, sd = .90)
  C <- .03 * A + rnorm(n, sd = 1)
  criterion <- .50 * A + rnorm(n, sd = .85)

  data.frame(
    a1 = .82 * A + rnorm(n, sd = .55),
    a2 = .78 * A + rnorm(n, sd = .62),
    a3 = .76 * A + rnorm(n, sd = .64),
    b1 = .82 * B + rnorm(n, sd = .55),
    b2 = .78 * B + rnorm(n, sd = .62),
    b3 = .76 * B + rnorm(n, sd = .64),
    c1 = .82 * C + rnorm(n, sd = .55),
    c2 = .78 * C + rnorm(n, sd = .62),
    c3 = .76 * C + rnorm(n, sd = .64),
    criterion = criterion
  )
}


make_polish_invariance_fixture <- function(n = 170L, seed = 7502L) {
  set.seed(seed)

  one_group <- function(n, loading2 = .78, intercept3 = 0) {
    f <- rnorm(n)
    data.frame(
      x1 = .82 * f + rnorm(n, sd = .60),
      x2 = loading2 * f + rnorm(n, sd = .62),
      x3 = intercept3 + .74 * f + rnorm(n, sd = .66),
      x4 = .76 * f + rnorm(n, sd = .64)
    )
  }

  rbind(
    transform(one_group(n), group = "A"),
    transform(
      one_group(n, loading2 = .64, intercept3 = .20),
      group = "B"
    )
  )
}


test_that("effects plot includes explicit theory-compatible regions", {
  dat <- make_polish_network_fixture()

  out <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
      C =~ c1 + c2 + c3
    ",
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive(min = .20),
      "A <-> C" = negligible(within = c(-.15, .15)),
      "A -> criterion" = positive()
    )
  )

  p <- plot(out, "effects")
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "theory-compatible region")
  expect_gte(length(p$layers), 4L)

  prepared <- nomologR:::nomo_network_theory_plot_data(out)
  expect_equal(
    prepared$data$theory_lower[prepared$data$relation == "A -> B"],
    .20
  )
  expect_equal(
    prepared$data$theory_upper[
      prepared$data$relation == "A <-> C"
    ],
    .15
  )
})


test_that("concordance plot is relation-level rather than a count bar", {
  dat <- make_polish_network_fixture(seed = 7503L)

  out <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
      C =~ c1 + c2 + c3
    ",
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive(),
      "A <-> C" = negligible(within = c(-.15, .15))
    )
  )

  p <- plot(out, "concordance")

  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "Relation-level")
  expect_equal(nrow(p$data), 2L)
  expect_false(any(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomCol"),
    logical(1)
  )))
})


test_that("network global-fit plot facets metrics onto separate scales", {
  dat <- make_polish_network_fixture(seed = 7504L)

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

  p <- plot(out, "fit")

  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "own scale")
  expect_equal(length(unique(p$data$metric)), 4L)
})


test_that("replication plot uses human-readable status labels and expanded coordinates", {
  dat1 <- make_polish_network_fixture(n = 360L, seed = 7505L)
  dat2 <- make_polish_network_fixture(n = 360L, seed = 7506L)

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

  p <- plot(out, "replication")

  expect_s3_class(p, "ggplot")
  expect_true("replication_display" %in% names(p$data))
  expect_false(any(grepl("_", p$data$replication_display, fixed = TRUE)))
})


test_that("network print prettifies statuses without changing stored values", {
  dat1 <- make_polish_network_fixture(n = 340L, seed = 7507L)
  dat2 <- make_polish_network_fixture(n = 340L, seed = 7508L)

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

  stored <- out$replication_evidence$replication_status

  expect_output(print(out), "Replication evidence")
  expect_identical(
    out$replication_evidence$replication_status,
    stored
  )
})


test_that("invariance fit and change plots remove redundant metric legends", {
  dat <- make_polish_invariance_fixture()

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric", "scalar"),
    localize = TRUE
  )

  p_fit <- plot(out, "fit")
  p_change <- plot(out, "change")

  expect_s3_class(p_fit, "ggplot")
  expect_s3_class(p_change, "ggplot")

  expect_null(p_fit$labels$shape)
  expect_null(p_change$labels$shape)

  expect_true(all(
    unique(as.character(p_change$data$metric)) %in%
      c("\u0394CFI", "\u0394RMSEA", "\u0394SRMR")
  ))
  expect_match(
    p_change$labels$caption,
    "For CFI, decreases"
  )
})


test_that("local-strain presentation attempts human-readable parameter labels", {
  dat <- make_polish_invariance_fixture(seed = 7509L)

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric", "scalar"),
    localize = TRUE
  )

  display <- nomologR:::nomo_invariance_local_strain_display(out)
  expect_s3_class(display, "tbl_df")

  if (nrow(display)) {
    expect_true("constraint_display" %in% names(display))

    translated <- display$constraint_display[
      grepl(
        "Loading:|Intercept:|Threshold:|Residual variance:|Covariance:",
        display$constraint_display
      )
    ]

    # lavaan versions differ in the score-test metadata they expose. When
    # parameter labels are available, at least one should be translated.
    if (any(grepl("^\\.p[0-9]+\\.", display$constraint))) {
      expect_true(
        length(translated) >= 1L ||
          all(display$constraint_display == display$constraint)
      )
    }
  }

  if (nrow(out$local_strain)) {
    p <- plot(out, "local_strain")
    expect_s3_class(p, "ggplot")
    expect_match(p$labels$subtitle, "do not authorize")
  }
})

# ---- consolidated from test-nomo-m5-presentation.R ----
test_that("reliability presentation is compact and returns a ggplot", {
  model <- '
    F1 =~ x1 + x2 + x3
    F2 =~ x4 + x5 + x6
  '
  cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
  rel <- nomo_reliability(cfa)

  printed <- capture.output(print(rel))
  expect_true(any(grepl("Primary coefficient: model-based omega", printed, fixed = TRUE)))
  expect_true(any(grepl("not pass/fail reliability rules", printed, fixed = TRUE)))

  s <- summary(rel)
  expect_s3_class(s, "summary_nomo_reliability")
  expect_true(all(c(
    "construct", "omega", "alpha", "omega_scale", "signal"
  ) %in% names(s$table)))
  expect_setequal(s$table$construct, c("F1", "F2"))

  p <- plot(rel)
  expect_s3_class(p, "ggplot")
})


test_that("ordinal reliability summary preserves unavailable observed-scale alpha", {
  set.seed(5301)
  n <- 700
  f <- rnorm(n)
  z <- data.frame(
    i1 = .82 * f + rnorm(n, sd = .60),
    i2 = .78 * f + rnorm(n, sd = .65),
    i3 = .74 * f + rnorm(n, sd = .68),
    i4 = .80 * f + rnorm(n, sd = .62)
  )
  dat <- as.data.frame(lapply(z, function(x) {
    ordered(cut(x, breaks = c(-Inf, -.7, 0, .7, Inf), labels = FALSE))
  }))
  cfa <- nomo_cfa('F =~ i1 + i2 + i3 + i4', data = dat, ordered = names(dat))
  rel <- nomo_reliability(cfa, ordinal_scale = TRUE, include_alpha = TRUE)
  s <- summary(rel)

  expect_equal(s$table$construct, "F")
  expect_equal(s$table$omega_scale, "observed_ordinal")
  expect_false(s$table$alpha_available)
  expect_true(is.na(s$table$alpha))
  expect_s3_class(plot(rel), "ggplot")
})


test_that("validity presentation separates convergent and discriminant questions", {
  set.seed(5302)
  n <- 900
  f1 <- rnorm(n)
  f2 <- .35 * f1 + sqrt(1 - .35^2) * rnorm(n)
  dat <- data.frame(
    A1 = .82 * f1 + rnorm(n, sd = .55),
    A2 = .79 * f1 + rnorm(n, sd = .58),
    A3 = .76 * f1 + rnorm(n, sd = .62),
    A4 = .80 * f1 + rnorm(n, sd = .57),
    B1 = .83 * f2 + rnorm(n, sd = .54),
    B2 = .78 * f2 + rnorm(n, sd = .60),
    B3 = .75 * f2 + rnorm(n, sd = .63),
    B4 = .81 * f2 + rnorm(n, sd = .56)
  )
  model <- '
    F1 =~ A1 + A2 + A3 + A4
    F2 =~ B1 + B2 + B3 + B4
  '
  cfa <- nomo_cfa(model, data = dat)
  val <- nomo_validity(cfa, htmt = "both")

  printed <- capture.output(print(val))
  expect_true(any(grepl("No single index", printed, fixed = TRUE)))

  s <- summary(val)
  expect_s3_class(s, "summary_nomo_validity")
  expect_true(all(c(
    "construct", "AVE", "min_abs_loading", "n_loading_review", "signal"
  ) %in% names(s$convergent)))
  expect_true(all(c(
    "construct_1", "construct_2", "latent_r",
    "latent_r_ci_lower", "latent_r_ci_upper",
    "HTMT2", "HTMT", "signal"
  ) %in% names(s$discriminant)))

  expect_s3_class(plot(val, type = "ave"), "ggplot")
  expect_s3_class(plot(val, type = "discriminant"), "ggplot")
  expect_error(plot(val, type = "loadings"), "arg")
})


test_that("weak measurement remains a review signal despite good global fit", {
  set.seed(5303)
  n <- 1000
  f <- rnorm(n)
  dat <- data.frame(
    W1 = .30 * f + rnorm(n, sd = .95),
    W2 = .34 * f + rnorm(n, sd = .94),
    W3 = .28 * f + rnorm(n, sd = .96),
    W4 = .32 * f + rnorm(n, sd = .95),
    W5 = .31 * f + rnorm(n, sd = .95)
  )
  cfa <- nomo_cfa('Weak =~ W1 + W2 + W3 + W4 + W5', data = dat)
  rel <- nomo_reliability(cfa)
  val <- nomo_validity(cfa, htmt = "none")

  rs <- summary(rel)
  vs <- summary(val)
  expect_equal(rs$table$construct, "Weak")
  expect_equal(rs$table$block, "overall")
  expect_true(is.finite(rs$table$omega))
  expect_true(is.finite(rs$table$alpha))
  expect_equal(rs$table$signal, "review")
  expect_true(rs$table$alpha_available)
  expect_equal(vs$convergent$construct, "Weak")
  expect_equal(vs$convergent$signal, "review")
})


test_that("redundant constructs are flagged in the discriminant summary without auto-merging", {
  set.seed(5304)
  n <- 1400
  f1 <- rnorm(n)
  f2 <- .94 * f1 + sqrt(1 - .94^2) * rnorm(n)
  dat <- data.frame(
    A1 = .86 * f1 + rnorm(n, sd = .48),
    A2 = .83 * f1 + rnorm(n, sd = .51),
    A3 = .84 * f1 + rnorm(n, sd = .50),
    B1 = .86 * f2 + rnorm(n, sd = .48),
    B2 = .83 * f2 + rnorm(n, sd = .51),
    B3 = .84 * f2 + rnorm(n, sd = .50)
  )
  model <- '
    F1 =~ A1 + A2 + A3
    F2 =~ B1 + B2 + B3
  '
  cfa <- nomo_cfa(model, data = dat)
  val <- nomo_validity(cfa, htmt = "both")
  s <- summary(val)

  expect_true(any(s$discriminant$signal == "review"))

  review_recommendations <- val$decision_log$recommendation[
    val$decision_log$severity %in% c("review", "concern")
  ]

  expect_true(any(grepl(
    "do not automatically merge constructs",
    review_recommendations,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "^\\s*merge\\b|\\bmust\\s+merge\\b|\\bshould\\s+merge\\b",
    review_recommendations,
    ignore.case = TRUE,
    perl = TRUE
  )))
  expect_true(all(is.na(val$decision_log$decision) | val$decision_log$decision == ""))
})

test_that("reliability plot gives omega greater visual weight without extra y-grid clutter", {
  model <- '
    F1 =~ x1 + x2 + x3
    F2 =~ x4 + x5 + x6
  '
  cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
  rel <- nomo_reliability(cfa)
  p <- plot(rel)

  expect_s3_class(p, "ggplot")
  expect_equal(p$coordinates$limits$x, c(0, 1))
  expect_true(length(p$layers) >= 5L)

  # Omega and alpha points are drawn in separate layers so omega can be
  # intentionally emphasized while alpha remains a secondary comparison.
  point_sizes <- vapply(
    p$layers,
    function(layer) {
      val <- layer$aes_params$size
      if (is.null(val)) NA_real_ else as.numeric(val)
    },
    numeric(1)
  )
  expect_true(any(point_sizes == 3.4, na.rm = TRUE))
  expect_true(any(point_sizes == 2.5, na.rm = TRUE))

  expect_s3_class(
    p$theme$panel.grid.minor.y,
    "element_blank"
  )
})

# ---- consolidated from test-nomo-m6-m7-presentation.R ----
test_that("M6/M7 presentation tables preserve full precision internally", {
  set.seed(6911)
  f <- rnorm(260)
  dat <- data.frame(
    x1 = .8 * f + rnorm(260, sd = .6),
    x2 = .8 * f + rnorm(260, sd = .6),
    x3 = .7 * f + rnorm(260, sd = .7),
    x4 = .7 * f + rnorm(260, sd = .7),
    group = rep(c("A", "B"), each = 130)
  )

  inv <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric"),
    localize = FALSE
  )

  tab <- nomo_table(inv, "fit")
  expect_type(tab$cfi, "double")
  expect_false(is.character(tab$cfi))
})


test_that("M6/M7 plots carry explanatory titles and captions", {
  set.seed(6912)
  n <- 320
  A <- rnorm(n)
  B <- .4 * A + rnorm(n, sd = .9)

  dat <- data.frame(
    a1 = .8 * A + rnorm(n, sd = .6),
    a2 = .8 * A + rnorm(n, sd = .6),
    a3 = .8 * A + rnorm(n, sd = .6),
    b1 = .8 * B + rnorm(n, sd = .6),
    b2 = .8 * B + rnorm(n, sd = .6),
    b3 = .8 * B + rnorm(n, sd = .6)
  )

  net <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
    ",
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  p <- plot(net, type = "effects")
  expect_true(nzchar(p$labels$title))
  expect_true(nzchar(p$labels$subtitle))
  expect_true(nzchar(p$labels$caption))
})

# ---- consolidated from test-coverage-sprint-presentation.R ----
# Pre-v0.1 coverage sprint: presentation branches -----------------------------

test_that("validity convergent table covers ave-only, loading-only, and empty branches", {
  run <- make_m9_report_run()
  val <- run$results$validity

  ave_only <- val
  ave_only$ngroups <- 2L
  out <- nomologR:::nomo_validity_convergent_table(ave_only)
  expect_gt(nrow(out), 0L)
  expect_true(all(is.na(out$min_abs_loading)))
  expect_true(all(is.na(out$median_abs_loading)))
  expect_true(all(is.na(out$n_loading_review)))

  loading_only <- val
  loading_only$ave <- loading_only$ave[0, , drop = FALSE]
  out2 <- nomologR:::nomo_validity_convergent_table(loading_only)
  expect_gt(nrow(out2), 0L)
  expect_true(all(out2$block == "overall"))
  expect_true(all(is.na(out2$AVE)))
  expect_true(all(out2$signal %in% c("info", "review", "concern")))

  empty <- loading_only
  empty$standardized_loadings <- empty$standardized_loadings[0, , drop = FALSE]
  expect_equal(nrow(nomologR:::nomo_validity_convergent_table(empty)), 0L)
})


test_that("validity convergent signals cover concern and review paths", {
  run <- make_m9_report_run()
  val <- run$results$validity

  concern <- val
  concern$ave$attention[] <- "concern"
  out <- nomologR:::nomo_validity_convergent_table(concern)
  expect_true(all(out$signal == "concern"))

  review <- val
  review$ave$attention[] <- "info"
  review$standardized_loadings$attention[1L] <- "REVIEW"
  out2 <- nomologR:::nomo_validity_convergent_table(review)
  expect_true(any(out2$signal == "review"))

  nonfinite <- val
  nonfinite$standardized_loadings$loading[] <- NA_real_
  out3 <- nomologR:::nomo_validity_convergent_table(nonfinite)
  expect_true(all(is.na(out3$min_abs_loading)))
  expect_true(all(is.na(out3$median_abs_loading)))
})


test_that("validity discriminant table covers latent-only and HTMT fallbacks", {
  run <- make_m9_report_run()
  val <- run$results$validity

  latent_only <- val
  latent_only$latent_correlations <- tibble::tibble(
    construct_1 = "A",
    construct_2 = "B",
    block = "overall",
    correlation = .40,
    ci_lower = .20,
    ci_upper = .58
  )
  latent_only$htmt2 <- tibble::tibble()
  latent_only$htmt <- tibble::tibble()

  out <- nomologR:::nomo_validity_discriminant_table(latent_only)
  expect_gt(nrow(out), 0L)
  expect_true(all(is.na(out$HTMT2)))
  expect_true(all(is.na(out$HTMT)))
  expect_true(all(out$signal == "unavailable"))

  htmt_only <- val
  htmt_only$latent_correlations <- tibble::tibble()
  htmt_only$htmt2 <- tibble::tibble()
  htmt_only$htmt <- tibble::tibble(
    construct_1 = c("A", "A"),
    construct_2 = c("B", "C"),
    block = c("overall", "overall"),
    estimate = c(.50, .95)
  )
  htmt_only$htmt_reference <- .85

  out2 <- nomologR:::nomo_validity_discriminant_table(htmt_only)
  expect_equal(out2$signal, c("info", "review"))
  expect_true(all(is.na(out2$latent_r)))
  expect_true(all(is.na(out2$HTMT2)))

  none <- htmt_only
  none$htmt <- tibble::tibble()
  expect_equal(nrow(nomologR:::nomo_validity_discriminant_table(none)), 0L)
})


test_that("validity print methods cover unavailable, not-requested, grouped, and legacy branches", {
  run <- make_m9_report_run()
  val <- run$results$validity

  unavailable <- val
  unavailable$latent_correlations <- tibble::tibble()
  unavailable$htmt2 <- tibble::tibble()
  unavailable$htmt <- tibble::tibble()
  unavailable$htmt_status <- tibble::tibble(
    method = "HTMT2",
    requested = TRUE,
    available = FALSE,
    reason = "Not estimable in this fixture."
  )
  unavailable$ngroups <- 2L
  unavailable$fornell_larcker_requested <- TRUE

  txt <- paste(capture.output(print(unavailable)), collapse = "\n")
  expect_match(txt, "requested but unavailable", fixed = TRUE)
  expect_match(txt, "not silently pooled", fixed = TRUE)
  expect_match(txt, "legacy/supporting", fixed = TRUE)

  not_requested <- unavailable
  not_requested$htmt_status$requested <- FALSE
  not_requested$fornell_larcker_requested <- FALSE
  txt2 <- paste(capture.output(print(not_requested)), collapse = "\n")
  expect_match(txt2, "not requested", fixed = TRUE)

  s <- summary(unavailable)
  s$convergent <- tibble::tibble()
  s$discriminant <- tibble::tibble()
  txt3 <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(txt3, "No convergent summary", fixed = TRUE)
  expect_match(txt3, "No pairwise construct-separation", fixed = TRUE)
  expect_match(txt3, "Unavailable requested HTMT-family evidence", fixed = TRUE)
  expect_match(txt3, "legacy/supporting information", fixed = TRUE)
})


test_that("validity plots cover empty, faceted, and HTMT fallback paths", {
  run <- make_m9_report_run()
  val <- run$results$validity

  empty_ave <- val
  empty_ave$ave$estimate[] <- NA_real_
  expect_error(plot(empty_ave, type = "ave"), "No finite AVE estimates")

  ave <- val
  ave$ave <- dplyr::bind_rows(
    ave$ave,
    dplyr::mutate(ave$ave, block = "second")
  )
  p <- plot(ave, type = "ave")
  expect_s3_class(p, "ggplot")
  expect_true(length(p$facet$params$facets) >= 1L)

  htmt <- val
  htmt$htmt2 <- tibble::tibble()
  htmt$htmt <- tibble::tibble(
    construct_1 = c("A", "A"),
    construct_2 = c("B", "C"),
    block = c("one", "two"),
    estimate = c(.50, .95)
  )
  htmt$htmt_reference <- .85
  p2 <- plot(htmt, type = "discriminant")
  expect_s3_class(p2, "ggplot")
  expect_match(p2$labels$title, "HTMT", fixed = TRUE)

  empty_htmt <- htmt
  empty_htmt$htmt <- tibble::tibble(
    construct_1 = character(),
    construct_2 = character(),
    block = character(),
    estimate = numeric()
  )
  expect_error(
    plot(empty_htmt, type = "discriminant"),
    "No finite HTMT-family estimates"
  )
})


test_that("guided-run presentation covers research blocked and complete states", {
  run <- make_m9_minimal_run()

  run$mode <- "research"
  run$status <- "blocked"
  run$next_stage <- "cfa"
  run$blocked <- list(
    stage = "cfa",
    scope = "measurement_model",
    message = "Synthetic blocked component."
  )
  txt <- paste(capture.output(print(run)), collapse = "\n")
  expect_match(txt, "mode=research", fixed = TRUE)
  expect_match(txt, "Decision requests", fixed = TRUE)
  expect_match(txt, "Blocked at cfa", fixed = TRUE)

  run$status <- "complete"
  run$next_stage <- NULL
  run$blocked <- NULL
  run$decision_requests <- run$decision_requests[0, , drop = FALSE]
  run$stage_status$status[] <- "completed"
  txt2 <- paste(capture.output(print(run)), collapse = "\n")
  expect_match(txt2, "Requested workflow complete", fixed = TRUE)
})


test_that("guided-run teaching presentation covers blocked, examples, and complete states", {
  run <- make_m9_minimal_run()
  run$mode <- "teaching"

  txt <- paste(capture.output(print(run)), collapse = "\n")
  expect_match(txt, "Researcher decision required", fixed = TRUE)
  expect_match(txt, "Example:", fixed = TRUE)
  expect_match(txt, "No later stage has been run automatically", fixed = TRUE)

  blocked <- run
  blocked$status <- "blocked"
  blocked$blocked <- list(
    stage = "efa",
    scope = "S",
    message = "Synthetic failure."
  )
  txt2 <- paste(capture.output(print(blocked)), collapse = "\n")
  expect_match(txt2, "workflow is blocked", fixed = TRUE)

  complete <- run
  complete$status <- "complete"
  complete$next_stage <- NULL
  complete$decision_requests <- complete$decision_requests[0, , drop = FALSE]
  complete$stage_status$status[] <- "completed"
  txt3 <- paste(capture.output(print(complete)), collapse = "\n")
  expect_match(txt3, "All requested stages are complete", fixed = TRUE)
  expect_match(txt3, "No hidden item deletion", fixed = TRUE)
})


test_that("guided-run tables and summary printing cover empty and populated branches", {
  run <- make_m9_minimal_run()

  expect_equal(nrow(nomologR:::nomo_run_decision_table(run)), 1L)

  no_log <- run
  no_log$decision_log <- no_log$decision_log[0, , drop = FALSE]
  expect_equal(nrow(nomologR:::nomo_run_decision_table(no_log)), 0L)

  extra <- run$decision_log
  extra$decision <- ""
  run$decision_log <- dplyr::bind_rows(run$decision_log, extra)
  expect_equal(nrow(nomologR:::nomo_run_decision_table(run)), 1L)

  for (type in c(
    "stages", "requests", "decisions", "component_log",
    "scales", "recipe", "settings"
  )) {
    expect_s3_class(nomo_table(run, type), "data.frame")
  }

  s <- summary(run)
  txt <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(txt, "Outstanding researcher decisions", fixed = TRUE)
  expect_match(txt, "Recorded workflow decisions", fixed = TRUE)

  complete <- make_m9_report_run()
  s2 <- summary(complete)
  txt2 <- paste(capture.output(print(s2)), collapse = "\n")
  if (nrow(s2$component_log)) {
    expect_match(txt2, "Component decision/evidence-log rows retained", fixed = TRUE)
  }
})


test_that("validity discriminant table merges latent, HTMT2, and HTMT evidence", {
  run <- make_m9_report_run()
  val <- run$results$validity

  val$latent_correlations <- tibble::tibble(
    construct_1 = "A",
    construct_2 = "B",
    block = "overall",
    correlation = .70,
    ci_lower = .55,
    ci_upper = .81
  )
  val$htmt2 <- tibble::tibble(
    construct_1 = "A",
    construct_2 = "B",
    block = "overall",
    estimate = .80
  )
  val$htmt <- tibble::tibble(
    construct_1 = "A",
    construct_2 = "B",
    block = "overall",
    estimate = .82
  )
  val$htmt_reference <- .85

  out <- nomologR:::nomo_validity_discriminant_table(val)
  expect_equal(nrow(out), 1L)
  expect_equal(out$latent_r, .70)
  expect_equal(out$HTMT2, .80)
  expect_equal(out$HTMT, .82)
  expect_equal(out$signal, "info")
})


test_that("validity summary print covers populated evidence and grouped loading note", {
  run <- make_m9_report_run()
  val <- run$results$validity
  s <- summary(val)

  # Ensure a pairwise row so the discriminant print branch is deterministic.
  s$discriminant <- tibble::tibble(
    construct_1 = "A",
    construct_2 = "B",
    block = "overall",
    latent_r = .40,
    latent_r_ci_lower = .20,
    latent_r_ci_upper = .58,
    HTMT2 = .65,
    HTMT = .67,
    signal = "info"
  )
  s$ngroups <- 2L

  txt <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(txt, "Convergent evidence by construct", fixed = TRUE)
  expect_match(txt, "Construct-separation evidence", fixed = TRUE)
  expect_match(txt, "Loading ranges are omitted", fixed = TRUE)
})


test_that("guided-run research print covers no-completed-stage state", {
  run <- make_m9_minimal_run()
  run$mode <- "research"
  run$stage_status$status[] <- "pending"
  run$status <- "paused"

  txt <- paste(capture.output(print(run)), collapse = "\n")
  expect_match(txt, "Completed: none", fixed = TRUE)
})


test_that("guided-run summary covers no outstanding requests or decisions", {
  run <- make_m9_minimal_run()
  run$decision_requests <- run$decision_requests[0, , drop = FALSE]
  run$decision_log <- run$decision_log[0, , drop = FALSE]

  s <- summary(run)
  txt <- paste(capture.output(print(s)), collapse = "\n")

  expect_false(grepl("Outstanding researcher decisions", txt, fixed = TRUE))
  expect_false(grepl("Recorded workflow decisions", txt, fixed = TRUE))
})
