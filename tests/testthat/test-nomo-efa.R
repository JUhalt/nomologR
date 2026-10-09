test_that("nomo_efa recovers a simple correlated two-factor structure", {
  set.seed(3101)
  n <- 700
  f1 <- rnorm(n)
  f2 <- 0.35 * f1 + sqrt(1 - 0.35^2) * rnorm(n)

  dat <- data.frame(
    a1 = .82 * f1 + rnorm(n, sd = .55),
    a2 = .78 * f1 + rnorm(n, sd = .60),
    a3 = .75 * f1 + rnorm(n, sd = .62),
    a4 = .80 * f1 + rnorm(n, sd = .58),
    b1 = .82 * f2 + rnorm(n, sd = .55),
    b2 = .78 * f2 + rnorm(n, sd = .60),
    b3 = .75 * f2 + rnorm(n, sd = .62),
    b4 = .80 * f2 + rnorm(n, sd = .58)
  )

  out <- nomo_efa(dat, factors = 2)

  expect_s3_class(out, "nomo_efa")
  expect_equal(nrow(out$item_summary), 8)
  expect_equal(dim(out$pattern_matrix), c(8, 2))
  expect_equal(dim(out$factor_correlations), c(2, 2))

  primary <- out$item_summary$primary_factor
  expect_true(length(unique(primary[1:4])) == 1L)
  expect_true(length(unique(primary[5:8])) == 1L)
  expect_false(primary[[1L]] == primary[[5L]])

  expect_true(all(abs(out$item_summary$primary_loading) > .55))
  expect_true(all(out$item_summary$communality > .35))
})


test_that("cross-loading and weak items trigger review without deletion", {
  set.seed(3102)
  n <- 900
  f1 <- rnorm(n)
  f2 <- 0.30 * f1 + sqrt(1 - 0.30^2) * rnorm(n)

  dat <- data.frame(
    a1 = .82 * f1 + rnorm(n, sd = .55),
    a2 = .78 * f1 + rnorm(n, sd = .60),
    a3 = .75 * f1 + rnorm(n, sd = .62),
    cross = .58 * f1 + .52 * f2 + rnorm(n, sd = .55),
    b1 = .82 * f2 + rnorm(n, sd = .55),
    b2 = .78 * f2 + rnorm(n, sd = .60),
    b3 = .75 * f2 + rnorm(n, sd = .62),
    weak = .12 * f1 + .10 * f2 + rnorm(n, sd = 1.00)
  )

  out <- nomo_efa(dat, factors = 2)
  cross <- out$item_summary[out$item_summary$item == "cross", ]
  weak <- out$item_summary[out$item_summary$item == "weak", ]

  expect_true(cross$cross_loading)
  expect_true(cross$attention %in% c("REVIEW", "STRONG REVIEW"))
  expect_true(weak$weak_primary || weak$low_communality)
  expect_true(weak$attention %in% c("REVIEW", "STRONG REVIEW"))
  expect_equal(out$items, names(dat))
  expect_equal(nrow(out$item_summary), ncol(dat))
})


test_that("ordinal items use polychoric EFA when declared ordinal", {
  set.seed(3103)
  n <- 550
  f1 <- rnorm(n)
  f2 <- 0.40 * f1 + sqrt(1 - 0.40^2) * rnorm(n)
  z <- data.frame(
    a1 = .80 * f1 + rnorm(n, sd = .65),
    a2 = .75 * f1 + rnorm(n, sd = .68),
    a3 = .78 * f1 + rnorm(n, sd = .66),
    a4 = .72 * f1 + rnorm(n, sd = .70),
    b1 = .80 * f2 + rnorm(n, sd = .65),
    b2 = .75 * f2 + rnorm(n, sd = .68),
    b3 = .78 * f2 + rnorm(n, sd = .66),
    b4 = .72 * f2 + rnorm(n, sd = .70)
  )
  dat <- as.data.frame(lapply(
    z,
    function(x) as.integer(cut(
      x,
      breaks = c(-Inf, -1, -.3, .3, 1, Inf),
      labels = FALSE
    ))
  ))
  types <- stats::setNames(rep("ordinal", ncol(dat)), names(dat))

  out <- nomo_efa(dat, factors = 2, types = types)

  expect_equal(out$correlation, "polychoric")
  expect_true(all(out$modeling_types$model_type == "ordinal"))
  expect_equal(nrow(out$item_summary), 8)
})


test_that("nomo_factors objects hand factor decisions into EFA explicitly", {
  set.seed(3104)
  dat <- as.data.frame(matrix(rnorm(400), ncol = 4))
  names(dat) <- paste0("i", 1:4)

  fake <- list(
    parallel = list(n_factors = 1L),
    items = names(dat),
    correlation = "pearson",
    missing = "pairwise",
    modeling_types = tibble::tibble(
      item = names(dat),
      screen_type = "numeric_continuous",
      model_type = "continuous",
      source = "inferred_from_storage"
    )
  )
  class(fake) <- c("nomo_factors", "list")

  out <- nomo_efa(dat, factors = fake)

  expect_equal(out$n_factors, 1L)
  expect_equal(out$factor_source, "nomo_factors")
  expect_equal(out$correlation, "pearson")
})


test_that("EFA rejects invalid factor counts and nonconstant items", {
  dat <- data.frame(
    a = rnorm(100),
    b = rnorm(100),
    c = rnorm(100),
    d = rnorm(100)
  )

  expect_error(nomo_efa(dat, factors = 0), "positive integer")
  expect_error(nomo_efa(dat, factors = 4), "smaller than")
  dat$c <- 1
  expect_error(nomo_efa(dat, factors = 1), "nonconstant")
})


test_that("presentation methods return stable user-facing objects", {
  set.seed(3105)
  f <- rnorm(300)
  dat <- data.frame(
    i1 = .8 * f + rnorm(300, sd = .6),
    i2 = .8 * f + rnorm(300, sd = .6),
    i3 = .7 * f + rnorm(300, sd = .7),
    i4 = .7 * f + rnorm(300, sd = .7)
  )
  out <- nomo_efa(dat, factors = 1)

  expect_s3_class(summary(out), "summary_nomo_efa")
  expect_s3_class(plot(out, type = "pattern"), "ggplot")
  expect_s3_class(plot(out, type = "items"), "ggplot")
  expect_s3_class(plot(out, type = "residuals"), "ggplot")
  expect_s3_class(plot(out, type = "factor_correlations"), "ggplot")

  expect_output(print(out), "No item was\\s+deleted and no model was refit automatically")
  expect_output(print(summary(out)), "Item structure")
})

# ---- recovered from hygiene consolidation: test-nomo-efa.R ----
# ---- consolidated from test-nomo-efa-factor-count.R ----
make_efa_factor_count_data <- function(n = 260L, seed = 8201L) {
  set.seed(seed)

  f1 <- rnorm(n)
  f2 <- .30 * f1 + sqrt(1 - .30^2) * rnorm(n)

  data.frame(
    a1 = .82 * f1 + rnorm(n, sd = .55),
    a2 = .78 * f1 + rnorm(n, sd = .60),
    a3 = .74 * f1 + rnorm(n, sd = .65),
    b1 = .82 * f2 + rnorm(n, sd = .55),
    b2 = .78 * f2 + rnorm(n, sd = .60),
    b3 = .74 * f2 + rnorm(n, sd = .65)
  )
}


test_that("nomo_efa can inherit M2 context while researcher overrides factor count", {
  dat <- make_efa_factor_count_data()

  fac <- nomo_factors(
    dat,
    criterion_set = "minimal",
    n_iter = 10L,
    seed = 2026
  )

  primary <- as.integer(fac$parallel$n_factors)
  chosen <- if (identical(primary, 1L)) 2L else 1L

  efa <- nomo_efa(
    dat,
    factors = fac,
    factor_count = chosen
  )

  expect_equal(efa$n_factors, chosen)
  expect_equal(
    efa$factor_source,
    "researcher_with_nomo_factors_context"
  )
  expect_equal(
    efa$factor_context$primary_parallel,
    primary
  )
  expect_equal(
    efa$factor_context$selected_factor_count,
    chosen
  )
})


test_that("factor-count override preserves inherited modeling-type provenance", {
  skip_on_cran()
  set.seed(8202L)
  n <- 260L
  f <- rnorm(n)

  make_ord <- function(x) {
    ordered(
      cut(
        x,
        breaks = c(-Inf, -.75, 0, .75, Inf),
        labels = FALSE
      )
    )
  }

  dat <- data.frame(
    q1 = make_ord(.82 * f + rnorm(n, sd = .60)),
    q2 = make_ord(.78 * f + rnorm(n, sd = .62)),
    q3 = make_ord(.76 * f + rnorm(n, sd = .64)),
    q4 = make_ord(.72 * f + rnorm(n, sd = .68))
  )

  fac <- nomo_factors(
    dat,
    types = stats::setNames(rep("ordinal", 4L), names(dat)),
    criterion_set = "minimal",
    n_iter = 10L,
    seed = 2026
  )

  efa <- nomo_efa(
    dat,
    factors = fac,
    factor_count = 1L
  )

  expect_true(all(
    efa$modeling_types$source == "inherited_from_nomo_factors"
  ))
  expect_true(any(
    efa$decision_log$metric == "modeling_types_inherited"
  ))
})


test_that("factor_count is reserved for a nomo_factors handoff", {
  dat <- make_efa_factor_count_data(seed = 8203L)

  expect_error(
    nomo_efa(
      dat,
      factors = 1L,
      factor_count = 1L
    ),
    "only when `factors` is a `nomo_factors` object"
  )

  fac <- nomo_factors(
    dat,
    criterion_set = "minimal",
    n_iter = 10L,
    seed = 2026
  )

  expect_error(
    nomo_efa(
      dat,
      factors = fac,
      factor_count = 1.5
    ),
    "positive integer"
  )
})

# ---- consolidated from test-nomo-efa-hardening.R ----
test_that("nomo_efa validates public arguments and item selection", {
  dat <- data.frame(a = 1:6, b = 2:7, c = 3:8, d = 4:9)

  expect_error(nomo_efa(as.matrix(dat), factors = 1), "data frame")
  expect_error(nomo_efa(dat[FALSE, ], factors = 1), "at least one row")
  expect_error(
    nomo_efa(data.frame(row.names = seq_len(4)), factors = 1),
    "at least one row and one column"
  )
  expect_error(nomo_efa(dat, factors = 1, guidance = 1), "guidance")
  expect_error(nomo_efa(dat, factors = 1, rotation = ""), "rotation")
  expect_error(nomo_efa(dat, factors = 1, fm = ""), "fm")
  expect_error(nomo_efa(dat, factors = 1, smooth = NA), "smooth")
  expect_error(nomo_efa(dat, factors = 1, items = c("a", "b")), "at least three")
  expect_error(
    nomo_efa(dat, factors = 1, items = c("a", "b", "missing")),
    "Unknown item"
  )
  expect_error(nomo_efa(dat, factors = 1.5), "positive integer")
})


test_that("invalid nomo_factors handoffs fail clearly", {
  dat <- data.frame(
    a = rnorm(80), b = rnorm(80), c = rnorm(80), d = rnorm(80)
  )
  bad <- list(
    parallel = list(n_factors = 0L),
    items = names(dat),
    correlation = "pearson",
    missing = "pairwise"
  )
  class(bad) <- c("nomo_factors", "list")

  expect_error(nomo_efa(dat, factors = bad), "does not contain a positive")
})


test_that("complete-case and pairwise missingness guards are explicit", {
  dat_complete <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(1, 2, NA, NA, NA),
    c = c(1, 2, 3, 4, 5),
    d = c(5, 4, 3, 2, 1)
  )
  expect_error(
    nomo_efa(dat_complete, factors = 1, missing = "complete"),
    "Too few complete cases"
  )

  dat_pair <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(1, 2, NA, NA, NA),
    c = c(5, 4, 3, 2, 1),
    d = c(2, 3, 4, 5, 6)
  )
  expect_error(
    nomo_efa(dat_pair, factors = 1, missing = "pairwise"),
    "fewer than three jointly observed"
  )

  dat_nonfinite <- data.frame(
    a = c(1, 1, 1, 2, 3),
    b = c(1, 2, 3, NA, NA),
    c = c(1, 2, 3, 4, 5),
    d = c(5, 4, 3, 2, 1)
  )
  expect_error(
    nomo_efa(dat_nonfinite, factors = 1, missing = "pairwise"),
    "non-finite values"
  )
})


test_that("orthogonal rotation is allowed and explicitly logged", {
  set.seed(3201)
  n <- 450
  f1 <- rnorm(n)
  f2 <- rnorm(n)
  dat <- data.frame(
    a1 = .8 * f1 + rnorm(n, sd = .6),
    a2 = .8 * f1 + rnorm(n, sd = .6),
    a3 = .7 * f1 + rnorm(n, sd = .7),
    a4 = .7 * f1 + rnorm(n, sd = .7),
    b1 = .8 * f2 + rnorm(n, sd = .6),
    b2 = .8 * f2 + rnorm(n, sd = .6),
    b3 = .7 * f2 + rnorm(n, sd = .7),
    b4 = .7 * f2 + rnorm(n, sd = .7)
  )

  out <- nomo_efa(dat, factors = 2, rotation = "varimax")

  expect_false(out$oblique)
  expect_equal(unname(out$factor_correlations), diag(2), tolerance = 1e-8)
  expect_equal(
    dimnames(out$factor_correlations),
    list(colnames(out$pattern_matrix), colnames(out$pattern_matrix))
  )
  row <- out$decision_log[out$decision_log$metric == "extraction_rotation", ]
  expect_equal(row$severity, "review")
  expect_match(row$recommendation, "Orthogonal")
})


test_that("researcher modeling-type overrides are retained in the EFA log", {
  set.seed(3202)
  n <- 350
  f <- rnorm(n)
  raw <- data.frame(
    q1 = .8 * f + rnorm(n, sd = .6),
    q2 = .75 * f + rnorm(n, sd = .65),
    q3 = .8 * f + rnorm(n, sd = .6),
    q4 = .75 * f + rnorm(n, sd = .65)
  )
  dat <- as.data.frame(lapply(
    raw,
    function(x) as.integer(cut(
      x, breaks = c(-Inf, -.6, 0, .6, Inf), labels = FALSE
    ))
  ))
  types <- stats::setNames(rep("ordinal", 4), names(dat))

  out <- nomo_efa(dat, factors = 1, types = types)

  expect_true(any(out$decision_log$metric == "modeling_type_override"))
  expect_equal(out$correlation, "polychoric")
  expect_match(out$extraction_note, "polychoric")
})


test_that("redundant item sets stop unless smoothing is explicit", {
  set.seed(3203)
  n <- 500
  f1 <- rnorm(n)
  f2 <- rnorm(n)
  dat <- data.frame(
    a1 = .8 * f1 + rnorm(n, sd = .6),
    a2 = .75 * f1 + rnorm(n, sd = .65),
    a3 = .7 * f1 + rnorm(n, sd = .7),
    b1 = .8 * f2 + rnorm(n, sd = .6),
    b2 = .75 * f2 + rnorm(n, sd = .65),
    b3 = .7 * f2 + rnorm(n, sd = .7)
  )
  dat$a3 <- dat$a2

  expect_error(
    nomo_efa(dat, factors = 2),
    "not positive definite"
  )

  out <- nomo_efa(dat, factors = 2, smooth = TRUE)
  expect_true(out$smoothed)
  expect_true(any(out$decision_log$metric == "smoothing"))
})


test_that("smoothing and ambiguity are inherited from a nomo_factors handoff", {
  set.seed(3204)
  n <- 450
  f1 <- rnorm(n)
  f2 <- .35 * f1 + sqrt(1 - .35^2) * rnorm(n)
  dat <- data.frame(
    a1 = .8 * f1 + rnorm(n, sd = .6),
    a2 = .75 * f1 + rnorm(n, sd = .65),
    a3 = .7 * f1 + rnorm(n, sd = .7),
    a4 = .72 * f1 + rnorm(n, sd = .68),
    b1 = .8 * f2 + rnorm(n, sd = .6),
    b2 = .75 * f2 + rnorm(n, sd = .65),
    b3 = .7 * f2 + rnorm(n, sd = .7),
    b4 = .72 * f2 + rnorm(n, sd = .68)
  )

  fake <- list(
    parallel = list(n_factors = 2L),
    items = names(dat),
    correlation = "pearson",
    missing = "pairwise",
    smoothed = FALSE,
    plausible_factors = c(1L, 2L, 3L),
    recommendation = "Compare neighboring solutions.",
    modeling_types = tibble::tibble(
      item = names(dat),
      screen_type = "numeric_continuous",
      model_type = "continuous",
      source = "inferred_from_storage"
    )
  )
  class(fake) <- c("nomo_factors", "list")

  out <- nomo_efa(dat, factors = fake)

  expect_equal(out$factor_context$primary_parallel, 2L)
  expect_equal(out$factor_context$plausible_factors, 1:3)
  expect_true(any(out$decision_log$metric == "retention_ambiguity"))
})


test_that("complete-case EFA reports sample adequacy descriptively", {
  set.seed(3205)
  n <- 160
  f <- rnorm(n)
  dat <- data.frame(
    i1 = .8 * f + rnorm(n, sd = .6),
    i2 = .75 * f + rnorm(n, sd = .65),
    i3 = .7 * f + rnorm(n, sd = .7),
    i4 = .72 * f + rnorm(n, sd = .68)
  )
  dat$i1[1:10] <- NA
  dat$i2[11:20] <- NA

  out <- nomo_efa(dat, factors = 1, missing = "complete")

  expect_equal(out$n_cases, 140)
  expect_equal(out$sample_adequacy$n_cases, 140)
  expect_equal(out$sample_adequacy$n_items, 4)
  expect_equal(out$sample_adequacy$cases_per_item, 35)
  expect_true(out$bartlett$available)
})


test_that("small samples trigger review rather than a hard minimum", {
  set.seed(3206)
  n <- 80
  f <- rnorm(n)
  dat <- data.frame(
    i1 = .8 * f + rnorm(n, sd = .6),
    i2 = .75 * f + rnorm(n, sd = .65),
    i3 = .7 * f + rnorm(n, sd = .7),
    i4 = .72 * f + rnorm(n, sd = .68)
  )

  out <- nomo_efa(dat, factors = 1)
  row <- out$decision_log[out$decision_log$metric == "sample_size", ]

  expect_equal(nrow(row), 1L)
  expect_equal(row$severity, "review")
  expect_match(row$reference, "not a universal minimum")
  expect_identical(
    row$observation,
    "80 cases were analyzed for 4 items (20.0 cases per item)."
  )
  expect_equal(out$min_pairwise_n, out$n_cases)

  printed <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(printed, "Cases: 80 |", fixed = TRUE)
  expect_false(grepl("minimum pairwise", printed, fixed = TRUE))
})


test_that("rows with no item data do not inflate the EFA sample-size evidence", {
  # #145, factors-3: under pairwise deletion, nrow() counted empty rows.
  set.seed(3208)
  n <- 120
  f <- rnorm(n)
  dat <- data.frame(
    i1 = .8 * f + rnorm(n, sd = .6),
    i2 = .75 * f + rnorm(n, sd = .65),
    i3 = .7 * f + rnorm(n, sd = .7),
    i4 = .72 * f + rnorm(n, sd = .68)
  )
  dat[1:60, ] <- NA
  dat$i1[61:62] <- NA

  out <- nomo_efa(dat, factors = 1)

  expect_equal(out$n_cases, 120L)
  expect_equal(out$min_pairwise_n, 58L)
  expect_equal(out$sample_adequacy$n_cases, 120L)
  expect_equal(out$sample_adequacy$min_pairwise_n, 58L)
  expect_equal(out$sample_adequacy$cases_per_item, 58 / 4)

  row <- out$decision_log[out$decision_log$metric == "sample_size", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$value, 58)
  expect_identical(
    row$observation,
    paste(
      "120 cases were analyzed for 4 items; the smallest number observed",
      "jointly on an item pair was 58 (14.5 cases per item)."
    )
  )

  expect_output(print(out), "Cases: 120 (minimum pairwise N: 58) |", fixed = TRUE)
  expect_output(
    print(summary(out)),
    "Cases: 120 (minimum pairwise N: 58) |",
    fixed = TRUE
  )
})


test_that("unsupported extraction methods fail before the engine can fall back", {
  set.seed(3207)
  dat <- data.frame(
    a = rnorm(120), b = rnorm(120), c = rnorm(120), d = rnorm(120)
  )

  expect_error(
    nomo_efa(dat, factors = 1, fm = "definitely-not-an-estimator"),
    "Unsupported extraction method"
  )
})


test_that("every documented extraction method runs or fails with a clear reason", {
  dat <- make_efa_factor_count_data(seed = 8204L)

  # minrank needs Rcsdp, which nomologR does not declare (#145, factors-2).
  err <- expect_error(
    nomo_efa(dat, factors = 2, fm = "minrank"),
    "Unsupported extraction method `fm = \"minrank\"`",
    fixed = TRUE
  )
  listed <- sub(".*Use one of: ", "", conditionMessage(err))
  expect_false(grepl("minrank", listed, fixed = TRUE))

  # psych::fa() cannot fit a one-factor alpha solution.
  expect_error(
    nomo_efa(dat, factors = 1, fm = "alpha"),
    "needs at least two factors"
  )
  alpha <- nomo_efa(dat, factors = 2, fm = "alpha")
  expect_identical(alpha$fm, "alpha")
  expect_identical(alpha$fit$fm, "alpha")
})


test_that("fm = \"minchi\" weights by the pairwise Ns instead of running minres", {
  dat <- make_efa_factor_count_data(n = 300L, seed = 8205L)
  for (j in seq_along(dat)) {
    dat[sample(nrow(dat), 25L * j), j] <- NA
  }

  minchi <- nomo_efa(dat, factors = 2, fm = "minchi")
  minres <- nomo_efa(dat, factors = 2, fm = "minres")

  expect_identical(minchi$fit$fm, "minchi")
  expect_false(isTRUE(all.equal(minchi$pattern_matrix, minres$pattern_matrix)))
})


test_that("EFA helper diagnostics cover review combinations safely", {
  pattern <- matrix(
    c(
      .72, .10,
      .45, .34,
      .25, .22
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("good", "cross", "weak"), c("F1", "F2"))
  )

  s <- nomologR:::nomo_efa_item_summary(
    pattern = pattern,
    communality = c(.60, .35, .15),
    uniqueness = c(.40, .65, .85),
    complexity = nomologR:::nomo_efa_complexity(pattern),
    guidance = nomo_defaults()
  )

  expect_equal(s$attention[s$item == "good"], "KEEP")
  expect_equal(s$attention[s$item == "cross"], "STRONG REVIEW")
  expect_equal(s$attention[s$item == "weak"], "STRONG REVIEW")
  expect_match(s$explanation[s$item == "cross"], "secondary loading")
  expect_match(s$explanation[s$item == "weak"], "primary loading")

  one <- matrix(0, 1, 1, dimnames = list("x", "x"))
  rp <- nomologR:::nomo_efa_residual_pairs(one)
  expect_equal(nrow(rp), 0L)

  expect_match(
    nomologR:::nomo_efa_extraction_note("ml", "pearson"),
    "Maximum-likelihood"
  )
  expect_match(
    nomologR:::nomo_efa_extraction_note("pa", "pearson"),
    "researcher-selected"
  )
})


test_that("direct log helper covers KMO concern and explicit smoothing branches", {
  item_summary <- tibble::tibble(
    item = c("a", "b"),
    primary_factor = c("F1", "F1"),
    primary_loading = c(.2, .8),
    secondary_factor = c(NA_character_, NA_character_),
    secondary_loading = c(NA_real_, NA_real_),
    loading_gap = c(NA_real_, NA_real_),
    communality = c(.2, .7),
    uniqueness = c(.8, .3),
    complexity = c(1, 1),
    weak_primary = c(TRUE, FALSE),
    cross_loading = c(FALSE, FALSE),
    low_communality = c(TRUE, FALSE),
    attention = c("STRONG REVIEW", "KEEP"),
    explanation = c("weak and low communality", "no numeric flags")
  )
  item_types <- tibble::tibble(
    item = c("a", "b"),
    screen_type = c("numeric_continuous", "numeric_continuous"),
    model_type = c("continuous", "continuous"),
    source = c("user_override", "inferred_from_storage")
  )

  log <- nomologR:::nomo_efa_log(
    k = 1L,
    factor_source = "researcher",
    factor_context = NULL,
    rotation = "oblimin",
    fm = "minres",
    extraction_note = "note",
    correlation_method = "pearson",
    item_types = item_types,
    missing = "pairwise",
    min_pairwise_n = 50L,
    smoothed = TRUE,
    original_min_eigen = -0.01,
    item_summary = item_summary,
    rmsr = .08,
    kmo = list(available = TRUE, overall = .45),
    n_cases = 50L,
    n_items = 2L,
    guidance = nomo_defaults()
  )

  expect_true(any(log$metric == "smoothing"))
  expect_true(any(log$metric == "modeling_type_override"))
  expect_true(any(log$metric == "item_structure_review"))
  expect_equal(
    log$severity[log$metric == "kmo"],
    "concern"
  )
})


test_that("public EFA outputs use neutral factor names and plain matrices", {
  set.seed(3301)
  n <- 500
  f1 <- rnorm(n)
  f2 <- .3 * f1 + sqrt(1 - .3^2) * rnorm(n)
  dat <- data.frame(
    a1 = .8 * f1 + rnorm(n, sd = .6),
    a2 = .75 * f1 + rnorm(n, sd = .65),
    a3 = .72 * f1 + rnorm(n, sd = .68),
    a4 = .78 * f1 + rnorm(n, sd = .62),
    b1 = .8 * f2 + rnorm(n, sd = .6),
    b2 = .75 * f2 + rnorm(n, sd = .65),
    b3 = .72 * f2 + rnorm(n, sd = .68),
    b4 = .78 * f2 + rnorm(n, sd = .62)
  )

  out <- nomo_efa(dat, factors = 2)

  expect_identical(colnames(out$pattern_matrix), c("F1", "F2"))
  expect_identical(colnames(out$structure_matrix), c("F1", "F2"))
  expect_identical(
    dimnames(out$factor_correlations),
    list(c("F1", "F2"), c("F1", "F2"))
  )
  expect_false(inherits(out$pattern_matrix, "loadings"))
  expect_false(inherits(out$structure_matrix, "loadings"))
})


test_that("M2 handoff preserves modeling-type provenance without inventing overrides", {
  set.seed(3302)
  f <- rnorm(300)
  dat <- data.frame(
    i1 = .8 * f + rnorm(300, sd = .6),
    i2 = .78 * f + rnorm(300, sd = .62),
    i3 = .75 * f + rnorm(300, sd = .65),
    i4 = .72 * f + rnorm(300, sd = .68)
  )

  fake <- list(
    parallel = list(n_factors = 1L),
    items = names(dat),
    correlation = "pearson",
    missing = "pairwise",
    smoothed = FALSE,
    plausible_factors = 1L,
    recommendation = "",
    modeling_types = tibble::tibble(
      item = names(dat),
      screen_type = "numeric_continuous",
      model_type = "continuous",
      source = "inferred_from_storage"
    )
  )
  class(fake) <- c("nomo_factors", "list")

  out <- nomo_efa(dat, factors = fake)

  expect_true(all(
    out$modeling_types$source == "inherited_from_nomo_factors"
  ))
  expect_false(any(
    out$decision_log$metric == "modeling_type_override"
  ))
  expect_true(any(
    out$decision_log$metric == "modeling_types_inherited"
  ))
})


test_that("explicit EFA type declarations remain researcher overrides", {
  set.seed(3303)
  f <- rnorm(350)
  raw <- data.frame(
    q1 = .8 * f + rnorm(350, sd = .6),
    q2 = .75 * f + rnorm(350, sd = .65),
    q3 = .78 * f + rnorm(350, sd = .62),
    q4 = .72 * f + rnorm(350, sd = .68)
  )
  dat <- as.data.frame(lapply(
    raw,
    function(z) as.integer(cut(
      z,
      breaks = c(-Inf, -.6, 0, .6, Inf),
      labels = FALSE
    ))
  ))
  types <- stats::setNames(rep("ordinal", 4), names(dat))

  out <- nomo_efa(dat, factors = 1, types = types)

  expect_true(all(out$modeling_types$source == "user_override"))
  expect_true(any(out$decision_log$metric == "modeling_type_override"))
  expect_false(any(out$decision_log$metric == "modeling_types_inherited"))
})


test_that("polished EFA plots avoid redundant matrix cells", {
  set.seed(3304)
  n <- 450
  f1 <- rnorm(n)
  f2 <- .35 * f1 + sqrt(1 - .35^2) * rnorm(n)
  dat <- data.frame(
    a1 = .8 * f1 + rnorm(n, sd = .6),
    a2 = .75 * f1 + rnorm(n, sd = .65),
    a3 = .72 * f1 + rnorm(n, sd = .68),
    a4 = .78 * f1 + rnorm(n, sd = .62),
    b1 = .8 * f2 + rnorm(n, sd = .6),
    b2 = .75 * f2 + rnorm(n, sd = .65),
    b3 = .72 * f2 + rnorm(n, sd = .68),
    b4 = .78 * f2 + rnorm(n, sd = .62)
  )

  out <- nomo_efa(dat, factors = 2)

  p_resid <- plot(out, type = "residuals")
  p_phi <- plot(out, type = "factor_correlations")
  p_items <- plot(out, type = "items")

  expect_equal(nrow(p_resid$data), choose(ncol(dat), 2))
  expect_equal(nrow(p_phi$data), choose(out$n_factors, 2))
  expect_true(all(c("Primary", "Secondary") %in% p_items$data$loading_type))
})


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("closeout: EFA item summary uses documented fallback references", {
  pattern <- matrix(
    c(.6, .2, .3, .5),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("i1", "i2"), c("F1", "F2"))
  )
  out <- nomologR:::nomo_efa_item_summary(
    pattern = pattern,
    communality = c(.40, .34),
    uniqueness = c(.60, .66),
    complexity = c(1.2, 1.4),
    guidance = list()
  )
  expect_equal(nrow(out), 2L)
})


test_that("closeout B: EFA estimation errors are wrapped with component context", {
  skip_on_cran()
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- data.frame(
    a = rnorm(80),
    b = rnorm(80),
    c = rnorm(80),
    d = rnorm(80)
  )

  testthat::local_mocked_bindings(
    fa = function(...) stop("synthetic EFA engine failure"),
    .package = "psych"
  )

  expect_error(
    nomo_efa(
      dat,
      factors = 1L,
      rotation = "varimax",
      correlation = "pearson",
      missing = "complete"
    ),
    "EFA estimation failed"
  )
})


test_that("closeout B: EFA derives structure, communalities, uniqueness, and complexity when engines omit them", {
  skip_on_cran()
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  set.seed(6201)
  n <- 90
  f1 <- rnorm(n)
  f2 <- .30 * f1 + sqrt(1 - .30^2) * rnorm(n)
  dat <- data.frame(
    a1 = .80 * f1 + rnorm(n, sd = .60),
    a2 = .75 * f1 + rnorm(n, sd = .65),
    a3 = .70 * f1 + rnorm(n, sd = .70),
    b1 = .80 * f2 + rnorm(n, sd = .60),
    b2 = .75 * f2 + rnorm(n, sd = .65),
    b3 = .70 * f2 + rnorm(n, sd = .70)
  )

  original_fa <- psych::fa
  testthat::local_mocked_bindings(
    fa = function(...) {
      z <- original_fa(...)
      z$Structure <- NULL
      z$communality <- NULL
      z$communalities <- NULL
      z$uniquenesses <- NULL
      z$complexity <- NULL
      z
    },
    .package = "psych"
  )

  guidance <- nomo_defaults()
  guidance$factor_small_n_reference <- NULL

  out <- nomo_efa(
    dat,
    factors = 2L,
    rotation = "oblimin",
    correlation = "pearson",
    missing = "complete",
    guidance = guidance
  )

  expect_equal(dim(out$structure_matrix), dim(out$pattern_matrix))
  expect_true(all(is.finite(out$item_summary$communality)))
  expect_true(any(out$decision_log$metric == "sample_size"))
})


test_that("closeout B: EFA uses an engine communalities alias when the primary field is absent", {
  skip_on_cran()
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  set.seed(6202)
  dat <- as.data.frame(matrix(rnorm(480), ncol = 6))
  names(dat) <- paste0("x", 1:6)

  original_fa <- psych::fa
  testthat::local_mocked_bindings(
    fa = function(...) {
      z <- original_fa(...)
      fallback <- if (!is.null(z$communality)) {
        z$communality
      } else {
        rep(.50, nrow(unclass(z$loadings)))
      }
      z$communality <- NULL
      z$communalities <- fallback
      z
    },
    .package = "psych"
  )

  out <- nomo_efa(
    dat,
    factors = 1L,
    rotation = "varimax",
    correlation = "pearson",
    missing = "complete"
  )
  expect_true(all(is.finite(out$item_summary$communality)))
})


test_that("closeout B: EFA presentation covers researcher, review, unavailable adequacy, and orthogonal branches", {
  item_summary <- tibble::tibble(
    item = c("i1", "i2"),
    primary_factor = c("F1", "F2"),
    primary_loading = c(.70, .60),
    secondary_factor = c("F2", "F1"),
    secondary_loading = c(.10, .20),
    communality = c(.50, .45),
    attention = c("KEEP", "REVIEW"),
    explanation = c("No review.", "Synthetic review.")
  )

  efa <- structure(
    list(
      factor_source = "researcher",
      n_cases = 80L,
      n_items = 2L,
      n_factors = 2L,
      correlation = "pearson",
      fm = "minres",
      rotation = "varimax",
      rmsr = .05,
      item_summary = item_summary,
      pattern_matrix = matrix(
        0,
        2, 2,
        dimnames = list(c("i1", "i2"), c("F1", "F2"))
      ),
      items = c("i1", "i2"),
      oblique = FALSE,
      factor_correlations = matrix(
        c(1, 0, 0, 1),
        2, 2,
        dimnames = list(c("F1", "F2"), c("F1", "F2"))
      ),
      residual_matrix = matrix(
        0,
        2, 2,
        dimnames = list(c("i1", "i2"), c("i1", "i2"))
      ),
      residual_pairs = tibble::tibble(),
      kmo = list(available = FALSE, overall = NA_real_),
      bartlett = list(
        available = TRUE,
        df = 1,
        chisq = 2,
        p_value = .123
      ),
      factor_context = NULL,
      extraction_note = "",
      structure_matrix = matrix(
        0,
        2, 2,
        dimnames = list(c("i1", "i2"), c("F1", "F2"))
      ),
      sample_adequacy = tibble::tibble(),
      decision_log = tibble::tibble(),
      guidance = nomo_defaults()
    ),
    class = c("nomo_efa", "list")
  )

  print_text <- paste(capture.output(print(efa)), collapse = "\n")
  expect_match(print_text, "researcher specified", fixed = TRUE)

  s <- summary(efa)
  summary_text <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(summary_text, "researcher specified", fixed = TRUE)
  expect_match(summary_text, "KMO: not computed", fixed = TRUE)
  expect_match(summary_text, "chi-square(1) = 2.00, p = .123", fixed = TRUE)
  expect_match(summary_text, "\nFlagged\n  - i2 (Review): Synthetic review.", fixed = TRUE)
  expect_match(summary_text, "Factor correlations", fixed = TRUE)
  # An orthogonal solution's factor correlations are fixed, not estimated
  # (#145, efa-7).
  expect_match(summary_text, "Fixed at 0: the orthogonal varimax rotation does not",
               fixed = TRUE)
  expect_false(grepl("0.000", summary_text, fixed = TRUE))

  expect_s3_class(plot(efa, type = "pattern"), "ggplot")
  expect_s3_class(plot(efa, type = "residuals"), "ggplot")
  expect_s3_class(plot(efa, type = "factor_correlations"), "ggplot")
})


test_that("closeout C: EFA presentation covers nomo_factors handoff and oblique pattern text", {
  item_summary <- tibble::tibble(
    item = c("i1", "i2"),
    primary_factor = c("F1", "F2"),
    primary_loading = c(.70, .65),
    secondary_factor = c("F2", "F1"),
    secondary_loading = c(.15, .10),
    communality = c(.50, .45),
    attention = c("KEEP", "KEEP"),
    explanation = c("No review.", "No review.")
  )

  efa <- structure(
    list(
      factor_source = "nomo_factors",
      n_cases = 100L,
      n_items = 2L,
      n_factors = 2L,
      correlation = "pearson",
      fm = "minres",
      rotation = "oblimin",
      rmsr = .04,
      item_summary = item_summary,
      pattern_matrix = matrix(
        c(.70, .15, .10, .65),
        2, 2,
        byrow = TRUE,
        dimnames = list(c("i1", "i2"), c("F1", "F2"))
      ),
      items = c("i1", "i2"),
      oblique = TRUE,
      factor_correlations = matrix(
        c(1, .30, .30, 1),
        2, 2,
        dimnames = list(c("F1", "F2"), c("F1", "F2"))
      ),
      residual_matrix = matrix(
        c(0, .02, .02, 0),
        2, 2,
        dimnames = list(c("i1", "i2"), c("i1", "i2"))
      ),
      residual_pairs = tibble::tibble(),
      kmo = list(available = FALSE, overall = NA_real_),
      bartlett = list(
        available = FALSE,
        df = NA_real_,
        chisq = NA_real_,
        p_value = NA_real_
      ),
      factor_context = list(),
      extraction_note = "",
      structure_matrix = matrix(
        c(.70, .36, .30, .65),
        2, 2,
        byrow = TRUE,
        dimnames = list(c("i1", "i2"), c("F1", "F2"))
      ),
      sample_adequacy = tibble::tibble(),
      decision_log = tibble::tibble(),
      guidance = nomo_defaults()
    ),
    class = c("nomo_efa", "list")
  )

  # One wording in print and summary; "handoff" names the contentvalidR object
  # (#145, clarity-29).
  printed <- paste(capture.output(print(efa)), collapse = "\n")
  expect_match(printed, "Factors: 2 (from nomo_factors())", fixed = TRUE)
  expect_false(grepl("handoff", printed, fixed = TRUE))

  summary_printed <- paste(
    capture.output(print(summary(efa))),
    collapse = "\n"
  )
  expect_match(summary_printed, "from nomo_factors()", fixed = TRUE)

  p <- plot(efa, type = "pattern")
  expect_s3_class(p, "ggplot")
  expect_match(
    plot_text(p$labels$subtitle),
    "Oblique solution",
    fixed = TRUE
  )
})

# Pre-1.0 audit findings (#145) -------------------------------------------------

# Six items and three factors at n = 60: the solution gives a1 a communality
# just above 1, which psych only warns about.
make_efa_heywood_data <- function() {
  set.seed(1)
  n <- 60
  f1 <- rnorm(n)
  f2 <- rnorm(n)
  data.frame(
    a1 = f1 + rnorm(n, sd = .5), a2 = f1 + rnorm(n, sd = .8), a3 = f1 + rnorm(n),
    b1 = f2 + rnorm(n, sd = .5), b2 = f2 + rnorm(n, sd = .8), b3 = f2 + rnorm(n)
  )
}


test_that("a Heywood case and a non-converged extraction are recorded, not swallowed (#145, efa-1)", {
  dat <- make_efa_heywood_data()
  out <- nomo_efa(dat, factors = 3)
  expect_gte(max(out$communalities), 1)
  # a1 is above 1; b1 is just below 1, where psych's extractions often stop
  # (its unique variance is below .005, not at it).
  expect_identical(out$heywood, c("a1", "b1"))
  expect_lt(1 - out$communalities[["b1"]], 0.005)
  row <- out$decision_log[out$decision_log$metric == "heywood", ]
  expect_identical(row$object, c("a1", "b1"))
  expect_identical(row$severity, c("concern", "concern"))
  expect_match(row$observation[[1L]],
               "The communality of a1 is 1.0001, so its unique variance is below 0", fixed = TRUE)
  expect_identical(
    row$observation[[2L]],
    sprintf("The communality of b1 is %s, so its unique variance (%s) is at or near 0: %s",
            nomologR:::nomo_present_stat(out$communalities[["b1"]], "proportion", digits = 3L),
            nomologR:::nomo_present_stat(1 - out$communalities[["b1"]], "proportion", digits = 3L),
            "an improper (Heywood) solution.")
  )
  expect_match(row$reference[[1L]], "A communality of .995 or more", fixed = TRUE)
  # The print counts the flags the summary shows: a Heywood item is a concern.
  status <- ifelse(out$item_summary$item %in% out$heywood, "concern", out$item_summary$attention)
  expect_false(identical(nomologR:::nomo_present_flag_counts(status),
                         nomologR:::nomo_present_flag_counts(out$item_summary$attention)))
  expect_output(print(out), paste0("Item flags: ", nomologR:::nomo_present_flag_counts(status)), fixed = TRUE)
  expect_identical(
    sum(grepl("Concern$", capture.output(print(summary(out))))),
    sum(nomologR:::nomo_present_flag(status) == "concern")
  )
  p <- plot(out, type = "items")
  expect_identical(unique(as.character(p$data$status[p$data$item == "a1"])), "concern")
  expect_match(out$engine_warnings, "ultra-Heywood", all = FALSE)
  # psych's own warning is kept on the object but not repeated in the log.
  expect_false(any(out$decision_log$metric == "engine_warnings"))

  printed <- capture.output(print(out))
  expect_true("Solution checks" %in% printed)
  expect_match(printed, "^  - Concern: The communality of a1 is 1.0001", all = FALSE)
  # The summary shows it with its item, as a concern beside its communality.
  summarized <- capture.output(print(summary(out)))
  expect_match(summarized, "^  a1 .* 1\\.0001  Concern$", all = FALSE)
  expect_match(summarized, "^  - a1 \\(Concern\\): The communality is 1.0001, so its unique", all = FALSE)
  expect_true("Solution checks" %in% summarized)
  expect_false(any(grepl("communality of a1", summarized, fixed = TRUE)))

  # Principal axis factoring stops at its iteration limit here.
  pa <- nomo_efa(dat, factors = 3, fm = "pa")
  row <- pa$decision_log[pa$decision_log$metric == "convergence", ]
  expect_identical(row$severity, "concern")
  expect_match(row$observation, "The pa extraction did not converge", fixed = TRUE)
  expect_match(row$observation, "maximum iteration exceeded", fixed = TRUE)

  # A communality of exactly 1 is a Heywood case, above 1 an ultra-Heywood case.
  # Below 1, the unique variance is stated with the communality's decimals.
  heywood_text <- nomologR:::nomo_efa_heywood_text
  expect_identical(
    heywood_text("x", 0.995),
    paste("The communality of x is .995, so its unique variance (.005) is at or near 0: an",
          "improper (Heywood) solution.")
  )
  expect_match(heywood_text(NULL, 0.9997), "^The communality is .9997, so its unique variance \\(.0003\\)")
  expect_match(heywood_text(NULL, 0.99491), "^The communality is .99, so its unique variance \\(.01\\)")
  expect_identical(heywood_text("x", 1),
                   "The communality of x is 1.00, so its unique variance is 0: an improper (Heywood) solution.")
  expect_match(heywood_text(NULL, 1.004), "^The communality is 1.004, so its unique variance is below 0")
  expect_identical(
    nomologR:::nomo_efa_engine_messages(c("Loading required namespace: GPArotation\n",
                                          " maximum  iteration exceeded\n", "",
                                          "maximum iteration exceeded")),
    "maximum iteration exceeded"
  )

  # Any other engine message is recorded for review.
  log <- nomologR:::nomo_efa_log(
    k = 2L, factor_source = "researcher", factor_context = NULL, rotation = "promax",
    fm = "minres", extraction_note = "", correlation_method = "pearson",
    item_types = tibble::tibble(item = "a", source = "inferred_from_storage"),
    missing = "complete", min_pairwise_n = 300L, smoothed = FALSE, original_min_eigen = 0.5,
    item_summary = tibble::tibble(item = character(), attention = character()),
    rmsr = 0.02, kmo = list(available = FALSE), n_cases = 300L, n_items = 6L,
    guidance = nomo_defaults(), engine_messages = "A message from the engine"
  )
  expect_identical(log$severity[log$metric == "engine_warnings"], "review")
  expect_identical(log$observation[log$metric == "engine_warnings"],
                   "psych::fa() reported: \"A message from the engine\".")
  # promax is oblique: the rotation needs no justification.
  expect_identical(log$severity[log$metric == "extraction_rotation"], "info")
})


test_that("a parallel-analysis count of 0 does not block a researcher's factor count (#145, efa-2)", {
  set.seed(3301)
  dat <- as.data.frame(matrix(rnorm(160 * 4), ncol = 4))
  names(dat) <- paste0("i", 1:4)
  dat$i2 <- dat$i1 + rnorm(160)
  zero <- list(
    parallel = list(n_factors = 0L),
    items = names(dat),
    correlation = "pearson",
    missing = "pairwise",
    plausible_factors = 0L,
    recommendation = "No factor."
  )
  class(zero) <- c("nomo_factors", "list")

  err <- expect_error(nomo_efa(dat, factors = zero), "does not contain a positive")
  expect_match(conditionMessage(err), "parallel analysis suggested 0 factors", fixed = TRUE)
  expect_match(conditionMessage(err), "`factor_count = 1`", fixed = TRUE)

  out <- nomo_efa(dat, factors = zero, factor_count = 1)
  expect_identical(out$n_factors, 1L)
  expect_identical(out$factor_source, "researcher_with_nomo_factors_context")
  expect_identical(out$factor_context$primary_parallel, 0L)
  row <- out$decision_log[out$decision_log$metric == "retention_ambiguity", ]
  expect_identical(row$severity, "review")
  expect_identical(
    row$observation,
    paste("The researcher selected 1 factor after reviewing the nomo_factors() evidence.",
          "Primary parallel analysis suggested 0. Parallel analysis found no factor above",
          "the null reference.")
  )

  # A malformed suggestion is still refused.
  zero$parallel$n_factors <- NA_integer_
  expect_error(nomo_efa(dat, factors = zero, factor_count = 1), "does not contain a positive")

  # The guided workflow's example is a count nomo_efa() can fit.
  requests <- nomologR:::nomo_run_factor_requests(list(
    scales = list(A = names(dat)),
    results = list(factors = list(A = list(parallel = list(n_factors = 0L))))
  ))
  expect_identical(requests$example, "decisions = list(factor_count = 1L)")
})


test_that("models the correlations cannot identify are flagged (#145, efa-3)", {
  set.seed(3302)
  g <- rnorm(300)
  dat <- as.data.frame(replicate(4, g + rnorm(300)))

  three <- nomo_efa(dat, factors = 3)
  expect_identical(three$dof, -3)
  row <- three$decision_log[three$decision_log$metric == "identification", ]
  expect_identical(row$severity, "concern")
  expect_match(row$observation, "^3 factors on 4 items leave -3 degrees of freedom: more parameters")
  expect_match(row$recommendation, "Fit fewer factors", fixed = TRUE)
  expect_match(paste(capture.output(print(three)), collapse = " "),
               "Concern: 3 factors on 4 items leave -3 degrees of freedom", fixed = TRUE)

  two <- nomo_efa(dat, factors = 2)
  expect_identical(two$decision_log$value[two$decision_log$metric == "identification"], -1)

  # One factor on three items is just identified: review, not concern.
  one <- nomo_efa(dat[, 1:3], factors = 1)
  row <- one$decision_log[one$decision_log$metric == "identification", ]
  expect_identical(row$severity, "review")
  expect_match(row$observation, "^1 factor on 3 items leaves 0 degrees of freedom: the model reproduces")

  four <- nomo_efa(dat, factors = 1)
  expect_false(any(four$decision_log$metric == "identification"))
})


test_that("rotation is checked, and the log and methods describe the solution fitted (#145, efa-4)", {
  dat <- make_efa_factor_count_data(seed = 8301L)

  expect_error(nomo_efa(dat, factors = 2, rotation = "oblimn"),
               "`rotation` must be one of \"oblimin\",", fixed = TRUE)
  expect_error(nomo_efa(dat, factors = 1, rotation = "oblimn"), 'not "oblimn"', fixed = TRUE)
  expect_error(nomo_efa(dat, factors = 2, rotation = "targetQ"), "needs a target matrix",
               fixed = TRUE)
  # psych runs bifactor and biquartimin only when it is attached, so they are
  # refused with the reason rather than failing inside the engine.
  for (rotation in c("bifactor", "biquartimin")) {
    expect_error(nomo_efa(dat, factors = 3, rotation = rotation),
                 sprintf("`rotation = \"%s\"` is not available: psych::fa() runs it", rotation),
                 fixed = TRUE)
  }

  rotation_row <- function(fit) {
    fit$decision_log[fit$decision_log$metric == "extraction_rotation", ]
  }

  one <- nomo_efa(dat, factors = 1)
  expect_identical(rotation_row(one)$severity, "info")
  expect_match(rotation_row(one)$observation, "a one-factor solution is not rotated", fixed = TRUE)
  expect_identical(rotation_row(one)$recommendation,
                   "Interpret the loadings directly; one factor has no factor correlations.")

  none <- nomo_efa(dat, factors = 2, rotation = "none")
  expect_false(none$oblique)
  expect_identical(rotation_row(none)$severity, "review")
  expect_match(rotation_row(none)$recommendation, "An unrotated solution was researcher-selected")

  geomin <- nomo_efa(dat, factors = 2, rotation = "geominT")
  expect_identical(rotation_row(geomin)$severity, "review")
  expect_match(rotation_row(geomin)$observation, "rotation = geominT (orthogonal)", fixed = TRUE)

  promax <- nomo_efa(dat, factors = 2, rotation = "promax")
  expect_true(promax$oblique)
  expect_identical(rotation_row(promax)$severity, "info")

  # The methods registry credits the rotation recorded: any oblique rotation
  # as "Oblique rotation", varimax by name, another orthogonal rotation by its
  # own entry, and an unrotated solution with none (#145, methods-efa-credit).
  rotations <- c("oblique_rotation", "orthogonal_rotation", "orthogonal_rotation_other")
  used <- function(fit) intersect(nomo_methods_used(fit), rotations)
  expect_identical(used(promax), "oblique_rotation")
  expect_identical(used(none), character())
  expect_identical(used(geomin), "orthogonal_rotation_other")
  expect_identical(used(nomo_efa(dat, factors = 2)), "oblique_rotation")

  # The console and plots say what was fitted (#145, efa-7).
  expect_output(print(one), "Rotation: not applicable (one factor)", fixed = TRUE)
  expect_output(print(none), "Rotation: none (unrotated)", fixed = TRUE)
  expect_output(print(summary(none)), "Fixed at 0: an unrotated solution does not estimate them.",
                fixed = TRUE)
  expect_match(plot_text(plot(one)$labels$subtitle), "One-factor solution", fixed = TRUE)
  expect_match(plot_text(plot(none)$labels$subtitle), "Unrotated solution", fixed = TRUE)
  expect_match(plot_text(plot(none, type = "factor_correlations")$labels$subtitle),
               "uncorrelated by construction", fixed = TRUE)
  expect_match(plot_text(plot(geomin)$labels$subtitle), "Orthogonal solution", fixed = TRUE)
})


test_that("every rotation nomo_efa() accepts fits two and three factors (#145, efa-4)", {
  skip_on_cran()
  set.seed(8305)
  f <- matrix(stats::rnorm(240 * 3), ncol = 3)
  dat <- as.data.frame(f[, rep(1:3, each = 3)] + matrix(stats::rnorm(240 * 9, sd = 0.8), ncol = 9))
  names(dat) <- paste0(rep(c("a", "b", "c"), each = 3), 1:3)
  rotations <- nomologR:::nomo_efa_rotations
  accepted <- unlist(rotations[c("oblique", "orthogonal", "unrotated")], use.names = FALSE)
  expect_length(accepted, 16L)
  expect_false(any(c(rotations$target, rotations$general) %in% accepted))
  for (rotation in accepted) {
    for (k in 2:3) {
      fit <- nomo_efa(dat, factors = k, rotation = rotation)
      expect_s3_class(fit, "nomo_efa")
      expect_identical(fit$oblique, rotation %in% rotations$oblique, label = rotation)
    }
  }
})


test_that("flag explanations are sentences that tell a value from its reference (#145, efa-5)", {
  pattern <- matrix(
    c(.72, .10,
      -.3996, .05,
      .45, -.3004),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("good", "near", "cross"), c("F1", "F2"))
  )
  s <- nomologR:::nomo_efa_item_summary(
    pattern = pattern,
    communality = c(.60, .3997, .50),
    uniqueness = c(.40, .6003, .50),
    complexity = c(1, 1, 2),
    guidance = nomo_defaults()
  )
  expect_identical(
    s$explanation[s$item == "near"],
    paste("The primary loading, 0.3996 in absolute value, is below the 0.40 teaching",
          "reference. The communality, .3997, is below the .40 teaching reference.")
  )
  expect_identical(
    s$explanation[s$item == "cross"],
    paste("The secondary loading, 0.3004 in absolute value, is at or above the 0.30",
          "cross-loading reference.")
  )
  expect_match(s$explanation[s$item == "good"], "^No loading or communality reaches")
  expect_false(any(grepl("|", s$explanation, fixed = TRUE)))
  expect_false(any(grepl("meets/exceeds", s$explanation, fixed = TRUE)))
})


test_that("a partial guidance list is completed from the defaults (#145, efa-6)", {
  dat <- make_efa_factor_count_data(seed = 8302L)
  expect_s3_class(nomo_efa(dat, factors = 2, guidance = list()), "nomo_efa")
  stricter <- nomo_efa(dat, factors = 2, guidance = list(efa_loading_reference = 0.95))
  expect_identical(stricter$guidance$efa_loading_reference, 0.95)
  expect_identical(stricter$guidance$efa_communality_reference, 0.40)
  expect_true(all(stricter$item_summary$weak_primary))

  types <- c("ordinal")
  names(types) <- NA_character_
  expect_error(nomo_efa(dat, factors = 2, types = types), "named character vector")
})


test_that("the log says where the factor count came from without milestone labels (#145, efa-8)", {
  dat <- make_efa_factor_count_data(seed = 8303L)
  fake <- list(
    parallel = list(n_factors = 2L),
    items = names(dat),
    correlation = "pearson",
    missing = "pairwise",
    plausible_factors = 1:3,
    modeling_types = tibble::tibble(
      item = names(dat), screen_type = "numeric_continuous",
      model_type = "continuous", source = "inferred_from_storage"
    )
  )
  class(fake) <- c("nomo_factors", "list")
  for (out in list(nomo_efa(dat, factors = fake), nomo_efa(dat, factors = fake, factor_count = 3))) {
    text <- unlist(out$decision_log[c("observation", "reference", "recommendation")])
    expect_false(any(grepl("\\bM2\\b", text)))
    expect_false(any(grepl("handoff", text, fixed = TRUE)))
  }
  adopted <- nomo_efa(dat, factors = fake)$decision_log
  expect_identical(
    adopted$observation[adopted$metric == "retention_ambiguity"],
    paste("The parallel-analysis count from nomo_factors() selected 2 factors for this EFA;",
          "the broader plausible set includes: 1, 2, 3.")
  )
})


test_that("EFA output follows the shared style at 80 and 40 columns (#144)", {
  dat <- make_efa_factor_count_data(seed = 8304L)
  dat$a3 <- dat$a3 + 0.9 * dat$b1
  out <- nomo_efa(dat, factors = 2)

  for (width in c(80L, 40L)) {
    local_reproducible_output(width = width)
    printed <- capture.output(print(out))
    summarized <- capture.output(print(summary(out)))
    expect_identical(printed[[1L]], "<nomo_efa> Exploratory factor analysis")
    expect_true(all(nchar(c(printed, summarized)) <= width))
    expect_match(gsub("\\s+", " ", paste(printed, collapse = " ")),
                 "See summary(x) for the loadings and the reason for each flag.", fixed = TRUE)
    expect_false(any(grepl("NA|-0\\.00|p-value|STRONG|KEEP", c(printed, summarized))))
    expect_true("Flagged" %in% summarized)
    expect_true("Abbreviations" %in% summarized)
  }

  local_reproducible_output(width = 40)
  summarized <- capture.output(print(summary(out)))
  # The status column is kept for width; dropped columns name nomo_table().
  expect_match(summarized[grepl("^  Item +Factor", summarized)], "Flag$")
  expect_match(paste(summarized, collapse = " "), "Not shown for width: .* See\\s+nomo_table\\(x,\\s+\"items\"\\)\\.")
})


test_that("the item plot shows each flag by shape and color with its references (#145, efa-7)", {
  out <- nomo_efa(nomo_demo_continuous, factors = 2)
  p <- plot(out, type = "items")
  built <- ggplot2::ggplot_build(p)
  expect_true(all(c("Primary", "Secondary") %in% p$data$loading_type))
  expect_identical(levels(p$data$status), c("none", "review", "concern", "not computed"))
  expect_identical(ggplot2::get_guide_data(p, "shape")$.label, c("No flag", "Review", "Concern"))
  expect_false(grepl("KEEP|STRONG", plot_text(p$labels$caption)))
  expect_match(plot_text(p$labels$subtitle), "0.40 loading and 0.30 cross-loading", fixed = TRUE)
  expect_s3_class(built, "ggplot_built")

  one <- plot(nomo_efa(nomo_demo_continuous[, 1:5], factors = 1), type = "items")
  expect_false("Secondary" %in% as.character(one$data$loading_type))
  # Only the reference line drawn is named.
  expect_identical(plot_text(one$labels$subtitle),
                   "Absolute loadings. The dashed line marks the 0.40 loading teaching reference.")
  expect_false(grepl("cross-loading", plot_text(one$labels$subtitle), fixed = TRUE))

  phi <- ggplot2::ggplot_build(plot(out, type = "factor_correlations"))
  expect_match(phi$data[[2L]]$label, "^-?\\.[0-9]{2}$")
  pattern <- ggplot2::ggplot_build(plot(out, type = "pattern"))
  expect_match(pattern$data[[2L]]$label, "^-?[0-9]\\.[0-9]{2}$")
  expect_match(plot_text(plot(out, type = "residuals")$labels$subtitle),
               "Root mean square residual (RMSR) = 0.0", fixed = TRUE)
})


test_that("a KMO below a reference names it in the EFA log (#144)", {
  log_with <- function(kmo) {
    nomologR:::nomo_efa_log(
      k = 1L, factor_source = "researcher", factor_context = NULL, rotation = "oblimin",
      fm = "minres", extraction_note = "", correlation_method = "pearson",
      item_types = tibble::tibble(item = "a", source = "inferred_from_storage"),
      missing = "complete", min_pairwise_n = 300L, smoothed = FALSE, original_min_eigen = 0.5,
      item_summary = tibble::tibble(item = character(), attention = character()),
      rmsr = 0.02, kmo = list(available = TRUE, overall = kmo), n_cases = 300L, n_items = 6L,
      guidance = nomo_defaults()
    )
  }
  observation <- function(log) log$observation[log$metric == "kmo"]
  expect_identical(observation(log_with(0.55)), "Overall KMO = .55, below the .60 review reference.")
  expect_identical(observation(log_with(0.4996)), "Overall KMO = .4996, below the .50 concern reference.")
  expect_identical(observation(log_with(0.8)), "Overall KMO = .80.")
})
