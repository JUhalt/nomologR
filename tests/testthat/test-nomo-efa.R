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

  expect_output(print(out), "No items were automatically deleted")
  expect_output(print(summary(out)), "Item-level structural review")
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
