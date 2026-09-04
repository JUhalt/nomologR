make_factor_coverage_data <- function(n = 180L, p = 8L, seed = 9001L) {
  set.seed(seed)
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  f1 <- z1
  f2 <- 0.35 * z1 + sqrt(1 - 0.35^2) * z2
  x <- lapply(seq_len(p), function(j) {
    f <- if (j <= ceiling(p / 2)) f1 else f2
    0.78 * f + stats::rnorm(n, sd = 0.62)
  })
  out <- as.data.frame(x)
  names(out) <- paste0("i", seq_len(p))
  out
}


test_that("nomo_factors hardens public input validation branches", {
  good <- make_factor_coverage_data(n = 30L, p = 3L)

  expect_error(
    nomo_factors(data.frame(a = numeric(), b = numeric(), c = numeric())),
    "at least one row"
  )
  expect_error(
    nomo_factors(data.frame(row.names = seq_len(3))),
    "at least one column"
  )
  expect_error(nomo_factors(good, guidance = 1), "guidance.*list")

  expect_error(nomo_factors(good, items = 1:3), "non-empty character")
  expect_error(nomo_factors(good, items = character()), "non-empty character")
  expect_error(nomo_factors(good, items = c("i1", NA_character_, "i3")), "missing or empty")
  expect_error(nomo_factors(good, items = c("i1", "", "i3")), "missing or empty")
  expect_error(nomo_factors(good, items = c("i1", "i1", "i3")), "duplicate")

  expect_error(nomo_factors(good, n_iter = NA_real_), "n_iter")
  expect_error(nomo_factors(good, n_iter = "10"), "n_iter")
  expect_error(nomo_factors(good, n_iter = 10.5), "n_iter")
  expect_error(nomo_factors(good, quantile = NA_real_), "quantile")
  expect_error(nomo_factors(good, quantile = "0.95"), "quantile")
  expect_error(nomo_factors(good, quantile = 0), "quantile")
  expect_error(nomo_factors(good, seed = NA_real_), "seed")
  expect_error(nomo_factors(good, seed = 1.5), "seed")
  expect_error(nomo_factors(good, fm = NA_character_), "fm")
  expect_error(nomo_factors(good, fm = ""), "fm")
  expect_error(nomo_factors(good, fm = 1), "fm")
  expect_error(nomo_factors(good, smooth = NA), "smooth")
  expect_error(nomo_factors(good, smooth = 1), "smooth")
})


test_that("nomo_factors rejects unusable observed-data configurations explicitly", {
  constant <- data.frame(
    a = rep(1, 8),
    b = 1:8,
    c = 8:1
  )
  expect_error(
    nomo_factors(constant),
    "nonconstant observed data"
  )

  all_missing <- data.frame(
    a = rep(NA_real_, 8),
    b = 1:8,
    c = 8:1
  )
  expect_error(
    nomo_factors(all_missing),
    "nonconstant observed data"
  )

  sparse <- data.frame(
    a = c(1, 2, 3, 4),
    b = c(2, 3, NA, NA),
    c = c(3, 4, NA, NA)
  )
  expect_error(
    nomo_factors(sparse, missing = "complete"),
    "Fewer than three complete cases"
  )
  expect_error(
    nomo_factors(sparse, missing = "pairwise"),
    "fewer than three jointly observed"
  )

  good <- make_factor_coverage_data(n = 40L, p = 4L, seed = 9002L)
  expect_error(nomo_factors(good, max_factors = 0), "positive integer")
  expect_error(nomo_factors(good, max_factors = 1.5), "positive integer")
})


test_that("model-type validation covers invalid defaults and overrides", {
  numeric_dat <- data.frame(a = 1:5, b = 2:6, c = 3:7)
  numeric_screen <- stats::setNames(
    rep("numeric_continuous", 3L),
    names(numeric_dat)
  )

  expect_error(
    nomo_factors_model_types(
      selected = data.frame(a = factor(letters[1:5]), b = 2:6, c = 3:7),
      items = c("a", "b", "c"),
      screen_types = c(a = "nominal", b = "numeric_continuous", c = "numeric_continuous")
    ),
    "defensible default"
  )

  expect_error(
    nomo_factors_model_types(numeric_dat, names(numeric_dat), numeric_screen, "ordinal"),
    "named character vector"
  )
  expect_error(
    nomo_factors_model_types(
      numeric_dat,
      names(numeric_dat),
      numeric_screen,
      c(a = "ordinal", a = "ordinal")
    ),
    "unique item names"
  )
  expect_error(
    nomo_factors_model_types(
      numeric_dat,
      names(numeric_dat),
      numeric_screen,
      c(z = "ordinal")
    ),
    "not selected"
  )
  expect_error(
    nomo_factors_model_types(
      numeric_dat,
      names(numeric_dat),
      numeric_screen,
      c(a = "nominal")
    ),
    "Unsupported modeling type"
  )

  ordered_dat <- numeric_dat
  ordered_dat$a <- ordered(letters[1:5])
  ordered_screen <- numeric_screen
  ordered_screen[["a"]] <- "ordered"
  expect_error(
    nomo_factors_model_types(
      ordered_dat,
      names(ordered_dat),
      ordered_screen,
      c(a = "continuous")
    ),
    "marked continuous"
  )

  character_dat <- numeric_dat
  character_dat$a <- letters[1:5]
  expect_error(
    nomo_factors_model_types(
      character_dat,
      names(character_dat),
      numeric_screen,
      c(a = "ordinal")
    ),
    "marked ordinal"
  )

  three_level <- numeric_dat
  three_level$a <- c(1, 2, 3, 1, 2)
  discrete_screen <- numeric_screen
  discrete_screen[["a"]] <- "numeric_discrete"
  expect_error(
    nomo_factors_model_types(
      three_level,
      names(three_level),
      discrete_screen,
      c(a = "binary")
    ),
    "exactly two observed values"
  )
})


test_that("numeric conversion and explicit correlation guards are covered", {
  selected <- data.frame(
    continuous = c(1.2, 2.3, 3.4, 4.5),
    ordinal = ordered(c("low", "mid", "high", "mid"), levels = c("low", "mid", "high")),
    binary_factor = factor(c("no", "yes", "no", "yes")),
    binary_logical = c(TRUE, FALSE, TRUE, FALSE),
    binary_numeric = c(10, 20, 10, 20)
  )
  types <- tibble::tibble(
    item = names(selected),
    screen_type = c("numeric_continuous", "ordered", "binary", "binary", "binary"),
    model_type = c("continuous", "ordinal", "binary", "binary", "binary"),
    source = "test"
  )

  converted <- nomo_factors_numeric_data(selected, types)
  expect_true(all(vapply(converted, is.numeric, logical(1))))
  expect_identical(sort(unique(converted$binary_numeric)), c(0, 1))

  expect_identical(
    nomo_factors_choose_correlation("pearson", c("ordinal", "ordinal")),
    "pearson"
  )
  expect_identical(
    nomo_factors_choose_correlation("mixed", c("continuous", "ordinal")),
    "mixed"
  )
  expect_error(
    nomo_factors_choose_correlation("tetrachoric", c("binary", "ordinal")),
    "requires all selected items to be binary"
  )
  expect_error(
    nomo_factors_choose_correlation("polychoric", c("continuous", "ordinal")),
    "requires ordinal/binary"
  )

  x <- data.frame(a = 1:6, b = 2:7, c = 3:8)
  expect_error(
    nomo_factors_correlation(
      x,
      model_types = rep("continuous", 3L),
      method = "not-a-method",
      use = "pairwise"
    ),
    "Could not estimate the not-a-method correlation matrix"
  )

  expect_error(
    nomo_factors_factor_eigenvalues("not-a-correlation-matrix", fm = "minres"),
    "Could not obtain common-factor eigenvalues"
  )
})


test_that("pairwise-N, MAP truncation, KMO singularity, and Bartlett unavailability are explicit", {
  x <- data.frame(
    a = c(1, 2, NA, 4),
    b = c(1, NA, 3, 4),
    c = c(NA, 2, 3, 4)
  )
  nmat <- nomo_factors_pairwise_n(x)
  expect_equal(unname(diag(nmat)), c(3L, 3L, 3L))
  expect_equal(nmat[1, 2], 2L)

  perfect <- matrix(1, nrow = 4, ncol = 4)
  mapped <- nomo_factors_map(perfect, max_factors = 3L)
  expect_true(mapped$truncated)
  expect_equal(mapped$m_last, 0L)

  kmo <- nomo_factors_kmo(perfect, paste0("i", 1:4))
  expect_false(kmo$available)
  expect_true(all(is.na(kmo$item$msa)))

  bart <- nomo_factors_bartlett(diag(3), n = 30L, available = FALSE)
  expect_false(bart$available)
  expect_match(bart$reason, "pairwise missing-data")
})


test_that("small samples and unavailable supporting diagnostics are logged", {
  item_types <- tibble::tibble(
    item = c("a", "b", "c"),
    screen_type = rep("numeric_continuous", 3L),
    model_type = rep("continuous", 3L),
    source = rep("inferred_from_storage", 3L)
  )
  pa <- list(n_factors = 1L, rule = "percentile", quantile = 0.95,
             sensitivity = tibble::tibble(rule = c("percentile", "mean", "crawford"), n_factors = 1L, selected = c(TRUE, FALSE, FALSE)),
             n_smoothed_null = 0L)
  map <- list(n_factors_original = 1L, n_factors_revised = 1L)
  criteria <- list(
    evidence = tibble::tibble(
      criterion = c("parallel", "map_original"),
      method = c("Parallel analysis", "MAP (original TR2)"),
      family = c("parallel", "map"),
      family_method = c("Parallel analysis", "MAP"),
      n_factors = c(1L, 1L),
      role = c("primary", "complementary"),
      reference = c("", "")
    ),
    status = tibble::tibble(
      criterion = c("parallel", "map_original"),
      method = c("Parallel analysis", "MAP (original TR2)"),
      status = c("available", "available"),
      reason = c("", ""),
      qualification = c("", "")
    )
  )
  synthesis <- nomo_factors_synthesis(criteria$evidence, parallel_n = 1L, status = criteria$status)

  log <- nomo_factors_log(
    item_types = item_types,
    requested_correlation = "pearson",
    correlation_method = "pearson",
    missing = "complete",
    min_pairwise_n = 40L,
    smoothed = FALSE,
    original_min_eigen = 0.2,
    kmo = list(available = FALSE, overall = NA_real_),
    bartlett = list(available = FALSE, p_value = NA_real_, reason = "Unavailable for test."),
    pa = pa,
    map = map,
    criteria = criteria,
    synthesis = synthesis,
    guidance = nomo_defaults()
  )

  expect_true(any(log$metric == "small_sample_prompt"))
  expect_true(any(log$metric == "kmo" & log$severity == "review"))
  expect_true(any(log$metric == "bartlett"))
})


test_that("criterion plans, metadata, extraction, and seed helpers cover all variants", {
  expect_identical(nomo_factors_criterion_plan("minimal"), c("parallel", "map_original"))
  expect_equal(length(nomo_factors_criterion_plan("core")), 4L)
  expect_equal(length(nomo_factors_criterion_plan("extended")), 6L)
  expect_equal(length(nomo_factors_criterion_plan("all")), 8L)

  ids <- c("parallel", "map_original", "map_revised", "ekc", "nest", "hull", "comparison_data", "kaiser")
  meta <- lapply(ids, nomo_factors_criterion_metadata)
  expect_true(all(vapply(meta, is.list, logical(1))))
  expect_true(all(vapply(meta, function(x) nzchar(x$method), logical(1))))

  expect_true(is.na(nomo_factors_extract_n(numeric())))
  expect_true(is.na(nomo_factors_extract_n(c(a = NA_real_, b = Inf))))
  expect_equal(nomo_factors_extract_n(c(other = 2, BvA = 3), preferred = "BvA"), 3L)
  expect_equal(nomo_factors_extract_n(c(2.4)), 2L)

  expect_true(nomo_factors_seed_offset(-10L, 5L) >= 1L)
  expect_type(nomo_factors_seed_offset(.Machine$integer.max, 100L), "integer")
})


test_that("criterion wrappers convert engine failures into documented unavailability", {
  ekc <- nomo_factors_ekc("not-a-correlation-matrix", n_obs = 20L)
  expect_false(isTRUE(ekc$available))
  expect_match(ekc$reason, "EKC could not be computed")

  nest <- nomo_factors_nest(
    "not-a-correlation-matrix",
    n_obs = 20L,
    n_iter = 10L,
    seed = 9201L
  )
  expect_false(isTRUE(nest$available))
  expect_match(nest$reason, "NEST could not be computed")

  hull <- nomo_factors_hull(
    "not-a-correlation-matrix",
    n_obs = 20L,
    n_iter = 10L,
    quantile = 0.95,
    seed = 9202L
  )
  expect_false(isTRUE(hull$available))
  expect_match(hull$reason, "Hull could not be computed")

  cd <- nomo_factors_cd(
    x = "not-data",
    max_factors = 2L,
    n_population = 100L,
    n_samples = 10L,
    alpha = 0.30,
    seed = 9203L
  )
  expect_false(isTRUE(cd$available))
  expect_match(cd$reason, "Comparison data could not be computed")
})


test_that("extended criteria document missing-data and short-scale skips", {
  dat <- make_factor_coverage_data(n = 140L, p = 8L, seed = 9301L)
  dat[1:10, 1] <- NA_real_
  dat[11:20, 2] <- NA_real_

  out <- nomo_factors(
    dat,
    criterion_set = "extended",
    missing = "pairwise",
    n_iter = 10,
    seed = 9302L
  )

  status <- out$criterion_status
  expect_identical(status$status[status$criterion == "ekc"], "skipped")
  expect_match(status$reason[status$criterion == "ekc"], "common sample size")
  expect_true(all(status$status[status$criterion %in% c("nest", "hull")] == "skipped"))
  expect_true(all(grepl(
    "complete common sample",
    status$reason[status$criterion %in% c("nest", "hull")],
    fixed = TRUE
  )))

  short <- make_factor_coverage_data(n = 150L, p = 5L, seed = 9303L)
  short_out <- nomo_factors(
    short,
    criterion_set = "extended",
    n_iter = 10,
    seed = 9304L
  )
  hull_status <- short_out$criterion_status[short_out$criterion_status$criterion == "hull", , drop = FALSE]
  expect_identical(hull_status$status, "skipped")
  expect_match(hull_status$reason, "at least six indicators")
})


test_that("all criterion set exposes comparison data and legacy context without legacy voting", {
  dat <- make_factor_coverage_data(n = 160L, p = 8L, seed = 9401L)
  guidance <- nomo_defaults()
  guidance$factor_cd_population <- 600L
  guidance$factor_cd_samples <- 10L
  guidance$factor_cd_alpha <- 0.30

  out <- nomo_factors(
    dat,
    criterion_set = "all",
    max_factors = 3L,
    n_iter = 10,
    seed = 9402L,
    guidance = guidance
  )

  expect_identical(
    out$criterion_status$criterion,
    c("parallel", "map_original", "map_revised", "ekc", "nest", "hull", "comparison_data", "kaiser")
  )
  expect_true(any(out$evidence$criterion == "kaiser"))
  expect_identical(out$evidence$role[out$evidence$criterion == "kaiser"], "legacy")
  expect_false(any(out$family_evidence$family == "kaiser"))
  expect_true(out$criterion_status$status[out$criterion_status$criterion == "comparison_data"] %in% c("available", "skipped"))
})


test_that("synthesis covers primary-only, neighboring, divergent, and skipped-method narratives", {
  primary_only <- nomo_factors_synthesis(
    tibble::tibble(),
    parallel_n = 1L,
    status = tibble::tibble(status = "skipped")
  )
  expect_identical(primary_only$agreement, "primary_only")
  expect_match(primary_only$text, "No additional recommended criterion families")
  expect_match(primary_only$text, "1 requested method was not evaluated")

  make_evidence <- function(counts) {
    tibble::tibble(
      criterion = c("parallel", "map_original", "ekc"),
      method = c("Parallel analysis", "MAP (original TR2)", "Empirical Kaiser criterion"),
      n_factors = as.integer(counts),
      role = c("primary", "complementary", "complementary"),
      reference = c("", "", "")
    )
  }

  near <- nomo_factors_synthesis(
    make_evidence(c(1, 2, 2)),
    parallel_n = 1L,
    status = tibble::tibble(status = c("available", "available", "skipped"))
  )
  expect_identical(near$agreement, "near")
  expect_match(near$text, "Carry both neighboring solutions")
  expect_match(near$text, "1 requested method was not evaluated")

  divergent <- nomo_factors_synthesis(
    make_evidence(c(1, 3, 4)),
    parallel_n = 1L,
    status = tibble::tibble(status = c("available", "skipped", "skipped"))
  )
  expect_identical(divergent$agreement, "divergent")
  expect_match(divergent$text, "materially divergent")
  expect_match(divergent$text, "2 requested methods were not evaluated")
})
