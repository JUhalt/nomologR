make_m2b_two_factor <- function(n = 450L, seed = 20260903L) {
  set.seed(seed)
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  f1 <- z1
  f2 <- 0.35 * z1 + sqrt(1 - 0.35^2) * z2

  data.frame(
    a1 = 0.85 * f1 + stats::rnorm(n, sd = 0.50),
    a2 = 0.82 * f1 + stats::rnorm(n, sd = 0.52),
    a3 = 0.78 * f1 + stats::rnorm(n, sd = 0.56),
    a4 = 0.80 * f1 + stats::rnorm(n, sd = 0.54),
    b1 = 0.85 * f2 + stats::rnorm(n, sd = 0.50),
    b2 = 0.82 * f2 + stats::rnorm(n, sd = 0.52),
    b3 = 0.78 * f2 + stats::rnorm(n, sd = 0.56),
    b4 = 0.80 * f2 + stats::rnorm(n, sd = 0.54)
  )
}


make_m2b_ordinal <- function(n = 450L, seed = 20260904L) {
  set.seed(seed)
  f <- stats::rnorm(n)
  latent <- replicate(6, 0.82 * f + stats::rnorm(n, sd = 0.58))

  out <- as.data.frame(
    lapply(seq_len(ncol(latent)), function(j) {
      x <- latent[, j]
      ordered(
        cut(
          x,
          breaks = stats::quantile(
            x,
            probs = seq(0, 1, length.out = 6),
            na.rm = TRUE
          ),
          include.lowest = TRUE,
          labels = FALSE
        ),
        levels = 1:5
      )
    })
  )
  names(out) <- paste0("i", seq_len(ncol(out)))
  out
}


test_that("core criterion set exposes multi-criterion retention evidence", {
  dat <- make_m2b_two_factor()

  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 401
  )

  expect_identical(out$criterion_set, "core")
  expect_identical(
    out$evidence$criterion,
    c("parallel", "map_original", "map_revised", "ekc")
  )
  expect_true(all(out$criterion_status$status == "available"))
  expect_equal(out$parallel$n_factors, 2L)
  expect_true(2L %in% out$plausible_factors)
  expect_true(all(c("map_original", "map_revised") %in% names(out$map$table)))
})


test_that("minimal criterion set stays deliberately compact", {
  dat <- make_m2b_two_factor(n = 320L, seed = 2)

  out <- nomo_factors(
    dat,
    criterion_set = "minimal",
    n_iter = 10,
    seed = 402
  )

  expect_identical(
    out$evidence$criterion,
    c("parallel", "map_original")
  )
  expect_identical(
    out$criterion_status$criterion,
    c("parallel", "map_original")
  )
})


test_that("parallel-analysis rule sensitivity is always retained", {
  dat <- make_m2b_two_factor(n = 350L, seed = 3)

  for (rule in c("percentile", "mean", "crawford")) {
    out <- nomo_factors(
      dat,
      criterion_set = "minimal",
      parallel_rule = rule,
      n_iter = 10,
      seed = 403
    )

    expect_identical(out$parallel$rule, rule)
    expect_identical(
      out$parallel$sensitivity$rule,
      c("percentile", "mean", "crawford")
    )
    expect_equal(sum(out$parallel$sensitivity$selected), 1L)
    expect_true(out$parallel$sensitivity$selected[out$parallel$sensitivity$rule == rule])
    expect_equal(
      out$parallel$n_factors,
      out$parallel$sensitivity$n_factors[out$parallel$sensitivity$rule == rule]
    )
  }
})


test_that("revised and original MAP are both explicit", {
  dat <- make_m2b_two_factor(n = 360L, seed = 4)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 404
  )

  expect_true(all(c(
    "map_original",
    "map_revised",
    "minimum_original",
    "minimum_revised"
  ) %in% names(out$map$table)))
  expect_true(out$map$n_factors_original %in% out$map$table$n_factors)
  expect_true(out$map$n_factors_revised %in% out$map$table$n_factors)
  expect_true(any(out$decision_log$metric == "map_revised"))
})


test_that("EKC is available in the core set for ordinary Pearson data", {
  dat <- make_m2b_two_factor(n = 380L, seed = 5)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 405
  )

  ekc_status <- out$criterion_status[
    out$criterion_status$criterion == "ekc",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(ekc_status), 1L)
  expect_identical(ekc_status$status, "available")
  expect_true(any(out$evidence$criterion == "ekc"))
})


test_that("ordinal analyses skip incompatible extended criteria explicitly", {
  dat <- make_m2b_ordinal()

  out <- nomo_factors(
    dat,
    criterion_set = "extended",
    n_iter = 10,
    seed = 406
  )

  expect_identical(out$correlation_method, "polychoric")
  expect_identical(out$correlation, "polychoric")
  expect_identical(out$modeling_types, out$item_types)
  skipped <- out$criterion_status[
    out$criterion_status$criterion %in% c("nest", "hull"),
    ,
    drop = FALSE
  ]
  expect_equal(nrow(skipped), 2L)
  expect_true(all(skipped$status == "skipped"))
  expect_true(all(grepl("continuous-reference", skipped$reason, fixed = TRUE)))
  expect_false(any(out$evidence$criterion %in% c("nest", "hull")))

  ekc_status <- out$criterion_status[
    out$criterion_status$criterion == "ekc",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(ekc_status), 1L)
  expect_identical(ekc_status$status, "available")
  expect_match(ekc_status$qualification, "approximate")

  ekc_log <- out$decision_log[out$decision_log$metric == "ekc", , drop = FALSE]
  expect_equal(nrow(ekc_log), 1L)
  expect_identical(ekc_log$severity, "review")
  expect_match(ekc_log$observation, "approximate")
})


test_that("NEST and Hull wrappers run on supported continuous data", {
  dat <- make_m2b_two_factor(n = 300L, seed = 6)
  r <- stats::cor(dat)

  nest <- nomo_factors_nest(
    corr = r,
    n_obs = nrow(dat),
    n_iter = 10,
    seed = 407
  )
  expect_true(nest$available)
  expect_true(is.finite(nest$n_factors))

  hull <- nomo_factors_hull(
    corr = r,
    n_obs = nrow(dat),
    n_iter = 10,
    quantile = 0.95,
    seed = 408
  )
  expect_true(hull$available)
  expect_true(is.finite(hull$n_factors))
})


test_that("comparison-data wrapper runs reproducibly on supported raw data", {
  dat <- make_m2b_two_factor(n = 180L, seed = 7)

  a <- nomo_factors_cd(
    x = dat,
    max_factors = 3L,
    n_population = 1000L,
    n_samples = 10L,
    alpha = 0.30,
    seed = 409
  )
  b <- nomo_factors_cd(
    x = dat,
    max_factors = 3L,
    n_population = 1000L,
    n_samples = 10L,
    alpha = 0.30,
    seed = 409
  )

  expect_true(a$available)
  expect_true(is.finite(a$n_factors))
  expect_equal(a$n_factors, b$n_factors)
})


test_that("legacy Kaiser evidence never votes in the synthesis", {
  evidence <- tibble::tibble(
    criterion = c("parallel", "map_original", "kaiser"),
    method = c(
      "Parallel analysis",
      "MAP (original TR2)",
      "Kaiser-Guttman (> 1)"
    ),
    n_factors = c(2L, 2L, 5L),
    role = c("primary", "complementary", "legacy"),
    reference = c("", "", "")
  )

  syn <- nomo_factors_synthesis(evidence, parallel_n = 2L)

  expect_equal(syn$n_available, 2L)
  expect_equal(syn$min_factors, 2L)
  expect_equal(syn$max_factors, 2L)
  expect_equal(syn$plausible_factors, 2L)
})


test_that("M2B plot suite exposes sensitivity and concordance", {
  dat <- make_m2b_two_factor(n = 320L, seed = 8)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 410
  )

  for (type in c(
    "retention",
    "parallel_rules",
    "scree",
    "map",
    "evidence",
    "concordance",
    "kmo"
  )) {
    expect_s3_class(plot(out, type = type), "ggplot")
  }
})


test_that("summary exposes criteria status, PA sensitivity, and concordance", {
  dat <- make_m2b_two_factor(n = 320L, seed = 9)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 411
  )
  s <- summary(out)

  expect_s3_class(s, "summary_nomo_factors")
  expect_identical(s$criterion_set, "core")
  expect_equal(nrow(s$parallel_sensitivity), 3L)
  expect_true(is.data.frame(s$criterion_status))
  expect_true(is.data.frame(s$concordance))
  expect_true(is.data.frame(s$family_evidence))
  expect_identical(s$correlation, out$correlation_method)
  expect_identical(s$modeling_types, out$item_types)
})


test_that("M2B presentation distinguishes convergence from voting", {
  dat <- make_m2b_two_factor(n = 320L, seed = 10)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 412
  )

  expect_match(out$recommendation, "Related methods within a family")

  p_rules <- plot(out, type = "parallel_rules")
  expect_true(any(grepl("selected", as.character(p_rules$data$rule_display), fixed = TRUE)))

  p_map <- plot(out, type = "map")
  expect_true(isTRUE(p_map$facet$params$free$y))

  p_evidence <- plot(out, type = "evidence")
  expect_identical(p_evidence$labels$x, "Suggested factor count")
  expect_false(any(grepl("(primary)", as.character(p_evidence$data$method_display), fixed = TRUE)))
  expect_match(p_evidence$labels$caption, "not independent votes")

  p_concordance <- plot(out, type = "concordance")
  expect_identical(p_concordance$labels$y, "Number of criterion families")
  expect_match(p_concordance$labels$caption, "not independent votes")
})



test_that("criterion-family synthesis groups MAP variants before concordance", {
  evidence <- tibble::tibble(
    criterion = c("parallel", "map_original", "map_revised", "ekc", "nest", "hull"),
    method = c(
      "Parallel analysis",
      "MAP (original TR2)",
      "MAP (revised TR4)",
      "Empirical Kaiser criterion",
      "NEST",
      "Hull (CAF)"
    ),
    n_factors = c(2L, 1L, 1L, 2L, 2L, 2L),
    role = c("primary", "complementary", "complementary", "complementary", "extended", "extended"),
    reference = rep("", 6L)
  )

  syn <- nomo_factors_synthesis(evidence, parallel_n = 2L)

  expect_equal(syn$n_methods, 6L)
  expect_equal(syn$n_families, 5L)
  expect_equal(syn$support_for_primary, 4L)
  expect_equal(
    syn$family_concordance$n_families[syn$family_concordance$n_factors == 1L],
    1L
  )
  expect_equal(
    syn$family_concordance$n_families[syn$family_concordance$n_factors == 2L],
    4L
  )
  expect_match(syn$text, "4 of 5 available criterion families")
  expect_match(syn$text, "MAP points to 1")
})


test_that("internally split criterion families are not forced into one concordance bar", {
  evidence <- tibble::tibble(
    criterion = c("parallel", "map_original", "map_revised", "ekc"),
    method = c(
      "Parallel analysis",
      "MAP (original TR2)",
      "MAP (revised TR4)",
      "Empirical Kaiser criterion"
    ),
    n_factors = c(2L, 1L, 2L, 2L),
    role = c("primary", "complementary", "complementary", "complementary"),
    reference = rep("", 4L)
  )

  syn <- nomo_factors_synthesis(evidence, parallel_n = 2L)
  map_family <- syn$family_evidence[syn$family_evidence$family == "map", , drop = FALSE]

  expect_false(map_family$internally_consistent)
  expect_true(is.na(map_family$n_factors))
  expect_match(map_family$candidates, "1, 2", fixed = TRUE)
  expect_equal(sum(syn$family_concordance$n_families), 2L)
  expect_match(syn$text, "MAP is internally split across 1 and 2")
})


test_that("summary surfaces qualified criteria for non-Pearson workflows", {
  dat <- make_m2b_ordinal(n = 320L, seed = 18)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 413
  )

  printed <- capture.output(print(summary(out)))
  expect_true(any(grepl("Criteria available with qualification", printed, fixed = TRUE)))
  expect_true(any(grepl("approximate", printed, fixed = TRUE)))
})
