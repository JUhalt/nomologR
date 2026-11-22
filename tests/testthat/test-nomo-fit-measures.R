# Fit measures are computed once, and only those nomologR reads (#183) ---------
#
# lavaan's full set of fit measures adds the GFI family and other indices
# nomologR never shows. With full-information maximum likelihood they took
# most of the time of nomo_reliability(), nomo_validity(), and
# nomo_invariance(), which also computed them again for a fit whose measures
# were already stored. nomologR 1.0.0 asked lavaan for the full set every time;
# a stand-in for fitMeasures() that answers every request for the measures
# nomologR reads with the full set reproduces it, so these tests hold each
# result to the 1.0.0 one.

fit_measure_stand_in <- function(env = parent.frame(), full = TRUE) {
  original <- lavaan::fitMeasures
  calls <- new.env()
  calls$n <- 0L
  testthat::local_mocked_bindings(
    fitMeasures = function(object, measures = "all", ...) {
      calls$n <- calls$n + 1L
      read <- all(nomologR:::nomo_fit_measure_names %in% measures)
      if (full && read) original(object) else original(object, measures, ...)
    },
    .package = "lavaan",
    .env = env
  )
  calls
}

# The parts of a result that are not lavaan fits, which carry their own
# timings. A guided run's components record calls that hold the CFA, fit and
# all, so calls are left out too.
strip_fits <- function(x) {
  if (isS4(x) || is.language(x)) return(NULL)
  if (is.list(x) && !is.data.frame(x)) x[] <- lapply(x, strip_fits)
  x
}

fiml_demo <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      dat <- nomo_demo_continuous
      set.seed(183)
      for (item in names(dat)) dat[[item]][sample(nrow(dat), 25)] <- NA
      cache <<- dat
    }
    cache
  }
})
demo_scales <- list(A = paste0("a", 1:5), B = paste0("b", 1:5))


test_that("the measures nomologR reads equal lavaan's full set (#183)", {
  skip_on_cran()
  model <- as.character(nomo_model(demo_scales))
  network_model <- "A =~ ag1 + ag2 + ag3 + ag4"
  fits <- list(
    ml = lavaan::cfa(model, nomo_demo_continuous),
    mlr_fiml = lavaan::cfa(model, fiml_demo(), estimator = "MLR", missing = "ml"),
    wlsmv = lavaan::cfa(model, nomo_demo_ordinal, ordered = unlist(demo_scales)),
    groups = lavaan::cfa(network_model, nomo_demo_network, group = "group", estimator = "MLM"),
    # ULS on continuous data has a second test but no scaled one. Asked for by
    # name, lavaan adds scaled CFI, TLI, and RMSEA that its full set leaves out.
    uls = lavaan::cfa(model, nomo_demo_continuous, estimator = "ULS"),
    uls_groups = lavaan::cfa(network_model, nomo_demo_network, group = "group", estimator = "ULS")
  )
  for (fit in fits) {
    full <- lavaan::fitMeasures(fit)
    read <- nomologR:::nomo_fit_measures_read(fit)
    # A version the model lacks is left out of both.
    expect_identical(names(read), intersect(nomologR:::nomo_fit_measure_names, names(full)))
    expect_identical(unclass(unname(read)), unclass(unname(full[names(read)])))
    expect_identical(class(read), class(full))
    expect_identical(nomologR:::nomo_cfa_fit_evidence(read, nomo_defaults()),
                     nomologR:::nomo_cfa_fit_evidence(full, nomo_defaults()))
  }
  expect_false(any(grepl("[.]scaled$", names(nomologR:::nomo_fit_measures_read(fits$uls)))))
  rel <- nomo_reliability(fits$uls)
  expect_identical(rel$fit_context,
                   nomologR:::nomo_cfa_fit_evidence(lavaan::fitMeasures(fits$uls), nomo_defaults()))
  expect_identical(nomo_validity(fits$uls)$fit_context, rel$fit_context)
  expect_false(any(grepl("scaled", rel$fit_context$variant)))
})


test_that("the measures read keep a version only where lavaan's full set has it (#183)", {
  skip_on_cran()
  ml <- lavaan::cfa("A =~ ag1 + ag2 + ag3 + ag4", nomo_demo_network)
  fiml <- lavaan::cfa("A =~ a1 + a2 + a3 + a4 + a5", fiml_demo(), missing = "ml")
  ordinal <- lavaan::cfa("A =~ a1 + a2 + a3 + a4 + a5", nomo_demo_ordinal,
                         ordered = paste0("a", 1:5))
  # A stand-in for lavaan that returns every version, wanted or not.
  vector <- function(...) structure(c(...), class = c("lavaan.vector", "numeric"))
  answer <- vector(chisq = 1, cfi = 0.9, cfi.scaled = 0.91, cfi.robust = 0.92)
  testthat::local_mocked_bindings(
    fitMeasures = function(object, measures, ...) answer,
    .package = "lavaan"
  )
  # Without a scaled chi-square, categorical data, or FIML, neither version.
  expect_identical(nomologR:::nomo_fit_measures_read(ml), vector(chisq = 1, cfi = 0.9))
  # With FIML or categorical data, the robust version but not the scaled one.
  expect_identical(nomologR:::nomo_fit_measures_read(fiml),
                   vector(chisq = 1, cfi = 0.9, cfi.robust = 0.92))
  expect_identical(nomologR:::nomo_fit_measures_read(ordinal),
                   vector(chisq = 1, cfi = 0.9, cfi.robust = 0.92))
  # With a scaled chi-square, both.
  answer <- vector(chisq = 1, chisq.scaled = 1.2, cfi = 0.9, cfi.scaled = 0.91, cfi.robust = 0.92)
  expect_identical(nomologR:::nomo_fit_measures_read(ml), answer)
})


test_that("reliability and validity reuse the CFA's fit measures, with 1.0.0's results (#183)", {
  skip_on_cran()
  cfa <- nomo_cfa(nomo_model(demo_scales), fiml_demo(), estimator = "MLR", missing = "ml")

  # Given the CFA, neither computes fit measures again.
  calls <- fit_measure_stand_in(full = FALSE)
  rel <- nomo_reliability(cfa)
  val <- nomo_validity(cfa)
  expect_identical(calls$n, 0L)
  # Given the lavaan fit, each asks for the measures it reads, once.
  fit <- cfa$fit
  rel_fit <- nomo_reliability(fit)
  val_fit <- nomo_validity(fit)
  expect_identical(calls$n, 2L)

  # 1.0.0 computed the full set from the fit in either case.
  old <- local({
    fit_measure_stand_in()
    cfa$fit_measures_all <- NULL
    list(rel = nomo_reliability(cfa), val = nomo_validity(cfa),
         rel_fit = nomo_reliability(fit), val_fit = nomo_validity(fit))
  })
  expect_identical(strip_fits(rel), strip_fits(old$rel))
  expect_identical(strip_fits(val), strip_fits(old$val))
  expect_identical(strip_fits(rel_fit), strip_fits(old$rel_fit))
  expect_identical(strip_fits(val_fit), strip_fits(old$val_fit))
  expect_identical(rel$fit_context,
                   nomologR:::nomo_cfa_fit_evidence(lavaan::fitMeasures(fit), nomo_defaults()))
  expect_identical(val$fit_context, rel$fit_context)
  expect_true(nrow(rel$fit_context) > 0L)
})


test_that("invariance computes each level's fit measures once, as 1.0.0 stored them (#183)", {
  skip_on_cran()
  model <- "Agency =~ ag1 + ag2 + ag3 + ag4"
  calls <- fit_measure_stand_in(full = FALSE)
  inv <- nomo_invariance(model, nomo_demo_network, group = "group")
  # 1.0.0 computed them twice per level.
  expect_identical(calls$n, length(inv$fits))
  expect_identical(names(inv$fit_measures), inv$fit_evidence$level)
  for (level in names(inv$fits)) {
    full <- lavaan::fitMeasures(inv$fits[[level]])
    expect_identical(unclass(inv$fit_measures[[level]]), unclass(full))
    row <- inv$fit_evidence[inv$fit_evidence$level == level, ]
    expect_identical(row$srmr, unname(full[["srmr"]]))
    expect_identical(row$cfi, unname(full[["cfi"]]))
  }
})


test_that("constraint labels read each level's parameter table once, with the same labels (#183)", {
  skip_on_cran()
  inv <- nomo_invariance("Agency =~ ag1 + ag2 + ag3 + ag4", nomo_demo_network,
                         group = "group")
  strain <- inv$local_strain
  expect_true(nrow(strain) > length(unique(strain$level)))
  # One label at a time, as 1.0.0 wrote them.
  one_by_one <- vapply(seq_len(nrow(strain)), function(i) {
    nomologR:::nomo_invariance_pretty_constraint(
      strain$constraint[[i]], inv$fits[[as.character(strain$level[[i]])]]
    )
  }, character(1))

  original <- lavaan::parTable
  read <- 0L
  testthat::local_mocked_bindings(
    parTable = function(...) {
      read <<- read + 1L
      original(...)
    },
    .package = "lavaan"
  )
  shown <- nomo_table(inv, "local_strain")
  expect_identical(read, length(unique(strain$level)))
  expect_identical(shown$constraint_display, one_by_one)
})


test_that("the network, its APA fit table, ESEM, and method variance give 1.0.0's results (#183)", {
  skip_on_cran()
  model <- "Agency =~ ag1 + ag2 + ag3 + ag4\nPersistence =~ pe1 + pe2 + pe3 + pe4"
  h <- nomo_hypotheses("Agency -> Persistence" = positive(),
                       "Agency -> Performance" = positive())
  # ESEM's rotation draws random starts, so each pair of fits shares a seed.
  fit_all <- function(estimator = NULL) {
    withr::with_seed(183, list(
      network = nomo_network(model, nomo_demo_network, h, estimator = estimator),
      esem = nomo_esem(as.character(nomo_model(demo_scales)), nomo_demo_continuous,
                       estimator = estimator),
      method = nomo_method_variance(
        "Agency =~ ag1 + ag2 + ag3 + ag4", nomo_demo_network, marker = c("sd1", "sd2", "sd3"),
        estimator = estimator
      )
    ))
  }
  results <- list()
  for (estimator in c("MLR", "ULS")) {
    new <- fit_all(estimator)
    new_apa <- nomo_apa_table(new$network, "fit")
    old <- local({
      fit_measure_stand_in()
      out <- fit_all(estimator)
      out$apa <- nomo_apa_table(out$network, "fit")
      out
    })
    expect_identical(strip_fits(new$network), strip_fits(old$network))
    expect_identical(new_apa, old$apa)
    expect_identical(strip_fits(new$esem), strip_fits(old$esem))
    expect_identical(capture.output(print(new$esem)), capture.output(print(old$esem)))
    expect_identical(strip_fits(new$method), strip_fits(old$method))
    results[[estimator]] <- new
  }
  # MLR is maximum likelihood, so lavaan's full set has AIC and BIC.
  expect_true(all(is.finite(results$MLR$esem$models$aic)))
  expect_identical(results$MLR$network$fit_evidence$index_version, "robust")
  # ULS has no scaled test, so the values are the standard ones.
  expect_identical(results$ULS$network$fit_evidence$index_version, "standard")
  expect_false(any(grepl("scaled", unlist(results$ULS$esem$fit_variants))))
})


test_that("a guided run gives 1.0.0's results and logs (#183)", {
  skip_on_cran()
  scales <- list(Agency = paste0("ag", 1:4), Persistence = paste0("pe", 1:4))
  h <- nomo_hypotheses("Agency -> Persistence" = positive())
  guided <- function() {
    suppressMessages(nomo_run(
      nomo_demo_network, scales,
      decisions = list(
        factor_count = c(Agency = 1, Persistence = 1),
        cfa_model = nomo_model(scales),
        measurement_model = "proceed"
      ),
      settings = list(
        factors = list(n_iter = 20, seed = 2026),
        cfa = list(estimator = "MLR"),
        invariance = list(group = "group", levels = c("configural", "metric")),
        network = list(hypotheses = h)
      )
    ))
  }
  new <- guided()
  old <- local({
    fit_measure_stand_in()
    guided()
  })
  expect_identical(new$status, "complete")
  expect_identical(new$decision_log, old$decision_log)
  expect_identical(nomo_table(new, "component_log"), nomo_table(old, "component_log"))
  expect_identical(strip_fits(new$results), strip_fits(old$results))
  expect_identical(new$results$reliability$fit_context, new$results$cfa$fit_evidence)
})
