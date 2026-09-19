methods_run <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)

    scales <- list(
      Agency = c("ag1", "ag2", "ag3", "ag4"),
      Persistence = c("pe1", "pe2", "pe3", "pe4"),
      SocialDesirability = c("sd1", "sd2", "sd3")
    )

    h <- nomo_hypotheses(
      "Agency -> Persistence" = positive(min = .20),
      "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15))
    )

    cached <<- nomo_run(
      data = nomo_demo_network,
      scales = scales,
      mode = "research",
      decisions = list(
        factor_count = c(Agency = 1, Persistence = 1, SocialDesirability = 1),
        cfa_model = nomo_model(scales),
        measurement_model = "proceed"
      ),
      settings = list(
        factors = list(seed = 2026),
        invariance = list(
          group = "group",
          levels = c("configural", "metric", "scalar"),
          localize = TRUE
        ),
        network = list(hypotheses = h)
      )
    )
    cached
  }
})


# Registry integrity ----------------------------------------------------------

test_that("the registry is internally consistent", {
  reg <- nomo_methods_registry()

  expect_gt(nrow(reg), 0L)
  expect_false(any(duplicated(reg$id)))
  expect_true(all(reg$stage %in% nomo_methods_stages()))
  expect_true(all(reg$lineage %in% nomo_methods_lineages()))
  expect_true(all(reg$role %in% nomo_methods_roles()))

  for (col in c("id", "method", "estimand", "assumptions", "implemented_by", "engine")) {
    expect_true(all(nzchar(reg[[col]])), info = col)
  }

  # Every method must cite something. An entry with no reference is exactly the
  # unsupported claim this registry exists to prevent.
  expect_true(all(lengths(reg$citations) > 0L))
})


test_that("registry and bibliography reference each other exactly", {
  reg <- nomo_methods_registry()
  bib <- nomo_bibliography()
  keys <- unlist(reg$citations, use.names = FALSE)

  expect_false(any(duplicated(bib$key)))
  expect_equal(setdiff(keys, bib$key), character())
  expect_equal(setdiff(bib$key, keys), character())
})


test_that("every DOI is well formed, and only a book may lack one", {
  bib <- nomo_bibliography()

  present <- bib$doi[!is.na(bib$doi)]
  expect_true(all(grepl("^10\\.[0-9]{4,9}/[^[:space:]]+$", present)))

  # A missing DOI has to be a deliberate exception, not an oversight.
  expect_equal(bib$key[is.na(bib$doi)], "nunnally_bernstein_1994")
})


test_that("the registry stays ASCII so R CMD check has nothing to flag", {
  reg <- nomo_methods_registry()
  bib <- nomo_bibliography()

  text <- c(
    reg$id, reg$stage, reg$method, reg$lineage, reg$role,
    reg$estimand, reg$assumptions, reg$implemented_by, reg$engine,
    bib$key, bib$short, bib$citation, bib$doi[!is.na(bib$doi)]
  )

  # Accented author names are written with \u escapes, so the parsed strings
  # may contain non-ASCII; the requirement is only that they parse, and that
  # nothing is mojibake. Check the source file instead.
  source_file <- testthat::test_path("..", "..", "R", "nomo_methods.R")
  skip_if_not(file.exists(source_file))
  raw <- readLines(source_file, warn = FALSE)
  expect_false(any(grepl("[^\x01-\x7F]", raw)))

  expect_true(all(nzchar(text)))
})


test_that("every stage of the workflow is represented", {
  reg <- nomo_methods_registry()
  expect_setequal(unique(reg$stage), nomo_methods_stages())
})


test_that("context methods are never primary evidence", {
  reg <- nomo_methods_registry()
  context <- reg[reg$role == "context", ]

  expect_gt(nrow(context), 0L)
  # A method shown only for recognition must not also be contemporary primary
  # evidence; that combination would contradict the label.
  expect_true(all(context$lineage %in% c("historical", "emerging")))
})


# Accessor --------------------------------------------------------------------

test_that("nomo_methods() returns the whole registry with short citations", {
  m <- nomo_methods()

  expect_s3_class(m, "tbl_df")
  expect_true("references" %in% names(m))
  expect_false("citations" %in% names(m))
  expect_equal(nrow(m), nrow(nomo_methods_registry()))
  expect_true(all(nzchar(m$references)))
})


test_that("nomo_methods() filters by stage and lineage", {
  rel <- nomo_methods(stage = "reliability")
  expect_true(all(rel$stage == "reliability"))
  expect_true("omega" %in% rel$id)

  hist <- nomo_methods(lineage = "historical")
  expect_true(all(hist$lineage == "historical"))
  expect_true("kaiser_guttman" %in% hist$id)

  both <- nomo_methods(stage = "factors", lineage = "historical")
  expect_true(all(both$stage == "factors" & both$lineage == "historical"))
})


test_that("nomo_methods() refuses unknown filters by name", {
  expect_error(nomo_methods(stage = "nonsense"), "Unknown `stage`")
  expect_error(nomo_methods(stage = "nonsense"), "nonsense")
  expect_error(nomo_methods(lineage = "modern"), "Unknown `lineage`")
  expect_error(nomo_methods(references = "yes"), "must be TRUE or FALSE")
})


test_that("references = TRUE expands to one row per method-reference pair", {
  long <- nomo_methods(stage = "reliability", references = TRUE)

  expect_true(all(c("citation_key", "citation", "doi") %in% names(long)))
  expect_gt(nrow(long), nrow(nomo_methods(stage = "reliability")))

  omega <- long[long$id == "omega", ]
  expect_true("dunn_2014" %in% omega$citation_key)
  expect_true(any(grepl("Dunn", omega$citation)))
  expect_true(all(nzchar(omega$citation)))
})


test_that("an empty filter result still returns the expected columns", {
  empty <- nomo_methods(stage = "compare", lineage = "emerging", references = TRUE)

  expect_equal(nrow(empty), 0L)
  expect_true(all(c("id", "citation_key", "citation", "doi") %in% names(empty)))
})


# Derivation from a real run --------------------------------------------------

test_that("nomo_methods(run) reports the methods the run actually used", {
  run <- methods_run()
  used <- nomo_methods(run)

  expect_s3_class(used, "tbl_df")
  expect_gt(nrow(used), 0L)
  expect_lt(nrow(used), nrow(nomo_methods()))
  expect_true(all(used$id %in% nomo_methods_registry()$id))

  # Rows come back in registry order, so the methods section reads in workflow
  # sequence rather than in the order components happened to be visited.
  registry_order <- nomo_methods_registry()$id
  expect_equal(used$id, registry_order[registry_order %in% used$id])
})


test_that("methods the run did use are credited", {
  used <- nomo_methods(methods_run())$id

  expect_true(all(c(
    "missingness_audit", "item_rest_correlation", "near_zero_variance",
    "parallel_analysis", "map_original", "map_revised", "ekc",
    "minres_extraction", "loading_diagnostics",
    "ml_cfa", "incremental_fit", "rmsea_interval", "srmr", "local_strain",
    "improper_solutions",
    "omega", "alpha",
    "standardized_loadings_ave", "htmt", "htmt2", "latent_correlation_ci",
    "multigroup_cfa", "invariance_hierarchy", "invariance_delta_fit",
    "score_diagnostics",
    "nomological_network", "two_step_sem", "equivalence_testing",
    "staged_workflow", "decision_log"
  ) %in% used))
})


test_that("methods the run did not use are not credited", {
  used <- nomo_methods(methods_run())$id

  # Continuous indicators estimated by ML.
  expect_false("wlsmv_cfa" %in% used)
  expect_false("categorical_correlations" %in% used)
  expect_false("categorical_invariance" %in% used)

  # Criteria outside the default "core" set were never run.
  expect_false(any(c("nest", "hull", "comparison_data", "kaiser_guttman") %in% used))

  # Not requested.
  expect_false("fornell_larcker" %in% used)
  expect_false("reliability_bootstrap_ci" %in% used)
  expect_false("partial_invariance" %in% used)

  # No holdout design, no revision, no model comparison, no replication.
  expect_false("holdout_split" %in% used)
  expect_false("revision_lineage" %in% used)
  expect_false("replication_same_model" %in% used)
  expect_false(any(c("lrt_standard", "lrt_scaled", "information_criteria") %in% used))
})


test_that("a one-factor solution is credited with no rotation method", {
  run <- methods_run()
  used <- nomo_methods(run)$id

  # Each scale retained one factor, and a single factor is not rotated. `oblique`
  # is FALSE in that case because no factor correlation matrix was produced, not
  # because an orthogonal rotation was chosen -- crediting one would describe a
  # choice the researcher never made.
  expect_equal(unique(vapply(run$results$efa, function(e) e$n_factors, numeric(1))), 1)
  expect_false("oblique_rotation" %in% used)
  expect_false("orthogonal_rotation" %in% used)
})


test_that("a same-sample design is not credited as a holdout", {
  run <- methods_run()

  expect_identical(run$sample_design, "same_sample")
  expect_false("holdout_split" %in% nomo_methods(run)$id)
})


test_that("historical methods used in the run are labelled as context", {
  used <- nomo_methods(methods_run())

  context <- used[used$role == "context", ]
  expect_true(all(c("fit_cutoffs", "item_total_reference", "loading_reference") %in% context$id))
  expect_true(all(context$lineage %in% c("historical", "emerging")))
})


test_that("the registry holds no method the package never displays", {
  # A fixed change-in-CFI rule is discussed in the invariance article but no
  # function computes or displays it, so it must not be in the registry or be
  # credited to a run.
  expect_false("delta_cfi_rule" %in% nomo_methods_registry()$id)
})


# Regression tests for crediting rules ------------------------------------------

methods_compare_models <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    full <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
    fixed <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + 0*b5"
    items <- c(paste0("a", 1:5), paste0("b", 1:5))
    cached <<- list(
      ml = list(
        nomo_cfa(full, data = nomo_demo_continuous),
        nomo_cfa(fixed, data = nomo_demo_continuous)
      ),
      mlr = list(
        nomo_cfa(full, data = nomo_demo_continuous, estimator = "MLR"),
        nomo_cfa(fixed, data = nomo_demo_continuous, estimator = "MLR")
      ),
      wlsmv = list(
        nomo_cfa(full, data = nomo_demo_ordinal, ordered = items),
        nomo_cfa(fixed, data = nomo_demo_ordinal, ordered = items)
      )
    )
    cached
  }
})


test_that("each difference test is credited by lavaan's method, not by name", {
  models <- methods_compare_models()
  credit <- function(pair) {
    nomo_methods(nomo_compare(
      pair[[1]], pair[[2]],
      rationale = "Crediting regression test.",
      evidence = FALSE
    ))$id
  }

  ml <- credit(models$ml)
  expect_true("lrt_standard" %in% ml)
  expect_false(any(c("lrt_scaled", "lrt_scaled_shifted") %in% ml))

  mlr <- credit(models$mlr)
  expect_true("lrt_scaled" %in% mlr)
  expect_false(any(c("lrt_standard", "lrt_scaled_shifted") %in% mlr))

  # lavaan labels the WLSMV test "satorra.2000"; it is the scaled-and-shifted
  # test, not the Satorra-Bentler scaled test.
  wlsmv <- credit(models$wlsmv)
  expect_true("lrt_scaled_shifted" %in% wlsmv)
  expect_false(any(c("lrt_standard", "lrt_scaled") %in% wlsmv))
})


test_that("information criteria are credited only where they are defined", {
  models <- methods_compare_models()
  ml <- nomo_compare(models$ml[[1]], models$ml[[2]],
                     rationale = "IC regression test.", evidence = FALSE)
  wlsmv <- nomo_compare(models$wlsmv[[1]], models$wlsmv[[2]],
                        rationale = "IC regression test.", evidence = FALSE)

  expect_true(any(ml$comparisons$ic_available))
  expect_true("information_criteria" %in% nomo_methods(ml)$id)

  expect_false(any(wlsmv$comparisons$ic_available))
  expect_false("information_criteria" %in% nomo_methods(wlsmv)$id)
})


test_that("every lavaan spelling of FIML is credited", {
  model <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
  for (miss in c("ml", "fiml", "direct")) {
    fit <- nomo_cfa(model, data = nomo_demo_continuous, missing = miss)
    expect_true("fiml" %in% nomo_methods(fit)$id, info = miss)
  }
  listwise <- nomo_cfa(model, data = nomo_demo_continuous, missing = "listwise")
  expect_false("fiml" %in% nomo_methods(listwise)$id)

  expect_false(nomo_methods_is_fiml(NA_character_))
  expect_false(nomo_methods_is_fiml(c("ml", "fiml")))
})


test_that("categorical and rotated solutions credit their own methods", {
  fo <- nomo_factors(nomo_demo_ordinal, n_iter = 10, seed = 2026)
  expect_true("categorical_correlations" %in% nomo_methods(fo)$id)

  oblique <- nomo_efa(nomo_demo_continuous, factors = 2)
  used <- nomo_methods(oblique)$id
  expect_true("oblique_rotation" %in% used)
  expect_false("orthogonal_rotation" %in% used)

  orthogonal <- nomo_efa(nomo_demo_continuous, factors = 2, rotation = "varimax")
  used <- nomo_methods(orthogonal)$id
  expect_true("orthogonal_rotation" %in% used)
  expect_false("oblique_rotation" %in% used)

  ordinal_efa <- nomo_efa(nomo_demo_ordinal, factors = 2)
  expect_true("categorical_correlations" %in% nomo_methods(ordinal_efa)$id)
})


test_that("an ordered CFA credits WLSMV, not maximum likelihood", {
  used <- nomo_methods(methods_compare_models()$wlsmv[[1]])$id
  expect_true(all(c("wlsmv_cfa", "categorical_correlations") %in% used))
  expect_false("ml_cfa" %in% used)

  # Without a fitted model there is no structure to read.
  expect_true(is.na(nomo_methods_cfa_structure(list())))
})


test_that("bootstrap intervals and ordinal-scale reliability are credited", {
  models <- methods_compare_models()

  boot <- nomo_reliability(models$ml[[1]], ci = "bootstrap", ci_boot = 20, ci_seed = 2026)
  expect_true("reliability_bootstrap_ci" %in% nomo_methods(boot)$id)

  ordinal <- nomo_methods(nomo_reliability(models$wlsmv[[1]]))$id
  expect_true("omega_ordinal_scale" %in% ordinal)
  # Alpha is unavailable for the ordered-score estimand and so is not cited.
  expect_false("alpha" %in% ordinal)
})


test_that("invariance credits partial releases and categorical sequences", {
  release <- nomo_partial(
    level = "scalar",
    syntax = "ag3 ~ 1",
    rationale = "The ag3 intercept is known to differ by administration mode."
  )
  # A release specification fits nothing, so it credits only partial invariance.
  expect_equal(nomo_methods(release)$id, "partial_invariance")

  inv <- nomo_invariance(
    "Agency =~ ag1 + ag2 + ag3 + ag4",
    data = nomo_demo_network,
    group = "group",
    levels = c("configural", "metric", "scalar"),
    partial = release
  )
  used <- nomo_methods(inv)$id
  expect_true(all(c(
    "multigroup_cfa", "invariance_hierarchy", "invariance_delta_fit",
    "score_diagnostics", "partial_invariance"
  ) %in% used))
  expect_false("categorical_invariance" %in% used)

  # The ordered branch, on the fields a categorical fit records.
  ordered <- structure(
    list(
      ordered = c("x1", "x2", "x3"),
      fit_evidence = tibble::tibble(
        level = c("configural", "thresholds"),
        delta_cfi = c(NA_real_, -.002)
      ),
      localize = FALSE,
      partial = NULL
    ),
    class = c("nomo_invariance", "list")
  )
  used <- nomo_methods(ordered)$id
  expect_true(all(c("categorical_invariance", "categorical_correlations") %in% used))
  expect_false(any(c("invariance_hierarchy", "score_diagnostics", "partial_invariance") %in% used))
})


test_that("specifications, splits, and replication credit their methods", {
  split <- nomo_split(nomo_demo_network, validation_prop = .40, seed = 2026)
  expect_equal(nomo_methods(split)$id, "holdout_split")

  h <- nomo_hypotheses(
    "Agency -> Persistence" = positive(),
    "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15))
  )
  expect_setequal(
    nomo_methods(h)$id,
    c("nomological_network", "equivalence_testing", "prediction_provenance")
  )

  directional <- nomo_hypotheses("Agency -> Persistence" = positive())
  expect_false("equivalence_testing" %in% nomo_methods(directional)$id)

  model <- nomo_model(list(
    Agency = paste0("ag", 1:4),
    Persistence = paste0("pe", 1:4),
    SocialDesirability = paste0("sd", 1:3)
  ))
  net <- nomo_network(model, data = split, hypotheses = h, missing = "ml")
  used <- nomo_methods(net)$id
  expect_true(all(c("replication_same_model", "fiml", "equivalence_testing") %in% used))
})


test_that("a split, revised workflow credits holdout, lineage, and its comparison", {
  split <- nomo_split(nomo_demo_network, validation_prop = .40, seed = 2026)
  scales <- list(Agency = paste0("ag", 1:4))
  run <- nomo_run(
    data = split,
    scales = scales,
    mode = "research",
    decisions = list(
      factor_count = c(Agency = 1),
      cfa_model = nomo_model(scales)
    ),
    settings = list(factors = list(n_iter = 20, seed = 2026))
  )
  expect_identical(run$sample_design, "calibration_validation")
  expect_true("holdout_split" %in% nomo_methods(run)$id)
  expect_false("revision_lineage" %in% nomo_methods(run)$id)

  revised <- nomo_revise(
    run,
    cfa_model = "Agency =~ ag1 + ag2 + ag3 + ag4\nag1 ~~ ag2",
    rationale = "Items ag1 and ag2 share wording.",
    origin = "post_hoc"
  )
  used <- nomo_methods(revised)$id
  expect_true(all(c("revision_lineage", "holdout_split", "lrt_standard", "nesting_check") %in% used))
})


test_that("a requested Fornell-Larcker comparison is credited", {
  fit <- methods_compare_models()$ml[[1]]
  expect_false("fornell_larcker" %in% nomo_methods(nomo_validity(fit))$id)
  expect_true(
    "fornell_larcker" %in% nomo_methods(nomo_validity(fit, fornell_larcker = TRUE))$id
  )
})


# Component objects -----------------------------------------------------------

test_that("component objects report their own methods", {
  run <- methods_run()

  expect_true("omega" %in% nomo_methods(run$results$reliability)$id)
  expect_true("htmt2" %in% nomo_methods(run$results$validity)$id)
  expect_true("multigroup_cfa" %in% nomo_methods(run$results$invariance)$id)
  expect_true("parallel_analysis" %in% nomo_methods(run$results$factors$Agency)$id)

  # A component knows nothing about stages it did not run.
  expect_false("omega" %in% nomo_methods(run$results$validity)$id)
})


test_that("a credited method missing from the registry is an internal error", {
  # Guards against a future component crediting a method nobody registered,
  # which would otherwise drop silently out of the report's methods section.
  local_mocked_bindings(
    nomo_methods_used = function(x, ...) c("omega", "unregistered_method")
  )
  expect_error(
    nomo_methods(structure(list(), class = "nomo_reliability")),
    "Internal error: methods used but absent from the registry: unregistered_method"
  )
})


test_that("nomo_methods() refuses an object it cannot read", {
  expect_error(nomo_methods(data.frame(a = 1)), "does not know how to read")
  expect_error(nomo_methods(data.frame(a = 1)), "data.frame")
})


# Surfaces --------------------------------------------------------------------

test_that("summary(run) carries and prints the methods used", {
  run <- methods_run()
  s <- summary(run)

  expect_equal(s$methods, nomo_methods(run))

  printed <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(printed, "Methods used \\(\\d+; full entries and references: nomo_methods\\(run\\)\\)")
  expect_match(printed, "Common-factor parallel analysis", fixed = TRUE)
})


test_that("nomo_table(run, \"methods\") matches nomo_methods(run)", {
  run <- methods_run()
  expect_equal(nomo_table(run, "methods"), nomo_methods(run))
})


test_that("the report methods table describes only what the run used", {
  run <- methods_run()
  tbl <- nomo_report_methods(run)

  expect_gt(nrow(tbl), 0L)
  expect_true(all(c("stage", "method", "lineage", "role") %in% names(tbl)))
  expect_false(any(grepl("Hull method", tbl$method)))
  expect_true(any(grepl("parallel analysis", tbl$method, ignore.case = TRUE)))
})


test_that("the report reference list names each work once, with a DOI link", {
  run <- methods_run()
  refs <- nomo_report_method_references(run)

  expect_gt(nrow(refs), 0L)
  expect_equal(names(refs), c("citation", "doi"))
  expect_false(any(duplicated(refs$citation)))
  expect_true(any(grepl("^https://doi\\.org/10\\.", refs$doi)))

  # Sorted so the list reads as a reference list rather than in run order.
  expect_equal(refs$citation, sort(refs$citation))
})


test_that("run components that are not result objects contribute no methods", {
  expect_identical(nomologR:::nomo_methods_used_component("not a component"), character())
  expect_identical(nomologR:::nomo_methods_used_component(NULL), character())
})
