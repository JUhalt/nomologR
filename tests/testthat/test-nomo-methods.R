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
        # Ten null data sets: the methods credited do not depend on how many,
        # and this fixture is built under CRAN's check-time limit.
        factors = list(seed = 2026, n_iter = 10),
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

  # A missing DOI has to be a deliberate exception, not an oversight. Books and
  # book chapters predate DOIs or were never assigned one, so they are listed
  # here individually rather than allowed as a category.
  expect_equal(
    sort(bib$key[is.na(bib$doi)]),
    c("gorsuch_1983", "hancock_mueller_2001", "hayduk_1987",
      "nunnally_bernstein_1994")
  )
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


test_that("?nomo_methods lists every stage `stage` accepts (#145)", {
  arguments <- nomo_test_rd_text("nomo_methods", "\\arguments")
  stage <- regmatches(
    arguments,
    regexpr("\\\\item\\{stage\\}.*?\\\\item\\{lineage\\}", arguments, perl = TRUE)
  )
  documented <- regmatches(stage, gregexpr('\\\\code\\{"[a-z_]+"\\}', stage))[[1L]]
  documented <- gsub('^\\\\code\\{"|"\\}$', "", documented)

  # "scores" was accepted but missing from the help page.
  expect_true("scores" %in% documented)
  expect_setequal(documented, nomo_methods_stages())
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
  # The shared form for a wrong choice: the argument, the choices, and what was
  # given (#144, guide point 26).
  expect_error(
    nomo_methods(stage = "nonsense"),
    paste0(
      "`stage` must be one or more of \"screen\", \"factors\", \"efa\", \"cfa\", ",
      "\"compare\", \"reliability\", \"validity\", \"invariance\", \"scores\", ",
      "\"network\", or \"workflow\", not \"nonsense\"."
    ),
    fixed = TRUE
  )
  expect_error(
    nomo_methods(stage = c("cfa", "nonsense", "more")),
    "not \"nonsense\", \"more\".", fixed = TRUE
  )
  expect_error(
    nomo_methods(lineage = "modern"),
    "`lineage` must be one or more of \"historical\", \"contemporary\", or \"emerging\", not \"modern\".",
    fixed = TRUE
  )
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
  skip_on_cran()
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


test_that("historical methods used in the run are labeled as context", {
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  used <- nomo_methods(methods_compare_models()$wlsmv[[1]])$id
  expect_true(all(c("wlsmv_cfa", "categorical_correlations") %in% used))
  expect_false("ml_cfa" %in% used)

  # Without a fitted model there is no structure to read.
  expect_true(is.na(nomo_methods_cfa_structure(list())))
})


test_that("bootstrap intervals and ordinal-scale reliability are credited", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  expect_match(printed, "Methods used: \\d+")
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
  expect_true(all(c("stage", "method", "lineage", "introduced", "role") %in% names(tbl)))
  expect_false(any(grepl("Hull method", tbl$method)))
  expect_true(any(grepl("parallel analysis", tbl$method, ignore.case = TRUE)))
  # The report carries the historical record: when each method was introduced.
  expect_identical(tbl$introduced[grepl("parallel analysis", tbl$method, ignore.case = TRUE)][[1L]],
                   1965L)
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

test_that("the history dates methods only from their originating references", {
  registry <- nomologR:::nomo_methods_registry()
  history <- nomologR:::nomo_methods_history()
  bib <- nomologR:::nomo_bibliography()

  expect_true(all(history$id %in% registry$id))
  expect_false(anyDuplicated(history$id) > 0L)

  dated <- history[!is.na(history$origin), , drop = FALSE]
  expect_true(all(dated$origin %in% bib$key))
  # The origin is a reference the entry already cites, not an outside claim.
  for (i in seq_len(nrow(dated))) {
    cites <- registry$citations[[match(dated$id[[i]], registry$id)]]
    expect_true(dated$origin[[i]] %in% cites, label = dated$id[[i]])
  }

  # `introduced` is the origin reference's year, and NA without an origin.
  origin <- history$origin[match(registry$id, history$id)]
  year <- as.integer(sub(".*\\((\\d{4})\\).*", "\\1", bib$short[match(origin, bib$key)]))
  expect_identical(registry$introduced, year)
  expect_identical(is.na(registry$introduced), is.na(origin))

  expect_identical(registry$introduced[registry$id == "parallel_analysis"], 1965L)
  expect_identical(registry$introduced[registry$id == "alpha"], 1951L)
  expect_identical(registry$introduced[registry$id == "kmo"], 1970L)
})


test_that("contemporary practice is named only for historical methods, from the registry", {
  registry <- nomologR:::nomo_methods_registry()
  named <- registry[!is.na(registry$contemporary_practice), , drop = FALSE]

  expect_true(all(named$lineage == "historical"))
  ids <- unlist(strsplit(named$contemporary_practice, "; ", fixed = TRUE))
  expect_true(all(ids %in% registry$id))
  expect_false(any(registry$lineage[match(ids, registry$id)] == "historical"))

  expect_identical(registry$contemporary_practice[registry$id == "alpha"], "omega")
  expect_match(registry$contemporary_practice[registry$id == "kaiser_guttman"],
               "parallel_analysis", fixed = TRUE)
  # Both columns reach the user.
  expect_true(all(c("introduced", "contemporary_practice") %in% names(nomo_methods())))
  expect_true(all(c("introduced", "contemporary_practice") %in%
                    names(nomo_methods(references = TRUE))))
})


# Crediting what ran: estimator, extraction, rotation, equivalence (#145) -------

test_that("a CFA is credited with the estimator that ran, not with the indicators' default (#145)", {
  # ML and its robust forms are maximum likelihood; WLSMV is credited only for
  # ordered indicators; anything else is credited as another estimator.
  ids <- nomologR:::nomo_methods_estimator_ids
  expect_identical(ids("ML", character()), "ml_cfa")
  expect_identical(ids("mlr", character()), "ml_cfa")
  expect_identical(ids("ULS", character()), "cfa_other_estimator")
  expect_identical(ids("GLS", character()), "cfa_other_estimator")
  expect_identical(ids("WLSMV", character()), "cfa_other_estimator")
  expect_identical(ids("WLSMV", "x1"), c("wlsmv_cfa", "categorical_correlations"))
  expect_identical(ids("ULSMV", "x1"), c("cfa_other_estimator", "categorical_correlations"))
  # A missing estimator is lavaan's default for the indicators.
  expect_identical(ids(NA_character_, character()), "ml_cfa")
  expect_identical(ids(NA_character_, "x1"), c("wlsmv_cfa", "categorical_correlations"))
  expect_identical(ids(NULL, character()), "ml_cfa")
  # So is "default", lavaan's own name for it: nomo_cfa() records the request
  # as "DEFAULT", nomo_esem() and nomo_method_variance() as it was typed.
  for (default in c("default", "DEFAULT", "Default")) {
    expect_identical(ids(default, character()), "ml_cfa", info = default)
    expect_identical(ids(default, "x1"), c("wlsmv_cfa", "categorical_correlations"),
                     info = default)
  }

  esem <- structure(list(estimator = "ULS", ordered = character(), missing = NA_character_),
                    class = c("nomo_esem", "list"))
  expect_identical(nomo_methods_used(esem), c("esem", "cfa_other_estimator"))
  esem$estimator <- "default"
  expect_identical(nomo_methods_used(esem), c("esem", "ml_cfa"))
  esem$ordered <- "x1"
  expect_identical(nomo_methods_used(esem),
                   c("esem", "wlsmv_cfa", "categorical_correlations"))
  mv <- structure(list(estimator = "GLS", missing = NA_character_),
                  class = c("nomo_method_variance", "list"))
  expect_identical(nomo_methods_used(mv), c("cfa_marker_technique", "cfa_other_estimator"))
  mv$estimator <- "default"
  expect_identical(nomo_methods_used(mv), c("cfa_marker_technique", "ml_cfa"))
  registry <- nomo_methods_registry()
  expect_match(registry$method[registry$id == "cfa_other_estimator"], "another estimator",
               fixed = TRUE)

  skip_on_cran()
  model <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
  for (estimator in c("ULS", "GLS")) {
    used <- nomo_methods(nomo_cfa(model, data = nomo_demo_continuous, estimator = estimator))
    expect_true("cfa_other_estimator" %in% used$id, info = estimator)
    expect_false("ml_cfa" %in% used$id, info = estimator)
  }
  items <- c(paste0("a", 1:5), paste0("b", 1:5))
  ulsmv <- nomo_methods(nomo_cfa(model, data = nomo_demo_ordinal, ordered = items,
                                 estimator = "ULSMV"))$id
  expect_true(all(c("cfa_other_estimator", "categorical_correlations") %in% ulsmv))
  expect_false("wlsmv_cfa" %in% ulsmv)

  # estimator = "default" runs lavaan's default, so it is credited as the same
  # call without `estimator` is: the recorded label differs, the engine does not.
  asked <- nomo_cfa(model, data = nomo_demo_continuous, estimator = "default")
  left <- nomo_cfa(model, data = nomo_demo_continuous)
  expect_identical(asked$estimator, "DEFAULT")
  expect_identical(asked$estimator_engine, left$estimator_engine)
  expect_identical(nomo_methods_used(asked), nomo_methods_used(left))
  expect_true("ml_cfa" %in% nomo_methods_used(asked))
  expect_false("cfa_other_estimator" %in% nomo_methods(asked)$id)

  asked <- nomo_cfa(model, data = nomo_demo_ordinal, ordered = items, estimator = "default")
  left <- nomo_cfa(model, data = nomo_demo_ordinal, ordered = items)
  expect_identical(asked$estimator_engine, left$estimator_engine)
  expect_identical(nomo_methods_used(asked), nomo_methods_used(left))
  expect_true(all(c("wlsmv_cfa", "categorical_correlations") %in% nomo_methods_used(asked)))
  expect_false("cfa_other_estimator" %in% nomo_methods(asked)$id)
})


test_that("an EFA is credited with its extraction and the rotation it records (#145)", {
  efa <- function(fm = "minres", rotation = "oblimin", n_factors = 2) {
    structure(
      list(fm = fm, rotation = rotation, n_factors = n_factors,
           oblique = rotation %in% nomologR:::nomo_efa_rotations$oblique),
      class = c("nomo_efa", "list")
    )
  }
  # MINRES under each name psych gives the same criterion; anything else is
  # another common-factor extraction.
  for (fm in c("minres", "uls", "ols", "old.min")) {
    expect_true("minres_extraction" %in% nomo_methods_used(efa(fm)), info = fm)
  }
  for (fm in c("ml", "pa", "wls", "gls", "minchi", "alpha")) {
    used <- nomo_methods_used(efa(fm))
    expect_true("common_factor_extraction" %in% used, info = fm)
    expect_false("minres_extraction" %in% used, info = fm)
  }
  expect_false(any(c("minres_extraction", "common_factor_extraction") %in%
                     nomo_methods_used(efa(NA_character_))))

  rotations <- function(rotation, n_factors = 2) {
    intersect(nomo_methods_used(efa(rotation = rotation, n_factors = n_factors)),
              c("oblique_rotation", "orthogonal_rotation", "orthogonal_rotation_other"))
  }
  expect_identical(rotations("oblimin"), "oblique_rotation")
  expect_identical(rotations("promax"), "oblique_rotation")
  expect_identical(rotations("geominQ"), "oblique_rotation")
  expect_identical(rotations("varimax"), "orthogonal_rotation")
  expect_identical(rotations("Varimax"), "orthogonal_rotation")
  expect_identical(rotations("quartimax"), "orthogonal_rotation_other")
  # An unrotated solution, and a one-factor solution, credit no rotation.
  expect_identical(rotations("none"), character())
  expect_identical(rotations("oblimin", n_factors = 1), character())
  # Neither does a solution that records no rotation.
  unrecorded <- structure(list(fm = "minres", n_factors = 2, oblique = FALSE),
                          class = c("nomo_efa", "list"))
  expect_identical(nomo_methods_used(unrecorded), "minres_extraction")
  expect_identical(rotations(NA_character_), character())

  registry <- nomo_methods_registry()
  expect_identical(registry$method[registry$id == "oblique_rotation"], "Oblique rotation")
  expect_identical(registry$lineage[registry$id == "orthogonal_rotation_other"], "historical")

  skip_on_cran()
  ml <- nomo_methods(nomo_efa(nomo_demo_continuous, factors = 2, fm = "ml"))$id
  expect_true("common_factor_extraction" %in% ml)
  unrotated <- nomo_methods(nomo_efa(nomo_demo_continuous, factors = 2, rotation = "none"))$id
  expect_false(any(c("oblique_rotation", "orthogonal_rotation", "orthogonal_rotation_other") %in%
                     unrotated))
  promax <- nomo_methods(nomo_efa(nomo_demo_continuous, factors = 2, rotation = "promax"))$id
  expect_true("oblique_rotation" %in% promax)
})


test_that("equivalence testing is credited only when an equivalence region was tested (#145)", {
  bare <- nomo_hypotheses(
    "Agency -> Persistence" = positive(),
    "Agency <-> SocialDesirability" = negligible()
  )
  expect_false("equivalence_testing" %in% nomo_methods(bare)$id)
  bounded <- nomo_hypotheses(
    "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15))
  )
  expect_true("equivalence_testing" %in% nomo_methods(bounded)$id)

  # A network is credited from its evidence: a finite equivalence interval.
  network <- function(lower, upper) {
    structure(
      list(hypotheses = bare,
           hypothesis_evidence = tibble::tibble(equivalence_ci_lower = lower,
                                                equivalence_ci_upper = upper)),
      class = c("nomo_network", "list")
    )
  }
  expect_false("equivalence_testing" %in% nomo_methods_used(network(NA_real_, NA_real_)))
  expect_true("equivalence_testing" %in% nomo_methods_used(network(-.05, .08)))
  expect_false("equivalence_testing" %in% nomo_methods_used(
    structure(list(hypotheses = bare), class = c("nomo_network", "list"))
  ))

  skip_on_cran()
  model <- nomo_model(list(
    Agency = paste0("ag", 1:4),
    Persistence = paste0("pe", 1:4),
    SocialDesirability = paste0("sd", 1:3)
  ))
  untested <- nomo_network(model, data = nomo_demo_network, hypotheses = bare)
  expect_true(all(is.na(untested$hypothesis_evidence$equivalence_ci_lower)))
  expect_false("equivalence_testing" %in% nomo_methods(untested)$id)
})


# Registry text and dates (#145) ------------------------------------------------

test_that("registry estimands say what each method computes (#145)", {
  reg <- nomo_methods_registry()
  row <- function(id) reg[reg$id == id, , drop = FALSE]

  # factors-6: revised MAP is a fourth matrix power, not a mean of fourth powers.
  expect_match(row("map_revised")$estimand, "trace of the fourth power", fixed = TRUE)
  expect_match(row("map_revised")$estimand, "not the mean of fourth-power", fixed = TRUE)
  expect_false(grepl("under-extraction", row("map_revised")$assumptions, fixed = TRUE))
  # ?nomo_factors defines the TR2 and TR4 labels its output uses.
  description <- nomo_test_rd_text("nomo_factors", "\\details")
  expect_match(description, "TR2, the original", fixed = TRUE)
  expect_match(description, "TR4, the revised form", fixed = TRUE)
  expect_match(description, "not the average of fourth-power", fixed = TRUE)
  # lit-6: alpha equals reliability under essential tau-equivalence.
  expect_false(startsWith(row("alpha")$estimand, "Lower bound"))
  expect_match(row("alpha")$estimand, "essentially", fixed = TRUE)
  expect_match(row("alpha")$estimand, "a lower bound when loadings", fixed = TRUE)
  # lit-7: KMO is r^2 / (r^2 + q^2).
  expect_match(row("kmo")$estimand, "squared correlations plus squared partial", fixed = TRUE)
  # lit-5: the shift is not about positivity.
  expect_false(grepl("positive", row("lrt_scaled_shifted")$estimand, fixed = TRUE))
  expect_match(row("lrt_scaled_shifted")$estimand, "left unadjusted", fixed = TRUE)
  # lit-8: EKC is not limited to uncorrelated factors.
  expect_false(grepl("approximately uncorrelated", row("ekc")$assumptions, fixed = TRUE))
  expect_match(row("ekc")$assumptions, "more accurate with correlated factors", fixed = TRUE)
  # lit-4: the TLI and the CFI are dated separately in the text.
  expect_match(row("incremental_fit")$estimand, "CFI (Bentler, 1990)", fixed = TRUE)
  # "a priori" and "post hoc" take no hyphen.
  expect_false(any(grepl("post-hoc|a-priori", c(reg$method, reg$estimand, reg$assumptions),
                         ignore.case = TRUE)))
})


test_that("methods are dated from the work that introduced them (#145, lit-3, lit-4)", {
  reg <- nomo_methods_registry()
  history <- nomo_methods_history()
  delta <- reg[reg$id == "invariance_delta_fit", ]
  expect_identical(delta$introduced, 2002L)
  expect_true("cheung_rensvold_2002" %in% delta$citations[[1]])
  expect_identical(history$origin[history$id == "invariance_delta_fit"], "cheung_rensvold_2002")
  expect_true(is.na(reg$introduced[reg$id == "reliability_bootstrap_ci"]))
})


test_that("short citations follow one APA 7 rule (#145, lit-10)", {
  bib <- nomo_bibliography()
  authors <- sub(" \\(\\d{4}\\)\\..*$", "", bib$citation)
  n_authors <- lengths(regmatches(authors, gregexpr("\\., (& )?\\S", authors))) + 1L

  expect_true(all(grepl("^[^,&]+ \\(\\d{4}\\)$", bib$short[n_authors == 1L])))
  expect_true(all(grepl("^[^,&]+ & [^,&]+ \\(\\d{4}\\)$", bib$short[n_authors == 2L])))
  expect_true(all(grepl("^[^,&]+ et al\\. \\(\\d{4}\\)$", bib$short[n_authors >= 3L])))
  expect_gt(sum(n_authors >= 3L), 20L)
  # "et al." never makes two works read alike.
  expect_false(anyDuplicated(bib$short) > 0L)
})


test_that("registry references are complete (#145, lit-9, lit-11)", {
  bib <- nomo_bibliography()
  cite <- function(key) bib$citation[bib$key == key]

  expect_match(cite("ronkko_cho_2022"), "25(1), 6-47.", fixed = TRUE)
  expect_match(cite("satorra_2000"),
               "In R. D. H. Heijmans, D. S. G. Pollock, & A. Satorra (Eds.)", fixed = TRUE)
  expect_match(cite("hu_bentler_1999"), "^Hu, L\\.-T\\., & Bentler")
  expect_false(grepl("Sorbom", cite("hancock_mueller_2001"), fixed = TRUE))
  expect_match(cite("hancock_mueller_2001"), "rbom (Eds.)", fixed = TRUE)
  expect_match(cite("fokkema_greiff_2017"), "equals trouble: Overfitting", fixed = TRUE)

  # The help pages that repeat these references give them as the registry does.
  refs <- function(topic) nomo_test_rd_text(topic, "\\references")
  for (topic in c("nomo_apa_table", "nomo_validity")) {
    expect_match(refs(topic), "Organizational Research Methods, 25}(1), 6-47.", fixed = TRUE,
                 info = topic)
  }
  for (topic in c("nomo_compare", "nomo_esem")) {
    expect_match(refs(topic), "In R. D. H. Heijmans, D. S. G. Pollock, & A. Satorra (Eds.)",
                 fixed = TRUE, info = topic)
  }
  expect_match(refs("nomo_cfa"), "Hu, L.-T., & Bentler", fixed = TRUE)
  expect_false(grepl("Sorbom", refs("nomo_hierarchical"), fixed = TRUE))
  expect_match(refs("nomo_split"), "equals trouble: Overfitting in the assessment", fixed = TRUE)
})
