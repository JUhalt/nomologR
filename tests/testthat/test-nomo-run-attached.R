# Scores and missing-data sensitivity in the guided workflow (#73) --------------

attached_scales <- list(A = paste0("a", 1:5), B = paste0("b", 1:5))

attached_decisions <- list(
  factor_count = list(value = c(A = 1, B = 1), rationale = "Prespecified."),
  cfa_model = list(value = nomo_model(attached_scales), rationale = "Prespecified."),
  measurement_model = list(value = "proceed", rationale = "Prespecified.")
)

attached_run <- local({
  cache <- list()
  function(settings = list(scores = list(method = "sum"),
                           missing = list(reliability = FALSE))) {
    key <- paste(deparse(settings), collapse = "")
    if (!is.null(cache[[key]])) return(cache[[key]])
    run <- nomo_run(
      nomo_demo_continuous, scales = attached_scales, mode = "research",
      decisions = attached_decisions,
      settings = c(list(factors = list(seed = 73)), settings)
    )
    cache[[key]] <<- run
    run
  }
})


test_that("requested scores and missing-data sensitivity follow the CFA", {
  run <- attached_run()
  expect_identical(run$status, "complete")

  expect_s3_class(run$results$scores, "nomo_scores")
  expect_identical(run$results$scores$method, "sum")
  standalone <- nomo_scores(run$results$cfa, method = "sum")
  expect_identical(run$results$scores$diagnostics, standalone$diagnostics)

  m <- run$results$missing$cfa
  expect_s3_class(m, "nomo_missing")
  expect_identical(m$strategies$strategy, c("listwise", "ml"))
  expect_null(m$reliability)
  expect_equal(
    m$estimates,
    nomo_missing(run$results$cfa, data = nomo_demo_continuous, reliability = FALSE)$estimates
  )

  log <- run$decision_log
  expect_match(log$observation[log$id == "scores"], "sum method the researcher specified",
               fixed = TRUE)
  expect_match(log$observation[log$id == "missing_data_cfa"],
               "Listwise deletion and FIML", fixed = TRUE)
})


test_that("a run that requests neither is unchanged, and the stage table keeps its shape", {
  run <- attached_run(list())
  expect_null(run$results$scores)
  expect_null(run$results$missing)
  expect_identical(run$stage_status$stage, nomologR:::nomo_run_stage_order())
  expect_false(any(c("scores", "missing_data_cfa") %in% run$decision_log$id))
})


test_that("attached evidence reaches the component log and the method credits", {
  run <- attached_run()
  component <- nomo_table(run, "component_log")
  expect_true("scores" %in% component$pipeline_component)
  expect_true("missing" %in% component$pipeline_component)
  expect_true(all(component$pipeline_scope[component$pipeline_component == "missing"] ==
                    "measurement_model"))

  used <- nomo_methods(run)$id
  expect_true(all(c("unit_weighted_score", "missing_sensitivity", "fiml") %in% used))
})


test_that("a network's missing-data sensitivity follows the network", {
  skip_on_cran()
  d <- nomo_demo_network
  set.seed(73)
  d$ag2[sample.int(nrow(d), 60)] <- NA
  scales <- list(Agency = paste0("ag", 1:4), Persistence = paste0("pe", 1:4))
  run <- nomo_run(
    d, scales = scales, mode = "research",
    decisions = list(
      factor_count = list(value = c(Agency = 1, Persistence = 1), rationale = "x"),
      cfa_model = list(value = nomo_model(scales), rationale = "x"),
      measurement_model = list(value = "proceed", rationale = "x")
    ),
    settings = list(
      factors = list(seed = 73),
      missing = list(reliability = FALSE),
      network = list(hypotheses = nomo_hypotheses("Agency -> Persistence" = positive()))
    )
  )

  expect_identical(run$status, "complete")
  expect_identical(names(run$results$missing), c("cfa", "network"))
  expect_s3_class(run$results$missing$network, "nomo_missing")
  expect_identical(run$results$missing$network$object, "nomo_network")
  component <- nomo_table(run, "component_log")
  expect_true("theory_network" %in%
                component$pipeline_scope[component$pipeline_component == "missing"])
})


test_that("scores need a method the researcher names, and settings are validated", {
  run_with <- function(settings) {
    nomo_run(nomo_demo_continuous, scales = attached_scales, settings = settings)
  }
  expect_error(run_with(list(scores = list(sum = TRUE))),
               "nomologR does not choose a scoring method", fixed = TRUE)
  expect_error(run_with(list(scores = list(method = "eap"))),
               "`settings$scores$method`", fixed = TRUE)
  expect_error(run_with(list(missing = list(strategies = 1))),
               "`settings$missing$strategies` must be a character vector.", fixed = TRUE)
  expect_error(run_with(list(missing = list(reliability = NA))),
               "`settings$missing$reliability` must be TRUE or FALSE.", fixed = TRUE)
  expect_error(run_with(list(scores = list(method = "sum", fit = 1))),
               "cannot override pipeline-controlled", fixed = TRUE)
})


test_that("requested evidence that cannot be computed is recorded, and the run goes on", {
  local_mocked_bindings(nomo_scores = function(...) stop("simulated scoring failure"))
  run <- nomo_run(
    nomo_demo_continuous, scales = attached_scales, mode = "research",
    decisions = attached_decisions,
    settings = list(factors = list(seed = 73), scores = list(method = "sum"))
  )
  expect_identical(run$status, "complete")
  expect_null(run$results$scores)
  entry <- run$decision_log[run$decision_log$id == "scores", ]
  expect_identical(entry$decision, "not computed")
  expect_match(entry$observation, "simulated scoring failure", fixed = TRUE)
})


test_that("attached settings can be added before the CFA and are locked after", {
  paused <- nomo_run(nomo_demo_continuous, scales = attached_scales,
                     settings = list(factors = list(seed = 73)))
  expect_identical(paused$next_stage, "efa")

  resumed <- nomo_run(resume = paused, decisions = attached_decisions,
                      settings = list(scores = list(method = "regression")))
  expect_identical(resumed$results$scores$method, "regression")

  expect_error(
    nomo_run(resume = resumed, settings = list(scores = list(method = "sum"))),
    "cannot be changed while resuming", fixed = TRUE
  )
})


test_that("the report presents scores and missing-data sensitivity", {
  run <- attached_run()
  scores <- nomologR:::nomo_report_scores(run$results$scores)
  expect_match(scores$summary, "Unit-weighted scores (method: sum)", fixed = TRUE)
  expect_match(scores$summary, "parallel model that unit weighting assumes", fixed = TRUE)
  expect_identical(scores$diagnostics, run$results$scores$diagnostics)

  missing <- nomologR:::nomo_report_missing(run$results$missing$cfa)
  expect_match(missing$summary, "27 of 500 cases (5.4%)", fixed = TRUE)
  expect_match(missing$summary, "The reference is FIML", fixed = TRUE)
  expect_identical(missing$differences$strategy[[1L]], "Listwise deletion")
  # Largest differences first.
  expect_false(is.unsorted(rev(abs(missing$differences$difference_in_se))))

  ordered <- nomo_cfa(
    "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
    data = nomo_demo_ordinal, ordered = c(paste0("a", 1:5), paste0("b", 1:5))
  )
  untested <- nomologR:::nomo_report_scores(nomo_scores(ordered, method = "sum"))
  expect_match(untested$summary, "implemented here for continuous indicators", fixed = TRUE)
})


test_that("the rendered report carries both sections", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  file <- nomo_report(attached_run(), file = tempfile(fileext = ".html"),
                      include_plots = FALSE, include_session = FALSE, quiet = TRUE)
  html <- gsub("[[:space:]]+", " ", paste(readLines(file, warn = FALSE), collapse = " "))
  expect_match(html, 'id="scores"', fixed = TRUE)
  expect_match(html, 'id="missing-data-sensitivity"', fixed = TRUE)
  expect_match(html, "Score properties (Grice, 2001)", fixed = TRUE)
})
