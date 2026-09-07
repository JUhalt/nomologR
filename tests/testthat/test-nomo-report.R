test_that("nomo_report validates workflow and output arguments", {
  run <- make_m9_minimal_run()

  expect_error(
    nomo_report(list()),
    "nomo_run"
  )

  expect_error(
    nomo_report(run, file = "report.pdf"),
    "HTML"
  )

  expect_error(
    nomo_report(run, title = ""),
    "title"
  )

  expect_error(
    nomo_report(run, max_table_rows = 0),
    "positive integer"
  )

  expect_error(
    nomo_report(run, include_plots = NA),
    "include_plots"
  )
})


test_that("report helpers expose data, calls, and stage tables", {
  run <- make_m9_minimal_run()

  chars <- nomologR:::nomo_report_data_characteristics(run)
  expect_s3_class(chars, "data.frame")
  expect_equal(chars$n_cases, 5L)
  expect_equal(chars$candidate_items, 2L)

  calls <- nomologR:::nomo_report_call_history(run)
  expect_equal(nrow(calls), 1L)
  expect_match(calls$call, "nomo_run")

  stages <- nomologR:::nomo_report_run_table(run, "stages")
  expect_equal(nrow(stages), 8L)
})


test_that("component table extraction covers M1 through M5 workflow results", {
  run <- make_m9_report_run()

  screen <- run$results$screen$WellBeing
  factors <- run$results$factors$WellBeing
  efa <- run$results$efa$WellBeing
  cfa <- run$results$cfa
  rel <- run$results$reliability
  val <- run$results$validity

  expect_gt(nrow(nomologR:::nomo_report_component_table(
    screen, "screen", "primary"
  )), 0L)

  expect_gt(nrow(nomologR:::nomo_report_component_table(
    factors, "factors", "primary"
  )), 0L)

  expect_gt(nrow(nomologR:::nomo_report_component_table(
    efa, "efa", "primary"
  )), 0L)

  expect_gt(nrow(nomologR:::nomo_report_component_table(
    cfa, "cfa", "primary"
  )), 0L)

  expect_gt(nrow(nomologR:::nomo_report_component_table(
    rel, "reliability", "primary"
  )), 0L)

  expect_gt(nrow(nomologR:::nomo_report_component_table(
    val, "validity", "primary"
  )), 0L)
})


test_that("matrix and list columns are converted to report-ready tables", {
  mat <- matrix(
    c(.8, .2, .1, .7),
    nrow = 2L,
    dimnames = list(c("i1", "i2"), c("F1", "F2"))
  )

  tab <- nomologR:::nomo_report_matrix_table(mat, "item")
  expect_equal(tab$item, c("i1", "i2"))

  list_tab <- tibble::tibble(
    id = 1:2,
    values = list(c("a", "b"), "c")
  )

  flat <- nomologR:::nomo_report_flatten_table(list_tab)
  expect_false(is.list(flat$values))
  expect_equal(flat$values[[1L]], "a; b")
})


test_that("evidence trace keeps recommendations attached to source evidence", {
  run <- make_m9_report_run()
  trace <- nomologR:::nomo_report_evidence_trace(run)

  expect_gt(nrow(trace), 0L)
  expect_true(all(grepl("^E[0-9]{4}$", trace$evidence_id)))
  expect_true(all(c(
    "pipeline_component",
    "pipeline_scope",
    "metric",
    "observation",
    "recommendation"
  ) %in% names(trace)))

  flagged <- nomologR:::nomo_report_flagged_trace(run)
  if (nrow(flagged)) {
    expect_true(all(tolower(flagged$severity) %in% c("review", "concern")))
  }
})


test_that("component summaries and plot failures are non-destructive", {
  run <- make_m9_report_run()

  txt <- nomologR:::nomo_report_component_summary_text(
    run$results$cfa
  )
  expect_match(txt, "confirmatory", ignore.case = TRUE)

  p <- nomologR:::nomo_report_safe_plot(
    run$results$cfa,
    "fit"
  )
  expect_true(p$ok)
  expect_s3_class(p$plot, "ggplot")

  bad <- nomologR:::nomo_report_safe_plot(NULL, "fit")
  expect_false(bad$ok)
})


test_that("package version and citation tables are report-ready", {
  versions <- nomologR:::nomo_report_package_versions()
  expect_true(all(c("package", "version") %in% names(versions)))
  expect_true("nomologR" %in% versions$package)

  cites <- nomologR:::nomo_report_citations()
  expect_true(all(
    c("package", "installed_version", "citation") %in% names(cites)
  ))
  expect_true("nomologR" %in% cites$package)
})


test_that("deviation helper identifies explicit partial and post-hoc evidence", {
  run <- make_m9_minimal_run()

  run$results$invariance <- structure(
    list(
      fit_evidence = tibble::tibble(),
      ordered_categories = tibble::tibble(),
      partial = list(
        releases = tibble::tibble(
          level = "metric",
          syntax = "F =~ x2",
          rationale = "Researcher-specified release."
        )
      ),
      local_strain = tibble::tibble(),
      decision_log = tibble::tibble()
    ),
    class = c("nomo_invariance", "list")
  )

  run$results$network <- list(
    hypothesis_evidence = tibble::tibble(
      relation = "A -> B",
      confirmatory_status = "posthoc",
      concordance = "concordant"
    )
  )

  devs <- nomologR:::nomo_report_deviations(run)
  expect_equal(nrow(devs), 2L)
  expect_true(any(grepl("partial", devs$type)))
  expect_true(any(grepl("post-hoc", devs$type)))
})


test_that("existing report is protected unless overwrite is explicit", {
  run <- make_m9_minimal_run()
  file <- tempfile(fileext = ".html")
  writeLines("existing", file)

  expect_error(
    nomo_report(run, file = file),
    "already exists"
  )
})


test_that("report template preparation injects a safe dynamic title", {
  template <- tempfile(fileext = ".Rmd")
  input <- tempfile(fileext = ".Rmd")

  writeLines(
    c(
      "---",
      'title: "__NOMO_REPORT_TITLE__"',
      "---"
    ),
    template
  )

  nomologR:::nomo_report_prepare_template(
    template,
    input,
    'A "quoted" report title'
  )

  out <- paste(readLines(input, warn = FALSE), collapse = "\n")
  expect_match(out, 'title: "A \\"quoted\\" report title"', fixed = TRUE)
  expect_false(grepl("__NOMO_REPORT_TITLE__", out, fixed = TRUE))
})
