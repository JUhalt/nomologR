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

# ---- recovered from hygiene consolidation: test-nomo-report.R ----
# ---- consolidated from test-nomo-report-coverage.R ----

test_that("report helper edge cases remain explicit and non-destructive", {
  expect_equal(
    nrow(nomologR:::nomo_report_matrix_table(NULL)),
    0L
  )
  expect_equal(
    nrow(nomologR:::nomo_report_matrix_table(1:3)),
    0L
  )

  no_rownames <- matrix(1:4, nrow = 2L)
  rownames(no_rownames) <- NULL
  tab <- nomologR:::nomo_report_matrix_table(no_rownames)
  expect_equal(ncol(tab), 2L)

  expect_equal(
    nrow(nomologR:::nomo_report_flatten_table(NULL)),
    0L
  )
  expect_equal(
    nrow(nomologR:::nomo_report_flatten_table("not a table")),
    0L
  )

  list_tab <- tibble::tibble(
    id = 1:2,
    values = list(NULL, character())
  )
  flat <- nomologR:::nomo_report_flatten_table(list_tab)
  expect_equal(flat$values, c("", ""))
})


test_that("report validation catches malformed run objects and paths", {
  run <- make_m9_minimal_run()

  malformed <- run
  malformed$source_data <- NULL
  expect_error(
    nomologR:::nomo_report_validate_run(malformed),
    "source_data"
  )

  expect_error(
    nomologR:::nomo_report_validate_scalar_logical(c(TRUE, FALSE), "flag"),
    "TRUE"
  )
  expect_error(
    nomologR:::nomo_report_validate_scalar_logical("yes", "flag"),
    "TRUE"
  )

  expect_error(
    nomologR:::nomo_report_validate_file(character(), FALSE),
    "non-empty"
  )
  expect_error(
    nomologR:::nomo_report_validate_file(NA_character_, FALSE),
    "non-empty"
  )
  expect_error(
    nomologR:::nomo_report_validate_file("   ", FALSE),
    "non-empty"
  )

  file <- tempfile(fileext = ".html")
  writeLines("existing", file)
  expect_silent(
    nomologR:::nomo_report_validate_file(file, TRUE)
  )
})


test_that("component-list helper handles absent, unnamed, and workflow results", {
  run <- make_m9_minimal_run()

  expect_length(
    nomologR:::nomo_report_component_list(run, "unknown"),
    0L
  )

  run$results$cfa <- NULL
  expect_length(
    nomologR:::nomo_report_component_list(run, "cfa"),
    0L
  )

  run$results$screen <- list(
    structure(list(item_summary = tibble::tibble()), class = "dummy"),
    structure(list(item_summary = tibble::tibble()), class = "dummy")
  )
  unnamed <- nomologR:::nomo_report_component_list(run, "screen")
  expect_equal(names(unnamed), c("scope_1", "scope_2"))

  run$results$cfa <- list(fit_evidence = tibble::tibble(metric = "CFI"))
  workflow <- nomologR:::nomo_report_component_list(run, "cfa")
  expect_equal(names(workflow), "workflow")
})


test_that("component summary helper preserves fallback behavior", {
  expect_equal(
    nomologR:::nomo_report_component_summary_text(NULL),
    "No component result is available."
  )

  # Register synthetic S3 methods explicitly. Defining methods only inside a
  # test_that() frame is not sufficient for reliable S3 dispatch from package
  # namespace code.
  registerS3method(
    "summary",
    "nomo_report_summary_boom",
    function(object, ...) stop("summary failed"),
    envir = asNamespace("base")
  )
  registerS3method(
    "print",
    "nomo_report_summary_boom",
    function(x, ...) {
      cat("fallback print")
      invisible(x)
    },
    envir = asNamespace("base")
  )

  obj <- structure(list(), class = "nomo_report_summary_boom")
  expect_match(
    nomologR:::nomo_report_component_summary_text(obj),
    "fallback print"
  )

  registerS3method(
    "summary",
    "nomo_report_double_boom",
    function(object, ...) stop("summary failed"),
    envir = asNamespace("base")
  )
  registerS3method(
    "print",
    "nomo_report_double_boom",
    function(x, ...) stop("print failed"),
    envir = asNamespace("base")
  )

  obj2 <- structure(list(), class = "nomo_report_double_boom")
  expect_match(
    nomologR:::nomo_report_component_summary_text(obj2),
    "Summary unavailable"
  )
})


test_that("data characteristics cover split roles, missingness, and empty item overlap", {
  dat <- data.frame(
    i1 = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10),
    i2 = 10:1,
    extra = letters[1:10]
  )

  run <- make_m9_minimal_run()
  run$scales <- list(S = c("i1", "i2"))
  run$source_data <- nomo_split(
    dat,
    validation_prop = .40,
    seed = 99L
  )
  run$sample_design <- "calibration_validation"

  chars <- nomologR:::nomo_report_data_characteristics(run)
  expect_equal(nrow(chars), 2L)
  expect_equal(
    chars$role,
    c("calibration / exploratory", "validation / confirmatory")
  )
  expect_equal(sum(chars$n_cases), 10L)
  expect_true(sum(chars$candidate_item_missing_cells) >= 1L)

  run$scales <- list(S = c("not_present"))
  no_overlap <- nomologR:::nomo_report_data_characteristics(run)
  expect_true(all(no_overlap$candidate_items == 0L))
  expect_true(all(is.na(no_overlap$candidate_item_missing_pct)))

  run$source_data <- "invalid source data"
  expect_equal(
    nrow(nomologR:::nomo_report_data_characteristics(run)),
    0L
  )
})


test_that("call history and run-table helpers fail closed", {
  run <- make_m9_minimal_run()

  run$call_history <- NULL
  calls <- nomologR:::nomo_report_call_history(run)
  expect_equal(nrow(calls), 1L)

  run$call <- NULL
  expect_equal(
    nrow(nomologR:::nomo_report_call_history(run)),
    0L
  )

  expect_equal(
    nrow(nomologR:::nomo_report_run_table(run, "not_a_real_table")),
    0L
  )
})


test_that("component-table helper covers alternate M1 through M5 report views", {
  run <- make_m9_report_run()

  screen <- run$results$screen$WellBeing
  factors <- run$results$factors$WellBeing
  efa <- run$results$efa$WellBeing
  cfa <- run$results$cfa
  rel <- run$results$reliability
  val <- run$results$validity

  expect_s3_class(
    nomologR:::nomo_report_component_table(screen, "screen", "relationships"),
    "data.frame"
  )
  expect_s3_class(
    nomologR:::nomo_report_component_table(screen, "screen", "cases"),
    "data.frame"
  )
  expect_s3_class(
    nomologR:::nomo_report_component_table(screen, "screen", "decision_log"),
    "data.frame"
  )

  for (type in c("criteria", "adequacy", "concordance", "decision_log")) {
    expect_s3_class(
      nomologR:::nomo_report_component_table(factors, "factors", type),
      "data.frame"
    )
  }

  for (type in c(
    "pattern", "factor_correlations", "residuals", "decision_log"
  )) {
    expect_s3_class(
      nomologR:::nomo_report_component_table(efa, "efa", type),
      "data.frame"
    )
  }

  for (type in c(
    "loadings", "factor_correlations", "heywood", "residuals",
    "modification_indices", "decision_log"
  )) {
    expect_s3_class(
      nomologR:::nomo_report_component_table(cfa, "cfa", type),
      "data.frame"
    )
  }

  for (type in c("alpha_status", "ci_status", "decision_log")) {
    expect_s3_class(
      nomologR:::nomo_report_component_table(rel, "reliability", type),
      "data.frame"
    )
  }

  for (type in c("discriminant", "htmt_status", "decision_log")) {
    expect_s3_class(
      nomologR:::nomo_report_component_table(val, "validity", type),
      "data.frame"
    )
  }

  expect_equal(
    nrow(nomologR:::nomo_report_component_table(cfa, "unknown", "primary")),
    0L
  )
  expect_equal(
    nrow(nomologR:::nomo_report_component_table(NULL, "cfa", "primary")),
    0L
  )
})


test_that("optional M7 and M6 branches are reportable through common helpers", {
  skip_on_cran()
  run <- make_m9_full_report_run()
  expect_identical(run$status, "complete")

  inv <- run$results$invariance
  net <- run$results$network

  for (type in c(
    "primary", "categories", "partial", "local_strain", "decision_log"
  )) {
    expect_s3_class(
      nomologR:::nomo_report_component_table(inv, "invariance", type),
      "data.frame"
    )
  }

  for (type in c(
    "primary", "fit", "measurement", "relations", "replication",
    "decision_log"
  )) {
    expect_s3_class(
      nomologR:::nomo_report_component_table(net, "network", type),
      "data.frame"
    )
  }

  expect_equal(
    names(nomologR:::nomo_report_component_list(run, "invariance")),
    "workflow"
  )
  expect_equal(
    names(nomologR:::nomo_report_component_list(run, "network")),
    "workflow"
  )
})


test_that("empty evidence traces and revision decisions remain auditable", {
  run <- make_m9_minimal_run()
  run$results$screen <- list()
  run$results$factors <- list()
  run$results$efa <- list()

  trace <- nomologR:::nomo_report_evidence_trace(run)
  expect_equal(nrow(trace), 0L)
  expect_true("evidence_id" %in% names(trace))

  flagged <- nomologR:::nomo_report_flagged_trace(run)
  expect_equal(nrow(flagged), 0L)

  run$decision_log <- tibble::tibble(
    id = "measurement_model",
    stage = "measurement_review",
    scope = "measurement_model",
    observation = "",
    reason = "",
    options = "",
    consequence = "",
    decision = "revise",
    rationale = "Theory requires a substantively revised model.",
    source = "researcher_decision"
  )

  devs <- nomologR:::nomo_report_deviations(run)
  expect_equal(nrow(devs), 1L)
  expect_equal(devs$type, "workflow deviation/revision decision")
  expect_equal(devs$detail, "revise")
})


test_that("safe plotting covers default and error paths", {
  run <- make_m9_report_run()

  default_plot <- nomologR:::nomo_report_safe_plot(
    run$results$reliability
  )
  expect_true(default_plot$ok)

  bad <- nomologR:::nomo_report_safe_plot(list())
  expect_false(bad$ok)
  expect_true(nzchar(bad$message))
})


test_that("citation sanitization removes angle-bracket URL artifacts", {
  txt <- paste(
    "Uhalt J (2026). nomologR.",
    "<https://github.com/JUhalt/nomologR>",
    "See also <https://example.org/path>."
  )

  clean <- nomologR:::nomo_report_sanitize_citation_text(txt)

  expect_false(grepl("<https://", clean, fixed = TRUE))
  expect_false(grepl(">%", clean, fixed = TRUE))
  expect_match(
    clean,
    "https://github.com/JUhalt/nomologR",
    fixed = TRUE
  )
  expect_match(clean, "https://example.org/path", fixed = TRUE)

  expect_equal(
    nomologR:::nomo_report_sanitize_citation_text(character()),
    character()
  )
})


test_that("template preparation rejects missing or duplicate title markers", {
  missing <- tempfile(fileext = ".Rmd")
  duplicate <- tempfile(fileext = ".Rmd")
  out <- tempfile(fileext = ".Rmd")

  writeLines(c("---", 'title: "ordinary"', "---"), missing)
  expect_error(
    nomologR:::nomo_report_prepare_template(missing, out, "Title"),
    "exactly one title marker"
  )

  writeLines(
    c(
      "---",
      'title: "__NOMO_REPORT_TITLE__"',
      'title: "__NOMO_REPORT_TITLE__"',
      "---"
    ),
    duplicate
  )
  expect_error(
    nomologR:::nomo_report_prepare_template(duplicate, out, "Title"),
    "exactly one title marker"
  )
})

# ---- consolidated from test-nomo-report-render.R ----
test_that("nomo_report renders a polished self-contained HTML archive when Pandoc is available", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  run <- make_m9_minimal_run()
  file <- tempfile(fileext = ".html")

  out <- nomo_report(
    run,
    file = file,
    title = "M9 render fixture",
    include_plots = FALSE,
    include_session = FALSE,
    quiet = TRUE
  )

  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")

  expect_match(
    html,
    'id="researcher-inputs-and-data-characteristics"',
    fixed = TRUE
  )
  expect_match(html, 'id="decision-log"', fixed = TRUE)
  expect_match(html, 'id="methods-and-citations"', fixed = TRUE)
  expect_match(html, 'id="methods-used-in-this-workflow"', fixed = TRUE)
  expect_match(html, 'id="method-references"', fixed = TRUE)
  expect_match(html, 'id="software"', fixed = TRUE)
  expect_match(html, 'id="full-evidence-trace"', fixed = TRUE)
  expect_match(html, "Report boundary", fixed = TRUE)

  # The custom report title should replace the generic template title.
  expect_match(html, "<title>M9 render fixture</title>", fixed = TRUE)
  expect_false(grepl(
    "<title>nomologR reproducible analysis report</title>",
    html,
    fixed = TRUE
  ))

  # Tables must be actual HTML, not escaped literal code output.
  expect_match(
    html,
    '<table class="table table-striped table-condensed nomo-table">',
    fixed = TRUE
  )
  expect_match(html, '<div class="table-responsive">', fixed = TRUE)
  expect_false(grepl(
    '&lt;table class=&quot;table table-striped table-condensed nomo-table&quot;&gt;',
    html,
    fixed = TRUE
  ))

  # The report is intended to be archiveable without a MathJax network request.
  expect_false(grepl("MathJax.js", html, fixed = TRUE))
})


test_that("nomo_report can overwrite intentionally", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  run <- make_m9_minimal_run()
  file <- tempfile(fileext = ".html")
  writeLines("old", file)

  out <- nomo_report(
    run,
    file = file,
    include_plots = FALSE,
    include_session = FALSE,
    overwrite = TRUE,
    quiet = TRUE
  )

  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 1000)
})


test_that("nomo_report creates nested output directories", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  run <- make_m9_minimal_run()
  root <- tempfile("nomo-report-dir-")
  file <- file.path(root, "nested", "audit.html")

  expect_false(dir.exists(dirname(file)))

  out <- nomo_report(
    run,
    file = file,
    include_plots = FALSE,
    include_session = FALSE,
    quiet = TRUE
  )

  expect_true(file.exists(out))
  expect_true(dir.exists(dirname(file)))
})


test_that("full optional-branch report renders invariance and network evidence", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  run <- make_m9_full_report_run()
  file <- tempfile(fileext = ".html")

  out <- nomo_report(
    run,
    file = file,
    title = "Full M9 branch fixture",
    include_plots = FALSE,
    include_session = TRUE,
    quiet = TRUE
  )

  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")

  expect_match(html, 'id="measurement-invariance"', fixed = TRUE)
  expect_match(html, 'id="nomological-network"', fixed = TRUE)

  # Pandoc may wrap visible heading text across physical HTML lines. Stable
  # heading IDs are the rendering contract we care about.
  expect_match(
    html,
    'id="local-equality-constraint-strain"',
    fixed = TRUE
  )
  expect_match(
    html,
    'id="theory-specified-relation-evidence"',
    fixed = TRUE
  )
  expect_match(html, 'id="session-information"', fixed = TRUE)

  expect_false(grepl("MathJax.js", html, fixed = TRUE))
  # Self-contained HTML can contain percent-encoded characters inside
# bundled third-party JavaScript and CSS. Strip those assets before
# checking report-visible HTML for encoded arrows.
report_html <- gsub(
  "(?is)<script\\b[^>]*>.*?</script>",
  "",
  html,
  perl = TRUE
)

report_html <- gsub(
  "(?is)<style\\b[^>]*>.*?</style>",
  "",
  report_html,
  perl = TRUE
)

expect_false(
  grepl("%3E", report_html, fixed = TRUE)
)
})


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("closeout: report table helpers return schema-correct empty fallbacks", {
  m <- matrix(1:4, 2, 2)
  flat <- nomologR:::nomo_report_flatten_table(m)
  expect_s3_class(flat, "data.frame")

  run <- make_m9_full_report_run()
  stages <- c(
    "screen", "factors", "efa", "cfa",
    "reliability", "validity", "invariance", "network"
  )

  for (stage in stages) {
    comps <- nomologR:::nomo_report_component_list(run, stage)
    if (length(comps)) {
      empty <- nomologR:::nomo_report_component_table(
        comps[[1L]],
        stage = stage,
        type = "not_a_real_table"
      )
      expect_equal(nrow(empty), 0L)
    }
  }
})


test_that("closeout: report deviations cover sparse post-hoc and workflow-decision schemas", {
  run <- make_m9_minimal_run()
  run$results$network <- list(
    hypothesis_evidence = tibble::tibble(
      confirmatory_status = "post_hoc_exploratory"
    )
  )
  run$decision_log <- tibble::tibble(
    id = "post_decision",
    decision = "revise"
  )

  dev <- nomologR:::nomo_report_deviations(run)
  expect_true(any(dev$type == "post-hoc nomological relation"))
  expect_true(any(dev$type == "workflow deviation/revision decision"))
})


test_that("closeout: report package-version and citation helpers expose package metadata", {
  versions <- nomologR:::nomo_report_package_versions()
  expect_s3_class(versions, "data.frame")
  expect_true(all(c("package", "version") %in% names(versions)))
  expect_true("nomologR" %in% versions$package)

  cites <- nomologR:::nomo_report_citations()
  expect_s3_class(cites, "data.frame")
  expect_true(all(c("package", "installed_version", "citation") %in% names(cites)))
  expect_true("nomologR" %in% cites$package)
})


test_that("closeout: report development-template fallback and missing-template guard are explicit", {

  dev_root <- tempfile("nomo-report-dev-")

  dev_template <- file.path(
    dev_root,
    "inst",
    "rmarkdown",
    "nomo-report.Rmd"
  )

  dir.create(
    dirname(dev_template),
    recursive = TRUE,
    showWarnings = FALSE
  )

  writeLines(
    'title: "__NOMO_REPORT_TITLE__"',
    dev_template
  )

  path <- nomologR:::nomo_report_template_path(
    installed = "",
    development = dev_template
  )

  expect_identical(path, dev_template)
  expect_true(file.exists(path))

  missing_template <- file.path(
    tempfile("nomo-report-missing-"),
    "inst",
    "rmarkdown",
    "nomo-report.Rmd"
  )

  expect_error(
    nomologR:::nomo_report_template_path(
      installed = "",
      development = missing_template
    ),
    "Could not locate"
  )
})


test_that("closeout: report template preparation fails closed when copy is impossible", {
  src <- tempfile(fileext = ".Rmd")
  writeLines('title: "__NOMO_REPORT_TITLE__"', src)
  impossible <- file.path(tempfile(), "child", "report.Rmd")
  expect_error(
    suppressWarnings(
      nomologR:::nomo_report_prepare_template(src, impossible, "Title")
    ),
    "Could not prepare"
  )
})


test_that("closeout: report rendering dependency and output-directory guards are explicit", {
  run <- make_m9_report_run()

  bad_parent <- tempfile()
  writeLines("not a directory", bad_parent)
  out <- file.path(bad_parent, "report.html")
  expect_error(
    nomo_report(run, file = out, overwrite = TRUE, quiet = TRUE),
    "Could not create report output directory"
  )
})


test_that("closeout B: report deviation extraction handles sparse partial and workflow-decision schemas", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- make_m9_minimal_run()
  x$results$invariance <- structure(list(), class = "synthetic_invariance")
  x$results$network <- NULL
  x$decision_log <- tibble::tibble()

  testthat::local_mocked_bindings(
    nomo_table = function(object, type, ...) {
      if (identical(type, "partial")) {
        return(tibble::tibble(syntax = "x1 ~ 1"))
      }
      tibble::tibble()
    },
    .package = "nomologR"
  )

  partial <- nomologR:::nomo_report_deviations(x)
  expect_identical(partial$scope[[1L]], "")
  expect_identical(partial$rationale[[1L]], "")

  x2 <- make_m9_minimal_run()
  x2$results$invariance <- NULL
  x2$results$network <- NULL
  x2$decision_log <- tibble::tibble(id = "posthoc_deviation")

  workflow <- nomologR:::nomo_report_deviations(x2)
  expect_equal(nrow(workflow), 1L)
  expect_identical(workflow$detail[[1L]], "")
  expect_identical(workflow$rationale[[1L]], "")
})


test_that("closeout B: report package metadata helpers can represent unavailable packages and citation failures", {
  missing <- nomologR:::nomo_report_package_versions(
    packages = "definitely_not_a_real_nomologr_package",
    namespace_available = function(pkg) FALSE
  )
  expect_identical(missing$version[[1L]], "not installed")

  missing_citation <- nomologR:::nomo_report_citations(
    packages = "definitely_not_a_real_nomologr_package",
    namespace_available = function(pkg) FALSE
  )
  expect_identical(
    missing_citation$installed_version[[1L]],
    "not installed"
  )

  failed_citation <- nomologR:::nomo_report_citations(
    packages = "nomologR",
    namespace_available = function(pkg) TRUE,
    citation_fun = function(pkg) stop("synthetic citation failure"),
    version_fun = function(pkg) "0.0.0"
  )
  expect_match(
    failed_citation$citation[[1L]],
    "Citation unavailable: synthetic citation failure",
    fixed = TRUE
  )

  expect_true(nomologR:::nomo_report_namespace_available("base"))
  expect_type(nomologR:::nomo_report_pandoc_available(), "logical")
})


test_that("closeout B: report dependency guards distinguish rmarkdown, knitr, and Pandoc", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  run <- make_m9_report_run()

  testthat::local_mocked_bindings(
    nomo_report_namespace_available = function(pkg) FALSE,
    .package = "nomologR"
  )
  expect_error(
    nomo_report(
      run,
      file = tempfile(fileext = ".html"),
      overwrite = TRUE
    ),
    "requires the suggested package `rmarkdown`"
  )

  testthat::local_mocked_bindings(
    nomo_report_namespace_available = function(pkg) {
      !identical(pkg, "knitr")
    },
    .package = "nomologR"
  )
  expect_error(
    nomo_report(
      run,
      file = tempfile(fileext = ".html"),
      overwrite = TRUE
    ),
    "requires the suggested package `knitr`"
  )

  testthat::local_mocked_bindings(
    nomo_report_namespace_available = function(pkg) TRUE,
    nomo_report_pandoc_available = function() FALSE,
    .package = "nomologR"
  )
  expect_error(
    nomo_report(
      run,
      file = tempfile(fileext = ".html"),
      overwrite = TRUE
    ),
    "Pandoc is required"
  )
})


test_that("nomo_report isolates knitr state only while knitting (#40)", {
  skip_if_not_installed("knitr")

  saved_chunk <- knitr::opts_chunk$get()
  saved_options <- options(
    knitr.in.progress = NULL,
    knitr.duplicate.label = NULL
  )
  on.exit(
    {
      options(saved_options)
      knitr::opts_chunk$restore(saved_chunk)
    },
    add = TRUE
  )

  # Outside a knit there is no shared state to protect, so nothing is touched.
  knitr::opts_chunk$set(comment = "#>")
  restore_outside <- nomologR:::nomo_report_isolate_knitr()
  expect_null(getOption("knitr.duplicate.label"))
  expect_identical(knitr::opts_chunk$get("comment"), "#>")
  restore_outside()
  expect_identical(knitr::opts_chunk$get("comment"), "#>")

  # Inside a knit the report is rendered with knitr's defaults, which its own
  # template then sets for itself, so the caller's chunk options cannot reach
  # it and duplicate chunk labels across the two documents are allowed.
  knitr::opts_chunk$restore()
  options(knitr.in.progress = TRUE)
  knitr::opts_chunk$set(dev = "svg", fig.width = 3.1, comment = "#>")

  restore_inside <- nomologR:::nomo_report_isolate_knitr()
  expect_identical(getOption("knitr.duplicate.label"), "allow")
  expect_null(knitr::opts_chunk$get("dev"))
  expect_identical(knitr::opts_chunk$get("comment"), "##")
  expect_equal(knitr::opts_chunk$get("fig.width"), 7)

  # The caller's state comes back, including an unset duplicate-label option.
  restore_inside()
  expect_null(getOption("knitr.duplicate.label"))
  expect_identical(knitr::opts_chunk$get("dev"), "svg")
  expect_equal(knitr::opts_chunk$get("fig.width"), 3.1)
  expect_identical(knitr::opts_chunk$get("comment"), "#>")
})


test_that("nomo_report renders from inside a knitted document (#40)", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  run <- make_m9_report_run()

  dir <- tempfile("nomo-nested-report-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # A calling document with no workaround of any kind: it reuses a chunk label
  # the report template also uses, and sets chunk options that would replace
  # the report's figures with an image format its template never asked for.
  #
  # The calling document sets `dev = "svg"` but never draws a figure of its
  # own, deliberately. What is under test is what reaches the report, and
  # svg() is not operational everywhere: on a macOS runner without cairo it
  # falls back to PNG for the file while still writing the .svg name into
  # the markdown, so a figure here would fail pandoc for a reason that has
  # nothing to do with nomo_report().
  outer <- file.path(dir, "outer.Rmd")
  writeLines(
    r"(---
title: "Thesis chapter"
output: html_document
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(dev = "svg", fig.width = 3.1, comment = "#>")
duplicate_label_before <- getOption("knitr.duplicate.label")
```

```{r report}
inner_file <- nomo_report(
  run,
  file = file.path(report_dir, "nested.html"),
  include_session = FALSE,
  quiet = TRUE
)

caller_state <- list(
  dev = knitr::opts_chunk$get("dev"),
  fig_width = knitr::opts_chunk$get("fig.width"),
  comment = knitr::opts_chunk$get("comment"),
  duplicate_label_restored = identical(
    getOption("knitr.duplicate.label"),
    duplicate_label_before
  )
)
```
)",
    outer
  )

  envir <- new.env(parent = globalenv())
  envir$run <- run
  envir$report_dir <- dir
  envir$nomo_report <- nomo_report

  outer_html <- rmarkdown::render(
    outer,
    output_dir = dir,
    intermediates_dir = dir,
    envir = envir,
    quiet = TRUE
  )

  # The nested render succeeds and keeps its own figures.
  expect_true(file.exists(envir$inner_file))
  inner <- paste(readLines(envir$inner_file, warn = FALSE), collapse = "\n")
  png_figures <- gregexpr("data:image/png;base64", inner, fixed = TRUE)[[1]]
  expect_gt(sum(png_figures > 0L), 0L)
  expect_false(grepl("Plot unavailable", inner, fixed = TRUE))

  # Pandoc wraps long heading lines, so compare on normalized whitespace.
  inner_text <- gsub("\\s+", " ", inner)
  expect_true(grepl("Methods and citations", inner_text, fixed = TRUE))

  # Rendering the report leaves the calling document's state as it found it.
  expect_identical(envir$caller_state$dev, "svg")
  expect_equal(envir$caller_state$fig_width, 3.1)
  expect_identical(envir$caller_state$comment, "#>")
  expect_true(envir$caller_state$duplicate_label_restored)
  expect_true(file.exists(outer_html))
})


# Word output (#35) ------------------------------------------------------------

test_that("the output format follows the file extension", {
  expect_null(nomologR:::nomo_report_output_format("report.html"))
  expect_null(nomologR:::nomo_report_output_format("report.HTM"))

  word <- nomologR:::nomo_report_output_format("report.docx")
  expect_s3_class(word, "rmarkdown_output_format")
  expect_identical(word$pandoc$to, "docx")
})


test_that("an unsupported extension names both supported formats", {
  run <- make_m9_minimal_run()
  expect_error(nomo_report(run, file = "report.pdf"), "HTML or Word")
  expect_error(nomo_report(run, file = "report.pdf"), ".docx", fixed = TRUE)
})


test_that("a Word report keeps every table and the interpretation contract", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  run <- make_m9_report_run()
  dir <- tempfile("nomo-word-report-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  docx <- nomo_report(run, file = file.path(dir, "report.docx"),
                      include_session = FALSE, quiet = TRUE)
  html <- nomo_report(run, file = file.path(dir, "report.html"),
                      include_session = FALSE, quiet = TRUE)
  expect_true(file.exists(docx))

  unz <- file.path(dir, "unz")
  utils::unzip(docx, exdir = unz)
  xml <- paste(readLines(file.path(unz, "word", "document.xml"), warn = FALSE,
                         encoding = "UTF-8"), collapse = "\n")
  text <- gsub("<[^>]+>", "", xml)

  # Pandoc drops raw HTML when writing Word, so a template that wrote HTML
  # tables would produce a report with none. Count the data tables in the HTML
  # report, leaving out the embedded scripts, which contain "<table" strings.
  body <- paste(readLines(html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  body <- gsub("(?s)<script[^>]*>.*?</script>", "", body, perl = TRUE)
  n_html <- lengths(regmatches(body, gregexpr("<table", body, fixed = TRUE)))
  n_word <- lengths(regmatches(xml, gregexpr("<w:tbl>", xml, fixed = TRUE)))
  expect_gt(n_word, 0L)
  expect_identical(n_word, n_html)

  # The contract is the report's statement of what it does not do; losing it in
  # Word would remove the package's core disclaimer from the document.
  expect_true(grepl("Interpretation contract", text, fixed = TRUE))
  expect_true(grepl("Methods and citations", text, fixed = TRUE))

  # Nothing HTML-specific reaches the Word text.
  expect_false(grepl("&amp;amp;", xml, fixed = TRUE))
  expect_false(grepl("&lt;div", xml, fixed = TRUE))
  expect_false(grepl("&lt;details", xml, fixed = TRUE))
})


test_that("the HTML report is unchanged by Word support", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  run <- make_m9_report_run()
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file), add = TRUE)
  nomo_report(run, file = file, include_session = FALSE, quiet = TRUE)
  html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_match(html, '<div class="table-responsive">', fixed = TRUE)
  expect_match(html, 'class="nomo-banner"', fixed = TRUE)
  expect_match(html, "<details>", fixed = TRUE)
  expect_match(html, "Show full evidence trace", fixed = TRUE)
})
