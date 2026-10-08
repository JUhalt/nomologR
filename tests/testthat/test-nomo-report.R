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

  # `file` is required, so the checks that come after it need a path. None of
  # these calls gets as far as writing it.
  file <- tempfile(fileext = ".html")

  expect_error(
    nomo_report(run, file = file, title = ""),
    "title"
  )

  expect_error(
    nomo_report(run, file = file, max_table_rows = 0),
    "positive integer"
  )

  expect_error(
    nomo_report(run, file = file, include_plots = NA),
    "include_plots"
  )

  expect_false(file.exists(file))
})


test_that("nomo_report() has no default path and writes only where asked", {
  run <- make_m9_minimal_run()

  # No default: the formal is empty, where it used to be a file name in the
  # working directory.
  expect_identical(deparse(formals(nomo_report)$file), "")

  scratch <- tempfile("nomo-report-wd-")
  dir.create(scratch)
  report_without_file <- function() {
    old <- setwd(scratch)
    on.exit(setwd(old), add = TRUE)
    nomo_report(run, include_plots = FALSE, include_session = FALSE)
  }

  # The error names the argument and suggests a path, and nothing is written
  # to the working directory.
  expect_error(report_without_file(), "`file` is required", fixed = TRUE)
  expect_error(report_without_file(), "no default path", fixed = TRUE)
  expect_error(report_without_file(), "file = \"report.html\"", fixed = TRUE)
  expect_length(list.files(scratch), 0L)
  unlink(scratch, recursive = TRUE)

  # A path given by position is still a path given.
  expect_error(nomo_report(run, "report.pdf"), "HTML or Word")
  # An object that is not a workflow is reported before the missing path.
  expect_error(nomo_report(list()), "nomo_run")
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


test_that("report tables read as words, with the shared flags and a dash for blanks (#89, #144)", {
  labels <- nomologR:::nomo_report_column_labels
  expect_identical(
    labels(c("omega_ci_lower", "delta_cfi", "pct_missing", "p_value", "df", "n_items",
             "median_interitem_r", "sepc.all", "attention")),
    c("Omega CI lower", "Change in CFI", "% missing", "p", "df", "Number of items",
      "Median inter-item r", "Standardized EPC (all)", "Flag")
  )
  # Names that are not the package's own are left as written: a capital letter
  # (a factor), a single word with a digit (an item), or a protected name.
  expect_identical(labels(c("F1", "AVE", "ag1", "anxiety"), protected = "anxiety"),
                   c("F1", "AVE", "ag1", "anxiety"))

  tab <- tibble::tibble(
    item = c("ag1", "ag2", "ag3"),
    attention = c("KEEP", "REVIEW", "STRONG REVIEW"),
    loading = c(.8, NA, .3),
    note = c("", NA, "low"),
    converged = c(TRUE, FALSE, NA)
  )
  shown <- nomologR:::nomo_report_display_table(tab)
  dash <- nomologR:::nomo_report_blank
  expect_identical(names(shown), c("Item", "Flag", "Loading", "Note", "Converged"))
  # Flags in sentence case, as in a printed table; no flag is a blank cell.
  expect_identical(shown$Flag, c("", "Review", "Concern"))
  expect_identical(shown$Note, c(dash, dash, "low"))
  expect_identical(shown$Converged, c("yes", "no", dash))
  # Numbers are rounded for display by their kind, with the dash for missing.
  expect_identical(shown$Loading, c("0.80", dash, "0.30"))
  expect_identical(attr(shown, "role"), c("text", "status", "numeric", "text", "text"))
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
  expect_true(any(grepl("post hoc", devs$type)))
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
  # Punctuation is written as character references, which pandoc reads back.
  expect_match(out, 'title: "A &#34;quoted&#34; report title"', fixed = TRUE)
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
  expect_identical(
    nomologR:::nomo_report_sanitize_citation_text("_\\texttt{semTools}: Useful tools_"),
    "_semTools: Useful tools_"
  )

  expect_equal(
    nomologR:::nomo_report_sanitize_citation_text(character()),
    character()
  )
})


test_that("the citations table gives each reference without R's header or BibTeX (#89)", {
  entries <- c(
    utils::bibentry("Manual", title = "examplepkg: An Example", author = "Ann Author",
                    year = "2026", note = "R package version 1.2.3",
                    url = "https://example.org/examplepkg"),
    utils::bibentry("Article", title = "An example article", author = "Ann Author",
                    journal = "Journal of Examples", year = "2025", volume = "4", pages = "1--9")
  )
  cites <- nomologR:::nomo_report_citations(
    packages = "examplepkg",
    namespace_available = function(pkg) TRUE,
    citation_fun = function(pkg) entries,
    version_fun = function(pkg) "1.2.3"
  )
  text <- cites$citation[[1L]]
  expect_match(text, "examplepkg: An Example", fixed = TRUE)
  expect_match(text, "An example article", fixed = TRUE)
  expect_match(text, "https://example.org/examplepkg", fixed = TRUE)
  expect_false(grepl("@Manual|@Article|BibTeX|To cite", text))
  expect_false(grepl("\n", text, fixed = TRUE))
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
  expect_true(any(dev$type == "post hoc nomological relation"))
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
  # The directory is created after the rmarkdown, knitr, and pandoc checks.
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())
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
  # nomo_report_pandoc_available() asks rmarkdown, a suggested package.
  skip_if_not_installed("rmarkdown")
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
  # A Word format comes from rmarkdown, a suggested package.
  skip_if_not_installed("rmarkdown")
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


test_that("each scale keeps its heading after the plot before it (#89)", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  # Two scales, paused before EFA: the item audit and factor retention each
  # draw a plot per scale, and careless responding follows the last one.
  set.seed(8901L)
  n <- 240L
  f <- rnorm(n)
  g <- .4 * f + sqrt(1 - .4^2) * rnorm(n)
  dat <- data.frame(
    a1 = .80 * f + rnorm(n, sd = .60), a2 = .75 * f + rnorm(n, sd = .65),
    a3 = .70 * f + rnorm(n, sd = .70), b1 = .80 * g + rnorm(n, sd = .60),
    b2 = .75 * g + rnorm(n, sd = .65), b3 = .70 * g + rnorm(n, sd = .70)
  )
  run <- nomo_run(
    dat,
    scales = list(Alpha = c("a1", "a2", "a3"), Beta = c("b1", "b2", "b3")),
    settings = list(
      factors = list(criterion_set = "minimal", n_iter = 10L, seed = 2026L),
      screen = list(effort = TRUE)
    )
  )

  dir <- tempfile("nomo-headings-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  html_file <- nomo_report(run, file = file.path(dir, "report.html"),
                           include_session = FALSE, quiet = TRUE)
  html <- paste(readLines(html_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Pandoc numbers repeated heading IDs, so the second section's scale
  # headings carry a "-1" suffix.
  # grepl() rather than expect_match(), whose failure message would print the
  # whole self-contained report.
  for (id in c("scale-alpha", "scale-beta", "scale-alpha-1", "scale-beta-1",
               "careless-responding")) {
    expect_true(grepl(sprintf('id="%s"', id), html, fixed = TRUE), label = id)
  }
  expect_false(grepl("## Scale:", html, fixed = TRUE))
  expect_false(grepl("## Careless", html, fixed = TRUE))
  # Table headings read as words, and a blank cell is a dash that pandoc does
  # not take for a list item, which would break the table (#89).
  expect_true(grepl(">\\s*Corrected item-rest r\\s*<", html))
  expect_false(grepl("<td[^>]*>\\s*NA\\s*</td>", html))
  expect_true(grepl(paste0("<td[^>]*>\\s*", nomologR:::nomo_report_blank, "\\s*</td>"), html))
  expect_false(grepl("<td[^>]*>\\s*<ul>", html))

  docx <- nomo_report(run, file = file.path(dir, "report.docx"),
                      include_session = FALSE, quiet = TRUE)
  unz <- file.path(dir, "unz")
  utils::unzip(docx, exdir = unz)
  xml <- paste(readLines(file.path(unz, "word", "document.xml"), warn = FALSE,
                         encoding = "UTF-8"), collapse = "\n")
  text <- gsub("<[^>]+>", "", xml)
  expect_false(grepl("## Scale:", text, fixed = TRUE))
  paragraphs <- strsplit(xml, "</w:p>", fixed = TRUE)[[1]]
  beta <- paragraphs[grepl("Scale: Beta", paragraphs, fixed = TRUE)]
  expect_gt(length(beta), 0L)
  expect_true(all(grepl('w:val="Heading2"', beta, fixed = TRUE)))
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


# Content defects and the shared style (#144, #145) ----------------------------

test_that("component summaries are printed at 80 columns whatever the console (#144)", {
  run <- make_m9_report_run()
  old <- options(width = 200L)
  on.exit(options(old), add = TRUE)
  txt <- nomologR:::nomo_report_component_summary_text(run$results$cfa)
  expect_true(all(nchar(strsplit(txt, "\n", fixed = TRUE)[[1L]], type = "width") <= 80L))
  expect_equal(getOption("width"), 200L)
})


test_that("report cells are escaped so pandoc shows them as written (#145)", {
  escape <- nomologR:::nomo_report_escape
  syntax <- "F =~ a*i1 + a*i2\ni1 ~~ i2"

  # Character references, which pandoc reads as the characters they stand for
  # and never as markup: `~~` is not a subscript, `*` not emphasis, and a quote
  # is not made typographic.
  expect_identical(escape(syntax, "html"),
                   "F =&#126; a&#42;i1 + a&#42;i2<br>i1 &#126;&#126; i2")
  # A Word table cell holds one line; "; " keeps the lavaan syntax valid.
  expect_identical(escape(syntax, "markdown"),
                   "F =&#126; a&#42;i1 + a&#42;i2; i1 &#126;&#126; i2")
  expect_identical(escape('method = "sum", it\'s', "html"),
                   "method = &#34;sum&#34;, it&#39;s")
  # The characters HTML and pandoc's TeX math would read; "\[" would open math.
  expect_identical(escape("C:\\Users <b>x</b> & [a]", "markdown"),
                   "C:&#92;Users &#60;b&#62;x&#60;/b&#62; &#38; &#91;a&#93;")
  # Dashes, an ellipsis, and a leading list marker stay as typed.
  expect_identical(escape("x -- y ... z", "html"), "x &#45;&#45; y &#46;&#46;&#46; z")
  expect_identical(escape(c("- a", "+ b", "1. c", "-0.5"), "html"),
                   c("&#45; a", "&#43; b", "1&#46; c", "-0.5"))
  # R names in backticks stay code, as the console writes them.
  expect_identical(escape("Scale `Agency` has x_y", "html"), "Scale `Agency` has x&#95;y")
  expect_identical(escape("`a_b` then c|d", "markdown"), "`a_b` then c&#124;d")
  expect_identical(escape(c(NA, ""), "html"), c(NA, ""))
})


test_that("report tables write escaped cells, keep trusted markdown, and note what they leave out", {
  skip_if_not_installed("knitr")
  tab <- tibble::tibble(
    id = c("cfa_model", "sample_design", "x"),
    decision = c("F =~ a*i1 + a*i2\ni1 ~~ i2", "same_sample", "x"),
    rationale = c("", NA, ""),
    severity = c("info", "review", "concern")
  )

  html <- nomologR:::nomo_report_table(tab)
  expect_match(html$markup, "F =&#126; a&#42;i1 + a&#42;i2<br>i1 &#126;&#126; i2", fixed = TRUE)
  expect_match(html$markup, 'class="table table-striped table-condensed nomo-table"', fixed = TRUE)
  # A column with no value in any row is left out, and the note names it; the
  # flag column stays even where it is blank.
  expect_false(grepl("Rationale", html$markup, fixed = TRUE))
  expect_identical(html$notes, "Columns with no value in any row are left out: Rationale.")
  expect_identical(html$labels, c("ID", "Decision", "Flag"))
  expect_true(all(c("Review", "Concern", "same_sample") %in% html$cells))

  word <- nomologR:::nomo_report_table(tab, word = TRUE, max_rows = 2)
  expect_match(word$markup, "|F =&#126; a&#42;i1 + a&#42;i2; i1 &#126;&#126; i2", fixed = TRUE)
  expect_identical(
    word$notes[[2L]],
    "Showing 2 of 3 rows. The underlying nomo_run object retains all rows."
  )

  # Numbers are right-aligned.
  numbers <- nomologR:::nomo_report_table(tibble::tibble(item = "a", p_value = 0))
  expect_match(numbers$markup, 'text-align:right;">\\s*&#60; .001', perl = TRUE)

  # Trusted markdown, such as R's citations, keeps its italics.
  cite <- tibble::tibble(citation = "Rosseel Y (2012). _lavaan_ <x> & *48*|1.")
  kept <- nomologR:::nomo_report_table(cite, markdown = TRUE)
  expect_match(kept$markup, "_lavaan_ &lt;x&gt; &amp; *48*|1.", fixed = TRUE)
  kept_word <- nomologR:::nomo_report_table(cite, word = TRUE, markdown = TRUE)
  # kable writes the bar as a character reference, so it cannot end the cell.
  expect_match(kept_word$markup, "_lavaan_ <x> & *48*&#124;1.", fixed = TRUE)

  expect_null(nomologR:::nomo_report_table(tibble::tibble()))
  expect_null(nomologR:::nomo_report_table(tibble::tibble(a = character())))
})


test_that("report numbers follow the console's kinds: p, percentages, counts, and statistics (#144, #145)", {
  shown <- nomologR:::nomo_report_display_table(tibble::tibble(
    item = c("a2", "a3"),
    n = c(500L, 500L),
    n_used = c(800, 799),
    pct_missing = c(0.03, 0.1234),
    percent_unique = c(56.24, 3),
    mode_prop = c(0.0112, 0.5),
    mean = c(4.1134, 4),
    p_value = c(0, 1),
    lrt_p = c(0.0432, NA),
    df = c(41, 33.471),
    cfi = c(0.9957, 0.9),
    rmsea = c(0.0198, 0.08),
    latent_r = c(0.4574, -0.002),
    omega = c(0.849, 0.7),
    loading = c(0.822, 1.04),
    HTMT2 = c(0.4527, 1.01),
    chi_square = c(53.909, 3),
    complexity = c(1, 1)
  ))
  dash <- nomologR:::nomo_report_blank
  # A proportion stored under `pct` is shown as the percentage its heading says.
  expect_identical(names(shown)[[4L]], "% missing")
  expect_identical(shown[["% missing"]], c("3.0", "12.3"))
  expect_identical(shown[["Percent unique"]], c("56.2", "3.0"))
  # p values in APA style, never 0 or 1.000.
  expect_identical(shown$p, c("< .001", "> .999"))
  expect_identical(shown[["LRT p"]], c(".043", dash))
  # Counts and degrees of freedom are whole numbers.
  expect_identical(shown$N, c("500", "500"))
  expect_identical(shown[["N used"]], c("800", "799"))
  expect_identical(shown$df, c("41", "33.47"))
  # An estimate keeps its precision when it happens to be whole, so a quantity
  # reads the same in every table of the report.
  expect_identical(shown$Complexity, c("1.00", "1.00"))
  # Leading zeros by bound, one precision per quantity.
  expect_identical(shown[["Mode proportion"]], c(".01", ".50"))
  expect_identical(shown$Mean, c("4.11", "4.00"))
  expect_identical(shown$CFI, c(".996", ".900"))
  expect_identical(shown$RMSEA, c("0.020", "0.080"))
  expect_identical(shown[["Latent r"]], c(".46", ".00"))
  expect_identical(shown$Omega, c(".85", ".70"))
  expect_identical(shown$Loading, c("0.82", "1.04"))
  expect_identical(shown$HTMT2, c("0.45", "1.01"))
  expect_identical(shown[["Chi-square"]], c("53.91", "3.00"))

  # A table of metric and value rows takes each row's kind from its metric, and
  # a value just off its reference shows the decimals that tell them apart.
  fit <- nomologR:::nomo_report_display_table(tibble::tibble(
    metric = c("chi_square", "df", "p_value", "CFI", "TLI", "KMO", "Bartlett", "pairs"),
    value = c(53.909, 41, 0.0853, 0.9957, 0.9496, 0.8206, 1e-200, 13),
    reference = c(NA, NA, NA, 0.95, 0.95, NA, NA, NA)
  ))
  expect_identical(fit$Value, c("53.91", "41", ".085", ".996", "0.9496", ".82", "< .001", "13"))
  expect_identical(fit$Reference, c(dash, dash, dash, ".950", "0.950", dash, dash, dash))

  expect_identical(
    nomologR:::nomo_report_number_kind(c("aic", "power", "lambda", "estimate", "", "omega_ci_n_success")),
    c("ic", "power", "loading", "estimate", "estimate", "count")
  )
})


test_that("report status and origin cells use the shared words (#144)", {
  skip_if_not_installed("knitr")
  shown <- nomologR:::nomo_report_display_table(tibble::tibble(
    stage = c("efa", "network"),
    status = c("awaiting_decision", NA),
    origin = c("a_priori", "post_hoc"),
    measurement_attention = c("unavailable", NA)
  ))
  expect_identical(shown$Status, c("Awaiting decision", ""))
  expect_identical(shown$Origin, c("A priori", "Post hoc"))
  expect_identical(shown[["Measurement flag"]], c("Not computed", ""))
  # A status column is kept when it is blank in every row.
  kept <- nomologR:::nomo_report_table(tibble::tibble(stage = "efa", severity = "info"))
  expect_identical(kept$labels, c("Stage", "Flag"))
})


test_that("the scale definitions table has one heading per column (#145)", {
  expect_identical(
    nomologR:::nomo_report_column_labels(c("scale", "n_items", "items")),
    c("Scale", "Number of items", "Items")
  )
})


test_that("a scale named Post... is not reported as a deviation (#145)", {
  run <- make_m9_minimal_run()
  run$decision_log <- tibble::tibble(
    id = c("scale_definition:PostpartumDepression", "factor_count:PartialScale",
           "revision", "post_decision:Agency"),
    stage = c("design", "efa", "workflow", "workflow"),
    scope = c("PostpartumDepression", "PartialScale", "measurement_model", "Agency"),
    decision = c("pp1, pp2", "1", "revised measurement model: F =~ a", "keep"),
    rationale = ""
  )
  devs <- nomologR:::nomo_report_deviations(run)
  # The package's own prefixes still count; the researcher's names do not.
  expect_identical(devs$detail, c("revised measurement model: F =~ a", "keep"))
  expect_identical(devs$scope, c("measurement_model", "Agency"))
})


test_that("the missing-data section says when the reference was not fitted (#145)", {
  strategies <- function(available) {
    tibble::tibble(
      strategy = c("listwise", "ml"), label = c("Listwise deletion", "FIML"),
      lavaan_missing = c("listwise", NA), requires = c("MCAR", "MAR"),
      role = c("comparison", "reference"), available = available,
      n_used = c(800, NA), converged = c(TRUE, NA), admissible = c(TRUE, NA)
    )
  }
  m <- list(
    pattern = list(n_incomplete = 0L, n_cases = 800L, pct_incomplete = 0),
    strategies = strategies(c(TRUE, FALSE)),
    estimates = tibble::tibble(parameter = character(), strategy = character(),
                               role = character(), estimate = numeric(),
                               reference_estimate = numeric(),
                               difference_in_se = numeric()),
    reference = "ml", fit = tibble::tibble(), reliability = NULL,
    decision_log = tibble::tibble()
  )
  complete <- nomologR:::nomo_report_missing(m)
  expect_match(complete$summary, "None of the 800 cases is missing", fixed = TRUE)
  expect_match(complete$summary, "the reference strategy (FIML) was not fitted", fixed = TRUE)
  expect_false(grepl("The reference is FIML", complete$summary, fixed = TRUE))
  expect_identical(complete$differences_empty,
                   "The reference strategy was not fitted, so there are no differences to show.")

  m$pattern <- list(n_incomplete = 15L, n_cases = 500L, pct_incomplete = 0.03)
  failed <- nomologR:::nomo_report_missing(m)
  expect_match(failed$summary, "15 of 500 cases (3.0%) are missing", fixed = TRUE)
  expect_match(failed$summary, "The reference strategy (FIML) could not be fitted.", fixed = TRUE)

  m$strategies <- strategies(c(TRUE, TRUE))
  fitted <- nomologR:::nomo_report_missing(m)
  expect_match(fitted$summary, "The reference is FIML.", fixed = TRUE)
  expect_identical(fitted$differences_empty, "No comparison strategy was fitted.")
})


test_that("the scores summary writes the parallel-model test in APA form (#144)", {
  scores <- list(
    weighting = "unit", method = "sum",
    scores = tibble::tibble(a = 1:3), diagnostics = tibble::tibble(factor = "A"),
    notes = tibble::tibble(),
    parallel_test = list(available = TRUE, chisq_diff = 41.934, df_diff = 16,
                         p_value = 0.0004, note = "")
  )
  text <- nomologR:::nomo_report_scores(scores)$summary
  expect_match(text, "fitted model: Delta chi-square(16) = 41.93, p < .001.", fixed = TRUE)
  scores$parallel_test <- list(available = FALSE, note = "Not tested: one item.")
  expect_match(nomologR:::nomo_report_scores(scores)$summary, "Not tested: one item.", fixed = TRUE)
})


test_that("the careless-responding section keeps the rows saying why an index has no value (#145)", {
  # No `scales`, and missing values: no per-scale means, and a Mahalanobis
  # distance only for the complete cases.
  screen <- nomo_screen(nomo_demo_continuous, effort = TRUE)
  expect_true(all(c("mahalanobis", "per_scale_indices") %in% screen$decision_log$metric))
  log <- nomologR:::nomo_report_effort(screen)$log
  expect_true(all(c("mahalanobis", "per_scale_indices", "long_string") %in% log$metric))
  # Item rows are not careless-responding evidence.
  expect_true(all(log$object == "cases"))
})


test_that("review and concern rows are listed concern first (#144)", {
  log <- tibble::tibble(
    object = c("a", "b", "c", "d"),
    severity = c("review", "info", "concern", "review")
  )
  flagged <- nomologR:::nomo_report_flagged_rows(log)
  expect_identical(flagged$object, c("c", "a", "d"))
  expect_identical(nrow(nomologR:::nomo_report_flagged_rows(tibble::tibble())), 0L)
  no_severity <- tibble::tibble(object = "a")
  expect_identical(nomologR:::nomo_report_flagged_rows(no_severity), no_severity)
})


test_that("the call history is R code with straight quotes (#145)", {
  run <- make_m9_minimal_run()
  code <- nomologR:::nomo_report_call_code(run)
  expect_identical(code, 'nomo_run(data = dat, scales = list(S = c("i1", "i2")))')

  run$call_history <- list(run$call, quote(nomo_run(resume = run, mode = "research")))
  two <- nomologR:::nomo_report_call_code(run)
  expect_match(two, "^# Step 1\nnomo_run\\(data = dat")
  expect_match(two, '\n\n# Step 2\nnomo_run(resume = run, mode = "research")', fixed = TRUE)

  run$call_history <- NULL
  run$call <- NULL
  expect_identical(nomologR:::nomo_report_call_code(run), "")
})


test_that("the report title is written so the render shows it as given (#145)", {
  yaml <- nomologR:::nomo_report_title_yaml('50% \\ done `r 1+1` <b>x</b> "C:\\Users"')
  # No backtick for knitr, no quote or backslash for YAML, no markup for pandoc.
  expect_false(grepl("[`\\\\<]", substring(yaml, 2L, nchar(yaml) - 1L)))
  expect_identical(
    yaml,
    '"50&#37; &#92; done &#96;r 1&#43;1&#96; &#60;b&#62;x&#60;&#47;b&#62; &#34;C&#58;&#92;Users&#34;"'
  )
  expect_identical(nomologR:::nomo_report_title_yaml("Plain title"), '"Plain title"')
})


test_that("software citations give each DOI once, as a working address (#145)", {
  clean <- nomologR:::nomo_report_sanitize_citation_text(c(
    "Rosseel Y (2012). _JSS_, *48*(2). doi:10.18637/jss.v048.i02 https://doi.org/10.18637/jss.v048.i02.",
    "Steiner M (2020). JOSS. doi:10.21105/joss.02521 https://doi.org/10.21105/joss.02521. https://doi.org/10.21105/joss.02521.",
    "Only a DOI (2020). doi:10.1/abc."
  ))
  expect_identical(clean, c(
    "Rosseel Y (2012). _JSS_, *48*(2). https://doi.org/10.18637/jss.v048.i02.",
    "Steiner M (2020). JOSS. https://doi.org/10.21105/joss.02521.",
    "Only a DOI (2020). https://doi.org/10.1/abc."
  ))
})


test_that("every abbreviation the report shows is defined once at its end (#144)", {
  abbr <- nomologR:::nomo_report_abbreviations(
    labels = c("Factor", "Loading", "SE", "p", "CI lower", "Number of items"),
    text = c("CFI", "FIML and MCAR", "ag1", "p < .001", "N = 40")
  )
  expect_identical(abbr$abbreviation, c("CFI", "CI", "FIML", "MCAR", "SE", "p"))
  expect_identical(abbr$meaning[abbr$abbreviation == "p"], "p value")
  expect_identical(nrow(nomologR:::nomo_report_abbreviations()), 0L)

  note <- nomologR:::nomo_report_reading_note()
  expect_match(note, nomologR:::nomo_report_blank, fixed = TRUE)
  expect_match(note, "blank for no flag", fixed = TRUE)
  expect_match(note, "never an instruction to delete", fixed = TRUE)
})


test_that("a rendered report keeps model syntax, quotes, and the title as written (#145)", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  run <- make_m9_minimal_run()
  run$decision_log <- dplyr::bind_rows(run$decision_log, tibble::tibble(
    id = "cfa_model", stage = "cfa", scope = "measurement_model",
    observation = "Model supplied.", reason = "", options = "", consequence = "",
    decision = "F =~ a*i1 + a*i2\ni1 ~~ i2",
    rationale = "Equal loadings, and a residual covariance from item wording.",
    source = "researcher_decision"
  ))
  title <- 'Scale "A" -- C:\\Users\\data & <b>x</b>'
  dir <- tempfile("nomo-escape-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  html_file <- nomo_report(run, file = file.path(dir, "report.html"), title = title,
                           include_plots = FALSE, include_session = FALSE, quiet = TRUE)
  html <- paste(readLines(html_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- gsub("(?s)<script[^>]*>.*?</script>", "", html, perl = TRUE)
  # grepl() rather than expect_match(), whose failure message would print the
  # whole self-contained report.
  expect_true(grepl("F =~ a*i1 + a*i2<br>i1 ~~ i2", html, fixed = TRUE))
  expect_false(grepl("<sub></sub>", html, fixed = TRUE))
  expect_false(grepl("a<em>i1", html, fixed = TRUE))
  # The call is code with straight quotes, not a table cell with curly ones.
  expect_true(grepl(
    "<pre><code>nomo_run(data = dat, scales = list(S = c(&quot;i1&quot;, &quot;i2&quot;)))",
    html, fixed = TRUE
  ))
  expect_false(grepl(paste0("c(", intToUtf8(0x201c), "i1"), html, fixed = TRUE))
  expect_true(grepl(
    "<title>Scale &quot;A&quot; -- C:\\Users\\data &amp; &lt;b&gt;x&lt;/b&gt;</title>",
    html, fixed = TRUE
  ))
  expect_true(grepl('id="abbreviations"', html, fixed = TRUE))
  # The facts under the title, each on its line, with the status in sentence
  # case and the design in words (#144).
  expect_true(grepl(
    "<strong>Workflow status:</strong> Paused<br /> <strong>Sample design:</strong> same sample",
    gsub("\\s+", " ", html), fixed = TRUE
  ))

  docx <- nomo_report(run, file = file.path(dir, "report.docx"), title = title,
                      include_plots = FALSE, include_session = FALSE, quiet = TRUE)
  unz <- file.path(dir, "unz")
  utils::unzip(docx, exdir = unz)
  xml <- paste(readLines(file.path(unz, "word", "document.xml"), warn = FALSE,
                         encoding = "UTF-8"), collapse = "")
  text <- gsub("<[^>]+>", "", xml)
  expect_true(grepl("F =~ a*i1 + a*i2; i1 ~~ i2", text, fixed = TRUE))
  expect_true(grepl("scales = list(S = c(&quot;i1&quot;, &quot;i2&quot;))", text, fixed = TRUE))
  expect_true(grepl("Scale &quot;A&quot; -- C:\\Users\\data &amp; &lt;b&gt;x&lt;/b&gt;", text,
                    fixed = TRUE))
})
