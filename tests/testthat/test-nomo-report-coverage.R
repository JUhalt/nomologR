
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
