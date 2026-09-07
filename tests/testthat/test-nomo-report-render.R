test_that("nomo_report renders a polished self-contained HTML archive when Pandoc is available", {
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
  expect_false(grepl("%3E", html, fixed = TRUE))
})
