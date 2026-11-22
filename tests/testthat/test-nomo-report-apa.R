# Manuscript tables appended to a report (#73) -----------------------------------

report_apa_run <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    scales <- list(Agency = paste0("ag", 1:4), Persistence = paste0("pe", 1:4))
    cache <<- nomo_run(
      nomo_demo_network, scales = scales, mode = "research",
      decisions = list(
        factor_count = list(value = c(Agency = 1, Persistence = 1), rationale = "x"),
        cfa_model = list(value = nomo_model(scales), rationale = "x"),
        measurement_model = list(value = "proceed", rationale = "x")
      ),
      settings = list(
        factors = list(seed = 73),
        network = list(hypotheses = nomo_hypotheses("Agency -> Persistence" = positive()))
      )
    )
    cache
  }
})


test_that("the tables follow the results the run holds, numbered in report order", {
  skip_on_cran()
  tables <- nomologR:::nomo_report_apa_tables(report_apa_run())

  expect_identical(vapply(tables, `[[`, integer(1), "number"), seq_along(tables))
  expect_identical(
    vapply(tables, `[[`, character(1), "title"),
    c("Standardized Factor Loadings", "Model Fit", "Reliability Estimates",
      "Average Variance Extracted",
      "Construct Correlations and Heterotrait-Monotrait Ratios",
      "Theory-Specified Relations", "Fit of the Nomological Network Model")
  )
  # Each is the table nomo_apa_table() gives for that result.
  expect_identical(tables[[1L]]$body,
                   nomo_apa_table(report_apa_run()$results$cfa, "loadings")$body)
  expect_identical(tables[[5L]]$body,
                   nomo_apa_table(report_apa_run()$results$validity)$body)
})


test_that("without a validity stage, the CFA's factor correlations give the pairs", {
  skip_on_cran()
  run <- report_apa_run()
  run$results$validity <- NULL
  titles <- vapply(nomologR:::nomo_report_apa_tables(run), `[[`, character(1), "title")
  expect_identical(
    titles,
    c("Standardized Factor Loadings", "Model Fit", "Factor Correlations",
      "Reliability Estimates", "Theory-Specified Relations",
      "Fit of the Nomological Network Model")
  )
})


test_that("a table that does not apply is left out rather than failing the report", {
  skip_on_cran()
  one <- nomo_run(
    nomo_demo_network, scales = list(Agency = paste0("ag", 1:4)), mode = "research",
    decisions = list(
      factor_count = list(value = c(Agency = 1), rationale = "x"),
      cfa_model = list(value = "Agency =~ ag1 + ag2 + ag3 + ag4", rationale = "x"),
      measurement_model = list(value = "proceed", rationale = "x")
    ),
    settings = list(factors = list(seed = 73))
  )
  titles <- vapply(nomologR:::nomo_report_apa_tables(one), `[[`, character(1), "title")
  expect_false("Factor Correlations" %in% titles)
  expect_true("Standardized Factor Loadings" %in% titles)

  paused <- nomo_run(nomo_demo_network, scales = list(Agency = paste0("ag", 1:4)),
                     settings = list(factors = list(seed = 73)))
  expect_length(nomologR:::nomo_report_apa_tables(paused), 0L)
})


test_that("apa_tables must be TRUE or FALSE", {
  # The argument is checked before the run's results are read, so the small
  # run the other report tests share is enough.
  expect_error(
    nomo_report(make_m9_minimal_run(), file = tempfile(fileext = ".html"),
                apa_tables = NA),
    "apa_tables"
  )
})


test_that("the appendix appears only when asked for", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  render <- function(apa_tables) {
    file <- nomo_report(report_apa_run(), file = tempfile(fileext = ".html"),
                        include_plots = FALSE, include_session = FALSE,
                        quiet = TRUE, apa_tables = apa_tables)
    gsub("[[:space:]]+", " ", paste(readLines(file, warn = FALSE), collapse = " "))
  }

  with_tables <- render(TRUE)
  expect_match(with_tables, 'id="manuscript-tables"', fixed = TRUE)
  expect_match(with_tables, "<strong>Table 1</strong>", fixed = TRUE)
  expect_match(with_tables, "Standardized Factor Loadings", fixed = TRUE)
  # The tables are the ones a knitted document shows, with the APA rules, not
  # the page's rule under every row (#145). The style sheet comes once.
  expect_match(with_tables, paste(
    '<div class="nomo-apa"> <p class="nomo-apa-number"><strong>Table 1</strong></p>',
    '<p class="nomo-apa-title"><em>Standardized Factor Loadings</em></p>'
  ), fixed = TRUE)
  expect_match(with_tables, "border-bottom: 1px solid currentColor !important;", fixed = TRUE)
  expect_identical(lengths(gregexpr(".nomo-apa thead th {", with_tables, fixed = TRUE)), 1L)
  expect_identical(lengths(gregexpr('<div class="nomo-apa">', with_tables, fixed = TRUE)),
                   length(nomologR:::nomo_report_apa_tables(report_apa_run())))

  expect_false(grepl('id="manuscript-tables"', render(FALSE), fixed = TRUE))
})


test_that("a Word report and a Word document ruled by the reference document have APA rules (#145)", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())
  dir <- withr::local_tempdir()

  # The "Table" style of a Word file, the style pandoc gives every table, and
  # the document's text. Pandoc writes a style's attributes in an order of its
  # own, so each element is matched by its attributes, not by its spelling.
  table_style <- function(docx) {
    unz <- tempfile(tmpdir = dir)
    utils::unzip(docx, exdir = unz)
    read <- function(part) {
      paste(readLines(file.path(unz, "word", part), warn = FALSE, encoding = "UTF-8"),
            collapse = " ")
    }
    styles <- read("styles.xml")
    table <- regmatches(styles, regexpr(
      '<w:style [^>]*w:styleId="Table"[^>]*>.*?</w:style>', styles, perl = TRUE
    ))
    list(table = gsub("[[:space:]]+", " ", table), document = read("document.xml"))
  }
  # A rule above and below the table and beneath the headings, and none
  # between rows or columns.
  expect_ruled <- function(docx) {
    style <- table_style(docx)
    expect_length(style$table, 1L)
    borders <- regmatches(style$table, regexpr("<w:tblBorders>.*?</w:tblBorders>",
                                               style$table, perl = TRUE))
    for (edge in c("top", "bottom")) {
      expect_match(borders, sprintf('<w:%s [^>]*w:val="single"', edge), label = edge)
      expect_match(borders, sprintf('<w:%s [^>]*w:sz="4"', edge), label = edge)
    }
    for (edge in c("left", "right", "insideH", "insideV")) {
      expect_match(borders, sprintf('<w:%s w:val="nil" />', edge), fixed = TRUE, label = edge)
    }
    expect_match(style$table, paste0(
      '<w:tblStylePr w:type="firstRow"> <w:tcPr> <w:tcBorders> <w:bottom [^>]*w:val="single"'
    ))
    # Each table takes that style, and no table sets borders of its own.
    expect_match(style$document, '<w:tblStyle w:val="Table" />', fixed = TRUE)
    expect_false(grepl("<w:tblBorders>", style$document, fixed = TRUE))
  }

  # The reference document itself, which is pandoc's default otherwise.
  reference <- nomologR:::nomo_apa_reference_docx()
  expect_true(file.exists(reference))
  expect_lt(file.size(reference), 20000)

  report <- nomo_report(report_apa_run(), file = file.path(dir, "report.docx"),
                        include_plots = FALSE, include_session = FALSE,
                        quiet = TRUE, apa_tables = TRUE)
  expect_ruled(report)
  expect_match(table_style(report)$document, "Standardized Factor Loadings", fixed = TRUE)

  # A researcher's own document names it as its reference document.
  rmd <- file.path(dir, "own.Rmd")
  writeLines(c(
    "---", "title: APA", "output:", "  word_document:",
    paste0("    reference_docx: !expr system.file(\"rmarkdown\", ",
           "\"nomo-apa-reference.docx\", package = \"nomologR\")"),
    "---", "",
    "```{r, echo = FALSE}",
    "nomologR::nomo_apa_table(nomologR::nomo_cfa('Agency =~ ag1 + ag2 + ag3 + ag4',",
    "  nomologR::nomo_demo_network), 'loadings', number = 1)",
    "```"
  ), rmd)
  own <- rmarkdown::render(rmd, quiet = TRUE, envir = new.env())
  expect_ruled(own)
  expect_match(table_style(own)$document, "Standardized Factor Loadings", fixed = TRUE)
})
