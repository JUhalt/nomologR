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
  tables <- nomologR:::nomo_report_apa_tables(report_apa_run())

  expect_identical(vapply(tables, `[[`, integer(1), "number"), seq_along(tables))
  expect_identical(
    vapply(tables, `[[`, character(1), "title"),
    c("Standardized Factor Loadings", "Model Fit", "Factor Correlations",
      "Reliability Estimates", "Theory-Specified Relations",
      "Fit of the Nomological Network Model")
  )
  # Each is the table nomo_apa_table() gives for that result.
  expect_identical(tables[[1L]]$body,
                   nomo_apa_table(report_apa_run()$results$cfa, "loadings")$body)
})


test_that("a table that does not apply is left out rather than failing the report", {
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
  expect_error(nomo_report(report_apa_run(), apa_tables = NA), "apa_tables")
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

  expect_false(grepl('id="manuscript-tables"', render(FALSE), fixed = TRUE))
})
