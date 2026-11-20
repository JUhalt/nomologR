# What the release documents say, held to this tree (#53, #145) ----------------
#
# cran-comments.md tells the CRAN reviewer how the package and its tests
# behave, and the README, ROADMAP, NEWS, and Get started state the release
# plan. These source tests check those statements against the code, the tests,
# and each other. The documents are left out of the built package, so the
# tests that read them are skipped where they are absent, as under R CMD check.

release_source_file <- function(...) {
  path <- testthat::test_path("..", "..", ...)
  skip_if_not(file.exists(path), paste(file.path(...), "is not available"))
  path
}

release_text <- function(...) {
  paste(readLines(release_source_file(...), warn = FALSE, encoding = "UTF-8"),
        collapse = "\n")
}


# The tests themselves ----------------------------------------------------------

test_that("every test that replaces another package's function is skipped on CRAN", {
  # cran-comments.md says so. A rebinding is a mocked binding whose `.package`
  # is not nomologR, or a call to a helper that makes one.
  skip_on_cran()
  files <- list.files(testthat::test_path(), pattern = "[.]R$", full.names = TRUE)
  expect_gt(length(files), 40L)
  deparsed <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = " ")
  rebinds_other <- function(text) {
    packages <- regmatches(text, gregexpr("[.]package = \"[^\"]+\"", text))[[1L]]
    any(packages != ".package = \"nomologR\"")
  }
  calls_any <- function(text, names) {
    any(vapply(names, function(name) grepl(paste0("\\b", name, "[(]"), text), logical(1)))
  }

  # Helpers defined at the top level of a test or helper file.
  skipping <- "skip_on_cran"
  rebinding <- character()
  blocks <- list()
  for (file in files) {
    for (expr in as.list(parse(file, keep.source = FALSE))) {
      if (!is.call(expr)) next
      head <- expr[[1L]]
      if ((identical(head, as.name("<-")) || identical(head, as.name("="))) &&
          is.name(expr[[2L]])) {
        body <- deparsed(expr[[3L]])
        if (grepl("skip_on_cran(", body, fixed = TRUE)) {
          skipping <- c(skipping, as.character(expr[[2L]]))
        }
        if (rebinds_other(body)) rebinding <- c(rebinding, as.character(expr[[2L]]))
      }
      if (identical(head, as.name("test_that"))) {
        blocks[[length(blocks) + 1L]] <- list(
          where = sprintf("%s: %s", basename(file), expr[[2L]]),
          text = deparsed(expr)
        )
      }
    }
  }
  expect_true("local_base_r_simulate_data" %in% rebinding)

  checked <- 0L
  for (block in blocks) {
    if (!rebinds_other(block$text) && !calls_any(block$text, rebinding)) next
    checked <- checked + 1L
    expect_true(calls_any(block$text, skipping),
                label = sprintf("'%s' is skipped on CRAN", block$where))
  }
  expect_gt(checked, 40L)
})
