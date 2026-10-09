# The articles (#144, #145) ------------------------------------------------------
#
# Source tests. They read vignettes/ from the source tree, where the articles
# are written, and are skipped where it is absent, as under R CMD check.

vignette_files <- function() {
  dir <- testthat::test_path("..", "..", "vignettes")
  skip_if_not(dir.exists(dir), "the vignette sources are not available")
  files <- list.files(dir, pattern = "[.]Rmd$", full.names = TRUE)
  stats::setNames(files, basename(files))
}

vignette_lines <- function(name) {
  readLines(vignette_files()[[name]], warn = FALSE, encoding = "UTF-8")
}

# The prose of an article: everything outside its fenced code, as one string.
vignette_prose <- function(name) {
  lines <- vignette_lines(name)
  fence <- grepl("^```", lines)
  paste(lines[cumsum(fence) %% 2L == 0L & !fence], collapse = " ")
}

# The code of the chunks an article runs: fenced `{r}` chunks without
# `eval = FALSE`. A plain "```r" block is shown, not run.
vignette_evaluated_code <- function(name) {
  lines <- vignette_lines(name)
  fence <- grep("^```", lines)
  open <- fence[seq(1L, length(fence), by = 2L)]
  close <- fence[seq(2L, length(fence), by = 2L)]
  runs <- grepl("^```\\{r", lines[open]) & !grepl("eval\\s*=\\s*FALSE", lines[open])
  unlist(lapply(which(runs), function(i) {
    lines[seq_len(close[[i]] - open[[i]] - 1L) + open[[i]]]
  }))
}


test_that("the articles use the shared wording (#144)", {
  for (name in names(vignette_files())) {
    prose <- vignette_prose(name)
    # "p value", "a priori", and "post hoc" take no hyphen; no all-caps verdicts.
    expect_false(grepl("p-value", prose, ignore.case = TRUE), label = paste(name, "p-value"))
    expect_false(grepl("post-hoc|a-priori", prose, ignore.case = TRUE),
                 label = paste(name, "post-hoc or a-priori"))
    expect_false(grepl("PASS/FAIL", prose, fixed = TRUE), label = paste(name, "PASS/FAIL"))
    # The invariance and comparison output says "CFI change", not "delta-CFI"
    # (handed over by the invariance batch).
    expect_false(grepl("delta-(CFI|RMSEA|SRMR)", prose), label = paste(name, "delta-CFI"))
    # Three or more authors are cited with "et al." (APA 7), as the registry's
    # short citations are: no in-text citation lists three surnames.
    expect_false(
      grepl("[A-Z][[:alpha:]-]+, [A-Z][[:alpha:]-]+,( and| &) [A-Z][[:alpha:]-]+,? [(]?[0-9]{4}",
            prose),
      label = paste(name, "three authors named in the text")
    )
  }
})


test_that("an article with a reference list lists every work it cites (#145, lit-13)", {
  for (name in c("hierarchical-models.Rmd", "scoring.Rmd")) {
    lines <- vignette_lines(name)
    heading <- grep("^#+ References\\s*$", lines)
    expect_length(heading, 1L)
    fence <- grepl("^```", lines)
    in_text <- lines[seq_len(heading - 1L)]
    in_text <- in_text[(cumsum(fence) %% 2L == 0L & !fence)[seq_len(heading - 1L)]]
    body <- paste(in_text, collapse = " ")
    references <- paste(lines[-seq_len(heading)], collapse = " ")

    cited <- regmatches(body, gregexpr(
      "[A-Z][[:alpha:]-]+( et al[.]| (and|&) [A-Z][[:alpha:]-]+)?('s)?,? [(]?[0-9]{4}",
      body
    ))[[1L]]
    expect_gt(length(cited), 3L)
    for (citation in unique(cited)) {
      surname <- sub("^([A-Z][[:alpha:]-]+).*$", "\\1", citation)
      year <- sub("^.*([0-9]{4})$", "\\1", citation)
      listed <- grepl(paste0(surname, ", [^()]*[(]", year, "[)]"), references)
      expect_true(listed, label = paste0(name, ": ", surname, " (", year, ") in the reference list"))
    }
  }
})


test_that("the articles report their sources as the sources say (#145)", {
  network <- vignette_prose("nomological-network.Rmd")
  # lit-17: Bagozzi and Heatherton named the model; their paper is not
  # marketing research.
  expect_match(network, "Bagozzi and Heatherton (1994) called it", fixed = TRUE)
  expect_false(grepl("marketing research calls it", network, fixed = TRUE))
  # lit-1: an omega from the same sample is Savalei's data-estimated variant.
  expect_match(network, "data-estimated variant", fixed = TRUE)

  basis <- vignette_prose("research-basis.Rmd")
  # lit-18: Koo and Li recommend the model and the definition; single or mean
  # depends on the use.
  expect_match(basis, "two-way mixed-effects model with absolute agreement", fixed = TRUE)
  expect_false(grepl("two-way, absolute-agreement, single-measurement form", basis, fixed = TRUE))
  # lit-9 and lit-11: the reference list matches the registry.
  expect_match(basis, "Organizational Research Methods, 25*(1), 6\u201347.", fixed = TRUE)
  expect_match(basis, "In R. D. H. Heijmans, D. S. G. Pollock, & A. Satorra (Eds.)", fixed = TRUE)
  expect_match(basis, "Hu, L.-T., & Bentler, P. M. (1999)", fixed = TRUE)

  hierarchical <- vignette_prose("hierarchical-models.Rmd")
  # lit-16: H is the squared determinacy for a unidimensional construct.
  expect_match(hierarchical, "H equals the squared determinacy", fixed = TRUE)

  invariance <- vignette_prose("measurement-invariance.Rmd")
  expect_match(invariance, "CFI change, RMSEA change, SRMR change", fixed = TRUE)
})


test_that("no article writes a file outside the temporary directory", {
  # CRAN policy: examples, vignettes, and tests write only under tempdir().
  writers <- "\\b(saveRDS|save|write[.]csv|write[.]table|writeLines|ggsave|sink|pdf|png|download[.]file|file[.]create)[(]"
  for (name in names(vignette_files())) {
    code <- vignette_evaluated_code(name)
    expect_false(any(grepl(writers, code)), label = paste(name, "calls a file writer"))
    # A report, or any other `file =`, goes to a path built in code, never to
    # a name typed in, which would land in the working directory.
    expect_false(any(grepl("file\\s*=\\s*[\"']", code)),
                 label = paste(name, "writes to a literal path"))
    if (any(grepl("nomo_report[(]|dir[.]create[(]", code))) {
      expect_true(any(grepl("<- tempfile[(]|tempdir[(][)]", code)),
                  label = paste(name, "builds its output path under tempdir()"))
    }
  }
})
