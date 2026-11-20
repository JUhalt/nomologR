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


# The release plan --------------------------------------------------------------

# The introduction of NEWS.md: what comes before its first second-level heading.
news_introduction <- function() {
  lines <- readLines(release_source_file("NEWS.md"), warn = FALSE, encoding = "UTF-8")
  paste(lines[seq_len(grep("^## ", lines)[[1L]] - 1L)], collapse = "\n")
}

# A document's text on one line, with typographic apostrophes as typed.
flat <- function(text) {
  gsub("\\s+", " ", gsub("\u2019", "'", text))
}

test_that("the release plan gives the dates and the order the maintainer set (#53)", {
  # Interface freeze 2026-11-27, release candidate 2026-12-01, and 1.0.0 on
  # GitHub and R-universe before the CRAN submission (decided 2026-11-16).
  plans <- list(
    "NEWS.md introduction" = news_introduction(),
    "README.Rmd" = release_text("README.Rmd"),
    "README.md" = release_text("README.md"),
    "ROADMAP.md" = release_text("ROADMAP.md")
  )
  for (name in names(plans)) {
    text <- flat(plans[[name]])
    expect_false(grepl("2026-10-1[37]", text), label = paste(name, "gives an old date"))
    expect_false(grepl("feature freeze", text), label = paste(name, "says feature freeze"))
    expect_match(text, "interface freeze", label = name)
    expect_match(text, "2026-11-27", fixed = TRUE, label = name)
    expect_match(text, "2026-12-01", fixed = TRUE, label = name)
    expect_match(text, "GitHub and R-universe first", fixed = TRUE, label = name)
  }
  # No CRAN interval comes before 1.0.0.
  roadmap <- flat(plans[["ROADMAP.md"]])
  expect_false(grepl("first acceptances|update interval", roadmap))

  for (name in c("vignettes/nomologR.Rmd", "cran-comments.md")) {
    expect_false(grepl("2026-10-1[37]", release_text(name)), label = paste(name, "gives an old date"))
  }
})


test_that("Get started describes 1.0.0 in words that stay true after it is out (#53)", {
  # The article is installed with the package, so it must not call 1.0.0 the
  # next release, and nothing in it has to be remembered on release day.
  lines <- readLines(release_source_file("vignettes", "nomologR.Rmd"), warn = FALSE,
                     encoding = "UTF-8")
  start <- grep("^# Where nomologR is heading", lines)
  expect_length(start, 1L)
  end <- start + grep("^# ", lines[-seq_len(start)])[[1L]]
  section <- flat(paste(lines[start:(end - 1L)], collapse = " "))
  expect_match(section, "From 1.0.0, the interfaces of both packages are stable", fixed = TRUE)
  expect_false(grepl("next release|planned|will |milestone", section))
  expect_false(grepl("v?[0-9]+[.][0-9]+[.][0-9]+", sub("1.0.0", "", section, fixed = TRUE)))
})


test_that("the README dates the stability promise as ?nomologR does (#113)", {
  help <- flat(release_text("man", "nomologR-package.Rd"))
  from <- regmatches(help, regexpr("From version [0-9.]+[0-9], the public interface", help))
  expect_length(from, 1L)
  version <- sub("From version ([0-9.]+[0-9]),.*$", "\\1", from)
  expect_identical(version, "1.0.0")
  for (name in c("README.Rmd", "README.md")) {
    readme <- flat(release_text(name))
    stability <- sub("^.*## Stability (.*?) ## Design principles.*$", "\\1", readme, perl = TRUE)
    expect_match(stability, paste0("^From `v", version, "`, nomologR's public interface"),
                 label = name)
    expect_false(grepl("v0[.]3[.]0", stability), label = paste(name, "names 0.3.0"))
  }
})
