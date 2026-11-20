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


# Continuous integration --------------------------------------------------------

# A workflow's text. The workflows are skipped only where the whole directory
# is absent; a workflow missing from it is a failure.
workflow_text <- function(name) {
  dir <- testthat::test_path("..", "..", ".github", "workflows")
  skip_if_not(dir.exists(dir), "the CI workflows are not available")
  path <- file.path(dir, name)
  expect_true(file.exists(path), label = paste(name, "exists"))
  if (!file.exists(path)) return("")
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

test_that("the coverage job fails when a line in R/ is not covered (#53)", {
  # The merge rule is 100% in every file of R/ under covr 4.0.0 or later. The
  # job reads covr's lines, each at its smallest count, not a rounded
  # percentage, and stops when one was never run.
  workflow <- workflow_text("test-coverage.yaml")
  expect_match(workflow, "extra-packages: covr@>=4.0.0", fixed = TRUE)
  expect_match(workflow, "if (utils::packageVersion(\"covr\") < \"4.0.0\") {", fixed = TRUE)
  expect_match(workflow, "covr::tally_coverage(cov, by = \"line\")", fixed = TRUE)
  expect_match(workflow, "startsWith(lines$filename, \"R/\")", fixed = TRUE)
  expect_match(workflow, "aggregate(value ~ filename + line, data = lines, FUN = min)", fixed = TRUE)
  expect_match(workflow, "uncovered <- lines[lines$value == 0, c(\"filename\", \"line\")]", fixed = TRUE)
  expect_match(workflow, "stop(nrow(uncovered), \" line(s) in R/ are not covered", fixed = TRUE)
})


test_that("a CI job checks the package under CRAN conditions and reports the time (#39)", {
  workflow <- workflow_text("cran-conditions.yaml")
  expect_match(workflow, "os: [ubuntu-latest, windows-latest]", fixed = TRUE)
  # NOT_CRAN is unset, as on CRAN, for the step and so for the build and the
  # check it starts; the step stops if skip_on_cran() would not skip.
  expect_match(workflow, "Sys.unsetenv(\"NOT_CRAN\")", fixed = TRUE)
  expect_false(grepl("\n\\s*NOT_CRAN:", workflow))
  expect_false(grepl("NOT_CRAN = ", workflow, fixed = TRUE))
  expect_match(workflow, "if (!skipped) stop(", fixed = TRUE)
  expect_match(workflow, "args = c(\"--as-cran\", \"--no-manual\")", fixed = TRUE)
  expect_false(grepl("--no-tests|--ignore-vignettes|--no-vignettes|--no-examples", workflow))
  # check-r-package would turn CRAN's incoming checks off.
  expect_false(grepl("uses: r-lib/actions/check-r-package", workflow, fixed = TRUE))
  # The times of the tests, the examples, the vignettes and the whole check.
  for (step in c("checking tests", "checking examples [.][.][.]",
                 "checking re-building of vignette outputs", "The whole check")) {
    expect_match(workflow, step, fixed = TRUE)
  }
  # Time never fails the job; an error or a warning does, after the times
  # are written down.
  expect_match(workflow, "error_on = \"never\"", fixed = TRUE)
  expect_match(workflow, "if (length(result$errors) || length(result$warnings)) {", fixed = TRUE)
  expect_match(workflow, "GITHUB_STEP_SUMMARY", fixed = TRUE)
  expect_false(grepl("timeout-minutes: [0-9]{1,2}\\s", workflow))
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
  # The release candidate on 2026-11-24, or earlier if both packages are
  # ready, its tag the interface freeze (decided 2026-11-20); then 1.0.0 on
  # GitHub and R-universe before the CRAN submission (decided 2026-11-16).
  plans <- list(
    "NEWS.md introduction" = news_introduction(),
    "README.Rmd" = release_text("README.Rmd"),
    "README.md" = release_text("README.md"),
    "ROADMAP.md" = release_text("ROADMAP.md")
  )
  for (name in names(plans)) {
    text <- flat(plans[[name]])
    expect_false(grepl("2026-10-1[37]|2026-11-27|2026-12-01", text),
                 label = paste(name, "gives an old date"))
    expect_false(grepl("feature freeze", text), label = paste(name, "says feature freeze"))
    expect_match(text, "interface freeze", label = name)
    expect_false(grepl("freeze (is )?on 20", text), label = paste(name, "dates the freeze apart"))
    expect_match(text, "2026-11-24", fixed = TRUE, label = name)
    expect_match(text, "or earlier", fixed = TRUE, label = name)
    expect_match(text, "GitHub and R-universe first", fixed = TRUE, label = name)
  }
  # No CRAN interval comes before 1.0.0.
  roadmap <- flat(plans[["ROADMAP.md"]])
  expect_false(grepl("first acceptances|update interval", roadmap))

  for (name in c("vignettes/nomologR.Rmd", "cran-comments.md", "CONTRIBUTING.md")) {
    expect_false(grepl("2026-10-1[37]|2026-11-27|2026-12-01", release_text(name)),
                 label = paste(name, "gives an old date"))
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


test_that("no document says 0.3.0 is waiting for CRAN (#39)", {
  # CRAN returned 0.3.0 on 2026-10-08 with two requests. README.md ships in
  # the tarball, so a CRAN reader would read it.
  for (name in c("README.Rmd", "README.md", "ROADMAP.md", "CONTRIBUTING.md")) {
    text <- flat(release_text(name))
    expect_false(grepl("CRAN's queue|until CRAN accepts the submission|is submitted to CRAN [(]",
                       text), label = paste(name, "says 0.3.0 is in CRAN's queue"))
    expect_false(grepl("`v0.3.0` is the first release submitted to CRAN|targeted for `v0.3.0`",
                       text, fixed = FALSE), label = paste(name, "describes 0.3.0 as pending"))
    expect_false(grepl("v0.3.0`? is submitted to CRAN", text),
                 label = paste(name, "says 0.3.0 is submitted"))
    expect_match(text, "returned `?v?0.3.0`? (on 2026-10-08 )?with two requests", label = name)
  }
})


test_that("the README says it describes the GitHub version, not the release it installs", {
  # The site and README document functions the R-universe release does not
  # have until 1.0.0 is out; the install block says so, naming the release
  # that the top of the README names.
  for (name in c("README.Rmd", "README.md")) {
    text <- flat(release_text(name))
    current <- sub("^.*Current stable release: `([^`]+)`.*$", "\\1", text)
    expect_match(current, "^[0-9]+[.][0-9]+[.][0-9]+$", label = name)
    expect_match(
      text,
      paste0("The R-universe build is the current stable release, `", current,
             "`. This README and the [website](https://juhalt.github.io/nomologR/) ",
             "describe the GitHub version"),
      fixed = TRUE, label = name
    )
  }
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
