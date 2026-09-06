# Checkpoint C hardening audit
#
# Run only after devtools::test() and devtools::check() are clean.

cat("\n=== Checkpoint C package coverage ===\n")

if (!requireNamespace("covr", quietly = TRUE)) {
  stop("Install the suggested package `covr` before running this audit.")
}

coverage <- covr::package_coverage(type = "tests")
print(coverage)

cat(
  "\nOverall package coverage: ",
  formatC(covr::percent_coverage(coverage), format = "f", digits = 2),
  "%\n",
  sep = ""
)

cat(
  "\nOpen an interactive line-by-line coverage report with:\n",
  "covr::report(coverage)\n",
  sep = ""
)

cat("\n=== Core M6/M7 files to inspect in coverage report ===\n")
cat(
  paste0(
    "- ",
    c(
      "R/nomo_hypotheses.R",
      "R/nomo_network.R",
      "R/nomo_network_presentation.R",
      "R/nomo_invariance.R",
      "R/nomo_invariance_presentation.R",
      "R/nomo_partial.R",
      "R/nomo_table.R"
    ),
    collapse = "\n"
  ),
  "\n",
  sep = ""
)

cat(
  "\nCheckpoint C target: retain >=90% coverage for core computational ",
  "paths where practicable, while prioritizing meaningful truth/failure ",
  "tests over superficial line coverage.\n",
  sep = ""
)
