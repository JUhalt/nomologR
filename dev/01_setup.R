# Development environment check -------------------------------------------
#
# This file intentionally does not mutate DESCRIPTION or recreate package
# infrastructure. Run it when setting up a new development machine.

dev_packages <- c(
  "devtools", "roxygen2", "testthat", "covr", "knitr", "rmarkdown",
  "psych", "lavaan", "semTools"
)

missing <- dev_packages[!vapply(dev_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  message("Install missing development packages with:")
  message("install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))")
} else {
  message("nomologR development dependencies are available.")
}
