# Assertions that code raises no warning, or runs silently, fail when a later
# release of lavaan, psych, semTools, EFAtools, or ggplot2 adds a warning of
# its own. On CRAN such a failure would fail the package's check, so these
# assertions run everywhere except on CRAN, where skip_on_cran() would skip
# (#145, cran-9). On CRAN the code is still run, so the rest of the test checks
# its result, and a warning is reported without failing the test.
nomo_test_on_cran <- function() {
  tryCatch({
    testthat::skip_on_cran()
    FALSE
  }, skip = function(e) TRUE)
}


expect_no_warning_off_cran <- function(object) {
  if (nomo_test_on_cran()) {
    return(invisible(object))
  }
  testthat::expect_no_warning({{ object }})
}


expect_silent_off_cran <- function(object) {
  if (nomo_test_on_cran()) {
    return(invisible(object))
  }
  testthat::expect_silent({{ object }})
}
