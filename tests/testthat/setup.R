# Plots printed by tests are drawn on a device that has no file. In a
# non-interactive session R's default device would write Rplots.pdf into the
# test directory.
#
# The default device is replaced for the run; no device is held open. Code
# under test that manages graphics devices itself (knitr, while a report is
# rendered) then never meets, or closes, a device it did not open: holding one
# open made a nested render fail on macOS, where knitr falls back from svg.
withr::local_options(
  list(device = function(...) grDevices::pdf(file = NULL, ...)),
  .local_envir = testthat::teardown_env()
)
