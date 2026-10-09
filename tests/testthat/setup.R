# Plots printed by tests are drawn on a device that has no file. Without an
# open device, R would open its default one for a non-interactive session and
# leave Rplots.pdf in the test directory.
grDevices::pdf(NULL)
nomo_test_device <- grDevices::dev.cur()
withr::defer(
  if (nomo_test_device %in% grDevices::dev.list()) {
    grDevices::dev.off(nomo_test_device)
  },
  testthat::teardown_env()
)
