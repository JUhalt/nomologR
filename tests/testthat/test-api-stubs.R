test_that("remaining future public functions fail explicitly rather than returning fake results", {
  dat <- data.frame(x = 1:5, y = 2:6)

  # nomo_factors() is implemented beginning in Milestone 2 and therefore is no
  # longer part of the future-API stub test.
  expect_error(nomo_efa(dat, factors = 1), "Milestone 3")
})
