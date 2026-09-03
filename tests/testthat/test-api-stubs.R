test_that("future public functions fail explicitly rather than returning fake results", {
  dat <- data.frame(x = 1:5, y = 2:6)

  expect_error(nomo_screen(dat), "Milestone 1")
  expect_error(nomo_factors(dat), "Milestone 2")
  expect_error(nomo_efa(dat, factors = 1), "Milestone 3")
})
