# The simulated fixtures of helper-fixtures.R ----------------------------------------
#
# The fixtures are drawn with base R, never with lavaan::simulateData(), whose
# data for one seed changed in lavaan 0.7-3. These tests hold the generator to
# that: its numbers are written here, so they are the same under every lavaan
# version. The last test holds the stand-in that test-nomo-power.R puts in
# place of lavaan::simulateData() to the same draws.

fixture_population <- "F =~ 0.8*x1 + 0.6*x2 + 0.5*x3\nG =~ 0.7*y1 + 0.5*y2\nF ~~ 0.5*G"


test_that("a fixture population implies the covariance matrix its values give", {
  expected <- matrix(
    c(1, .48, .40, .28, .20,
      .48, 1, .30, .21, .15,
      .40, .30, 1, .175, .125,
      .28, .21, .175, 1, .35,
      .20, .15, .125, .35, 1),
    5L, 5L, dimnames = rep(list(c("x1", "x2", "x3", "y1", "y2")), 2L)
  )
  expect_equal(nomo_test_population_cov(fixture_population), expected, tolerance = 1e-14)
  # Factors are uncorrelated unless the population says otherwise, and a
  # covariance matrix is used as it is.
  expect_identical(nomo_test_population_cov("F =~ 0.8*x1\nG =~ 0.7*y1")[["x1", "y1"]], 0)
  expect_identical(nomo_test_population_cov(expected), expected)
  # A parameter without a value, or one the fixtures do not use, is refused.
  expect_error(nomo_test_population_cov("F =~ x1 + 0.6*x2"), "ustart")
  expect_error(nomo_test_population_cov("F =~ 0.8*x1 + 0.6*x2\nx1 ~~ 0.2*x2"), "factors")
})


test_that("fixtures do not depend on lavaan's version, and exact ones match the population", {
  sigma <- nomo_test_population_cov(fixture_population)
  drawn <- nomo_test_simulate(fixture_population, n = 200, seed = 2026)
  expect_identical(dim(drawn), c(200L, 5L))
  expect_identical(names(drawn), colnames(sigma))
  expect_equal(sum(abs(drawn)), 791.385814559928, tolerance = 1e-10)
  expect_identical(nomo_test_simulate(fixture_population, n = 200, seed = 2026), drawn)
  expect_false(isTRUE(all.equal(stats::cov(drawn), sigma, tolerance = 1e-6)))

  exact <- nomo_test_simulate(fixture_population, n = 200, seed = 2026, exact = TRUE)
  expect_equal(stats::cov(exact), sigma, tolerance = 1e-12)
  expect_equal(unname(colMeans(exact)), rep(0, 5L), tolerance = 1e-12)
  # A covariance matrix must be positive definite.
  expect_error(nomo_test_simulate(matrix(c(1, 2, 2, 1), 2L), n = 10, seed = 1))
})


test_that("the stand-in for lavaan::simulateData() draws with base R, for one test only", {
  sigma <- nomo_test_population_cov(fixture_population)
  lavaan_simulate_data <- lavaan::simulateData
  with_stand_in <- function() {
    local_base_r_simulate_data()
    # The code under test sets the seed; the stand-in draws from that stream,
    # and a second sample continues the stream.
    set.seed(2026)
    first <- lavaan::simulateData(fixture_population, sample.nobs = 200, standardized = TRUE)
    second <- lavaan::simulateData(fixture_population, sample.nobs = 200, standardized = TRUE)
    expect_identical(first, nomo_test_simulate(fixture_population, n = 200, seed = 2026))
    expect_false(isTRUE(all.equal(second, first)))
    # As in lavaan, empirical data have the population matrix as their
    # maximum-likelihood covariance matrix.
    empirical <- lavaan::simulateData(fixture_population, sample.nobs = 200,
                                      standardized = TRUE, empirical = TRUE)
    expect_equal(stats::cov(empirical) * 199 / 200, sigma, tolerance = 1e-12)
    # What the stand-in does not serve stops.
    expect_error(lavaan::simulateData(fixture_population, sample.nobs = 200), "standardized")
    expect_error(
      lavaan::simulateData(fixture_population, sample.nobs = 200, standardized = TRUE,
                           skewness = 1),
      "length"
    )
  }
  with_stand_in()
  # lavaan's own function is back when the test that asked for the stand-in ends.
  expect_identical(lavaan::simulateData, lavaan_simulate_data)
})
