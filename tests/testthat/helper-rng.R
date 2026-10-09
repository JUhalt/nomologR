# The session's random-number state, read without changing it; NULL when the
# session has none. A test compares it before and after a call that sets a
# seed, inside withr::with_seed() so that the test's own seed is put back too.
nomo_test_rng_state <- function() {
  get0(".Random.seed", envir = globalenv(), inherits = FALSE)
}
