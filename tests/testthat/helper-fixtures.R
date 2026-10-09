# ---- consolidated from helper-m8.R ----
make_m8_full_data <- function(n = 300L, seed = 8401L) {
  set.seed(seed)

  f <- rnorm(n)
  group <- rep(c("A", "B"), length.out = n)

  data.frame(
    i1 = .82 * f + rnorm(n, sd = .55),
    i2 = .78 * f + rnorm(n, sd = .60),
    i3 = .75 * f + rnorm(n, sd = .64),
    i4 = .72 * f + rnorm(n, sd = .68),
    criterion = .55 * f + rnorm(n, sd = .75),
    group = group
  )
}


m8_full_settings <- function() {
  list(
    factors = list(
      criterion_set = "minimal",
      n_iter = 10L,
      seed = 2026L
    )
  )
}


m8_model <- function() {
  "WellBeing =~ i1 + i2 + i3 + i4"
}

# ---- consolidated from helper-m9.R ----
make_m9_report_run <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) return(cache)

    set.seed(9101L)
    n <- 260L
    f <- rnorm(n)

    dat <- data.frame(
      i1 = .82 * f + rnorm(n, sd = .55),
      i2 = .78 * f + rnorm(n, sd = .60),
      i3 = .75 * f + rnorm(n, sd = .64),
      i4 = .72 * f + rnorm(n, sd = .68)
    )

    cache <<- nomo_run(
      data = dat,
      scales = list(WellBeing = names(dat)),
      settings = list(
        factors = list(
          criterion_set = "minimal",
          n_iter = 10L,
          seed = 2026L
        )
      ),
      decisions = list(
        factor_count = 1L,
        cfa_model = list(
          value = "WellBeing =~ i1 + i2 + i3 + i4",
          rationale = "Prespecified one-factor measurement model."
        ),
        measurement_model = list(
          value = "proceed",
          rationale = "Measurement evidence reviewed for the test fixture."
        )
      )
    )

    cache
  }
})


make_m9_minimal_run <- function() {
  dat <- data.frame(i1 = 1:5, i2 = 5:1)

  x <- list(
    call = quote(nomo_run(data = dat, scales = list(S = c("i1", "i2")))),
    call_history = list(
      quote(nomo_run(data = dat, scales = list(S = c("i1", "i2"))))
    ),
    mode = "teaching",
    status = "paused",
    next_stage = "efa",
    sample_design = "same_sample",
    sample_source = "data_frame",
    sample_n = tibble::tibble(
      role = c("exploratory", "confirmatory"),
      n = c(5L, 5L)
    ),
    scales = list(S = c("i1", "i2")),
    guidance = nomo_defaults(),
    settings = list(),
    decisions = list(),
    results = list(
      screen = list(),
      factors = list(),
      efa = list(),
      cfa = NULL,
      reliability = NULL,
      validity = NULL,
      invariance = NULL,
      network = NULL
    ),
    stage_status = tibble::tibble(
      stage = c(
        "screen", "factors", "efa", "cfa", "reliability",
        "validity", "invariance", "network"
      ),
      status = c(
        "completed", "completed", "awaiting_decision",
        rep("not_started", 5L)
      ),
      detail = rep("", 8L)
    ),
    decision_requests = tibble::tibble(
      id = "factor_count:S",
      stage = "efa",
      scope = "S",
      observation = "Retention evidence is available.",
      reason = "Factor count changes the fitted model.",
      options = "Choose a factor count.",
      consequence = "EFA is paused.",
      example = "factor_count = 1L"
    ),
    decision_log = tibble::tibble(
      id = "sample_design",
      stage = "design",
      scope = "sample",
      observation = "Same sample.",
      reason = "Sample role matters.",
      options = "Continue or split.",
      consequence = "Same-sample confirmation is not independent.",
      decision = "same_sample",
      rationale = "",
      source = "researcher_input"
    ),
    blocked = NULL,
    source_data = dat,
    state_version = 2L
  )

  class(x) <- c("nomo_run", "list")
  x
}


make_m9_full_report_run <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) return(cache)

    set.seed(9202L)
    n <- 260L
    f <- rnorm(n)

    dat <- data.frame(
      w1 = .82 * f + rnorm(n, sd = .55),
      w2 = .78 * f + rnorm(n, sd = .60),
      w3 = .75 * f + rnorm(n, sd = .64),
      w4 = .72 * f + rnorm(n, sd = .68),
      criterion = .55 * f + rnorm(n, sd = .75),
      group = rep(c("A", "B"), length.out = n)
    )

    h <- nomo_hypotheses(
      "WellBeing -> criterion" = positive(min = .10)
    )

    cache <<- nomo_run(
      data = dat,
      scales = list(
        WellBeing = c("w1", "w2", "w3", "w4")
      ),
      settings = list(
        factors = list(
          criterion_set = "minimal",
          n_iter = 10L,
          seed = 2026L
        ),
        invariance = list(
          group = "group",
          levels = c("configural", "metric"),
          localize = TRUE
        ),
        network = list(
          hypotheses = h
        )
      ),
      decisions = list(
        factor_count = list(
          value = 1L,
          rationale = "Prespecified one-factor exploratory solution."
        ),
        cfa_model = list(
          value = "WellBeing =~ w1 + w2 + w3 + w4",
          rationale = "Prespecified one-factor measurement model."
        ),
        measurement_model = list(
          value = "proceed",
          rationale = "Measurement evidence reviewed for the report fixture."
        )
      )
    )

    cache
  }
})


# Plot titles, subtitles, and captions are wrapped with line breaks (#89), so
# tests compare their text with the breaks collapsed to single spaces.
plot_text <- function(x) gsub("\\s+", " ", x)


# ---- simulated fixtures ----
# No fixture is drawn with lavaan::simulateData(): the same seed gives
# different data from one lavaan version to the next (0.7-3 changed its
# default generator), and lavaan lists the function as deprecated. The
# functions below use lavaan only to read the population syntax, and base R
# for everything else, so a fixture is the same data under every lavaan
# version.

# The covariance matrix of the observed variables that a population model
# implies. `population` is lavaan syntax in which every loading and every
# factor variance or covariance carries its value, as in "A =~ 0.7*a1" and
# "A ~~ 0.4*B"; a factor variance that is left out is 1, and a factor
# covariance that is left out is 0. Each observed variable has variance 1, as
# with simulateData(standardized = TRUE): its error variance is 1 minus the
# variance its factors explain, which is negative for a loading above 1. A
# covariance matrix is returned as it is.
nomo_test_population_cov <- function(population) {
  if (is.matrix(population)) return(population)
  table <- lavaan::lavaanify(population)
  table <- table[table$user == 1L, , drop = FALSE]
  loadings <- table[table$op == "=~", , drop = FALSE]
  covariances <- table[table$op == "~~", , drop = FALSE]
  factors <- unique(loadings$lhs)
  items <- unique(loadings$rhs)
  # Loadings and factor (co)variances with values are all the fixtures need;
  # anything else is refused rather than read as something it is not.
  stopifnot(
    nrow(loadings) + nrow(covariances) == nrow(table),
    !anyNA(table$ustart),
    all(c(covariances$lhs, covariances$rhs) %in% factors),
    !any(items %in% factors)
  )
  lambda <- matrix(0, length(items), length(factors), dimnames = list(items, factors))
  lambda[cbind(loadings$rhs, loadings$lhs)] <- loadings$ustart
  psi <- diag(length(factors))
  dimnames(psi) <- list(factors, factors)
  psi[cbind(covariances$lhs, covariances$rhs)] <- covariances$ustart
  psi[cbind(covariances$rhs, covariances$lhs)] <- covariances$ustart
  sigma <- lambda %*% psi %*% t(lambda)
  diag(sigma) <- 1
  sigma
}


# `n` multivariate normal cases with the covariance matrix of `population`
# (lavaan population syntax or a covariance matrix; see above), drawn with
# set.seed(seed) and rnorm().
#
# With `exact = TRUE` the draws are first centered and whitened with the
# Cholesky factor of their own covariance matrix, so the data have mean 0 and
# the population matrix as their sample covariance matrix, cov(data), exactly.
# Normal-theory maximum likelihood reads complete data only through that
# matrix: a model that holds in the population fits these data perfectly, and
# one that does not misfits by an amount the population fixes, whatever the
# seed. Use it when a test needs an outcome the population guarantees rather
# than one a draw happens to give.
nomo_test_simulate <- function(population, n, seed, exact = FALSE) {
  sigma <- nomo_test_population_cov(population)
  set.seed(seed)
  nomo_test_draw(sigma, n, exact)
}


# The draw itself, from the session's random stream as it stands.
nomo_test_draw <- function(sigma, n, exact = FALSE) {
  z <- matrix(stats::rnorm(n * ncol(sigma)), nrow = n)
  if (exact) {
    z <- scale(z, center = TRUE, scale = FALSE)
    z <- z %*% solve(chol(stats::cov(z)))
  }
  # chol() stops unless the matrix is positive definite.
  data <- as.data.frame(z %*% chol(sigma))
  names(data) <- colnames(sigma)
  data
}


# For a test of code that draws its own samples, as nomo_power_simulate() does
# with lavaan::simulateData(): until the calling test ends, that function is
# replaced by the base-R draw above. The samples come from the session's random
# stream, so a seed set by the code under test gives the same samples under
# every lavaan version, and what the test asserts about them cannot change with
# lavaan's generator.
#
# The stand-in serves a standardized population of the kind
# nomo_test_population_cov() reads. As in lavaan, `empirical = TRUE` gives data
# whose maximum-likelihood covariance matrix (divisor n) is the population
# matrix. Any other argument stops, rather than being ignored.
local_base_r_simulate_data <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    simulateData = function(model, sample.nobs, standardized = FALSE,
                            empirical = FALSE, ...) {
      stopifnot(...length() == 0L, isTRUE(standardized))
      data <- nomo_test_draw(nomo_test_population_cov(model), sample.nobs, exact = empirical)
      if (empirical) data <- data * sqrt(sample.nobs / (sample.nobs - 1))
      data
    },
    .package = "lavaan", .env = env
  )
}
