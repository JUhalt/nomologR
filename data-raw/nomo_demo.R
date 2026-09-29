# Generate the nomologR teaching datasets ---------------------------------------
#
# Every public example dataset is simulated from a documented population model
# so that learners (and the test suite) can compare package output with a known
# data-generating process. See data-raw/README.md for the design rules and
# R/data.R for the user-facing documentation of each dataset.
#
# Regenerate from the package root with:
#
#   source("data-raw/nomo_demo.R")
#
# Any change to a population value, seed, or sample size must be recorded in
# NEWS.md because examples, vignettes, and tests depend on these data.

RNGkind("Mersenne-Twister", "Inversion", "Rejection")

# Generate one indicator with standardized population variance.
#
# `loadings` are standardized loadings on the supplied standardized latent
# variables; `phi` is the latent correlation used for cross-loaded indicators.
make_indicator <- function(latent, loadings, phi = 0) {
  common <- Reduce(`+`, Map(`*`, latent, loadings))
  common_var <- sum(loadings^2)
  if (length(loadings) == 2L) {
    common_var <- common_var + 2 * prod(loadings) * phi
  }
  common + stats::rnorm(length(latent[[1L]]), sd = sqrt(1 - common_var))
}

# Continuous-like indicators are reported on a conventional rating metric
# (population mean 4, SD 1). The linear transformation does not change any
# correlation-based or standardized result.
rating_metric <- function(x) round(4 + x, 2)


# nomo_demo_continuous / nomo_demo_ordinal ------------------------------------
#
# Two correlated factors (phi = .40), five candidate indicators per factor.
# Deliberate teaching features:
#   * a5 cross-loads (.45 on A, .35 on B);
#   * b5 is a weak indicator (.30 on B);
#   * a small amount of MCAR missingness in a2 and b3 (continuous version only).

set.seed(20260913L)

n_two_factor <- 500L
phi_ab <- 0.40

f_a <- stats::rnorm(n_two_factor)
f_b <- phi_ab * f_a + sqrt(1 - phi_ab^2) * stats::rnorm(n_two_factor)

two_factor_latent <- data.frame(
  a1 = make_indicator(list(f_a), 0.80),
  a2 = make_indicator(list(f_a), 0.75),
  a3 = make_indicator(list(f_a), 0.70),
  a4 = make_indicator(list(f_a), 0.72),
  a5 = make_indicator(list(f_a, f_b), c(0.45, 0.35), phi = phi_ab),
  b1 = make_indicator(list(f_b), 0.78),
  b2 = make_indicator(list(f_b), 0.74),
  b3 = make_indicator(list(f_b), 0.80),
  b4 = make_indicator(list(f_b), 0.70),
  b5 = make_indicator(list(f_b), 0.30)
)

nomo_demo_continuous <- as.data.frame(lapply(two_factor_latent, rating_metric))

missing_a2 <- sample.int(n_two_factor, 15L)
missing_b3 <- sample.int(n_two_factor, 12L)
nomo_demo_continuous$a2[missing_a2] <- NA_real_
nomo_demo_continuous$b3[missing_b3] <- NA_real_

# The ordinal version thresholds the same latent responses (before missingness
# was introduced) into five ordered categories. The thresholds are shifted so
# that response distributions lean toward agreement, as many attitude and
# self-report scales do.
ordinal_thresholds <- c(-Inf, -1.80, -0.80, 0.20, 1.20, Inf)

nomo_demo_ordinal <- as.data.frame(lapply(two_factor_latent, function(x) {
  cut(
    x,
    breaks = ordinal_thresholds,
    labels = as.character(1:5),
    ordered_result = TRUE
  )
}))


# nomo_demo_network -------------------------------------------------------------
#
# A three-construct validation study with an observed outcome and two
# administration groups.
#
# Measurement model (standardized loadings):
#   Agency             =~ ag1 .80, ag2 .75, ag3 .70, ag4 .78
#   Persistence        =~ pe1 .78, pe2 .72, pe3 .76, pe4 .70
#   SocialDesirability =~ sd1 .70, sd2 .75, sd3 .65
#
# Structural population model (within group):
#   Persistence ~ .45 * Agency
#   Performance ~ .40 * Agency + 0 * Persistence
#   Agency ~~ 0 * SocialDesirability
#
# Group features (for measurement-invariance teaching):
#   * the latent Agency mean is .25 SD higher in the `paper` group;
#   * the ag3 intercept is .50 higher in the `paper` group (a known source of
#     scalar non-invariance). All loadings are equal across groups.

set.seed(20260914L)

n_per_group <- 400L
group <- factor(
  rep(c("online", "paper"), each = n_per_group),
  levels = c("online", "paper")
)
n_network <- length(group)
is_paper <- group == "paper"

agency <- stats::rnorm(n_network) + 0.25 * is_paper
persistence <- 0.45 * (agency - 0.25 * is_paper) +
  stats::rnorm(n_network, sd = sqrt(1 - 0.45^2))
social_desirability <- stats::rnorm(n_network)

network_scores <- data.frame(
  ag1 = make_indicator(list(agency), 0.80),
  ag2 = make_indicator(list(agency), 0.75),
  ag3 = make_indicator(list(agency), 0.70) + 0.50 * is_paper,
  ag4 = make_indicator(list(agency), 0.78),
  pe1 = make_indicator(list(persistence), 0.78),
  pe2 = make_indicator(list(persistence), 0.72),
  pe3 = make_indicator(list(persistence), 0.76),
  pe4 = make_indicator(list(persistence), 0.70),
  sd1 = make_indicator(list(social_desirability), 0.70),
  sd2 = make_indicator(list(social_desirability), 0.75),
  sd3 = make_indicator(list(social_desirability), 0.65)
)

performance <- 0.40 * (agency - 0.25 * is_paper) +
  stats::rnorm(n_network, sd = sqrt(1 - 0.40^2))

nomo_demo_network <- data.frame(
  lapply(network_scores, rating_metric),
  Performance = round(70 + 10 * performance, 1),
  group = group
)


# nomo_demo_longitudinal --------------------------------------------------------
#
# One construct, Wellbeing, measured by the same four items on three occasions
# (t1, t2, t3), for longitudinal-invariance teaching.
#
# Measurement model at every occasion (standardized at t1):
#   Wellbeing =~ w1 .80, w2 .75, w3 .70, w4 .65
# The loadings are equal across occasions.
#
# Latent structure: Wellbeing has variance 1 at every occasion, means 0, .30,
# and .50 (in t1 standard deviations), and correlations .60 between adjacent
# occasions and .45 between t1 and t3.
#
# Occasion features:
#   * each item's unique factor correlates .20 with its own unique factor on the
#     other occasions (the autocorrelated residuals of repeated items);
#   * the w3 intercept is .40 higher at t2 and t3 than at t1, a known source of
#     longitudinal scalar non-invariance.

set.seed(20260928L)

mvn <- function(n, mu, sigma) {
  z <- matrix(stats::rnorm(n * length(mu)), n, length(mu)) %*% chol(sigma)
  sweep(z, 2L, mu, "+")
}

n_longitudinal <- 500L
occasions <- c("t1", "t2", "t3")
wellbeing_r <- matrix(
  c(1, .60, .45,
    .60, 1, .60,
    .45, .60, 1),
  3L, 3L
)
wellbeing <- mvn(n_longitudinal, c(0, .30, .50), wellbeing_r)
unique_r <- matrix(.20, 3L, 3L)
diag(unique_r) <- 1
wellbeing_loadings <- c(w1 = .80, w2 = .75, w3 = .70, w4 = .65)
drift <- c(t1 = 0, t2 = .40, t3 = .40)

longitudinal_scores <- list()
for (item in names(wellbeing_loadings)) {
  loading <- wellbeing_loadings[[item]]
  unique <- mvn(n_longitudinal, c(0, 0, 0), (1 - loading^2) * unique_r)
  for (t in seq_along(occasions)) {
    shift <- if (item == "w3") drift[[t]] else 0
    longitudinal_scores[[paste0(item, "_", occasions[[t]])]] <-
      loading * wellbeing[, t] + unique[, t] + shift
  }
}
longitudinal_scores <- longitudinal_scores[
  paste0(rep(names(wellbeing_loadings), times = 3L), "_", rep(occasions, each = 4L))
]
nomo_demo_longitudinal <- as.data.frame(lapply(longitudinal_scores, rating_metric))


# Save --------------------------------------------------------------------------

dir.create("data", showWarnings = FALSE)
save(nomo_demo_continuous, file = "data/nomo_demo_continuous.rda", compress = "bzip2", version = 2)
save(nomo_demo_ordinal, file = "data/nomo_demo_ordinal.rda", compress = "bzip2", version = 2)
save(nomo_demo_network, file = "data/nomo_demo_network.rda", compress = "bzip2", version = 2)
save(nomo_demo_longitudinal, file = "data/nomo_demo_longitudinal.rda", compress = "bzip2", version = 2)
