# Manual M6/M7 output audit
#
# Run after devtools::load_all().
# This script is deliberately interactive: it prints the compact researcher
# outputs and opens report-ready ggplot objects for visual inspection.

devtools::load_all()

set.seed(7001)
n <- 600
A <- rnorm(n)
B <- .45 * A + rnorm(n, sd = sqrt(1 - .45^2))
C <- .03 * A + rnorm(n, sd = 1)
criterion <- .50 * A + rnorm(n, sd = .85)

dat <- data.frame(
  a1 = .82 * A + rnorm(n, sd = .55),
  a2 = .78 * A + rnorm(n, sd = .62),
  a3 = .76 * A + rnorm(n, sd = .64),
  b1 = .82 * B + rnorm(n, sd = .55),
  b2 = .78 * B + rnorm(n, sd = .62),
  b3 = .76 * B + rnorm(n, sd = .64),
  c1 = .82 * C + rnorm(n, sd = .55),
  c2 = .78 * C + rnorm(n, sd = .62),
  c3 = .76 * C + rnorm(n, sd = .64),
  criterion = criterion
)

split <- nomo_split(dat, validation_prop = .40, seed = 2026)

h <- nomo_hypotheses(
  "A -> B" = positive(min = .20),
  "A <-> C" = negligible(within = c(-.15, .15)),
  "A -> criterion" = positive()
)

net <- nomo_network(
  "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
    C =~ c1 + c2 + c3
  ",
  data = split,
  hypotheses = h
)

print(net)
summary(net)

nomo_table(net, "hypotheses")
nomo_table(net, "measurement")
nomo_table(net, "replication")

plot(net, "effects")
plot(net, "concordance")
plot(net, "fit")
plot(net, "replication")


# Continuous invariance with an explicit partial-invariance decision
set.seed(7002)
make_group <- function(n, loading2 = .78, intercept3 = 0) {
  f <- rnorm(n)
  data.frame(
    x1 = .82 * f + rnorm(n, sd = .60),
    x2 = loading2 * f + rnorm(n, sd = .62),
    x3 = intercept3 + .74 * f + rnorm(n, sd = .66),
    x4 = .76 * f + rnorm(n, sd = .64)
  )
}

inv_dat <- rbind(
  transform(make_group(220), group = "A"),
  transform(make_group(220, loading2 = .64, intercept3 = .20), group = "B")
)

partial <- nomo_partial(
  level = c("metric", "scalar"),
  syntax = c("F =~ x2", "x3 ~ 1"),
  rationale = c(
    "Example researcher decision after substantive review of loading noninvariance.",
    "Example researcher decision after substantive review of intercept noninvariance."
  )
)

inv <- nomo_invariance(
  "F =~ x1 + x2 + x3 + x4",
  data = inv_dat,
  group = "group",
  levels = c("configural", "metric", "scalar"),
  partial = partial,
  localize = TRUE
)

print(inv)
summary(inv)

nomo_table(inv, "fit")
nomo_table(inv, "partial")
nomo_table(inv, "local_strain")

plot(inv, "fit")
plot(inv, "change")
if (nrow(inv$local_strain)) plot(inv, "local_strain")
