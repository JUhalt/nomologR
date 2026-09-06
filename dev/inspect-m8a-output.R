# M8A researcher-facing output inspection --------------------------------------

library(nomologR)

set.seed(8301)
n <- 260
f <- rnorm(n)

dat <- data.frame(
  w1 = .82 * f + rnorm(n, sd = .55),
  w2 = .78 * f + rnorm(n, sd = .60),
  w3 = .75 * f + rnorm(n, sd = .64),
  w4 = .72 * f + rnorm(n, sd = .68)
)

settings <- list(
  factors = list(
    criterion_set = "minimal",
    n_iter = 50L,
    seed = 2026L
  )
)

cat("\n=== Teaching-mode pause after factor retention ===\n")
run <- nomo_run(
  data = dat,
  scales = list(WellBeing = names(dat)),
  mode = "teaching",
  settings = settings
)
print(run)

cat("\n=== Stage table ===\n")
print(nomo_table(run, "stages"))

cat("\n=== Decision request ===\n")
print(nomo_table(run, "requests"))

cat("\n=== Resume with explicit researcher decision ===\n")
run <- nomo_run(
  resume = run,
  decisions = list(
    factor_count = list(
      value = 1L,
      rationale = paste(
        "Parallel/MAP evidence and the substantive definition are consistent",
        "with a one-factor exploratory model."
      )
    )
  )
)
print(run)

cat("\n=== Recorded decisions ===\n")
print(nomo_table(run, "decisions"))

cat("\n=== Component-log provenance ===\n")
print(utils::head(nomo_table(run, "component_log"), 12))

cat("\n=== Research-mode view of the same analysis ===\n")
run_research <- nomo_run(
  resume = run,
  mode = "research"
)
print(run_research)

cat("\n=== Summary ===\n")
print(summary(run))
