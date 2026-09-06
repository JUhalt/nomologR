# M8 full workflow researcher-facing inspection --------------------------------

library(nomologR)

set.seed(8701)
n <- 420
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

settings <- list(
  factors = list(
    criterion_set = "minimal",
    n_iter = 50L,
    seed = 2026L
  )
)

cat("\n=== 1. Teaching mode: pause before EFA ===\n")
run <- nomo_run(
  data = dat,
  scales = list(WellBeing = c("w1", "w2", "w3", "w4")),
  mode = "teaching",
  settings = settings
)
print(run)

cat("\n=== 2. Explicit factor-count handoff ===\n")
run <- nomo_run(
  resume = run,
  decisions = list(
    factor_count = list(
      value = 1L,
      rationale = "Retention evidence and substantive interpretation support one factor."
    )
  )
)
print(run)

cat("\n=== 3. Explicit CFA-model handoff ===\n")
run <- nomo_run(
  resume = run,
  decisions = list(
    cfa_model = list(
      value = "WellBeing =~ w1 + w2 + w3 + w4",
      rationale = "Prespecified one-factor measurement model."
    )
  )
)
print(run)

cat("\n=== 4. Measurement evidence tables ===\n")
print(run$results$cfa$fit_evidence)
print(run$results$reliability$evidence)
print(run$results$validity$ave)

cat("\n=== 5. Add future branches at the measurement-review pause ===\n")
run <- nomo_run(
  resume = run,
  settings = list(
    invariance = list(
      group = "group",
      levels = c("configural", "metric"),
      localize = TRUE
    ),
    network = list(
      hypotheses = h
    )
  )
)
print(nomo_table(run, "settings"))

cat("\n=== 6. Explicit measurement-model continuation ===\n")
run <- nomo_run(
  resume = run,
  decisions = list(
    measurement_model = list(
      value = "proceed",
      rationale = paste(
        "CFA, reliability, and convergent/discriminant evidence were reviewed;",
        "the prespecified model is retained for planned downstream analyses."
      )
    )
  )
)
print(run)

cat("\n=== 7. Final stage map ===\n")
print(nomo_table(run, "stages"))

cat("\n=== 8. Workflow decisions ===\n")
print(nomo_table(run, "decisions"))

cat("\n=== 9. Component recipe ===\n")
print(nomo_table(run, "recipe"))

cat("\n=== 10. Component evidence provenance ===\n")
print(utils::head(nomo_table(run, "component_log"), 20))

cat("\n=== 11. Research-mode view of the same completed workflow ===\n")
run_research <- nomo_run(
  resume = run,
  mode = "research"
)
print(run_research)

cat("\n=== 12. Summary ===\n")
print(summary(run))
