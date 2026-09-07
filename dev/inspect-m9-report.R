# M9 end-to-end report audit ---------------------------------------------------

library(nomologR)

set.seed(9201)
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

run <- nomo_run(
  data = dat,
  scales = list(WellBeing = c("w1", "w2", "w3", "w4")),
  settings = list(
    factors = list(
      criterion_set = "minimal",
      n_iter = 50L,
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
      rationale = "Retention evidence and substantive definition support one factor."
    ),
    cfa_model = list(
      value = "WellBeing =~ w1 + w2 + w3 + w4",
      rationale = "Prespecified one-factor measurement model."
    ),
    measurement_model = list(
      value = "proceed",
      rationale = paste(
        "Measurement evidence was reviewed; the prespecified model is retained",
        "for the planned invariance and network analyses."
      )
    )
  )
)

stopifnot(identical(run$status, "complete"))

out <- file.path("dev", "m9-report-audit.html")

nomo_report(
  run,
  file = out,
  title = "nomologR M9 researcher-facing report audit",
  include_plots = TRUE,
  include_session = TRUE,
  overwrite = TRUE,
  quiet = FALSE
)

cat("\nRendered M9 audit report:\n", normalizePath(out, winslash = "/"), "\n", sep = "")
cat("\nPlease inspect:\n")
cat("- section order and navigation\n")
cat("- table width/readability\n")
cat("- plot redundancy\n")
cat("- same-sample caveat\n")
cat("- CFA modification-index quarantine\n")
cat("- decision and evidence traceability\n")
cat("- invariance/network sections\n")
cat("- citations and session information\n")
