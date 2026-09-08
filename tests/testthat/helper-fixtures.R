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
