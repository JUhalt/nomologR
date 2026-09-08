# Internal decision-log infrastructure -------------------------------------

nomo_log_new <- function() {
  tibble::tibble(
    stage = character(),
    object = character(),
    metric = character(),
    value = numeric(),
    reference = character(),
    severity = character(),
    observation = character(),
    recommendation = character(),
    decision = character(),
    rationale = character()
  )
}

nomo_log_add <- function(log,
                         stage,
                         object,
                         metric,
                         value = NA_real_,
                         reference = "",
                         severity = c("info", "review", "concern"),
                         observation = "",
                         recommendation = "",
                         decision = "",
                         rationale = "") {
  severity <- match.arg(severity)

  if (!inherits(log, "data.frame")) {
    stop("`log` must be a data frame created by `nomo_log_new()`.", call. = FALSE)
  }

  dplyr::bind_rows(
    log,
    tibble::tibble(
      stage = as.character(stage),
      object = as.character(object),
      metric = as.character(metric),
      value = as.numeric(value),
      reference = as.character(reference),
      severity = severity,
      observation = as.character(observation),
      recommendation = as.character(recommendation),
      decision = as.character(decision),
      rationale = as.character(rationale)
    )
  )
}
