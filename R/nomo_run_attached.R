# Scores and missing-data sensitivity in the guided workflow (#73) --------------
#
# Both are evidence about the measurement model, so they follow the CFA and are
# shown before the researcher decides whether to carry the model forward.
# Missing-data sensitivity for a network follows the network. Neither is a stage
# of its own, and neither runs unless requested.

nomo_run_attached_requested <- function(x, name) {
  length(x$settings[[name]]) > 0L
}


# Requested evidence that could not be computed. It is optional and later
# stages do not depend on it, so the run continues rather than blocking, and the
# design log says what was requested and why it is absent.
nomo_run_attached_failed <- function(x, id, stage, scope, result) {
  x$decision_log <- nomo_run_workflow_log_add(
    x$decision_log,
    id = id,
    stage = stage,
    scope = scope,
    observation = paste("Requested but not computed:", result$error),
    reason = "This evidence is optional, and no later stage depends on it.",
    options = "Correct the request in `settings` and start a new `nomo_run()`.",
    consequence = "The workflow continued without it.",
    decision = "not computed",
    source = "pipeline"
  )
  x
}


nomo_run_run_scores <- function(x) {
  if (!nomo_run_attached_requested(x, "scores")) return(x)
  method <- x$settings$scores$method

  result <- nomo_run_safe_component(
    fun = nomo_scores,
    fixed = list(fit = x$results$cfa, guidance = x$guidance),
    extra = list(method = method),
    stage = "scores"
  )
  if (!result$ok) {
    return(nomo_run_attached_failed(x, "scores", "cfa", "measurement_model", result))
  }

  x$results$scores <- result$value
  x$decision_log <- nomo_run_workflow_log_add(
    x$decision_log,
    id = "scores",
    stage = "cfa",
    scope = "measurement_model",
    observation = sprintf(
      paste(
        "Scores were computed from the fitted measurement model with the",
        "%s method the researcher specified, with Grice's (2001) criteria for them."
      ),
      method
    ),
    reason = "Scoring is a modeling decision, so the method is the researcher's.",
    options = "Read the score properties and notes before using the scores in a later analysis.",
    consequence = "The data were not modified; only cases the model used were scored.",
    decision = paste0("method = \"", method, "\""),
    source = "researcher_input"
  )
  x
}


nomo_run_run_missing <- function(x, target = c("cfa", "network")) {
  target <- match.arg(target)
  if (!nomo_run_attached_requested(x, "missing")) return(x)
  fitted <- x$results[[target]]
  if (is.null(fitted)) return(x)

  roles <- nomo_run_data_roles(x$source_data)
  data <- if (identical(target, "cfa")) roles$confirmatory else x$source_data

  extra <- list()
  if (!is.null(x$settings$missing$strategies)) {
    extra$strategies <- x$settings$missing$strategies
  }
  if (identical(target, "cfa") && !is.null(x$settings$missing$reliability)) {
    extra$reliability <- x$settings$missing$reliability
  }

  result <- nomo_run_safe_component(
    fun = nomo_missing,
    fixed = list(x = fitted, data = data),
    extra = extra,
    stage = "missing"
  )
  if (!result$ok) {
    return(nomo_run_attached_failed(
      x, paste0("missing_data_", target), target,
      if (identical(target, "cfa")) "measurement_model" else "theory_network", result
    ))
  }

  x$results$missing[[target]] <- result$value
  x$decision_log <- nomo_run_workflow_log_add(
    x$decision_log,
    id = paste0("missing_data_", target),
    stage = target,
    scope = if (identical(target, "cfa")) "measurement_model" else "theory_network",
    observation = sprintf(
      "The %s was refitted under %s to show how much its results depend on missing-data handling.",
      if (identical(target, "cfa")) "measurement model" else "network",
      paste(result$value$strategies$label, collapse = " and ")
    ),
    reason = paste(
      "Whether data are missing at random cannot be tested from the data at",
      "hand, so the dependence on the strategy is reported."
    ),
    options = "Report the strategy chosen in advance, together with this sensitivity.",
    consequence = "The fitted model and its results are unchanged; the refits are kept alongside.",
    decision = paste("strategies:", paste(result$value$strategies$strategy, collapse = ", ")),
    source = "researcher_input"
  )
  x
}
