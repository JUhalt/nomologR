# Careless responding in the guided workflow (#73) -----------------------------
#
# Careless-responding indices describe a respondent across the whole instrument
# (Curran, 2016; Meade & Craig, 2012), and several cannot be computed within one
# scale: even-odd consistency correlates across scales, and psychometric pairs
# can span scales. So a run computes them once, over every item, with the run's
# scales, and never inside the per-scale item audits.

nomo_run_effort_arguments <- function() {
  c("effort", "scales", "reverse", "scale_range", "pair_magnitude")
}


# The instrument-wide screen that settings$screen$effort = TRUE requests, or
# NULL. Keying comes from the settings, or else from a contentvalidR handoff's
# declared keying; settings are the researcher's and take precedence.
nomo_run_effort_request <- function(settings, scales, handoff = NULL) {
  s <- settings$screen
  if (is.null(s) || !isTRUE(s$effort)) return(NULL)

  arguments <- list(
    effort = TRUE,
    scales = if (!is.null(s$scales)) s$scales else scales
  )
  if (!is.null(s$pair_magnitude)) arguments$pair_magnitude <- s$pair_magnitude

  from_settings <- !is.null(s$reverse) || !is.null(s$scale_range)
  declared <- !is.null(handoff) && isTRUE(handoff$keying$declared)
  keying <- if (from_settings && declared) {
    "override"
  } else if (from_settings) {
    "settings"
  } else if (declared) {
    "handoff"
  } else {
    "undeclared"
  }

  source <- if (identical(keying, "handoff")) handoff$keying else s
  arguments$reverse <- source$reverse
  arguments$scale_range <- source$scale_range

  list(arguments = arguments, keying = keying)
}


nomo_run_effort_log <- function(log, effort, screen) {
  keying <- switch(
    effort$keying,
    handoff = "Reverse keying and the response scale came from the content-review handoff.",
    override = paste(
      "Reverse keying or the response scale was set in `settings$screen`, in",
      "place of the keying the content-review handoff declared."
    ),
    settings = "Reverse keying and the response scale were set in `settings$screen`.",
    undeclared = "No reverse keying was declared, so no item was recoded."
  )

  nomo_run_workflow_log_add(
    log,
    id = "careless_responding",
    stage = "screen",
    scope = "instrument",
    observation = sprintf(
      paste(
        "Careless-responding indices were computed once across all %d items,",
        "using %d scale(s), not within each scale. %s"
      ),
      length(screen$items), length(effort$arguments$scales), keying
    ),
    reason = paste(
      "The indices describe a respondent across the whole instrument, and",
      "several, such as even-odd consistency, cannot be computed within one scale."
    ),
    options = paste(
      "Read the flags alongside the other indices and the decision log; they",
      "disagree by design."
    ),
    consequence = "No case was removed and no response in the data was recoded.",
    decision = "effort = TRUE",
    source = if (identical(effort$keying, "handoff")) "content_review" else "researcher_input"
  )
}
