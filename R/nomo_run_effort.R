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


# `settings$screen$reverse` is checked against the run's items before any audit
# runs. Each per-scale audit keeps only its own scale's items, so a misspelled
# name would otherwise be dropped without a word, whether or not careless-
# responding indices were requested (#145).
nomo_run_check_reverse <- function(settings, scales) {
  reverse <- settings$screen$reverse
  if (is.null(reverse)) return(invisible(NULL))
  if (!is.character(reverse) || anyNA(reverse)) {
    stop("`settings$screen$reverse` must be a character vector of item names.", call. = FALSE)
  }
  unknown <- setdiff(reverse, unlist(scales, use.names = FALSE))
  if (length(unknown)) {
    stop(
      sprintf(
        "`settings$screen$reverse` names item(s) outside `scales`: %s.",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}


# The keying the item audits and the careless-responding screen use (#145).
# `reverse` and `scale_range` are resolved one at a time, as nomo_screen()
# resolves them: each comes from `settings$screen` when given there, else from
# a contentvalidR handoff. A handoff's reverse-keyed items are those it declares
# with keying -1 among the run's items, so a revision that reinstates or removes
# an item keeps its declared keying. `origin` says where each value came from:
# "handoff", "settings", "completed" (given where a handoff that declares keying
# recorded none), "override" (given in place of a different handoff value), or
# "none".
nomo_run_screen_keying <- function(settings, scales, handoff = NULL) {
  s <- settings$screen
  nomo_keying_resolve(s$reverse, s$scale_range, unlist(scales, use.names = FALSE),
                      handoff)
}


# The same resolution for any caller: `reverse` and `scale_range` as given (NULL
# when not), the items in use, and the handoff. nomo_screen() uses it to record
# keying given in the call alongside a handoff (#145).
nomo_keying_resolve <- function(reverse, scale_range, items, handoff = NULL) {
  declared <- !is.null(handoff) && isTRUE(handoff$keying$declared)
  ev <- handoff$evidence

  given <- list(reverse = reverse, scale_range = scale_range)
  recorded <- list(
    reverse = if (declared) intersect(as.character(ev$item[ev$keying %in% -1]), items),
    scale_range = handoff$keying$scale_range
  )
  same <- list(
    reverse = function(a, b) setequal(a, b),
    scale_range = function(a, b) is.numeric(a) && length(a) == length(b) && isTRUE(all(a == b))
  )

  arguments <- c(reverse = "reverse", scale_range = "scale_range")
  origin <- vapply(arguments, function(arg) {
    mine <- given[[arg]]
    theirs <- recorded[[arg]]
    if (is.null(mine)) {
      if (is.null(theirs)) "none" else "handoff"
    } else if (is.null(theirs)) {
      if (declared) "completed" else "settings"
    } else if (same[[arg]](mine, theirs)) {
      "handoff"
    } else {
      "override"
    }
  }, character(1))
  value <- lapply(arguments, function(arg) {
    if (is.null(given[[arg]])) recorded[[arg]] else given[[arg]]
  })

  list(
    reverse = value$reverse,
    scale_range = value$scale_range,
    origin = origin,
    given = given,
    recorded = recorded
  )
}


# A design-log row whenever `settings$screen` replaces keying a handoff
# declared or supplies a response scale it did not record. The per-scale item
# audits use the keying whether or not careless-responding indices were
# requested, so the row does not wait for `effort` (#145).
nomo_run_keying_log <- function(log, keying) {
  changed <- names(keying$origin)[keying$origin %in% c("override", "completed")]
  if (!length(changed)) return(log)

  shown <- function(value) {
    if (length(value)) paste(value, collapse = if (is.numeric(value)) " to " else ", ") else "none"
  }
  what <- c(reverse = "the reverse-keyed item(s)", scale_range = "the response scale")
  sentences <- vapply(changed, function(arg) {
    if (identical(keying$origin[[arg]], "override")) {
      sprintf(
        paste(
          "`settings$screen$%s` (%s) was used in place of %s the content-review",
          "handoff declared (%s)."
        ),
        arg, shown(keying$given[[arg]]), what[[arg]], shown(keying$recorded[[arg]])
      )
    } else {
      sprintf(
        "`settings$screen$%s` (%s) supplied %s the content-review handoff did not record.",
        arg, shown(keying$given[[arg]]), what[[arg]]
      )
    }
  }, character(1))

  nomo_run_workflow_log_add(
    log,
    id = "keying",
    stage = "screen",
    scope = "scales",
    observation = paste(sentences, collapse = " "),
    reason = paste(
      "Declared keying tells the item audits whether a negative item-rest",
      "correlation is an item not yet recoded. Content review declared it, so",
      "replacing or completing it is a researcher decision."
    ),
    options = paste(
      "Record why the declared keying was changed or completed; leave the",
      "setting out to use the handoff's keying."
    ),
    consequence = paste(
      "The item audits, and careless-responding indices when requested, use",
      "this keying. No response in the data is recoded."
    ),
    decision = sprintf(
      "reverse: %s; scale_range: %s",
      shown(keying$reverse), shown(keying$scale_range)
    ),
    source = "researcher_input"
  )
}


# The instrument-wide screen that settings$screen$effort = TRUE requests, or
# NULL. Keying is the run's, resolved by nomo_run_screen_keying().
nomo_run_effort_request <- function(settings, scales, keying) {
  s <- settings$screen
  if (is.null(s) || !isTRUE(s$effort)) return(NULL)

  arguments <- list(
    effort = TRUE,
    scales = if (!is.null(s$scales)) s$scales else scales
  )
  if (!is.null(s$pair_magnitude)) arguments$pair_magnitude <- s$pair_magnitude
  arguments$reverse <- keying$reverse
  arguments$scale_range <- keying$scale_range

  list(arguments = arguments, keying = keying)
}


# Where reverse keying and the response scale came from, in one sentence.
nomo_run_keying_text <- function(keying) {
  o <- keying$origin
  said <- c(
    handoff = "came from the content-review handoff",
    settings = "was set in `settings$screen`",
    completed = "was set in `settings$screen`, where the content-review handoff recorded none",
    override = "was set in `settings$screen`, in place of the content-review handoff's",
    none = "was not recorded"
  )
  if (identical(o[["reverse"]], "none")) {
    return(paste0(
      "No reverse keying was declared, so no item was recoded",
      if (identical(o[["scale_range"]], "none")) "." else {
        sprintf("; the response scale %s.", said[[o[["scale_range"]]]])
      }
    ))
  }
  if (identical(o[["reverse"]], o[["scale_range"]])) {
    return(switch(
      o[["reverse"]],
      handoff = "Reverse keying and the response scale came from the content-review handoff.",
      settings = "Reverse keying and the response scale were set in `settings$screen`.",
      override = paste(
        "Reverse keying and the response scale were set in `settings$screen`, in",
        "place of the keying the content-review handoff declared."
      )
    ))
  }
  sprintf(
    "Reverse keying %s; the response scale %s.",
    said[[o[["reverse"]]]], said[[o[["scale_range"]]]]
  )
}


nomo_run_effort_log <- function(log, effort, screen) {
  origin <- effort$keying$origin
  from_handoff <- all(origin %in% c("handoff", "none")) && any(origin == "handoff")

  nomo_run_workflow_log_add(
    log,
    id = "careless_responding",
    stage = "screen",
    scope = "instrument",
    observation = sprintf(
      paste(
        "Careless-responding indices were computed once across all %d items,",
        "using %s, not within each scale. %s"
      ),
      length(screen$items), nomo_present_count(length(effort$arguments$scales), "scale"),
      nomo_run_keying_text(effort$keying)
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
    source = if (from_handoff) "content_review" else "researcher_input"
  )
}
