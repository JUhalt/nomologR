# contentvalidR handoff -------------------------------------------------------
#
# The exchange object is specified in #46 and documented identically in both
# packages. Neither package depends on the other: nomologR recognizes the
# "cv_handoff" class and reads documented fields, and its tests read stored
# output from real contentvalidR releases. Unknown fields are ignored, so the
# producer can add fields within a schema version; an unknown schema version is
# refused.

nomo_handoff_schema_supported <- 1L


nomo_handoff_is <- function(x) inherits(x, "cv_handoff")


# Validates a handoff and returns what nomologR uses from it. Anything that
# could only arise from an object not produced by contentvalidR's
# content_handoff() (hand-built, or edited afterwards) stops with that
# explanation rather than being guessed at.
nomo_handoff_read <- function(x) {
  prov <- if (is.list(x$provenance)) x$provenance else list()
  producer <- nomo_handoff_scalar(prov$package_version, "an unknown version")
  ours <- as.character(utils::packageVersion("nomologR"))

  version <- suppressWarnings(as.integer(nomo_handoff_scalar(prov$schema_version, NA)))
  if (is.na(version) || !version %in% nomo_handoff_schema_supported) {
    stop(
      sprintf(
        paste(
          "This handoff uses schema version %s, written by contentvalidR %s.",
          "nomologR %s reads schema version %s. Update nomologR, or produce the",
          "handoff with a contentvalidR release that writes a supported version."
        ),
        nomo_handoff_scalar(prov$schema_version, "(none)"), producer, ours,
        paste(nomo_handoff_schema_supported, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  nomo_handoff_require(names(x), c("items", "item_evidence", "item_statistics", "provenance"),
                       "handoff")
  if (!is.data.frame(x$item_evidence)) nomo_handoff_malformed("`item_evidence` is not a data frame")
  if (!is.data.frame(x$item_statistics)) nomo_handoff_malformed("`item_statistics` is not a data frame")
  nomo_handoff_require(
    names(x$item_evidence),
    c("item", "scale", "carried", "status", "recommendation", "n_judges", "rule", "round"),
    "item_evidence"
  )

  evidence <- tibble::as_tibble(x$item_evidence)
  if (!is.logical(evidence$carried) || anyNA(evidence$carried)) {
    nomo_handoff_malformed("`carried` must be TRUE or FALSE for every reviewed item")
  }

  # `carried` is the only field that decides what is analysed, and `items` must
  # be exactly the carried items. A disagreement cannot come from the producer.
  carried <- as.character(evidence$item[evidence$carried])
  if (!identical(sort(unique(as.character(x$items))), sort(unique(carried)))) {
    nomo_handoff_malformed("`items` does not match the items marked `carried`")
  }

  scales <- nomo_handoff_scales(x$scales, evidence, carried)
  keying <- nomo_handoff_keying(evidence, carried)

  memberships <- unlist(scales, use.names = FALSE)
  shared <- sort(unique(memberships[duplicated(memberships)]))

  out <- list(
    items = unique(carried),
    scales = scales,
    evidence = evidence,
    statistics = tibble::as_tibble(x$item_statistics),
    panel = if (is.data.frame(x$panel_statistics)) {
      tibble::as_tibble(x$panel_statistics)
    } else {
      NULL
    },
    provenance = list(
      schema_version = version,
      package = nomo_handoff_scalar(prov$package, "contentvalidR"),
      package_version = producer,
      workflow = nomo_handoff_scalar(prov$workflow, NA_character_),
      mode = nomo_handoff_scalar(prov$mode, NA_character_),
      keep = paste(as.character(prov$keep), collapse = ", "),
      method = nomo_handoff_scalar(prov$method, NA_character_),
      citation = as.character(unlist(prov$citation)),
      created = if (length(prov$created)) prov$created[[1L]] else NA
    ),
    keying = keying,
    shared = shared
  )
  class(out) <- c("nomo_handoff", "list")
  out
}


nomo_handoff_scalar <- function(x, default) {
  if (!length(x) || is.na(x[[1L]])) return(default)
  as.character(x[[1L]])
}


nomo_handoff_malformed <- function(problem) {
  stop(
    paste0(
      "This handoff is malformed: ", problem, ". An object produced by ",
      "contentvalidR's content_handoff() cannot be like this, so it may have ",
      "been built by hand or edited afterwards. Produce it again with ",
      "content_handoff()."
    ),
    call. = FALSE
  )
}


nomo_handoff_require <- function(present, required, what) {
  missing <- setdiff(required, present)
  if (length(missing)) {
    nomo_handoff_malformed(sprintf(
      "`%s` lacks the field(s) %s", what, paste(missing, collapse = ", ")
    ))
  }
}


# `scales` is NULL exactly when the review had no construct mapping, and then
# `scale` is NA for every item; the two always agree in producer output. Only
# carried items are kept, since held-back items are not analysed.
nomo_handoff_scales <- function(scales, evidence, carried) {
  no_mapping <- all(is.na(evidence$scale))
  if (is.null(scales)) {
    if (!no_mapping) nomo_handoff_malformed("`scales` is NULL but items name a scale")
    return(NULL)
  }
  if (no_mapping || !is.list(scales) || is.null(names(scales))) {
    nomo_handoff_malformed("`scales` and the items' `scale` column disagree")
  }
  scales <- lapply(scales, function(items) intersect(as.character(items), carried))
  scales[lengths(scales) > 0L]
}


# Keying and the response scale, mapped onto nomo_screen()'s `reverse` and
# `scale_range` as agreed in #46:
#
# * columns absent (a producer before 0.7.0), or keying NA for every item:
#   keying was not declared, so `reverse` is NULL;
# * keying 1 for every item: declared, with nothing reversed, so `reverse` is
#   character(0) and no range is needed;
# * any -1: those carried items are `reverse`, and the response range is
#   `scale_range` if it was recorded. A missing range is never inferred, so the
#   screen refuses to recode rather than guess one.
#
# Two invariants are asserted, not handled: keying is NA for every item or for
# none, and response_min and response_max follow the same rule and are NA
# together.
nomo_handoff_keying <- function(evidence, carried) {
  out <- list(recorded = FALSE, declared = FALSE, reverse = NULL, scale_range = NULL)
  if (!"keying" %in% names(evidence)) return(out)
  out$recorded <- TRUE

  keying <- evidence$keying
  if (anyNA(keying) && !all(is.na(keying))) {
    nomo_handoff_malformed("`keying` is missing for some items but not others")
  }

  has_range <- all(c("response_min", "response_max") %in% names(evidence))
  if (has_range) {
    lo <- evidence$response_min
    hi <- evidence$response_max
    if (!identical(is.na(lo), is.na(hi)) ||
          (anyNA(lo) && !all(is.na(lo)))) {
      nomo_handoff_malformed(
        "`response_min` and `response_max` are not missing together for every item"
      )
    }
    if (!all(is.na(lo))) {
      range <- unique(cbind(as.numeric(lo), as.numeric(hi)))
      if (nrow(range) > 1L) {
        nomo_handoff_malformed("items record different response scales")
      }
      out$scale_range <- as.numeric(range[1L, ])
    }
  }

  if (all(is.na(keying))) return(out)

  out$declared <- TRUE
  reversed <- as.character(evidence$item[evidence$carried & keying == -1])
  out$reverse <- intersect(reversed, carried)
  out
}


# Decision-log rows recording what content review decided and that item
# membership came from it rather than from these data. Held-back items are
# reported in the producer's own words, quoted, and are never re-worded.
nomo_handoff_log <- function(h, stage = "screen") {
  log <- nomo_log_new()
  p <- h$provenance
  ev <- h$evidence

  describe <- function(label, value) {
    if (length(value) && !is.na(value) && nzchar(value)) sprintf("%s: %s", label, value)
  }
  context <- paste(c(
    describe("workflow", p$workflow),
    describe("mode", p$mode),
    describe("carry rule", p$keep),
    describe("method", p$method)
  ), collapse = "; ")

  log <- nomo_log_add(
    log, stage = stage, object = "content_review", metric = "content_review_provenance",
    value = length(h$items),
    reference = paste(p$citation, collapse = "; "),
    severity = "info",
    observation = sprintf(
      "Items and their construct membership came from content review in %s %s (%s), not from these data.",
      p$package, p$package_version, context
    ),
    recommendation = paste(
      "Changing which items are analysed, or where they belong, is a researcher",
      "decision to record with its rationale; nomologR does not re-decide it."
    )
  )

  status_counts <- table(as.character(ev$status))
  log <- nomo_log_add(
    log, stage = stage, object = "content_review", metric = "carry_decisions",
    value = sum(ev$carried),
    severity = "info",
    observation = sprintf(
      "%d of %d reviewed item(s) were carried and %d held back. Status counts: %s.",
      sum(ev$carried), nrow(ev), sum(!ev$carried),
      paste(sprintf("%s %d", names(status_counts), as.integer(status_counts)),
            collapse = ", ")
    ),
    recommendation = "Only carried items are analysed."
  )

  held <- ev[!ev$carried, , drop = FALSE]
  for (i in seq_len(nrow(held))) {
    log <- nomo_log_add(
      log, stage = stage, object = as.character(held$item[[i]]),
      metric = "held_back_item",
      severity = "info",
      observation = sprintf(
        "%s was held back by content review: status \"%s\", recommendation \"%s\".",
        held$item[[i]], held$status[[i]], held$recommendation[[i]]
      ),
      recommendation = paste(
        "Not analysed here. Reinstating it is a researcher decision to record",
        "with its rationale."
      )
    )
  }

  k <- h$keying
  keying_note <- if (!k$recorded) {
    paste(
      "This handoff predates keying fields (contentvalidR 0.7.0), so reverse",
      "keying and the response scale are not declared."
    )
  } else if (!k$declared) {
    "Content review did not declare reverse keying."
  } else if (!length(k$reverse)) {
    "Content review declared keying with no reverse-keyed item among those carried."
  } else {
    sprintf(
      "Content review declared reverse-keyed item(s) %s, %s.",
      paste(k$reverse, collapse = ", "),
      if (is.null(k$scale_range)) {
        "without the response scale, so they cannot be recoded here"
      } else {
        sprintf("on a %g to %g response scale", k$scale_range[[1L]], k$scale_range[[2L]])
      }
    )
  }
  log <- nomo_log_add(
    log, stage = stage, object = "content_review", metric = "keying",
    severity = "info", observation = keying_note,
    recommendation = paste(
      "Keying is used only as declared: an undeclared item is never treated as",
      "forward keyed, and a response scale is never inferred from the data."
    )
  )

  if (length(h$shared)) {
    log <- nomo_log_add(
      log, stage = stage, object = "content_review", metric = "shared_items",
      value = length(h$shared), severity = "review",
      observation = sprintf(
        "Item(s) %s belong to more than one scale in the handoff.",
        paste(h$shared, collapse = ", ")
      ),
      recommendation = paste(
        "A measurement model written from these scales would make each a",
        "cross-loading nobody requested. Decide where each belongs before modelling."
      )
    )
  }

  log
}


nomo_handoff_check_data <- function(h, data_names) {
  absent <- setdiff(h$items, data_names)
  if (length(absent)) {
    stop(
      sprintf(
        paste(
          "Content review carried item(s) that are not columns of `data`: %s.",
          "Check the item names in the response data against the handoff;",
          "nomologR does not drop carried items."
        ),
        paste(absent, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
