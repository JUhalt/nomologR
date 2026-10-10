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
# explanation rather than being guessed at. Each decision is taken as its
# producer recorded it: nothing here branches on the producer's version.
nomo_handoff_read <- function(x) {
  if (!is.list(x)) nomo_handoff_malformed("it is not a list")
  prov <- if (is.list(x$provenance)) x$provenance else list()
  producer <- nomo_handoff_scalar(prov$package_version, "an unknown version")
  ours <- as.character(utils::packageVersion("nomologR"))

  version <- nomo_handoff_schema_version(prov$schema_version)
  if (is.na(version) || !version %in% nomo_handoff_schema_supported) {
    stop(
      sprintf(
        paste(
          "This handoff uses schema version %s, written by contentvalidR %s.",
          "nomologR %s reads schema version %s. Update nomologR, or produce the",
          "handoff with a contentvalidR release that writes a supported version."
        ),
        nomo_handoff_version_text(prov$schema_version), producer, ours,
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
  # One row per reviewed item: a second row could carry an item and hold it
  # back at once. contentvalidR before 1.0 wrote one row per objective for a
  # congruence fit without a target mapping, so this refusal does not say the
  # producer could not have written the object. It reads the rows alone, never
  # the producer's version.
  repeated <- unique(as.character(evidence$item[duplicated(evidence$item)]))
  if (length(repeated)) {
    stop(
      sprintf(
        paste(
          "This handoff does not have one row per reviewed item:",
          "`item_evidence` lists %s more than once. A congruence handoff",
          "written by contentvalidR before 1.0 without a target mapping had",
          "one row per objective; such a handoff has to be made again from a",
          "new fit with contentvalidR 1.0 or later. Otherwise the object may",
          "have been built by hand or edited afterwards: produce it again",
          "with content_handoff()."
        ),
        paste(repeated, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # `carried` is the only field that decides what is analyzed, and `items` must
  # be exactly the carried items. A disagreement cannot come from the producer.
  carried <- as.character(evidence$item[evidence$carried])
  if (!identical(sort(unique(as.character(x$items))), sort(unique(carried)))) {
    nomo_handoff_malformed("`items` does not match the items marked `carried`")
  }

  # content_handoff() does not stop when no item meets its carry rule, so an
  # empty handoff is a real producer object. It is refused here, before either
  # reader asks for `items` or `scales`, with what content review decided.
  if (!length(carried)) {
    keep <- as.character(unlist(prov$keep))
    stop(
      sprintf(
        paste0(
          "This handoff carries no items: content review held back all %s ",
          "(status counts: %s)%s. nomologR analyzes only carried items, so ",
          "there is nothing to screen. To analyze items the review did not ",
          "carry, produce the handoff again with contentvalidR's ",
          "content_handoff(), naming the statuses to carry in `keep`, and ",
          "record why."
        ),
        nomo_present_count(nrow(evidence), "reviewed item"),
        nomo_handoff_status_counts(evidence$status),
        if (length(keep)) {
          sprintf(" under the carry rule keep = %s",
                  paste0("\"", keep, "\"", collapse = ", "))
        } else {
          ""
        }
      ),
      call. = FALSE
    )
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


# The schema version as a whole number, or NA. contentvalidR writes one integer;
# a version that is not a single whole number (1.9, "1.5", c(1, 2)) is refused
# rather than truncated to one this release reads.
nomo_handoff_schema_version <- function(x) {
  if (length(x) != 1L || !(is.numeric(x) || is.character(x))) return(NA_integer_)
  value <- suppressWarnings(as.numeric(x))
  if (!is.finite(value) || value != round(value)) return(NA_integer_)
  suppressWarnings(as.integer(value))
}


# The recorded schema version as the refusal quotes it.
nomo_handoff_version_text <- function(x) {
  if (!length(x)) return("(none)")
  shown <- as.character(unlist(x))
  if (length(shown) == 1L) shown else sprintf("c(%s)", paste(shown, collapse = ", "))
}


# "Review 10, Not supported 3": how many reviewed items had each status, in
# contentvalidR's own words.
nomo_handoff_status_counts <- function(status) {
  counts <- table(as.character(status))
  if (!length(counts)) return("none recorded")
  paste(sprintf("%s %d", names(counts), as.integer(counts)), collapse = ", ")
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
# carried items are kept, since held-back items are not analyzed.
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
# Three invariants are asserted, not handled: keying is NA for every item or for
# none; a recorded keying is 1 or -1, never a word or another number, since an
# unreadable value would otherwise read as forward keyed; and response_min and
# response_max follow the same rule and are NA together.
nomo_handoff_keying <- function(evidence, carried) {
  out <- list(recorded = FALSE, declared = FALSE, reverse = NULL, scale_range = NULL)
  if (!"keying" %in% names(evidence)) return(out)
  out$recorded <- TRUE

  keying <- evidence$keying
  if (anyNA(keying) && !all(is.na(keying))) {
    nomo_handoff_malformed("`keying` is missing for some items but not others")
  }
  if (!all(is.na(keying)) && !(is.numeric(keying) && all(keying %in% c(1, -1)))) {
    nomo_handoff_malformed("`keying` must be 1 (forward) or -1 (reverse) for each item")
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
#
# `given` holds the `scales`, `reverse`, and `scale_range` the call supplied
# alongside the handoff. The rows say what was actually used: construct
# membership is credited to content review only when the review made one and
# the call did not replace it, and keying the call replaced or completed is
# recorded as such (#145).
nomo_handoff_log <- function(h, stage = "screen", given = list()) {
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

  scales_replaced <- !is.null(given$scales) && !is.null(h$scales) &&
    !nomo_handoff_same_scales(given$scales, h$scales)
  log <- nomo_log_add(
    log, stage = stage, object = "content_review", metric = "content_review_provenance",
    value = length(h$items),
    reference = paste(p$citation, collapse = "; "),
    severity = "info",
    observation = sprintf(
      "%s came from content review in %s %s (%s), not from these data.",
      if (!is.null(h$scales) && !scales_replaced) {
        "Items and their construct membership"
      } else {
        "Items"
      },
      p$package, p$package_version, context
    ),
    recommendation = paste(
      "Changing which items are analyzed, or where they belong, is a researcher",
      "decision to record with its rationale; nomologR does not re-decide it."
    )
  )
  if (scales_replaced) {
    log <- nomo_log_add(
      log, stage = stage, object = "content_review", metric = "scales_override",
      severity = "info",
      observation = sprintf(
        paste(
          "`scales` was supplied in the call, so it was used in place of the",
          "construct membership the handoff declared (%s)."
        ),
        nomo_present_or(names(h$scales), "and")
      ),
      recommendation = "Record why the declared construct membership was set aside."
    )
  }

  log <- nomo_log_add(
    log, stage = stage, object = "content_review", metric = "carry_decisions",
    value = sum(ev$carried),
    severity = "info",
    observation = sprintf(
      "%d of %s %s carried and %d held back. Status counts: %s.",
      sum(ev$carried), nomo_present_count(nrow(ev), "reviewed item"),
      nomo_present_noun(sum(ev$carried), "was", "were"), sum(!ev$carried),
      nomo_handoff_status_counts(ev$status)
    ),
    recommendation = "Only carried items are analyzed."
  )

  held <- ev[!ev$carried, , drop = FALSE]
  for (i in seq_len(nrow(held))) {
    # A review whose results had no recommendation records NA; it is left out
    # rather than quoted as the word "NA".
    quoted <- c(
      if (!is.na(held$status[[i]])) sprintf("status \"%s\"", held$status[[i]]),
      if (!is.na(held$recommendation[[i]])) {
        sprintf("recommendation \"%s\"", held$recommendation[[i]])
      }
    )
    log <- nomo_log_add(
      log, stage = stage, object = as.character(held$item[[i]]),
      metric = "held_back_item",
      severity = "info",
      observation = sprintf(
        "%s was held back by content review%s.",
        held$item[[i]],
        if (length(quoted)) paste0(": ", paste(quoted, collapse = ", ")) else ""
      ),
      recommendation = paste(
        "Not analyzed here. Reinstating it is a researcher decision to record",
        "with its rationale."
      )
    )
  }

  k <- h$keying
  used <- nomo_keying_resolve(given$reverse, given$scale_range, h$items, h)
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
      "Content review declared reverse-keyed %s %s, %s.",
      nomo_present_noun(length(k$reverse), "item", "items"),
      paste(k$reverse, collapse = ", "),
      if (!is.null(k$scale_range)) {
        sprintf("on a %s response scale", nomo_handoff_range_text(k$scale_range))
      } else if (is.null(used$scale_range)) {
        "without the response scale, so they cannot be recoded here"
      } else {
        "without the response scale"
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
  log <- nomo_handoff_keying_rows(log, used, stage)

  if (length(h$shared)) {
    one <- length(h$shared) == 1L
    log <- nomo_log_add(
      log, stage = stage, object = "content_review", metric = "shared_items",
      value = length(h$shared), severity = "review",
      observation = sprintf(
        "%s %s %s to more than one scale in the handoff.",
        if (one) "Item" else "Items", paste(h$shared, collapse = ", "),
        if (one) "belongs" else "belong"
      ),
      recommendation = paste(
        "A measurement model written from these scales would make each a",
        "cross-loading nobody requested. Decide where each belongs before modeling."
      )
    )
  }

  log
}


# Whether the `scales` a call supplied are the handoff's own: the same scale
# names, each with the same items in any order.
nomo_handoff_same_scales <- function(given, declared) {
  if (!is.list(given) || !identical(sort(names(given)), sort(names(declared)))) {
    return(FALSE)
  }
  all(vapply(names(declared), function(s) {
    setequal(as.character(unlist(given[[s]])), declared[[s]])
  }, logical(1)))
}


# A response scale as the log writes it: "1 to 5".
nomo_handoff_range_text <- function(range) {
  nomo_screen_range_text(range[[1L]], range[[2L]])
}


# Rows for keying the call supplied alongside a handoff that declared keying:
# a response scale that completes keying the handoff recorded without one, and
# a `reverse` or `scale_range` that replaces what the handoff declared. Keying
# given to match the handoff, or to a handoff that declared none, needs no row.
nomo_handoff_keying_rows <- function(log, used, stage) {
  origin <- used$origin
  shown <- function(value) {
    if (is.numeric(value)) return(nomo_handoff_range_text(value))
    if (length(value)) paste(value, collapse = ", ") else "none"
  }
  if (identical(origin[["scale_range"]], "completed")) {
    log <- nomo_log_add(
      log, stage = stage, object = "content_review", metric = "keying_completed",
      severity = "info",
      observation = sprintf(
        paste(
          "`scale_range` (%s) was supplied in the call. The handoff declared",
          "the keying without a response scale, so the call's range completes",
          "it, and the declared reverse-keyed items are recoded on it where an",
          "index needs them recoded."
        ),
        shown(used$scale_range)
      ),
      recommendation = "Record where the response scale came from."
    )
  }
  replaced <- names(origin)[origin == "override"]
  if (length(replaced)) {
    what <- c(reverse = "the reverse-keyed items", scale_range = "the response scale")
    log <- nomo_log_add(
      log, stage = stage, object = "content_review", metric = "keying_override",
      severity = "info",
      observation = paste(vapply(replaced, function(arg) {
        sprintf(
          "`%s` (%s) was supplied in the call, so it was used in place of %s the handoff declared (%s).",
          arg, shown(used$given[[arg]]), what[[arg]], shown(used$recorded[[arg]])
        )
      }, character(1)), collapse = " "),
      recommendation = "Record why the declared keying was set aside."
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
          "Content review carried %s not %s of `data`: %s.",
          "Check the item names in the response data against the handoff;",
          "nomologR does not drop carried items."
        ),
        if (length(absent) == 1L) "an item that is" else "items that are",
        if (length(absent) == 1L) "a column" else "columns",
        paste(absent, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
