# Presentation methods for convergent/discriminant evidence -------------------

nomo_validity_convergent_table <- function(x) {
  # An AVE table without rows has no columns either.
  ave_cols <- c("construct", "block", "estimate", "attention")
  ave <- if (all(ave_cols %in% names(x$ave))) {
    x$ave[, ave_cols, drop = FALSE]
  } else {
    tibble::tibble()
  }
  if (nrow(ave)) {
    names(ave)[names(ave) == "estimate"] <- "AVE"
    names(ave)[names(ave) == "attention"] <- "ave_attention"
  }

  loads <- x$standardized_loadings
  # The loading table carries no group or level labels, so loadings are not
  # pooled into a summary across groups or levels; nomo_invariance() compares
  # them.
  if (nrow(loads) && isTRUE(x$ngroups == 1L) && isTRUE(x$nlevels == 1L)) {
    pieces <- split(loads, loads$factor)
    loading_summary <- tibble::tibble(
      construct = names(pieces),
      min_abs_loading = vapply(pieces, function(z) {
        vals <- abs(z$loading[is.finite(z$loading)])
        if (length(vals)) min(vals) else NA_real_
      }, numeric(1)),
      median_abs_loading = vapply(pieces, function(z) {
        vals <- abs(z$loading[is.finite(z$loading)])
        if (length(vals)) stats::median(vals) else NA_real_
      }, numeric(1)),
      n_loading_review = vapply(pieces, function(z) {
        sum(z$attention != "KEEP", na.rm = TRUE)
      }, integer(1)),
      loading_concern = vapply(pieces, function(z) {
        any(z$attention == "STRONG REVIEW", na.rm = TRUE)
      }, logical(1))
    )
  } else {
    loading_summary <- tibble::tibble()
  }

  if (nrow(ave) && nrow(loading_summary)) {
    out <- merge(ave, loading_summary, by = "construct", all = TRUE, sort = FALSE)
  } else if (nrow(ave)) {
    out <- ave
    out$min_abs_loading <- NA_real_
    out$median_abs_loading <- NA_real_
    out$n_loading_review <- NA_integer_
    out$loading_concern <- FALSE
  } else if (nrow(loading_summary)) {
    out <- loading_summary
    out$block <- "overall"
    out$AVE <- NA_real_
    out$ave_attention <- "unavailable"
  } else {
    return(tibble::tibble())
  }

  # A construct with loadings but no AVE row, such as a single-indicator
  # factor, has no AVE to flag.
  out$block[is.na(out$block)] <- "overall"
  out$ave_attention[is.na(out$ave_attention)] <- "unavailable"
  out$signal <- ifelse(
    out$loading_concern | out$ave_attention == "concern",
    "concern",
    ifelse(
      out$ave_attention == "review" |
        (is.finite(out$n_loading_review) & out$n_loading_review > 0L),
      "review",
      "info"
    )
  )
  out$loading_concern <- NULL
  # In the order the model defines the constructs, as the other tables list
  # them; merge() and split() would not keep it (#145).
  out <- out[order(
    match(out$construct, nomo_validity_construct_order(x)),
    match(out$block, unique(out$block))
  ), , drop = FALSE]
  tibble::as_tibble(out[, c(
    "construct", "block", "AVE", "min_abs_loading", "median_abs_loading",
    "n_loading_review", "signal"
  ), drop = FALSE])
}


# Constructs in the order the model defines them.
# With one construct there are no pairs, and the pair tables have no columns;
# `[[` returns NULL for a missing column where tibble's `$` warns.
nomo_validity_construct_order <- function(x) {
  unique(as.character(c(
    x$standardized_loadings[["factor"]],
    x$ave[["construct"]],
    x$latent_correlations[["construct_1"]],
    x$latent_correlations[["construct_2"]]
  )))
}


# Latent correlations list a pair in model order (A, B), while the HTMT-family
# tables come from a matrix's lower triangle (B, A). Both are symmetric, so a
# pair is put in model order before tables are merged or labeled; otherwise each
# pair appears twice, once with its correlation and once with its HTMT (#89).
nomo_validity_orient_pairs <- function(tab, order) {
  if (!nrow(tab) || !all(c("construct_1", "construct_2") %in% names(tab))) {
    return(tab)
  }
  pos_1 <- match(tab$construct_1, order)
  pos_2 <- match(tab$construct_2, order)
  swap <- !is.na(pos_1) & !is.na(pos_2) & pos_1 > pos_2
  if (any(swap)) {
    first <- tab$construct_1[swap]
    tab$construct_1[swap] <- tab$construct_2[swap]
    tab$construct_2[swap] <- first
  }
  tab
}


# A pair's separation flag. HTMT2 leads and HTMT stands in for it, as before;
# the latent correlation sets the flag only where neither was computed, as the
# fix plan has it, so a pair is evaluated whenever either exists. A latent
# correlation beyond 1 is inadmissible, a concern whatever HTMT says (#145).
nomo_validity_separation_flag <- function(htmt2, htmt, latent_attention, reference) {
  primary <- ifelse(is.finite(htmt2), htmt2, htmt)
  ifelse(
    !is.finite(primary), latent_attention,
    ifelse(latent_attention %in% "concern", "concern",
           ifelse(primary > reference, "review", "info"))
  )
}


# `attention = TRUE` keeps the latent correlation's own flag, which the summary
# reads to say when it and the HTMT-family value disagree.
nomo_validity_discriminant_table <- function(x, attention = FALSE) {
  order <- nomo_validity_construct_order(x)
  latent <- nomo_validity_orient_pairs(x$latent_correlations, order)
  if (nrow(latent)) {
    # A saved object from before latent correlations were flagged (#145).
    if (!"attention" %in% names(latent)) {
      latent <- nomo_validity_latent_attention(latent, x$htmt_reference)
    }
    latent_cols <- intersect(
      c("construct_1", "construct_2", "block", "correlation", "ci_lower", "ci_upper",
        "attention"),
      names(latent)
    )
    latent <- latent[, latent_cols, drop = FALSE]
    names(latent)[names(latent) == "correlation"] <- "latent_r"
    names(latent)[names(latent) == "ci_lower"] <- "latent_r_ci_lower"
    names(latent)[names(latent) == "ci_upper"] <- "latent_r_ci_upper"
    names(latent)[names(latent) == "attention"] <- "latent_r_attention"
  }

  h2 <- nomo_validity_orient_pairs(x$htmt2, order)
  if (nrow(h2)) {
    h2 <- h2[, c("construct_1", "construct_2", "block", "estimate"), drop = FALSE]
    names(h2)[names(h2) == "estimate"] <- "HTMT2"
  }

  h1 <- nomo_validity_orient_pairs(x$htmt, order)
  if (nrow(h1)) {
    h1 <- h1[, c("construct_1", "construct_2", "block", "estimate"), drop = FALSE]
    names(h1)[names(h1) == "estimate"] <- "HTMT"
  }

  keys <- c("construct_1", "construct_2", "block")
  available <- list(latent, h2, h1)
  available <- available[vapply(available, nrow, integer(1)) > 0L]
  if (!length(available)) return(tibble::tibble())

  out <- available[[1L]]
  if (length(available) > 1L) {
    for (i in 2:length(available)) {
      out <- merge(out, available[[i]], by = keys, all = TRUE, sort = FALSE)
    }
  }

  if (!"latent_r" %in% names(out)) out$latent_r <- NA_real_
  if (!"latent_r_ci_lower" %in% names(out)) out$latent_r_ci_lower <- NA_real_
  if (!"latent_r_ci_upper" %in% names(out)) out$latent_r_ci_upper <- NA_real_
  if (!"latent_r_attention" %in% names(out)) out$latent_r_attention <- "unavailable"
  if (!"HTMT2" %in% names(out)) out$HTMT2 <- NA_real_
  if (!"HTMT" %in% names(out)) out$HTMT <- NA_real_

  out$latent_r_attention[is.na(out$latent_r_attention)] <- "unavailable"
  out$signal <- nomo_validity_separation_flag(
    out$HTMT2, out$HTMT, out$latent_r_attention, x$htmt_reference
  )
  out <- out[order(
    match(out$construct_1, order), match(out$construct_2, order), out$block
  ), , drop = FALSE]

  tibble::as_tibble(out[, c(
    keys, "latent_r", "latent_r_ci_lower", "latent_r_ci_upper",
    "HTMT2", "HTMT", "signal", if (isTRUE(attention)) "latent_r_attention"
  ), drop = FALSE])
}


# Pairs left unflagged by their HTMT-family value although their latent
# correlation could exceed the reference. The flag follows HTMT (#145), so the
# summary says so rather than showing a blank flag beside a flagged log row.
nomo_validity_latent_beside <- function(x) {
  tab <- nomo_validity_discriminant_table(x, attention = TRUE)
  if (!nrow(tab)) return(character())
  beside <- tab[tab$latent_r_attention == "review" & tab$signal == "info", , drop = FALSE]
  nomo_validity_unit(paste(beside$construct_1, beside$construct_2, sep = " vs. "),
                     beside$block)
}


# Shared by print() and summary(): the counts, the references, and how the
# separation evidence was evaluated. Constructs are counted once however many
# groups or levels they appear in, and a flag count names what it counts, so
# "none" never reads as "no separation" (#145).
nomo_validity_present_facts <- function(constructs, ngroups, nlevels, ave_reference,
                                        htmt_reference, convergent, discriminant) {
  constructs <- unique(c(constructs, convergent[["construct"]]))
  blocks <- if (isTRUE(ngroups > 1L)) {
    c("Groups", nomo_present_count(ngroups, "group"))
  } else if (isTRUE(nlevels > 1L)) {
    c("Levels", nomo_present_count(nlevels, "level"))
  } else {
    character()
  }
  nomo_present_facts(c(
    sprintf("Constructs: %d", length(constructs)),
    if (length(blocks)) sprintf("%s: %s", blocks[[1L]], sub(" .*$", "", blocks[[2L]])) else "",
    paste0("AVE reference: ", nomo_present_stat(ave_reference, "reliability")),
    paste0("HTMT reference: ", nomo_present_stat(htmt_reference, "htmt"))
  ))

  pair_cols <- intersect(c("construct_1", "construct_2"), names(discriminant))
  pairs <- nrow(unique(discriminant[pair_cols]))
  within <- if (length(blocks)) paste0(" in ", blocks[[2L]]) else ""
  convergent_text <- if (nrow(convergent)) {
    sprintf("%s (%s%s)", nomo_present_flag_counts(convergent$signal),
            nomo_present_count(length(unique(convergent$construct)), "construct"), within)
  } else {
    "not evaluated"
  }
  separation_text <- if (!pairs) {
    "not applicable (one construct)"
  } else if (all(discriminant[["signal"]] == "unavailable")) {
    sprintf("not evaluated (%s)", nomo_present_count(pairs, "pair"))
  } else {
    sprintf("%s (%s%s)", nomo_present_flag_counts(discriminant$signal),
            nomo_present_count(pairs, "pair"), within)
  }
  nomo_present_facts(c(
    paste0("Convergent flags: ", convergent_text),
    paste0("Separation flags: ", separation_text)
  ))
  invisible(NULL)
}


# What the HTMT-family evidence rests on, when it was not computed.
nomo_validity_htmt_note <- function(x) {
  status <- x$htmt_status
  if (!nrow(status) || any(status$available)) return("")
  if (!any(status$requested)) {
    return("HTMT-family values were not requested, so the latent correlations carry the construct-separation evidence.")
  }
  if (identical(x$htmt_applicable, FALSE)) {
    return(paste0(
      "HTMT-family values are not defined for this model: ",
      sub("^HTMT/HTMT2 ", "HTMT ", status$reason[[1L]])
    ))
  }
  "HTMT-family values could not be computed (see x$htmt_status), so the latent correlations carry the construct-separation evidence."
}


# A construct or pair as a flagged unit: "A", "A vs. B", and the group or level
# when the model has more than one.
nomo_validity_unit <- function(unit, block) {
  ifelse(block == "overall", unit, paste0(unit, " in ", block))
}


# One sentence per flagged construct and pair, for print().
nomo_validity_flagged_brief <- function(x, convergent, discriminant) {
  units <- character()
  status <- character()
  text <- character()

  ave_ref <- x$ave_reference
  flagged <- convergent[convergent[["signal"]] %in% c("review", "concern"), , drop = FALSE]
  for (i in seq_len(nrow(flagged))) {
    row <- flagged[i, , drop = FALSE]
    parts <- character()
    if (is.finite(row$AVE) && (row$AVE < ave_ref || row$AVE > 1 || row$AVE < 0)) {
      parts <- c(parts, if (row$AVE < 0 || row$AVE > 1) {
        sprintf("AVE %s is outside 0 to 1", nomo_present_stat(row$AVE, "reliability"))
      } else {
        sprintf("AVE %s is below the review reference %s",
                nomo_present_stat(row$AVE, "reliability", reference = ave_ref),
                nomo_present_stat(ave_ref, "reliability"))
      })
    }
    if (is.finite(row$n_loading_review) && row$n_loading_review > 0L) {
      loads <- x$standardized_loadings
      items <- loads$item[loads$factor == row$construct & loads$attention != "KEEP"]
      total <- sum(loads$factor == row$construct)
      parts <- c(parts, sprintf(
        "%d of %d standardized loadings %s flagged (%s)",
        row$n_loading_review, total,
        nomo_present_noun(row$n_loading_review, "is", "are"),
        paste(items, collapse = ", ")
      ))
    }
    if (!length(parts)) parts <- "the AVE is not admissible"
    units <- c(units, nomo_validity_unit(row$construct, row$block))
    status <- c(status, row$signal)
    text <- c(text, paste0(nomo_validity_capitalize(paste(parts, collapse = "; ")), "."))
  }

  ref <- x$htmt_reference
  flagged <- discriminant[discriminant[["signal"]] %in% c("review", "concern"), , drop = FALSE]
  for (i in seq_len(nrow(flagged))) {
    row <- flagged[i, , drop = FALSE]
    parts <- character()
    method <- if (is.finite(row$HTMT2)) "HTMT2" else "HTMT"
    value <- if (is.finite(row$HTMT2)) row$HTMT2 else row$HTMT
    if (is.finite(value) && value > ref) {
      parts <- c(parts, sprintf(
        "%s %s is above the review reference %s", method,
        nomo_present_stat(value, "htmt", reference = ref), nomo_present_stat(ref, "htmt")
      ))
    }
    r <- row$latent_r
    if (is.finite(r)) {
      limit <- max(abs(c(row$latent_r_ci_lower, row$latent_r_ci_upper)))
      read <- if (is.finite(limit)) limit else abs(r)
      if (abs(r) > 1) {
        parts <- c(parts, sprintf("latent r %s is beyond 1", nomo_present_stat(r, "r")))
      } else if (read > ref) {
        interval <- nomo_present_ci(row$latent_r_ci_lower, row$latent_r_ci_upper, kind = "r")
        parts <- c(parts, if (interval != nomo_present_missing) {
          sprintf("latent r %s, 95%% CI %s, could exceed %s", nomo_present_stat(r, "r"),
                  interval, nomo_present_stat(ref, "r"))
        } else {
          sprintf("latent r %s exceeds %s", nomo_present_stat(r, "r", reference = ref),
                  nomo_present_stat(ref, "r"))
        })
      }
    }
    units <- c(units, nomo_validity_unit(paste(row$construct_1, row$construct_2, sep = " vs. "),
                                         row$block))
    status <- c(status, row$signal)
    text <- c(text, paste0(nomo_validity_capitalize(paste(parts, collapse = "; ")), "."))
  }
  list(unit = units, status = status, text = text)
}


nomo_validity_capitalize <- function(x) {
  paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))
}


#' @export
print.nomo_validity <- function(x, ...) {
  convergent <- nomo_validity_convergent_table(x)
  discriminant <- nomo_validity_discriminant_table(x)
  nomo_present_header("nomo_validity", "Convergent and discriminant evidence")
  nomo_validity_present_facts(
    nomo_validity_construct_order(x), x$ngroups, x$nlevels, x$ave_reference,
    x$htmt_reference, convergent, discriminant
  )
  note <- nomo_validity_htmt_note(x)
  if (nzchar(note)) nomo_present_text(note)
  if (x$ngroups > 1L || x$nlevels > 1L) {
    nomo_present_text(
      "Loadings are not pooled across groups or levels; nomo_invariance() ",
      "compares them."
    )
  }

  brief <- nomo_validity_flagged_brief(x, convergent, discriminant)
  nomo_present_flagged(unit = brief$unit, status = brief$status, text = brief$text)

  cat("\n")
  key <- c(
    "AVE = average variance extracted",
    "HTMT = heterotrait-monotrait ratio, HTMT2 its geometric-mean form",
    if (any(grepl("95% CI", brief$text, fixed = TRUE))) "CI = confidence interval"
  )
  nomo_present_text(paste0(paste(key, collapse = "; "), "."))
  if (isTRUE(x$fornell_larcker_requested)) {
    nomo_present_text("The Fornell-Larcker comparison is legacy, supporting evidence only.")
  }
  nomo_present_text(
    "No single index is treated as a declaration that a construct is valid or invalid."
  )
  nomo_present_pointer("summary(x)", "the evidence for each construct and pair")
  invisible(x)
}


#' Summarize convergent and discriminant measurement evidence
#'
#' @param object A `nomo_validity` object.
#' @param ... Unused.
#'
#' @return An object of class `summary_nomo_validity`. `$convergent` combines
#'   AVE with a compact loading summary, while `$discriminant` aligns latent
#'   correlations, HTMT2, and original HTMT without repeating the full CFA
#'   loading table. Printed, it shows both tables, the legacy Fornell-Larcker
#'   comparison when it was requested, and the full reason for each flag.
#' @export
summary.nomo_validity <- function(object, ...) {
  out <- list(
    convergent = nomo_validity_convergent_table(object),
    discriminant = nomo_validity_discriminant_table(object),
    latent_beside = nomo_validity_latent_beside(object),
    htmt_status = object$htmt_status,
    htmt_applicable = object$htmt_applicable,
    ngroups = object$ngroups,
    nlevels = object$nlevels,
    ave_reference = object$ave_reference,
    htmt_reference = object$htmt_reference,
    construct_order = nomo_validity_construct_order(object),
    fornell_larcker_requested = object$fornell_larcker_requested,
    fornell_larcker = object$fornell_larcker,
    fornell_larcker_pairs = object$fornell_larcker_pairs,
    fornell_larcker_reason = object$fornell_larcker_reason,
    fit_context = object$fit_context,
    references = object$references,
    decision_log = object$decision_log
  )
  class(out) <- c("summary_nomo_validity", "list")
  out
}


#' @export
print.summary_nomo_validity <- function(x, ...) {
  nomo_present_header("nomo_validity", "Convergent and discriminant evidence",
                      summary = TRUE)
  nomo_validity_present_facts(
    x$construct_order, x$ngroups, x$nlevels, x$ave_reference, x$htmt_reference,
    x$convergent, x$discriminant
  )

  nomo_present_section("Convergent evidence by construct")
  if (nrow(x$convergent)) {
    show <- x$convergent
    show$flag <- nomo_present_status(show$signal)
    ave_ref <- x$ave_reference
    nomo_present_table(
      show,
      nomo_present_drop_constant(c(
        "Construct" = "construct", "Block" = "block", "AVE" = "AVE",
        "Min |loading|" = "min_abs_loading", "Median |loading|" = "median_abs_loading",
        "Loadings flagged" = "n_loading_review", "Flag" = "flag"
      ), "Block", show$block),
      formats = list(
        AVE = function(v) nomo_present_stat(v, "reliability", reference = ave_ref),
        min_abs_loading = function(v) nomo_present_stat(v, "loading"),
        median_abs_loading = function(v) nomo_present_stat(v, "loading")
      ),
      more = "nomo_table(x, \"convergent\")"
    )
    if (x$ngroups > 1L || x$nlevels > 1L) {
      nomo_present_text(
        "Loading summaries are left out rather than pooled across groups or levels.",
        indent = 2L
      )
    }
  } else {
    nomo_present_text("No convergent summary is available.", indent = 2L)
  }

  nomo_present_section("Construct separation")
  if (nrow(x$discriminant)) {
    show <- x$discriminant
    show$interval <- nomo_present_ci(show$latent_r_ci_lower, show$latent_r_ci_upper, kind = "r")
    show$flag <- nomo_present_status(show$signal)
    htmt <- function(v) nomo_present_stat(v, "htmt", reference = x$htmt_reference)
    nomo_present_table(
      show,
      nomo_present_drop_constant(c(
        "Construct 1" = "construct_1", "Construct 2" = "construct_2",
        "Block" = "block", "Latent r" = "latent_r", "95% CI" = "interval",
        "HTMT2" = "HTMT2", "HTMT" = "HTMT", "Flag" = "flag"
      ), "Block", show$block),
      formats = list(latent_r = function(v) nomo_present_stat(v, "r"), HTMT2 = htmt,
                     HTMT = htmt),
      more = "nomo_table(x, \"discriminant\")"
    )
    beyond <- pmax(abs(show$latent_r_ci_lower), abs(show$latent_r_ci_upper)) > 1
    if (any(beyond, na.rm = TRUE)) {
      nomo_present_text(
        "An interval that runs past 1 is lavaan's symmetric interval; a ",
        "correlation cannot reach 1, and the limit beyond the reference is what ",
        "flags the pair.",
        indent = 2L
      )
    }
    beside <- x$latent_beside
    if (length(beside)) {
      n <- length(beside)
      nomo_present_text(
        paste(beside, collapse = ", "), " ", nomo_present_noun(n, "is", "are"),
        " not flagged: ", nomo_present_noun(n, "its HTMT-family value is", "their HTMT-family values are"),
        " within the reference, although ",
        nomo_present_noun(n, "its latent correlation", "their latent correlations"),
        " could exceed it. The flag follows HTMT2 or HTMT, and the latent ",
        "correlation only where neither was computed.",
        indent = 2L
      )
    }
  } else {
    nomo_present_text("No pairwise construct-separation summary is available.", indent = 2L)
  }

  unavailable <- x$htmt_status[
    x$htmt_status$requested & !x$htmt_status$available,
    , drop = FALSE
  ]
  if (nrow(unavailable)) {
    nomo_present_section(if (identical(x$htmt_applicable, FALSE)) {
      "HTMT-family values not defined"
    } else {
      "HTMT-family values not computed"
    })
    nomo_present_bullets(paste0(unavailable$method, ": ", unavailable$reason))
  }

  if (isTRUE(x$fornell_larcker_requested)) {
    nomo_present_section("Fornell-Larcker comparison (legacy, supporting only)")
    pairs <- x$fornell_larcker_pairs
    if (is.data.frame(pairs) && nrow(pairs)) {
      pairs <- nomo_validity_orient_pairs(pairs, x$construct_order)
      pairs$flag <- nomo_present_status(pairs$attention)
      # Root AVE is compared with |r|, so both print as correlations do.
      nomo_present_table(
        pairs,
        c("Construct 1" = "construct_1", "Construct 2" = "construct_2",
          "|Latent r|" = "abs_latent_correlation", "Smaller root AVE" = "min_sqrt_ave",
          "Flag" = "flag"),
        formats = list(
          abs_latent_correlation = function(v) nomo_present_stat(v, "r"),
          min_sqrt_ave = function(v) nomo_present_stat(v, "r")
        ),
        more = "summary(x)$fornell_larcker_pairs"
      )
    } else {
      nomo_present_text(x$fornell_larcker_reason, indent = 2L)
    }
  }

  # Each flag in full, as the decision log explains it.
  log <- x$decision_log
  unit <- gsub(" vs ", " vs. ", log$object, fixed = TRUE)
  unit[unit == "measurement_model"] <- ""
  nomo_present_flagged(unit = unit, status = log$severity, text = log$observation)

  key <- c(
    AVE = "Average variance extracted, the mean share of its indicators' variance a construct explains",
    "|loading|" = "Absolute standardized loading",
    "Latent r" = "Correlation between two constructs in the CFA",
    CI = "Confidence interval, as lavaan computes it",
    HTMT2 = "Heterotrait-monotrait ratio with geometric means (Roemer et al., 2021)",
    HTMT = "Heterotrait-monotrait ratio (Henseler et al., 2015)",
    "Root AVE" = "Square root of AVE, which the Fornell-Larcker comparison sets against |r|"
  )
  # AVE and the HTMT family are named in the closing paragraph whatever the
  # tables show, so they are always defined.
  pairs <- x$fornell_larcker_pairs
  shown <- c(
    AVE = TRUE,
    "|loading|" = any(is.finite(x$convergent[["min_abs_loading"]])),
    "Latent r" = any(is.finite(x$discriminant[["latent_r"]])),
    CI = any(is.finite(x$discriminant[["latent_r_ci_lower"]])),
    HTMT2 = TRUE,
    HTMT = TRUE,
    "Root AVE" = isTRUE(x$fornell_larcker_requested) && is.data.frame(pairs) && nrow(pairs) > 0L
  )
  nomo_present_key(key[shown])

  cat("\n")
  nomo_present_text(
    "Standardized loadings and AVE address convergent evidence; latent ",
    "correlations and HTMT-family statistics address construct separation. ",
    "These are complementary questions, not interchangeable pass/fail tests."
  )
  nomo_present_pointer(
    c("nomo_table(x, \"discriminant\")", "x$decision_log"),
    c("every value", "the reasoning behind each flag")
  )
  invisible(x)
}


#' Plot convergent or discriminant validity evidence
#'
#' @param x A `nomo_validity` object.
#' @param type Plot type: `"ave"` or `"discriminant"`. The discriminant plot
#'   draws each pair at the value that sets its flag: HTMT2, HTMT when HTMT2
#'   was not computed, and the latent correlation with its interval when
#'   neither was.
#' @param ... Unused.
#' @return A `ggplot2` object. The conceptual 0-to-1 coefficient range is shown
#'   by default and expands only if an empirical value lies outside it. Each
#'   point's shape and color show its flag, the same flag as in `summary()` and
#'   [nomo_table()], with a legend whenever a point is flagged.
#' @export
plot.nomo_validity <- function(x, type = c("ave", "discriminant"), ...) {
  type <- nomo_match_arg(type)

  if (type == "ave") {
    dat <- x$ave
    dat <- dat[is.finite(dat$estimate), , drop = FALSE]
    if (!nrow(dat)) stop("No finite AVE estimates are available to plot.", call. = FALSE)

    dat$construct <- factor(dat$construct, levels = rev(unique(dat$construct)))
    dat$flag <- nomo_plot_status(dat$attention)
    ref <- x$ave_reference
    p <- ggplot2::ggplot(
      dat, ggplot2::aes(x = estimate, y = construct, shape = flag, colour = flag)
    ) +
      ggplot2::geom_vline(xintercept = ref, linetype = 2) +
      ggplot2::geom_point(size = 3) +
      nomo_plot_status_scales(dat$flag) +
      ggplot2::scale_x_continuous(labels = nomo_plot_bounded_labels) +
      ggplot2::coord_cartesian(xlim = nomo_plot_x_limits(dat$estimate)) +
      nomo_plot_labs(
        title = "Convergent evidence: AVE",
        subtitle = paste0(
          "Dashed line = review reference (", nomo_present_stat(ref, "reliability"),
          "). Item loadings remain in the CFA plot."
        ),
        x = "Average variance extracted (AVE)",
        y = NULL,
        caption = "AVE is convergent-validity evidence, not reliability."
      ) +
      ggplot2::theme_minimal()

    if (length(unique(dat$block)) > 1L) {
      p <- p + ggplot2::facet_wrap(stats::as.formula("~ block"))
    }
    return(p)
  }

  # Each pair is drawn at the value that sets its flag, with the flag the
  # summary and nomo_table() show: HTMT2, HTMT where HTMT2 is missing, and the
  # latent correlation with its interval where neither was computed (#145).
  dat <- nomo_validity_discriminant_table(x)
  if (!nrow(dat)) {
    stop(
      "No construct pairs are available to plot; inspect `x$latent_correlations` ",
      "and `x$htmt_status`.",
      call. = FALSE
    )
  }
  dat$source <- ifelse(is.finite(dat$HTMT2), "HTMT2",
                       ifelse(is.finite(dat$HTMT), "HTMT", "latent r"))
  dat$estimate <- ifelse(dat$source == "HTMT2", dat$HTMT2,
                         ifelse(dat$source == "HTMT", dat$HTMT, dat$latent_r))
  dat$pair <- paste(dat$construct_1, dat$construct_2, sep = " vs. ")
  missing <- dat[!is.finite(dat$estimate), , drop = FALSE]
  dat <- dat[is.finite(dat$estimate), , drop = FALSE]
  if (!nrow(dat)) {
    stop(
      "No HTMT-family value or latent correlation is available to plot; inspect ",
      "`x$htmt_status` and `x$latent_correlations`.",
      call. = FALSE
    )
  }

  sources <- intersect(c("HTMT2", "HTMT", "latent r"), dat$source)
  latent <- dat$source == "latent r"
  # With mixed sources, the pairs read from the latent correlation say so.
  if (length(sources) > 1L) dat$pair[latent] <- paste0(dat$pair[latent], " (latent r)")
  dat$pair <- factor(dat$pair, levels = rev(unique(dat$pair)))
  dat$flag <- nomo_plot_status(dat$signal)
  dat$ci_lower <- ifelse(latent, dat$latent_r_ci_lower, NA_real_)
  dat$ci_upper <- ifelse(latent, dat$latent_r_ci_upper, NA_real_)
  intervals <- dat[is.finite(dat$ci_lower) & is.finite(dat$ci_upper), , drop = FALSE]

  ref <- x$htmt_reference
  only_latent <- identical(sources, "latent r")
  kind <- if (only_latent) "r" else "htmt"
  # A negative latent correlation is read by its size, so the reference is
  # drawn on both sides.
  lines <- if (any(dat$estimate[latent] < 0)) c(-ref, ref) else ref
  subtitle <- paste0("Dashed line = review reference (", nomo_present_stat(ref, kind), ").")
  if (nrow(intervals)) {
    subtitle <- paste(
      subtitle,
      "Bars = 95% confidence interval of latent r; its limit farthest from zero",
      "is compared with the reference."
    )
  }
  caption <- "Values past the line prompt investigation; they do not mandate merging."
  if (nrow(missing)) {
    caption <- paste0(
      caption, " Not drawn: ", paste(unique(missing$pair), collapse = ", "),
      ", with no value computed."
    )
  }
  label <- nomo_validity_capitalize(nomo_present_or(sources))

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = estimate, y = pair)) +
    ggplot2::geom_vline(xintercept = lines, linetype = 2) +
    ggplot2::geom_segment(
      data = intervals,
      ggplot2::aes(x = ci_lower, xend = ci_upper, y = pair, yend = pair),
      colour = "grey60", linewidth = 0.7
    ) +
    ggplot2::geom_point(ggplot2::aes(shape = flag, colour = flag), size = 3) +
    nomo_plot_status_scales(dat$flag) +
    ggplot2::coord_cartesian(
      xlim = nomo_plot_x_limits(c(dat$estimate, intervals$ci_lower, intervals$ci_upper))
    ) +
    nomo_plot_labs(
      title = paste0("Construct separation: ", nomo_present_or(sources, "and")),
      subtitle = subtitle,
      x = label,
      y = NULL,
      caption = caption
    ) +
    ggplot2::theme_minimal()
  if (only_latent) {
    p <- p + ggplot2::scale_x_continuous(labels = nomo_plot_bounded_labels)
  }

  if (length(unique(dat$block)) > 1L) {
    p <- p + ggplot2::facet_wrap(stats::as.formula("~ block"))
  }
  p
}

utils::globalVariables(c(
  "estimate", "construct", "flag", "block", "pair", "ci_lower", "ci_upper"
))
