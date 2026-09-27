# Presentation methods for convergent/discriminant evidence -------------------

nomo_validity_convergent_table <- function(x) {
  ave <- x$ave[, c("construct", "block", "estimate", "attention"), drop = FALSE]
  if (nrow(ave)) {
    names(ave)[names(ave) == "estimate"] <- "AVE"
    names(ave)[names(ave) == "attention"] <- "ave_attention"
  }

  loads <- x$standardized_loadings
  # The v0.1 loading table is intentionally a compact first-order CFA view and
  # does not carry group/level labels. Do not silently pool those loadings in a
  # multi-group/multilevel summary; M7 will handle block-specific invariance.
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
  tibble::as_tibble(out[, c(
    "construct", "block", "AVE", "min_abs_loading", "median_abs_loading",
    "n_loading_review", "signal"
  ), drop = FALSE])
}


# Constructs in the order the model defines them.
nomo_validity_construct_order <- function(x) {
  unique(as.character(c(
    x$standardized_loadings$factor,
    x$latent_correlations$construct_1,
    x$latent_correlations$construct_2
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


nomo_validity_discriminant_table <- function(x) {
  order <- nomo_validity_construct_order(x)
  latent <- nomo_validity_orient_pairs(x$latent_correlations, order)
  if (nrow(latent)) {
    latent_cols <- intersect(
      c("construct_1", "construct_2", "block", "correlation", "ci_lower", "ci_upper"),
      names(latent)
    )
    latent <- latent[, latent_cols, drop = FALSE]
    names(latent)[names(latent) == "correlation"] <- "latent_r"
    names(latent)[names(latent) == "ci_lower"] <- "latent_r_ci_lower"
    names(latent)[names(latent) == "ci_upper"] <- "latent_r_ci_upper"
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
  if (!"HTMT2" %in% names(out)) out$HTMT2 <- NA_real_
  if (!"HTMT" %in% names(out)) out$HTMT <- NA_real_

  primary <- ifelse(is.finite(out$HTMT2), out$HTMT2, out$HTMT)
  out$signal <- ifelse(
    !is.finite(primary), "unavailable",
    ifelse(primary > x$htmt_reference, "review", "info")
  )
  out <- out[order(
    match(out$construct_1, order), match(out$construct_2, order), out$block
  ), , drop = FALSE]

  tibble::as_tibble(out[, c(
    keys, "latent_r", "latent_r_ci_lower", "latent_r_ci_upper",
    "HTMT2", "HTMT", "signal"
  ), drop = FALSE])
}

#' @export
print.nomo_validity <- function(x, ...) {
  convergent <- nomo_validity_convergent_table(x)
  discriminant <- nomo_validity_discriminant_table(x)
  nomo_present_header("nomo_validity", "Convergent and discriminant evidence")
  nomo_present_facts(c(
    sprintf("Constructs: %d", length(unique(x$standardized_loadings$factor))),
    sprintf("AVE review reference: %s", format(x$ave_reference, trim = TRUE)),
    sprintf("HTMT-family review reference: %s", format(x$htmt_reference, trim = TRUE))
  ))

  if (nrow(convergent)) {
    nomo_present_facts(sprintf(
      "Convergent evidence: %s across %d construct(s)",
      nomo_present_flag_counts(convergent$signal), nrow(convergent)
    ))
  }
  if (nrow(discriminant)) {
    nomo_present_facts(sprintf(
      "Construct separation: %s across %d pair(s)",
      nomo_present_flag_counts(discriminant$signal), nrow(discriminant)
    ))
  } else if (any(x$htmt_status$requested & !x$htmt_status$available)) {
    nomo_present_text(
      "HTMT-family evidence: requested but unavailable; inspect `$htmt_status`."
    )
  } else if (!any(x$htmt_status$requested)) {
    nomo_present_text("HTMT-family evidence: not requested.")
  }

  if (x$ngroups > 1L || x$nlevels > 1L) {
    nomo_present_text(
      "Block-specific loading ranges are not silently pooled; use the fitted ",
      "CFA and later invariance workflow for group/level comparisons."
    )
  }
  if (isTRUE(x$fornell_larcker_requested)) {
    nomo_present_text("Fornell-Larcker: legacy/supporting output only.")
  }
  nomo_present_text(
    "No single index is treated as a declaration that a construct is valid or invalid."
  )
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
#'   loading table.
#' @export
summary.nomo_validity <- function(object, ...) {
  out <- list(
    convergent = nomo_validity_convergent_table(object),
    discriminant = nomo_validity_discriminant_table(object),
    htmt_status = object$htmt_status,
    ngroups = object$ngroups,
    nlevels = object$nlevels,
    fornell_larcker_requested = object$fornell_larcker_requested,
    fornell_larcker = object$fornell_larcker,
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

  nomo_present_section("Convergent evidence by construct")
  if (nrow(x$convergent)) {
    show <- x$convergent
    show$flag <- nomo_present_flag(show$signal)
    nomo_present_table(
      show,
      nomo_present_drop_constant(c(
        "Construct" = "construct", "Block" = "block", "AVE" = "AVE",
        "Min |loading|" = "min_abs_loading", "Median |loading|" = "median_abs_loading",
        "Loadings flagged" = "n_loading_review", "Flag" = "flag"
      ), "Block", show$block),
      more = "nomo_table(x, \"convergent\")"
    )
    if (x$ngroups > 1L || x$nlevels > 1L) {
      nomo_present_text(
        "Loading ranges are omitted here rather than pooled across groups/levels.",
        indent = 2L
      )
    }
  } else {
    nomo_present_text("No convergent summary is available.", indent = 2L)
  }

  nomo_present_section("Construct separation")
  if (nrow(x$discriminant)) {
    show <- x$discriminant
    show$interval <- nomo_present_ci(show$latent_r_ci_lower, show$latent_r_ci_upper)
    show$flag <- nomo_present_flag(show$signal)
    nomo_present_table(
      show,
      nomo_present_drop_constant(c(
        "Construct 1" = "construct_1", "Construct 2" = "construct_2",
        "Block" = "block", "Latent r" = "latent_r", "95% CI" = "interval",
        "HTMT2" = "HTMT2", "HTMT" = "HTMT", "Flag" = "flag"
      ), "Block", show$block),
      more = "nomo_table(x, \"discriminant\")"
    )
  } else {
    nomo_present_text("No pairwise construct-separation summary is available.", indent = 2L)
  }

  unavailable <- x$htmt_status[
    x$htmt_status$requested & !x$htmt_status$available,
    , drop = FALSE
  ]
  if (nrow(unavailable)) {
    nomo_present_section("Unavailable requested HTMT-family evidence")
    nomo_present_bullets(paste0(unavailable$method, ": ", unavailable$reason))
  }

  cat("\n")
  if (isTRUE(x$fornell_larcker_requested)) {
    nomo_present_text(
      "Fornell-Larcker was requested as legacy/supporting information only."
    )
  }

  nomo_present_text(
    "Standardized loadings and AVE address convergent evidence; latent ",
    "correlations and HTMT-family statistics address construct separation. ",
    "These are complementary questions, not interchangeable pass/fail tests."
  )
  invisible(x)
}


#' Plot convergent or discriminant validity evidence
#'
#' @param x A `nomo_validity` object.
#' @param type Plot type: `"ave"` or `"discriminant"`.
#' @param ... Unused.
#' @return A `ggplot2` object. The conceptual 0-to-1 coefficient range is shown
#'   by default and expands only if an empirical value lies outside it.
#' @export
plot.nomo_validity <- function(x, type = c("ave", "discriminant"), ...) {
  type <- match.arg(type)

  if (type == "ave") {
    dat <- x$ave
    dat <- dat[is.finite(dat$estimate), , drop = FALSE]
    if (!nrow(dat)) stop("No finite AVE estimates are available to plot.", call. = FALSE)

    dat$construct <- factor(dat$construct, levels = rev(unique(dat$construct)))
    dat$flag <- nomo_present_flag_legend(dat$attention)
    ref <- x$ave_reference
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = estimate, y = construct, shape = flag)) +
      ggplot2::geom_vline(xintercept = ref, linetype = 2) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_shape_manual(values = nomo_present_flag_shapes, drop = TRUE) +
      ggplot2::coord_cartesian(xlim = nomo_plot_x_limits(dat$estimate)) +
      nomo_plot_labs(
        title = "Convergent evidence: AVE",
        subtitle = paste0(
          "Dashed line = review reference (", format(ref, trim = TRUE),
          "). Item loadings remain in the CFA plot."
        ),
        x = "Average variance extracted",
        y = NULL,
        shape = "Flag",
        caption = "AVE is convergent-validity evidence, not reliability."
      ) +
      ggplot2::theme_minimal()

    if (length(unique(dat$flag)) == 1L) p <- p + ggplot2::guides(shape = "none")
    if (length(unique(dat$block)) > 1L) {
      p <- p + ggplot2::facet_wrap(stats::as.formula("~ block"))
    }
    return(p)
  }

  dat <- x$htmt2
  method <- "HTMT2"
  if (!nrow(dat)) {
    dat <- x$htmt
    method <- "HTMT"
  }
  dat <- dat[is.finite(dat$estimate), , drop = FALSE]
  if (!nrow(dat)) {
    stop(
      "No finite HTMT-family estimates are available to plot; inspect `x$htmt_status`.",
      call. = FALSE
    )
  }

  dat <- nomo_validity_orient_pairs(dat, nomo_validity_construct_order(x))
  dat$pair <- paste(dat$construct_1, dat$construct_2, sep = " vs ")
  dat$flag <- nomo_present_flag_legend(
    ifelse(dat$estimate > x$htmt_reference, "review", "info")
  )
  dat$pair <- factor(dat$pair, levels = rev(unique(dat$pair)))
  ref <- x$htmt_reference

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = estimate, y = pair, shape = flag)) +
    ggplot2::geom_vline(xintercept = ref, linetype = 2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_shape_manual(values = nomo_present_flag_shapes, drop = TRUE) +
    ggplot2::coord_cartesian(xlim = nomo_plot_x_limits(dat$estimate)) +
    nomo_plot_labs(
      title = paste0("Construct separation: ", method),
      subtitle = paste0(
        "Dashed line = review reference (", format(ref, trim = TRUE), ")."
      ),
      x = method,
      y = NULL,
      shape = "Flag",
      caption = "Values above the line prompt investigation; they do not mandate merging."
    ) +
    ggplot2::theme_minimal()

  if (length(unique(dat$flag)) == 1L) p <- p + ggplot2::guides(shape = "none")
  if (length(unique(dat$block)) > 1L) {
    p <- p + ggplot2::facet_wrap(stats::as.formula("~ block"))
  }
  p
}

utils::globalVariables(c(
  "estimate", "construct", "flag", "block", "pair"
))
