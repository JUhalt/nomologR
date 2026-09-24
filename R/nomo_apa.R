# Manuscript-ready tables ------------------------------------------------------
#
# The formatting rules follow the Publication Manual of the American
# Psychological Association (7th ed.), as summarized by Purdue OWL; APA's own
# site blocks automated access, so the summary was quoted rather than the
# Manual. See #35 for the rules and their sources.
#
# Every non-ASCII symbol is written as a \u escape so R CMD check has nothing to
# flag in the code itself.

nomo_apa_dash <- "\u2014"


# A null default, written out because the base R operator for it arrived only in
# R 4.4 and this package supports R 4.2.
nomo_apa_or <- function(x, y) if (is.null(x)) y else x


# First letter capitalized, as for a model name in a table stub. A helper rather
# than tools::toTitleCase(), which would be an undeclared import.
nomo_apa_capitalize <- function(x) {
  x <- as.character(x)
  paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))
}


# Whether a statistic can exceed 1 decides whether it keeps its leading zero:
# "If the statistic can be greater than 1, use a leading 0 ... If the statistic
# cannot be greater than 1, do not use a leading 0." The rule turns on the
# theoretical bound, not the typical value, so TLI (which can exceed 1) keeps
# its zero while CFI (which cannot) loses it.
nomo_apa_number <- function(x, digits = 2L, bounded = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  # Round first so a small negative value that rounds to zero is written as
  # zero, not as a signed "-.000".
  x <- round(x, digits)
  x[is.finite(x) & x == 0] <- 0
  out <- formatC(x, format = "f", digits = digits)
  if (isTRUE(bounded)) {
    out <- sub("^(-?)0\\.", "\\1.", out)
  }
  out[!is.finite(x)] <- nomo_apa_dash
  out
}


# p values are bounded, so they take no leading zero, and anything below .001
# is written as a bound rather than a rounded zero.
nomo_apa_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  out <- nomo_apa_number(p, digits = 3L, bounded = TRUE)
  out[is.finite(p) & p < 0.001] <- "< .001"
  out
}


nomo_apa_interval <- function(estimate, lower, upper, digits = 2L,
                              bounded = FALSE) {
  est <- nomo_apa_number(estimate, digits, bounded)
  lo <- nomo_apa_number(lower, digits, bounded)
  hi <- nomo_apa_number(upper, digits, bounded)
  has_ci <- is.finite(suppressWarnings(as.numeric(lower))) &
    is.finite(suppressWarnings(as.numeric(upper)))
  ifelse(has_ci, sprintf("%s [%s, %s]", est, lo, hi), est)
}


nomo_apa_new <- function(body, title, stub, general = character(),
                         specific = character(), probability = character(),
                         number = NULL, source = "") {
  if (!is.null(number)) {
    if (!is.numeric(number) || length(number) != 1L || !is.finite(number) ||
          number < 1 || number != round(number)) {
      stop("`number` must be NULL or one whole number of at least 1.", call. = FALSE)
    }
    number <- as.integer(number)
  }
  body[] <- lapply(body, as.character)
  out <- list(
    number = number,
    title = title,
    body = body,
    stub = stub,
    notes = list(general = general, specific = specific, probability = probability),
    source = source
  )
  class(out) <- c("nomo_apa_table", "list")
  out
}


# Tables -----------------------------------------------------------------------

#' Manuscript-ready tables in APA style
#'
#' Formats the evidence in a `nomologR` result as a table ready for a thesis,
#' dissertation, or manuscript, following APA 7 conventions: a bold table
#' number, an italic title, no vertical rules, and notes below the table in the
#' order general, specific, probability.
#'
#' @details
#' **Leading zeros follow the statistic, not the value.** APA 7 drops the
#' leading zero only for statistics that *cannot* exceed 1. So *p* values,
#' correlations, reliability coefficients, and CFI are written `.95`, while TLI,
#' RMSEA, SRMR, and standardized loadings keep it, `0.95`, because each can
#' exceed 1 in principle: TLI is not bounded above, and a standardized loading
#' does in an improper (Heywood) solution. Many published tables print
#' standardized loadings without the zero; this follows the rule as written.
#'
#' **No verdicts.** Table notes keep the package's reference-value language.
#' No cell reads PASS or FAIL, and fit indices are not labeled good or poor.
#'
#' The rules come from the *Publication Manual of the American Psychological
#' Association* (7th ed.), checked against Purdue OWL's APA 7 guides.
#' Journal-specific templates are out of scope.
#'
#' @param x A result object: `nomo_cfa`, `nomo_reliability`, `nomo_invariance`,
#'   or `nomo_network`.
#' @param type Which table to build. For `nomo_cfa`: `"loadings"`, `"fit"`, or
#'   `"factor_correlations"`. For `nomo_network`: `"hypotheses"` or `"fit"`.
#'   Other objects have one table each.
#' @param number Optional table number, printed in bold as "Table 1".
#' @param title Optional title; a descriptive default is supplied.
#' @param ... Unused.
#'
#' @return A `nomo_apa_table` object, which prints in the console and renders
#'   as a formatted table, with its title and notes, when knitted.
#'
#' @references
#' American Psychological Association. (2020). *Publication manual of the
#' American Psychological Association* (7th ed.).
#' \doi{10.1037/0000165-000}
#'
#' @export
nomo_apa_table <- function(x, type = NULL, number = NULL, title = NULL, ...) {
  UseMethod("nomo_apa_table")
}


#' @export
nomo_apa_table.default <- function(x, type = NULL, number = NULL, title = NULL, ...) {
  stop(
    paste0(
      "No APA table is available for an object of class `", class(x)[[1L]],
      "`. Supported: nomo_cfa, nomo_reliability, nomo_invariance, nomo_network."
    ),
    call. = FALSE
  )
}


#' @export
nomo_apa_table.nomo_cfa <- function(x, type = c("loadings", "fit", "factor_correlations"),
                                    number = NULL, title = NULL, ...) {
  type <- match.arg(type)
  sample_note <- sprintf(
    "Estimated with %s; *N* = %d.", x$estimator, as.integer(x$n_used)
  )

  if (type == "loadings") {
    sl <- x$standardized_loadings
    factors <- unique(as.character(sl$factor))
    items <- unique(as.character(sl$item))
    body <- data.frame(Item = items, stringsAsFactors = FALSE)
    for (f in factors) {
      rows <- sl[sl$factor == f, , drop = FALSE]
      body[[f]] <- ifelse(
        items %in% rows$item,
        nomo_apa_number(rows$loading[match(items, rows$item)], 2L, bounded = FALSE),
        ""
      )
    }
    return(nomo_apa_new(
      body = body,
      title = nomo_apa_or(title, "Standardized Factor Loadings"),
      stub = "Item",
      general = paste(
        "Standardized loadings from a confirmatory factor analysis.",
        sample_note,
        "Blank cells are loadings fixed to zero by the model."
      ),
      number = number, source = "nomo_cfa"
    ))
  }

  if (type == "factor_correlations") {
    fc <- x$factor_correlations
    if (!nrow(fc)) {
      stop("The model has one factor, so there are no factor correlations.", call. = FALSE)
    }
    body <- data.frame(
      Factors = paste(fc$factor1, "with", fc$factor2),
      r = nomo_apa_interval(fc$correlation, fc$ci_lower, fc$ci_upper, bounded = TRUE),
      p = nomo_apa_p(fc$p_value),
      stringsAsFactors = FALSE
    )
    names(body) <- c("Factors", "*r* [95% CI]", "*p*")
    return(nomo_apa_new(
      body = body,
      title = nomo_apa_or(title, "Factor Correlations"),
      stub = "Factors",
      general = paste("Latent correlations from the confirmatory factor analysis.", sample_note),
      number = number, source = "nomo_cfa"
    ))
  }

  nomo_apa_fit_table(
    x$fit_evidence, estimator = x$estimator, n = x$n_used,
    title = title, number = number, source = "nomo_cfa"
  )
}


nomo_apa_fit_table <- function(fe, estimator, n, title, number, source,
                               model_label = "Measurement model") {
  value <- function(metric) {
    v <- fe$value[fe$metric == metric]
    if (length(v)) v[[1L]] else NA_real_
  }
  rmsea <- nomo_apa_interval(
    value("RMSEA"), value("RMSEA_CI_lower"), value("RMSEA_CI_upper"),
    digits = 3L, bounded = FALSE
  )
  body <- data.frame(
    Model = model_label,
    chisq = nomo_apa_number(value("chi_square"), 2L),
    df = nomo_apa_number(value("df"), 0L),
    p = nomo_apa_p(value("p_value")),
    CFI = nomo_apa_number(value("CFI"), 3L, bounded = TRUE),
    TLI = nomo_apa_number(value("TLI"), 3L, bounded = FALSE),
    RMSEA = rmsea,
    SRMR = nomo_apa_number(value("SRMR"), 3L, bounded = FALSE),
    stringsAsFactors = FALSE
  )
  names(body) <- c(
    "Model", "\u03c7\u00b2", "*df*", "*p*", "CFI", "TLI", "RMSEA [90% CI]", "SRMR"
  )
  nomo_apa_new(
    body = body,
    title = nomo_apa_or(title, "Model Fit"),
    stub = "Model",
    general = paste(
      sprintf("Estimated with %s; *N* = %d.", estimator, as.integer(n)),
      "CFI = comparative fit index; TLI = Tucker-Lewis index; RMSEA = root mean",
      "square error of approximation; SRMR = standardized root mean square",
      "residual. Fit indices are reported as evidence, not against fixed",
      "cutoffs."
    ),
    number = number, source = source
  )
}


#' @export
nomo_apa_table.nomo_reliability <- function(x, type = NULL, number = NULL,
                                            title = NULL, ...) {
  constructs <- unique(c(as.character(x$omega$construct), as.character(x$alpha$construct)))
  pick <- function(tbl, construct) {
    row <- tbl[tbl$construct == construct, , drop = FALSE]
    if (!nrow(row)) return(nomo_apa_dash)
    nomo_apa_interval(row$estimate[[1L]], row$ci_lower[[1L]], row$ci_upper[[1L]],
                      bounded = TRUE)
  }
  body <- data.frame(
    Construct = constructs,
    omega = vapply(constructs, pick, character(1), tbl = x$omega, USE.NAMES = FALSE),
    alpha = vapply(constructs, pick, character(1), tbl = x$alpha, USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  has_ci <- any(is.finite(x$omega$ci_lower))
  names(body) <- c(
    "Construct",
    if (has_ci) "\u03c9 [95% CI]" else "\u03c9",
    if (has_ci) "\u03b1 [95% CI]" else "\u03b1"
  )
  nomo_apa_new(
    body = body,
    title = nomo_apa_or(title, "Reliability Estimates"),
    stub = "Construct",
    general = paste(
      "\u03c9 = coefficient omega; \u03b1 = coefficient alpha.",
      if (has_ci) "Intervals are percentile bootstrap intervals." else "",
      "Coefficient alpha assumes equal loadings and is reported alongside omega",
      "for comparison with published work."
    ),
    number = number, source = "nomo_reliability"
  )
}


#' @export
nomo_apa_table.nomo_invariance <- function(x, type = NULL, number = NULL,
                                           title = NULL, ...) {
  fe <- x$fit_evidence
  change <- function(v) nomo_apa_number(v, 3L, bounded = TRUE)
  lrt <- ifelse(
    is.finite(fe$lrt_chisq),
    sprintf("%s (%s)", nomo_apa_number(fe$lrt_chisq, 2L), nomo_apa_number(fe$lrt_df, 0L)),
    nomo_apa_dash
  )
  body <- data.frame(
    Model = nomo_apa_capitalize(as.character(fe$level)),
    chisq = nomo_apa_number(fe$chisq, 2L),
    df = nomo_apa_number(fe$df, 0L),
    CFI = nomo_apa_number(fe$cfi, 3L, bounded = TRUE),
    RMSEA = nomo_apa_number(fe$rmsea, 3L, bounded = FALSE),
    SRMR = nomo_apa_number(fe$srmr, 3L, bounded = FALSE),
    dCFI = change(fe$delta_cfi),
    dRMSEA = nomo_apa_number(fe$delta_rmsea, 3L, bounded = FALSE),
    lrt = lrt,
    p = nomo_apa_p(fe$lrt_p),
    stringsAsFactors = FALSE
  )
  names(body) <- c(
    "Model", "\u03c7\u00b2", "*df*", "CFI", "RMSEA", "SRMR",
    "\u0394CFI", "\u0394RMSEA", "\u0394\u03c7\u00b2 (\u0394*df*)", "*p*"
  )
  nomo_apa_new(
    body = body,
    title = nomo_apa_or(title, "Measurement Invariance Across Groups"),
    stub = "Model",
    general = paste(
      sprintf("Grouping variable: %s.", x$group),
      "Each model adds constraints to the one above it; changes are relative to",
      "the preceding model. Changes in fit are reported as evidence and are not",
      "compared with fixed cutoffs."
    ),
    number = number, source = "nomo_invariance"
  )
}


#' @export
nomo_apa_table.nomo_network <- function(x, type = c("hypotheses", "fit"),
                                        number = NULL, title = NULL, ...) {
  type <- match.arg(type)
  if (type == "fit") {
    fe <- x$fit_evidence
    # The network's fit evidence keeps the RMSEA without its interval, so the
    # interval is read from the fit: the robust, scaled, or plain one, matching
    # the RMSEA the evidence reports.
    measures <- tryCatch(lavaan::fitMeasures(x$fit), error = function(e) numeric())
    variants <- c(".robust", ".scaled", "")
    reported <- vapply(variants, function(v) {
      is.finite(nomo_network_fit_measure(measures, paste0("rmsea", v)))
    }, logical(1))
    # Without any RMSEA the plain interval is absent too, so the cell is empty.
    variant <- if (any(reported)) variants[reported][[1L]] else ""
    ci <- function(bound) {
      nomo_network_fit_measure(measures, paste0("rmsea.ci.", bound, variant))
    }
    long <- data.frame(
      metric = c("chi_square", "df", "p_value", "CFI", "TLI", "RMSEA",
                 "RMSEA_CI_lower", "RMSEA_CI_upper", "SRMR"),
      value = c(fe$chisq, fe$df, fe$pvalue, fe$cfi, fe$tli, fe$rmsea,
                ci("lower"), ci("upper"), fe$srmr),
      stringsAsFactors = FALSE
    )
    return(nomo_apa_fit_table(
      long, estimator = "the network model", n = nomo_apa_or(x$data_n, NA_integer_),
      title = nomo_apa_or(title, "Fit of the Nomological Network Model"),
      model_label = "Nomological network",
      number = number, source = "nomo_network"
    ))
  }

  he <- x$hypothesis_evidence

  # `estimate` is the value its interval belongs to, on the scale `scale`
  # names; the standardized and unstandardized columns are not always that one.
  # A standardized correlation cannot exceed 1 and loses its leading zero; a
  # standardized path can, with more than one predictor, and keeps it.
  bounded <- as.character(he$scale) == "standardized" &
    as.character(he$relation_type) == "association"
  estimate <- vapply(seq_len(nrow(he)), function(i) {
    nomo_apa_interval(he$estimate[[i]], he$ci_lower[[i]], he$ci_upper[[i]],
                      bounded = bounded[[i]])
  }, character(1))

  # Post hoc hypotheses are marked, because a prediction written after the data
  # were seen is exploratory however the estimate turns out.
  post_hoc <- as.character(he$confirmatory_status) != "a_priori"
  label <- paste0(he$id, ": ", he$relation)
  if (any(post_hoc)) label[post_hoc] <- paste0(label[post_hoc], "^a^")

  body <- data.frame(
    Hypothesis = label,
    Prediction = as.character(he$prediction),
    Estimate = estimate,
    Evidence = nomo_apa_concordance(he$concordance),
    stringsAsFactors = FALSE
  )
  names(body) <- c("Hypothesis", "Prediction", "Estimate [95% CI]", "Evidence")

  scales <- unique(as.character(he$scale))
  nomo_apa_new(
    body = body,
    title = nomo_apa_or(title, "Theory-Specified Relations"),
    stub = "Hypothesis",
    general = paste(
      sprintf("Estimates are on the %s scale.", paste(scales, collapse = " and ")),
      "Evidence describes how each estimate relates to the prediction",
      "registered for it; it is evidence about the prediction, not a verdict on",
      "the measure."
    ),
    specific = if (any(post_hoc)) {
      "Specified after the data were seen, so this relation is exploratory."
    } else {
      character()
    },
    number = number, source = "nomo_network"
  )
}


# Evidence labels in plain words. They describe the relation between an
# estimate and its prediction, and none is a pass or a fail.
nomo_apa_concordance <- function(x) {
  labels <- c(
    concordant = "Concordant",
    directionally_concordant_imprecise = "Direction concordant, imprecise",
    direction_concordant_below_magnitude = "Direction concordant, below predicted magnitude",
    inconclusive = "Inconclusive",
    inconsistent = "Inconsistent",
    not_evaluable = "Not evaluable",
    not_confirmable_without_sesoi = "Not confirmable without a smallest effect of interest"
  )
  x <- as.character(x)
  out <- unname(labels[x])
  unknown <- is.na(out)
  out[unknown] <- nomo_apa_capitalize(gsub("_", " ", x[unknown]))
  out
}


# Printing ---------------------------------------------------------------------

nomo_apa_notes_text <- function(notes) {
  parts <- character()
  general <- notes$general[nzchar(notes$general)]
  if (length(general)) {
    parts <- c(parts, paste("*Note.*", paste(general, collapse = " ")))
  }
  specific <- notes$specific[nzchar(notes$specific)]
  if (length(specific)) {
    parts <- c(parts, paste0("^", letters[seq_along(specific)], "^ ", specific, collapse = " "))
  }
  probability <- notes$probability[nzchar(notes$probability)]
  if (length(probability)) {
    parts <- c(parts, paste(probability, collapse = " "))
  }
  gsub("  +", " ", parts)
}


#' @export
print.nomo_apa_table <- function(x, ...) {
  strip <- function(s) gsub("\\*|\\^", "", s)
  heading <- if (is.null(x$number)) "Table" else sprintf("Table %d", x$number)
  cat(heading, "\n", x$title, "\n", sep = "")

  body <- x$body
  names(body) <- strip(names(body))
  widths <- pmax(nchar(names(body)), vapply(body, function(col) max(nchar(col), 0L), integer(1)))
  rule <- strrep("-", sum(widths) + 2L * (length(widths) - 1L))
  line <- function(cells) {
    paste(mapply(function(cell, w, i) {
      if (i == 1L) formatC(cell, width = -w) else formatC(cell, width = w)
    }, cells, widths, seq_along(cells)), collapse = "  ")
  }
  cat(rule, "\n", line(names(body)), "\n", rule, "\n", sep = "")
  for (i in seq_len(nrow(body))) cat(line(unlist(body[i, ])), "\n", sep = "")
  cat(rule, "\n", sep = "")

  notes <- nomo_apa_notes_text(x$notes)
  if (length(notes)) cat(strip(notes), sep = "\n")
  invisible(x)
}


# Knitted, the table becomes markdown that pandoc renders as HTML or Word: a
# bold number, an italic title, a table with the stub column left-aligned and
# the rest centered, and the notes below.
nomo_apa_markdown <- function(x) {
  body <- x$body
  align <- c(":---", rep(":---:", ncol(body) - 1L))
  row <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")
  c(
    if (is.null(x$number)) "**Table**" else sprintf("**Table %d**", x$number),
    "",
    sprintf("*%s*", x$title),
    "",
    row(names(body)),
    row(align),
    vapply(seq_len(nrow(body)), function(i) row(unlist(body[i, ])), character(1)),
    "",
    nomo_apa_notes_text(x$notes)
  )
}


#' @exportS3Method knitr::knit_print
knit_print.nomo_apa_table <- function(x, ...) {
  knitr::asis_output(paste(nomo_apa_markdown(x), collapse = "\n"))
}
