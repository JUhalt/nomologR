# Nomological-network presentation --------------------------------------------
#
# print() shows the evidence for each hypothesis, its replication, and what is
# flagged about the measurement, the fit, the cases, and the relations the
# hypothesized paths changed. summary() adds the fit of the network and of the
# measurement model alone, the changed relations, the predictions in full, the
# concordance counts, and each flag with its recommendation. Stored values
# never change; only their display does (#144, #145).

# Display labels for the stored concordance and replication statuses. They are
# short enough that the status column fits beside the estimates on an
# 80-column console (#145). "In region, imprecise" reads for every prediction,
# negligible() included, which has no direction to be concordant with.
nomo_network_status_labels <- c(
  concordant = "Concordant",
  directionally_concordant_imprecise = "In region, imprecise",
  direction_concordant_below_magnitude = "Smaller than predicted",
  direction_concordant_above_magnitude = "Larger than predicted",
  inconclusive = "Inconclusive",
  inconsistent = "Inconsistent",
  not_confirmable_without_sesoi = "Not confirmable",
  not_evaluable = "Not evaluable",
  replicated_concordance = "Replicated",
  mixed_or_inconclusive = "Mixed",
  not_replicated = "Not replicated",
  unstable = "Unstable",
  replicated_inconsistency = "Inconsistent in both",
  direction_replicated_but_uncertain = "Same direction",
  sign_reversal = "Sign reversal",
  direction_not_replicated = "Direction not replicated",
  sign_change_within_uncertainty = "Sign change, uncertain",
  post_hoc_exploratory = "Post hoc",
  a_priori = "A priori"
)


# `opposite` marks a shared direction that is the opposite of the prediction:
# the stored status is the same, and the label says which it is.
nomo_network_pretty_status <- function(x, opposite = FALSE) {
  x <- as.character(x)
  out <- unname(nomo_network_status_labels[x])
  missing <- is.na(out)
  out[missing] <- gsub("_", " ", x[missing], fixed = TRUE)
  out[x %in% "direction_replicated_but_uncertain" & opposite] <- "Opposite in both"
  out
}


nomo_network_concordance_levels <- function() {
  unname(nomo_network_status_labels[c(
    "inconsistent", "not_evaluable", "not_confirmable_without_sesoi",
    "inconclusive", "direction_concordant_below_magnitude",
    "direction_concordant_above_magnitude", "directionally_concordant_imprecise",
    "concordant"
  )])
}


# Concordance and replication statuses as the status a plot draws: no flag for
# the supportive result, concern for the results the decision log treats as
# concerns, not computed where no decision was possible, and review otherwise.
nomo_network_concordance_flag <- function(x) {
  x <- as.character(x)
  out <- rep("review", length(x))
  out[x == "concordant"] <- "info"
  out[x == "inconsistent"] <- "concern"
  out[x == "not_evaluable"] <- "unavailable"
  out
}

nomo_network_replication_flag <- function(x, opposite = FALSE) {
  x <- as.character(x)
  out <- rep("review", length(x))
  out[x == "replicated_concordance"] <- "info"
  out[x %in% c("sign_reversal", "direction_not_replicated", "not_replicated",
               "unstable", "replicated_inconsistency") |
        (x == "direction_replicated_but_uncertain" & opposite)] <- "concern"
  out[x == "not_evaluable"] <- "unavailable"
  out
}


# Numbers -----------------------------------------------------------------------

# How an estimate is printed: a standardized association is a correlation and
# drops its leading zero; a path, or any unstandardized value, keeps it.
nomo_network_kind <- function(scale, relation_type) {
  ifelse(scale %in% "standardized" & relation_type %in% "association", "r", "estimate")
}


# Values in the format of their kind, each with the decimals that tell it apart
# from the reference beside it (a bound of the predicted region).
nomo_network_number <- function(x, kind, reference = NA_real_) {
  reference <- rep_len(reference, length(x))
  kind <- rep_len(kind, length(x))
  vapply(seq_along(x), function(i) {
    nomo_present_stat(x[[i]], kind[[i]], reference = reference[[i]])
  }, character(1))
}


# The finite bound of a region closest to each value: the reference a printed
# value must not be confused with.
nomo_network_nearest_bound <- function(x, lower, upper) {
  vapply(seq_along(x), function(i) {
    bounds <- c(lower[[i]], upper[[i]])
    bounds <- bounds[is.finite(bounds)]
    if (!length(bounds) || !is.finite(x[[i]])) return(NA_real_)
    bounds[[which.min(abs(bounds - x[[i]]))]]
  }, numeric(1))
}


# A bound of a predicted region as the researcher gave it: at least two
# decimals, more when given, in the format of the estimate's kind; zero is 0.
nomo_network_bound_text <- function(x, kind) {
  decimals <- vapply(x, function(v) {
    nchar(sub("^[^.]*\\.?", "", format(signif(abs(v), 6), scientific = FALSE,
                                       drop0trailing = TRUE)))
  }, numeric(1))
  out <- vapply(seq_along(x), function(i) {
    nomo_present_stat(x[[i]], kind[[i]], digits = min(4L, max(2L, decimals[[i]])))
  }, character(1))
  out[x == 0] <- "0"
  out
}


# A predicted region in words: "> 0", ">= 0.20", "[-.15, .15]", "(0, 0.50]".
# The stored `theoretical_region` keeps its own form.
nomo_network_region_text <- function(h, kind) {
  vapply(seq_len(nrow(h)), function(i) {
    lower <- h$lower[[i]]
    upper <- h$upper[[i]]
    if (!isTRUE(h$magnitude_specified[[i]]) && identical(h$prediction[[i]], "negligible")) {
      return("not specified")
    }
    text <- nomo_network_bound_text(c(lower, upper), rep(kind[[i]], 2L))
    if (is.infinite(lower)) {
      return(paste(if (isTRUE(h$upper_inclusive[[i]])) "<=" else "<", text[[2L]]))
    }
    if (is.infinite(upper)) {
      return(paste(if (isTRUE(h$lower_inclusive[[i]])) ">=" else ">", text[[1L]]))
    }
    paste0(if (isTRUE(h$lower_inclusive[[i]])) "[" else "(", text[[1L]], ", ", text[[2L]],
           if (isTRUE(h$upper_inclusive[[i]])) "]" else ")")
  }, character(1))
}


# Hypothesis evidence -------------------------------------------------------------

# The evidence as each row is displayed: the estimate, and the interval its
# concordance was judged on, which for a negligible() prediction with a region
# is the equivalence interval rather than the 95% interval (#145).
nomo_network_display_evidence <- function(evidence, hypotheses) {
  show <- as.data.frame(evidence, stringsAsFactors = FALSE)
  h <- as.data.frame(hypotheses, stringsAsFactors = FALSE)
  h <- h[match(show$id, h$id), , drop = FALSE]
  show$kind <- nomo_network_kind(show$scale, show$relation_type)
  show$equivalence <- show$prediction == "negligible" & h$magnitude_specified %in% TRUE
  lower <- ifelse(show$equivalence, show$equivalence_ci_lower, show$ci_lower)
  upper <- ifelse(show$equivalence, show$equivalence_ci_upper, show$ci_upper)
  show$estimate_shown <- nomo_network_number(
    show$estimate, show$kind, nomo_network_nearest_bound(show$estimate, h$lower, h$upper)
  )
  lower_shown <- nomo_network_number(lower, show$kind, nomo_network_nearest_bound(lower, h$lower, h$upper))
  upper_shown <- nomo_network_number(upper, show$kind, nomo_network_nearest_bound(upper, h$lower, h$upper))
  show$interval <- ifelse(is.finite(lower) & is.finite(upper),
                          sprintf("[%s, %s]", lower_shown, upper_shown), nomo_present_missing)
  show$concordance_shown <- nomo_network_pretty_status(show$concordance)
  show$origin_shown <- nomo_present_origin(show$origin, cell = TRUE)
  show$region_shown <- nomo_network_region_text(h, show$kind)
  # A post hoc prediction is marked in its row wherever its concordance is
  # shown, as ?nomo_expectations promises (#145).
  post_hoc <- show$origin %in% "post_hoc"
  show$relation_shown <- paste0(show$relation, ifelse(post_hoc, " (post hoc)", ""))
  show$hypothesis <- paste0(show$id, " ", show$relation, ifelse(post_hoc, ", post hoc", ""))
  show
}


# The notes under the hypothesis table: the scale, the interval, the post hoc
# predictions, and the predictions without a region, each defined once.
nomo_network_evidence_notes <- function(show, equivalence_alpha) {
  scales <- unique(show$scale)
  level <- sub(" CI$", "", nomo_present_ci_label(1 - 2 * equivalence_alpha))
  neg <- show$id[show$equivalence]
  notes <- c(
    if (length(scales) == 1L) sprintf("Estimates are %s.", scales) else nomo_hypotheses_scale_note(show),
    if (!length(neg)) {
      "CI = confidence interval."
    } else if (length(neg) == nrow(show)) {
      sprintf("CI = confidence interval: the %s equivalence interval %s judged on.",
              level,
              nomo_present_noun(length(neg), "the negligible() prediction is",
                                "each negligible() prediction is"))
    } else {
      sprintf(paste(
        "CI = confidence interval (95%%); for %s, %s, it is the %s equivalence",
        "interval the concordance is judged on."
      ),
      nomo_present_or(neg, "and"),
      nomo_present_noun(length(neg), "a negligible() prediction", "negligible() predictions"),
      level)
    }
  )
  post_hoc <- show$id[show$origin == "post_hoc"]
  if (length(post_hoc)) {
    notes <- c(notes, sprintf(
      "%s %s specified post hoc, so %s concordance is exploratory, not confirmatory evidence.",
      nomo_present_or(post_hoc, "and"), nomo_present_noun(length(post_hoc), "was", "were"),
      nomo_present_noun(length(post_hoc), "its", "their")
    ))
  }
  bare <- show$id[show$concordance == "not_confirmable_without_sesoi"]
  if (length(bare)) {
    notes <- c(notes, sprintf(paste(
      "Not confirmable: %s %s no region, the smallest effect size of interest",
      "(SESOI) a negligible() prediction needs, so a non-significant estimate",
      "cannot confirm %s."
    ), nomo_present_or(bare, "and"), nomo_present_noun(length(bare), "has", "have"),
    nomo_present_noun(length(bare), "it", "them")))
  }
  notes
}


# Whether a table's stub and the columns it never drops fit the console with
# one-space gaps; when they do not, the rows are printed as bullets instead.
nomo_network_table_fits <- function(cells, keep) {
  widths <- vapply(names(cells), function(nm) {
    max(nchar(c(nm, cells[[nm]]), type = "width"))
  }, numeric(1))
  kept <- seq_along(cells) == 1L | tolower(names(cells)) %in% tolower(keep)
  2 + sum(widths[kept]) + sum(kept) - 1 <= nomo_present_width()
}


# A table, or on a console too narrow for its kept columns, one bullet per row:
# "- H1 Agency -> Persistence (Concordant): 0.46, 95% CI [0.39, 0.53]."
# `formats` gives each numeric column its text, so numbers stay right-aligned.
nomo_network_present_rows <- function(show, columns, formats, keep, more, bullet) {
  cells <- lapply(unname(columns), function(col) {
    if (is.function(formats[[col]])) formats[[col]](show[[col]]) else as.character(show[[col]])
  })
  names(cells) <- names(columns)
  if (nomo_network_table_fits(cells, keep)) {
    nomo_present_table(show, columns, formats = formats, more = more, keep = keep)
  } else {
    nomo_present_bullets(bullet)
  }
}


# Shared by print() and summary(): the hypothesis evidence, and replication.
nomo_network_present_evidence <- function(x) {
  show <- nomo_network_display_evidence(x$hypothesis_evidence, x$hypotheses$hypotheses)
  interval_label <- if (!any(show$equivalence)) {
    "95% CI"
  } else if (all(show$equivalence)) {
    nomo_present_ci_label(1 - 2 * x$equivalence_alpha)
  } else {
    "CI"
  }
  # The status is the result, so it follows the stub and the relation; the
  # scale and the post hoc predictions are stated in the notes beneath, which
  # no console width hides.
  nomo_present_section("Hypothesis evidence")
  nomo_network_present_rows(
    show,
    stats::setNames(c("id", "relation_shown", "concordance_shown", "estimate", "interval"),
                    c("ID", "Relation", "Concordance", "Estimate", interval_label)),
    formats = list(estimate = function(v) show$estimate_shown),
    keep = c(nomo_present_keep, "Relation"), more = "nomo_table(x, \"hypotheses\")",
    bullet = sprintf(
      "%s (%s): %s, %s %s.", show$hypothesis, show$concordance_shown, show$estimate_shown,
      ifelse(show$equivalence, nomo_present_ci_label(1 - 2 * x$equivalence_alpha), "95% CI"),
      show$interval
    )
  )
  nomo_present_text(
    paste(nomo_network_evidence_notes(show, x$equivalence_alpha), collapse = " "),
    indent = 2L
  )

  replication <- x$replication_evidence
  if (!nrow(replication)) return(invisible(NULL))
  rep <- as.data.frame(replication, stringsAsFactors = FALSE)
  shown <- show[match(rep$id, show$id), , drop = FALSE]
  opposite <- nomo_network_opposite_direction(rep$prediction, rep$primary_estimate)
  rep$status_shown <- nomo_network_pretty_status(rep$replication_status, opposite)
  primary_shown <- nomo_network_number(rep$primary_estimate, shown$kind)
  validation_shown <- nomo_network_number(rep$validation_estimate, shown$kind)
  role <- tools::toTitleCase(x$sample_role)
  nomo_present_section("Replication evidence")
  nomo_network_present_rows(
    rep,
    stats::setNames(c("id", "relation", "status_shown", "primary_estimate", "validation_estimate"),
                    c("ID", "Relation", "Replication", role, "Validation")),
    formats = list(primary_estimate = function(v) primary_shown,
                   validation_estimate = function(v) validation_shown),
    keep = c(nomo_present_keep, "Relation"), more = "nomo_table(x, \"replication\")",
    bullet = sprintf("%s %s (%s): %s %s, validation %s.", rep$id, rep$relation,
                     rep$status_shown, tolower(role), primary_shown, validation_shown)
  )
}


# Single indicators, for print() and summary(): the reliability behind each
# composite modeled as a single indicator, and how the evidence moves with it.
nomo_network_present_single <- function(single, sensitivity, detail = FALSE) {
  if (!is.data.frame(single) || !nrow(single)) return(invisible(NULL))
  if (!isTRUE(detail)) {
    nomo_present_facts(paste0(
      "Single indicator: ", single$variable, " (reliability ",
      nomo_present_stat(single$reliability, "reliability"), ", ", single$coefficient, ")"
    ))
    return(invisible(NULL))
  }

  # Numbers stay numeric, so they are right-aligned like the variances; a
  # missing standard error is shown as "--", and the column is left out when
  # no composite has one.
  nomo_present_section("Single indicators")
  nomo_present_table(
    single,
    c("Composite" = "variable", "Coefficient" = "coefficient",
      "Reliability" = "reliability", "SE" = "se", "Variance" = "variance",
      "Error variance" = "error_variance"),
    formats = list(
      reliability = function(v) nomo_present_stat(v, "reliability"),
      se = function(v) nomo_present_stat(v, "estimate"),
      variance = function(v) nomo_present_stat(v, "estimate"),
      error_variance = function(v) nomo_present_stat(v, "estimate")
    ),
    more = "nomo_table(x, \"single_indicators\")"
  )

  wide <- nomo_network_sensitivity_wide(sensitivity)
  estimate <- function(v) nomo_present_stat(v, "estimate")
  nomo_present_section("Sensitivity to the reliability")
  nomo_present_table(
    wide,
    c("Composite" = "variable", "ID" = "id", "-.10" = "minus_10",
      "-.05" = "minus_05", "Given" = "given", "+.05" = "plus_05",
      "+.10" = "plus_10", "Concordance" = "concordance"),
    formats = list(minus_10 = estimate, minus_05 = estimate, given = estimate,
                   plus_05 = estimate, plus_10 = estimate),
    more = "nomo_table(x, \"sensitivity\")"
  )
  nomo_present_text(
    "Estimates with each composite's reliability shifted by the amount shown, ",
    "one composite at a time.",
    indent = 2L
  )
}


# The sensitivity table with one row per composite and hypothesis, and the
# estimate at each shift of the reliability in its own column.
nomo_network_sensitivity_wide <- function(sensitivity) {
  keys <- unique(as.data.frame(sensitivity)[c("variable", "id")])
  columns <- c(minus_10 = -0.10, minus_05 = -0.05, given = 0, plus_05 = 0.05,
               plus_10 = 0.10)
  for (column in names(columns)) {
    keys[[column]] <- vapply(seq_len(nrow(keys)), function(i) {
      hit <- sensitivity$variable == keys$variable[[i]] &
        sensitivity$id == keys$id[[i]] &
        abs(sensitivity$shift - columns[[column]]) < 1e-9
      if (any(hit)) sensitivity$estimate[hit][[1L]] else NA_real_
    }, numeric(1))
  }
  keys$concordance <- vapply(seq_len(nrow(keys)), function(i) {
    seen <- sensitivity$concordance[
      sensitivity$variable == keys$variable[[i]] & sensitivity$id == keys$id[[i]]
    ]
    ifelse(length(unique(seen)) > 1L, "Changes", "Unchanged")
  }, character(1))
  keys
}


# Facts ---------------------------------------------------------------------------

# "Cases: 800", or "Cases: 479 of 800" when listwise deletion dropped rows; a
# split names each sample (#145).
nomo_network_cases_text <- function(label, n_used, data_n) {
  n_used <- c(n_used, NA_real_)[[1L]]
  sprintf("%s: %s", label, ifelse(
    is.finite(n_used) & n_used < data_n,
    sprintf("%d of %d", as.integer(n_used), as.integer(data_n)),
    format(as.integer(data_n))
  ))
}


# Shared facts: the cases analyzed and convergence.
nomo_network_present_facts <- function(x) {
  validation <- !is.na(x$validation_n)
  nomo_present_facts(c(
    nomo_network_cases_text(
      if (validation) paste(tools::toTitleCase(x$sample_role), "cases") else "Cases",
      x[["n_used"]], x$data_n
    ),
    if (validation) nomo_network_cases_text("Validation cases", x[["validation_n_used"]], x$validation_n),
    paste0("Converged: ", if (isTRUE(x$converged)) "yes" else "no")
  ))
}


# The severity of a metric's decision-log row in the primary sample; NA when
# the log has none.
nomo_network_log_severity <- function(log, metric) {
  log$severity[log$metric == metric & log$stage == "network"][1L]
}


# A stream's status in words: "review", "concern", or "no flags", the same in
# print() and summary().
nomo_network_stream_status <- function(severity) {
  flag <- nomo_present_flag(severity)
  ifelse(nzchar(flag), flag, "no flags")
}


# Flagged ---------------------------------------------------------------------------

# The decision-log rows that need attention, each under a name a reader knows:
# the stream for a sample's row, the hypothesis for its evidence, and no name
# where the explanation itself names the relation or composite; a row of the
# validation sample or of the replication says so. The hypothesis tables show
# the concordance and replication statuses, so print() leaves those rows out;
# summary() keeps them and adds each recommendation.
nomo_network_present_flagged <- function(log, evidence, detail = FALSE) {
  log <- as.data.frame(log, stringsAsFactors = FALSE)
  rows <- log[log$severity %in% c("review", "concern"), , drop = FALSE]
  if (!isTRUE(detail)) {
    rows <- rows[!rows$metric %in% c("theory_concordance", "replication_status"), , drop = FALSE]
  }
  streams <- c(
    convergence = "Convergence", cases_used = "Cases",
    measurement_context = "Measurement context", model_fit = "Model fit",
    engine_warnings = "Engine warnings"
  )
  ids <- evidence$id[match(rows$object, evidence$relation)]
  unit <- ifelse(is.na(ids), rows$object, paste(ids, rows$object))
  unit[rows$metric %in% names(streams)] <- unname(streams[rows$metric[rows$metric %in% names(streams)]])
  named <- vapply(seq_len(nrow(rows)), function(i) {
    grepl(paste0("`", rows$object[[i]], "`"), rows$observation[[i]], fixed = TRUE)
  }, logical(1))
  unit[named | rows$object %in% "hypotheses"] <- ""
  prefix <- c(network_validation = "Validation", network_replication = "Replication")[rows$stage]
  unit <- ifelse(is.na(prefix), unit, trimws(paste0(prefix, ": ", unit)))
  unit <- sub(":$", "", unit)
  text <- rows$observation
  if (isTRUE(detail)) text <- paste(text, rows$recommendation)
  nomo_present_flagged(unit = unit, status = rows$severity, text = text)
  invisible(text)
}


# Abbreviations the network's output can show, and the estimators it names.
nomo_network_abbreviations <- c(
  CFI = "comparative fit index",
  TLI = "Tucker-Lewis index",
  RMSEA = "root mean square error of approximation",
  SRMR = "standardized root mean square residual",
  SE = "standard error",
  SEM = "structural equation model",
  ML = "maximum likelihood",
  MLR = "maximum likelihood with robust standard errors and a scaled test statistic",
  MLM = "maximum likelihood with robust standard errors and a Satorra-Bentler scaled test statistic",
  MLMV = "maximum likelihood with robust standard errors and a mean- and variance-adjusted test statistic",
  MLF = "maximum likelihood with first-order standard errors",
  WLSMV = "weighted least squares, mean- and variance-adjusted",
  DWLS = "diagonally weighted least squares",
  WLS = "weighted least squares",
  ULS = "unweighted least squares",
  ULSMV = "unweighted least squares, mean- and variance-adjusted",
  GLS = "generalized least squares"
)


# The key to the abbreviations a printed text holds, each defined once: those
# in `shown` always, and any other the text uses. CI and SESOI are defined
# beside the hypothesis table, so they are never repeated here.
nomo_network_present_key <- function(text, shown = character()) {
  text <- paste(text, collapse = " ")
  used <- names(nomo_network_abbreviations)[vapply(
    names(nomo_network_abbreviations),
    function(a) grepl(paste0("\\b", a, "\\b"), text, perl = TRUE),
    logical(1)
  )]
  nomo_present_key(nomo_network_abbreviations[union(shown, used)], title = "Abbreviations")
}


# Facts inside a section: "Label: value" parts joined by " | ", indented and
# broken only between parts, as nomo_present_facts() does at the margin.
nomo_network_section_facts <- function(parts, indent = 2L) {
  width <- nomo_present_prose_width() - indent
  lines <- parts[[1L]]
  for (part in parts[-1L]) {
    last <- length(lines)
    candidate <- paste(lines[[last]], part, sep = " | ")
    if (nchar(candidate, type = "width") < width) lines[[last]] <- candidate else lines <- c(lines, part)
  }
  nomo_present_cat(paste0(strrep(" ", indent), lines))
}


#' @export
print.nomo_network <- function(x, ...) {
  nomo_present_header("nomo_network", "Nomological network")
  nomo_network_present_facts(x)
  nomo_present_facts(c(
    sprintf("Theory relations: %d", x$hypotheses$n),
    sprintf("Added to the model from hypotheses: %d",
            sum(x$model_relations$added_from_hypothesis))
  ))
  measurement <- x$measurement_context$summary
  nomo_present_facts(c(
    paste0("Measurement context: ", if (identical(measurement$latent_constructs[[1L]], 0L)) {
      "no latent variables"
    } else {
      nomo_network_stream_status(measurement$attention[[1L]])
    }),
    paste0("Model fit: ", nomo_network_stream_status(
      nomo_network_log_severity(x$decision_log, "model_fit")
    ))
  ))
  nomo_network_present_single(
    x[["single_indicators"]], x[["single_indicator_sensitivity"]]
  )

  nomo_network_present_evidence(x)
  flagged <- nomo_network_present_flagged(x$decision_log, x$hypothesis_evidence)
  nomo_network_present_key(flagged)

  cat("\n")
  nomo_present_text(
    "Theory concordance, uncertainty, measurement quality, and replication are ",
    "distinct evidence streams. Statistical significance alone is not a ",
    "validity verdict."
  )
  nomo_present_pointer(
    c("summary(x)", "nomo_table(x, \"hypotheses\")"),
    c("the fit and the flagged evidence in full", "every column")
  )

  invisible(x)
}


#' @export
summary.nomo_network <- function(object, ...) {
  counts <- as.data.frame(table(
    object$hypothesis_evidence$concordance,
    useNA = "ifany"
  ))
  names(counts) <- c("concordance", "n")

  replication_counts <- if (nrow(object$replication_evidence)) {
    tmp <- as.data.frame(table(
      object$replication_evidence$replication_status,
      useNA = "ifany"
    ))
    names(tmp) <- c("replication_status", "n")
    tibble::as_tibble(tmp)
  } else {
    tibble::tibble()
  }

  out <- list(
    sample_role = object$sample_role,
    data_n = object$data_n,
    n_used = object[["n_used"]],
    validation_n = object$validation_n,
    validation_n_used = object[["validation_n_used"]],
    converged = object$converged,
    estimator = object$estimator,
    fit_evidence = object$fit_evidence,
    measurement_context = object$measurement_context,
    validation_fit_evidence = object$validation$fit_evidence,
    validation_measurement_context = object$validation$measurement_context,
    hypotheses = object$hypotheses,
    equivalence_alpha = object$equivalence_alpha,
    hypothesis_evidence = object$hypothesis_evidence,
    concordance_counts = tibble::as_tibble(counts),
    replication_evidence = object$replication_evidence,
    replication_counts = replication_counts,
    model_relations = object$model_relations,
    model_changes = object[["model_changes"]],
    single_indicators = object[["single_indicators"]],
    single_indicator_sensitivity = object[["single_indicator_sensitivity"]],
    decision_log = object$decision_log
  )

  class(out) <- c("summary_nomo_network", "list")
  out
}


# The fit lines of one model: its chi-square test, then its indices.
nomo_network_fit_lines <- function(label, fit) {
  nomo_present_text(paste0(label, ": ", nomo_present_chisq(fit$chisq, fit$df, fit$pvalue)),
                    indent = 2L, exdent = 4L)
  nomo_network_section_facts(c(
    paste("CFI", nomo_present_stat(fit$cfi, "fit_bounded")),
    paste("TLI", nomo_present_stat(fit$tli, "fit")),
    paste("RMSEA", nomo_present_stat(fit$rmsea, "fit")),
    paste("SRMR", nomo_present_stat(fit$srmr, "fit"))
  ), indent = 4L)
}


# The fit of the network model and of the measurement model alone in one
# sample, and the test of the structural restrictions between them (#145).
nomo_network_present_fit <- function(prefix, fit, context) {
  sentence <- function(s) paste0(toupper(substr(s, 1L, 1L)), substring(s, 2L))
  nomo_network_fit_lines(sentence(paste0(prefix, "network model")), fit)
  status <- c(context$fit_status, "not_computed")[[1L]]
  label <- sentence(paste0(prefix, "measurement model alone"))
  if (identical(status, "fitted")) {
    nomo_network_fit_lines(label, context$fit)
    test <- context$structural_test
    if (is.finite(test$chisq_diff[[1L]])) {
      nomo_present_text(paste0(
        sentence(paste0(prefix, "structural restrictions: ")),
        nomo_present_chisq(test$chisq_diff, test$df_diff, test$p_value, delta = TRUE),
        if (!identical(test$method[[1L]], "standard")) sprintf(" (%s)", test$method[[1L]]) else ""
      ), indent = 2L, exdent = 4L)
    }
  } else {
    nomo_present_text(paste0(label, ": ", switch(
      status,
      same = "the network model itself, whose structural part adds no restriction.",
      no_latent = "none, as the model has no latent variables.",
      "not available; it could not be fitted."
    )), indent = 2L, exdent = 4L)
  }
}


# The version of the statistics shown when it is not the standard one, as
# nomo_cfa() labels it: "scaled chi-square; robust CFI, TLI, and RMSEA".
nomo_network_version_text <- function(fit) {
  parts <- c(
    if (fit$chisq_version %in% "scaled") "scaled chi-square",
    if (fit$index_version %in% c("scaled", "robust")) {
      paste(fit$index_version, "CFI, TLI, and RMSEA")
    } else if (fit$index_version %in% "mixed") {
      "a mix of scaled and robust CFI, TLI, and RMSEA"
    }
  )
  if (length(parts)) paste0("Version: ", paste(parts, collapse = "; "), ".") else ""
}


#' @export
print.summary_nomo_network <- function(x, ...) {
  nomo_present_header("nomo_network", "Nomological network", summary = TRUE)
  nomo_network_present_facts(x)
  estimator <- c(x$estimator, NA_character_)[[1L]]
  nomo_present_facts(paste0("Estimator: ", ifelse(is.na(estimator), nomo_present_missing, estimator)))

  nomo_present_section("Measurement context")
  mc <- x$measurement_context$summary[1L, , drop = FALSE]
  nomo_network_section_facts(c(
    paste0("Status: ", nomo_network_stream_status(mc$attention[[1L]])),
    sprintf("Constructs: %s", format(mc$latent_constructs, trim = TRUE)),
    sprintf("Loading flags: %s", format(mc$loading_review_flags, trim = TRUE)),
    sprintf("Negative variances: %s", format(mc$negative_variance_flags, trim = TRUE)),
    sprintf("Fit flags: %s", format(mc$global_fit_review_flags, trim = TRUE)),
    sprintf("Engine warnings: %s", format(mc$engine_warning_count, trim = TRUE))
  ))
  nomo_present_text(mc$observation, indent = 2L)

  nomo_present_section("Model fit")
  fit <- x$fit_evidence[1L, , drop = FALSE]
  validation <- is.data.frame(x$validation_fit_evidence) && nrow(x$validation_fit_evidence) > 0L
  prefix <- ifelse(validation, paste0(x$sample_role, " sample, "), "")
  nomo_network_present_fit(prefix, fit, x$measurement_context)
  if (validation) {
    nomo_network_present_fit("validation sample, ", x$validation_fit_evidence[1L, , drop = FALSE],
                             x$validation_measurement_context)
  }
  version <- nomo_network_version_text(fit)
  if (nzchar(version)) nomo_present_text(version, indent = 2L)

  changes <- x[["model_changes"]]
  if (is.data.frame(changes) && nrow(changes)) {
    nomo_present_section("Relations the hypothesized paths changed")
    nomo_present_bullets(paste0(changes$relation, ": ", ifelse(
      changes$change == "fixed_to_zero",
      "fixed to zero, although the model as given estimates it.",
      ifelse(
        changes$change == "not_estimated",
        paste0("fixed to zero; the model as given does not contain ",
               gsub(" and ", " or ", changes$new_variable, fixed = TRUE), "."),
        "estimated as a residual covariance of two outcomes that neither the model nor the hypotheses name."
      )
    )))
  }

  nomo_network_present_evidence(x)
  nomo_network_present_single(
    x[["single_indicators"]], x[["single_indicator_sensitivity"]], detail = TRUE
  )

  nomo_present_section("Predictions and context")
  context <- nomo_network_display_evidence(x$hypothesis_evidence, x$hypotheses$hypotheses)
  context$scope_shown <- gsub("_", " ", context$evidence_scope)
  nomo_present_table(
    context,
    nomo_present_drop_constant(
      c("ID" = "id", "Prediction" = "prediction", "Region" = "region_shown",
        "Scale" = "scale", "Evidence scope" = "scope_shown", "Origin" = "origin_shown"),
      "Scale", context$scale
    ),
    more = "nomo_table(x, \"hypotheses\")"
  )

  nomo_present_section("Concordance")
  counts <- x$concordance_counts
  nomo_network_section_facts(paste0(
    nomo_network_pretty_status(counts$concordance), ": ", counts$n
  ))
  if (nrow(x$replication_counts)) {
    replication <- x$replication_counts
    nomo_present_section("Replication")
    nomo_network_section_facts(paste0(
      nomo_network_pretty_status(replication$replication_status), ": ", replication$n
    ))
  }

  flagged <- nomo_network_present_flagged(x$decision_log, x$hypothesis_evidence, detail = TRUE)

  # The fit indices and the estimator are shown above; a standard error only
  # when a single indicator's reliability has one.
  nomo_network_present_key(
    c(flagged, estimator),
    c("CFI", "TLI", "RMSEA", "SRMR", if (any(is.finite(x[["single_indicators"]]$se))) "SE")
  )

  cat("\n")
  nomo_present_text(
    "Theory concordance, uncertainty, measurement quality, model fit, and ",
    "replication are distinct evidence streams; none is a validity verdict."
  )
  nomo_present_pointer(
    c("nomo_table(x, \"decision_log\")", "plot(x)"),
    c("every decision-log row", "the evidence")
  )
  invisible(x)
}


# Plots -------------------------------------------------------------------------

nomo_network_theory_plot_data <- function(x) {
  dat <- x$hypothesis_evidence

  h <- x$hypotheses$hypotheses[, c(
    "id",
    "lower",
    "upper",
    "lower_inclusive",
    "upper_inclusive",
    "magnitude_specified"
  ), drop = FALSE]

  idx <- match(dat$id, h$id)

  dat$theory_lower <- h$lower[idx]
  dat$theory_upper <- h$upper[idx]
  dat$theory_lower_inclusive <- h$lower_inclusive[idx]
  dat$theory_upper_inclusive <- h$upper_inclusive[idx]

  # The interval each concordance was judged on: the equivalence interval for
  # a negligible() prediction with a region, the 95% interval otherwise (#145).
  equivalence <- dat$prediction == "negligible" & h$magnitude_specified[idx] %in% TRUE
  dat$interval_lower <- ifelse(equivalence, dat$equivalence_ci_lower, dat$ci_lower)
  dat$interval_upper <- ifelse(equivalence, dat$equivalence_ci_upper, dat$ci_upper)
  dat$equivalence <- equivalence

  finite_values <- c(
    dat$interval_lower[is.finite(dat$interval_lower)],
    dat$interval_upper[is.finite(dat$interval_upper)],
    dat$estimate[is.finite(dat$estimate)],
    dat$theory_lower[is.finite(dat$theory_lower)],
    dat$theory_upper[is.finite(dat$theory_upper)],
    0
  )

  # `finite_values` always contains the explicit zero reference above, so
  # the empty-vector branch is unreachable by construction.
  plot_min <- min(finite_values, na.rm = TRUE)
  plot_max <- max(finite_values, na.rm = TRUE)
  span <- plot_max - plot_min
  if (!is.finite(span) || span <= 0) span <- 1
  plot_min <- plot_min - .10 * span
  plot_max <- plot_max + .10 * span

  # An unbounded side of a region runs to the edge of the plot. A region that
  # was never specified, a bare negligible(), has missing bounds and no band:
  # drawing one across the plot would call every value compatible (#145).
  dat$theory_lower_plot <- ifelse(is.infinite(dat$theory_lower), plot_min, dat$theory_lower)
  dat$theory_upper_plot <- ifelse(is.infinite(dat$theory_upper), plot_max, dat$theory_upper)

  list(
    data = dat,
    limits = c(plot_min, plot_max)
  )
}


# The relation as an axis label: its ID, and "(post hoc)" when it was
# specified after the data were seen (#145).
nomo_network_plot_label <- function(id, relation, origin) {
  paste0(id, "  ", relation, ifelse(origin %in% "post_hoc", " (post hoc)", ""))
}


# Labels for an axis of estimates: without the leading zero when every value
# drawn is a standardized correlation, which cannot exceed 1.
nomo_network_axis_labels <- function(kind) {
  if (length(kind) && all(kind == "r")) nomo_plot_bounded_labels else ggplot2::waiver()
}


nomo_network_scale_title <- function(scale) {
  scales <- unique(scale)
  if (length(scales) == 1L) {
    paste(tools::toTitleCase(scales), "estimate")
  } else {
    "Estimate (standardized or unstandardized, as each hypothesis specifies)"
  }
}


#' Plot nomological-network evidence
#'
#' Every plot marks status the same way: a filled circle for no flag, an open
#' circle for review, a filled square for concern, and a cross where nothing
#' could be decided, with a legend whenever a flag is drawn. Post hoc
#' hypotheses are labeled as such.
#'
#' @param x A `nomo_network` object.
#' @param type Plot type: `"effects"`, `"concordance"`, `"fit"`, or
#'   `"replication"`.
#' @param ... Unused.
#'
#' @return A `ggplot2` object.
#' @export
plot.nomo_network <- function(
    x,
    type = c("effects", "concordance", "fit", "replication"),
    ...) {
  type <- nomo_match_arg(type)

  if (type == "effects") {
    prepared <- nomo_network_theory_plot_data(x)
    dat <- prepared$data
    limits <- prepared$limits

    dat <- dat[
      is.finite(dat$estimate) &
        is.finite(dat$interval_lower) &
        is.finite(dat$interval_upper),
      ,
      drop = FALSE
    ]
    if (!nrow(dat)) {
      stop(
        "No finite hypothesis estimates and confidence intervals are available.",
        call. = FALSE
      )
    }

    dat$label <- nomo_network_plot_label(dat$id, dat$relation, dat$origin)
    dat$label <- factor(dat$label, levels = rev(dat$label))
    dat$status <- nomo_plot_status(nomo_network_concordance_flag(dat$concordance))
    band <- dat[is.finite(dat$theory_lower_plot) & is.finite(dat$theory_upper_plot), , drop = FALSE]
    caption <- c(
      if (any(dat$equivalence)) {
        sprintf(
          "For %s, a negligible() prediction, the line is the %s equivalence interval its concordance is judged on.",
          nomo_present_or(as.character(dat$id[dat$equivalence]), "and"),
          sub(" CI$", "", nomo_present_ci_label(1 - 2 * x$equivalence_alpha))
        )
      },
      if (nrow(band) < nrow(dat)) {
        sprintf("No region was specified for %s, so no band is drawn.",
                nomo_present_or(as.character(dat$id[!dat$id %in% band$id]), "and"))
      },
      "Theory regions are researcher specified; measurement context and a priori or post hoc status remain separate evidence streams."
    )

    return(
      ggplot2::ggplot(dat, ggplot2::aes(y = label)) +
        ggplot2::geom_segment(
          data = band,
          ggplot2::aes(
            x = theory_lower_plot,
            xend = theory_upper_plot,
            yend = label
          ),
          linewidth = 5,
          alpha = .16,
          lineend = "butt"
        ) +
        ggplot2::geom_vline(xintercept = 0, linetype = 2) +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = interval_lower,
            xend = interval_upper,
            yend = label
          ),
          linewidth = .6,
          na.rm = TRUE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = estimate, shape = status, colour = status),
          size = 3,
          na.rm = TRUE
        ) +
        nomo_plot_status_scales(dat$status, name = "Concordance flag") +
        # Every relation keeps its place, whether or not it has a band.
        ggplot2::scale_y_discrete(limits = levels(dat$label)) +
        ggplot2::coord_cartesian(xlim = limits, clip = "off") +
        ggplot2::scale_x_continuous(
          labels = nomo_network_axis_labels(nomo_network_kind(dat$scale, dat$relation_type)),
          expand = ggplot2::expansion(mult = c(.02, .04))
        ) +
        nomo_plot_labs(
          title = "Theory-specified nomological effects",
          subtitle = paste(
            "Points are estimates and thin lines their 95% confidence intervals;",
            "shaded bands are the theory-compatible regions."
          ),
          x = nomo_network_scale_title(dat$scale),
          y = NULL,
          caption = paste(strwrap(paste(caption, collapse = " "), width = 85), collapse = "\n")
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title.position = "plot",
          plot.margin = ggplot2::margin(8, 18, 8, 8)
        )
    )
  }

  if (type == "concordance") {
    dat <- x$hypothesis_evidence
    if (!nrow(dat)) {
      stop("No hypothesis evidence is available to plot.", call. = FALSE)
    }

    dat$concordance_display <- factor(
      nomo_network_pretty_status(dat$concordance),
      levels = nomo_network_concordance_levels()
    )
    dat$label <- nomo_network_plot_label(dat$id, dat$relation, dat$origin)
    dat$label <- factor(dat$label, levels = rev(dat$label))
    dat$status <- nomo_plot_status(nomo_network_concordance_flag(dat$concordance))

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(
          x = concordance_display,
          y = label,
          shape = status,
          colour = status
        )
      ) +
        ggplot2::geom_point(size = 3.4) +
        nomo_plot_status_scales(dat$status, name = "Concordance flag") +
        ggplot2::scale_x_discrete(drop = FALSE) +
        nomo_plot_labs(
          title = "Relation-level nomological evidence",
          subtitle = paste(
            "Each theory-specified relation is shown at its current",
            "evidence classification."
          ),
          x = "Theory-evidence classification",
          y = NULL,
          caption = paste(
            "This is not a package-level validity score.",
            "Inspect estimates, uncertainty, measurement context, and",
            "replication alongside the classification."
          )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 35,
            hjust = 1
          )
        )
    )
  }

  if (type == "fit") {
    refs <- x$guidance$fit_reference
    kinds <- c(CFI = "fit_bounded", TLI = "fit", RMSEA = "fit", SRMR = "fit")
    dat <- data.frame(
      metric = factor(names(kinds), levels = names(kinds)),
      value = c(
        x$fit_evidence$cfi[[1L]],
        x$fit_evidence$tli[[1L]],
        x$fit_evidence$rmsea[[1L]],
        x$fit_evidence$srmr[[1L]]
      ),
      reference = vapply(c("cfi", "tli", "rmsea", "srmr"), function(nm) {
        suppressWarnings(as.numeric(refs[[nm]])[1L])
      }, numeric(1)),
      below = c(TRUE, TRUE, FALSE, FALSE),
      panel_x = 1
    )
    dat <- dat[is.finite(dat$value), , drop = FALSE]

    if (!nrow(dat)) {
      stop("No finite global fit evidence is available.", call. = FALSE)
    }

    # A value beyond its reference is marked for review; the values and the
    # references print in their own formats, since each panel has its scale.
    beyond <- is.finite(dat$reference) &
      ifelse(dat$below, dat$value < dat$reference, dat$value > dat$reference)
    dat$status <- nomo_plot_status(ifelse(beyond, "review", "info"))
    kind <- kinds[as.character(dat$metric)]
    dat$value_label <- vapply(seq_len(nrow(dat)), function(i) {
      nomo_present_stat(dat$value[[i]], kind[[i]], reference = dat$reference[[i]])
    }, character(1))
    dat$reference_label <- ifelse(
      is.finite(dat$reference),
      paste("reference", vapply(seq_len(nrow(dat)), function(i) {
        nomo_present_stat(dat$reference[[i]], kind[[i]])
      }, character(1))),
      ""
    )
    version <- nomo_network_version_text(x$fit_evidence[1L, , drop = FALSE])

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(x = panel_x, y = value)
      ) +
        ggplot2::geom_hline(
          ggplot2::aes(yintercept = reference),
          linetype = 2,
          na.rm = TRUE
        ) +
        ggplot2::geom_segment(
          ggplot2::aes(x = panel_x, xend = panel_x, y = reference, yend = value),
          colour = "grey60",
          na.rm = TRUE
        ) +
        ggplot2::geom_text(
          ggplot2::aes(y = reference, label = reference_label),
          x = 0.75, hjust = 0, vjust = -0.5, size = 3,
          na.rm = TRUE
        ) +
        ggplot2::geom_point(ggplot2::aes(shape = status, colour = status), size = 3) +
        ggplot2::geom_text(
          ggplot2::aes(label = value_label),
          nudge_x = .12, hjust = 0, size = 3.4
        ) +
        nomo_plot_status_scales(dat$status) +
        ggplot2::facet_wrap(
          stats::as.formula("~ metric"),
          scales = "free_y",
          nrow = 1
        ) +
        ggplot2::scale_x_continuous(
          breaks = NULL,
          limits = c(0.7, 1.6)
        ) +
        ggplot2::scale_y_continuous(
          breaks = NULL,
          expand = ggplot2::expansion(mult = .35)
        ) +
        nomo_plot_labs(
          title = "Nomological-network global fit evidence",
          subtitle = paste(
            "Points are the network model's values; dashed lines are the configured",
            "teaching references. Each metric uses its own scale."
          ),
          x = NULL,
          y = NULL,
          caption = paste(
            "Reference values prompt inspection; they are not pass/fail rules.",
            "summary(x) separates the measurement model's fit from the structural",
            "restrictions.", version
          )
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (!nrow(x$replication_evidence)) {
    stop(
      "No validation sample was fitted, so replication evidence is unavailable.",
      call. = FALSE
    )
  }

  dat <- x$replication_evidence
  dat <- dat[
    is.finite(dat$primary_estimate) &
      is.finite(dat$validation_estimate),
    ,
    drop = FALSE
  ]

  if (!nrow(dat)) {
    stop("No finite replication estimates are available to plot.", call. = FALSE)
  }

  # Points are labeled with their IDs, set off to the side away from the
  # identity line, and the caption names the relations (#145).
  opposite <- nomo_network_opposite_direction(dat$prediction, dat$primary_estimate)
  dat$status <- nomo_plot_status(nomo_network_replication_flag(dat$replication_status, opposite))
  dat$replication_display <- nomo_network_pretty_status(dat$replication_status, opposite)
  above <- dat$validation_estimate >= dat$primary_estimate
  dat$hjust <- ifelse(above, 1.4, -0.4)
  he <- x$hypothesis_evidence
  kind <- nomo_network_kind(he$scale[match(dat$id, he$id)], he$relation_type[match(dat$id, he$id)])
  role <- tools::toTitleCase(c(x$sample_role, "primary")[[1L]])

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = primary_estimate,
      y = validation_estimate
    )
  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::geom_point(ggplot2::aes(shape = status, colour = status), size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = id, hjust = hjust),
      size = 3.4
    ) +
    nomo_plot_status_scales(dat$status, name = "Replication flag") +
    ggplot2::scale_x_continuous(
      labels = nomo_network_axis_labels(kind),
      expand = ggplot2::expansion(mult = c(.12, .20))
    ) +
    ggplot2::scale_y_continuous(
      labels = nomo_network_axis_labels(kind),
      expand = ggplot2::expansion(mult = c(.12, .20))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    nomo_plot_labs(
      title = paste(role, "versus validation nomological effects"),
      subtitle = "The dashed identity line marks identical estimates in the two samples.",
      x = paste(role, "estimate"),
      y = "Validation estimate",
      caption = paste(
        paste0(paste(dat$id, dat$relation, sep = " = ", collapse = "; "), "."),
        "Replication status reflects theory concordance and stability; the",
        "validation model is not respecified from the first sample's results."
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(8, 24, 12, 16)
    )
}
