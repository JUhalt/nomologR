# Measurement-invariance presentation -----------------------------------------
#
# print() gives the cases, the sequence, the fit at each level with its change
# from the level before, and every flag; summary() adds the chi-square tests,
# what each level holds equal, the researcher's releases, the latent means, the
# largest score diagnostics, and each flag's recommendation. Both follow the
# output style shared with contentvalidR (#144).


# Abbreviations ------------------------------------------------------------------

# Every abbreviation an output shows is defined once in it (guide point 23).
# Each definition reads after "CFI = " in a note; a key starts it with a
# capital. The estimators are lavaan's. nomo_method_variance() uses these too.
nomo_invariance_glossary <- c(
  CFA = "confirmatory factor analysis",
  CFI = "comparative fit index",
  TLI = "Tucker-Lewis index",
  RMSEA = "root mean square error of approximation",
  SRMR = "standardized root mean square residual",
  LRT = "likelihood-ratio test of a level against the level before it",
  df = "degrees of freedom",
  CI = "confidence interval",
  ML = "maximum likelihood",
  MLR = "maximum likelihood with robust standard errors and a scaled test statistic",
  MLM = paste("maximum likelihood with robust standard errors and the",
              "Satorra-Bentler scaled test statistic"),
  MLMV = paste("maximum likelihood with robust standard errors and a mean- and",
               "variance-adjusted test statistic"),
  MLMVS = paste("maximum likelihood with robust standard errors and a mean- and",
                "variance-adjusted (Satterthwaite) test statistic"),
  MLF = "maximum likelihood with first-order standard errors",
  WLSMV = paste("diagonally weighted least squares with robust standard errors",
                "and a mean- and variance-adjusted test statistic"),
  WLSM = paste("diagonally weighted least squares with robust standard errors",
               "and a mean-adjusted test statistic"),
  DWLS = "diagonally weighted least squares",
  WLS = "weighted least squares",
  ULS = "unweighted least squares",
  ULSMV = paste("unweighted least squares with robust standard errors and a",
                "mean- and variance-adjusted test statistic"),
  GLS = "generalized least squares"
)


# The definitions of the abbreviations given, in that order, for
# nomo_present_key(). Unknown abbreviations are left out.
nomo_invariance_key_entries <- function(abbr) {
  abbr <- unique(abbr[abbr %in% names(nomo_invariance_glossary)])
  text <- unname(nomo_invariance_glossary[abbr])
  stats::setNames(paste0(toupper(substr(text, 1L, 1L)), substring(text, 2L)), abbr)
}


# The same definitions as one note after a blank line, as print() gives them:
# "CFI = comparative fit index; RMSEA = root mean square error of
# approximation."
nomo_invariance_key_note <- function(abbr) {
  abbr <- unique(abbr[abbr %in% names(nomo_invariance_glossary)])
  if (!length(abbr)) return(invisible(NULL))
  cat("\n")
  nomo_present_text(paste(abbr, "=", nomo_invariance_glossary[abbr], collapse = "; "), ".")
}


# Facts ---------------------------------------------------------------------------

# The estimator: the researcher's or the ordered default, else the one lavaan
# chose, which the result does not record.
nomo_invariance_estimator <- function(x) {
  if (length(x$estimator) == 1L && !is.na(x$estimator)) return(x$estimator)
  fit <- Find(Negate(is.null), x$fits)
  estimator <- if (!is.null(fit)) {
    tryCatch(lavaan::lavInspect(fit, "options")$estimator, error = function(e) NULL)
  }
  if (is.character(estimator) && length(estimator) == 1L) estimator else NA_character_
}


# The scaled test lavaan computed, in words: "Yuan-Bentler scaled" for MLR,
# "scaled and shifted" for WLSMV; "scaled" when lavaan does not say.
nomo_invariance_test_label <- function(x) {
  fit <- Find(Negate(is.null), x$fits)
  test <- if (!is.null(fit)) {
    tryCatch(as.character(lavaan::lavInspect(fit, "options")$test), error = function(e) NULL)
  }
  nomo_invariance_test_words(test)
}


nomo_invariance_test_words <- function(test) {
  test <- setdiff(test, c("standard", "none", "default"))
  if (!length(test)) return("scaled")
  words <- c(
    satorra.bentler = "Satorra-Bentler scaled",
    yuan.bentler = "Yuan-Bentler scaled",
    yuan.bentler.mplus = "Yuan-Bentler scaled",
    mean.var.adjusted = "mean- and variance-adjusted",
    scaled.shifted = "scaled and shifted",
    mean.adjusted = "mean-adjusted"
  )
  if (test[[1L]] %in% names(words)) words[[test[[1L]]]] else gsub(".", " ", test[[1L]], fixed = TRUE)
}


# "Cases: 800 (online n = 400, paper n = 400)", or "Cases: 447 of 800 used
# (...)" when lavaan left cases out (guide point 2; #145). Across occasions
# every occasion has the same people, so there are no group sizes.
nomo_invariance_cases_fact <- function(x) {
  count <- function(v) nomo_present_stat(v, "count")
  n_used <- x[["n_used"]]
  text <- if (length(n_used) != 1L || !is.finite(n_used)) {
    sprintf("Cases: %s in the data", count(x$data_n))
  } else if (n_used < x$data_n) {
    sprintf("Cases: %s of %s used", count(n_used), count(x$data_n))
  } else {
    sprintf("Cases: %s", count(n_used))
  }
  sizes <- x[["group_n"]]
  if (is.data.frame(sizes) && nrow(sizes)) {
    # A group stays on the line with its size.
    text <- sprintf("%s (%s)", text, paste0(sizes$group, nomo_present_nbsp, "n = ",
                                            count(sizes$n), collapse = ", "))
  }
  text
}


nomo_invariance_present_header <- function(x, summary = FALSE) {
  if (identical(x$design, "occasions")) {
    nomo_present_header("nomo_invariance_longitudinal",
                        "Measurement invariance across occasions", summary = summary)
  } else {
    nomo_present_header("nomo_invariance", "Measurement invariance across groups",
                        summary = summary)
  }
}


# Shared by print() and summary(): the sample, the estimator, the design, the
# indicators, and any researcher-specified releases.
nomo_invariance_present_facts <- function(x) {
  estimator <- x$estimator_shown
  nomo_present_facts(c(
    nomo_invariance_cases_fact(x),
    if (!is.na(estimator)) paste("Estimator:", estimator) else ""
  ))
  sizes <- x[["group_n"]]
  nomo_present_facts(c(
    if (identical(x$design, "occasions")) {
      sprintf("Occasions: %s", paste(x$groups, collapse = ", "))
    } else if (is.data.frame(sizes) && nrow(sizes)) {
      sprintf("Grouping variable: %s", x$group)
    } else {
      sprintf("Grouping variable: %s (%s)", x$group, paste(x$groups, collapse = ", "))
    },
    sprintf("Indicators: %s", gsub("_", " ", x$indicator_type, fixed = TRUE))
  ))
  if (length(x$ordered)) {
    nomo_present_facts(c(
      sprintf("Ordered identification: %s", x$ID.cat),
      sprintf("Parameterization: %s", x$parameterization)
    ))
  }
  if (!is.null(x$partial) && x$partial$n > 0L) {
    nomo_present_facts(sprintf("Partial releases: %d (researcher specified)", x$partial$n))
  }
}


# What print() and summary() need beyond the stored fields, read once.
nomo_invariance_display_fields <- function(x) {
  x$estimator_shown <- nomo_invariance_estimator(x)
  x$test_label <- nomo_invariance_test_label(x)
  x
}


# Fit tables ----------------------------------------------------------------------

# Each column in its kind (guide points 8 and 9): CFI and its change are
# bounded and lose the leading zero; RMSEA and SRMR keep it.
nomo_invariance_fit_formats <- function() {
  stat <- function(kind, signed = FALSE) {
    function(v) nomo_present_stat(v, kind, signed = signed)
  }
  list(
    chisq = stat("stat"), df = stat("df"), pvalue = nomo_present_p,
    cfi = stat("fit_bounded"), rmsea = stat("fit"), srmr = stat("fit"),
    delta_cfi = stat("fit_bounded", TRUE), delta_rmsea = stat("fit", TRUE),
    delta_srmr = stat("fit", TRUE),
    lrt_chisq = stat("stat"), lrt_df = stat("df"), lrt_p = nomo_present_p
  )
}


# One sentence on the versions of the fit statistics shown (#145): "The
# chi-square is the Yuan-Bentler scaled test statistic, and the likelihood-
# ratio tests are scaled difference tests; CFI and RMSEA are robust values."
# Empty when every value is the standard one. With `chisq = FALSE`, for
# print(), which shows no chi-square, only the tests are named. `tests` names
# the difference tests and `indices` the indices shown, by display name.
nomo_invariance_versions_note <- function(variants, test = "scaled", chisq = TRUE,
                                          tests = "the likelihood-ratio tests",
                                          indices = c(CFI = "cfi", RMSEA = "rmsea")) {
  if (!length(variants)) return("")
  version <- function(name) {
    v <- if (name %in% names(variants)) variants[[name]] else NA_character_
    if (isTRUE(grepl("robust", v, fixed = TRUE))) {
      "robust"
    } else if (isTRUE(grepl("scaled", v, fixed = TRUE))) {
      "scaled"
    } else {
      ""
    }
  }
  parts <- character()
  if (nzchar(version("chisq"))) {
    parts <- paste0(
      if (chisq) sprintf("the chi-square is the %s test statistic, and ", test),
      tests, " are scaled difference tests"
    )
  }
  indices <- vapply(indices, version, character(1))
  indices <- indices[nzchar(indices)]
  for (v in unique(indices)) {
    named <- names(indices)[indices == v]
    parts <- c(parts, sprintf(
      "%s %s", nomo_present_or(named, "and"),
      nomo_present_noun(length(named), paste("is a", v, "value"), paste("are", v, "values"))
    ))
  }
  if (!length(parts)) return("")
  text <- paste(parts, collapse = "; ")
  paste0(toupper(substr(text, 1L, 1L)), substring(text, 2L), ".")
}


nomo_invariance_present_versions <- function(x, chisq = TRUE) {
  note <- nomo_invariance_versions_note(x$fit_variants, x$test_label, chisq)
  if (nzchar(note)) nomo_present_text(note, indent = 2L)
}


# Labels ----------------------------------------------------------------------------

# Labels use ASCII so figures render on every graphics device. Non-ASCII
# symbols such as the Greek capital delta or arrows are dropped or mangled by
# the default pdf() device, and R CMD check treats that as an error. A change
# is named as the tables name it (#145).
nomo_invariance_metric_label <- function(x) {
  lookup <- c(
    delta_cfi = "CFI change",
    delta_rmsea = "RMSEA change",
    delta_srmr = "SRMR change"
  )
  out <- unname(lookup[as.character(x)])
  missing <- is.na(out)
  out[missing] <- as.character(x)[missing]
  out
}


# Across occasions, `occasion_map` translates each occasion's column and factor
# names back to the item or factor in the one-occasion model, and the occasion
# takes the place of the group.
nomo_invariance_parameter_label <- function(pt, row, group_labels, occasion_map = NULL) {
  lhs <- as.character(pt$lhs[[row]])
  op <- as.character(pt$op[[row]])
  rhs <- as.character(pt$rhs[[row]])
  occasion <- NULL
  if (!is.null(occasion_map)) {
    k <- match(if (identical(op, "=~")) rhs else lhs, occasion_map$name)
    occasion <- if (is.na(k)) "" else occasion_map$occasion[[k]]
    generic <- function(name) {
      k <- match(name, occasion_map$name)
      if (is.na(k)) name else occasion_map$generic[[k]]
    }
    lhs <- generic(lhs)
    rhs <- generic(rhs)
  }

  base <- if (identical(op, "=~")) {
    paste0("Loading: ", lhs, " -> ", rhs)
  } else if (identical(op, "~1")) {
    paste0("Intercept: ", lhs)
  } else if (identical(op, "|")) {
    paste0("Threshold: ", lhs, " | ", rhs)
  } else if (identical(op, "~~") && identical(lhs, rhs)) {
    paste0("Residual variance: ", lhs)
  } else if (identical(op, "~~")) {
    paste0("Covariance: ", lhs, " <-> ", rhs)
  } else {
    paste(lhs, op, rhs)
  }
  if (!is.null(occasion)) return(list(base = base, group = occasion))

  group_id <- suppressWarnings(as.integer(pt$group[[row]]))
  group_name <- if (
    is.finite(group_id) &&
      group_id >= 1L &&
      group_id <= length(group_labels)
  ) {
    group_labels[[group_id]]
  } else if (is.finite(group_id) && group_id > 0L) {
    paste0("Group ", group_id)
  } else {
    ""
  }

  list(base = base, group = group_name)
}


# An equality constraint in words. lavaan writes each as the first group's (or
# occasion's) parameter equal to another's, and a score test frees that one
# constraint. With two groups it compares them; with three or more, the other
# groups stay equal to the first, so the test frees the named group from the
# value the rest share: "Intercept: y3 (three vs. others)", not "(one vs.
# three)", which read as a comparison of two groups (#145).
nomo_invariance_pretty_constraint <- function(constraint, fit, occasion_map = NULL) {
  if (is.null(fit) ||
      !is.character(constraint) ||
      length(constraint) != 1L ||
      is.na(constraint) ||
      !nzchar(constraint)) {
    return(constraint)
  }

  parts <- strsplit(
    constraint,
    "\\s*==\\s*",
    perl = TRUE
  )[[1L]]

  if (length(parts) != 2L) return(constraint)

  pt <- tryCatch(
    as.data.frame(lavaan::parTable(fit)),
    error = function(e) NULL
  )

  if (is.null(pt) || !nrow(pt)) return(constraint)

  plabel_col <- if ("plabel" %in% names(pt)) {
    "plabel"
  } else if ("label" %in% names(pt)) {
    "label"
  } else {
    NULL
  }

  if (is.null(plabel_col)) return(constraint)

  left <- which(as.character(pt[[plabel_col]]) == trimws(parts[[1L]]))
  right <- which(as.character(pt[[plabel_col]]) == trimws(parts[[2L]]))

  if (!length(left) || !length(right)) return(constraint)

  group_labels <- tryCatch(
    as.character(lavaan::lavInspect(fit, "group.label")),
    error = function(e) character()
  )

  a <- nomo_invariance_parameter_label(
    pt,
    left[[1L]],
    group_labels,
    occasion_map
  )
  b <- nomo_invariance_parameter_label(
    pt,
    right[[1L]],
    group_labels,
    occasion_map
  )

  if (identical(a$base, b$base)) {
    if (nzchar(a$group) || nzchar(b$group)) {
      units <- if (is.null(occasion_map)) {
        length(group_labels)
      } else {
        length(unique(occasion_map$occasion))
      }
      group_text <- if (units > 2L && nzchar(b$group)) {
        paste(b$group, "vs. others")
      } else {
        paste(c(a$group, b$group)[nzchar(c(a$group, b$group))], collapse = " vs. ")
      }
      return(paste0(a$base, " (", group_text, ")"))
    }
    return(a$base)
  }

  left_label <- if (nzchar(a$group)) {
    paste0(a$base, " [", a$group, "]")
  } else {
    a$base
  }
  right_label <- if (nzchar(b$group)) {
    paste0(b$base, " [", b$group, "]")
  } else {
    b$base
  }

  paste(left_label, "=", right_label)
}


nomo_invariance_local_strain_display <- function(x) {
  dat <- x$local_strain
  if (!nrow(dat)) return(dat)
  occasion_map <- if (identical(x$design, "occasions")) {
    named <- c(x$long_items, x$long_factors)
    tibble::tibble(
      name = unname(unlist(named)),
      generic = rep(names(named), lengths(named)),
      occasion = rep(x$occasions, length(named))
    )
  }

  dat$constraint_display <- vapply(
    seq_len(nrow(dat)),
    function(i) {
      level <- as.character(dat$level[[i]])
      fit <- x$fits[[level]]
      nomo_invariance_pretty_constraint(
        dat$constraint[[i]],
        fit,
        occasion_map
      )
    },
    character(1)
  )

  dat
}


# With three or more groups or occasions, the sentence that says what a label
# such as "(three vs. others)" tests; empty with two.
nomo_invariance_others_note <- function(x) {
  if (length(x$groups) < 3L) return("")
  unit <- if (identical(x$design, "occasions")) "occasion" else "group"
  sprintf(
    "Each diagnostic frees one %s's parameter while the other %ss stay equal: \"vs. others\" names the %s freed.",
    unit, unit, unit
  )
}


# What each level holds equal beyond the level before it, as a line under the
# fit table. A column of the cumulative constraints was too wide for the
# console once a strict level held loadings, intercepts, and residuals equal,
# and it pushed RMSEA and SRMR out of the table.
#
# A researcher-specified release is named beside the level it is first made
# at, as in "intercepts from scalar, except ag3 ~ 1": without it, the line said
# that the released intercept was held equal too.
nomo_invariance_present_constraints <- function(fit) {
  held <- lapply(strsplit(fit$constraints, ", ", fixed = TRUE), setdiff, "none")
  released <- fit[["partial_requested"]]
  if (is.null(released)) released <- rep("", nrow(fit))
  released <- strsplit(released, "; ", fixed = TRUE)
  added <- vapply(seq_along(held), function(i) {
    new <- setdiff(held[[i]], if (i > 1L) held[[i - 1L]])
    if (!length(new)) return("")
    freed <- setdiff(released[[i]], c("", if (i > 1L) released[[i - 1L]]))
    # A release such as "ag3 ~ 1" is kept on one line.
    freed <- gsub(" ", nomo_present_nbsp, freed, fixed = TRUE)
    paste0(
      paste(nomo_present_or(new, "and"), "from", fit$level[[i]]),
      if (length(freed)) paste(", except", nomo_present_or(freed, "and"))
    )
  }, character(1))
  added <- added[nzchar(added)]
  if (length(added)) {
    nomo_present_text("Held equal: ", paste(added, collapse = "; "), ".", indent = 2L)
  }
}


# Flags ---------------------------------------------------------------------------

# Each flag in the decision log under the unit it concerns (guide point 22): a
# level, the cases, the score diagnostics, the identification, or the items
# and factors named. A level that failed, did not converge, or raised a lavaan
# warning is flagged with what went wrong.
nomo_invariance_flag_units <- function(log) {
  units <- c(score_diagnostics = "Score diagnostics",
             factor_identification = "Identification", cases_used = "Cases")
  out <- unname(units[log$metric])
  ifelse(is.na(out), log$object, out)
}


nomo_invariance_present_flagged <- function(log, recommendation = FALSE) {
  if (!is.data.frame(log) || !nrow(log)) return(invisible(NULL))
  nomo_present_flagged(log, recommendation = recommendation,
                       unit = nomo_invariance_flag_units(log))
}


# print() and summary() ---------------------------------------------------------------

#' @export
print.nomo_invariance <- function(x, ...) {
  object <- x
  x <- nomo_invariance_display_fields(x)
  nomo_invariance_present_header(x)
  nomo_invariance_present_facts(x)
  nomo_present_facts(sprintf("Requested: %s", paste(x$requested_levels, collapse = " -> ")))
  nomo_present_facts(sprintf("Completed: %s", nomo_invariance_level_path(x$completed_levels)))

  nomo_present_section("Fit by level")
  nomo_present_table(
    x$fit_evidence,
    c("Level" = "level", "CFI" = "cfi", "RMSEA" = "rmsea", "SRMR" = "srmr",
      "CFI change" = "delta_cfi", "RMSEA change" = "delta_rmsea",
      "LRT p" = "lrt_p"),
    formats = nomo_invariance_fit_formats(),
    more = "nomo_table(x, \"fit\")"
  )
  nomo_invariance_present_versions(x, chisq = FALSE)
  nomo_invariance_present_flagged(x$decision_log)

  nomo_invariance_key_note(c("CFI", "RMSEA", "SRMR", "LRT", x$estimator_shown))
  cat("\n")
  nomo_present_text(
    "Fit changes and score diagnostics are evidence, not pass/fail rules, and ",
    "nomologR never frees a parameter because of them."
  )
  n_local <- nrow(x$local_strain)
  nomo_present_pointer(
    c("summary(x)", if (n_local) "nomo_table(x, \"local_strain\")"),
    c("each level's chi-square test",
      if (n_local) sprintf("all %s", nomo_present_count(n_local, "score diagnostic")))
  )
  invisible(object)
}


#' @export
summary.nomo_invariance <- function(object, ...) {
  local_display <- nomo_invariance_local_strain_display(object)
  # The ten largest across all levels. The diagnostics come in level order, so
  # the first ten had been the metric level's alone, and a scalar-level strain
  # (an intercept) never reached the summary.
  top_local <- if (nrow(local_display)) {
    utils::head(local_display[order(-local_display$score_x2), , drop = FALSE], 10L)
  } else {
    tibble::tibble()
  }
  shown <- nomo_invariance_display_fields(object)

  out <- list(
    design = object$design,
    group = object$group,
    groups = object$groups,
    data_n = object$data_n,
    n_used = object[["n_used"]],
    group_n = object[["group_n"]],
    estimator_shown = shown$estimator_shown,
    test_label = shown$test_label,
    fit_variants = object[["fit_variants"]],
    indicator_type = object$indicator_type,
    identification_note = object$identification_note,
    ordered = object$ordered,
    ordered_categories = object$ordered_categories,
    ID.cat = object$ID.cat,
    parameterization = object$parameterization,
    requested_levels = object$requested_levels,
    completed_levels = object$completed_levels,
    fit_evidence = object$fit_evidence,
    partial = object$partial,
    partial_requested = object$partial_requested,
    top_local_strain = top_local,
    n_local_strain = nrow(local_display),
    latent_means = object[["latent_means"]],
    decision_log = object$decision_log,
    note = paste(
      "No single CFI change, RMSEA change, SRMR change, chi-square difference,",
      "or score diagnostic is treated as a universal invariance rule."
    )
  )
  class(out) <- c("summary_nomo_invariance", "list")
  out
}


#' @export
print.summary_nomo_invariance <- function(x, ...) {
  across_occasions <- identical(x$design, "occasions")
  formats <- nomo_invariance_fit_formats()
  nomo_invariance_present_header(x, summary = TRUE)
  nomo_invariance_present_facts(x)
  nomo_present_facts(sprintf(
    "Levels completed: %s", nomo_invariance_level_path(x$completed_levels)
  ))

  nomo_present_section("Identification and sequence")
  nomo_present_text(x$identification_note, indent = 2L)

  if (nrow(x$ordered_categories)) {
    nomo_present_section("Observed ordered categories")
    nomo_present_table(x$ordered_categories, c("Item" = "item", "Categories" = "categories"),
                       more = "nomo_table(x, \"categories\")")
  }

  fit <- x$fit_evidence
  nomo_present_section("Fit by level")
  nomo_present_table(
    fit,
    c("Level" = "level", "Chi-square" = "chisq", "df" = "df", "p" = "pvalue",
      "CFI" = "cfi", "RMSEA" = "rmsea", "SRMR" = "srmr"),
    formats = formats,
    more = "nomo_table(x, \"fit\")"
  )
  nomo_invariance_present_constraints(fit)
  nomo_invariance_present_versions(x)
  # A single level has no change to show (#145).
  if (nrow(fit) > 1L) {
    nomo_present_section("Changes from the preceding level")
    nomo_present_table(
      fit[-1L, , drop = FALSE],
      c("Level" = "level", "CFI change" = "delta_cfi", "RMSEA change" = "delta_rmsea",
        "SRMR change" = "delta_srmr", "Delta chi-square" = "lrt_chisq", "df" = "lrt_df",
        "p" = "lrt_p"),
      formats = formats,
      more = "nomo_table(x, \"fit\")"
    )
  }

  if (!is.null(x$partial) && x$partial$n > 0L) {
    nomo_present_section("Researcher-specified partial invariance")
    nomo_partial_present_releases(x$partial$releases)
  }

  means <- x[["latent_means"]]
  has_means <- is.data.frame(means) && nrow(means) > 0L
  if (has_means) {
    shown <- means
    shown$interval <- nomo_present_ci(shown$ci_lower, shown$ci_upper, kind = "estimate")
    nomo_present_section(if (across_occasions) {
      sprintf("Latent change from %s, in its latent standard deviations",
              shown$reference_occasion[[1L]])
    } else {
      sprintf("Latent means relative to %s, in its latent standard deviations",
              shown$reference_group[[1L]])
    })
    columns <- c("Level" = "level", "Group" = "group", "Occasion" = "occasion",
                 "Factor" = "factor", "Difference" = "estimate", "95% CI" = "interval",
                 "p" = "p_value")
    # Across occasions the difference is a change from the first occasion.
    if (across_occasions) names(columns)[columns == "estimate"] <- "Change"
    nomo_present_table(
      shown,
      columns,
      formats = list(estimate = function(v) nomo_present_stat(v, "estimate"),
                     p_value = nomo_present_p),
      more = "nomo_table(x, \"latent_means\")"
    )
    nomo_present_text(
      "Comparable only with invariant intercepts, full or partial.", indent = 2L
    )
  }

  if (nrow(x$top_local_strain)) {
    nomo_present_section("Largest score diagnostics for equality constraints")
    nomo_present_table(
      x$top_local_strain,
      c("Level" = "level", "Constraint" = "constraint_display",
        "Score chi-square" = "score_x2", "df" = "df", "p" = "p_value"),
      formats = list(score_x2 = function(v) nomo_present_stat(v, "stat"),
                     df = function(v) nomo_present_stat(v, "df"), p_value = nomo_present_p),
      more = "nomo_table(x, \"local_strain\")"
    )
    others <- nomo_invariance_others_note(x)
    if (nzchar(others)) nomo_present_text(others, indent = 2L)
  }

  nomo_invariance_present_flagged(x$decision_log, recommendation = TRUE)
  nomo_present_key(nomo_invariance_key_entries(c(
    "CFI", "RMSEA", "SRMR", "df", if (has_means) "CI", x$estimator_shown
  )))

  cat("\n")
  nomo_present_text(x$note)
  n_local <- x[["n_local_strain"]]
  nomo_present_pointer(
    c(if (length(n_local) && n_local > 0L) "nomo_table(x, \"local_strain\")",
      "nomo_table(x, \"decision_log\")"),
    c(if (length(n_local) && n_local > 0L) {
      sprintf("all %s", nomo_present_count(n_local, "score diagnostic"))
    }, "every recorded decision")
  )
  invisible(x)
}


# Plots --------------------------------------------------------------------------------

# The level names along the x-axis of each facet, angled: three facets share a
# 7-inch plot, so four or more level names such as "configural" and "metric"
# ran into each other when set horizontally.
nomo_invariance_level_axis <- function() {
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}


#' Plot measurement-invariance evidence
#'
#' @param x A `nomo_invariance` object.
#' @param type Plot type: `"fit"`, `"change"`, or `"local_strain"`.
#' @param ... Unused.
#'
#' @return A `ggplot2` object. `"fit"` shows CFI, RMSEA, and SRMR at each
#'   level, `"change"` each one's change from the level before, and
#'   `"local_strain"` the 20 largest score diagnostics for the equality
#'   constraints, one panel per level.
#' @export
plot.nomo_invariance <- function(
    x,
    type = c("fit", "change", "local_strain"),
    ...) {
  type <- nomo_match_arg(type)

  if (type == "fit") {
    pieces <- lapply(
      c("cfi", "rmsea", "srmr"),
      function(metric) {
        data.frame(
          level = x$fit_evidence$level,
          metric = toupper(metric),
          value = x$fit_evidence[[metric]]
        )
      }
    )
    dat <- do.call(rbind, pieces)
    dat <- dat[is.finite(dat$value), , drop = FALSE]

    if (!nrow(dat)) {
      stop("No finite invariance fit evidence is available.", call. = FALSE)
    }

    dat$level <- factor(dat$level, levels = x$completed_levels)

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(
          x = level,
          y = value,
          group = 1
        )
      ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(size = 2.8) +
        ggplot2::facet_wrap(
          stats::as.formula("~ metric"),
          scales = "free_y"
        ) +
        nomo_plot_labs(
          title = "Measurement-invariance fit across levels",
          subtitle = paste(
            "Absolute fit should be read alongside change evidence",
            "and localized strain."
          ),
          x = "Invariance level",
          y = "Fit index",
          caption = paste(
            "No single fit index determines invariance. CFI = comparative fit index;",
            "RMSEA = root mean square error of approximation; SRMR = standardized",
            "root mean square residual."
          )
        ) +
        ggplot2::theme_minimal() +
        nomo_invariance_level_axis()
    )
  }

  if (type == "change") {
    pieces <- lapply(
      c("delta_cfi", "delta_rmsea", "delta_srmr"),
      function(metric) {
        data.frame(
          level = x$fit_evidence$level,
          metric = nomo_invariance_metric_label(metric),
          value = x$fit_evidence[[metric]]
        )
      }
    )
    dat <- do.call(rbind, pieces)
    dat <- dat[is.finite(dat$value), , drop = FALSE]

    if (!nrow(dat)) {
      stop(
        "No finite change-in-fit evidence is available; at least two completed levels are needed.",
        call. = FALSE
      )
    }

    dat$metric <- factor(
      dat$metric,
      levels = nomo_invariance_metric_label(c("delta_cfi", "delta_rmsea", "delta_srmr"))
    )
    dat$level <- factor(dat$level, levels = x$completed_levels)

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(
          x = level,
          y = value,
          group = 1
        )
      ) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::geom_line() +
        ggplot2::geom_point(size = 2.8) +
        ggplot2::facet_wrap(
          stats::as.formula("~ metric"),
          scales = "free_y"
        ) +
        nomo_plot_labs(
          title = "Change in fit as equality constraints accumulate",
          subtitle = "The dashed line marks no change from the preceding fitted level.",
          x = "Invariance level",
          y = "Change in fit index",
          caption = paste(
            "A decrease in CFI and an increase in RMSEA or SRMR indicate worse",
            "fit. Interpret magnitude in context; no universal cutoff is imposed.",
            "CFI = comparative fit index; RMSEA = root mean square error of",
            "approximation; SRMR = standardized root mean square residual."
          )
        ) +
        ggplot2::theme_minimal() +
        nomo_invariance_level_axis()
    )
  }

  dat <- nomo_invariance_local_strain_display(x)
  # Without diagnostics the table has no score column to read (#145).
  if (nrow(dat)) dat <- dat[is.finite(dat$score_x2), , drop = FALSE]

  if (!nrow(dat)) {
    stop(
      "No equality-constraint score diagnostics are available to plot.",
      call. = FALSE
    )
  }

  dat <- utils::head(
    dat[order(dat$score_x2, decreasing = TRUE), , drop = FALSE],
    20L
  )
  dat$constraint_display <- factor(
    dat$constraint_display,
    levels = rev(unique(dat$constraint_display))
  )
  dat$level <- factor(
    dat$level,
    levels = unique(c(x$completed_levels, as.character(dat$level)))
  )
  others <- nomo_invariance_others_note(x)

  # One panel per level, the constraints sharing rows across them. With one
  # shape per level in a single panel, a level's point sat on another's when
  # their scores were close, as an item's scalar and strict intercepts across
  # occasions often are, and the level beneath could not be seen.
  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = score_x2,
      y = constraint_display
    )
  ) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::facet_wrap(stats::as.formula("~ level"), nrow = 1L) +
    ggplot2::expand_limits(x = 0) +
    nomo_plot_labs(
      title = "Largest score diagnostics for equality constraints",
      subtitle = paste(
        "Larger score chi-squares localize strain in equality constraints.",
        "\nThey do not authorize automatic partial invariance."
      ),
      x = "Score chi-square",
      y = NULL,
      # Wrapped here, since a caption with a line break is left as written.
      caption = paste(unlist(lapply(
        c(others[nzchar(others)],
          "Any release must be specified by the researcher and documented with a rationale."),
        strwrap, width = 85L
      )), collapse = "\n")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(8, 16, 8, 8)
    )
}
