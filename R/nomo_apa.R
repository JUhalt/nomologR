# Manuscript-ready tables ------------------------------------------------------
#
# The formatting rules follow the Publication Manual of the American
# Psychological Association (7th ed.), as summarized by Purdue OWL; APA's own
# site blocks automated access, so the summary was quoted rather than the
# Manual. See #35 for the rules and their sources, and #144 for the output
# style shared with contentvalidR.
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
  # Rounded half away from zero, as the console rounds (#144), so .625 is .63;
  # and rounded first so a small negative value that rounds to zero is written
  # as zero, not as a signed "-.000".
  x <- nomo_present_round(x, digits)
  out <- formatC(x, format = "f", digits = digits)
  if (isTRUE(bounded)) {
    out <- sub("^(-?)0\\.", "\\1.", out)
  }
  out[!is.finite(x)] <- nomo_apa_dash
  out
}


# p values are bounded, so they take no leading zero; anything below .001 is
# written as a bound rather than a rounded zero, and anything that would round
# to 1.000 as "> .999".
nomo_apa_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  out <- nomo_apa_number(p, digits = 3L, bounded = TRUE)
  out[is.finite(p) & out == "1.000"] <- "> .999"
  out[is.finite(p) & p < 0.001] <- "< .001"
  out
}


# Degrees of freedom as the console prints them: whole, or with two decimals
# for an adjusted test whose degrees of freedom are fractional, which a whole
# number would misreport.
nomo_apa_df <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- nomo_present_stat(x, "df")
  out[!is.finite(x)] <- nomo_apa_dash
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
  if (!is.character(title) || length(title) != 1L || is.na(title)) {
    stop("`title` must be NULL or one string.", call. = FALSE)
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


# A result with one table takes no `type`, so a value given is refused rather
# than ignored.
nomo_apa_one_table <- function(type, source) {
  if (!is.null(type)) {
    stop(sprintf(
      "`type` must be NULL for a `%s` result, which has one APA table, not %s.",
      source, paste(sprintf('"%s"', as.character(type)), collapse = ", ")
    ), call. = FALSE)
  }
  invisible(NULL)
}


# Notes --------------------------------------------------------------------------

# Sentences joined into one note, with the empty ones left out.
nomo_apa_join <- function(...) {
  parts <- unlist(list(...), use.names = FALSE)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  paste(parts, collapse = " ")
}


# How a model was estimated and on how many cases (#145): the estimator spelled
# out with its abbreviation, as APA asks of every abbreviation, and N.
nomo_apa_sample_note <- function(estimator, n = NA_real_) {
  estimator <- as.character(estimator)
  estimator <- if (length(estimator)) estimator[[1L]] else NA_character_
  n <- suppressWarnings(as.numeric(n))
  n <- if (length(n)) n[[1L]] else NA_real_
  words <- if (is.na(estimator) || !nzchar(estimator)) {
    ""
  } else if (estimator %in% names(nomo_cfa_glossary)) {
    sprintf("Estimated with %s (%s)", nomo_cfa_glossary[[estimator]], estimator)
  } else {
    sprintf("Estimated with %s", estimator)
  }
  size <- if (is.finite(n)) sprintf("*N* = %d", as.integer(round(n))) else ""
  text <- c(words, size)
  text <- paste(text[nzchar(text)], collapse = "; ")
  if (nzchar(text)) paste0(text, ".") else ""
}


# The estimator lavaan used, for a result that does not record it.
nomo_apa_fit_estimator <- function(fit) {
  estimator <- tryCatch(lavaan::lavInspect(fit, "options")$estimator,
                        error = function(e) NULL)
  if (is.character(estimator) && length(estimator) == 1L) estimator else NA_character_
}


# Which chi-square and which versions of the fit indices a table shows (#145).
# `chisq` is "scaled" for a scaled test statistic; `indices` names each index's
# version: "robust", "scaled", or "" for the standard one. `models` above 1
# adds the scaled difference tests of an invariance table. Empty when every
# value is the standard one.
nomo_apa_versions_note <- function(chisq, indices, test = "", models = 1L) {
  parts <- character()
  if (identical(chisq, "scaled")) {
    if (!nzchar(test)) test <- "scaled"
    parts <- if (models > 1L) {
      sprintf(paste(
        "The \u03c7\u00b2 values are %s test statistics, and the",
        "\u0394\u03c7\u00b2 values are scaled difference tests."
      ), test)
    } else {
      sprintf("The \u03c7\u00b2 value is the %s test statistic.", test)
    }
  }
  indices <- indices[!is.na(indices) & nzchar(indices)]
  for (v in unique(indices)) {
    named <- names(indices)[indices == v]
    parts <- c(parts, sprintf(
      "%s %s.", nomo_present_or(named, "and"),
      nomo_present_noun(length(named), paste("is a", v, "value"), paste("are", v, "values"))
    ))
  }
  paste(parts, collapse = " ")
}


# A fit statistic's version from its lavaan name: "robust", "scaled", or "".
nomo_apa_version <- function(variant) {
  variant <- as.character(variant)
  ifelse(grepl("robust", variant, fixed = TRUE), "robust",
         ifelse(grepl("scaled", variant, fixed = TRUE), "scaled", ""))
}


# An empty cell is explained in the general note whenever a table has one
# (APA 7, Section 7.14; guide point 31), in the form contentvalidR uses.
nomo_apa_dash_note <- function(body, reason) {
  cells <- unlist(body[-1L], use.names = FALSE)
  if (any(cells == nomo_apa_dash)) sprintf("%s = %s.", nomo_apa_dash, reason) else ""
}


# Superscript letters for specific notes: a, b, c in the order the marked cells
# are read, across each row from the top. `marks` is a list of list(rows =
# logical, column = heading, note = sentence); a cell with two marks carries
# both letters, "^a,b^".
nomo_apa_mark <- function(body, marks) {
  marks <- Filter(function(m) any(m$rows), marks)
  if (!length(marks)) return(list(body = body, specific = character()))
  first <- vapply(marks, function(m) {
    which(m$rows)[[1L]] * (ncol(body) + 1L) + match(m$column, names(body))
  }, numeric(1))
  marks <- marks[order(first)]
  tags <- matrix("", nrow(body), ncol(body))
  for (i in seq_along(marks)) {
    col <- match(marks[[i]]$column, names(body))
    rows <- which(marks[[i]]$rows)
    tags[rows, col] <- ifelse(nzchar(tags[rows, col]),
                              paste0(tags[rows, col], ",", letters[[i]]), letters[[i]])
  }
  for (j in seq_len(ncol(body))) {
    tagged <- nzchar(tags[, j])
    body[[j]][tagged] <- paste0(body[[j]][tagged], "^", tags[tagged, j], "^")
  }
  list(body = body, specific = vapply(marks, `[[`, character(1), "note"))
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
#' standardized loadings without the zero; this follows the rule as written,
#' as the output style shared with `contentvalidR` does.
#'
#' **No verdicts.** Table notes keep the package's reference-value language.
#' No cell reads PASS or FAIL, and fit indices are not labeled good or poor.
#'
#' **Notes say what the table shows.** The general note defines each
#' abbreviation, gives the estimator and the number of cases analyzed, and
#' says when the chi-square is a scaled test statistic and the fit indices are
#' robust or scaled values. For a network's hypotheses it also defines each
#' evidence label the table shows, such as "Concordant", and each prediction
#' is given with the region it names. An interval's heading carries its level,
#' such as "90% CI" for a reliability interval bootstrapped at that level. A
#' cell left empty is an em dash, explained in the note; a cell that does not
#' apply, such as the change in fit of the first invariance model, is blank.
#' Specific notes, marked with superscript letters, name the parameters a
#' partial invariance model frees, the hypotheses specified post hoc, the
#' estimates whose interval is the equivalence interval a `negligible()`
#' prediction is judged on, and the estimates on another scale than the rest.
#' A correlation the model fixes, such as the zero correlations of a bifactor
#' model, is not tabled as an estimate. Degrees of freedom that are not whole,
#' as for a mean- and variance-adjusted test, are given to two decimals.
#'
#' **Discriminant evidence without the Fornell-Larcker matrix.** For a
#' `nomo_validity` result, the `"discriminant"` table gives each pair of
#' constructs one row: the latent correlation with its 95% confidence interval
#' (Rönkkö & Cho, 2022), and the heterotrait-monotrait ratios HTMT2 (Roemer et
#' al., 2021) and HTMT (Henseler et al., 2015) when they were computed. It does
#' not print the correlation matrix with the square root of AVE on its
#' diagonal, because that comparison often misses discriminant-validity
#' problems (Henseler et al., 2015). AVE is convergent evidence and has its own
#' `"convergent"` table.
#'
#' **Knitting.** In an R Markdown or Quarto document the table renders as a
#' table with its number, title, and notes. Each column's share of the page
#' follows its widest entry. Knitted to PDF, the Greek letters and math
#' symbols are written as TeX math, so the default `pdflatex` engine compiles
#' them.
#'
#' The rules come from the *Publication Manual of the American Psychological
#' Association* (7th ed.), checked against Purdue OWL's APA 7 guides.
#' Journal-specific templates are out of scope.
#'
#' **Stability.** The `type` values and the structure of the returned object
#' follow the stability policy in `?nomologR`. Formatting may still be
#' corrected where it departs from APA style, with the correction described in
#' NEWS.
#'
#' @param x A result object: `nomo_cfa`, `nomo_reliability`, `nomo_retest`,
#'   `nomo_validity`, `nomo_invariance`, or `nomo_network`.
#' @param type Which table to build. For `nomo_cfa`: `"loadings"`, `"fit"`, or
#'   `"factor_correlations"`. For `nomo_validity`: `"discriminant"` or
#'   `"convergent"`. For `nomo_network`: `"hypotheses"` or `"fit"`. Other
#'   objects have one table each, so `type` is left `NULL` for them; any other
#'   value is an error.
#' @param number Optional table number, printed in bold as "Table 1". Without
#'   one, the number line is left out, for the document to supply.
#' @param title Optional title, one string; a descriptive default is supplied.
#' @param ... Unused.
#'
#' @return A `nomo_apa_table` object, which prints in the console and renders
#'   as a formatted table, with its title and notes, when knitted. The fields
#'   to read are:
#'
#'   * `number`: the table number, an integer, or `NULL` when none was given.
#'   * `title`: the table title.
#'   * `body`: a data frame of formatted cells, all character. Its column
#'     names are the column headings.
#'   * `stub`: the name of the stub column, the first column of `body`, which
#'     says what each row describes.
#'   * `notes`: a list of three character vectors, `general`, `specific`, and
#'     `probability`, the three kinds of APA table note in the order they are
#'     printed. Any of them may be empty.
#'   * `source`: the class of the result the table was built from, such as
#'     `"nomo_cfa"`.
#'
#'   Headings, cells, and notes are written in Markdown, as pandoc reads it:
#'   italics as `*p*` and a superscript as `^a^`. `print()` shows the table as
#'   aligned text under its number and title, with its notes, the italics
#'   dropped, and a note marker written `(a)`; a table wider than the console
#'   wraps its long headings and text cells, and names any column it still
#'   cannot fit.
#'
#' @references
#' American Psychological Association. (2020). *Publication manual of the
#' American Psychological Association* (7th ed.).
#' \doi{10.1037/0000165-000}
#'
#' Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models
#' with unobservable variables and measurement error. *Journal of Marketing
#' Research, 18*(1), 39-50. \doi{10.2307/3151312}
#'
#' Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using
#' structural equation modeling: An alternative to coefficient alpha.
#' *Psychometrika, 74*(1), 155-167. \doi{10.1007/s11336-008-9099-3}
#'
#' Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for
#' assessing discriminant validity in variance-based structural equation
#' modeling. *Journal of the Academy of Marketing Science, 43*(1), 115-135.
#' \doi{10.1007/s11747-014-0403-8}
#'
#' Roemer, E., Schuberth, F., & Henseler, J. (2021). HTMT2--An improved
#' criterion for assessing discriminant validity in structural equation
#' modeling. *Industrial Management & Data Systems, 121*(12), 2637-2650.
#' \doi{10.1108/IMDS-02-2021-0082}
#'
#' Rönkkö, M., & Cho, E. (2022). An updated guideline for assessing
#' discriminant validity. *Organizational Research Methods, 25*(1), 6-47.
#' \doi{10.1177/1094428120968614}
#'
#' @examples
#' model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
#'
#' nomo_apa_table(cfa, "loadings", number = 1)
#' nomo_apa_table(cfa, "fit", number = 2)
#' nomo_apa_table(nomo_reliability(cfa), number = 3)
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
      "`. Supported: nomo_cfa, nomo_reliability, nomo_retest, nomo_validity,",
      " nomo_invariance, nomo_network."
    ),
    call. = FALSE
  )
}


#' @export
nomo_apa_table.nomo_cfa <- function(x, type = c("loadings", "fit", "factor_correlations"),
                                    number = NULL, title = NULL, ...) {
  type <- nomo_match_arg(type)
  sample_note <- nomo_apa_sample_note(x$estimator, x$n_used)

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
      general = nomo_apa_join(
        "Standardized loadings from a confirmatory factor analysis.",
        sample_note,
        "Blank cells are loadings fixed to zero by the model.",
        nomo_apa_dash_note(body, "not estimated")
      ),
      number = number, source = "nomo_cfa"
    ))
  }

  if (type == "factor_correlations") {
    fc <- x$factor_correlations
    if (length(unique(as.character(x$standardized_loadings$factor))) < 2L) {
      stop("The model has one factor, so there are no factor correlations.", call. = FALSE)
    }
    # A correlation the model fixes, such as a zero of a bifactor model, is not
    # an estimate, so it is not tabled as one with a degenerate interval (#145).
    fixed <- nomo_cfa_fixed_correlations(fc, x$fit)
    pairs <- paste(fc$factor1, "with", fc$factor2)
    fc <- fc[!fixed, , drop = FALSE]
    if (!nrow(fc)) {
      stop(paste(
        "No factor correlation is estimated in this model: the model fixes each",
        "one, or relates its factors through a higher-order factor."
      ), call. = FALSE)
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
      general = nomo_apa_join(
        "*r* = latent correlation from the confirmatory factor analysis; CI =",
        "confidence interval.",
        sample_note,
        if (any(fixed)) {
          sprintf("Correlations the model fixes are not shown: %s.",
                  nomo_present_or(pairs[fixed], "and"))
        },
        nomo_apa_dash_note(body, "not estimated")
      ),
      number = number, source = "nomo_cfa"
    ))
  }

  fe <- x$fit_evidence
  variant <- if ("variant" %in% names(fe)) fe$variant else rep(NA_character_, nrow(fe))
  version <- stats::setNames(nomo_apa_version(variant), fe$metric)
  nomo_apa_fit_table(
    fe, estimator = x$estimator, n = x$n_used,
    title = title, number = number, source = "nomo_cfa",
    versions = nomo_apa_versions_note(
      version[["chi_square"]], version[c("CFI", "TLI", "RMSEA")],
      test = nomo_cfa_test_label(x$fit)
    )
  )
}


nomo_apa_fit_table <- function(fe, estimator, n, title, number, source,
                               model_label = "Measurement model", versions = "",
                               sample = "") {
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
    df = nomo_apa_df(value("df")),
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
    # Abbreviations in the order the headings show them (guide point 31).
    general = nomo_apa_join(
      "CFI = comparative fit index; TLI = Tucker-Lewis index; RMSEA = root mean",
      "square error of approximation; CI = confidence interval; SRMR =",
      "standardized root mean square residual.",
      nomo_apa_sample_note(estimator, n),
      sample,
      versions,
      nomo_apa_dash_note(body, "not available"),
      "Fit indices are reported as evidence, not against fixed cutoffs."
    ),
    number = number, source = source
  )
}


# The omega a table reports, by the scale of its indicators' scores.
nomo_apa_omega_words <- c(
  continuous = "coefficient omega",
  ordinal = paste("categorical omega, for the sum of the observed ordinal item",
                  "scores (Green & Yang, 2009)"),
  latent = paste("coefficient omega for the continuous latent responses",
                 "underlying the ordered items")
)


#' @export
nomo_apa_table.nomo_reliability <- function(x, type = NULL, number = NULL,
                                            title = NULL, ...) {
  nomo_apa_one_table(type, "nomo_reliability")
  # A multi-group fit has a row per construct and group, labeled by group (#145).
  keys <- unique(dplyr::bind_rows(
    x$omega[, c("construct", "block")], x$alpha[, c("construct", "block")]
  ))
  pick <- function(i, tbl) {
    row <- tbl[tbl$construct == keys$construct[[i]] & tbl$block == keys$block[[i]], ,
               drop = FALSE]
    if (!nrow(row)) return(nomo_apa_dash)
    nomo_apa_interval(row$estimate[[1L]], row$ci_lower[[1L]], row$ci_upper[[1L]],
                      bounded = TRUE)
  }
  label <- if (length(unique(keys$block)) > 1L) {
    paste0(keys$construct, " (", keys$block, ")")
  } else {
    keys$construct
  }
  omega <- vapply(seq_len(nrow(keys)), pick, character(1), tbl = x$omega)
  alpha <- vapply(seq_len(nrow(keys)), pick, character(1), tbl = x$alpha)
  # Alpha is tabled only where some construct has it, so the note never
  # describes a column of dashes (#145).
  has_alpha <- any(alpha != nomo_apa_dash)
  has_ci <- any(is.finite(c(x$omega$ci_lower, x$alpha$ci_lower)))
  # The level the intervals were computed at, not a fixed 95% (#145).
  ci <- nomo_reliability_ci_label(x$ci_status)
  heading <- function(symbol) if (has_ci) sprintf("%s [%s]", symbol, ci) else symbol
  omega_heading <- heading("\u03c9")
  body <- data.frame(Construct = label, omega = omega, stringsAsFactors = FALSE)
  names(body) <- c("Construct", omega_heading)
  if (has_alpha) body[[heading("\u03b1")]] <- alpha

  # Omega for ordered indicators is named for the scale it is on (#145).
  status <- x$alpha_status
  ordered <- status$indicator_type[match(keys$construct, status$construct)] %in% "ordered"
  scale <- ifelse(ordered, if (isTRUE(x$ordinal_scale)) "ordinal" else "latent", "continuous")
  one_scale <- length(unique(scale)) == 1L
  marks <- if (one_scale) list() else {
    lapply(setdiff(unique(scale), "continuous"), function(s) list(
      rows = scale == s & omega != nomo_apa_dash, column = omega_heading,
      note = paste0(nomo_apa_capitalize(nomo_apa_omega_words[[s]]), ".")
    ))
  }
  marked <- nomo_apa_mark(body, marks)

  requested <- status$requested %in% TRUE
  draws <- if (is.data.frame(x$ci_status) && nrow(x$ci_status)) {
    x$ci_status$requested_draws[[1L]]
  } else {
    NA_integer_
  }
  nomo_apa_new(
    body = marked$body,
    title = nomo_apa_or(title, "Reliability Estimates"),
    stub = "Construct",
    general = nomo_apa_join(
      paste0(
        "\u03c9 = ",
        if (one_scale) nomo_apa_omega_words[[scale[[1L]]]] else "coefficient omega",
        if (has_ci) "; CI = confidence interval",
        if (has_alpha) "; \u03b1 = coefficient alpha",
        "."
      ),
      if (has_ci) {
        sprintf("%s = percentile bootstrap confidence interval%s.", ci,
                if (is.finite(draws)) sprintf(", from %d draws", as.integer(draws)) else "")
      },
      if (has_alpha) {
        paste(
          "Coefficient alpha assumes equal loadings and is reported alongside omega",
          "for comparison with published work."
        )
      } else if (any(requested)) {
        if (all(ordered)) {
          paste(
            "Coefficient alpha is not reported: observed-score alpha is not",
            "computed for ordered indicators, so omega is the reliability estimate."
          )
        } else {
          "Coefficient alpha could not be computed for these constructs."
        }
      },
      nomo_apa_dash_note(body, "not computed")
    ),
    specific = marked$specific,
    number = number, source = "nomo_reliability"
  )
}


# A partial-invariance release in words: "the intercept of ag3", "the loading
# of x2 on F". Syntax of another form is given as written.
nomo_apa_release_words <- function(syntax, observed = character()) {
  s <- trimws(as.character(syntax))
  out <- s
  intercept <- grepl("^[^~=|]+~[[:space:]]*1$", s)
  out[intercept] <- paste("the intercept of", trimws(sub("~.*$", "", s[intercept])))
  loading <- grepl("=~", s, fixed = TRUE)
  out[loading] <- sprintf("the loading of %s on %s", trimws(sub("^.*=~", "", s[loading])),
                          trimws(sub("=~.*$", "", s[loading])))
  covariance <- grepl("~~", s, fixed = TRUE)
  lhs <- trimws(sub("~~.*$", "", s[covariance]))
  rhs <- trimws(sub("^.*~~", "", s[covariance]))
  out[covariance] <- ifelse(
    lhs == rhs,
    paste(ifelse(lhs %in% observed, "the residual variance of", "the variance of"), lhs),
    sprintf("the %s of %s and %s",
            ifelse(lhs %in% observed, "residual covariance", "covariance"), lhs, rhs)
  )
  threshold <- grepl("|", s, fixed = TRUE)
  out[threshold] <- sprintf("threshold %s of %s", trimws(sub("^.*\\|", "", s[threshold])),
                            trimws(sub("\\|.*$", "", s[threshold])))
  out
}


#' @export
nomo_apa_table.nomo_invariance <- function(x, type = NULL, number = NULL,
                                           title = NULL, ...) {
  nomo_apa_one_table(type, "nomo_invariance")
  fe <- x$fit_evidence
  # The first model has no model above it, so its change cells do not apply
  # and are blank; a dash marks a value that was not computed (#145).
  first <- seq_len(nrow(fe)) == 1L
  blank_first <- function(v) {
    v[first] <- ""
    v
  }
  lrt <- ifelse(
    is.finite(fe$lrt_chisq),
    sprintf("%s (%s)", nomo_apa_number(fe$lrt_chisq, 2L), nomo_apa_df(fe$lrt_df)),
    nomo_apa_dash
  )
  # A level fitted with researcher-specified releases is a partial model, and
  # its row says so (#145).
  partial <- if ("partial_requested" %in% names(fe)) {
    as.character(fe$partial_requested)
  } else {
    rep("", nrow(fe))
  }
  partial[is.na(partial)] <- ""
  model <- nomo_apa_capitalize(as.character(fe$level))
  model[nzchar(partial)] <- paste("Partial", tolower(model[nzchar(partial)]))
  body <- data.frame(
    Model = model,
    chisq = nomo_apa_number(fe$chisq, 2L),
    df = nomo_apa_df(fe$df),
    CFI = nomo_apa_number(fe$cfi, 3L, bounded = TRUE),
    RMSEA = nomo_apa_number(fe$rmsea, 3L, bounded = FALSE),
    SRMR = nomo_apa_number(fe$srmr, 3L, bounded = FALSE),
    dCFI = blank_first(nomo_apa_number(fe$delta_cfi, 3L, bounded = TRUE)),
    dRMSEA = blank_first(nomo_apa_number(fe$delta_rmsea, 3L, bounded = FALSE)),
    lrt = blank_first(lrt),
    p = blank_first(nomo_apa_p(fe$lrt_p)),
    stringsAsFactors = FALSE
  )
  names(body) <- c(
    "Model", "\u03c7\u00b2", "*df*", "CFI", "RMSEA", "SRMR",
    "\u0394CFI", "\u0394RMSEA", "\u0394\u03c7\u00b2 (\u0394*df*)", "*p*"
  )

  across_occasions <- identical(x$design, "occasions")
  across <- if (across_occasions) "occasions" else "groups"
  fit <- Find(Negate(is.null), x$fits)
  observed <- tryCatch(lavaan::lavNames(fit, "ov"), error = function(e) character())
  marks <- lapply(unique(partial[nzchar(partial)]), function(set) {
    words <- nomo_apa_release_words(strsplit(set, "; ", fixed = TRUE)[[1L]], observed)
    list(rows = partial == set, column = "Model", note = sprintf(
      "Partial invariance: %s %s freed across %s.", nomo_present_or(words, "and"),
      nomo_present_noun(length(words), "was", "were"), across
    ))
  })
  marked <- nomo_apa_mark(body, marks)

  # The sample: each group's n and N, with the estimator and the versions of
  # the fit statistics (#145).
  sizes <- x[["group_n"]]
  design <- if (across_occasions) {
    sprintf(
      "Occasions: %s. Each item's residuals are correlated across occasions.",
      paste(x$occasions, collapse = ", ")
    )
  } else if (is.data.frame(sizes) && nrow(sizes)) {
    sprintf("Grouping variable: %s, with %s.", x$group,
            nomo_present_or(sprintf("*n* = %d (%s)", as.integer(sizes$n), sizes$group), "and"))
  } else {
    sprintf("Grouping variable: %s.", x$group)
  }
  variants <- nomo_apa_or(x[["fit_variants"]], character())
  version <- function(name) {
    if (name %in% names(variants)) nomo_apa_version(variants[[name]]) else ""
  }
  nomo_apa_new(
    body = marked$body,
    title = nomo_apa_or(
      title,
      if (across_occasions) {
        "Measurement Invariance Across Occasions"
      } else {
        "Measurement Invariance Across Groups"
      }
    ),
    stub = "Model",
    general = nomo_apa_join(
      "CFI = comparative fit index; RMSEA = root mean square error of",
      "approximation; SRMR = standardized root mean square residual; \u0394 =",
      "change from the model above.",
      design,
      nomo_apa_sample_note(nomo_invariance_estimator(x), nomo_apa_or(x[["n_used"]], NA_real_)),
      nomo_apa_versions_note(
        version("chisq"), c(CFI = version("cfi"), RMSEA = version("rmsea")),
        test = nomo_invariance_test_label(x), models = nrow(fe)
      ),
      "Each model adds constraints to the one above it. Changes in fit are",
      "reported as evidence and are not compared with fixed cutoffs.",
      nomo_apa_dash_note(body, "not computed")
    ),
    specific = marked$specific,
    number = number, source = "nomo_invariance"
  )
}


# The sample a network table reports, beside the validation sample it leaves
# out (#145).
nomo_apa_network_sample <- function(x) {
  validation <- suppressWarnings(as.numeric(nomo_apa_or(x[["validation_n_used"]], NA_real_)))
  if (!length(validation) || !is.finite(validation[[1L]])) return("")
  role <- as.character(nomo_apa_or(x$sample_role, "primary"))[[1L]]
  sprintf("The table reports the %s sample; the validation sample (*N* = %d) is reported separately.",
          role, as.integer(round(validation[[1L]])))
}


#' @export
nomo_apa_table.nomo_network <- function(x, type = c("hypotheses", "fit"),
                                        number = NULL, title = NULL, ...) {
  type <- nomo_match_arg(type)
  # The cases the fit analyzed, which listwise deletion can make fewer than the
  # rows supplied, and the estimator lavaan used (#145).
  n <- suppressWarnings(as.numeric(nomo_apa_or(x[["n_used"]], NA_real_)))
  if (!length(n) || !is.finite(n[[1L]])) n <- x$data_n
  estimator <- as.character(nomo_apa_or(x$estimator, NA_character_))
  if (!length(estimator) || is.na(estimator[[1L]])) estimator <- nomo_apa_fit_estimator(x$fit)
  sample_note <- nomo_apa_sample_note(estimator, n)

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
    index <- as.character(nomo_apa_or(fe[["index_version"]], NA_character_))[[1L]]
    index <- switch(index, robust = "robust", scaled = "scaled",
                    mixed = "robust or scaled", "")
    return(nomo_apa_fit_table(
      long, estimator = estimator, n = n,
      title = nomo_apa_or(title, "Fit of the Nomological Network Model"),
      model_label = "Nomological network",
      number = number, source = "nomo_network",
      versions = nomo_apa_versions_note(
        if (identical(as.character(fe[["chisq_version"]])[1L], "scaled")) "scaled" else "",
        c(CFI = index, TLI = index, RMSEA = index),
        test = nomo_cfa_test_label(x$fit)
      ),
      sample = nomo_apa_network_sample(x)
    ))
  }

  he <- x$hypothesis_evidence
  spec <- as.data.frame(x$hypotheses$hypotheses, stringsAsFactors = FALSE)
  h <- spec[match(he$id, spec$id), , drop = FALSE]

  # Each estimate in its kind: a standardized correlation cannot exceed 1 and
  # loses its leading zero; a standardized path can, with more than one
  # predictor, and keeps it, as does any unstandardized value. A value just off
  # a bound of its predicted region shows the decimals that tell them apart.
  kind <- nomo_network_kind(he$scale, he$relation_type)
  # The interval each concordance was judged on, as the console shows it: the
  # equivalence interval for a negligible() prediction with a region (#145).
  equivalence <- he$prediction %in% "negligible" & h$magnitude_specified %in% TRUE
  lower <- ifelse(equivalence, he$equivalence_ci_lower, he$ci_lower)
  upper <- ifelse(equivalence, he$equivalence_ci_upper, he$ci_upper)
  shown <- function(v) nomo_network_number(v, kind, nomo_network_nearest_bound(v, h$lower, h$upper))
  estimated <- is.finite(he$estimate)
  interval <- estimated & is.finite(lower) & is.finite(upper)
  estimate <- ifelse(
    !estimated, nomo_apa_dash,
    ifelse(interval,
           sprintf("%s [%s, %s]", shown(he$estimate), shown(lower), shown(upper)),
           shown(he$estimate))
  )

  # The prediction with its region, as the researcher gave it; a direction
  # alone needs no region beside it.
  region <- nomo_network_region_text(h, kind)
  region <- gsub(">=", "\u2265", gsub("<=", "\u2264", region, fixed = TRUE), fixed = TRUE)
  prediction <- nomo_apa_capitalize(as.character(he$prediction))
  prediction <- ifelse(region %in% c("> 0", "< 0", "not specified"), prediction,
                       paste0(prediction, ", ", region))

  equivalence_alpha <- nomo_apa_or(x$equivalence_alpha, 0.05)
  equivalence_ci <- nomo_present_ci_label(1 - 2 * equivalence_alpha)
  equivalence_level <- sub(" CI$", "", equivalence_ci)
  all_equivalence <- length(equivalence) && all(equivalence)
  estimate_heading <- sprintf("Estimate [%s]", if (all_equivalence) equivalence_ci else "95% CI")
  body <- data.frame(
    Hypothesis = paste0(he$id, ": ", he$relation),
    Prediction = prediction,
    Estimate = estimate,
    Evidence = nomo_apa_concordance(he$concordance),
    stringsAsFactors = FALSE
  )
  names(body) <- c("Hypothesis", "Prediction", estimate_heading, "Evidence")

  # Specific notes: post hoc hypotheses, which are exploratory however the
  # estimate turns out; the equivalence intervals among 95% intervals; and the
  # estimates on another scale than the rest (#145). A cell is marked for what
  # it shows, so an empty one carries no marker.
  tost <- sprintf("two one-sided tests at \u03b1 = %s", nomo_present_level(equivalence_alpha))
  scale <- as.character(he$scale)
  scales <- unique(scale)
  marks <- c(
    list(list(
      rows = as.character(he$confirmatory_status) != "a_priori", column = "Hypothesis",
      note = "Specified after the data were seen, so this relation is exploratory."
    )),
    if (!all_equivalence) list(list(
      rows = equivalence & interval, column = estimate_heading,
      note = sprintf(paste(
        "The interval is the %s equivalence interval (%s), on which concordance",
        "with a negligible prediction is judged."
      ), equivalence_level, tost)
    )),
    if (length(scales) > 1L) lapply(setdiff(scales, "standardized"), function(s) list(
      rows = scale == s & estimated, column = estimate_heading,
      note = sprintf("%s estimate.", nomo_apa_capitalize(s))
    ))
  )
  marked <- nomo_apa_mark(body, marks)

  single <- x[["single_indicators"]]
  single_note <- if (is.data.frame(single) && nrow(single)) {
    one <- nrow(single) == 1L
    sprintf(
      paste(
        "%s %s modeled as %s, with %s fixed at",
        "(1 \u2212 reliability) \u00d7 variance (reliability: %s)."
      ),
      nomo_present_or(single$variable, "and"),
      if (one) "was" else "were",
      if (one) "a single-indicator latent variable" else "single-indicator latent variables",
      if (one) "its error variance" else "error variances",
      paste0(
        single$variable, " = ",
        nomo_apa_number(single$reliability, 2L, bounded = TRUE),
        ifelse(single$coefficient == "unspecified", "",
               paste0(", ", single$coefficient)),
        collapse = "; "
      )
    )
  } else {
    ""
  }
  # Each evidence label the table shows, defined once, in the order shown.
  shown_evidence <- unique(as.character(he$concordance))
  shown_evidence <- shown_evidence[shown_evidence %in% names(nomo_apa_evidence_words)]
  evidence_note <- if (length(shown_evidence)) {
    paste0(paste(sprintf("%s = %s", nomo_apa_concordance(shown_evidence),
                         nomo_apa_evidence_words[shown_evidence]), collapse = "; "), ".")
  }
  nomo_apa_new(
    body = marked$body,
    title = nomo_apa_or(title, "Theory-Specified Relations"),
    stub = "Hypothesis",
    general = nomo_apa_join(
      if (all_equivalence) {
        sprintf(paste(
          "CI = confidence interval: the equivalence interval (%s), on which",
          "concordance with a negligible prediction is judged."
        ), tost)
      } else {
        "CI = confidence interval."
      },
      if (length(scales) == 1L) {
        sprintf("Estimates are %s.", scales)
      } else {
        "Estimates are standardized except where marked."
      },
      sample_note,
      nomo_apa_network_sample(x),
      "Evidence describes how each estimate relates to the prediction",
      "registered for it; it is evidence about the prediction, not a verdict on",
      "the measure.",
      evidence_note,
      single_note,
      nomo_apa_dash_note(body, "not estimated")
    ),
    specific = marked$specific,
    number = number, source = "nomo_network"
  )
}


#' @export
nomo_apa_table.nomo_retest <- function(x, type = NULL, number = NULL,
                                       title = NULL, ...) {
  nomo_apa_one_table(type, "nomo_retest")
  icc <- x$icc
  # Word entries in the stub take sentence case; a composite the researcher
  # named keeps its name.
  composite <- as.character(icc$composite)
  composite[composite == "composite"] <- "Composite"
  body <- data.frame(
    Composite = composite,
    n = as.character(icc$n),
    agreement = nomo_apa_interval(icc$icc_agreement, icc$agreement_ci_lower,
                                  icc$agreement_ci_upper, bounded = TRUE),
    consistency = nomo_apa_interval(icc$icc_consistency, icc$consistency_ci_lower,
                                    icc$consistency_ci_upper, bounded = TRUE),
    change = nomo_apa_interval(icc$mean_change, icc$change_ci_lower,
                               icc$change_ci_upper, bounded = FALSE),
    SEM = nomo_apa_number(icc$sem, 2L, bounded = FALSE),
    SDC = nomo_apa_number(icc$sdc, 2L, bounded = FALSE),
    stringsAsFactors = FALSE
  )
  # SEM is italic, the statistical symbol for the standard error of
  # measurement, not roman SEM for structural equation modeling (#145).
  names(body) <- c(
    "Composite", "*n*", "ICC(A,1) [95% CI]", "ICC(C,1) [95% CI]",
    "Mean change [95% CI]", "*SEM*", "SDC"
  )
  nomo_apa_new(
    body = body,
    title = nomo_apa_or(title, "Test-Retest Reliability"),
    stub = "Composite",
    general = nomo_apa_join(
      "ICC(A,1) = intraclass correlation from a two-way mixed-effects model,",
      "absolute agreement, single measurement; ICC(C,1) = the same with",
      "consistency (Koo & Li, 2016; McGraw & Wong, 1996); CI = confidence",
      "interval; *SEM* = standard error of measurement; SDC = smallest",
      "detectable change, 1.96 \u00d7 \u221a2 \u00d7 *SEM* (Weir, 2005).",
      "Mean change is from the first to the last occasion.",
      if (is.na(x$interval)) "" else sprintf("Interval between occasions: %s.", x$interval),
      nomo_apa_dash_note(body, "not computed")
    ),
    number = number, source = "nomo_retest"
  )
}


# Convergent and discriminant evidence. The discriminant table follows current
# practice rather than the Fornell-Larcker matrix with AVE on its diagonal: each
# pair's latent correlation with its interval (Ronkko & Cho, 2022) beside the
# heterotrait-monotrait ratios (Henseler et al., 2015; Roemer et al., 2021).
# AVE is convergent evidence and has a table of its own.
#' @export
nomo_apa_table.nomo_validity <- function(x, type = c("discriminant", "convergent"),
                                         number = NULL, title = NULL, ...) {
  type <- nomo_match_arg(type)
  groups <- function(block) length(unique(as.character(block))) > 1L

  if (type == "convergent") {
    tab <- nomo_validity_convergent_table(x)
    if (!nrow(tab) || !any(is.finite(tab$AVE))) {
      stop("No average variance extracted is available for this model.", call. = FALSE)
    }
    # A multi-group model lists each loading once per group, so indicators are
    # counted by name.
    loadings <- x$standardized_loadings
    k <- vapply(as.character(tab$construct), function(construct) {
      length(unique(loadings$item[loadings$factor == construct]))
    }, integer(1), USE.NAMES = FALSE)
    body <- data.frame(
      Construct = as.character(tab$construct),
      Group = as.character(tab$block),
      k = as.character(k),
      AVE = nomo_apa_number(tab$AVE, 2L, bounded = TRUE),
      stringsAsFactors = FALSE
    )
    if (!groups(tab$block)) body$Group <- NULL
    names(body)[names(body) == "k"] <- "*k*"
    return(nomo_apa_new(
      body = body,
      title = nomo_apa_or(title, "Average Variance Extracted"),
      stub = "Construct",
      general = nomo_apa_join(
        "*k* = number of indicators; AVE = average variance extracted (Fornell &",
        "Larcker, 1981), the average proportion of indicator variance the",
        sprintf("construct explains. The review reference is %s;",
                nomo_apa_number(x$ave_reference, 2L, bounded = TRUE)),
        "a value below it prompts a look at the loadings and content coverage.",
        "AVE is convergent evidence and is not a reliability coefficient.",
        nomo_apa_dash_note(body, "not computed")
      ),
      number = number, source = "nomo_validity"
    ))
  }

  tab <- nomo_validity_discriminant_table(x)
  if (!nrow(tab)) {
    stop("The model has one construct, so there are no construct pairs.", call. = FALSE)
  }
  body <- data.frame(
    Constructs = paste(tab$construct_1, "with", tab$construct_2),
    Group = as.character(tab$block),
    r = nomo_apa_interval(tab$latent_r, tab$latent_r_ci_lower, tab$latent_r_ci_upper,
                          bounded = TRUE),
    # A heterotrait-monotrait ratio can exceed 1, so it keeps its leading zero.
    HTMT2 = nomo_apa_number(tab$HTMT2, 2L, bounded = FALSE),
    HTMT = nomo_apa_number(tab$HTMT, 2L, bounded = FALSE),
    stringsAsFactors = FALSE
  )
  if (!groups(tab$block)) body$Group <- NULL
  ratios <- c(HTMT2 = any(is.finite(tab$HTMT2)), HTMT = any(is.finite(tab$HTMT)))
  body <- body[, setdiff(names(body), names(ratios)[!ratios]), drop = FALSE]
  names(body)[names(body) == "r"] <- "*r* [95% CI]"

  defined <- c(
    HTMT2 = paste(
      "HTMT2 = heterotrait-monotrait ratio based on geometric means, suited to",
      "indicators with unequal loadings (Roemer et al., 2021)."
    ),
    HTMT = "HTMT = heterotrait-monotrait ratio (Henseler et al., 2015)."
  )
  nomo_apa_new(
    body = body,
    title = nomo_apa_or(title, "Construct Correlations and Heterotrait-Monotrait Ratios"),
    stub = "Constructs",
    general = c(
      "*r* = latent correlation from the confirmatory factor analysis; CI =",
      "confidence interval, whose upper limit shows how high the correlation",
      "plausibly is (R\u00f6nkk\u00f6 & Cho, 2022).",
      defined[ratios],
      if (any(ratios)) {
        sprintf(paste(
          "The review reference for the ratios is %s. A ratio below it adds",
          "evidence that the constructs are empirically distinct; it does not",
          "by itself establish discriminant validity."
        ), nomo_apa_number(x$htmt_reference, 2L, bounded = FALSE))
      } else {
        "Heterotrait-monotrait ratios were not computed for this model."
      },
      nomo_apa_dash_note(body, "not computed")
    ),
    number = number, source = "nomo_validity"
  )
}


# Evidence labels in plain words: the ones the console shows, read from the
# network's own display labels so the two never differ (#145). They describe
# the relation between an estimate and its prediction, and none is a pass or a
# fail.
nomo_apa_concordance <- function(x) {
  x <- as.character(x)
  out <- unname(nomo_network_status_labels[x])
  unknown <- is.na(out)
  out[unknown] <- nomo_apa_capitalize(gsub("_", " ", x[unknown]))
  out[is.na(x)] <- nomo_apa_dash
  out
}


# What each evidence label means, for the note of a table that shows it, in
# the words of the Concordance section of ?nomo_network. "The interval" is the
# one the table shows for the estimate.
nomo_apa_evidence_words <- c(
  concordant = "the interval lies inside the predicted region",
  directionally_concordant_imprecise = paste(
    "the estimate lies inside the predicted region and its interval extends",
    "outside it"
  ),
  direction_concordant_below_magnitude = paste(
    "the estimate has the predicted sign and is smaller than the predicted",
    "magnitude, which its interval reaches"
  ),
  direction_concordant_above_magnitude = paste(
    "the estimate has the predicted sign and is larger than the predicted",
    "magnitude, which its interval reaches"
  ),
  inconclusive = "the estimate lies outside the predicted region, which its interval reaches",
  inconsistent = "the estimate and its interval lie outside the predicted region",
  not_evaluable = paste(
    "no estimate with a standard error is available, so the relation is not",
    "compared with its prediction"
  ),
  not_confirmable_without_sesoi = paste(
    "a negligible prediction without a smallest effect size of interest,",
    "which a nonsignificant estimate cannot confirm"
  )
)


# Printing ---------------------------------------------------------------------

# The general, specific, and probability notes, one paragraph each. In a
# manuscript the specific notes run on in one paragraph (APA 7, Section 7.14);
# the console gives each its own line (`run_in = FALSE`), so no marker is left
# at the end of a line, away from its note.
nomo_apa_notes_text <- function(notes, run_in = TRUE) {
  parts <- character()
  general <- notes$general[nzchar(notes$general)]
  if (length(general)) {
    parts <- c(parts, paste("*Note.*", paste(general, collapse = " ")))
  }
  specific <- notes$specific[nzchar(notes$specific)]
  if (length(specific)) {
    specific <- paste0("^", letters[seq_along(specific)], "^ ", specific)
    parts <- c(parts, if (isTRUE(run_in)) paste(specific, collapse = " ") else specific)
  }
  probability <- notes$probability[nzchar(notes$probability)]
  if (length(probability)) {
    parts <- c(parts, paste(probability, collapse = " "))
  }
  gsub("  +", " ", parts)
}


# Markdown as the console shows it: italics dropped, and a superscript note
# marker written "(a)" in a cell and before its note alike (#145).
nomo_apa_console_text <- function(s) {
  s <- gsub("[[:space:]]*\\^([^^]+)\\^", " (\\1)", s)
  trimws(gsub("*", "", s, fixed = TRUE))
}

# A note marker as the console writes it: "(a)", or "(a,b)" for two notes.
nomo_apa_marker <- "\\([a-z](,[a-z])*\\)"


# A cell's statistical clauses bound with no-break spaces, as the console binds
# them; a bound kept with its relation, "\u2265 0.20" or "> 0", though not the
# arrow of a relation such as "Agency -> Persistence"; a note marker with the
# word it follows, "scalar (a)"; and every "x = y" of a note, Greek symbols
# included ("\u03b1 = .05").
nomo_apa_bind <- function(text) {
  nb <- nomo_present_nbsp
  text <- gsub("(^|[[:space:]])([\u2264\u2265<>]=?) ", paste0("\\1\\2", nb), text)
  text <- gsub(paste0(" (", nomo_apa_marker, ")$"), paste0(nb, "\\1"), text)
  nomo_present_bind(gsub(" = ", paste0(nb, "=", nb), text, fixed = TRUE))
}


# A cell's text, or a heading, in lines of at most `width` characters, with
# every clause kept whole; a cell's continued lines are indented 2.
nomo_apa_wrap_cell <- function(text, width, exdent = 2L) {
  if (nchar(text, type = "width") <= width) return(text)
  nomo_present_wrap(nomo_apa_bind(text), width = width + 1L, exdent = exdent)
}


# The narrowest a text can be wrapped: its longest unbroken word, plus the
# indent of a continued line.
nomo_apa_cell_floor <- function(text, exdent = 2L) {
  words <- strsplit(nomo_apa_bind(text), " ", fixed = TRUE)[[1L]]
  if (length(words) < 2L) return(nchar(text, type = "width"))
  max(nchar(words, type = "width")) + exdent
}


# A table laid out for a console `room` columns wide. A table too wide first
# wraps its long headings over two or more lines, then wraps its text cells
# (the stub, and words such as an evidence label), widest first, then
# tightens its column gaps to one space. Only then are columns dropped, from
# the right, never the stub or the Evidence status (guide point 5), and named
# beneath the table (#145). A p value is dropped like any other column: in a
# manuscript table it belongs to the test beside it, and would be misread
# without it.
nomo_apa_console_layout <- function(headings, cells, room) {
  marker <- paste0(" ", nomo_apa_marker, "$")
  stripped <- lapply(cells, function(v) sub(marker, "", v))
  text <- vapply(stripped, function(v) any(grepl("[A-Za-z]", v)), logical(1))
  text[[1L]] <- TRUE
  # A note marker hangs to the right of a column of numbers, so the numbers
  # stay aligned; in a text cell it follows the word it marks.
  marks <- lapply(seq_along(cells), function(j) {
    if (text[[j]]) rep("", length(cells[[j]])) else substring(cells[[j]], nchar(stripped[[j]]) + 1L)
  })
  cells[!text] <- stripped[!text]
  hang <- vapply(marks, function(m) max(nchar(m, type = "width"), 0L), integer(1))
  keep <- "evidence"

  layout <- function(cols) {
    heads <- lapply(headings[cols], identity)
    body <- lapply(cells[cols], as.list)
    width <- function(j) {
      max(nchar(c(heads[[j]], unlist(body[[j]])), type = "width"), 1L) + hang[[cols[[j]]]]
    }
    widths <- function() vapply(seq_along(cols), width, integer(1))
    total <- function(gap) sum(widths()) + gap * (length(cols) - 1L)
    if (total(2L) > room) {
      for (j in seq_along(cols)) {
        cell_width <- max(nchar(unlist(body[[j]]), type = "width"), 0L)
        floor <- nomo_apa_cell_floor(heads[[j]], exdent = 0L)
        heads[[j]] <- nomo_apa_wrap_cell(heads[[j]], max(cell_width, floor), exdent = 0L)
      }
    }
    floors <- vapply(seq_along(cols), function(j) {
      original <- cells[[cols[[j]]]]
      max(vapply(original, nomo_apa_cell_floor, numeric(1)),
          nchar(heads[[j]], type = "width"))
    }, numeric(1))
    shrink <- text[cols]
    while (total(2L) > room && any(shrink)) {
      now <- widths()
      j <- which(shrink)[which.max(now[shrink])]
      target <- max(floors[[j]], now[[j]] - (total(2L) - room))
      body[[j]] <- lapply(cells[[cols[[j]]]], nomo_apa_wrap_cell, width = target)
      if (width(j) >= now[[j]]) shrink[[j]] <- FALSE
    }
    gap <- if (total(2L) <= room) 2L else 1L
    list(cols = cols, heads = heads, body = body, widths = widths(), gap = gap,
         fits = total(gap) <= room)
  }

  cols <- seq_along(cells)
  hidden <- character()
  laid <- layout(cols)
  droppable <- function(cols) {
    cols[cols != 1L & !(tolower(headings[cols]) %in% keep)]
  }
  while (!laid$fits && length(droppable(cols))) {
    drop <- max(droppable(cols))
    hidden <- c(headings[[drop]], hidden)
    cols <- setdiff(cols, drop)
    laid <- layout(cols)
  }

  align <- function(s, w, left) {
    space <- strrep(" ", pmax(0L, w - nchar(s, type = "width")))
    if (left) paste0(s, space) else paste0(space, s)
  }
  left <- text[laid$cols]
  hang <- hang[laid$cols]
  marks <- marks[laid$cols]
  k <- length(laid$cols)
  # One column's piece of a line: the text in its column, then its note marker
  # in the room the column keeps for one.
  piece <- function(j, s, mark = "") {
    paste0(align(s, laid$widths[[j]] - hang[[j]], left[[j]]), align(mark, hang[[j]], TRUE))
  }
  emit <- function(parts) sub("[[:space:]]+$", "", paste(parts, collapse = strrep(" ", laid$gap)))
  # Headings sit on the bottom line of the heading rows, as in print.
  depth <- max(lengths(laid$heads))
  heads <- lapply(laid$heads, function(h) c(rep("", depth - length(h)), h))
  lines <- vapply(seq_len(depth), function(l) {
    emit(vapply(seq_len(k), function(j) piece(j, heads[[j]][[l]]), character(1)))
  }, character(1))
  rows <- unlist(lapply(seq_along(cells[[1L]]), function(r) {
    row <- lapply(laid$body, `[[`, r)
    height <- max(lengths(row))
    vapply(seq_len(height), function(l) {
      emit(vapply(seq_len(k), function(j) {
        s <- if (l <= length(row[[j]])) row[[j]][[l]] else ""
        piece(j, s, if (l == 1L) marks[[j]][[r]] else "")
      }, character(1)))
    }, character(1))
  }))
  rule <- strrep("-", sum(laid$widths) + laid$gap * (k - 1L))
  list(lines = c(rule, lines, rule, rows, rule), hidden = hidden)
}


#' @export
print.nomo_apa_table <- function(x, ...) {
  nomo_present_header("nomo_apa_table", "Manuscript table in APA style")
  cat("\n")
  if (!is.null(x$number)) cat(sprintf("Table %d\n", x$number))
  nomo_present_text(x$title)

  headings <- nomo_apa_console_text(names(x$body))
  cells <- lapply(x$body, function(v) nomo_apa_console_text(as.character(v)))
  laid <- nomo_apa_console_layout(headings, cells, nomo_present_width())
  nomo_present_cat(laid$lines)
  if (length(laid$hidden)) {
    nb <- nomo_present_nbsp
    nomo_present_text("Not shown for width: ",
                      paste(gsub(" ", nb, laid$hidden, fixed = TRUE), collapse = ", "),
                      ". See", nb, "x$body.")
  }

  # Notes wrap to the console, as every other printed text does (#89), each
  # specific note on its own line.
  for (note in nomo_apa_console_text(nomo_apa_notes_text(x$notes, run_in = FALSE))) {
    nomo_present_text(nomo_apa_bind(note))
  }
  nomo_present_pointer(c("x$body", "knitr::knit_print(x)"),
                       c("the cells", "the Markdown a knitted document renders"))
  invisible(x)
}


# Symbols as TeX math, for a PDF knitted with pdflatex, which stops at a Greek
# letter or a math symbol in the text (#145). Longest first, so the change in
# chi-square is not read as a change and a chi-square. The em dash pandoc
# writes as "---" itself.
nomo_apa_tex_map <- c(
  "\u0394\u03c7\u00b2" = "$\\Delta\\chi^2$",
  "\u03c7\u00b2" = "$\\chi^2$",
  "\u221a2" = "$\\sqrt{2}$",
  "\u0394" = "$\\Delta$",
  "\u03c9" = "$\\omega$",
  "\u03b1" = "$\\alpha$",
  "\u00d7" = "$\\times$",
  "\u2212" = "$-$",
  "\u2265" = "$\\geq$",
  "\u2264" = "$\\leq$",
  "\u00b2" = "$^2$"
)

nomo_apa_tex <- function(s) {
  for (symbol in names(nomo_apa_tex_map)) {
    s <- gsub(symbol, nomo_apa_tex_map[[symbol]], s, fixed = TRUE)
  }
  s
}


# Whether knitr is rendering to LaTeX, as for a PDF.
nomo_apa_latex <- function() {
  isTRUE(requireNamespace("knitr", quietly = TRUE) && knitr::is_latex_output())
}


# Knitted, the table becomes markdown that pandoc renders as HTML, Word, or
# PDF: a bold number, an italic title, a table with the stub column
# left-aligned and the rest centered, and the notes below, each kind its own
# paragraph. Each column's dashes follow its widest entry as it is shown, a
# superscript counted as one character, with two more for the cell's padding,
# so when a table is too wide for pandoc to keep its natural widths, the
# shares it gives the columns follow their content and the stub is not
# squeezed (#145).
nomo_apa_markdown <- function(x, latex = nomo_apa_latex()) {
  convert <- if (isTRUE(latex)) nomo_apa_tex else identity
  body <- x$body
  shown <- function(s) {
    nchar(gsub("\\^[^^]+\\^", "a", gsub("*", "", s, fixed = TRUE)), type = "width")
  }
  dashes <- vapply(seq_along(body), function(j) {
    max(3L, shown(c(names(body)[[j]], body[[j]]))) + 2L
  }, numeric(1))
  heads <- convert(names(body))
  cells <- lapply(body, function(v) convert(as.character(v)))
  align <- paste0(":", strrep("-", dashes), c("", rep(":", length(dashes) - 1L)))
  row <- function(v) paste0("| ", paste(v, collapse = " | "), " |")
  notes <- convert(nomo_apa_notes_text(x$notes))
  c(
    if (!is.null(x$number)) c(sprintf("**Table %d**", x$number), ""),
    sprintf("*%s*", convert(x$title)),
    "",
    row(heads),
    row(align),
    vapply(seq_len(nrow(body)), function(i) {
      row(vapply(cells, `[[`, character(1), i))
    }, character(1)),
    as.vector(rbind("", notes))
  )
}


#' @exportS3Method knitr::knit_print
knit_print.nomo_apa_table <- function(x, ...) {
  knitr::asis_output(paste(nomo_apa_markdown(x), collapse = "\n"))
}
