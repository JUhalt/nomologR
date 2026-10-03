# Console presentation helpers (#89, #144) -------------------------------------
#
# Every print() and summary() method writes through these helpers so that the
# console output follows one set of rules, the output style nomologR shares
# with contentvalidR (#144):
#
# * a header names the object's class and what it is, on one line;
# * prose is wrapped to min(console width, 80) - 1, and a statistical clause
#   ("p < .001", "N = 473", "[.12, .34]", "F(2, 14)") is never broken across
#   lines; tables may use the full console width;
# * R code is ASCII; output is ASCII except proper names, which the code writes
#   with \u escapes, such as Muth\u00e9n;
# * tables are aligned text with curated columns and no type row; a column that
#   is empty everywhere is dropped, and a table too wide for the console first
#   tightens its column gaps, then drops trailing columns (never the stub or a
#   status column) and names them with the call that shows them;
# * numbers have one precision and one leading-zero rule per quantity, are
#   rounded half away from zero for display only, never print a negative zero,
#   and p values follow APA style; a missing value is shown as "--";
# * a flagged unit's explanation is printed in full as a wrapped bullet, never
#   cut off inside a table cell;
# * status words are shown in one vocabulary: blank (no flag), "review",
#   "concern", "not computed"; sentence case in cells and at line starts,
#   lowercase in running prose.
#
# The helpers change only what is printed. Returned objects, nomo_table() types,
# and decision-log values are untouched, as the stability policy requires.
#
# Helper API (for every print() and summary() method) ---------------------------
#
# Layout
#   nomo_present_header(class, title, summary = FALSE, source = NULL)
#       "<class> Title" or "<class summary> Title", wrapped only when the
#       console is narrower than the line. `source` adds a second line directly
#       beneath, a narrative citation as a sentence ("Anderson and Gerbing
#       (1991)."); "&" is written "and".
#   nomo_present_section(title)       a blank line, then the title (sentence
#                                     case, no colon); content is indented 2.
#   nomo_present_text(..., indent = 0L, exdent = indent)   wrapped prose.
#   nomo_present_facts(parts)         "Label: value | Label: value", broken only
#                                     between parts; a label keeps its value.
#   nomo_present_bullets(items, indent = 2L)   "  - item", hanging indent.
#   nomo_present_notes(notes)         bullets from a data frame with `severity`
#                                     and `note`: "  - Review: Note."
#   nomo_present_flagged(log, recommendation = FALSE, unit, status, text)
#       the "Flagged" section: one bullet per unit, "  - b5 (Review): Sentence.",
#       concern before review, units sharing a status and an explanation
#       grouped ("  - B2, C2 (Review): ..."). Reads a decision log (`object`,
#       `severity`, `observation`, and `recommendation` when asked), or the
#       `unit`, `status`, and `text` vectors given.
#   nomo_present_table(x, columns, formats = list(), more = NULL, indent = 2L,
#                      keep = nomo_present_keep)
#       an aligned table; `columns` maps display labels to columns of `x`,
#       `formats` maps a column to a function returning text. Columns are
#       indented 2 and separated by 2 spaces, tightened to 1 before any column
#       is dropped for width. Columns whose label is in `keep` (the status and
#       decisive p columns) and the first (stub) column are never dropped; the
#       others go from the right, and the line "Not shown for width: X, Y. See
#       <more>." names them, where `more` is the call that shows them, such as
#       nomo_table(x, "fit"). Missing cells are "--".
#   nomo_present_key(entries, title = "What these columns mean")
#       a key section; `entries` is a named vector, c(SE = "Standard error."),
#       printed "  SE -- Standard error." with a 6-space hanging indent.
#   nomo_present_pointer(call, what, blank = TRUE)
#       the closing pointer line: "See summary(x) for the flagged items and
#       nomo_table(x, "fit") for every index." Calls are never broken.
#   nomo_present_wrap(text, width = nomo_present_prose_width(), ...)
#       strwrap() with every statistical clause kept whole (nomo_present_bind()),
#       for a layout the helpers above do not cover; `...` goes to strwrap().
#
# Numbers (display only; stored values are untouched)
#   nomo_round_half_up(x, digits)     the rounding agreed with contentvalidR:
#                                     half away from zero, so .625 is .63.
#   nomo_present_round(x, digits)     the same, then the negative-zero guard.
#   nomo_present_stat(x, kind, digits = NULL, reference = NULL, signed = FALSE)
#       THE entry point for a statistic: the kind's precision and leading-zero
#       rule (table below). `digits` overrides the precision. `reference` gives
#       a value just off a printed reference (a teaching reference or
#       criterion) the extra decimals it needs to differ from it (guide point
#       9): a communality of .398 beside a reference of .40 prints .398.
#       `signed = TRUE` prints a change with its sign, unsigned when it rounds
#       to zero.
#   nomo_present_number(x, digits = 3L, bounded = FALSE)   fixed decimals;
#                                     `bounded = TRUE` drops the leading zero.
#   nomo_present_signed(x, digits = 3L, bounded = FALSE)   "+0.022", "-0.400",
#                                     and "0.000" (never "+0.000" or "-0.000").
#   nomo_present_p(p)                 ".043", "< .001", "> .999".
#   nomo_present_p_clause(p)          "p = .043", "p < .001", "p > .999"; "" when
#                                     missing.
#   nomo_present_ci(lower, upper, digits = 3L, bounded = FALSE, kind = NULL)
#                                     "[LL, UL]"; "--" when either bound is
#                                     missing, never half an interval.
#   nomo_present_ci_label(level = 0.95)   "95% CI", the level from the object.
#   nomo_present_chisq(stat, df, p, n = NULL, delta = FALSE)
#       "chi-square(34) = 75.83, p < .001"; with `n` (a contingency table)
#       "chi-square(1, N = 40) = 10.00, p = .002"; with `delta = TRUE`
#       "Delta chi-square(6) = 12.34, p = .015".
#   nomo_present_percent(x, base = NULL)   a proportion as a percentage: whole
#                                     numbers when every base is under 100, one
#                                     decimal otherwise or when no base is given.
#
# Words
#   nomo_present_status(x)            stored status words for a cell or a line
#                                     start: KEEP, none, info, ok -> "" (no
#                                     flag); REVIEW, review -> "Review"; STRONG
#                                     REVIEW, concern -> "Concern"; unavailable
#                                     -> "Not computed". Stored values never
#                                     change.
#   nomo_present_flag(x)              the same in lowercase, for running prose.
#   nomo_present_flag_counts(x)       "1 review, 2 concern" or "none".
#   nomo_present_count(n, singular, plural), nomo_present_noun(...)   "2 factors".
#   nomo_present_or(x, word = "or")   "a, b, or c".
#   nomo_present_origin(x, cell = FALSE)   "a priori", "post hoc" (no hyphen);
#                                     sentence case in a cell.
#   nomo_present_ordinal(n)           "1st", "2nd", "93rd", "11th".
#
# Widths and markers
#   nomo_present_width()              the console width (at least 40), for tables.
#   nomo_present_prose_width()        min(nomo_present_width(), 80) - 1, the
#                                     width prose is wrapped to.
#   nomo_present_missing              "--", the missing marker. APA tables keep
#                                     their em dash (nomo_apa_dash).
#
# Plots use the status shapes and colors in R/nomo_plot_helpers.R.
#
# Kinds for nomo_present_stat() -------------------------------------------------
#
# One precision and one leading-zero rule per quantity (guide points 8 and 9;
# APA 7, Section 6.36). A statistic that cannot exceed 1 in absolute value is
# "bounded" and printed without a leading zero; one that can keeps it.
#
#   kind         digits     leading zero  quantities
#   p            3          no            p values: "< .001", ".043", "> .999"
#   fit_bounded  3          no            CFI
#   fit          3          kept          TLI (can exceed 1), RMSEA, SRMR
#   r            2          no            correlations, kappa
#   reliability  2          no            omega, alpha, ICC, AVE, determinacy, H
#   loading      2          kept          standardized loadings (a Heywood value
#                                         exceeds 1)
#   htmt         2          kept          HTMT, HTMT2 (can exceed 1)
#   proportion   2          no            proportions and shares
#   power        2          no            power and coverage
#   level        as given,  no            a significance level: .05, .10, .025
#                at least 2
#   stat         2          kept          chi-square, t, F, z, and differences
#   estimate     2          kept          unstandardized estimates, means, SDs,
#                                         SEs
#   ic           2          kept          AIC, BIC
#   df           0 or 2     kept          degrees of freedom: "34", or "33.47"
#                                         when fractional
#   count        0          kept          counts: N, n, k
#
# A change (signed = TRUE) follows its quantity's rule: a CFI change prints
# "+.002", an RMSEA change "-0.004".

nomo_present_missing <- "--"

# A no-break space binds the parts of a statistical clause before strwrap(),
# which does not break at it, and is turned back into a space afterwards.
nomo_present_nbsp <- "\u00a0"

# Status and decisive-test columns, by display label, that a table never drops
# for width (guide point 5). Matched in any case.
nomo_present_keep <- c("Flag", "Status", "Concordance", "Replication", "Meets",
                       "p", "LRT p", "Method p")

nomo_present_kinds <- list(
  p = list(digits = 3L, bounded = TRUE),
  fit_bounded = list(digits = 3L, bounded = TRUE),
  fit = list(digits = 3L, bounded = FALSE),
  r = list(digits = 2L, bounded = TRUE),
  reliability = list(digits = 2L, bounded = TRUE),
  loading = list(digits = 2L, bounded = FALSE),
  htmt = list(digits = 2L, bounded = FALSE),
  proportion = list(digits = 2L, bounded = TRUE),
  power = list(digits = 2L, bounded = TRUE),
  level = list(digits = 2L, bounded = TRUE),
  stat = list(digits = 2L, bounded = FALSE),
  estimate = list(digits = 2L, bounded = FALSE),
  ic = list(digits = 2L, bounded = FALSE),
  df = list(digits = 0L, bounded = FALSE),
  count = list(digits = 0L, bounded = FALSE)
)


# Widths ------------------------------------------------------------------------

# options() only accepts a width from 10 to 10000, so it is always usable.
nomo_present_width <- function() {
  max(40L, as.integer(getOption("width", 80L)))
}

# Prose is capped at 80 columns, less one, however wide the console (guide
# point 24). strwrap() keeps each line shorter than the width it is given.
nomo_present_prose_width <- function() {
  min(nomo_present_width(), 80L) - 1L
}


# Line binding ------------------------------------------------------------------

# Joins the parts of a clause that must stay on one line with no-break spaces:
# a name, its relation, and the next word ("p < .001", "N = 473", "alpha =
# .05", "CI = confidence"); a closing parenthesis and its value ("F(2, 14) =
# 3.21", "chi-square(34) = 75.83"); an interval ("[.12, .34]") and its label
# ("95% CI [.12, .34]"); and the degrees of freedom of a test ("F(2, 14)",
# "chi-square(1, N = 40)"). As contentvalidR does.
nomo_present_bind <- function(x) {
  nb <- nomo_present_nbsp
  rel <- "([<>]=?|!?=)"
  # A name that itself follows a relation is left free, so a chain such as
  # "a > b > c" binds only its first clause and can still be wrapped.
  x <- gsub(paste0("(?<![<>=] )\\b([A-Za-z][A-Za-z0-9-]*) ", rel, " "),
            paste0("\\1", nb, "\\2", nb), x, perl = TRUE)
  x <- gsub(paste0("\\) ", rel, " "), paste0(")", nb, "\\1", nb), x, perl = TRUE)
  x <- gsub("\\[([^]\\[]*), ([^]\\[]*)\\]", paste0("[\\1,", nb, "\\2]"), x,
            perl = TRUE)
  x <- gsub("\\(([0-9.]+), ", paste0("(\\1,", nb), x, perl = TRUE)
  x <- gsub("([0-9.]+%) CI\\b", paste0("\\1", nb, "CI"), x, perl = TRUE)
  gsub("\\bCI \\[", paste0("CI", nb, "["), x, perl = TRUE)
}

# Wrapped lines with every clause kept whole; the no-break spaces are spaces
# again in what is printed.
nomo_present_wrap <- function(text, width = nomo_present_prose_width(), ...) {
  lines <- strwrap(nomo_present_bind(text), width = width, ...)
  gsub(nomo_present_nbsp, " ", lines, fixed = TRUE)
}

nomo_present_cat <- function(lines) {
  cat(paste0(lines, "\n"), sep = "")
}


# Layout ------------------------------------------------------------------------

nomo_present_header <- function(class, title, summary = FALSE, source = NULL) {
  tag <- sprintf("<%s%s>", class, if (isTRUE(summary)) " summary" else "")
  tag <- gsub(" ", nomo_present_nbsp, tag, fixed = TRUE)
  nomo_present_cat(nomo_present_wrap(paste(tag, title)))
  if (length(source) && !is.na(source[[1L]]) && nzchar(source[[1L]])) {
    source <- gsub(" & ", " and ", source[[1L]], fixed = TRUE)
    if (!grepl("[.!?]$", source)) source <- paste0(source, ".")
    nomo_present_cat(nomo_present_wrap(source))
  }
  invisible(NULL)
}


nomo_present_section <- function(title) {
  cat("\n")
  nomo_present_cat(nomo_present_wrap(title))
}


# One or more paragraphs, each wrapped to the prose width.
nomo_present_text <- function(..., indent = 0L, exdent = indent) {
  text <- paste0(...)
  nomo_present_cat(nomo_present_wrap(text, indent = indent, exdent = exdent))
}


# A line of "label: value" parts joined by " | ", broken between parts when it
# would run past the prose width. A label keeps its value on its line.
nomo_present_facts <- function(parts) {
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (!length(parts)) return(invisible(NULL))
  width <- nomo_present_prose_width()
  parts <- sub(": ", paste0(":", nomo_present_nbsp), parts, fixed = TRUE)
  # A single part longer than the line, such as a long stage sequence, is
  # wrapped with a hanging indent.
  emit <- function(line) nomo_present_cat(nomo_present_wrap(line, width, exdent = 2L))
  line <- parts[[1L]]
  for (part in parts[-1L]) {
    candidate <- paste(line, part, sep = " | ")
    # strwrap() breaks a line that is not shorter than its width.
    if (nchar(candidate, type = "width") >= width) {
      emit(line)
      line <- part
    } else {
      line <- candidate
    }
  }
  emit(line)
}


# Bullets with a hanging indent, so a long explanation stays readable.
nomo_present_bullets <- function(items, indent = 2L) {
  items <- items[!is.na(items) & nzchar(items)]
  pad <- strrep(" ", indent)
  for (item in items) {
    nomo_present_cat(nomo_present_wrap(item, initial = paste0(pad, "- "),
                                       prefix = paste0(pad, "  ")))
  }
}


# Notes with a severity: a review or concern note is prefixed with its status,
# and an informational note is printed as it is.
nomo_present_notes <- function(notes) {
  if (!is.data.frame(notes) || !nrow(notes)) return(invisible(NULL))
  status <- nomo_present_status(notes$severity)
  nomo_present_bullets(ifelse(nzchar(status), paste0(status, ": ", notes$note), notes$note))
}


# The "Flagged" section (guide point 22): one bullet per unit, "- b5 (Review):
# A complete sentence.", concern before review, and units that share a status
# and an explanation grouped on one bullet ("- B2, C2 (Review): ..."). From a
# decision log, the unit is its `object` and the explanation its observation,
# followed by its recommendation when `recommendation = TRUE`, as a summary
# gives it. Nothing is printed when nothing is flagged.
nomo_present_flagged <- function(log = NULL, recommendation = FALSE,
                                 unit = log[["object"]], status = log[["severity"]],
                                 text = NULL) {
  if (is.null(text) && !is.null(log)) {
    text <- log[["observation"]]
    if (isTRUE(recommendation)) text <- paste(text, log[["recommendation"]])
  }
  shown <- nomo_present_status(status)
  n <- length(shown)
  unit <- if (is.null(unit)) rep("", n) else as.character(unit)
  text <- if (is.null(text)) rep("", n) else trimws(as.character(text))
  unit[is.na(unit)] <- ""
  text[is.na(text)] <- ""
  flagged <- shown %in% c("Concern", "Review")
  if (!any(flagged)) return(invisible(NULL))
  shown <- shown[flagged]
  unit <- unit[flagged]
  text <- text[flagged]
  text <- ifelse(nzchar(text) & !grepl("[.!?]$", text), paste0(text, "."), text)

  key <- paste(shown, text, sep = "\r")
  keys <- unique(key[order(match(shown, c("Concern", "Review")))])
  bullets <- vapply(keys, function(k) {
    same <- key == k
    units <- unique(unit[same & nzchar(unit)])
    label <- if (length(units)) {
      paste0(paste(units, collapse = ", "), nomo_present_nbsp, "(", shown[same][[1L]], ")")
    } else {
      shown[same][[1L]]
    }
    if (nzchar(text[same][[1L]])) paste0(label, ": ", text[same][[1L]]) else label
  }, character(1), USE.NAMES = FALSE)
  nomo_present_section("Flagged")
  nomo_present_bullets(bullets)
}


# A key to the abbreviations a table shows (guide point 23): a section, then one
# entry per abbreviation, "  SE -- Standard error.", with a 6-space hanging
# indent.
nomo_present_key <- function(entries, title = "What these columns mean") {
  entries <- entries[!is.na(entries) & nzchar(entries)]
  if (!length(entries)) return(invisible(NULL))
  nomo_present_section(title)
  text <- trimws(entries)
  text <- ifelse(grepl("[.!?]$", text), text, paste0(text, "."))
  nb <- nomo_present_nbsp
  for (i in seq_along(text)) {
    nomo_present_cat(nomo_present_wrap(
      paste0(names(entries)[[i]], nb, "--", nb, text[[i]]), indent = 2L, exdent = 6L
    ))
  }
}


# The closing pointer (guide point 6): "See <call using x> for <what>.", one
# sentence for any number of calls, after a blank line unless `blank = FALSE`.
nomo_present_pointer <- function(call, what, blank = TRUE) {
  if (!length(call)) return(invisible(NULL))
  call <- gsub(" ", nomo_present_nbsp, call, fixed = TRUE)
  if (isTRUE(blank)) cat("\n")
  nomo_present_text("See ", nomo_present_or(paste(call, "for", what), "and"), ".")
}


# A proportion as a percentage (guide point 14): whole numbers when every base
# is under 100, and one decimal otherwise, so that 499 of 500 reads as 99.8%
# rather than rounding to 100%. Without a base, one decimal. Rounded half up;
# "--" when it is not available.
nomo_present_percent <- function(x, base = NULL) {
  x <- suppressWarnings(as.numeric(x))
  base <- suppressWarnings(as.numeric(base))
  digits <- if (length(base) && all(is.finite(base) & base < 100)) 0L else 1L
  out <- paste0(formatC(nomo_present_round(100 * x, digits), format = "f",
                        digits = digits), "%")
  out[!is.finite(x)] <- nomo_present_missing
  out
}


# A noun in the number its count takes, and the count with it: "1 factor",
# "2 factors", rather than "2 factor(s)".
nomo_present_noun <- function(n, singular, plural = paste0(singular, "s")) {
  ifelse(n == 1, singular, plural)
}

nomo_present_count <- function(n, singular, plural = paste0(singular, "s")) {
  paste(n, nomo_present_noun(n, singular, plural))
}


# Alternatives joined as prose: "a", "a or b", "a, b, or c"; with
# `word = "and"`, a list: "a and b", "a, b, and c".
nomo_present_or <- function(x, word = "or") {
  n <- length(x)
  if (n < 3L) return(paste(x, collapse = paste0(" ", word, " ")))
  paste0(paste(x[-n], collapse = ", "), ", ", word, " ", x[[n]])
}


# A stored origin in words: "a priori" and "post hoc" take no hyphen (guide
# point 25), and a cell starts with a capital.
nomo_present_origin <- function(x, cell = FALSE) {
  out <- gsub("[_-]", " ", tolower(as.character(x)))
  if (isTRUE(cell)) out <- paste0(toupper(substr(out, 1L, 1L)), substring(out, 2L))
  out[is.na(x)] <- nomo_present_missing
  out
}


# An ordinal: "1st", "2nd", "3rd", "11th", "93rd".
nomo_present_ordinal <- function(n) {
  n <- as.integer(round(n))
  last <- n %% 10L
  teen <- n %% 100L %in% 11:13
  suffix <- ifelse(teen, "th", ifelse(last == 1L, "st", ifelse(last == 2L, "nd",
                   ifelse(last == 3L, "rd", "th"))))
  paste0(n, suffix)
}


# Numbers -----------------------------------------------------------------------

# Rounding for display, half away from zero, as agreed with contentvalidR
# (#144): .625 prints .63, where formatC() rounds half to even and prints .62.
# The 1e-9 absorbs the binary error that leaves .625 a hair below its tie.
nomo_round_half_up <- function(x, digits) {
  s <- 10^digits
  sign(x) * floor(abs(x) * s + 0.5 + 1e-9) / s
}

# The negative-zero guard: a value that rounds to zero is zero, so it is never
# printed "-0.00".
nomo_present_round <- function(x, digits) {
  x <- nomo_round_half_up(x, digits)
  x[is.finite(x) & x == 0] <- 0
  x
}


nomo_present_number <- function(x, digits = 3L, bounded = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  out <- formatC(nomo_present_round(x, digits), format = "f", digits = digits)
  if (isTRUE(bounded)) out <- sub("^(-?)0\\.", "\\1.", out)
  out[!is.finite(x)] <- nomo_present_missing
  out
}


# A change with its sign. A value that rounds to zero carries none: "+0.000"
# and "-0.000" would show a direction the data do not (guide point 15).
nomo_present_signed <- function(x, digits = 3L, bounded = FALSE) {
  x <- nomo_present_round(suppressWarnings(as.numeric(x)), digits)
  out <- nomo_present_number(x, digits, bounded)
  up <- is.finite(x) & x > 0
  out[up] <- paste0("+", out[up])
  out
}


# APA style: three decimals without the leading zero, "< .001" below that, and
# "> .999" where three decimals would print 1.000.
nomo_present_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  out <- nomo_present_number(p, 3L, bounded = TRUE)
  out[is.finite(p) & out == "1.000"] <- "> .999"
  out[is.finite(p) & p < 0.001] <- "< .001"
  out
}


# "p < .001", "p = .043", or "p > .999"; empty when p is not available.
nomo_present_p_clause <- function(p) {
  shown <- nomo_present_p(p)
  ifelse(shown == nomo_present_missing, "",
         ifelse(grepl("^[<>]", shown), paste("p", shown), paste("p =", shown)))
}


# "[LL, UL]", or the missing marker when either bound is missing: never half an
# interval. `kind` takes the digits and bound rule of nomo_present_stat().
nomo_present_ci <- function(lower, upper, digits = 3L, bounded = FALSE, kind = NULL) {
  if (!is.null(kind)) {
    rule <- nomo_present_kinds[[nomo_match_arg(kind, names(nomo_present_kinds))]]
    if (missing(digits)) digits <- rule$digits
    bounded <- rule$bounded
  }
  lower <- suppressWarnings(as.numeric(lower))
  upper <- suppressWarnings(as.numeric(upper))
  out <- sprintf("[%s, %s]", nomo_present_number(lower, digits, bounded),
                 nomo_present_number(upper, digits, bounded))
  out[!is.finite(lower) | !is.finite(upper)] <- nomo_present_missing
  out
}


# The heading of an interval, with the level taken from the object: "95% CI".
nomo_present_ci_label <- function(level = 0.95) {
  paste0(format(100 * level, trim = TRUE), "% CI")
}


# Degrees of freedom: whole numbers as integers, a fractional (scaled) df with
# two decimals.
nomo_present_df <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- nomo_present_number(x, 2L)
  whole <- is.finite(x) & abs(x - round(x)) < 1e-8
  out[whole] <- format(round(x[whole]), trim = TRUE, scientific = FALSE)
  out
}


# A significance level keeps the digits it was given, at least two, without the
# leading zero: .05, .10, .025, .001. A computed level such as .05 / 3 is shown
# to three significant digits (.0167).
nomo_present_level <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- vapply(x, function(a) {
    if (!is.finite(a)) return(nomo_present_missing)
    a <- signif(a, 3L)
    given <- format(a, digits = 15L, scientific = FALSE, drop0trailing = TRUE)
    decimals <- if (grepl(".", given, fixed = TRUE)) {
      nchar(sub("^[^.]*\\.", "", given))
    } else {
      0L
    }
    nomo_present_number(a, max(2L, decimals), bounded = TRUE)
  }, character(1))
  unname(out)
}


# One entry point for a statistic: the precision and leading-zero rule of its
# kind (see the table at the top of this file).
nomo_present_stat <- function(x, kind, digits = NULL, reference = NULL,
                              signed = FALSE) {
  kind <- nomo_match_arg(kind, names(nomo_present_kinds))
  if (kind == "p") return(nomo_present_p(x))
  if (kind == "df") return(nomo_present_df(x))
  if (kind == "level") return(nomo_present_level(x))
  rule <- nomo_present_kinds[[kind]]
  if (is.null(digits)) digits <- rule$digits
  format_at <- function(v, d) {
    if (isTRUE(signed)) nomo_present_signed(v, d, rule$bounded) else
      nomo_present_number(v, d, rule$bounded)
  }
  x <- suppressWarnings(as.numeric(x))
  if (is.null(reference)) return(format_at(x, digits))

  # A value that differs from its reference but would print as the reference
  # gets up to three more decimals, so a flag never reads "0.40 is below 0.40".
  reference <- rep_len(suppressWarnings(as.numeric(reference)), length(x))
  shown_digits <- rep(as.integer(digits), length(x))
  for (extra in 1:3) {
    same <- is.finite(x) & is.finite(reference) & x != reference &
      nomo_present_round(x, shown_digits) == nomo_present_round(reference, shown_digits)
    if (!any(same)) break
    shown_digits[same] <- shown_digits[same] + 1L
  }
  vapply(seq_along(x), function(i) format_at(x[[i]], shown_digits[[i]]), character(1))
}


# A chi-square test as APA writes it (guide point 17): "chi-square(34) =
# 75.83, p < .001"; with the N of a contingency table, "chi-square(1, N = 40) =
# 10.00, p = .002"; and a difference test, "Delta chi-square(6) = 12.34, p =
# .015". Empty when there is no statistic.
nomo_present_chisq <- function(stat, df, p = NA_real_, n = NULL, delta = FALSE) {
  size <- max(length(stat), length(df), length(p))
  stat <- rep_len(suppressWarnings(as.numeric(stat)), size)
  p <- rep_len(p, size)
  inside <- nomo_present_df(df)
  if (!is.null(n)) inside <- paste0(inside, ", N = ", nomo_present_stat(n, "count"))
  out <- paste0(if (isTRUE(delta)) "Delta chi-square(" else "chi-square(", inside,
                ") = ", nomo_present_stat(stat, "stat"))
  clause <- nomo_present_p_clause(p)
  out <- ifelse(nzchar(clause), paste0(out, ", ", clause), out)
  out[!is.finite(stat)] <- ""
  out
}


# Status words ------------------------------------------------------------------

# The package records flags in several vocabularies (KEEP / REVIEW / STRONG
# REVIEW for loadings, info / review / concern for evidence, and so on). They
# are displayed in one: blank for no flag, then "review", then "concern", and
# "not computed". This is the lowercase form, for running prose.
nomo_present_flag <- function(x) {
  key <- toupper(trimws(as.character(x)))
  map <- c(
    "KEEP" = "", "NONE" = "", "INFO" = "", "OK" = "",
    "REVIEW" = "review",
    "STRONG REVIEW" = "concern", "CONCERN" = "concern",
    "UNAVAILABLE" = "not computed"
  )
  out <- unname(map[key])
  unknown <- is.na(out)
  out[unknown] <- tolower(as.character(x))[unknown]
  out[is.na(out)] <- ""
  out
}

# The same words in sentence case, for a table cell or the start of a line
# (guide point 18): "", "Review", "Concern", "Not computed".
nomo_present_status <- function(x) {
  flag <- nomo_present_flag(x)
  paste0(toupper(substr(flag, 1L, 1L)), substring(flag, 2L))
}


nomo_present_flag_counts <- function(x) {
  flags <- nomo_present_flag(x)
  review <- sum(flags == "review")
  concern <- sum(flags == "concern")
  if (!review && !concern) return("none")
  sprintf("%d review, %d concern", review, concern)
}


# The display flag as a plot legend shows it: "none" stands in for the blank
# flag, and the levels run in order of severity.
nomo_present_flag_legend <- function(x) {
  flag <- nomo_present_flag(x)
  flag[!nzchar(flag)] <- "none"
  factor(flag, levels = c("none", "review", "concern"))
}

# The status shapes of nomo_plot_status_shapes (R/nomo_plot_helpers.R), for the
# plots that map nomo_present_flag_legend(): filled circle, open circle, filled
# square.
nomo_present_flag_shapes <- c(none = 16, review = 1, concern = 15)


# Tables ------------------------------------------------------------------------

# Leave out a column whose values are all the same, such as a block column that
# only ever says "overall"; it says nothing the heading does not.
nomo_present_drop_constant <- function(columns, label, values) {
  if (length(unique(values)) < 2L) columns[names(columns) != label] else columns
}


# An aligned text table. `columns` maps each display label to a column of `x`;
# `formats` maps a column name to a function returning character values. A
# column that is empty or missing in every row is dropped. A table wider than
# the console tightens its column gaps from 2 spaces to 1, then drops columns
# from the right, never the first (the stub) or one whose label is in `keep`,
# and names them alongside `more`, the call that shows them.
nomo_present_table <- function(x, columns, formats = list(), more = NULL,
                               indent = 2L, keep = nomo_present_keep) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  columns <- columns[unname(columns) %in% names(x)]
  if (!nrow(x) || !length(columns)) return(invisible(NULL))
  missing_mark <- nomo_present_missing

  cells <- lapply(unname(columns), function(col) {
    value <- x[[col]]
    fmt <- formats[[col]]
    if (is.function(fmt)) {
      out <- fmt(value)
    } else if (is.numeric(value) && !is.integer(value)) {
      out <- nomo_present_number(value)
    } else if (is.logical(value)) {
      out <- ifelse(is.na(value), missing_mark, ifelse(value, "yes", "no"))
    } else {
      out <- as.character(value)
    }
    out[is.na(out)] <- missing_mark
    out
  })
  names(cells) <- names(columns)
  # Numbers, including formatted p values, are right-aligned.
  numeric_col <- vapply(unname(columns), function(col) is.numeric(x[[col]]), logical(1))

  # A lone "-" from an older formatter counts as missing too.
  shown <- vapply(cells, function(v) any(nzchar(v) & !(v %in% c(missing_mark, "-"))),
                  logical(1))
  cells <- cells[shown]
  numeric_col <- numeric_col[shown]
  if (!length(cells)) return(invisible(NULL))

  # By position, so two columns may share a label.
  widths <- vapply(seq_along(cells), function(i) {
    max(nchar(c(names(cells)[[i]], cells[[i]]), type = "width"))
  }, integer(1))
  kept <- seq_along(cells) == 1L | tolower(names(cells)) %in% tolower(keep)
  room <- nomo_present_width()
  total <- function(gap) indent + sum(widths) + gap * (length(widths) - 1L)
  gap <- if (total(2L) > room) 1L else 2L
  hidden <- character()
  while (total(gap) > room && any(!kept)) {
    drop <- max(which(!kept))
    hidden <- c(names(cells)[[drop]], hidden)
    cells <- cells[-drop]
    widths <- widths[-drop]
    numeric_col <- numeric_col[-drop]
    kept <- kept[-drop]
  }

  align <- function(v, w, right) {
    space <- strrep(" ", pmax(0L, w - nchar(v, type = "width")))
    if (right) paste0(space, v) else paste0(v, space)
  }
  pad <- strrep(" ", indent)
  sep <- strrep(" ", gap)
  emit <- function(row) cat(pad, sub("\\s+$", "", paste(row, collapse = sep)), "\n", sep = "")
  emit(vapply(seq_along(cells), function(i) {
    align(names(cells)[[i]], widths[[i]], numeric_col[[i]])
  }, character(1)))
  for (r in seq_len(nrow(x))) {
    emit(vapply(seq_along(cells), function(i) {
      align(cells[[i]][[r]], widths[[i]], numeric_col[[i]])
    }, character(1)))
  }
  if (length(hidden)) {
    note <- paste0("Not shown for width: ", paste(hidden, collapse = ", "), ".")
    if (!is.null(more)) {
      note <- paste0(note, " See ", gsub(" ", nomo_present_nbsp, more, fixed = TRUE), ".")
    }
    nomo_present_text(note, indent = indent)
  }
  invisible(NULL)
}
