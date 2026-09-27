# Console presentation helpers (#89) ------------------------------------------
#
# Every print() and summary() method writes through these helpers so that the
# console output follows one set of rules:
#
# * a header names the object's class and what it is;
# * prose is wrapped to the console width, and output is ASCII;
# * tables are aligned text with curated columns and no type row; a column that
#   is empty everywhere is dropped, and a table too wide for the console drops
#   trailing columns and names them rather than wrapping into blocks;
# * numbers have fixed decimals, p-values follow APA style, and a missing value
#   is shown as "-";
# * a flagged row's explanation is printed in full as a wrapped bullet, never
#   cut off inside a table cell;
# * flags use one display wording: blank (no flag), "review", or "concern".
#
# The helpers change only what is printed. Returned objects, nomo_table() types,
# and decision-log values are untouched, as the stability policy requires.

# options() only accepts a width from 10 to 10000, so it is always usable.
nomo_present_width <- function() {
  max(40L, as.integer(getOption("width", 80L)))
}


nomo_present_header <- function(class, title, summary = FALSE) {
  cat(sprintf("<%s%s> %s\n", class, if (isTRUE(summary)) " summary" else "", title))
}


nomo_present_section <- function(title) {
  cat("\n", title, "\n", sep = "")
}


# One or more paragraphs, each wrapped to the console width.
nomo_present_text <- function(..., indent = 0L) {
  text <- paste0(...)
  lines <- strwrap(text, width = nomo_present_width() - 1L, indent = indent,
                   exdent = indent)
  cat(paste0(lines, "\n"), sep = "")
}


# A line of "label: value" parts joined by " | ", broken between parts when it
# would run past the console width.
nomo_present_facts <- function(parts) {
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(invisible(NULL))
  width <- nomo_present_width() - 1L
  # A single part longer than the console, such as a long stage sequence, is
  # wrapped with a hanging indent.
  emit <- function(line) {
    cat(paste0(strwrap(line, width = width, exdent = 2L), "\n"), sep = "")
  }
  line <- parts[[1L]]
  for (part in parts[-1L]) {
    candidate <- paste(line, part, sep = " | ")
    if (nchar(candidate) > width) {
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
    lines <- strwrap(item, width = nomo_present_width() - 1L,
                     initial = paste0(pad, "- "), prefix = paste0(pad, "  "))
    cat(paste0(lines, "\n"), sep = "")
  }
}


# Notes with a severity: a review or concern note is prefixed with its flag,
# and an informational note is printed as it is.
nomo_present_notes <- function(notes) {
  if (!is.data.frame(notes) || !nrow(notes)) return(invisible(NULL))
  flag <- nomo_present_flag(notes$severity)
  nomo_present_bullets(ifelse(nzchar(flag), paste0(flag, ": ", notes$note), notes$note))
}


# A noun in the number its count takes, and the count with it: "1 factor",
# "2 factors", rather than "2 factor(s)".
nomo_present_noun <- function(n, singular, plural = paste0(singular, "s")) {
  ifelse(n == 1, singular, plural)
}

nomo_present_count <- function(n, singular, plural = paste0(singular, "s")) {
  paste(n, nomo_present_noun(n, singular, plural))
}


# Alternatives joined as prose: "a", "a or b", "a, b, or c".
nomo_present_or <- function(x) {
  n <- length(x)
  if (n < 3L) return(paste(x, collapse = " or "))
  paste0(paste(x[-n], collapse = ", "), ", or ", x[[n]])
}


nomo_present_number <- function(x, digits = 3L) {
  x <- suppressWarnings(as.numeric(x))
  out <- formatC(x, format = "f", digits = digits)
  out[!is.finite(x)] <- "-"
  out
}


nomo_present_signed <- function(x, digits = 3L) {
  x <- suppressWarnings(as.numeric(x))
  out <- formatC(x, format = "f", digits = digits, flag = "+")
  out[!is.finite(x)] <- "-"
  out
}


# APA style: "< .001", or three decimals without the leading zero.
nomo_present_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  out <- sub("^0\\.", ".", formatC(p, format = "f", digits = 3))
  out[is.finite(p) & p < 0.001] <- "< .001"
  out[!is.finite(p)] <- "-"
  out
}


# "p < .001" or "p = .043"; empty when p is not available.
nomo_present_p_clause <- function(p) {
  shown <- nomo_present_p(p)
  ifelse(shown == "-", "",
         ifelse(startsWith(shown, "<"), paste("p", shown), paste("p =", shown)))
}


nomo_present_ci <- function(lower, upper, digits = 3L) {
  out <- sprintf("[%s, %s]", nomo_present_number(lower, digits),
                 nomo_present_number(upper, digits))
  out[!is.finite(suppressWarnings(as.numeric(lower))) &
        !is.finite(suppressWarnings(as.numeric(upper)))] <- "-"
  out
}


# The package records flags in several vocabularies (KEEP / REVIEW / STRONG
# REVIEW for loadings, info / review / concern for evidence, and so on). They
# are displayed in one: blank for no flag, then "review", then "concern".
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

nomo_present_flag_shapes <- c(none = 16, review = 17, concern = 15)


# Leave out a column whose values are all the same, such as a block column that
# only ever says "overall"; it says nothing the heading does not.
nomo_present_drop_constant <- function(columns, label, values) {
  if (length(unique(values)) < 2L) columns[names(columns) != label] else columns
}


# An aligned text table. `columns` maps each display label to a column of `x`;
# `formats` maps a column name to a function returning character values. A
# column that is empty or "-" in every row is dropped, and a table wider than
# the console drops trailing columns, which `more` then names alongside the call
# that shows them.
nomo_present_table <- function(x, columns, formats = list(), more = NULL,
                               indent = 2L) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  columns <- columns[unname(columns) %in% names(x)]
  if (!nrow(x) || !length(columns)) return(invisible(NULL))

  cells <- lapply(unname(columns), function(col) {
    value <- x[[col]]
    fmt <- formats[[col]]
    if (is.function(fmt)) {
      out <- fmt(value)
    } else if (is.numeric(value) && !is.integer(value)) {
      out <- nomo_present_number(value)
    } else if (is.logical(value)) {
      out <- ifelse(is.na(value), "-", ifelse(value, "yes", "no"))
    } else {
      out <- as.character(value)
    }
    out[is.na(out)] <- "-"
    out
  })
  names(cells) <- names(columns)
  # Numbers, including formatted p-values, are right-aligned.
  numeric_col <- vapply(unname(columns), function(col) is.numeric(x[[col]]), logical(1))

  shown <- vapply(cells, function(v) any(nzchar(v) & v != "-"), logical(1))
  cells <- cells[shown]
  numeric_col <- numeric_col[shown]
  if (!length(cells)) return(invisible(NULL))

  # By position, so two columns may share a label.
  widths <- vapply(seq_along(cells), function(i) {
    max(nchar(names(cells)[[i]]), nchar(cells[[i]]))
  }, integer(1))
  budget <- nomo_present_width() - 1L - indent
  hidden <- character()
  while (length(cells) > 1L && sum(widths) + 2L * (length(widths) - 1L) > budget) {
    last <- length(cells)
    hidden <- c(names(cells)[[last]], hidden)
    cells <- cells[-last]
    widths <- widths[-last]
    numeric_col <- numeric_col[-last]
  }

  align <- function(v, w, right) formatC(v, width = if (right) w else -w)
  pad <- strrep(" ", indent)
  header <- vapply(seq_along(cells), function(i) {
    align(names(cells)[[i]], widths[[i]], numeric_col[[i]])
  }, character(1))
  cat(pad, sub("\\s+$", "", paste(header, collapse = "  ")), "\n", sep = "")
  for (r in seq_len(nrow(x))) {
    row <- vapply(seq_along(cells), function(i) {
      align(cells[[i]][[r]], widths[[i]], numeric_col[[i]])
    }, character(1))
    cat(pad, sub("\\s+$", "", paste(row, collapse = "  ")), "\n", sep = "")
  }
  if (length(hidden)) {
    note <- paste0("Not shown for width: ", paste(hidden, collapse = ", "), ".")
    if (!is.null(more)) note <- paste0(note, " See ", more, ".")
    nomo_present_text(note, indent = indent)
  }
  invisible(NULL)
}
