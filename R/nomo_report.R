# Reproducible nomologR report -------------------------------------------------

nomo_report_empty_table <- function() {
  tibble::tibble()
}


nomo_report_matrix_table <- function(x, row_name = "row") {
  if (is.null(x) || !is.matrix(x) || !length(x)) {
    return(nomo_report_empty_table())
  }

  out <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  rn <- rownames(x)

  if (!is.null(rn)) {
    out <- data.frame(
      stats::setNames(list(rn), row_name),
      out,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  tibble::as_tibble(out, .name_repair = "minimal")
}


nomo_report_flatten_table <- function(x) {
  if (is.null(x)) return(nomo_report_empty_table())

  if (is.matrix(x)) {
    x <- nomo_report_matrix_table(x)
  }

  if (!inherits(x, "data.frame")) {
    return(nomo_report_empty_table())
  }

  out <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)

  for (nm in names(out)) {
    if (is.list(out[[nm]])) {
      out[[nm]] <- vapply(
        out[[nm]],
        function(z) {
          if (is.null(z) || !length(z)) return("")
          paste(as.character(unlist(z, recursive = TRUE, use.names = FALSE)), collapse = "; ")
        },
        character(1)
      )
    }
  }

  tibble::as_tibble(out, .name_repair = "minimal")
}


# Report tables for reading (#89, #144) -----------------------------------------
#
# The report shows the tables the package returns, whose column names are the
# code's. For reading, a name becomes words ("omega_ci_lower" is "Omega CI
# lower"), a flag uses the shared status words in sentence case ("Review",
# "Concern", "Not computed", blank for no flag), a missing or empty cell is an
# em dash, as in the APA tables, and TRUE/FALSE is yes/no. Numbers are rounded
# for display by the console's rules (R/nomo_present.R), chosen from the
# column's name: p values to three decimals ("< .001"), a proportion stored
# under `pct` shown as a percentage, counts and degrees of freedom as whole
# numbers, and each statistic with its kind's precision and leading zero. A
# table of `metric` and `value` rows takes each row's kind from its metric.
# Names that are not the package's own are left as they are: any name with a
# capital letter, such as a factor called F1, a single word with a digit, such
# as an item called ag1, and whatever the caller protects, such as the run's
# item and scale names.
#
# Pandoc reads markdown inside the cells, even those of an HTML table, so a cell
# is escaped before it is written (nomo_report_escape()): lavaan's `~~` would
# otherwise become a subscript and `a*x1 + a*x2` lose its asterisks.

nomo_report_label_words <- c(
  abs = "absolute", ave = "AVE", cfa = "CFA", cfi = "CFI", chisq = "chi-square",
  ci = "CI", delta = "change in", df = "df", doi = "DOI", efa = "EFA",
  epc = "EPC", htmt = "HTMT", htmt2 = "HTMT2", id = "ID", interitem = "inter-item",
  kmo = "KMO", lrt = "LRT", mi = "MI", msa = "MSA", n = "N", pct = "%",
  prop = "proportion", rmsea = "RMSEA", sd = "SD", se = "SE", srmr = "SRMR",
  tli = "TLI"
)

nomo_report_label_overrides <- c(
  attention = "Flag", severity = "Flag", signal = "Flag",
  measurement_attention = "Measurement flag",
  df = "df", p = "p", p_value = "p", pvalue = "p", lrt_p = "LRT p", r = "r", z = "z",
  chi_square = "Chi-square", score_x2 = "Score chi-square",
  n_cases = "Cases", n_columns = "Columns",
  n_unique = "Unique values", mode_n = "Responses at the mode",
  n_interitem_estimable = "Inter-item correlations estimated",
  negative_interitem_n = "Negative pairs",
  n_loading_review = "Loadings flagged",
  item1 = "Item 1", item2 = "Item 2",
  cross_loading = "Cross-loading", near_zero_variance = "Near-zero variance",
  lavaan_missing = "lavaan missing",
  corrected_item_rest_r = "Corrected item-rest r", item_rest_n = "Item-rest N",
  scale_item_rest_r = "Within-scale item-rest r",
  scale_item_rest_n = "Within-scale item-rest N",
  scale_negative_interitem_n = "Negative pairs within scale",
  omega_ci_n_success = "Omega CI successful draws",
  alpha_ci_n_success = "Alpha CI successful draws",
  lhs = "lhs", op = "op", rhs = "rhs",
  sepc.all = "Standardized EPC (all)", sepc.lv = "Standardized EPC (latent)",
  sepc.nox = "Standardized EPC (no x)"
)

nomo_report_flag_columns <- c("attention", "severity", "signal", "measurement_attention")

# Stored origins, shown as words without a hyphen or underscore ("A priori",
# "Post hoc"; guide point 25).
nomo_report_origin_columns <- c("origin", "confirmatory_status")

# Stored statuses, shown as words in sentence case (guide point 18):
# "awaiting_decision" is "Awaiting decision". The network's concordance and
# replication statuses take the labels its print() gives them.
nomo_report_network_status_columns <- c(
  "concordance", "primary_concordance", "validation_concordance",
  "replication_status"
)

# N in a heading is a number of cases (guide point 2): `n`, `n_observed`,
# `item_rest_n`. A count of anything else is named in words: `n_items` is
# "Number of items".
nomo_report_case_words <- c(
  "cases", "observed", "missing", "used", "complete", "incomplete", "valid", "total"
)

nomo_report_blank <- "\u2014"


nomo_report_column_labels <- function(names, protected = character()) {
  vapply(names, function(name) {
    if (name %in% protected || grepl("[A-Z]", name)) return(name)
    if (name %in% names(nomo_report_label_overrides)) {
      return(nomo_report_label_overrides[[name]])
    }
    words <- strsplit(name, "[._]")[[1L]]
    if (length(words) == 1L && grepl("[0-9]", name)) return(name)
    if (length(words) > 1L && words[[1L]] == "n" &&
        !words[[2L]] %in% nomo_report_case_words) {
      words[[1L]] <- "number of"
    }
    known <- words %in% names(nomo_report_label_words)
    words[known] <- nomo_report_label_words[words[known]]
    label <- paste(words, collapse = " ")
    paste0(toupper(substr(label, 1L, 1L)), substring(label, 2L))
  }, character(1), USE.NAMES = FALSE)
}


# The kind of number a column holds, from its name or, in a table of `metric`
# and `value` rows, from the row's metric: one of the kinds of
# nomo_present_stat(), or "pct" (a proportion shown as a percentage),
# "percent" (a percentage already), and "residual" (a residual correlation,
# with the three decimals the EFA print gives it, since two would show most
# as zero). A name says nothing more for "estimate", the default: two decimals
# with the leading zero.
#
# A log metric whose name does not say what its value is has its kind listed
# (nomo_report_metric_kinds): `htmt_missing_data` names a statistic and holds
# the number of cases used, and `missingness` holds a proportion that the
# row's own sentence gives as a percentage. Such a proportion is "pct_sign",
# shown with its percent sign ("3.0%"), since a "Value" heading cannot carry
# the unit as "% missing" does.
nomo_report_metric_kinds <- c(
  htmt_missing_data = "count", htmt_single_indicator = "count",
  missingness = "pct_sign", all_missing = "pct_sign",
  missing_mechanism = "pct_sign", response_concentration = "pct_sign",
  floor_concentration = "pct_sign", ceiling_concentration = "pct_sign"
)

nomo_report_metric_kind <- function(metric) {
  listed <- unname(nomo_report_metric_kinds[as.character(metric)])
  ifelse(is.na(listed), nomo_report_number_kind(metric), listed)
}


nomo_report_number_kind <- function(name) {
  words <- strsplit(tolower(as.character(name)), "[._ ]+")
  vapply(words, function(w) {
    w <- w[!is.na(w) & nzchar(w)]
    if (!length(w)) return("estimate")
    has <- function(...) any(w %in% c(...))
    if (has("pvalue", "bartlett") || w[[length(w)]] == "p" ||
        grepl("(^|_)p_value$", paste(w, collapse = "_"))) {
      return("p")
    }
    if (has("pct")) return("pct")
    if (has("percent")) return("percent")
    if (has("df")) return("df")
    if (has("n", "count", "cases", "cells", "items", "judges", "npar", "step")) return("count")
    if (has("cfi")) return("fit_bounded")
    if (has("tli", "rmsea", "srmr", "rmsr")) return("fit")
    if (has("aic", "bic")) return("ic")
    if (has("htmt", "htmt2")) return("htmt")
    if (has("omega", "alpha", "ave", "reliability", "icc", "determinacy")) return("reliability")
    if (has("loading", "lambda")) return("loading")
    if (has("power", "coverage")) return("power")
    if (has("prop", "proportion", "communality", "uniqueness", "kmo", "msa")) return("proportion")
    if (has("r", "correlation", "validity", "univocality", "accuracy")) return("r")
    # The decision log's corrected item-rest correlation, `corrected_item_rest`.
    if (identical(utils::tail(w, 2L), c("item", "rest"))) return("r")
    if (has("chisq", "chi", "x2", "z", "t", "f", "mi", "statistic")) return("stat")
    # `residual` and `abs_residual`, not a residual variance.
    if (w[[length(w)]] == "residual") return("residual")
    "estimate"
  }, character(1), USE.NAMES = FALSE)
}


# Values of one kind as text. `digits` replaces the kind's precision.
# `reference` is for one value: the numbers it is read against. The value is
# shown with the decimals it needs to differ from each of them (guide point 9),
# so a flagged omega of .698 is not printed ".70" beside a reference of .70.
nomo_report_format_kind <- function(value, kind, digits = NULL, signed = FALSE,
                                    reference = NULL) {
  stat <- function(v, k, d = digits) {
    if (!length(reference)) return(nomo_present_stat(v, k, digits = d, signed = signed))
    shown <- vapply(reference, function(r) {
      nomo_present_stat(v, k, digits = d, reference = r, signed = signed)
    }, character(1))
    shown[[which.max(nchar(shown))]]
  }
  switch(
    kind,
    pct = stat(100 * value, "estimate", 1L),
    pct_sign = paste0(stat(100 * value, "estimate", 1L), "%"),
    percent = stat(value, "estimate", 1L),
    residual = stat(value, "r", 3L),
    stat(value, kind)
  )
}


# A numeric column as text: each value by its kind, a missing value as the
# report's dash. Integer storage is a count. In a table of metric and value
# rows, a whole value in a row whose metric names no kind is a count too: the
# evidence trace mixes counts with statistics. `reference` holds, for each
# row, the numbers its value is read against (nomo_report_value_reference());
# a value that is not a count is shown with the decimals that tell it from
# them. `digits` replaces the kind's precision for the whole column. A change
# from the preceding model, stored under `delta`, and a difference from a
# reference estimate, stored under `difference`, carry their sign, and none
# when they round to zero (guide point 15), as the invariance and missing-data
# prints show them.
nomo_report_format_numbers <- function(value, name, metric = NULL, reference = NULL,
                                       digits = NULL) {
  kind <- if (is.integer(value)) "count" else nomo_report_number_kind(name)
  signed <- grepl("^(delta|difference)([._]|$)", tolower(name))
  value <- as.numeric(value)
  finite <- is.finite(value)
  if (identical(kind, "estimate") && !is.null(metric)) {
    kinds <- nomo_report_metric_kind(metric)
    whole <- finite & abs(value - round(value)) < 1e-8
    kinds[kinds == "estimate" & whole] <- "count"
  } else {
    kinds <- rep(kind, length(value))
  }

  out <- rep(nomo_report_blank, length(value))
  for (k in unique(kinds[finite])) {
    i <- finite & kinds == k
    out[i] <- nomo_report_format_kind(value[i], k, digits, signed)
  }
  for (i in which(finite & kinds != "count" & lengths(reference) > 0L)) {
    out[[i]] <- nomo_report_format_kind(value[[i]], kinds[[i]], digits, signed,
                                        reference[[i]])
  }
  out
}


# The numbers a text names: ".70" in "configured review reference = .70", both
# limits of "[-0.15, 0.15]", "90.0" in "90.0% of responses". A digit that is
# part of a name ("TR2", "HTMT2") or an exponent is not one, and a hyphen
# between two numbers (".50-.75") is not a sign.
nomo_report_reference_numbers <- function(text) {
  number <- paste0(
    "(?<![A-Za-z0-9.^])-?(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)",
    "(?![A-Za-z0-9]|\\.[0-9])"
  )
  text <- as.character(text)
  text[is.na(text)] <- ""
  lapply(regmatches(text, gregexpr(number, text, perl = TRUE)), as.numeric)
}


# For each row of a table, the numbers its `value` is read against, or NULL
# when the table has no `reference` column. A reference stored as a number,
# such as the teaching reference of a fit index, is that number. A reference
# stored as text, as in every decision log ("configured review reference =
# .70", "Teaching reference .30; not a retention rule"), gives the numbers it
# names, and only for a row flagged for review or concern: the flag says the
# value fell on one side of its reference, so the two must not print alike.
nomo_report_value_reference <- function(x) {
  reference <- x[["reference"]]
  if (is.numeric(reference)) return(lapply(reference, function(r) r[is.finite(r)]))
  if (!is.character(reference)) return(NULL)
  flagged <- rep(FALSE, nrow(x))
  for (name in intersect(names(x), nomo_report_flag_columns)) {
    flagged <- flagged | nomo_present_status(x[[name]]) %in% c("Review", "Concern")
  }
  numbers <- nomo_report_reference_numbers(reference)
  numbers[!flagged] <- list(numeric())
  numbers
}


# In a table that sets an estimate beside the reference estimate it is
# compared with, these columns take three decimals, as print.nomo_missing()
# gives them: the table exists to show small differences, and both estimates
# must have one precision to be compared (guide point 9).
nomo_report_compared_columns <- c(
  "estimate", "reference", "reference_estimate", "difference"
)


# A stored word as a cell shows it: underscores as spaces, in sentence case.
nomo_report_sentence <- function(x) {
  x <- gsub("_", " ", as.character(x), fixed = TRUE)
  out <- paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))
  out[is.na(x)] <- NA_character_
  out
}


nomo_report_display_table <- function(x, protected = character()) {
  x <- nomo_report_flatten_table(x)
  metric <- if (is.character(x[["metric"]])) x[["metric"]]
  value_reference <- nomo_report_value_reference(x)
  compared <- is.numeric(x[["estimate"]]) &&
    (is.numeric(x[["reference"]]) || is.numeric(x[["reference_estimate"]]))
  role <- rep("text", ncol(x))
  for (j in seq_along(x)) {
    name <- names(x)[[j]]
    value <- x[[j]]
    if (name %in% nomo_report_flag_columns) {
      # No flag is a blank cell, as in the console; the status column is never
      # left out for being blank.
      value <- nomo_present_status(value)
      role[[j]] <- "status"
    } else if (is.logical(value)) {
      value <- ifelse(value, "yes", "no")
    } else if (is.numeric(value)) {
      value <- nomo_report_format_numbers(
        value, name, metric,
        reference = if (identical(name, "value")) value_reference,
        digits = if (compared && name %in% nomo_report_compared_columns) 3L
      )
      role[[j]] <- "numeric"
    } else if (name %in% nomo_report_origin_columns && is.character(value)) {
      value <- nomo_present_origin(value, cell = TRUE)
    } else if (name %in% nomo_report_network_status_columns && is.character(value)) {
      value <- nomo_report_sentence(nomo_network_pretty_status(value))
      role[[j]] <- "status"
    } else if (identical(name, "status") && is.character(value)) {
      value <- nomo_report_sentence(value)
      role[[j]] <- "status"
    }
    if (is.character(value) && !identical(role[[j]], "status")) {
      value[is.na(value) | !nzchar(trimws(value)) | value == nomo_present_missing] <-
        nomo_report_blank
    }
    value[is.na(value)] <- ""
    x[[j]] <- value
  }
  names(x) <- nomo_report_column_labels(names(x), protected)
  attr(x, "role") <- role
  x
}


# Text for a cell, a heading, or a note, escaped so that pandoc shows it as
# written (#145). Each character markdown reads as markup becomes a numeric
# character reference, which pandoc reads as that character and nothing else,
# in the HTML report and in the markdown of the Word one alike. (A backslash
# escape would not do: with R Markdown's `tex_math_single_backslash`, "\[" opens
# display math.) Runs of hyphens and dots, which pandoc would set as dashes and
# an ellipsis, are escaped the same way, and so is whatever would start a block
# where the text begins: a list marker, alone or before a space, in every form
# pandoc reads ("- x", "+ x", "1. x", "1) x", "(1) x", "a) x", "(iv) x",
# "#. x"), and a run of colons, which opens a fenced div. In an HTML table a
# rationale that began "1) ..." became a numbered list that swallowed the rows
# after it, and a cell holding only "+" a bullet. A line break is <br> in HTML
# and "; " in a Word table cell, which also keeps lavaan syntax valid. R names
# in backticks stay code spans, as the console writes them. `cell = TRUE` is
# for a table cell: in a Word table, a code span holding a bar, such as a
# lavaan threshold `x1 | t1`, is escaped like other text, since a bar there
# would end the cell and its character reference would be shown as typed
# inside code.
nomo_report_escape <- function(x, to = c("html", "markdown"), cell = FALSE) {
  to <- match.arg(to)
  html <- identical(to, "html")
  bar_ends_cell <- !html && isTRUE(cell)
  markup <- c("\\", "`", "*", "_", "~", "^", "[", "]", "$", "@", "#", "|",
              "\"", "'", "!", "{", "}", "&", "<", ">")
  # What numbers a list item: digits, one letter, a roman numeral, or "#",
  # which is a character reference by the time the marker is looked for.
  number <- "(?:[0-9]+|[A-Za-z]|[ivxlcdm]+|[IVXLCDM]+|&#35;)"
  escape_text <- function(s) {
    chars <- strsplit(s, "", fixed = TRUE)[[1L]]
    special <- chars %in% markup
    chars[special] <- sprintf("&#%d;", vapply(chars[special], utf8ToInt, integer(1)))
    s <- paste(chars, collapse = "")
    s <- gsub("-(?=-)|(?<=-)-", "&#45;", s, perl = TRUE)
    s <- gsub("\\.(?=\\.)|(?<=\\.)\\.", "&#46;", s, perl = TRUE)
    # Text that starts like a list item.
    s <- sub("^(\\s*)-(?=\\s|$)", "\\1&#45;", s, perl = TRUE)
    s <- sub("^(\\s*)\\+(?=\\s|$)", "\\1&#43;", s, perl = TRUE)
    s <- sub(paste0("^(\\s*)\\((?=", number, "\\)(?:\\s|$))"), "\\1&#40;", s, perl = TRUE)
    s <- sub(paste0("^(\\s*(?:&#40;)?", number, ")\\.(?=\\s|$)"), "\\1&#46;", s, perl = TRUE)
    s <- sub(paste0("^(\\s*(?:&#40;)?", number, ")\\)(?=\\s|$)"), "\\1&#41;", s, perl = TRUE)
    # Text that starts with colons; the length is -1 when it does not.
    colons <- attr(regexpr("^\\s*:+", s), "match.length")
    s <- paste0(gsub(":", "&#58;", substring(s, 1L, colons), fixed = TRUE),
                substring(s, colons + 1L))
    gsub("\r?\n", if (html) "<br>" else "; ", s)
  }

  # Code spans are left as they are: pandoc shows their contents verbatim.
  # They are found before anything is escaped, so that two spans in one cell
  # stay two spans; those with a bar are then escaped for a Word table cell.
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    spans <- gregexpr("`[^`\r\n]+`", s)
    code <- regmatches(s, spans)[[1L]]
    text <- regmatches(s, spans, invert = TRUE)[[1L]]
    text <- vapply(text, function(t) if (nzchar(t)) escape_text(t) else t,
                   character(1), USE.NAMES = FALSE)
    barred <- bar_ends_cell & grepl("|", code, fixed = TRUE)
    code[barred] <- vapply(code[barred], escape_text, character(1), USE.NAMES = FALSE)
    paste0(text, c(code, ""), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}


# One report table, ready to write: HTML for an HTML report, a pipe table for a
# Word one, with the notes that go beneath it. NULL when there are no rows.
# Columns with no value in any row are left out, except the first and a status
# column, and the note names them. `markdown = TRUE` is for cells that are
# markdown already, such as R's citations with their italics; they are only
# made safe for the table.
nomo_report_table <- function(x, protected = character(), word = FALSE,
                              max_rows = Inf, markdown = FALSE) {
  x <- nomo_report_display_table(x, protected)
  if (!nrow(x) || !ncol(x)) return(NULL)
  role <- attr(x, "role")

  empty <- vapply(x, function(v) all(v %in% c("", nomo_report_blank)), logical(1))
  drop <- empty & role != "status" & seq_along(role) > 1L
  dropped <- names(x)[drop]
  x <- x[, !drop, drop = FALSE]
  role <- role[!drop]

  n_total <- nrow(x)
  if (n_total > max_rows) x <- utils::head(x, max_rows)

  # In trusted markdown, a bar in a pipe table cell is kable's to escape.
  to <- if (word) "markdown" else "html"
  escape <- function(v) {
    if (!isTRUE(markdown)) return(nomo_report_escape(v, to, cell = TRUE))
    if (word) return(gsub("[\r\n]+", " ", v))
    v <- gsub("&", "&amp;", v, fixed = TRUE)
    gsub(">", "&gt;", gsub("<", "&lt;", v, fixed = TRUE), fixed = TRUE)
  }
  cells <- as.data.frame(lapply(x, escape), stringsAsFactors = FALSE,
                         check.names = FALSE)
  labels <- nomo_report_escape(names(x), to, cell = TRUE)
  align <- ifelse(role == "numeric", "r", "l")
  markup <- if (word) {
    knitr::kable(cells, format = "pipe", col.names = labels, align = align,
                 row.names = FALSE)
  } else {
    knitr::kable(cells, format = "html", col.names = labels, align = align,
                 escape = FALSE, row.names = FALSE,
                 table.attr = 'class="table table-striped table-condensed nomo-table"')
  }

  notes <- c(
    if (length(dropped)) {
      sprintf("Columns with no value in any row are left out: %s.",
              paste(dropped, collapse = ", "))
    },
    if (n_total > nrow(x)) {
      sprintf("Showing %d of %d rows. The underlying nomo_run object retains all rows.",
              nrow(x), n_total)
    }
  )
  list(markup = paste(markup, collapse = "\n"), notes = notes,
       labels = names(x), cells = unlist(lapply(x, as.character), use.names = FALSE))
}


# Abbreviations (guide point 23) -----------------------------------------------
#
# Every abbreviation the report shows is defined once, in a table at its end.
# The report collects the headings and cells it writes; an abbreviation that
# appears in them is listed. A symbol such as p, r, or N is listed when it heads
# a column, since the same letter in a cell is usually part of a word or a
# formatted clause.

nomo_report_glossary <- c(
  AIC = "Akaike information criterion",
  APA = "American Psychological Association",
  AVE = "average variance extracted",
  BIC = "Bayesian information criterion",
  CFA = "confirmatory factor analysis",
  CFI = "comparative fit index",
  CI = "confidence interval",
  Csv = "coefficient of substantive validity",
  CVI = "content validity index; I-CVI is the index of one item",
  DOI = "digital object identifier",
  EFA = "exploratory factor analysis",
  EKC = "empirical Kaiser criterion",
  EPC = "expected parameter change",
  FIML = "full-information maximum likelihood",
  HTMT = "heterotrait-monotrait ratio of correlations",
  HTMT2 = "heterotrait-monotrait ratio computed from geometric means of correlations",
  ID = "identifier",
  IOC = "index of item-objective congruence",
  KMO = "Kaiser-Meyer-Olkin measure of sampling adequacy",
  LRT = "likelihood-ratio test",
  MAP = "minimum average partial",
  MAR = "missing at random",
  MCAR = "missing completely at random",
  MI = "modification index",
  MINRES = "minimum residual factor extraction",
  ML = "maximum likelihood",
  MLR = "maximum likelihood with robust standard errors and a scaled test statistic",
  MSA = "measure of sampling adequacy",
  NEST = "next eigenvalue sufficiency test",
  Psa = "proportion of substantive agreement",
  RMSEA = "root mean square error of approximation",
  RMSR = "root mean square residual",
  SD = "standard deviation",
  SE = "standard error",
  SEM = "structural equation model",
  SESOI = "smallest effect size of interest",
  SRMR = "standardized root mean square residual",
  TLI = "Tucker-Lewis index",
  TR2 = "MAP criterion averaging squared partial correlations",
  TR4 = "MAP criterion averaging partial correlations raised to the fourth power",
  WLSMV = "weighted least squares, mean- and variance-adjusted",
  df = "degrees of freedom",
  lhs = "left-hand side of a lavaan model term",
  N = "number of cases",
  op = "lavaan operator: =~ loads on, ~~ covaries with, ~ is regressed on",
  p = "p value",
  r = "correlation",
  rhs = "right-hand side of a lavaan model term",
  z = "estimate divided by its standard error"
)

nomo_report_glossary_symbols <- c("df", "lhs", "N", "op", "p", "r", "rhs", "z")


nomo_report_abbreviations <- function(labels = character(), text = character()) {
  words <- function(s) unique(unlist(strsplit(as.character(s), "[^A-Za-z0-9]+")))
  in_labels <- words(labels)
  in_text <- words(text)
  keys <- names(nomo_report_glossary)
  used <- keys %in% in_labels |
    (keys %in% in_text & !keys %in% nomo_report_glossary_symbols)
  tibble::tibble(
    abbreviation = keys[used],
    meaning = unname(nomo_report_glossary[used])
  )
}


# How to read the report's tables, stated once near its start.
nomo_report_reading_note <- function() {
  paste(
    "Values are rounded for display only; the `nomo_run` object and `nomo_table()`",
    "keep every value as computed. p values have three decimals, and smaller",
    sprintf("ones read \"< .001\". A %s marks a value that is not available.", nomo_report_blank),
    "The Flag column uses one wording for every analysis: blank for no flag,",
    "Review to look at a result again (never an instruction to delete),",
    "Concern for a stronger signal, and Not computed when the evidence could",
    "not be produced. A name written with underscores, such as",
    "`researcher_input`, is a value shown as the object stores it, so it can be",
    "matched in `nomo_table()`. Abbreviations are defined at the end of the",
    "report."
  )
}


nomo_report_validate_run <- function(x) {
  if (!inherits(x, "nomo_run")) {
    stop("`x` must be an object created by `nomo_run()`.", call. = FALSE)
  }

  required <- c(
    "status",
    "sample_design",
    "sample_n",
    "scales",
    "stage_status",
    "results",
    "decision_log",
    "source_data"
  )

  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      sprintf(
        "The `nomo_run` object is missing required field(s): %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


nomo_report_validate_scalar_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be `TRUE` or `FALSE`.", name), call. = FALSE)
  }
  invisible(TRUE)
}


nomo_report_validate_file <- function(file, overwrite) {
  if (!is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !nzchar(trimws(file))) {
    stop("`file` must be one non-empty character path.", call. = FALSE)
  }

  if (!grepl("\\.(html?|docx)$", file, ignore.case = TRUE)) {
    stop(
      paste(
        "nomologR renders HTML or Word reports; `file` must end in `.html`,",
        "`.htm`, or `.docx`."
      ),
      call. = FALSE
    )
  }

  if (file.exists(file) && !isTRUE(overwrite)) {
    stop(
      sprintf(
        "Report file already exists: %s. Use `overwrite = TRUE` to replace it.",
        file
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


nomo_report_template_path <- function(
    installed = system.file(
      "rmarkdown",
      "nomo-report.Rmd",
      package = "nomologR"
    ),
    development = file.path(
      "inst",
      "rmarkdown",
      "nomo-report.Rmd"
    )) {

  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }

  if (file.exists(development)) {
    return(development)
  }

  stop(
    "Could not locate the packaged `nomo_report()` R Markdown template.",
    call. = FALSE
  )
}


nomo_report_component_list <- function(x, stage) {
  if (!stage %in% names(x$results)) return(list())

  obj <- x$results[[stage]]
  if (is.null(obj)) return(list())

  if (stage %in% c("screen", "factors", "efa")) {
    if (!is.list(obj) || !length(obj)) return(list())
    if (is.null(names(obj))) {
      names(obj) <- paste0("scope_", seq_along(obj))
    }
    return(obj)
  }

  stats::setNames(list(obj), "workflow")
}


nomo_report_component_summary_text <- function(obj) {
  if (is.null(obj)) return("No component result is available.")

  # Printed at the 80 columns the console style is written for, so the archive
  # does not depend on the width of the console that rendered it (#144).
  old <- options(width = 80L)
  on.exit(options(old), add = TRUE)

  tryCatch(
    paste(
      utils::capture.output(print(summary(obj))),
      collapse = "\n"
    ),
    error = function(e) {
      tryCatch(
        paste(utils::capture.output(print(obj)), collapse = "\n"),
        error = function(e2) {
          paste0("Summary unavailable: ", conditionMessage(e2))
        }
      )
    }
  )
}


nomo_report_data_characteristics <- function(x) {
  roles <- tryCatch(
    nomo_run_data_roles(x$source_data),
    error = function(e) NULL
  )

  if (is.null(roles)) {
    return(nomo_report_empty_table())
  }

  all_items <- unique(unlist(x$scales, use.names = FALSE))

  describe <- function(dat, role) {
    item_names <- intersect(all_items, names(dat))
    missing_cells <- if (length(item_names)) {
      sum(is.na(dat[item_names]))
    } else {
      0L
    }
    total_cells <- nrow(dat) * length(item_names)

    tibble::tibble(
      role = role,
      n_cases = nrow(dat),
      n_columns = ncol(dat),
      candidate_items = length(item_names),
      candidate_item_missing_cells = missing_cells,
      candidate_item_missing_pct = if (total_cells > 0L) {
        missing_cells / total_cells
      } else {
        NA_real_
      }
    )
  }

  if (identical(roles$design, "calibration_validation")) {
    dplyr::bind_rows(
      describe(roles$exploratory, "calibration / exploratory"),
      describe(roles$confirmatory, "validation / confirmatory")
    )
  } else {
    describe(roles$exploratory, "same sample")
  }
}


# The content review that supplied a run's scales (#46), or NULL when the
# scales were supplied directly. Status and recommendation are shown exactly as
# contentvalidR wrote them.
nomo_report_content_review <- function(x) {
  h <- x$handoff
  if (is.null(h)) return(NULL)
  p <- h$provenance
  ev <- h$evidence

  keying <- h$keying
  keying_text <- if (!keying$recorded) {
    "The handoff predates keying fields, so reverse keying is not declared."
  } else if (!keying$declared) {
    "Reverse keying was not declared."
  } else if (!length(keying$reverse)) {
    "Keying was declared with no reverse-keyed item among those carried."
  } else {
    sprintf(
      "Declared reverse-keyed item(s): %s%s.",
      paste(keying$reverse, collapse = ", "),
      if (is.null(keying$scale_range)) {
        ", with no response scale recorded"
      } else {
        sprintf(", on a %g to %g response scale", keying$scale_range[[1L]],
                keying$scale_range[[2L]])
      }
    )
  }

  list(
    summary = paste(
      sprintf(
        paste(
          "Scales and item membership came from content review in %s %s",
          "(workflow: %s; carry rule: %s; method: %s), not from these data.",
          "%d of %s %s carried; %s"
        ),
        p$package, p$package_version, p$workflow, p$keep, p$method,
        sum(ev$carried), nomo_present_count(nrow(ev), "reviewed item"),
        nomo_present_noun(sum(ev$carried), "was", "were"),
        # A revision can reinstate or remove items (#145).
        if (setequal(unlist(x$scales), unlist(h$scales))) {
          "only carried items were analyzed."
        } else {
          "a researcher revision changed which items were analyzed, as the decision log records."
        }
      ),
      keying_text,
      if (length(p$citation)) {
        sprintf("Content-review sources: %s.", paste(p$citation, collapse = "; "))
      }
    ),
    items = tibble::tibble(
      item = as.character(ev$item),
      scale = as.character(ev$scale),
      carried = ev$carried,
      status = as.character(ev$status),
      recommendation = as.character(ev$recommendation),
      judges = ev$n_judges,
      rule = as.character(ev$rule)
    )
  )
}


# The instrument-wide careless-responding screen of a run (#73): a summary,
# one row per index with the rule a source states for it (or none), and the
# log rows the indices wrote.
nomo_report_effort <- function(screen) {
  e <- screen$effort
  limit <- screen$effort_settings$long_string_limit
  long_applied <- !isFALSE(screen$effort_settings$long_string_rule_applied)
  columns <- c(
    "Long-string" = "long_string",
    "Long-string, within-scale mean" = "long_string_mean",
    "Inter-item SD" = "inter_item_sd",
    "Inter-item SD, within-scale mean" = "inter_item_sd_mean",
    "Mahalanobis distance" = "mahalanobis",
    "Even-odd consistency" = "even_odd",
    "Psychometric antonyms" = "antonym_r",
    "Psychometric synonyms" = "synonym_r"
  )
  finite <- function(column) e[[column]][is.finite(e[[column]])]

  indices <- tibble::tibble(
    index = names(columns),
    cases_with_value = vapply(columns, function(column) length(finite(column)),
                              integer(1), USE.NAMES = FALSE),
    median = vapply(columns, function(column) {
      v <- finite(column)
      if (length(v)) stats::median(v) else NA_real_
    }, numeric(1), USE.NAMES = FALSE),
    # The long-string rule is not applied to a short item set, which then has
    # no count of flagged cases, like an index with no stated rule. A screen
    # saved before the setting existed applied the rule.
    cases_flagged = c(if (long_applied) sum(e$flag_long_string) else NA,
                      NA, NA, NA, NA, NA,
                      sum(e$flag_antonym), sum(e$flag_synonym)),
    rule = c(
      if (long_applied) {
        sprintf("a run of %d or more, half the items (Curran, 2016)", as.integer(limit))
      } else {
        sprintf("not applied: fewer than %d items",
                as.integer(screen$effort_settings$long_string_min_items))
      },
      "none stated", "none stated", "none stated", "none stated", "none stated",
      "a positive correlation (Curran, 2016)",
      "a negative correlation (Curran, 2016)"
    )
  )

  # The rows the indices write, including those saying why an index has no
  # value for some or all cases (#145).
  log <- screen$decision_log
  log <- log[log$metric %in% c("long_string", "mahalanobis", "even_odd",
                               "per_scale_indices", "psychometric_antonym",
                               "psychometric_synonym", "index_disagreement"), ,
             drop = FALSE]

  list(
    summary = sprintf(
      paste(
        "Computed once across all %d items for %d cases, using %s.",
        "%s %s flagged by at least one rule a source states. Cases",
        "are flagged, never removed, and the indices disagree by design, so",
        "read them together."
      ),
      length(screen$items), nrow(e),
      nomo_present_count(length(screen$effort_settings$scales), "scale"),
      nomo_present_count(sum(e$n_flags > 0L), "case"),
      nomo_present_noun(sum(e$n_flags > 0L), "was", "were")
    ),
    indices = indices,
    log = tibble::as_tibble(log)
  )
}


# Scores computed within a run (#73): what they are, Grice's criteria, the
# parallel-model test a unit weight implies, and the notes.
nomo_report_scores <- function(scores) {
  parallel <- scores$parallel_test
  parallel_text <- if (isTRUE(parallel$available)) {
    sprintf(
      paste(
        "The parallel model that unit weighting assumes was compared with the",
        "fitted model: %s."
      ),
      nomo_present_chisq(parallel$chisq_diff, parallel$df_diff, parallel$p_value,
                         delta = TRUE)
    )
  } else if (nzchar(parallel$note)) {
    parallel$note
  }

  list(
    summary = paste(c(
      sprintf(
        "%s-weighted scores (method: %s) for %s and %s, computed from the fitted measurement model.",
        if (identical(scores$weighting, "unit")) "Unit" else "Model",
        scores$method, nomo_present_count(nrow(scores$scores), "case"),
        nomo_present_count(nrow(scores$diagnostics), "factor")
      ),
      parallel_text,
      "No value here is a pass/fail threshold."
    ), collapse = " "),
    diagnostics = scores$diagnostics,
    notes = scores$notes
  )
}


# Missing-data sensitivity computed within a run (#73).
nomo_report_missing <- function(m) {
  p <- m$pattern
  strategies <- m$strategies[, c(
    "label", "lavaan_missing", "requires", "role", "available", "n_used",
    "converged", "admissible"
  )]
  compared <- m$estimates[m$estimates$role == "comparison" &
                            is.finite(m$estimates$difference_in_se), , drop = FALSE]
  compared <- compared[order(-abs(compared$difference_in_se)), , drop = FALSE]
  differences <- tibble::tibble(
    parameter = compared$parameter,
    strategy = nomo_missing_label(compared$strategy),
    estimate = compared$estimate,
    reference = compared$reference_estimate,
    difference_in_se = compared$difference_in_se
  )

  # The reference is fitted only when some case is missing a modeled variable;
  # the summary and the empty-table message say which applies (#145).
  reference_fitted <- any(m$strategies$role == "reference" & m$strategies$available)
  reference <- nomo_missing_label_inline(m$reference)
  summary <- if (p$n_incomplete == 0L) {
    sprintf(
      paste(
        "None of the %d cases is missing a modeled variable, so every strategy",
        "uses the same cases: the reference strategy (%s) was not fitted, and",
        "there are no differences to compare."
      ),
      p$n_cases, reference
    )
  } else {
    paste(
      sprintf(
        "%d of %d cases (%s) are missing at least one modeled variable.",
        p$n_incomplete, p$n_cases,
        nomo_present_percent(p$pct_incomplete, base = p$n_cases)
      ),
      if (reference_fitted) {
        sprintf("The reference is %s.", reference)
      } else {
        sprintf("The reference strategy (%s) could not be fitted.", reference)
      },
      "Differences are in units of the reference standard error; Schafer and",
      "Graham (2002) treat a bias beyond about half a standard error as",
      "practically important. Whether data are missing at random cannot be",
      "tested from the data at hand."
    )
  }

  # A strategy by the name the Strategies table gives it, not its lavaan code
  # ("FIML", not "ml"), as in the differences above.
  named <- function(tab) {
    if ("strategy" %in% names(tab)) tab$strategy <- nomo_missing_label(tab$strategy)
    tab
  }

  list(
    summary = summary,
    strategies = strategies,
    differences = differences,
    differences_empty = if (reference_fitted) {
      "No comparison strategy was fitted."
    } else {
      "The reference strategy was not fitted, so there are no differences to show."
    },
    fit = named(m$fit),
    reliability = named(m$reliability),
    log = m$decision_log
  )
}


# The APA tables for the results a run holds (#73), numbered in report order.
# A table that does not apply, such as factor correlations for a one-factor
# model, is left out rather than failing the report.
nomo_report_apa_tables <- function(x) {
  r <- x$results
  # The validity stage's pair table gives the same latent correlations with the
  # heterotrait-monotrait ratios beside them, so it takes the place of the CFA's
  # factor correlations when the run has one.
  factor_correlations <- if (is.null(r$validity)) r$cfa
  requests <- list(
    list(obj = r$cfa, type = "loadings"),
    list(obj = r$cfa, type = "fit"),
    list(obj = factor_correlations, type = "factor_correlations"),
    list(obj = r$reliability, type = NULL),
    list(obj = r$validity, type = "convergent"),
    list(obj = r$validity, type = "discriminant"),
    list(obj = r$invariance, type = NULL),
    list(obj = r$network, type = "hypotheses"),
    list(obj = r$network, type = "fit")
  )
  tables <- list()
  for (req in requests) {
    if (is.null(req$obj)) next
    tab <- tryCatch(
      nomo_apa_table(req$obj, type = req$type, number = length(tables) + 1L),
      error = function(e) NULL
    )
    if (!is.null(tab)) tables[[length(tables) + 1L]] <- tab
  }
  tables
}


nomo_report_calls <- function(x) {
  calls <- x$call_history
  if (is.null(calls) || !length(calls)) {
    calls <- if (!is.null(x$call)) list(x$call) else list()
  }
  calls
}


# The calls as R code for a code block, which pandoc leaves as written: in a
# table cell its straight quotes became typographic ones, so a copied call did
# not parse (#145). Several calls are numbered by comments.
nomo_report_call_code <- function(x) {
  calls <- nomo_report_calls(x)
  if (!length(calls)) return("")
  # deparse() breaks a line only after the cutoff, so 60 keeps most lines
  # within 80 columns.
  code <- vapply(calls, function(z) {
    paste(deparse(z, width.cutoff = 60L), collapse = "\n")
  }, character(1))
  if (length(code) > 1L) code <- paste0("# Step ", seq_along(code), "\n", code)
  paste(code, collapse = "\n\n")
}


nomo_report_run_table <- function(x, type) {
  tryCatch(
    nomo_report_flatten_table(nomo_table(x, type)),
    error = function(e) nomo_report_empty_table()
  )
}


# The nomo_table() type each measurement stage's report section leads with.
nomo_report_table_defaults <- function() {
  c(
    factors = "evidence",
    efa = "items",
    cfa = "fit",
    reliability = "coefficients",
    validity = "convergent"
  )
}


nomo_report_component_table <- function(obj,
                                        stage,
                                        type = "primary") {
  if (is.null(obj)) return(nomo_report_empty_table())

  result <- tryCatch(
    {
      if (identical(stage, "screen")) {
        switch(
          type,
          primary = obj$item_summary,
          relationships = obj$relationship_summary,
          cases = obj$case_summary,
          decision_log = obj$decision_log,
          nomo_report_empty_table()
        )
      } else if (stage %in% names(nomo_report_table_defaults())) {
        # The same tables nomo_table() returns; a type the object does not
        # offer is an error, caught below, and gives an empty table.
        if (identical(type, "primary")) type <- nomo_report_table_defaults()[[stage]]
        nomo_table(obj, type)
      } else if (identical(stage, "invariance")) {
        switch(
          type,
          primary = nomo_table(obj, "fit"),
          categories = nomo_table(obj, "categories"),
          partial = nomo_table(obj, "partial"),
          local_strain = nomo_table(obj, "local_strain"),
          decision_log = nomo_table(obj, "decision_log"),
          nomo_report_empty_table()
        )
      } else if (identical(stage, "network")) {
        switch(
          type,
          primary = nomo_table(obj, "hypotheses"),
          fit = nomo_table(obj, "fit"),
          measurement = nomo_table(obj, "measurement"),
          relations = nomo_table(obj, "relations"),
          replication = nomo_table(obj, "replication"),
          decision_log = nomo_table(obj, "decision_log"),
          nomo_report_empty_table()
        )
      } else {
        nomo_report_empty_table()
      }
    },
    error = function(e) nomo_report_empty_table()
  )

  nomo_report_flatten_table(result)
}


nomo_report_evidence_trace <- function(x) {
  log <- tryCatch(
    nomo_run_component_logs(x),
    error = function(e) tibble::tibble()
  )

  if (!nrow(log)) {
    return(tibble::tibble(
      evidence_id = character(),
      pipeline_component = character(),
      pipeline_scope = character(),
      stage = character(),
      object = character(),
      metric = character(),
      value = character(),
      reference = character(),
      severity = character(),
      observation = character(),
      recommendation = character()
    ))
  }

  log <- nomo_report_flatten_table(log)
  log$evidence_id <- sprintf("E%04d", seq_len(nrow(log)))

  preferred <- c(
    "evidence_id",
    "pipeline_component",
    "pipeline_scope",
    "stage",
    "object",
    "metric",
    "value",
    "reference",
    "severity",
    "observation",
    "recommendation"
  )

  preferred <- intersect(preferred, names(log))
  rest <- setdiff(names(log), preferred)
  log[, c(preferred, rest), drop = FALSE]
}


nomo_report_flagged_trace <- function(x) {
  trace <- nomo_report_evidence_trace(x)
  if (!nrow(trace) || !"severity" %in% names(trace)) {
    return(nomo_report_empty_table())
  }

  nomo_report_flagged_rows(trace)
}


# The review and concern rows of a log, concern first (guide point 22), each
# group in the log's order.
nomo_report_flagged_rows <- function(log) {
  if (!nrow(log) || !"severity" %in% names(log)) return(log)
  severity <- tolower(log$severity)
  log <- log[severity %in% c("review", "concern"), , drop = FALSE]
  log[order(tolower(log$severity) != "concern"), , drop = FALSE]
}


nomo_report_deviations <- function(x) {
  rows <- list()
  cursor <- 0L

  inv <- x$results$invariance
  if (!is.null(inv)) {
    partial <- tryCatch(
      nomo_table(inv, "partial"),
      error = function(e) tibble::tibble()
    )

    if (nrow(partial)) {
      partial <- nomo_report_flatten_table(partial)
      for (i in seq_len(nrow(partial))) {
        cursor <- cursor + 1L
        detail_cols <- intersect(
          c("level", "syntax", "constraint", "release"),
          names(partial)
        )
        rationale_col <- intersect(
          c("rationale", "reason"),
          names(partial)
        )

        rows[[cursor]] <- tibble::tibble(
          type = "researcher-specified partial invariance",
          stage = "invariance",
          scope = if ("level" %in% names(partial)) {
            as.character(partial$level[[i]])
          } else {
            ""
          },
          # "level: scalar; syntax: ag3 ~ 1", with the syntax as written.
          detail = paste(
            vapply(
              detail_cols,
              function(nm) paste0(nm, ": ", as.character(partial[[nm]][[i]])),
              character(1)
            ),
            collapse = "; "
          ),
          rationale = if (length(rationale_col)) {
            as.character(partial[[rationale_col[[1L]]]][[i]])
          } else {
            ""
          }
        )
      }
    }
  }

  net <- x$results$network
  if (!is.null(net) && !is.null(net$hypothesis_evidence)) {
    h <- nomo_report_flatten_table(net$hypothesis_evidence)

    if (nrow(h) && "confirmatory_status" %in% names(h)) {
      idx <- grepl(
        "post",
        as.character(h$confirmatory_status),
        ignore.case = TRUE
      )

      for (i in which(idx)) {
        cursor <- cursor + 1L
        rows[[cursor]] <- tibble::tibble(
          type = "post hoc nomological relation",
          stage = "network",
          scope = if ("relation" %in% names(h)) {
            as.character(h$relation[[i]])
          } else {
            ""
          },
          # In the words the report's tables use: "post hoc exploratory;
          # concordance: Concordant".
          detail = paste(c(
            nomo_present_origin(h$confirmatory_status[[i]]),
            if ("concordance" %in% names(h)) {
              paste("concordance:", nomo_network_pretty_status(h$concordance[[i]]))
            }
          ), collapse = "; "),
          rationale = ""
        )
      }
    }
  }

  if (inherits(x$decision_log, "data.frame") && nrow(x$decision_log)) {
    dl <- x$decision_log
    decision_revise <- if ("decision" %in% names(dl)) {
      tolower(as.character(dl$decision)) == "revise"
    } else {
      rep(FALSE, nrow(dl))
    }
    # A decision id is the package's own prefix, then a colon and the
    # researcher's scale or item name ("scale_definition:PostpartumDepression"),
    # so only the prefix is read (#145).
    id_prefix <- sub(":.*$", "", as.character(dl$id))
    revise <- which(
      decision_revise |
        grepl("post|partial|deviation|revision", id_prefix, ignore.case = TRUE)
    )

    field <- function(name, i) {
      if (name %in% names(dl)) as.character(dl[[name]][[i]]) else ""
    }
    for (i in revise) {
      cursor <- cursor + 1L
      decision <- field("decision", i)
      rows[[cursor]] <- tibble::tibble(
        type = "workflow deviation/revision decision",
        stage = field("stage", i),
        scope = field("scope", i),
        # A row that records no decision, such as the comparison of a revision
        # with its parent model, shows what it observed.
        detail = if (nzchar(decision)) decision else field("observation", i),
        rationale = field("rationale", i)
      )
    }
  }

  if (!length(rows)) {
    return(tibble::tibble(
      type = character(),
      stage = character(),
      scope = character(),
      detail = character(),
      rationale = character()
    ))
  }

  dplyr::bind_rows(rows)
}


nomo_report_lineage <- function(x) {
  lineage <- nomo_run_lineage(x)
  if (!nrow(lineage)) return(tibble::tibble())
  nomo_report_flatten_table(lineage)
}


# A workflow always credits at least the staged workflow and its decision log,
# so neither table below is empty for a nomo_run.
nomo_report_methods <- function(x) {
  methods <- nomo_methods(x)

  nomo_report_flatten_table(
    methods[, c(
      "stage", "method", "lineage", "introduced", "role", "estimand",
      "implemented_by", "engine", "references"
    ), drop = FALSE]
  )
}


# One row per distinct work, not per method-reference pair: a reference list
# should name each source once even when several methods rest on it.
nomo_report_method_references <- function(x) {
  refs <- nomo_methods(x, references = TRUE)

  refs <- refs[!duplicated(refs$citation_key), , drop = FALSE]
  refs <- refs[order(refs$citation), , drop = FALSE]
  refs$doi <- ifelse(
    is.na(refs$doi),
    "",
    paste0("https://doi.org/", refs$doi)
  )

  nomo_report_flatten_table(refs[, c("citation", "doi"), drop = FALSE])
}


nomo_report_namespace_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}


# Rendering the report from inside another knitr document (a thesis chapter,
# say) nests two renders that share knitr's global state. Two things go wrong
# without this, and the second is silent:
#
#   1. The outer document's chunk labels are already registered, so the
#      template's own labels collide and the render stops with "Duplicate
#      chunk label".
#   2. The nested render inherits the outer document's chunk options. An outer
#      `dev = "svg"`, for example, removes every figure from the report while
#      it still renders and reports success.
#
# Chunk options are therefore reset to knitr's defaults for the report, which
# sets its own options in its template, and the caller's options are restored
# afterwards. rmarkdown::render() restores chunk options on exit itself, so
# that half of the restore is a guarantee rather than the only protection; the
# duplicate-label option is not one it touches.
#
# Returns a function that puts the caller's knitr state back; the caller runs
# it on exit. Outside a knit it changes nothing and the restore is a no-op.
nomo_report_isolate_knitr <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) {
    return(invisible(function() invisible(NULL)))
  }

  previous_options <- options(knitr.duplicate.label = "allow")
  saved_chunk_options <- knitr::opts_chunk$get()
  knitr::opts_chunk$restore()

  invisible(function() {
    options(previous_options)
    knitr::opts_chunk$restore(saved_chunk_options)
    invisible(NULL)
  })
}


nomo_report_pandoc_available <- function() {
  rmarkdown::pandoc_available()
}


nomo_report_package_versions <- function(
    packages = c(
      "nomologR",
      "psych",
      "lavaan",
      "semTools",
      "EFAtools",
      "rmarkdown",
      "knitr"
    ),
    namespace_available = nomo_report_namespace_available,
    version_fun = utils::packageVersion) {
  tibble::tibble(
    package = packages,
    version = vapply(
      packages,
      function(pkg) {
        if (!isTRUE(namespace_available(pkg))) return("not installed")
        as.character(version_fun(pkg))
      },
      character(1)
    )
  )
}


nomo_report_sanitize_citation_text <- function(x) {
  x <- as.character(x)

  if (!length(x)) return(character())

  x <- gsub("[\r\n\t]+", " ", x, perl = TRUE)
  x <- gsub(
    "<(https?://[^<>[:space:]]+)>",
    "\\1",
    x,
    perl = TRUE
  )
  # LaTeX markup some packages put in their citations, such as
  # \texttt{semTools}, keeps only its text.
  x <- gsub("\\\\[A-Za-z]+\\{([^{}]*)\\}", "\\1", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  # R writes a DOI twice, "doi:10.x/y https://doi.org/10.x/y", and pandoc links
  # the first form to an address no browser opens; some packages add the URL
  # once more. Each DOI is given once, as its https://doi.org/ address (#145).
  x <- gsub("(^|[[:space:]])doi:(10\\.[^[:space:]]+?)[.,;]?[[:space:]]+(https?://doi\\.org/\\2)",
            "\\1\\3", x, perl = TRUE)
  x <- gsub("(^|[[:space:]])doi:(10\\.[^[:space:]]+?)([.,;]?)(?=[[:space:]]|$)",
            "\\1https://doi.org/\\2\\3", x, perl = TRUE)
  x <- gsub("(https?://[^[:space:]]+?)([.,;]?)([[:space:]]+\\1[.,;]?)+(?=[[:space:]]|$)",
            "\\1\\2", x, perl = TRUE)
  trimws(x)
}


nomo_report_citations <- function(
    packages = c("nomologR", "psych", "lavaan", "semTools", "EFAtools"),
    namespace_available = nomo_report_namespace_available,
    citation_fun = utils::citation,
    version_fun = utils::packageVersion) {
  rows <- lapply(packages, function(pkg) {
    if (!isTRUE(namespace_available(pkg))) {
      return(tibble::tibble(
        package = pkg,
        installed_version = "not installed",
        citation = "Package not installed in the rendering session."
      ))
    }

    # The reference only. print() adds a "To cite" header and a BibTeX entry,
    # whose "@Manual{" pandoc reads as a citation key (#89).
    citation_text <- tryCatch(
      paste(format(suppressWarnings(citation_fun(pkg)), style = "text"), collapse = " "),
      error = function(e) {
        paste0("Citation unavailable: ", conditionMessage(e))
      }
    )
    citation_text <- nomo_report_sanitize_citation_text(citation_text)

    tibble::tibble(
      package = pkg,
      installed_version = as.character(version_fun(pkg)),
      citation = citation_text
    )
  })

  dplyr::bind_rows(rows)
}


nomo_report_session_text <- function() {
  paste(utils::capture.output(utils::sessionInfo()), collapse = "\n")
}


nomo_report_safe_plot <- function(obj, type = NULL) {
  if (is.null(obj)) {
    return(list(ok = FALSE, plot = NULL, message = "No component result is available."))
  }

  tryCatch(
    {
      p <- if (is.null(type)) {
        plot(obj)
      } else {
        plot(obj, type = type)
      }

      list(ok = TRUE, plot = p, message = "")
    },
    error = function(e) {
      list(
        ok = FALSE,
        plot = NULL,
        message = conditionMessage(e)
      )
    }
  )
}


nomo_report_prepare_template <- function(template, input, title) {
  copied <- file.copy(template, input, overwrite = TRUE)
  if (!isTRUE(copied)) {
    stop("Could not prepare the temporary report template.", call. = FALSE)
  }

  lines <- readLines(input, warn = FALSE, encoding = "UTF-8")
  marker <- 'title: "__NOMO_REPORT_TITLE__"'
  hits <- which(lines == marker)

  if (length(hits) != 1L) {
    stop(
      "The packaged report template does not contain exactly one title marker.",
      call. = FALSE
    )
  }

  lines[[hits]] <- paste0("title: ", nomo_report_title_yaml(title))

  writeLines(lines, input, useBytes = TRUE)
  invisible(input)
}


# The title as a YAML string that the render shows exactly as given (#145).
# Three readers see it: knitr runs inline code (`r ...`), YAML reads escapes,
# and pandoc reads markdown, so "C:\Users" lost its backslashes and <b> became
# markup. Every ASCII punctuation mark becomes a numeric character reference,
# which pandoc reads as that character: knitr finds no backtick, YAML no quote
# or backslash, and pandoc no markup. The result is quoted for YAML.
nomo_report_title_yaml <- function(title) {
  chars <- strsplit(title, "", fixed = TRUE)[[1L]]
  punct <- grepl("^[[:punct:]]$", chars) & chars %in% rawToChar(as.raw(32:126), multiple = TRUE)
  chars[punct] <- sprintf("&#%d;", vapply(chars[punct], utf8ToInt, integer(1)))
  encodeString(paste(chars, collapse = ""), quote = '"')
}


#' Render a reproducible nomologR analysis report
#'
#' `nomo_report()` renders a self-contained HTML document from a `nomo_run`
#' object. The report archives researcher inputs, sample roles, item and factor
#' evidence, EFA/CFA results, reliability, convergent/discriminant evidence,
#' optional invariance and nomological-network results, researcher decisions,
#' deviations and post hoc decisions, method citations, an evidence trace, and
#' session information.
#'
#' The report is a presentation and provenance layer. It does not refit models,
#' alter data, free parameters, remove items, or manufacture additional
#' statistical conclusions.
#'
#' Its tables are the ones [nomo_table()] returns, with values rounded for
#' display by the rules printed output follows (p values to three decimals and
#' "< .001" below that, proportions stored as `pct_*` shown as percentages,
#' counts as whole numbers), flags in the shared wording ("Review", "Concern",
#' "Not computed", blank for no flag), and each cell shown exactly as written,
#' so lavaan syntax such as `~~` and `a*x1` survives. The workflow's calls are
#' printed as R code, and a table at the end defines every abbreviation the
#' report shows.
#'
#' Reports can be rendered from complete, paused, or blocked `nomo_run` objects.
#' Incomplete stages are labeled as such rather than silently omitted.
#'
#' `nomo_report()` writes one file, the report named by `file`, and creates
#' that file's directory when it does not exist. The working files of the
#' render (the copy of the template, the intermediate files, and the figures)
#' are written to a temporary directory under [tempdir()] and removed when the
#' call returns, whether or not it succeeds. The working directory, the
#' options, and the graphics device are left as they were. The report is
#' rendered in an environment of its own, so objects in the global environment
#' are neither read nor changed.
#'
#' `nomo_report()` behaves the same from the console, a script, or a chunk
#' inside another R Markdown or Quarto document, such as a thesis chapter.
#' Rendering from inside a document nests two renders that share knitr's global
#' state, so the report is rendered with knitr's default chunk options, which
#' its own template then sets, and the calling document's options are restored
#' afterwards. The calling document's chunk options therefore cannot change the
#' report, and rendering the report does not change the calling document.
#'
#' @param x An object created by [nomo_run()].
#' @param file Output path. Required: `nomo_report()` has no default path, so
#'   it writes a report only where you ask, and stops with an error when `file`
#'   is not given. The extension chooses the format: `.html` or
#'   `.htm` for a self-contained HTML report, `.docx` for a Word document. The
#'   Word report carries the same tables, figures, and interpretation contract
#'   as the HTML one; collapsible sections are shown expanded, and it uses
#'   Word's default styles.
#' @param title Report title.
#' @param include_plots Logical; include a compact set of component plots when
#'   those plots are available.
#' @param include_session Logical; include full `sessionInfo()` output.
#' @param max_table_rows Positive integer used for ordinary display tables.
#'   The final evidence-trace appendix is not truncated.
#' @param overwrite Logical; replace an existing `file`.
#' @param quiet Logical passed to [rmarkdown::render()].
#' @param apa_tables Logical; if `TRUE`, append a *Manuscript tables* appendix
#'   with the [nomo_apa_table()] tables for the results the run holds. These
#'   are the CFA loadings and fit; reliability; the validity stage's average
#'   variance extracted and construct pairs (without a validity stage, the CFA
#'   factor correlations take the pairs' place, after the fit); and, when
#'   present, invariance and the network's hypotheses and fit. They are
#'   numbered in that order. Default `FALSE`, which leaves the report unchanged.
#'
#' @return The normalized report file path, invisibly.
#'
#' @examples
#' \donttest{
#' # Rendering requires the rmarkdown package and pandoc, which RStudio and
#' # Quarto installations include.
#' if (requireNamespace("rmarkdown", quietly = TRUE) &&
#'     rmarkdown::pandoc_available()) {
#'   run <- nomo_run(
#'     data = nomo_demo_network,
#'     scales = list(Agency = c("ag1", "ag2", "ag3", "ag4")),
#'     settings = list(factors = list(n_iter = 20, seed = 2026)),
#'     decisions = list(
#'       factor_count = 1L,
#'       cfa_model = "Agency =~ ag1 + ag2 + ag3 + ag4",
#'       measurement_model = "proceed"
#'     )
#'   )
#'
#'   report_file <- nomo_report(
#'     run,
#'     file = tempfile(fileext = ".html")
#'   )
#'   file.exists(report_file)
#' }
#' }
#'
#' @export
nomo_report <- function(x,
                        file,
                        title = "nomologR reproducible analysis report",
                        include_plots = TRUE,
                        include_session = TRUE,
                        max_table_rows = 50L,
                        overwrite = FALSE,
                        quiet = TRUE,
                        apa_tables = FALSE) {
  nomo_report_validate_run(x)
  nomo_report_validate_scalar_logical(apa_tables, "apa_tables")
  nomo_report_validate_scalar_logical(include_plots, "include_plots")
  nomo_report_validate_scalar_logical(include_session, "include_session")
  nomo_report_validate_scalar_logical(overwrite, "overwrite")
  nomo_report_validate_scalar_logical(quiet, "quiet")
  # No default path: a report is written only where the caller asks, never to
  # the working directory by default (CRAN policy on the user's file space).
  if (missing(file)) {
    stop(
      paste(
        "`file` is required: nomo_report() has no default path, so it writes",
        "only where you ask. Give the path of the report, for example",
        "`file = \"report.html\"`, or `file = \"report.docx\"` for Word."
      ),
      call. = FALSE
    )
  }
  nomo_report_validate_file(file, overwrite)

  if (!is.character(title) ||
      length(title) != 1L ||
      is.na(title) ||
      !nzchar(trimws(title))) {
    stop("`title` must be one non-empty character value.", call. = FALSE)
  }

  if (!is.numeric(max_table_rows) ||
      length(max_table_rows) != 1L ||
      is.na(max_table_rows) ||
      !is.finite(max_table_rows) ||
      max_table_rows < 1 ||
      abs(max_table_rows - round(max_table_rows)) > sqrt(.Machine$double.eps)) {
    stop("`max_table_rows` must be one positive integer.", call. = FALSE)
  }
  max_table_rows <- as.integer(round(max_table_rows))

  if (!nomo_report_namespace_available("rmarkdown")) {
    stop(
      "Rendering a report requires the suggested package `rmarkdown`.",
      call. = FALSE
    )
  }

  if (!nomo_report_namespace_available("knitr")) {
    stop(
      "Rendering a report requires the suggested package `knitr`.",
      call. = FALSE
    )
  }

  if (!nomo_report_pandoc_available()) {
    stop(
      "Pandoc is required to render the report but was not found.",
      call. = FALSE
    )
  }

  template <- nomo_report_template_path()

  output_dir <- dirname(file)
  if (!dir.exists(output_dir)) {
    ok <- dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(ok) && !dir.exists(output_dir)) {
      stop(
        sprintf("Could not create report output directory: %s", output_dir),
        call. = FALSE
      )
    }
  }
  # The full path, taken while the working directory is still the caller's.
  target <- file.path(
    normalizePath(output_dir, winslash = "/", mustWork = TRUE),
    basename(file)
  )

  # The report is the only file written outside tempdir() (CRAN policy on the
  # user's file space). The copy of the template, the intermediate files of
  # knitr and pandoc, and the figures all go to a scratch directory there,
  # which is removed on exit, and the finished report is then copied to `file`.
  # Nothing else is left beside the report, in the working directory, or in the
  # package directory, even when a render stops part-way: rendering straight
  # into the report's directory left the figures there when it did.
  scratch <- tempfile(pattern = "nomologR-report-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

  input <- file.path(scratch, "report.Rmd")
  nomo_report_prepare_template(
    template = template,
    input = input,
    title = title
  )

  restore_knitr <- nomo_report_isolate_knitr()
  on.exit(restore_knitr(), add = TRUE)

  # The template is rendered where it was copied, so its intermediate files
  # and figures are written beside it. `knit_root_dir` keeps the chunks there
  # too when a calling document has set a root directory of its own.
  # rmarkdown::render() and knitr restore the working directory, the options,
  # and the graphics device on exit, so the caller's are left as found.
  output_format <- nomo_report_output_format(file)
  rendered <- rmarkdown::render(
    input = input,
    output_format = output_format,
    output_file = if (is.null(output_format)) "report.html" else "report.docx",
    output_dir = scratch,
    knit_root_dir = scratch,
    params = list(
      run = x,
      report_title = title,
      include_plots = include_plots,
      include_session = include_session,
      apa_tables = apa_tables,
      max_table_rows = max_table_rows,
      generated_at = format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S %Z"
      )
    ),
    envir = nomo_report_render_env(),
    clean = TRUE,
    quiet = quiet
  )

  invisible(nomo_report_deliver(rendered, target))
}


# The finished report, copied from the scratch directory to the path the
# caller asked for. Returns that path, normalized.
nomo_report_deliver <- function(rendered, target) {
  if (!isTRUE(file.copy(rendered, target, overwrite = TRUE))) {
    stop(sprintf("Could not write the report file: %s", target), call. = FALSE)
  }
  normalizePath(target, winslash = "/", mustWork = TRUE)
}


# The environment the template's code runs in. Its parent is the base
# environment rather than the global one: the template calls base R and names
# the package of everything else (`knitr::`, `utils::`, `nomologR:::`), so it
# needs nothing from the caller's workspace. It therefore cannot pick up an
# object there that masks a base function, and whatever it assigns stays in
# this environment, which is discarded; the global environment is neither read
# nor changed (CRAN policy). A test reads the template to check that every
# name it uses is defined there, in base R, or by a package it names.
nomo_report_render_env <- function() {
  new.env(parent = baseenv())
}


# The template's own YAML describes the HTML report, so an HTML file needs no
# format here. A Word file gets word_document(), and the template writes
# markdown instead of raw HTML for it, since pandoc drops raw HTML from .docx.
nomo_report_output_format <- function(file) {
  if (!grepl("\\.docx$", file, ignore.case = TRUE)) return(NULL)
  args <- list(toc = TRUE, fig_width = 8, fig_height = 5.2)
  # Section numbering for Word arrived in a later rmarkdown than this package
  # requires, so it is used only where the installed version supports it.
  if ("number_sections" %in% names(formals(rmarkdown::word_document))) {
    args$number_sections <- TRUE
  }
  do.call(rmarkdown::word_document, args)
}
