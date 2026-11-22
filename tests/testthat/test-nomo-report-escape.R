# The report's escaping, vectorized (#183) --------------------------------------
#
# nomo_report_escape() split every string into characters and escaped it on
# its own, which took a fifth of a report's time. It now escapes a vector at
# once. This is the 1.0.0 version, which every string must still match.

escape_1.0.0 <- function(x, to = c("html", "markdown"), cell = FALSE) {
  to <- match.arg(to)
  html <- identical(to, "html")
  bar_ends_cell <- !html && isTRUE(cell)
  markup <- c("\\", "`", "*", "_", "~", "^", "[", "]", "$", "@", "#", "|",
              "\"", "'", "!", "{", "}", "&", "<", ">")
  number <- "(?:[0-9]+|[A-Za-z]|[ivxlcdm]+|[IVXLCDM]+|&#35;)"
  escape_text <- function(s) {
    chars <- strsplit(s, "", fixed = TRUE)[[1L]]
    special <- chars %in% markup
    chars[special] <- sprintf("&#%d;", vapply(chars[special], utf8ToInt, integer(1)))
    s <- paste(chars, collapse = "")
    s <- gsub("-(?=-)|(?<=-)-", "&#45;", s, perl = TRUE)
    s <- gsub("\\.(?=\\.)|(?<=\\.)\\.", "&#46;", s, perl = TRUE)
    s <- sub("^(\\s*)-(?=\\s|$)", "\\1&#45;", s, perl = TRUE)
    s <- sub("^(\\s*)\\+(?=\\s|$)", "\\1&#43;", s, perl = TRUE)
    s <- sub(paste0("^(\\s*)\\((?=", number, "\\)(?:\\s|$))"), "\\1&#40;", s, perl = TRUE)
    s <- sub(paste0("^(\\s*(?:&#40;)?", number, ")\\.(?=\\s|$)"), "\\1&#46;", s, perl = TRUE)
    s <- sub(paste0("^(\\s*(?:&#40;)?", number, ")\\)(?=\\s|$)"), "\\1&#41;", s, perl = TRUE)
    colons <- attr(regexpr("^\\s*:+", s), "match.length")
    s <- paste0(gsub(":", "&#58;", substring(s, 1L, colons), fixed = TRUE),
                substring(s, colons + 1L))
    gsub("\r?\n", if (html) "<br>" else "; ", s)
  }
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


test_that("the vectorized escape gives the 1.0.0 text for every string (#183)", {
  skip_on_cran()
  escape <- nomologR:::nomo_report_escape
  pieces <- c(
    strsplit("\\`*_~^[]$@#|\"'!{}&<>-.:+()0123456789abcivxlmIVXLCDM ;", "")[[1L]],
    " ", " ", "\n", "\r\n", "\t", "\u00e9", "\u2014", "&#35;", "#35;", "&#38;",
    "``", "`x1 | t1`", "`a_b`", "1.", "(1)", "a)", "iv.", ":::", "--", "..."
  )
  starts <- c(
    "- a", "+ b", "1. c", "-0.5", "1) x", "(a) x", "(1) x", "i. x", "#. x",
    "IV) x", "  2) x", "+", "-", "1.", "a)", "(b)", "(Intercept)", "e.g. x",
    "(n = 5) x", "1.5 x", "a.b", ":::", " ::: x", "a: b", "Scale `Agency` has x_y",
    "`a_b` then c|d", "free `x1 | t1`", "Released `ag3 | t1` and `ag3 | t2` here",
    "`", "``", "a`b", "`x` `y`", "&&##", "A =~ a1 + a2\nB ~~ C", "x -- y ... z"
  )
  set.seed(183)
  random <- vapply(seq_len(1500), function(i) {
    paste(sample(pieces, sample(0:20, 1L), replace = TRUE), collapse = "")
  }, character(1))
  corpus <- c(NA, "", starts, random, paste0(sample(starts, 300, TRUE), sample(random, 300)))

  for (to in c("html", "markdown")) {
    for (cell in c(FALSE, TRUE)) {
      expect_identical(escape(corpus, to, cell = cell), escape_1.0.0(corpus, to, cell = cell))
    }
  }
  # The shapes of other inputs are kept too.
  for (x in list(c(a = "x_y", b = "1) z"), character(), NULL, factor(c("a_b", NA)), c(1.5, NA))) {
    expect_identical(escape(x, "html"), escape_1.0.0(x, "html"))
  }
})
