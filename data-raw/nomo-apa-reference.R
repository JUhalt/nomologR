# Builds inst/rmarkdown/nomo-apa-reference.docx, the Word reference document
# that nomo_report() uses for a Word report and ?nomo_apa_table offers for a
# researcher's own Word documents (#145).
#
# The document is pandoc's default reference.docx with one change: its "Table"
# style, the style pandoc gives every table it writes, draws the rules of an
# APA 7 table. Pandoc's default draws only the rule beneath the column
# headings. This style adds a rule above the table and one below it, and
# draws nothing between rows or columns. Everything else is pandoc's default.
#
# Run from the package root:
#   Rscript data-raw/nomo-apa-reference.R
#
# It needs pandoc, found as rmarkdown finds it, and a zip program for
# utils::zip() (Rtools has one; on other systems, set R_ZIPCMD).

local({
  pandoc <- rmarkdown::pandoc_exec()
  version <- as.character(rmarkdown::pandoc_version())
  target <- file.path(normalizePath(file.path("inst", "rmarkdown"), mustWork = TRUE),
                      "nomo-apa-reference.docx")

  work <- tempfile("nomo-apa-reference-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  default <- file.path(work, "default.docx")
  status <- system2(pandoc, c("-o", shQuote(default), "--print-default-data-file",
                              "reference.docx"))
  stopifnot(identical(status, 0L), file.exists(default))

  parts <- file.path(work, "parts")
  utils::unzip(default, exdir = parts)
  entries <- utils::unzip(default, list = TRUE)$Name

  styles_file <- file.path(parts, "word", "styles.xml")
  styles <- readChar(styles_file, file.size(styles_file), useBytes = TRUE)

  # The "Table" style, from its opening tag to its closing one. Each change
  # replaces one exact piece of it and stops if that piece is not there once,
  # so the default of a new pandoc is never edited blindly. pandoc's file has
  # Windows line endings, which the new lines follow.
  open_tag <- '<w:style w:type="table" w:default="1" w:styleId="Table">'
  start <- regexpr(open_tag, styles, fixed = TRUE)
  stopifnot(start > 0L)
  size <- regexpr("</w:style>", substring(styles, start), fixed = TRUE) +
    nchar("</w:style>") - 1L
  table <- substr(styles, start, start + size - 1L)
  nl <- if (grepl("\r\n", table, fixed = TRUE)) "\r\n" else "\n"

  replace_once <- function(text, old, new) {
    at <- gregexpr(old, text, fixed = TRUE)[[1L]]
    if (length(at) != 1L || at[[1L]] < 0L) {
      stop("The Table style of pandoc ", version, "'s reference.docx does not ",
           "have this piece once: ", old, call. = FALSE)
    }
    sub(old, new, text, fixed = TRUE)
  }

  rule <- 'w:val="single" w:sz="4" w:space="0" w:color="auto"'

  # A rule above the table and one below it, and none between rows or
  # columns. In a table's properties, w:tblBorders follows w:tblInd.
  indent <- '<w:tblInd w:w="0" w:type="dxa" />'
  table <- replace_once(table, indent, paste0(
    indent,
    nl, "      <w:tblBorders>",
    nl, "        <w:top ", rule, " />",
    nl, '        <w:left w:val="nil" />',
    nl, "        <w:bottom ", rule, " />",
    nl, '        <w:right w:val="nil" />',
    nl, '        <w:insideH w:val="nil" />',
    nl, '        <w:insideV w:val="nil" />',
    nl, "      </w:tblBorders>"
  ))

  # The rule beneath the column headings, which pandoc's default draws, at the
  # weight of the other two.
  table <- replace_once(table, '<w:bottom w:val="single"/>', paste0("<w:bottom ", rule, " />"))

  styles <- paste0(substr(styles, 1L, start - 1L), table,
                   substring(styles, start + size))

  con <- file(styles_file, open = "wb")
  writeChar(styles, con, eos = NULL, useBytes = TRUE)
  close(con)

  # The parts go back in pandoc's order, [Content_Types].xml first, with no
  # directory entries and no extra file attributes.
  out <- file.path(work, "nomo-apa-reference.docx")
  old_wd <- setwd(parts)
  status <- tryCatch(utils::zip(out, files = entries, flags = "-X -D -nw -9 -q"),
                     finally = setwd(old_wd))
  stopifnot(identical(as.integer(status), 0L), file.exists(out))

  stopifnot(file.copy(out, target, overwrite = TRUE))
  message("Wrote ", target, " (", file.size(target), " bytes) from pandoc ", version, ".")
})
