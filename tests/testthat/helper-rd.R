# Help pages for tests that hold the documentation to the code (#145). The
# frozen interface is what the help pages say, so a value the code accepts or
# returns must be named there.
#
# From the source tree the Rd files in man/ are read; elsewhere, as under
# R CMD check or covr, the installed package's help database is.
nomo_test_rd <- function(topic) {
  file <- paste0(topic, ".Rd")
  source_file <- testthat::test_path("..", "..", "man", file)
  if (file.exists(source_file)) {
    return(tools::parse_Rd(source_file))
  }
  tools::Rd_db("nomologR")[[file]]
}


# One part of a help page as Rd source with its white space collapsed: a
# top-level section such as "\\value" or "\\details", or a titled
# "\\section{}".
nomo_test_rd_text <- function(topic, section) {
  rd <- nomo_test_rd(topic)
  tags <- vapply(rd, attr, character(1), "Rd_tag")
  if (startsWith(section, "\\")) {
    parts <- rd[tags == section]
  } else {
    titled <- rd[tags == "\\section"]
    titles <- vapply(titled, function(s) {
      paste(as.character(s[[1L]]), collapse = "")
    }, character(1))
    parts <- titled[titles == section]
  }
  text <- paste(
    vapply(parts, function(p) {
      paste(as.character(structure(list(p), class = "Rd"), deparse = TRUE), collapse = "")
    }, character(1)),
    collapse = " "
  )
  gsub("\\s+", " ", text)
}


# The bullets of an Rd itemized list, each as Rd source.
nomo_test_rd_items <- function(text) {
  trimws(strsplit(text, "\\item ", fixed = TRUE)[[1L]][-1L])
}
