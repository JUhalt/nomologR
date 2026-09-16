# Verify the methods-registry bibliography against the DOI registries.
#
# Run before every release (see the release-certification checklist). For each
# reference with a DOI, this resolves the DOI through doi.org content
# negotiation, which covers both Crossref and DataCite registrations, and then
# checks that the registered metadata matches what the registry cites:
#
#   * the DOI resolves;
#   * the first author's family name matches the start of the citation;
#   * the registered year matches the cited year (one year of tolerance, since
#     online-first and print years often differ);
#   * the registered title matches the cited title after normalizing case,
#     punctuation, and markup.
#
# A well-formed DOI can still point at the wrong work, so syntax checks in the
# test suite are not a substitute for this script. It needs network access and
# is therefore not part of R CMD check.
#
# Usage, from the package root:
#   Rscript dev/verify-method-dois.R
#
# Exit status is non-zero when any reference fails, so the script can gate a
# release step.

for (pkg in c("curl", "jsonlite", "pkgload")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required to verify DOIs.", call. = FALSE)
  }
}

pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
bib <- nomo_bibliography()

# Registered titles can carry markup and line breaks inside a word -- Crossref
# stores "Mplus" as "M <i>plus</i>" across a line break -- so comparison drops
# markup and all non-alphanumeric characters, including whitespace.
normalize <- function(x) {
  x <- tolower(x)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&amp;", "and", x, fixed = TRUE)
  x <- gsub("&", "and", x, fixed = TRUE)
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  gsub("[^a-z0-9]+", "", x)
}

fetch_csl <- function(doi) {
  handle <- curl::new_handle(followlocation = TRUE, timeout = 30)
  curl::handle_setheaders(
    handle,
    Accept = "application/vnd.citationstyles.csl+json",
    `User-Agent` = "nomologR release check (https://github.com/JUhalt/nomologR)"
  )
  response <- tryCatch(
    curl::curl_fetch_memory(paste0("https://doi.org/", doi), handle = handle),
    error = function(e) NULL
  )
  if (is.null(response) || response$status_code != 200L) return(NULL)
  tryCatch(
    jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE),
    error = function(e) NULL
  )
}

csl_year <- function(csl) {
  for (field in c("published-print", "issued", "published-online", "created")) {
    parts <- csl[[field]][["date-parts"]]
    if (length(parts) && length(parts[[1]])) return(as.integer(parts[[1]][[1]]))
  }
  NA_integer_
}

check_one <- function(row) {
  result <- list(
    key = row$key, doi = row$doi, resolves = FALSE, author = NA,
    year = NA, title = NA, registered = ""
  )

  csl <- fetch_csl(row$doi)
  if (is.null(csl)) {
    result$registered <- "DOI did not resolve to citation metadata"
    return(result)
  }
  result$resolves <- TRUE

  citation <- row$citation
  cited_year <- as.integer(sub("^.*?\\((\\d{4})\\).*$", "\\1", citation, perl = TRUE))

  family <- if (length(csl$author)) csl$author[[1]]$family else NA_character_
  if (is.null(family)) family <- NA_character_
  result$author <- !is.na(family) &&
    startsWith(normalize(citation), normalize(family))

  year <- csl_year(csl)
  result$year <- !is.na(year) && !is.na(cited_year) && abs(year - cited_year) <= 1L

  title <- csl$title
  if (is.list(title)) title <- title[[1]]
  title <- if (is.null(title)) NA_character_ else as.character(title)
  result$title <- !is.na(title) && grepl(normalize(title), normalize(citation), fixed = TRUE)

  result$registered <- sprintf(
    "%s (%s). %s",
    if (is.na(family)) "?" else family,
    if (is.na(year)) "?" else year,
    if (is.na(title)) "?" else title
  )
  result
}

with_doi <- bib[!is.na(bib$doi), , drop = FALSE]
without_doi <- bib$key[is.na(bib$doi)]

cat(sprintf("Verifying %d DOIs against doi.org...\n\n", nrow(with_doi)))

results <- lapply(seq_len(nrow(with_doi)), function(i) {
  Sys.sleep(0.2)
  check_one(with_doi[i, ])
})

failed <- 0L
for (r in results) {
  ok <- isTRUE(r$resolves) && isTRUE(r$author) && isTRUE(r$year) && isTRUE(r$title)
  if (!ok) {
    failed <- failed + 1L
    problems <- c(
      if (!isTRUE(r$resolves)) "does not resolve",
      if (isTRUE(r$resolves) && !isTRUE(r$author)) "first author differs",
      if (isTRUE(r$resolves) && !isTRUE(r$year)) "year differs",
      if (isTRUE(r$resolves) && !isTRUE(r$title)) "title differs"
    )
    cat(sprintf("FAIL  %-30s %s\n", r$key, paste(problems, collapse = "; ")))
    cat(sprintf("      doi:        %s\n", r$doi))
    cat(sprintf("      cited:      %s\n", bib$citation[bib$key == r$key]))
    cat(sprintf("      registered: %s\n\n", r$registered))
  }
}

cat(sprintf(
  "\n%d of %d DOIs verified; %d failed.\n",
  length(results) - failed, length(results), failed
))
if (length(without_doi)) {
  cat(
    "Without a DOI (verify by hand):",
    paste(without_doi, collapse = ", "), "\n"
  )
}

if (failed > 0L) quit(status = 1L)
