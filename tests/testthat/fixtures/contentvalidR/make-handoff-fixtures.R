# Generate contentvalidR handoff fixtures for nomologR's reader tests.
#
# Usage, once per producer version, each in its own R process:
#
#   git -C <contentvalidR-repo> worktree add <dir-060> v0.6.0
#   git -C <contentvalidR-repo> worktree add <dir-070> v0.7.0
#   Rscript make-handoff-fixtures.R <dir-060> <out-dir>
#   Rscript make-handoff-fixtures.R <dir-070> <out-dir>
#
# Each run loads one contentvalidR source tree with pkgload and writes that
# version's fixtures as handoff-<fit>-v<version>.rds. Separate processes
# matter: two versions of a package cannot be loaded into one R session.
#
# Every input is written out in this file, so the fixtures do not depend on
# either tag's example data. That is what makes the 0.6.0 and 0.7.0 fixtures
# comparable: the same input, run through two producers. It also lets the
# script run from nomologR's repository, which has none of contentvalidR's data.
#
# The fixtures are genuine producer output, never edited afterwards. One
# consequence: provenance$created records the day they were generated, so a
# regenerated file differs in that field (and in its md5) while every
# statistic stays identical.
#
# Requires pkgload. Everything else is base R and the contentvalidR source.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("usage: Rscript make-handoff-fixtures.R <contentvalidR-source> <out-dir>",
       call. = FALSE)
}
src <- args[[1]]
out <- args[[2]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressMessages(pkgload::load_all(src, quiet = TRUE, export_all = FALSE))
version <- as.character(utils::packageVersion("contentvalidR"))

# Keying and the response scale first appear in 0.7.0. Decided from the
# function itself rather than the version string, so the script says what the
# producer can actually express.
has_instrument <- all(c("reverse_keyed", "response_scale") %in%
                        names(formals(content_handoff)))

# ---------------------------------------------------------------------------
# 1. The walkthrough item sort: twelve items, twenty judges, three constructs.
#    These counts are the ones data-raw/build-walkthrough-data.R ships in
#    contentvalidR 0.7.0 (inst/extdata/walkthrough_sort.csv).
# ---------------------------------------------------------------------------
sort_counts <- list(
  EF1 = c(EF = 19, TF =  1, TA =  0), EF2 = c(EF = 18, TF =  1, TA =  1),
  EF3 = c(EF = 18, TF =  2, TA =  0), EF4 = c(EF = 18, TF =  2, TA =  0),
  EF5 = c(EF = 11, TF =  1, TA =  8), EF6 = c(EF = 15, TF =  3, TA =  2),
  TF1 = c(EF =  1, TF = 19, TA =  0), TF2 = c(EF =  2, TF = 17, TA =  1),
  TF3 = c(EF =  1, TF = 18, TA =  1), TF4 = c(EF =  4, TF = 16, TA =  0),
  TF5 = c(EF = 13, TF =  6, TA =  1), TF6 = c(EF =  2, TF = 18, TA =  0)
)
walkthrough_sort <- do.call(rbind, lapply(names(sort_counts), function(id) {
  counts <- sort_counts[[id]]
  data.frame(item = id, rater = seq_len(sum(counts)),
             target_construct = substr(id, 1, 2),
             assigned_construct = rep(names(counts), times = counts),
             stringsAsFactors = FALSE)
}))
reverse_worded <- c("EF2", "TF2")

sort_fit <- sort_validity(walkthrough_sort)
walkthrough <- if (has_instrument) {
  content_handoff(sort_fit, reverse_keyed = reverse_worded,
                  response_scale = c(1, 5))
} else {
  content_handoff(sort_fit)
}

# ---------------------------------------------------------------------------
# 2. An expert relevance panel with Krippendorff's alpha, so panel_statistics
#    carries a row with a bootstrap interval. Eight experts, five items, the
#    1-4 relevance scale; identical to inst/extdata/expert_relevance_example.csv
#    in both tags.
# ---------------------------------------------------------------------------
relevance <- rbind(
  c(4, 4, 4, 3, 2), c(4, 4, 3, 3, 2), c(4, 4, 4, 3, 3), c(4, 4, 3, 2, 2),
  c(4, 3, 3, 3, 2), c(4, 4, 4, 2, 3), c(4, 4, 3, 3, 2), c(4, 3, 4, 3, 2)
)
colnames(relevance) <- paste0("Item", 1:5)
expert <- content_handoff(
  expert_validity(relevance, mode = "relevance", lo = 1, hi = 4,
                  agreement = "krippendorff", agreement_B = 200, seed = 7)
)

# ---------------------------------------------------------------------------
# 3. A three-round Delphi, built to reach every case a reader must handle:
#    S1  ordinary: rated in all three rounds, kappa defined;
#    S2  unanimous in rounds 2 and 3, so kappa is 0/0 and undefined, although
#        no expert changed their rating;
#    S3  set aside after reaching consensus in round 2, so its last round is 2;
#    S4  rated in round 1 only, so it has no pair of rounds at all;
#    S5  unanimous in round 2 only, so kappa is 0 however many experts kept
#        their rating.
# ---------------------------------------------------------------------------
delphi_long <- function(ratings, round) {
  do.call(rbind, lapply(names(ratings), function(item) {
    data.frame(expert = paste0("E", seq_along(ratings[[item]])), item = item,
               round = round, rating = ratings[[item]],
               stringsAsFactors = FALSE)
  }))
}
delphi_ratings <- rbind(
  delphi_long(list(S1 = c(4, 4, 3, 4, 2, 4, 3, 4), S2 = c(4, 4, 4, 3, 4, 4, 4, 4),
                   S3 = c(3, 4, 4, 3, 4, 3, 4, 4), S4 = c(2, 1, 2, 3, 1, 2, 2, 1),
                   S5 = c(3, 2, 3, 3, 2, 3, 2, 3)), 1),
  delphi_long(list(S1 = c(4, 4, 4, 4, 3, 4, 4, 4), S2 = rep(4, 8),
                   S3 = c(4, 4, 4, 4, 4, 3, 4, 4), S5 = rep(3, 8)), 2),
  delphi_long(list(S1 = c(4, 4, 4, 4, 3, 4, 4, 4), S2 = rep(4, 8),
                   S5 = c(3, 3, 4, 3, 3, 3, 4, 3)), 3)
)
delphi <- content_handoff(
  delphi_validity(delphi_ratings, lo = 1, hi = 4, consensus_threshold = 0.75,
                  B = 200, seed = 11)
)

# ---------------------------------------------------------------------------
fixtures <- list(`walkthrough-sort` = walkthrough,
                 `expert-krippendorff` = expert,
                 delphi = delphi)
for (fit in names(fixtures)) {
  path <- file.path(out, sprintf("handoff-%s-v%s.rds", fit, version))
  saveRDS(fixtures[[fit]], path)
  cat(sprintf("wrote %s\n", basename(path)))
}
