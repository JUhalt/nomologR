# Build the shared walkthrough data used by the joint contentvalidR/nomologR
# example (contentvalidR#55, nomologR#53 criterion 5).
#
# The data are SIMULATED. No participant was involved and no real instrument is
# reproduced. The construct, its two facets, and all twelve item stems were
# written for this example. The point of the walkthrough is to carry one item
# set from expert review through empirical screening, so the items are designed
# to disagree with each other in specific, documented ways: see "What each item
# is built to do" below.
#
# This script uses only base R and is deterministic. Run it from the package
# root. It writes three files into inst/extdata:
#
#   walkthrough_items.csv      the item set, and what each item is built to do
#   walkthrough_sort.csv       the expert panel's assignments
#   walkthrough_responses.csv  the response data the second stage screens
#
# nomologR mirrors the response file so that neither package has to depend on
# the other. This script is the single source for both copies, so they cannot
# drift: regenerating here and copying across reproduces them exactly.

set.seed(20260921)

# -----------------------------------------------------------------------------
# The item set
# -----------------------------------------------------------------------------
# Construct: Study Persistence, "the tendency to keep working on academic tasks
# when they become difficult or uninteresting."
#   EF, Effort Regulation: keeping going when the work is dull or hard.
#   TF, Task Focus: staying on the task rather than switching away from it.
# TA, Test Anxiety, is a neighbouring construct that judges can pick instead. It
# is a distractor in the sort and has no items of its own.

items <- data.frame(
  item = c("EF1", "EF2", "EF3", "EF4", "EF5", "EF6",
           "TF1", "TF2", "TF3", "TF4", "TF5", "TF6"),
  facet = c(rep("EF", 6), rep("TF", 6)),
  stem = c(
    "When my coursework gets boring, I keep working on it anyway.",
    "I stop studying once the material stops being interesting.",
    "I finish the assignments that count toward my grade.",
    "I keep to the study schedule I set for myself.",
    "I get tense when I fall behind on coursework.",
    "I finish assignments even when I would rather do something else.",
    "I stay on one task until it is finished.",
    "I switch between tasks while I study.",
    "I keep my attention on the task in front of me.",
    "I keep working through a task without taking breaks.",
    "I work hard to stay on top of my reading.",
    "I put my phone away while I study."
  ),
  role = c(
    "ordinary: the panel places it and it behaves as intended",
    "reverse-worded: behaves as intended once recoded",
    "flagged by an empirical screen and worth keeping anyway",
    "passes content review, then carries almost no common variance",
    "fails content review: the wording pulls judges toward test anxiety",
    "meets the content criterion by one judge, then behaves well",
    "ordinary: the panel places it and it behaves as intended",
    "reverse-worded: behaves as intended once recoded",
    "ordinary: the panel places it and it behaves as intended",
    "passes content review, then loads on both facets",
    "fails content review: a competing facet takes more assignments",
    "behaves differently in the two cohorts"
  ),
  stringsAsFactors = FALSE
)
# Written the other way round. Shipped as a column so a reader can pass it
# straight to content_handoff(reverse_keyed = ...), which is how anything
# downstream learns to recode.
reverse_keyed <- c("EF2", "TF2")
items$reverse_worded <- items$item %in% reverse_keyed

# -----------------------------------------------------------------------------
# What each item is built to do
# -----------------------------------------------------------------------------
# The walkthrough needs items that behave differently at the two stages,
# because the claim it is making is that the two stages can disagree.
#
#   EF1 TF1 TF3              ordinary items: the panel places them, and they
#                            behave as intended in the response data.
#   EF2 TF2  REVERSE-WORDED. Ordinary items written the other way round, so a
#        high answer means LESS persistence. Judges sort them exactly as they
#        would the forward version, since sorting is about the construct and
#        not its direction. Once recoded they behave as intended; before
#        recoding, each correlates negatively with its own facet. That negative
#        number is a coding error, not evidence against the item, and telling
#        the two apart is why the handoff records keying at all.
#   EF3  FLAGGED EMPIRICALLY AND WORTH KEEPING ANYWAY. Finishing graded work is
#        part of persistence and almost everyone endorses it, so its answers
#        pile up at the top of the scale. Restricted variance attenuates any
#        correlation it can have with anything, so a screen keyed on item-total
#        correlation flags it -- and it is the only item covering the completion
#        of required work, so dropping it narrows the domain the panel defined.
#        This is the reverse of EF4 and the sharper of the two cases: the
#        empirical stage is also not the final word.
#   EF4  PASSES CONTENT REVIEW AND FAILS EMPIRICALLY. Judges read "keep to the
#        study schedule I set" as effort regulation and sort it there, 18 of 20.
#        It is really about planning, so in the response data it carries almost
#        no common variance with the rest of the facet. This is the item the
#        walkthrough is built around: content review cannot see this.
#   EF5  FAILS CONTENT REVIEW. The stem is about tension, so 8 of 20 judges put
#        it under test anxiety. The target still wins, but not by enough to meet
#        the exact criterion, so it is flagged Review.
#   EF6  MEETS THE CRITERION BY ONE JUDGE, at 15 of 20, and then behaves well.
#        A borderline content result is not a prediction of empirical failure.
#   TF4  PASSES CONTENT REVIEW AND IS EMPIRICALLY AMBIGUOUS. "Working through a
#        task without taking breaks" is effort as much as focus, and it loads on
#        both facets. The panel saw one facet; the data see two.
#   TF5  FAILS CONTENT REVIEW THE OTHER WAY. "Work hard to stay on top of my
#        reading" reads as effort, and 13 of 20 judges assign it to EF, so a
#        competing construct beats the target outright.
#   TF6  BEHAVES DIFFERENTLY BY COHORT. Putting a phone away is a specific
#        behaviour, and the two cohorts faced different classroom device rules,
#        so its thresholds are shifted in cohort B. This is what the grouping
#        variable is there for.
#
# Items that fail content review stay in the response file. A reader has to be
# able to see what would have happened if they had been kept.

# -----------------------------------------------------------------------------
# Expert item sort: 20 judges assign each item to EF, TF, or TA
# -----------------------------------------------------------------------------
# sort_validity() uses the exact target-count test with p0 = 0.5, which needs 15
# of 20 assignments to the target. The counts below are chosen against that
# criterion, not fitted to it after the fact.

sort_counts <- list(
  EF1 = c(EF = 19, TF =  1, TA =  0),
  EF2 = c(EF = 18, TF =  1, TA =  1),
  EF3 = c(EF = 18, TF =  2, TA =  0),
  EF4 = c(EF = 18, TF =  2, TA =  0),
  EF5 = c(EF = 11, TF =  1, TA =  8),
  EF6 = c(EF = 15, TF =  3, TA =  2),
  TF1 = c(EF =  1, TF = 19, TA =  0),
  TF2 = c(EF =  2, TF = 17, TA =  1),
  TF3 = c(EF =  1, TF = 18, TA =  1),
  TF4 = c(EF =  4, TF = 16, TA =  0),
  TF5 = c(EF = 13, TF =  6, TA =  1),
  TF6 = c(EF =  2, TF = 18, TA =  0)
)

n_judges <- 20L
stopifnot(all(vapply(sort_counts, sum, numeric(1)) == n_judges))

# Judges are spread across the categories in a fixed order rather than at
# random, so the file is identical on every machine and every R version.
sort_rows <- lapply(items$item, function(id) {
  counts <- sort_counts[[id]]
  data.frame(
    item = id,
    rater = seq_len(n_judges),
    target_construct = items$facet[items$item == id],
    assigned_construct = rep(names(counts), times = counts),
    stringsAsFactors = FALSE
  )
})
walkthrough_sort <- do.call(rbind, sort_rows)

out_dir <- file.path("inst", "extdata")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# The stems and the roles carry commas, so this one file is quoted.
write.csv(items, file.path(out_dir, "walkthrough_items.csv"), row.names = FALSE)
write.csv(walkthrough_sort, file.path(out_dir, "walkthrough_sort.csv"),
          row.names = FALSE, quote = FALSE)

# -----------------------------------------------------------------------------
# Respondent data: 400 respondents, 12 items, 5-point scale, two cohorts
# -----------------------------------------------------------------------------
# The generating model, stated so that anyone reading the walkthrough knows
# exactly what the empirical stage is recovering:
#
#   eta ~ MVN(0, R), R = [[1, .45], [.45, 1]]        two correlated facets
#   y*_j = lambda_EFj * eta_EF + lambda_TFj * eta_TF + e_j
#   e_j  ~ N(0, 1 - communality_j)                   so each y* has variance 1
#   y_j  = the 5-point category y*_j falls into, by the thresholds below
#
# Every loading is listed. Nothing is drawn at random except eta and e.

loadings <- rbind(
  #            EF     TF
  EF1 = c(EF = 0.72, TF = 0.00),
  EF2 = c(0.68, 0.00),
  EF3 = c(0.60, 0.00),
  EF4 = c(0.15, 0.00),   # passes content review, empirically near-noise
  EF5 = c(0.55, 0.00),
  EF6 = c(0.60, 0.00),
  TF1 = c(0.00, 0.70),
  TF2 = c(0.00, 0.66),
  TF3 = c(0.00, 0.62),
  TF4 = c(0.45, 0.40),   # cross-loads: effort as much as focus
  TF5 = c(0.00, 0.58),
  TF6 = c(0.00, 0.64)
)
colnames(loadings) <- c("EF", "TF")
stopifnot(identical(rownames(loadings), items$item))

factor_r <- 0.45
n_respondents <- 400L
thresholds <- stats::qnorm(c(0.10, 0.30, 0.60, 0.85))
# Cohort B faced a classroom rule about phones, which moves TF6 and nothing
# else. The shift is on the thresholds, not on the loading: the item measures
# the same thing, respondents just answer it higher.
cohort_shift <- c(TF6 = 0.35)
# EF3 is endorsed by almost everyone, so its answers pile up in the top
# categories. This shifts its location only; its loading is untouched, which is
# the point -- the item is not measuring less, it just has less room to vary.
item_shift <- c(EF3 = 1.75)

chol_r <- chol(matrix(c(1, factor_r, factor_r, 1), nrow = 2))
eta <- matrix(stats::rnorm(n_respondents * 2), ncol = 2) %*% chol_r
colnames(eta) <- c("EF", "TF")

cohort <- rep(c("A", "B"), each = n_respondents / 2)

responses <- vapply(items$item, function(id) {
  lambda <- loadings[id, ]
  communality <- sum(lambda^2) + 2 * prod(lambda) * factor_r
  y_star <- as.vector(eta %*% lambda) +
    stats::rnorm(n_respondents, sd = sqrt(max(0, 1 - communality)))
  if (id %in% names(item_shift)) y_star <- y_star + item_shift[[id]]
  cuts <- thresholds
  shift <- if (id %in% names(cohort_shift)) cohort_shift[[id]] else 0
  # A positive shift moves answers up the scale for that cohort.
  y_star <- y_star + ifelse(cohort == "B", shift, 0)
  y <- as.integer(cut(y_star, breaks = c(-Inf, cuts, Inf), labels = FALSE))
  # A reverse-worded item is answered in the opposite direction: someone high
  # in persistence disagrees with it. The construct it measures is unchanged.
  if (id %in% reverse_keyed) y <- 6L - y
  y
}, integer(n_respondents))

walkthrough_responses <- data.frame(
  respondent = seq_len(n_respondents),
  cohort = cohort,
  responses,
  stringsAsFactors = FALSE
)
# nomologR reads one row per respondent and one column per item, named exactly
# as the handoff's `item` values, coded from 1 up, with content-review failures
# still present.
stopifnot(identical(names(walkthrough_responses)[-(1:2)], items$item))
stopifnot(all(vapply(walkthrough_responses[items$item], min, numeric(1)) >= 1))

write.csv(walkthrough_responses,
          file.path(out_dir, "walkthrough_responses.csv"),
          row.names = FALSE, quote = FALSE)

message("wrote walkthrough_items.csv (", nrow(items), " rows), ",
        "walkthrough_sort.csv (", nrow(walkthrough_sort), " rows), and ",
        "walkthrough_responses.csv (", nrow(walkthrough_responses),
        " rows) into ", out_dir)
