# Build nomo_demo_walkthrough and nomo_demo_walkthrough_items ------------------
#
# The shared teaching data of the joint contentvalidR / nomologR walkthrough
# (#53, #60). contentvalidR generates them with a documented population model
# and ships them in its inst/extdata; nomologR keeps a copy so its companion
# article builds without contentvalidR installed.
#
# The copies in data-raw/walkthrough/ are contentvalidR's files at tag v0.9.0,
# byte for byte, including its generator (build-walkthrough-data.R). See
# data-raw/walkthrough/README.md for their git blob hashes. Rerunning that
# generator from contentvalidR's package root reproduces the CSVs; this script
# only converts them to R data, so the two packages cannot drift apart.
#
# Regenerate from the package root with:
#
#   source("data-raw/nomo_demo_walkthrough.R")

source_dir <- file.path("data-raw", "walkthrough")

responses <- utils::read.csv(file.path(source_dir, "walkthrough_responses.csv"))
items <- utils::read.csv(file.path(source_dir, "walkthrough_items.csv"))

item_names <- items$item
stopifnot(
  identical(names(responses), c("respondent", "cohort", item_names)),
  nrow(responses) == 400L,
  all(vapply(responses[item_names], function(x) all(x %in% 1:5), logical(1)))
)

nomo_demo_walkthrough <- data.frame(
  respondent = as.integer(responses$respondent),
  cohort = factor(responses$cohort, levels = c("A", "B")),
  lapply(responses[item_names], as.integer),
  check.names = FALSE
)

nomo_demo_walkthrough_items <- data.frame(
  item = items$item,
  facet = items$facet,
  stem = items$stem,
  reverse_worded = as.logical(items$reverse_worded),
  role = items$role,
  stringsAsFactors = FALSE
)

save(nomo_demo_walkthrough, file = "data/nomo_demo_walkthrough.rda",
     compress = "bzip2", version = 2)
save(nomo_demo_walkthrough_items, file = "data/nomo_demo_walkthrough_items.rda",
     compress = "bzip2", version = 2)
