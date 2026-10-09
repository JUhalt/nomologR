# The global environment and the random-number state ---------------------------
#
# The package assigns nothing into the global environment and removes nothing
# from it. A function that takes a seed sets it through withr::with_seed(),
# which puts the caller's random-number state back afterwards.

test_that("the code never names or assigns into the global environment", {
  # An installed package keeps no R sources, so the check runs from the source
  # tree only.
  source_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(file.exists(file.path(source_dir, "nomo_split.R")))
  # The name is put together here so that a search of the package for it finds
  # nothing, in the code or in its tests.
  global_name <- paste0(".Global", "Env")
  superassignments <- 0L

  for (file in list.files(source_dir, pattern = "[.]R$", full.names = TRUE)) {
    lines <- readLines(file, warn = FALSE)
    expect_false(any(grepl(global_name, lines, fixed = TRUE)),
                 label = sprintf("%s names the global environment", basename(file)))

    data <- utils::getParseData(parse(file, keep.source = TRUE))
    parent <- integer(max(data$id))
    parent[data$id] <- data$parent
    line_of <- integer(max(data$id))
    line_of[data$id] <- data$line1
    children <- function(id) {
      kids <- data[data$parent == id, ]
      kids[order(kids$line1, kids$col1), ]
    }
    called <- function(name) {
      data$id[data$token == "SYMBOL_FUNCTION_CALL" & data$text == name]
    }
    where <- function(id) sprintf("%s:%d", rep(basename(file), length(id)), line_of[id])

    # A seed set directly would move the caller's random-number stream.
    expect_identical(where(called("set.seed")), character(),
                     label = sprintf("set.seed() calls in %s", basename(file)))

    # globalenv() may be read as the parent of a new environment and nothing
    # else, so it is never the target of assign(), rm(), eval() or `$<-`.
    for (id in called("globalenv")) {
      call <- parent[[parent[[id]]]]
      siblings <- children(parent[[call]])
      at <- match(call, siblings$id)
      outer <- data$text[data$parent == siblings$id[[1L]] &
                           data$token == "SYMBOL_FUNCTION_CALL"]
      expect_true(
        identical(outer, "new.env") && at > 2L &&
          siblings$token[[at - 1L]] == "EQ_SUB" && siblings$text[[at - 2L]] == "parent",
        label = sprintf("%s uses globalenv() only as the parent of new.env()", where(id))
      )
    }

    # `<<-` assigns into the global environment when no enclosing function
    # holds the variable. Each one here sets a variable that an enclosing
    # function takes as an argument or created with `<-` on an earlier line of
    # its own body.
    functions <- unique(data$parent[data$token == "FUNCTION"])
    enclosing <- function(id) {
      out <- integer()
      id <- parent[[id]]
      while (id > 0L) {
        if (id %in% functions) out <- c(out, id)
        id <- parent[[id]]
      }
      out
    }
    first_symbol <- function(id) {
      kids <- children(id)
      for (k in seq_len(nrow(kids))) {
        if (kids$token[[k]] == "SYMBOL") return(kids$text[[k]])
        if (kids$token[[k]] == "expr") {
          found <- first_symbol(kids$id[[k]])
          if (!is.null(found)) return(found)
        }
      }
      NULL
    }

    for (id in data$id[data$token == "LEFT_ASSIGN" & data$text == "<<-"]) {
      superassignments <- superassignments + 1L
      target <- first_symbol(children(parent[[id]])$id[[1L]])
      outer_functions <- enclosing(id)[-1L]
      held <- target %in% data$text[data$token == "SYMBOL_FORMALS" &
                                      data$parent %in% outer_functions]
      earlier <- data[data$token == "SYMBOL" & data$text == target &
                        data$line1 < line_of[[id]], ]
      for (k in seq_len(nrow(earlier))) {
        if (held) break
        left <- earlier$parent[[k]]
        assignment <- children(parent[[left]])
        held <- sum(data$parent == left) == 1L && assignment$id[[1L]] == left &&
          any(assignment$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN") &
                assignment$text %in% c("<-", "=")) &&
          enclosing(left)[1L] %in% outer_functions
      }
      expect_true(held, label = sprintf("%s sets `%s`, a variable of an enclosing function",
                                        where(id), target))
    }
  }
  expect_gt(superassignments, 30L)
})


test_that("a seeded call leaves a session that has no random-number state without one", {
  skip_on_cran()
  # Only a fresh R process has no random-number state, and a test may not
  # remove the session's own. The process loads the installed package that
  # this test is running against, so it needs one: under R CMD check and
  # covr, not from a source tree that devtools has loaded.
  package_dir <- find.package("nomologR")
  skip_if_not(file.exists(file.path(package_dir, "R", "nomologR.rdb")))

  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
    "state <- function(label) {",
    "  present <- exists('.Random.seed', envir = globalenv(), inherits = FALSE)",
    "  writeLines(paste(label, if (present) 'present' else 'absent'))",
    "}",
    "state('start')",
    sprintf("suppressPackageStartupMessages(library(nomologR, lib.loc = %s))",
            deparse(dirname(package_dir))),
    "state('loaded')",
    "split <- nomo_split(data.frame(x = 1:40), seed = 3)",
    "state('nomo_split')",
    "factors <- suppressWarnings(nomo_factors(stats::na.omit(nomo_demo_continuous),",
    "                                         criterion_set = 'all', n_iter = 10, seed = 3))",
    "state('nomo_factors')",
    "population <- 'A =~ 0.7*a1 + 0.7*a2 + 0.6*a3\\nB =~ 0.7*b1 + 0.6*b2 + 0.6*b3\\nA ~~ 0.3*B'",
    "power <- nomo_power_simulate(population, n = 40, reps = 2, seed = 3)",
    "state('nomo_power_simulate')",
    "ran <- factors$criterion_status$criterion[factors$criterion_status$status == 'available']",
    "writeLines(paste('criteria', paste(ran, collapse = ' ')))"
  ), script)

  # R CMD check points R_TESTS at a startup file that a process started from
  # another directory cannot find.
  output <- withr::with_envvar(
    c(R_TESTS = ""),
    suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(script)),
      stdout = TRUE, stderr = TRUE
    ))
  )
  expect_null(attr(output, "status"), label = paste(output, collapse = "\n"))
  # A package that drew random numbers as it loaded would leave nothing to
  # test here.
  skip_if_not("loaded absent" %in% output)

  expect_true("start absent" %in% output)
  expect_true("nomo_split absent" %in% output)
  expect_true("nomo_factors absent" %in% output)
  expect_true("nomo_power_simulate absent" %in% output)
  # The factor-retention call reached the criteria that set seeds of their
  # own, beside parallel analysis.
  criteria <- grep("^criteria ", output, value = TRUE)
  expect_match(criteria, "nest", fixed = TRUE)
  expect_match(criteria, "hull", fixed = TRUE)
  expect_match(criteria, "comparison_data", fixed = TRUE)
})
