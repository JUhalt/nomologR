# as.data.frame() for result objects (#145) --------------------------------------
#
# A result keeps the call that made it, and base R's method for a list
# evaluates that call again, for the object it returns too, without end. These
# tests never let that happen: each object's call is replaced by one that
# stops, so a class without its method fails here at once.

stopping_call <- quote(stop("the stored call was evaluated"))

# The classes whose objects keep a call, and the two summaries that carry it.
refused_classes <- c(
  "nomo_screen", "nomo_factors", "nomo_efa", "nomo_cfa", "nomo_esem", "nomo_compare",
  "nomo_split", "nomo_reliability", "nomo_retest", "nomo_validity",
  "nomo_method_variance", "nomo_hierarchical", "nomo_scores", "nomo_missing",
  "nomo_invariance", "nomo_hypotheses", "nomo_network", "nomo_power", "nomo_run",
  "summary_nomo_scores", "summary_nomo_power"
)


test_that("as.data.frame() refuses a result that keeps its call, and says where the tables are", {
  for (cls in refused_classes) {
    x <- structure(list(call = stopping_call, n = 1L), class = c(cls, "list"))
    advice <- if (cls == "nomo_split") {
      "Its samples are the data frames `x$calibration` and `x$validation`."
    } else if (startsWith(cls, "summary_")) {
      "Use `nomo_table()` on the result it summarizes to get one of its tables."
    } else {
      "Use `nomo_table()` to get one of its tables."
    }
    expect_error(
      as.data.frame(x),
      sprintf(
        "A `%s` object is not a table, so it cannot be converted to a data frame. %s",
        cls, advice
      ),
      fixed = TRUE, label = cls
    )
    # data.frame() converts its arguments the same way.
    expect_error(data.frame(x), "is not a table", fixed = TRUE, label = cls)
  }

  # A longitudinal invariance result is a nomo_invariance object, named as itself.
  longitudinal <- structure(
    list(call = stopping_call),
    class = c("nomo_invariance_longitudinal", "nomo_invariance", "list")
  )
  expect_error(as.data.frame(longitudinal),
               "A `nomo_invariance_longitudinal` object is not a table", fixed = TRUE)
})


test_that("only the classes whose objects keep a call have an as.data.frame() method", {
  registered <- function(cls) {
    !is.null(utils::getS3method("as.data.frame", cls, optional = TRUE))
  }
  for (cls in c(refused_classes, "nomo_partial")) expect_true(registered(cls), label = cls)
  # A class that keeps no call converts as base R converts a list.
  for (cls in c("nomo_single_indicator", "nomo_apa_table", "nomo_expectation",
                "summary_nomo_cfa")) {
    expect_false(registered(cls), label = cls)
  }

  # Objects as their functions return them, with the stored call swapped for
  # one that stops.
  skip_on_cran()
  objects <- list(
    nomo_split = nomo_split(data.frame(x = 1:20)),
    nomo_hypotheses = nomo_hypotheses("A -> B" = positive()),
    nomo_screen = nomo_screen(nomo_demo_continuous[, paste0("a", 1:5)]),
    nomo_cfa = nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5", nomo_demo_continuous,
                        modification_indices = FALSE)
  )
  for (name in names(objects)) {
    x <- objects[[name]]
    expect_true(is.call(x$call), label = name)
    x$call <- stopping_call
    expect_error(as.data.frame(x), sprintf("A `%s` object is not a table", name),
                 fixed = TRUE, label = name)
    # Every result is still a list.
    expect_true(is.list(x), label = name)
    expect_true(inherits(x, "list"), label = name)
  }
  scores <- summary(nomo_scores(objects$nomo_cfa))
  expect_true(is.call(scores$call))
  scores$call <- stopping_call
  expect_error(
    as.data.frame(scores),
    "Use `nomo_table()` on the result it summarizes to get one of its tables.",
    fixed = TRUE
  )
})


test_that("a power analysis that keeps no call converts as base R converts a list", {
  # nomo_power_rmsea() stores no call, so nothing is evaluated again; the
  # result of nomo_power_simulate(), of the same class, is refused above.
  rmsea <- nomo_power_rmsea(df = 50, n = 200)
  expect_null(rmsea$call)
  expected <- as.data.frame(unclass(rmsea))
  expect_identical(nrow(expected), 1L)
  expect_identical(as.data.frame(rmsea), expected)
  expect_identical(as.data.frame(summary(rmsea)), expected)
})
