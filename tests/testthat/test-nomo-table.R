# Report-ready tables ------------------------------------------------------------

test_that("invariance local-strain tables add readable constraint labels", {
  skip_on_cran()

  inv <- nomo_invariance(
    "Agency =~ ag1 + ag2 + ag3 + ag4",
    data = nomo_demo_network,
    group = "group",
    levels = c("configural", "metric", "scalar")
  )
  strain <- nomo_table(inv, "local_strain")

  expect_s3_class(strain, "tbl_df")
  expect_true(all(c("constraint", "constraint_display") %in% names(strain)))
  expect_identical(strain$constraint, inv$local_strain$constraint)
  expect_false(any(grepl("^\\.p[0-9]+\\.", strain$constraint_display)))
})


test_that("every measurement-stage result has nomo_table() (#89)", {
  skip_on_cran()
  hs <-"visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9"
  cfa <- nomo_cfa(hs, data = lavaan::HolzingerSwineford1939)
  fac <- nomo_factors(nomo_demo_continuous, n_iter = 20, seed = 2026)
  objects <- list(
    nomo_factors = fac,
    nomo_efa = nomo_efa(nomo_demo_continuous, factors = fac),
    nomo_cfa = cfa,
    nomo_reliability = nomo_reliability(cfa),
    nomo_validity = nomo_validity(cfa)
  )

  for (cls in names(objects)) {
    x <- objects[[cls]]
    types <- eval(formals(utils::getS3method("nomo_table", cls))$type)
    # The default is the first type, and every type is a tibble.
    expect_identical(nomo_table(x), nomo_table(x, types[[1L]]), label = cls)
    for (type in types) {
      expect_s3_class(nomo_table(x, type), "tbl_df")
    }
    expect_error(nomo_table(x, "not_a_table"), "`type` must be one of", fixed = TRUE, label = cls)
  }

  # Tables are the object's own evidence, not copies that could differ.
  expect_identical(nomo_table(cfa, "loadings"), cfa$standardized_loadings)
  expect_identical(nomo_table(cfa, "modification_indices"), cfa$top_modification_indices)
  expect_identical(nomo_table(objects$nomo_efa, "residuals"), objects$nomo_efa$residual_pairs)
  expect_identical(nomo_table(objects$nomo_validity, "discriminant"),
                   summary(objects$nomo_validity)$discriminant)
  expect_identical(nomo_table(fac, "criteria"), summary(fac)$criterion_status)

  pattern <- nomo_table(objects$nomo_efa, "pattern")
  expect_identical(names(pattern)[[1L]], "item")
  expect_equal(unname(as.matrix(pattern[, -1L])),
               unname(unclass(objects$nomo_efa$pattern_matrix)))
})


test_that("empty invariance local-strain tables are returned unchanged", {
  empty <- structure(
    list(local_strain = tibble::tibble(), fits = list()),
    class = c("nomo_invariance", "list")
  )

  expect_identical(nomo_table(empty, "local_strain"), tibble::tibble())
})


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("report-ready table generic refuses unsupported classes normally", {
  expect_error(
    nomo_table(list(a = 1)),
    "no applicable method",
    ignore.case = TRUE
  )
})


test_that("nomo_table generic covers hypothesis and unsupported-object behavior", {
  h <- nomo_hypotheses(
    "A -> B" = positive(),
    "A <-> C" = negligible(within = c(-.10, .10))
  )

  expect_identical(
    nomo_table(h),
    h$hypotheses
  )

  expect_error(
    nomo_table(list(a = 1)),
    "no applicable method",
    ignore.case = TRUE
  )
})


# The documented surface (#145) -------------------------------------------------
#
# The `type` values nomo_table() accepts are covered by the stability policy,
# so ?nomo_table must list exactly what each method accepts.

test_that("?nomo_table lists every type each method accepts, and no other (#145)", {
  items <- nomo_test_rd_items(nomo_test_rd_text("nomo_table", "\\details"))
  # A bullet names its classes before the first colon.
  heads <- sub("}:.*$", "}", items)
  ns <- asNamespace("nomologR")

  for (method in ls(ns, pattern = "^nomo_table\\.nomo_")) {
    cls <- sub("^nomo_table\\.", "", method)
    item <- items[grepl(paste0("\\code{", cls, "}"), heads, fixed = TRUE)]
    expect_identical(length(item), 1L, label = cls)

    accepted <- eval(formals(get(method, envir = ns))$type)
    documented <- regmatches(item, gregexpr('\\\\code\\{"[a-z_]+"\\}', item))[[1L]]
    documented <- unique(gsub('^\\\\code\\{"|"\\}$', "", documented))
    expect_setequal(documented, as.character(accepted))
  }
})


test_that("the fit-table columns ?nomo_table names are the ones returned (#145)", {
  skip_on_cran()
  items <- nomo_test_rd_items(nomo_test_rd_text("nomo_table", "Fit tables"))

  two <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
  cfa <- nomo_cfa(two, data = nomo_demo_continuous)
  no_b5 <- nomo_cfa(sub("b5$", "0*b5", two), data = nomo_demo_continuous)
  agency <- c("ag1", "ag2", "ag3", "ag4")
  persistence <- c("pe1", "pe2", "pe3", "pe4")
  model <- nomo_model(list(Agency = agency, Persistence = persistence))

  # The ?nomo_method_variance example's population, with a method factor.
  set.seed(2010)
  marker_data <- lavaan::simulateData(
    paste(
      "A =~ 0.7*a1 + 0.7*a2 + 0.6*a3 + 0.6*a4",
      "B =~ 0.7*b1 + 0.6*b2 + 0.6*b3 + 0.5*b4",
      "M =~ 0.7*m1 + 0.7*m2 + 0.6*m3",
      "CMV =~ 0.3*a1 + 0.3*a2 + 0.3*a3 + 0.3*a4 + 0.3*b1 + 0.3*b2 + 0.3*b3 +",
      "  0.3*b4 + 0.3*m1 + 0.3*m2 + 0.3*m3",
      "A ~~ 0.4*B",
      "A ~~ 0*M",
      "B ~~ 0*M",
      "CMV ~~ 0*A + 0*B + 0*M",
      sep = "\n"
    ),
    sample.nobs = 600, standardized = TRUE
  )

  tables <- list(
    nomo_cfa = nomo_table(cfa, "fit"),
    nomo_missing = nomo_table(
      nomo_missing(cfa, data = nomo_demo_continuous, reliability = FALSE), "fit"
    ),
    nomo_esem = nomo_table(nomo_esem(model, nomo_demo_network), "models"),
    nomo_method_variance = nomo_table(
      nomo_method_variance(
        "A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4",
        data = marker_data, marker = c("m1", "m2", "m3")
      ),
      "models"
    ),
    nomo_compare = nomo_table(
      nomo_compare(full = cfa, no_b5 = no_b5, rationale = "Test b5.", evidence = FALSE),
      "models"
    ),
    nomo_invariance = nomo_table(
      nomo_invariance(
        paste("Agency =~", paste(agency, collapse = " + ")),
        data = nomo_demo_network, group = "group", levels = c("configural", "metric")
      ),
      "fit"
    ),
    nomo_network = nomo_table(
      nomo_network(
        model, data = nomo_demo_network,
        hypotheses = nomo_hypotheses("Agency -> Persistence" = positive())
      ),
      "fit"
    )
  )
  # Every documented fit table is checked.
  expect_setequal(sub("^\\\\code\\{([a-z_]+)\\}.*$", "\\1", items), names(tables))

  for (cls in names(tables)) {
    item <- items[startsWith(items, paste0("\\code{", cls, "},"))]
    named <- regmatches(item, gregexpr("\\\\code\\{[A-Za-z_][A-Za-z0-9_]*\\}", item))[[1L]]
    named <- setdiff(gsub("^\\\\code\\{|\\}$", "", named), cls)
    tb <- tables[[cls]]
    # The long nomo_cfa table names its indices in `metric`.
    returned <- c(names(tb), if ("metric" %in% names(tb)) tb$metric)
    expect_true(all(named %in% returned), label = paste(cls, toString(setdiff(named, returned))))
  }

  # The p-value is `p_value` in the nomo_cfa and nomo_missing tables and
  # `pvalue` where lavaan's names are kept (?nomologR).
  expect_true("p_value" %in% tables$nomo_cfa$metric)
  expect_true("p_value" %in% names(tables$nomo_missing))
  for (cls in c("nomo_esem", "nomo_method_variance", "nomo_invariance", "nomo_network")) {
    expect_true("pvalue" %in% names(tables[[cls]]), label = cls)
    expect_false("p_value" %in% names(tables[[cls]]), label = cls)
  }
})
