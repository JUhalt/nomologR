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
