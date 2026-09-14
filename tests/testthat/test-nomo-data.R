# Teaching datasets -------------------------------------------------------------
#
# The documented population features of the teaching datasets are relied on by
# examples and vignettes. These tests fail if regenerated data no longer show
# them; any intentional change must be recorded in NEWS.md.

test_that("teaching datasets have the documented structure", {
  expect_identical(dim(nomo_demo_continuous), c(500L, 10L))
  expect_identical(
    names(nomo_demo_continuous),
    c(paste0("a", 1:5), paste0("b", 1:5))
  )
  expect_true(all(vapply(nomo_demo_continuous, is.numeric, logical(1))))
  expect_identical(sum(is.na(nomo_demo_continuous)), 27L)
  expect_identical(sum(is.na(nomo_demo_continuous$a2)), 15L)
  expect_identical(sum(is.na(nomo_demo_continuous$b3)), 12L)

  expect_identical(dim(nomo_demo_ordinal), c(500L, 10L))
  expect_identical(names(nomo_demo_ordinal), names(nomo_demo_continuous))
  expect_true(all(vapply(nomo_demo_ordinal, is.ordered, logical(1))))
  expect_true(all(vapply(
    nomo_demo_ordinal,
    function(x) identical(levels(x), as.character(1:5)),
    logical(1)
  )))
  expect_false(anyNA(nomo_demo_ordinal))

  expect_identical(dim(nomo_demo_network), c(800L, 13L))
  expect_identical(
    names(nomo_demo_network),
    c(paste0("ag", 1:4), paste0("pe", 1:4), paste0("sd", 1:3), "Performance", "group")
  )
  expect_identical(levels(nomo_demo_network$group), c("online", "paper"))
  expect_identical(as.vector(table(nomo_demo_network$group)), c(400L, 400L))
  expect_false(anyNA(nomo_demo_network))
})


test_that("continuous teaching data show the documented structure and item flags", {
  fac <- nomo_factors(
    nomo_demo_continuous,
    criterion_set = "minimal",
    n_iter = 20,
    seed = 2026
  )
  efa <- nomo_efa(nomo_demo_continuous, factors = fac)
  items <- efa$item_summary

  expect_identical(ncol(efa$pattern_matrix), 2L)

  a_factor <- unique(items$primary_factor[items$item %in% paste0("a", 1:4)])
  b_factor <- unique(items$primary_factor[items$item %in% paste0("b", 1:5)])
  expect_length(a_factor, 1L)
  expect_length(b_factor, 1L)
  expect_false(identical(a_factor, b_factor))

  expect_true(items$cross_loading[items$item == "a5"])
  expect_false(any(items$cross_loading[items$item != "a5"]))
  expect_true(items$weak_primary[items$item == "b5"])
  expect_identical(items$attention[items$item == "b5"], "STRONG REVIEW")
})


test_that("ordinal teaching data use polychoric evidence for two factors", {
  skip_on_cran()

  fac <- nomo_factors(
    nomo_demo_ordinal,
    criterion_set = "minimal",
    n_iter = 20,
    seed = 2026
  )
  efa <- nomo_efa(nomo_demo_ordinal, factors = fac)

  expect_identical(fac$correlation, "polychoric")
  expect_identical(ncol(efa$pattern_matrix), 2L)
})


test_that("network teaching data reproduce the documented theory evidence", {
  skip_on_cran()

  model <- nomo_model(list(
    Agency = paste0("ag", 1:4),
    Persistence = paste0("pe", 1:4),
    SocialDesirability = paste0("sd", 1:3)
  ))
  h <- nomo_hypotheses(
    "Agency -> Persistence" = positive(min = .20),
    "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15)),
    "Agency -> Performance" = positive(),
    "Persistence -> Performance" = positive(min = .20)
  )

  net <- nomo_network(model, data = nomo_demo_network, hypotheses = h)
  evidence <- nomo_table(net, "hypotheses")

  expect_identical(
    evidence$concordance,
    c("concordant", "concordant", "concordant", "inconsistent")
  )
  expect_true(evidence$equivalence_supported[evidence$id == "H2"])
  expect_equal(evidence$estimate[evidence$id == "H1"], .45, tolerance = .10)
  expect_equal(evidence$estimate[evidence$id == "H3"], .40, tolerance = .10)
})


test_that("network teaching data localize the known scalar non-invariance", {
  skip_on_cran()

  inv <- nomo_invariance(
    "Agency =~ ag1 + ag2 + ag3 + ag4",
    data = nomo_demo_network,
    group = "group",
    levels = c("configural", "metric", "scalar")
  )
  strain <- nomo_table(inv, "local_strain")
  scalar <- strain[strain$level == "scalar", , drop = FALSE]

  expect_gt(nrow(scalar), 0L)
  expect_match(scalar$constraint_display[[1L]], "Intercept: ag3", fixed = TRUE)
})
