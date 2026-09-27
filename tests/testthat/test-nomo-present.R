# Console presentation helpers (#89) --------------------------------------------

test_that("numbers, p-values, and intervals follow one format", {
  expect_identical(nomologR:::nomo_present_number(c(0.12345, NA, Inf)),
                   c("0.123", "-", "-"))
  expect_identical(nomologR:::nomo_present_number(2, 1L), "2.0")
  expect_identical(nomologR:::nomo_present_signed(c(0.0216, -0.4, NA)),
                   c("+0.022", "-0.400", "-"))
  expect_identical(nomologR:::nomo_present_p(c(0.00004, 0.0431, 0.5, NA)),
                   c("< .001", ".043", ".500", "-"))
  expect_identical(nomologR:::nomo_present_p_clause(c(0.0002, 0.2, NA)),
                   c("p < .001", "p = .200", ""))
  expect_identical(nomologR:::nomo_present_ci(c(0.1, NA), c(0.3, NA)),
                   c("[0.100, 0.300]", "-"))
})


test_that("every flag vocabulary is shown in one wording", {
  flag <- nomologR:::nomo_present_flag
  expect_identical(flag(c("KEEP", "REVIEW", "STRONG REVIEW")), c("", "review", "concern"))
  expect_identical(flag(c("info", "review", "concern")), c("", "review", "concern"))
  expect_identical(flag(c("none", "INFO", "unavailable")), c("", "", "not computed"))
  expect_identical(flag(c("Something Else", NA)), c("something else", ""))
  expect_identical(nomologR:::nomo_present_flag_counts(c("KEEP", "KEEP")), "none")
  expect_identical(nomologR:::nomo_present_flag_counts(c("REVIEW", "STRONG REVIEW", "KEEP")),
                   "1 review, 1 concern")
})


test_that("text, facts, and bullets wrap to the console width", {
  local_reproducible_output(width = 40)
  long <- paste(rep("word", 30), collapse = " ")

  text <- utils::capture.output(nomologR:::nomo_present_text(long, indent = 2L))
  expect_true(all(nchar(text) <= 40L))
  expect_true(all(startsWith(text, "  ")))

  facts <- utils::capture.output(nomologR:::nomo_present_facts(
    c("First part here", "", "Second part is longer", "Third part")
  ))
  expect_gt(length(facts), 1L)
  expect_true(all(nchar(facts) <= 40L))
  expect_false(any(grepl("|  |", facts, fixed = TRUE)))
  expect_identical(utils::capture.output(nomologR:::nomo_present_facts(character())),
                   character())

  bullets <- utils::capture.output(nomologR:::nomo_present_bullets(c(long, NA, "")))
  expect_true(startsWith(bullets[[1L]], "  - "))
  expect_true(all(startsWith(bullets[-1L], "    ")))
  expect_true(all(nchar(bullets) <= 40L))
})


test_that("tables align, drop empty columns, and name what does not fit", {
  local_reproducible_output(width = 60)
  d <- data.frame(
    item = c("a1", "a2"), value = c(0.5, -0.25), n = c(10L, 200L),
    ok = c(TRUE, NA), empty = c(NA, NA), p = c(0.2, 0.0001),
    stringsAsFactors = FALSE
  )
  out <- utils::capture.output(nomologR:::nomo_present_table(
    d,
    c("Item" = "item", "Value" = "value", "N" = "n", "OK" = "ok",
      "Empty" = "empty", "p" = "p", "Missing" = "not_a_column"),
    formats = list(p = nomologR:::nomo_present_p)
  ))
  expect_identical(out[[1L]], "  Item   Value    N  OK        p")
  expect_identical(out[[2L]], "  a1     0.500   10  yes    .200")
  expect_identical(out[[3L]], "  a2    -0.250  200  -    < .001")

  wide <- data.frame(a = "x", b = strrep("y", 30), c = strrep("z", 30))
  narrow <- utils::capture.output(nomologR:::nomo_present_table(
    wide, c("A" = "a", "B" = "b", "C" = "c"), more = "nomo_table(x)"
  ))
  expect_false(any(grepl("zzz", narrow)))
  expect_match(narrow[[length(narrow)]], "Not shown for width: C. See nomo_table(x).",
               fixed = TRUE)
  bare <- utils::capture.output(nomologR:::nomo_present_table(
    wide, c("A" = "a", "B" = "b", "C" = "c")
  ))
  expect_match(bare[[length(bare)]], "Not shown for width: C.$")

  expect_identical(utils::capture.output(nomologR:::nomo_present_table(d[0, ], c("Item" = "item"))),
                   character())
  expect_identical(utils::capture.output(nomologR:::nomo_present_table(d, c("E" = "empty"))),
                   character())
})


test_that("the CFA print and summary read as designed (#89)", {
  cfa <- nomo_cfa(
    nomo_model(list(A = paste0("a", 1:5), B = paste0("b", 1:5))),
    data = nomo_demo_continuous
  )
  expect_snapshot(print(cfa))
  expect_snapshot(print(summary(cfa)))
})
