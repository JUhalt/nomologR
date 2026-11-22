# The output style shared with contentvalidR (#144) and the pre-1.0 audit's
# presentation findings on the helpers (#145) ------------------------------------

test_that("a value that rounds to zero prints unsigned, and p never prints 1.000 (#145)", {
  number <- nomologR:::nomo_present_number
  expect_identical(number(c(-0.0001, -0.0004)), c("0.000", "0.000"))
  expect_identical(number(-0.004, 2L), "0.00")
  expect_identical(nomologR:::nomo_present_ci(-0.0001, 0.0001), "[0.000, 0.000]")

  # A change that rounds to zero has no direction to show.
  signed <- nomologR:::nomo_present_signed
  expect_identical(signed(c(-0.0001, 0, 1e-12)), c("0.000", "0.000", "0.000"))
  expect_identical(signed(c(0.0216, -0.4, 0.0005)), c("+0.022", "-0.400", "+0.001"))

  expect_identical(nomologR:::nomo_present_p(c(0.9996, 1, 0.9994)), c("> .999", "> .999", ".999"))
  expect_identical(nomologR:::nomo_present_p_clause(0.9996), "p > .999")
  expect_identical(nomologR:::nomo_apa_p(c(0.9996, 0.0004, 0.5)), c("> .999", "< .001", ".500"))
  # The duplicate p helper reads the new bound too.
  expect_identical(nomologR:::nomo_present_p_text(c(0.9996, 0.043)), c("> .999", "= .043"))
})


test_that("display rounding is half away from zero, in the console and APA tables (#144)", {
  expect_identical(nomologR:::nomo_round_half_up(c(0.625, 0.125, -0.625), 2), c(0.63, 0.13, -0.63))
  expect_identical(nomologR:::nomo_present_number(c(0.625, 0.125, -0.625), 2L),
                   c("0.63", "0.13", "-0.63"))
  expect_identical(nomologR:::nomo_apa_number(c(0.625, -0.0004), 2L, bounded = TRUE),
                   c(".63", ".00"))
  expect_identical(nomologR:::nomo_apa_number(NA), nomologR:::nomo_apa_dash)
  # Stored values are untouched: only the text is rounded.
  expect_identical(nomologR:::nomo_present_round(c(-0.004, NA, Inf), 2L), c(0, NA, Inf))
})


test_that("bounded statistics drop the leading zero, and each kind has one rule (#145)", {
  number <- nomologR:::nomo_present_number
  expect_identical(number(c(0.5, -0.25, 1.5), 2L, bounded = TRUE), c(".50", "-.25", "1.50"))
  expect_identical(nomologR:::nomo_present_signed(c(0.02, -0.004), 2L, bounded = TRUE),
                   c("+.02", ".00"))
  expect_identical(nomologR:::nomo_present_ci(0.123, 0.345, 2L, bounded = TRUE), "[.12, .35]")

  stat <- nomologR:::nomo_present_stat
  x <- c(0.4567, NA)
  expect_identical(stat(x, "p"), c(".457", "--"))
  expect_identical(stat(x, "fit_bounded"), c(".457", "--"))
  expect_identical(stat(x, "fit"), c("0.457", "--"))
  for (kind in c("r", "reliability", "proportion", "power")) {
    expect_identical(stat(x, kind), c(".46", "--"), label = kind)
  }
  for (kind in c("loading", "htmt", "stat", "estimate", "ic")) {
    expect_identical(stat(x, kind), c("0.46", "--"), label = kind)
  }
  expect_identical(stat(c(34, 33.4667, NA), "df"), c("34", "33.47", "--"))
  expect_identical(stat(c(473, NA), "count"), c("473", "--"))
  expect_identical(stat(c(0.05, 0.1, 0.025, 0.05 / 3, 1, NA), "level"),
                   c(".05", ".10", ".025", ".0167", "1.00", "--"))
  expect_identical(stat(1.23456, "estimate", digits = 3L), "1.235")
  expect_identical(stat(c(0.002, -0.0001), "fit_bounded", signed = TRUE), c("+.002", ".000"))
  expect_identical(stat(0.004, "fit", signed = TRUE), "+0.004")
  expect_error(stat(1, "loadings"), "`kind` must be one of", fixed = TRUE)

  # A value just off its reference shows the decimals that tell them apart, so a
  # flag never reads "0.40 is below the 0.40 reference".
  expect_identical(stat(c(0.398, 0.4004, 0.45, 0.40, NA), "loading", reference = 0.40),
                   c("0.398", "0.4004", "0.45", "0.40", "--"))
  expect_identical(stat(0.40000001, "loading", reference = 0.40), "0.40000")
})


test_that("an interval is whole or missing, with its level in the label (#144)", {
  ci <- nomologR:::nomo_present_ci
  expect_identical(ci(c(0.1, NA, 0.2), c(NA, NA, 0.4)), c("--", "--", "[0.200, 0.400]"))
  expect_identical(ci(0.123, 0.345, kind = "r"), "[.12, .35]")
  expect_identical(ci(0.12345, 0.345, digits = 3L, kind = "loading"), "[0.123, 0.345]")
  expect_identical(nomologR:::nomo_present_ci_label(c(0.95, 0.9)), c("95% CI", "90% CI"))
  # Each level keeps its own digits (#145, R-0b follow-up).
  expect_identical(nomologR:::nomo_present_ci_label(c(0.95, 0.975, 0.9)),
                   c("95% CI", "97.5% CI", "90% CI"))
})


test_that("percentages are whole on a base under 100 and take one decimal otherwise (#144)", {
  percent <- nomologR:::nomo_present_percent
  expect_identical(percent(c(0.375, 0.625), base = 8), c("38%", "63%"))
  expect_identical(percent(c(0.375, 0.625), base = 500), c("37.5%", "62.5%"))
  expect_identical(percent(0.998, base = c(40, 500)), "99.8%")
  expect_identical(percent(c(0.998, NA)), c("99.8%", "--"))
})


test_that("a chi-square test is written one way (#144)", {
  chisq <- nomologR:::nomo_present_chisq
  expect_identical(chisq(75.834, 34, 0.00001), "chi-square(34) = 75.83, p < .001")
  expect_identical(chisq(10, 1, 0.0016, n = 40), "chi-square(1, N = 40) = 10.00, p = .002")
  expect_identical(chisq(12.34, 6, 0.015, delta = TRUE), "Delta chi-square(6) = 12.34, p = .015")
  expect_identical(chisq(c(NA, 3.2), c(2, 33.4667)), c("", "chi-square(33.47) = 3.20"))
})


test_that("status words display in sentence case without changing (#144)", {
  status <- nomologR:::nomo_present_status
  expect_identical(
    status(c("KEEP", "none", "info", "OK", "REVIEW", "review", "STRONG REVIEW", "concern",
             "unavailable", NA, "note")),
    c("", "", "", "", "Review", "Review", "Concern", "Concern", "Not computed", "", "Note")
  )
  expect_identical(nomologR:::nomo_present_origin(c("a_priori", "post_hoc", "a-priori", NA)),
                   c("a priori", "post hoc", "a priori", "--"))
  expect_identical(nomologR:::nomo_present_origin("post_hoc", cell = TRUE), "Post hoc")
  expect_identical(nomologR:::nomo_present_ordinal(c(1, 2, 3, 4, 11, 12, 13, 21, 93, 111)),
                   c("1st", "2nd", "3rd", "4th", "11th", "12th", "13th", "21st", "93rd", "111th"))
  # A missing number is the missing marker, not "NANA" (#145, R-0b follow-up).
  expect_identical(nomologR:::nomo_present_ordinal(c(93, NA)), c("93rd", "--"))
})


test_that("statistical clauses are never broken across lines (#145)", {
  local_reproducible_output(width = 40)
  # Unbound, strwrap() would end the first line on "p <" and start the next
  # with ".001".
  lead <- strrep("x", 30)
  text <- utils::capture.output(nomologR:::nomo_present_text(lead, " p < .001 here."))
  expect_identical(text, c(lead, "p < .001 here."))

  bullets <- utils::capture.output(nomologR:::nomo_present_bullets(
    paste(strrep("y", 28), "p = .043 and N = 473.")
  ))
  expect_identical(bullets[[2L]], "    p = .043 and N = 473.")

  bind <- function(x) {
    gsub(nomologR:::nomo_present_nbsp, "_", nomologR:::nomo_present_bind(x), fixed = TRUE)
  }
  expect_identical(bind("so p < .001, n = 40, alpha = .05"), "so p_<_.001, n_=_40, alpha_=_.05")
  expect_identical(bind("CI = confidence interval"), "CI_=_confidence interval")
  expect_identical(bind("95% CI [.12, .34] and F(2, 14) = 3.21"),
                   "95%_CI_[.12,_.34] and F(2,_14)_=_3.21")
  expect_identical(bind("chi-square(34) = 75.83 and chi-square(1, N = 40)"),
                   "chi-square(34)_=_75.83 and chi-square(1,_N_=_40)")
  # A chain binds only its first clause, so it can still be wrapped.
  expect_identical(bind("a > b > c"), "a_>_b > c")
  # "Delta" stays with its test, a page with its number, a short call with its
  # arguments, and a name with underscores or dots with its value (#145,
  # R-0b follow-up). A call with a named argument, or a long one, stays free.
  expect_identical(bind("Delta chi-square(2) = 3.00"), "Delta_chi-square(2)_=_3.00")
  expect_identical(bind("Gorsuch (1983, p. 260) and pp. 6-47"),
                   "Gorsuch (1983,_p._260) and pp._6-47")
  expect_identical(bind('See nomo_table(x, "criteria") now.'), 'See nomo_table(x,_"criteria") now.')
  expect_identical(bind('nomo_table(x, type = "fit")'), 'nomo_table(x, type_=_"fit")')
  expect_identical(bind('nomo_table(x, "hypothesis_evidence")'), 'nomo_table(x, "hypothesis_evidence")')
  expect_identical(bind("with ord.scale = TRUE and ci_lower = .12"),
                   "with ord.scale_=_TRUE and ci_lower_=_.12")
  expect_identical(bind("x.y = 1"), "x.y_=_1")
  # Code is not bound into one long run: a named argument whose value opens
  # another call stays free, and so does a chain.
  expect_identical(bind("decisions = list(cfa_model = list(value = model, source = x)"),
                   "decisions_=_list(cfa_model = list(value_=_model, source_=_x)")
  expect_identical(bind("(p = .043) and f(n = g(1))"), "(p_=_.043) and f(n = g(1))")
  # Unbound, the first line would end on "nomo_table(x,".
  call <- utils::capture.output(nomologR:::nomo_present_text(
    strrep("z", 15), ' see nomo_table(x, "criteria") gives it.'
  ))
  expect_identical(call, c(paste(strrep("z", 15), "see"), 'nomo_table(x, "criteria") gives it.'))

  # Nothing is left bound in what is printed.
  wrapped <- utils::capture.output(nomologR:::nomo_present_text(
    paste(rep("The interval is [0.12, 0.34] with p < .001 and F(2, 14) = 3.21.", 4),
          collapse = " ")
  ))
  expect_false(any(grepl(nomologR:::nomo_present_nbsp, wrapped, fixed = TRUE)))
  expect_true(all(nchar(wrapped) <= 39L))
  expect_false(any(grepl("(^| )p$|^< |\\[[0-9.]+,$|^[0-9.]+\\]|F\\(2,$", wrapped)))
})


test_that("prose stops at 80 columns while tables use the full width (#144)", {
  local_reproducible_output(width = 120)
  long <- paste(rep("word", 60), collapse = " ")
  text <- utils::capture.output(nomologR:::nomo_present_text(long))
  expect_true(all(nchar(text) <= 79L))
  expect_gt(max(nchar(text)), 70L)
  expect_identical(nomologR:::nomo_present_prose_width(), 79L)
  expect_identical(nomologR:::nomo_present_width(), 120L)
})


test_that("headers, sections, and facts fit narrow consoles (#145)", {
  local_reproducible_output(width = 60)
  header <- utils::capture.output(nomologR:::nomo_present_header(
    "nomo_invariance_longitudinal", "Measurement invariance across occasions", summary = TRUE
  ))
  expect_identical(header, c("<nomo_invariance_longitudinal summary> Measurement",
                             "invariance across occasions"))
  expect_identical(
    utils::capture.output(nomologR:::nomo_present_header(
      "nomo_psa", "Proportion of substantive agreement (Psa)", source = "Anderson & Gerbing (1991)"
    )),
    c("<nomo_psa> Proportion of substantive agreement (Psa)", "Anderson and Gerbing (1991).")
  )
  section <- utils::capture.output(nomologR:::nomo_present_section(
    "Largest differences from the reference, in reference standard errors"
  ))
  expect_identical(section[[1L]], "")
  expect_true(all(nchar(section) <= 59L))
  expect_length(section, 3L)

  # A label keeps its value: "hypotheses: 2" is never split into "hypotheses:"
  # and a stranded "2".
  facts <- utils::capture.output(nomologR:::nomo_present_facts(c(
    "Theory relations: 3", "Added to the model from hypotheses: 2", "Fitted: yes"
  )))
  expect_identical(facts, c("Theory relations: 3",
                            "Added to the model from hypotheses: 2 | Fitted: yes"))
  expect_true(all(nchar(facts) <= 59L))

  # Facts inside a section are indented, counted in the width, and a part
  # longer than the line hangs beneath its own indent (#145, R-0b follow-up).
  facts <- utils::capture.output(nomologR:::nomo_present_facts(c(
    "Relations: 3", "Added from hypotheses: 2", "Converged: yes",
    paste("Stages:", paste(rep("configural -> metric", 3L), collapse = " -> "))
  ), indent = 4L))
  # "Relations: 3 | Added from hypotheses: 2 | Converged: yes" is 56 columns,
  # 60 with the indent, past the 59 of the prose width.
  expect_identical(facts, c(
    "    Relations: 3 | Added from hypotheses: 2",
    "    Converged: yes",
    "    Stages: configural -> metric -> configural -> metric",
    "      -> configural -> metric"
  ))
  # No text prints nothing, not an empty line (#145).
  expect_identical(utils::capture.output(nomologR:::nomo_present_text(character(), indent = 2L)),
                   character())
})


test_that("a model's syntax prints under a header, with its notes wrapped (#145)", {
  local_reproducible_output(width = 80)
  model <- nomo_model(list(A = c("a1", "a2"), B = c("b1", "b2", "b3"), C = c("c1", "c2", "c3")),
                      structure = "bifactor")
  printed <- utils::capture.output(print(model))
  expect_identical(printed[[1L]], "<nomo_model> Measurement model syntax")
  expect_identical(printed[[2L]], "G =~ NA*a1 + a2 + b1 + b2 + b3 + c1 + c2 + c3")
  expect_true("Identification notes" %in% printed)
  expect_match(printed, "^  - Concern: Group factor A has two indicators", all = FALSE)
  expect_true(all(nchar(printed) <= 79L))
  plain <- utils::capture.output(print(nomo_model(list(A = c("a1", "a2", "a3")))))
  expect_identical(plain, c("<nomo_model> Measurement model syntax", "A =~ a1 + a2 + a3", "",
                            "See nomo_cfa(x, data) for a guided fit of this model."))
  unnamed <- nomo_model(list(A = c("a1", "a2", "a3")))
  attr(unnamed, "notes") <- "A note without a severity."
  expect_identical(utils::capture.output(print(unnamed))[5L], "  - A note without a severity.")
})


test_that("a narrow table keeps its status and p columns and tightens before dropping (#145)", {
  local_reproducible_output(width = 60)
  d <- data.frame(
    item = c("a1", "a2"), value = c(0.5, -0.25), long1 = strrep("q", 12),
    long2 = strrep("w", 12), long3 = strrep("e", 12), flag = c("", "Review"),
    p = c(0.2, 0.0001), stringsAsFactors = FALSE
  )
  columns <- c("Item" = "item", "Value" = "value", "Long one" = "long1",
               "Long two" = "long2", "Long three" = "long3", "Flag" = "flag", "p" = "p")
  table <- function(...) {
    utils::capture.output(nomologR:::nomo_present_table(
      d, columns, formats = list(p = nomologR:::nomo_present_p), ...
    ))
  }

  # Two-space gaps would need 72 columns and one-space gaps 66, so a column goes;
  # the flag and p columns stay, and the last other column is dropped. The
  # columns left fit with two-space gaps again (58 columns), so they get them.
  out <- table(more = "nomo_table(x, \"items\")")
  expect_identical(out[[1L]], "  Item   Value  Long one      Long two      Flag         p")
  expect_identical(out[[3L]], "  a2    -0.250  qqqqqqqqqqqq  wwwwwwwwwwww  Review  < .001")
  expect_identical(out[4:5], c("  Not shown for width: Long three. See",
                               "  nomo_table(x, \"items\")."))

  # A table that fits with one-space gaps loses nothing.
  local_reproducible_output(width = 66)
  out <- table()
  expect_length(out, 3L)
  expect_identical(out[[1L]], "  Item  Value Long one     Long two     Long three   Flag        p")

  # With nothing else left to drop, the stub and the kept columns stay.
  local_reproducible_output(width = 40)
  out <- table(keep = c("Value", "Long one", "Long two", "Long three", "Flag", "p"))
  expect_false(any(grepl("Not shown", out, fixed = TRUE)))
  out <- table(keep = character())
  # Two-space gaps would need 42 columns here, so the gaps stay at one.
  expect_identical(out[[1L]], "  Item  Value Long one     Long two")
  expect_match(gsub("\\s+", " ", paste(out[-(1:3)], collapse = " ")),
               "Not shown for width: Long three, Flag, p.", fixed = TRUE)

  # A missing cell is "--", and a column missing everywhere is still dropped.
  d$long1[[1L]] <- NA
  d$flag <- NA_character_
  local_reproducible_output(width = 80)
  out <- table()
  expect_identical(out[[2L]], "  a1     0.500  --            wwwwwwwwwwww  eeeeeeeeeeee    .200")
  expect_false(any(grepl("Flag", out, fixed = TRUE)))
  expect_identical(
    utils::capture.output(nomologR:::nomo_present_table(
      data.frame(a = c("x", "y"), b = c("-", "-")), c("A" = "a", "B" = "b")
    )),
    c("  A", "  x", "  y")
  )
  # Unless the caller keeps it, as a decisive test column whose "--" a note
  # explains (#145, R-0b follow-up).
  expect_identical(
    utils::capture.output(nomologR:::nomo_present_table(
      data.frame(a = c("x", "y"), b = c("-", "-")), c("A" = "a", "B" = "b"), keep_empty = "b"
    )),
    c("  A  B", "  x  -", "  y  -")
  )

  # The table returns the labels it dropped, so a caller can leave out notes
  # on columns not shown; a label of several words is never broken.
  local_reproducible_output(width = 40)
  hidden <- NULL
  out <- utils::capture.output(hidden <- nomologR:::nomo_present_table(
    data.frame(item = "a1", long = strrep("x", 35), other = strrep("y", 30)),
    c("Item" = "item", "Ninechars" = "long", "Max SE bias" = "other"), more = "x$table"
  ))
  expect_identical(hidden, c("Ninechars", "Max SE bias"))
  # Unbound, the first line of the note would end on "Max".
  expect_identical(out, c("  Item", "  a1", "  Not shown for width: Ninechars,",
                          "  Max SE bias. See x$table."))
  shown <- NULL
  invisible(utils::capture.output(shown <- nomologR:::nomo_present_table(d, c("Item" = "item"),
                                                                        more = "x")))
  expect_identical(shown, character())
  expect_identical(nomologR:::nomo_present_table(d[0, ], c("Item" = "item"), more = "x"),
                   character())
})


test_that("tables that drop a column for width point to the call that shows it (#145)", {
  skip_on_cran()
  local_reproducible_output(width = 50)
  pointed <- function(lines) {
    text <- gsub("\\s+", " ", paste(lines, collapse = " "))
    notes <- regmatches(text, gregexpr("Not shown for width: [^.]+\\.( See [^.]+\\.)?", text))[[1L]]
    expect_gt(length(notes), 0L)
    expect_true(all(grepl("See nomo_table\\(x, \"[a-z_]+\"\\)\\.$", notes)), label = paste(notes))
  }

  set.seed(2026)
  n <- 500
  g <- rnorm(n)
  s <- matrix(rnorm(n * 3), n, 3)
  dat <- as.data.frame(sapply(1:9, function(i) .6 * g + .45 * s[, ceiling(i / 3)] + rnorm(n, sd = .65)))
  names(dat) <- paste0("x", 1:9)
  factors <- list(A = c("x1", "x2", "x3"), B = c("x4", "x5", "x6"), C = c("x7", "x8", "x9"))
  hier <- nomo_hierarchical(nomo_cfa(nomo_model(factors, structure = "bifactor"), data = dat))
  pointed(utils::capture.output(print(hier)))
  pointed(utils::capture.output(print(summary(hier))))

  true <- stats::rnorm(150)
  scores <- data.frame(agency_t1 = 3 + true + stats::rnorm(150, sd = .45),
                       agency_t2 = 3.5 + true + stats::rnorm(150, sd = .45))
  pointed(utils::capture.output(print(summary(nomo_retest(scores, c("agency_t1", "agency_t2"))))))

  inv <- nomo_invariance("Agency =~ ag1 + ag2 + ag3 + ag4", data = nomo_demo_network,
                         group = "group", levels = c("configural", "metric"))
  printed <- utils::capture.output(print(inv))
  pointed(printed)
  expect_match(printed[grepl("^  Level", printed)], "LRT p$")
})


test_that("the Flagged section, key, and pointer read as the guide shows (#144)", {
  local_reproducible_output(width = 80)
  flagged <- nomologR:::nomo_present_flagged
  log <- data.frame(
    object = c("b5", "B2", "C2", "x", "a1", NA),
    severity = c("review", "review", "review", "info", "concern", "review"),
    observation = c("Low loading", "Same text.", "Same text.", "Fine.", "Heywood case.", "A note."),
    recommendation = c("Look again.", "", "", "", "Check.", ""),
    stringsAsFactors = FALSE
  )
  expect_identical(
    utils::capture.output(flagged(log)),
    c("", "Flagged", "  - a1 (Concern): Heywood case.", "  - b5 (Review): Low loading.",
      "  - B2, C2 (Review): Same text.", "  - Review: A note.")
  )
  expect_identical(utils::capture.output(flagged(log, recommendation = TRUE))[3:4],
                   c("  - a1 (Concern): Heywood case. Check.",
                     "  - b5 (Review): Low loading. Look again."))
  expect_identical(
    utils::capture.output(flagged(unit = c("ag1", "ag2"), status = c("REVIEW", "STRONG REVIEW"),
                                  text = c("", NA))),
    c("", "Flagged", "  - ag2 (Concern)", "  - ag1 (Review)")
  )
  expect_identical(utils::capture.output(flagged(status = "KEEP")), character())

  # The recommendations may be given with `text`; an observation without a
  # full stop, such as a lavaan message, gets one before its recommendation,
  # and a missing recommendation adds nothing (#145, R-0b follow-up).
  expect_identical(
    utils::capture.output(flagged(unit = c("a", "b", "c"), status = rep("review", 3L),
                                  text = c("A lavaan message", "Plain.", "Alone"),
                                  recommendation = c("Check it.", "", NA))),
    c("", "Flagged", "  - a (Review): A lavaan message. Check it.", "  - b (Review): Plain.",
      "  - c (Review): Alone.")
  )
  then <- nomologR:::nomo_present_then_recommend
  expect_identical(then(c("No stop", "Ends?", NA, "Alone", ""), c("Do this.", "Do that.", "Only.", NA, "")),
                   c("No stop. Do this.", "Ends? Do that.", "Only.", "Alone", ""))
  # Grouped units: a later unit's leading "The" is lowercased; a construct
  # named A is not an article.
  expect_identical(
    utils::capture.output(flagged(
      unit = c("The Agency EFA", "The Persistence EFA", "B in validity", "A in validity"),
      status = rep("review", 4L), text = rep(c("Weak factor.", "Low AVE."), each = 2L)
    ))[3:4],
    c("  - The Agency EFA, the Persistence EFA (Review): Weak factor.",
      "  - B in validity, A in validity (Review): Low AVE.")
  )

  key <- utils::capture.output(nomologR:::nomo_present_key(c(
    SE = "Standard error",
    CI = paste("Confidence interval, from the profile likelihood, which is a long",
               "definition that wraps onto a second line."),
    Empty = ""
  )))
  expect_identical(key, c(
    "", "What these columns mean", "  SE -- Standard error.",
    "  CI -- Confidence interval, from the profile likelihood, which is a long",
    "      definition that wraps onto a second line."
  ))
  expect_identical(utils::capture.output(nomologR:::nomo_present_key(character())), character())

  # A call is never broken inside its parentheses.
  local_reproducible_output(width = 60)
  pointer <- utils::capture.output(nomologR:::nomo_present_pointer(
    c("summary(x)", "nomo_table(x, \"fit\")"), c("the flagged items", "every fit index")
  ))
  expect_identical(pointer, c(
    "", "See summary(x) for the flagged items and",
    "nomo_table(x, \"fit\") for every fit index."
  ))
  expect_identical(
    utils::capture.output(nomologR:::nomo_present_pointer("summary(x)", "more", blank = FALSE)),
    "See summary(x) for more."
  )
  expect_identical(utils::capture.output(nomologR:::nomo_present_pointer(character(), "x")),
                   character())
})


test_that("the code is ASCII, and the stated output rule allows accented names (#145)", {
  # An installed package keeps no R sources, so the check runs from the source
  # tree only.
  source_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(file.exists(file.path(source_dir, "nomo_present.R")))
  # Help text may hold an accented author name; the code itself never does.
  for (file in list.files(source_dir, pattern = "[.]R$", full.names = TRUE)) {
    code <- grep("^\\s*#", readLines(file, warn = FALSE), value = TRUE, invert = TRUE)
    expect_false(any(grepl("[^\x01-\x7F]", code)), label = basename(file))
  }
  # Proper names such as Muthen with its accent are printed as written, so the
  # helpers' stated rule says so rather than promising ASCII output.
  rules <- readLines(file.path(source_dir, "nomo_present.R"), warn = FALSE)
  expect_false(any(grepl("output is ASCII;", rules, fixed = TRUE)))
  expect_true(any(grepl("output is ASCII except proper names", rules, fixed = TRUE)))
})


test_that("every printed table names the call that shows a column it drops (#145)", {
  # Read from the source tree, as above: each nomo_present_table() call passes
  # `more`, so no "Not shown for width" note is left without a pointer.
  source_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(file.exists(file.path(source_dir, "nomo_present.R")))
  calls <- 0L
  for (file in list.files(source_dir, pattern = "[.]R$", full.names = TRUE)) {
    data <- utils::getParseData(parse(file, keep.source = TRUE))
    named <- data$parent[data$token == "SYMBOL_FUNCTION_CALL" &
                           data$text == "nomo_present_table"]
    for (call in data$parent[match(named, data$id)]) {
      calls <- calls + 1L
      args <- data[data$parent == call, ]
      expect_true(any(args$token == "SYMBOL_SUB" & args$text == "more"),
                  label = sprintf("%s:%d passes `more`", basename(file),
                                  data$line1[data$id == call]))
    }
  }
  expect_gt(calls, 50L)

  # A factor-retention summary on a narrow console: the evidence table drops
  # its role column and points to the table that holds it.
  set.seed(4503)
  f <- stats::rnorm(150)
  dat <- as.data.frame(replicate(5, 0.8 * f + stats::rnorm(150, sd = 0.6)))
  fac <- nomo_factors(dat, criterion_set = "minimal", n_iter = 10, seed = 78)
  local_reproducible_output(width = 40)
  text <- gsub("\\s+", " ", paste(utils::capture.output(print(summary(fac))), collapse = " "))
  notes <- regmatches(text, gregexpr("Not shown for width: [^.]+\\.( See [^ ]+ [^ ]+)?", text))[[1L]]
  expect_gt(length(notes), 0L)
  expect_true(all(grepl("See nomo_table\\(x, \"[a-z_]+\"\\)\\.$", notes)), label = paste(notes))
})


test_that("revised MAP is never described as an average of fourth powers (#145)", {
  # TR4 is the trace of the fourth matrix power of the partial-correlation
  # matrix (R/nomo_factors.R, the registry); every printed string that names a
  # fourth power says so.
  source_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(file.exists(file.path(source_dir, "nomo_present.R")))
  seen <- 0L
  for (file in list.files(source_dir, pattern = "[.]R$", full.names = TRUE)) {
    data <- utils::getParseData(parse(file, keep.source = TRUE))
    strings <- data$text[data$token == "STR_CONST" & grepl("fourth", data$text)]
    seen <- seen + length(strings)
    for (string in strings) {
      expect_true(grepl("matrix|trace", string), label = sprintf("%s: %s", basename(file), string))
    }
  }
  expect_gt(seen, 3L)
})


test_that("every pointer to a decision log names nomo_table() (#145, out-16)", {
  # Every class's log is a nomo_table() type, so no output points to the field.
  source_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(file.exists(file.path(source_dir, "nomo_present.R")))
  for (file in list.files(source_dir, pattern = "[.]R$", full.names = TRUE)) {
    data <- utils::getParseData(parse(file, keep.source = TRUE))
    strings <- data$text[data$token == "STR_CONST"]
    expect_false(any(grepl("^.x[$]decision_log.$", strings)), label = basename(file))
  }
})


# `digits` in print() (#145, clarity-22) ----------------------------------------

test_that("every print method takes `digits`, NULL by default (#145)", {
  methods <- getNamespaceInfo(asNamespace("nomologR"), "S3methods")
  printed <- methods[methods[, 1L] == "print", 3L]
  expect_gt(length(printed), 40L)
  for (method in printed) {
    f <- get(method, envir = asNamespace("nomologR"))
    expect_identical(names(formals(f)), c("x", "digits", "..."), label = method)
    expect_null(formals(f)$digits, label = method)
  }
})


test_that("`digits` reaches every statistic but p, df, levels, counts, and percentages (#145)", {
  shown <- function(digits) {
    nomologR:::nomo_present_local_digits(digits)
    stat <- nomologR:::nomo_present_stat
    c(stat(0.4567, "r"), stat(0.4567, "fit"), stat(1.23456, "estimate", digits = 3L),
      stat(0.04567, "p"), stat(33.4667, "df"), stat(0.05, "level"), stat(473, "count"),
      nomologR:::nomo_present_percent(0.5, base = 50),
      nomologR:::nomo_present_ci(0.1, 0.2, kind = "r"),
      # A value just off its reference still shows the decimals that tell them
      # apart.
      stat(0.398, "loading", reference = 0.40))
  }
  expect_identical(shown(NULL), c(".46", "0.457", "1.235", ".046", "33.47", ".05", "473",
                                  "50%", "[.10, .20]", "0.398"))
  expect_identical(shown(4), c(".4567", "0.4567", "1.2346", ".046", "33.47", ".05", "473",
                               "50%", "[.1000, .2000]", "0.3980"))
  expect_identical(shown(1), c(".5", "0.5", "1.2", ".046", "33.47", ".05", "473",
                               "50%", "[.1, .2]", "0.398"))

  # Only for the call that set it, however that call ends.
  expect_null(nomologR:::nomo_present_digits())
  failing <- function() {
    nomologR:::nomo_present_local_digits(2)
    stop("stopped")
  }
  expect_error(failing(), "stopped")
  expect_null(nomologR:::nomo_present_digits())

  # NULL keeps the setting of the call around it.
  outer <- function() {
    nomologR:::nomo_present_local_digits(3)
    inner <- function() {
      nomologR:::nomo_present_local_digits(NULL)
      nomologR:::nomo_present_digits()
    }
    c(inner(), nomologR:::nomo_present_digits())
  }
  expect_identical(outer(), c(3L, 3L))

  for (bad in list(-1, 1.5, 11, "2", c(1, 2), NA_real_)) {
    expect_error(nomologR:::nomo_present_local_digits(bad),
                 "`digits` must be NULL or one whole number from 0 to 10.", fixed = TRUE)
    expect_error(nomologR:::nomo_present_digits_unused(bad, "why"),
                 "`digits` must be NULL or one whole number from 0 to 10.", fixed = TRUE)
  }
  expect_silent(nomologR:::nomo_present_digits_unused(NULL, "why"))
  expect_message(nomologR:::nomo_present_digits_unused(2, "a reason."),
                 "`digits` changes nothing here: a reason.", fixed = TRUE)
})


test_that("`digits` reaches plain table columns; a region's bounds keep their own (#145)", {
  table <- function(digits) {
    nomologR:::nomo_present_local_digits(digits)
    utils::capture.output(nomologR:::nomo_present_table(
      data.frame(item = "a1", value = 0.12345, n = 5L),
      c("Item" = "item", "Value" = "value", "N" = "n"),
      more = "x"
    ))
  }
  expect_identical(table(NULL)[[2L]], "  a1    0.123  5")
  expect_identical(table(1)[[2L]], "  a1      0.1  5")

  bounds <- function(digits) {
    nomologR:::nomo_present_local_digits(digits)
    nomologR:::nomo_network_bound_text(c(0.2, -0.15, 0.125, 0), c("estimate", "r", "r", "r"))
  }
  expect_identical(bounds(NULL), c("0.20", "-.15", ".125", "0"))
  # Never fewer decimals than given, and more when asked.
  expect_identical(bounds(1), c("0.20", "-.15", ".125", "0"))
  expect_identical(bounds(4), c("0.2000", "-.1500", ".1250", "0"))
})


# Cases (#145, clarity-08) ------------------------------------------------------

test_that("reliability, validity, and hierarchical printouts give the cases analyzed (#145)", {
  # Ten cases lose x1, all of them in the first school, so lavaan's listwise
  # deletion leaves 291 of the 301; the count is summed over the groups.
  hs <- lavaan::HolzingerSwineford1939
  hs$x1[1:10] <- NA
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6", data = hs,
                     group = "school")
  expect_identical(nomologR:::nomo_lavaan_cases(fit), list(n_used = 291, data_n = 301))
  cases <- nomologR:::nomo_lavaan_cases(fit)
  expect_identical(nomologR:::nomo_lavaan_cases_fact(cases), "Cases: 291 of 301 used")

  local_reproducible_output(width = 80)
  rel <- nomo_reliability(fit)
  expect_true("Cases: 291 of 301 used | Constructs: 2" %in% capture.output(print(rel)))
  expect_identical(summary(rel)[c("n_used", "data_n")], list(n_used = 291, data_n = 301))
  expect_match(capture.output(print(summary(rel))),
               "^Cases: 291 of 301 used \\| Constructs: 2 \\| Review reference: \\.70$",
               all = FALSE)

  # A model fitted from a covariance matrix analyzed the sample size it was
  # given.
  covariance <- lavaan::cfa(
    "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6",
    sample.cov = stats::cov(lavaan::HolzingerSwineford1939[paste0("x", 1:6)]),
    sample.nobs = 301
  )
  expect_identical(nomologR:::nomo_lavaan_cases_fact(nomologR:::nomo_lavaan_cases(covariance)),
                   "Cases: 301")

  # An object saved without its fit prints without the fact, rather than
  # reading another field for it.
  unfitted <- rel
  unfitted$fit <- NULL
  expect_identical(nomologR:::nomo_lavaan_cases(unfitted[["fit"]]),
                   list(n_used = NA_real_, data_n = NA_real_))
  printed <- capture.output(print(unfitted))
  expect_identical(printed[[2L]], "Constructs: 2")
  expect_false(any(grepl("^Cases", capture.output(print(summary(unfitted))))))
  expect_identical(nomologR:::nomo_lavaan_cases_fact(list()), "")
})
