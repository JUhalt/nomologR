# Shared plot helpers (#89) -------------------------------------------------------

test_that("plot titles, subtitles, and captions are wrapped to fit the plot", {
  long <- paste(rep("evidence", 20), collapse = " ")
  labs <- nomologR:::nomo_plot_labs(
    title = long, subtitle = long, caption = long, x = long,
    y = NULL, shape = "Review"
  )
  lines <- function(x) strsplit(x, "\n", fixed = TRUE)[[1L]]

  expect_true(all(nchar(lines(labs$title)) <= 60L))
  expect_true(all(nchar(lines(labs$subtitle)) <= 75L))
  expect_true(all(nchar(lines(labs$caption)) <= 85L))
  expect_gt(length(lines(labs$caption)), 1L)
  # Only the three text labels are wrapped; axis and legend titles are not.
  expect_identical(labs$x, long)
  expect_identical(labs$shape, "Review")

  # Text the author already broke is left as written.
  kept <- nomologR:::nomo_plot_labs(caption = "First line\nsecond line.")
  expect_identical(kept$caption, "First line\nsecond line.")
})


test_that("status is carried by shape and repeated in color (#144)", {
  status <- nomologR:::nomo_plot_status(
    c("KEEP", "REVIEW", "STRONG REVIEW", "unavailable", "info", "note")
  )
  expect_identical(as.character(status),
                   c("none", "review", "concern", "not computed", "none", NA))
  expect_identical(levels(status), c("none", "review", "concern", "not computed"))
  expect_identical(unname(nomologR:::nomo_plot_status_shapes), c(16, 1, 15, 4))
  expect_identical(unname(nomologR:::nomo_plot_status_colors[c("review", "concern")]),
                   c("#E69F00", "#D55E00"))
  expect_identical(nomologR:::nomo_present_flag_shapes,
                   nomologR:::nomo_plot_status_shapes[c("none", "review", "concern")])

  expect_false(nomologR:::nomo_plot_show_legend(c("none", "none", NA)))
  expect_true(nomologR:::nomo_plot_show_legend("review"))
  # A raw status vector is read the same way; without a flag there is no legend.
  expect_length(nomologR:::nomo_plot_status_scales(c("KEEP", "REVIEW")), 2L)
  expect_length(nomologR:::nomo_plot_status_scales(c("KEEP", "info")), 3L)

  skip_on_cran()
  draw <- function(flags) {
    dat <- data.frame(x = seq_along(flags), y = seq_along(flags) / 10,
                      status = nomologR:::nomo_plot_status(flags))
    ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y, shape = status, colour = status)) +
      ggplot2::geom_point() +
      nomologR:::nomo_plot_status_scales(dat$status, name = "Flag") +
      ggplot2::scale_y_continuous(labels = nomologR:::nomo_plot_bounded_labels)
  }
  legend <- function(p, aesthetic) ggplot2::get_guide_data(p, aesthetic)$.label

  # One legend for shape and color, listing only what is drawn, in order.
  p <- draw(c("concern", "KEEP", "review"))
  expect_identical(legend(p, "shape"), c("No flag", "Review", "Concern"))
  expect_identical(legend(p, "colour"), c("No flag", "Review", "Concern"))
  built <- ggplot2::ggplot_build(p)$data[[1L]]
  expect_identical(built$shape, c(15, 16, 1))
  expect_identical(built$colour, c("#D55E00", "#595959", "#E69F00"))

  # A single non-default status still gets its legend; only "no flag" alone
  # goes without one.
  expect_identical(legend(draw(c("review", "REVIEW")), "shape"), "Review")
  expect_identical(legend(draw("unavailable"), "shape"), "Not computed")
  expect_null(ggplot2::get_guide_data(draw(c("KEEP", "info")), "shape"))
})


test_that("bounded axes are labeled without the leading zero (#144)", {
  expect_identical(
    nomologR:::nomo_plot_bounded_labels(c(0, 0.25, 0.5, 0.75, 1, -0.5, NA)),
    c("0", ".25", ".50", ".75", "1.00", "-.50", NA)
  )
  # A break that two decimals would round is shown with the decimals it needs,
  # so a gridline at .125 never reads .13 (#145). The tolerance keeps the
  # binary error of computed breaks from asking for more.
  labels <- nomologR:::nomo_plot_bounded_labels
  expect_identical(labels(seq(0.1, 0.2, by = 0.025)),
                   c(".100", ".125", ".150", ".175", ".200"))
  expect_identical(labels(c(-0.1, 0.3 - 0.1 - 0.2, 0.1, NA)), c("-.10", "0", ".10", NA))
  expect_identical(labels(c(0.0025, 0.005, 1 / 3)), c(".0025", ".0050", ".3333"))
  expect_identical(labels(c(0.25, 0.5), digits = 3L), c(".250", ".500"))
  expect_identical(labels(c(NA, Inf)), c(NA_character_, NA_character_))

  skip_on_cran()
  p <- ggplot2::ggplot(data.frame(x = 1:2, y = c(0.1, 0.2)), ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(labels = labels)
  shown <- ggplot2::ggplot_build(p)$layout$panel_params[[1L]]$y$get_labels()
  expect_identical(shown[!is.na(shown)], c(".100", ".125", ".150", ".175", ".200"))
})


test_that("subtitles and captions end as sentences; titles do not (#145, pres-17)", {
  labs <- nomologR:::nomo_plot_labs(
    title = "A plot title", subtitle = "Unique off-diagonal pairs",
    caption = "Not drawn: x1 (see x$notes) ", x = "Axis"
  )
  expect_identical(labs$title, "A plot title")
  expect_identical(labs$subtitle, "Unique off-diagonal pairs.")
  expect_identical(labs$caption, "Not drawn: x1 (see x$notes).")
  expect_identical(labs$x, "Axis")
  # Text that already ends a sentence is kept, and a hand-broken caption gets
  # its period too.
  kept <- nomologR:::nomo_plot_labs(subtitle = "Is it?", caption = "One.\nTwo")
  expect_identical(kept$subtitle, "Is it?")
  expect_identical(kept$caption, "One.\nTwo.")
  expect_identical(nomologR:::nomo_plot_sentence(""), "")
  expect_null(nomologR:::nomo_plot_labs(caption = NULL)$caption)
})


test_that("every global-fit plot shares one design (#145, clarity-13)", {
  status <- nomologR:::nomo_plot_fit_status(
    c("CFI", "TLI", "RMSEA", "SRMR", "SRMR"), c(.94, .96, .07, .05, .09),
    c(.95, .95, .06, .08, NA)
  )
  expect_identical(as.character(status), c("review", "none", "review", "none", "none"))
  expect_identical(nomologR:::nomo_plot_fit_breaks(c(.9, 1.01)), 1)
  expect_identical(nomologR:::nomo_plot_fit_breaks(c(0, .1)), 0)
  expect_identical(nomologR:::nomo_plot_fit_breaks(c(.3, .9)), numeric())

  skip_on_cran()
  dat <- data.frame(
    model = rep(c("one", "two"), each = 4L),
    metric = rep(c("CFI", "TLI", "RMSEA", "SRMR"), 2L),
    value = c(.97, .93, .05, NA, .99, .96, .02, .03),
    reference = c(.95, .95, .06, .08, .95, .95, .06, .08)
  )
  dat$status <- nomologR:::nomo_plot_fit_status(dat$metric, dat$value, dat$reference)
  versions <- data.frame(CFI = c("robust", "robust"), TLI = c("robust", "robust"),
                         RMSEA = c("robust", ""))
  p <- nomologR:::nomo_plot_fit_indices(dat, title = "Fit", values = "the values",
                                       versions = versions, extra = c("", "More."))
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$title, "Fit")
  # The subtitle makes no claim about where a scale ends: TLI can exceed 1.
  expect_identical(plot_text(p$labels$subtitle),
                   "Points are the values. Each index has its own scale.")
  # The versions sentence follows the shared sentences, and an index whose
  # version differs across models names each version.
  expect_identical(
    plot_text(p$labels$caption),
    paste("Dashed lines are review references: CFI .950, TLI 0.950, RMSEA 0.060, SRMR 0.080.",
          "They are not cutoffs. Interpret global fit with local strain, estimator, sample,",
          "and theory. CFI and TLI are robust values. RMSEA is a robust or standard value.",
          "More.")
  )
  # A value that is not finite is not drawn; the first model is the top row.
  expect_identical(nrow(p$data), 7L)
  expect_identical(p$data$row[p$data$model == "one"], rep(2L, 3L))
  built <- ggplot2::ggplot_build(p)
  expect_identical(built$layout$panel_params[[1L]]$y$get_labels(), c("two", "one"))
  # Each panel's only axis label is 1 for CFI and TLI and 0 for RMSEA and SRMR.
  expect_identical(built$layout$panel_params[[1L]]$x$get_labels(), "1")
  expect_identical(built$layout$panel_params[[3L]]$x$get_labels(), "0")
  # The room above and below the rows grows with the number of models, and
  # nothing is clipped at the panel's edge.
  expect_equal(built$layout$panel_params[[1L]]$y.range, c(0.25, 2.85))
  expect_identical(p$coordinates$clip, "off")
  # The reference label sits on the wider side of its dashed line.
  geoms <- vapply(p$layers, function(l) class(l$geom)[[1L]], character(1))
  refs <- ggplot2::layer_data(p, 4L)
  expect_identical(refs$label, c("reference .950", "reference 0.950", "reference 0.060",
                                 "reference 0.080"))
  expect_identical(refs$hjust, c(-0.08, -0.08, 1.08, 1.08))
  # Each value sits above its point on a borderless patch of the background,
  # which hides the dashed line behind its digits. The points are drawn last.
  expect_identical(unname(geoms[5:6]), c("GeomLabel", "GeomPoint"))
  values <- ggplot2::layer_data(p, 5L)
  expect_setequal(values$label, c(".970", "0.930", "0.050", ".990", "0.960", "0.020", "0.030"))
  expect_identical(unique(values$vjust), -0.5)
  border <- if (utils::packageVersion("ggplot2") >= "4.0.0") {
    p$layers[[5L]]$aes_params$linewidth
  } else {
    p$layers[[5L]]$geom_params$label.size
  }
  expect_identical(border, 0)

  # One model has no row names, and with no reference the caption names none.
  one <- dat[dat$model == "one", , drop = FALSE]
  one$reference <- NA_real_
  q <- nomologR:::nomo_plot_fit_indices(one, title = "Fit", values = "the values")
  expect_true(inherits(q$theme$axis.text.y, "element_blank"))
  expect_identical(
    plot_text(q$labels$caption),
    "Interpret global fit with local strain, estimator, sample, and theory."
  )
  expect_identical(nrow(ggplot2::layer_data(q, 2L)), 0L)
  expect_equal(ggplot2::ggplot_build(q)$layout$panel_params[[1L]]$y.range, c(0.3, 1.7))
  # One reference is named in the singular (guide point 2).
  one$reference[[1L]] <- 0.95
  expect_match(
    plot_text(nomologR:::nomo_plot_fit_indices(one, title = "Fit", values = "x")$labels$caption),
    "^The dashed line is a review reference: CFI .950. It is not a cutoff. Interpret"
  )

  # The versions sentence: standard values name no version, and a shared
  # version is named once for the three indices.
  note <- nomologR:::nomo_plot_fit_versions_note
  expect_identical(note(NULL), "")
  expect_identical(note(data.frame(CFI = "", TLI = "", RMSEA = "")), "")
  expect_identical(note(data.frame(CFI = "scaled", TLI = "scaled", RMSEA = "scaled")),
                   "CFI, TLI, and RMSEA are scaled values.")
})
