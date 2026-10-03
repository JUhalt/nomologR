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
  kept <- nomologR:::nomo_plot_labs(caption = "First line\nsecond line")
  expect_identical(kept$caption, "First line\nsecond line")
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
  expect_identical(nomologR:::nomo_plot_bounded_labels(c(0.125, 0.0001), digits = 2L),
                   c(".13", "0"))
})
