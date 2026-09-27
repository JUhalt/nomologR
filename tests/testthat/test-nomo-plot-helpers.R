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
