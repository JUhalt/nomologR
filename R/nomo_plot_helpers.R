# Shared plot helpers (#89) -------------------------------------------------------
#
# ggplot2 does not wrap titles, so a long subtitle or caption ran off the edge of
# a plot drawn at an ordinary size. Every plot sets its text through
# nomo_plot_labs(), which wraps each at a width that fits a 7-inch plot whose
# panel begins after the axis labels: about 60 characters for the title, 75 for
# the subtitle, and 85 for the smaller caption. Text that already contains a
# line break is left as written.
#
# A title is a name and takes no period. A subtitle and a caption are
# sentences, so each ends with a period (#145, pres-17): one that ends without
# a period, question mark, or exclamation mark gets one.
nomo_plot_labs <- function(...) {
  args <- list(...)
  widths <- c(title = 60L, subtitle = 75L, caption = 85L)
  for (nm in intersect(names(widths), names(args))) {
    text <- args[[nm]]
    if (!is.character(text) || length(text) != 1L || is.na(text)) next
    if (nm != "title") text <- nomo_plot_sentence(text)
    if (!grepl("\n", text, fixed = TRUE)) {
      text <- paste(strwrap(text, width = widths[[nm]]), collapse = "\n")
    }
    args[[nm]] <- text
  }
  do.call(ggplot2::labs, args)
}


# Text that ends as a sentence does: trailing spaces dropped, and a period added
# unless it already ends with one, a question mark, or an exclamation mark.
nomo_plot_sentence <- function(text) {
  text <- sub("[[:space:]]+$", "", text)
  if (nzchar(text) && !grepl("[.?!]$", text)) text <- paste0(text, ".")
  text
}


# Status in plots (#144) ----------------------------------------------------------
#
# Every plot that marks a status draws it the same way, as contentvalidR does
# (guide points 27 to 29). The shape carries the status and the color repeats
# it, so neither is needed alone and the plot reads in grayscale. Only a status
# that needs attention is warm; no flag is a neutral gray. The two warm colors
# are from the Okabe-Ito colorblind-safe palette.
#
#   status        shape               color
#   no flag       filled circle (16)  neutral gray
#   review        open circle (1)     #E69F00 (orange)
#   concern       filled square (15)  #D55E00 (vermillion)
#   not computed  cross (4)           light gray
#
# Usage, for a plot of rows with a stored status column:
#
#   dat$status <- nomo_plot_status(dat$attention)
#   ggplot2::ggplot(dat, ggplot2::aes(x, y, shape = status, colour = status)) +
#     ggplot2::geom_point() +
#     nomo_plot_status_scales(dat$status, name = "Flag") +
#     ggplot2::scale_x_continuous(labels = nomo_plot_bounded_labels)
#
# nomo_plot_status_scales() gives one legend for shape and color, listing only
# the statuses drawn, and leaves it out only when every point has no flag: a
# legend appears whenever a non-default status is drawn, even a single one.
# nomo_plot_bounded_labels() labels the axis of a statistic that cannot exceed
# 1 without the leading zero (0, .25, .50, .75, 1.00), with a third or fourth
# decimal only when a break needs it (.100, .125, .150).

nomo_plot_status_levels <- c("none", "review", "concern", "not computed")

nomo_plot_status_labels <- c(
  "none" = "No flag", "review" = "Review", "concern" = "Concern",
  "not computed" = "Not computed"
)

nomo_plot_status_shapes <- c("none" = 16, "review" = 1, "concern" = 15, "not computed" = 4)

nomo_plot_status_colors <- c(
  "none" = "#595959", "review" = "#E69F00", "concern" = "#D55E00",
  "not computed" = "#8C8C8C"
)


# A stored status in any of the package's vocabularies as a plot status: a
# factor with the four levels in order of the table above. A word outside them
# is missing.
nomo_plot_status <- function(x) {
  flag <- nomo_present_flag(x)
  flag[!nzchar(flag)] <- "none"
  factor(flag, levels = nomo_plot_status_levels)
}


# Whether a legend is needed: some point carries a status other than no flag.
nomo_plot_show_legend <- function(status) {
  status <- as.character(status)
  any(!is.na(status) & status != "none")
}


# The shape and color scales for a status, with one legend titled `name` that
# lists the statuses drawn in order, or no legend when nothing is flagged.
nomo_plot_status_scales <- function(status, name = "Flag") {
  if (!is.factor(status)) status <- nomo_plot_status(status)
  drawn <- unique(as.character(status[!is.na(status)]))
  breaks <- nomo_plot_status_levels[nomo_plot_status_levels %in% drawn]
  labels <- unname(nomo_plot_status_labels[breaks])
  scales <- list(
    ggplot2::scale_shape_manual(name = name, values = nomo_plot_status_shapes,
                                breaks = breaks, labels = labels, drop = TRUE),
    ggplot2::scale_colour_manual(name = name, values = nomo_plot_status_colors,
                                 breaks = breaks, labels = labels, drop = TRUE)
  )
  if (!nomo_plot_show_legend(status)) {
    scales <- c(scales, list(ggplot2::guides(shape = "none", colour = "none")))
  }
  scales
}


# Axis labels for a bounded statistic in APA style: 0, .25, .50, .75, 1.00.
# The breaks take the fewest decimals, at least `digits` and at most 4, that
# show every break as it is, so a gridline at .125 reads .125, not .13. The
# tolerance absorbs the binary error of computed breaks such as 0.1 + 0.05.
nomo_plot_bounded_labels <- function(x, digits = 2L) {
  x <- suppressWarnings(as.numeric(x))
  breaks <- x[is.finite(x)]
  inexact <- function(d) any(abs(nomo_present_round(breaks, d) - breaks) > 1e-8)
  while (digits < 4L && inexact(digits)) digits <- digits + 1L
  out <- nomo_present_number(x, digits, bounded = TRUE)
  out[is.finite(x) & nomo_present_round(x, digits) == 0] <- "0"
  out[!is.finite(x)] <- NA_character_
  out
}


# Global fit (#145, clarity-13) ---------------------------------------------------
#
# plot(x, type = "fit") of nomo_cfa(), nomo_network(), and nomo_compare() is one
# design, drawn by nomo_plot_fit_indices():
#
# * Each index has a panel and a scale of its own, since CFI and TLI sit near 1
#   and RMSEA and SRMR near 0. The strip names the direction that favors fit:
#   CFI and TLI on the top row, RMSEA and SRMR below.
# * Each scale reaches the index's value under perfect fit, 1 or 0, which is
#   the axis's only label.
# * Each model is a row, named on the axis when there is more than one.
# * A dashed line marks the review reference, labeled at the foot of the panel
#   on the side with more room, and a gray segment joins it to each value.
# * Each value is printed above its point in its own format: CFI cannot exceed
#   1 and loses its leading zero, while TLI, RMSEA, and SRMR keep it (guide
#   point 8). One axis label function serves every panel and could not tell
#   them apart, so the values are printed rather than read off the axis.
# * The point carries the status in the shared shapes and colors.
# * The subtitle and the caption use the same words for every class.

nomo_plot_fit_metrics <- c("CFI", "TLI", "RMSEA", "SRMR")


# The status of each fit index against its reference, by the rule nomo_cfa()
# flags with: a CFI or TLI below its reference, or an RMSEA or SRMR above it,
# is marked for review. An index without a reference is not flagged.
nomo_plot_fit_status <- function(metric, value, reference) {
  beyond <- is.finite(value) & is.finite(reference) &
    ifelse(metric %in% c("CFI", "TLI"), value < reference, value > reference)
  nomo_plot_status(ifelse(beyond, "review", "none"))
}


# `dat` has a row per model and index, with `model`, `metric` (one of
# nomo_plot_fit_metrics), `value`, `reference` (NA when none is configured),
# and `status`, a plot status. Rows without a finite value are not drawn; the
# caller stops first when none is left. Models are drawn top to bottom in their
# order in `dat`. `values` says what the points are, and `extra` adds sentences
# to the caption.
nomo_plot_fit_indices <- function(dat, title, values, extra = NULL) {
  dat <- dat[dat$metric %in% nomo_plot_fit_metrics & is.finite(dat$value), , drop = FALSE]
  models <- unique(as.character(dat$model))
  dat$row <- match(as.character(dat$model), rev(models))
  strips <- paste0(nomo_plot_fit_metrics, ": ",
                   c("higher", "higher", "lower", "lower"), " values favor fit")
  dat$panel <- factor(strips[match(dat$metric, nomo_plot_fit_metrics)], levels = strips)
  dat$value_label <- nomo_cfa_fit_number(dat$metric, dat$value, dat$reference)
  dat$reference_label <- ifelse(
    is.finite(dat$reference),
    paste("reference", nomo_cfa_fit_number(dat$metric, dat$reference)),
    ""
  )

  # Each panel reaches the index's value under perfect fit, 1 for CFI and TLI
  # and 0 for RMSEA and SRMR, so how far a value and its reference sit from it
  # can be read.
  anchors <- data.frame(panel = factor(strips, levels = strips), value = c(1, 1, 0, 0), row = 1)
  anchors <- anchors[anchors$panel %in% dat$panel, , drop = FALSE]

  # One reference per panel. Its label goes on the side of the line with more
  # of the panel's width, so it stays inside the panel.
  refs <- dat[is.finite(dat$reference), , drop = FALSE]
  refs <- refs[!duplicated(refs$panel), , drop = FALSE]
  middle <- vapply(as.character(refs$panel), function(p) {
    mean(range(c(dat$value[dat$panel == p], refs$reference[refs$panel == p],
                 anchors$value[anchors$panel == p])))
  }, numeric(1))
  refs$hjust <- ifelse(refs$reference > middle, 1.08, -0.08)
  refs$foot <- rep(-Inf, nrow(refs))

  references <- if (nrow(refs)) {
    paste0(
      "Dashed lines are review references: ",
      paste(refs$metric, nomo_cfa_fit_number(refs$metric, refs$reference), collapse = ", "),
      ". They are not cutoffs."
    )
  }
  caption <- paste(c(
    references,
    "Interpret global fit with local strain, estimator, sample, and theory.",
    extra[nzchar(extra)]
  ), collapse = " ")

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = value, y = row)) +
    ggplot2::geom_blank(data = anchors) +
    ggplot2::geom_vline(data = refs, ggplot2::aes(xintercept = reference), linetype = 2) +
    ggplot2::geom_segment(
      ggplot2::aes(x = reference, xend = value, yend = row),
      colour = "grey60", na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = refs,
      ggplot2::aes(x = reference, y = foot, label = reference_label, hjust = hjust),
      vjust = -0.6, size = 3, colour = "grey30"
    ) +
    ggplot2::geom_point(ggplot2::aes(shape = status, colour = status), size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = value_label), vjust = -1.1, size = 3.4) +
    nomo_plot_status_scales(dat$status, name = "Flag") +
    ggplot2::facet_wrap(ggplot2::vars(panel), ncol = 2L, scales = "free_x") +
    ggplot2::scale_x_continuous(
      breaks = nomo_plot_fit_breaks, labels = as.character,
      expand = ggplot2::expansion(mult = 0.15)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(models), labels = rev(models),
      limits = c(0.3, length(models) + 0.7), expand = c(0, 0)
    ) +
    nomo_plot_labs(
      title = title,
      subtitle = paste0(
        "Points are ", values, ". Each index has its own scale, which ends at ",
        "its value under perfect fit."
      ),
      x = NULL, y = NULL,
      caption = caption
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(1, "lines")
    )
  # A single model needs no name on its row.
  if (length(models) < 2L) p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  p
}


# The one break of a global-fit panel: the value under perfect fit, 1 or 0,
# whichever the panel's limits hold.
nomo_plot_fit_breaks <- function(limits) {
  c(0, 1)[c(0, 1) >= limits[[1L]] & c(0, 1) <= limits[[2L]]]
}


utils::globalVariables(c(
  "value", "row", "panel", "status", "reference", "value_label",
  "reference_label", "hjust", "foot"
))
