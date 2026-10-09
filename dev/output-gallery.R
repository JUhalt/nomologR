# Output gallery (#89) ----------------------------------------------------------
#
# Regenerates every print() and summary() a user sees, at a fixed console
# width, and every plot type, then flags presentation faults.
#
# Console output, one row per output in faults.csv:
#
#   over_width  lines wider than the console
#   truncated   cells cut off with an ellipsis
#   hidden      tibble columns hidden behind "n more variables"
#   type_rows   tibble column-type rows such as <chr> and <dbl>
#   error       the call stopped where it should have printed
#   dropped     tables that left columns out ("Not shown for width: ..."). It
#               is counted at every width and is a fault at 80 columns or
#               more, where a table should show every column it has. On a
#               narrower console a table drops columns by design and names
#               them, so there the count is information.
#
# Plots, one row per plot type in plot-text.csv:
#
#   title, subtitle, caption   the longest line of each, in characters; a
#               label of several lines is measured line by line (#145)
#   lines       the most lines any of the three has
#   overflow_in how far the widest label runs past the edge of a plot drawn
#               at 7 x 5 inches, in inches (0 when all of them fit)
#   long        TRUE when a label is clipped at that size
#   error       the message of a plot type that stopped. It is a fault: each
#               class is drawn from a fixture that supports all of its types.
#
# Usage, from the package root:
#   Rscript dev/output-gallery.R [output directory] [console width]
#
# The directory receives gallery.txt (all output), faults.csv, plots/ (PNG
# files), and plot-text.csv. The last lines printed are the counts to report.

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args) >= 1L) args[[1L]] else file.path(tempdir(), "nomo_gallery")
width <- if (length(args) >= 2L) as.integer(args[[2L]]) else 80L

pkgload::load_all(".", quiet = TRUE)
unlink(out, recursive = TRUE)
dir.create(file.path(out, "plots"), recursive = TRUE)
options(width = width)
set.seed(2026)

gallery <- file.path(out, "gallery.txt")
faults <- list()
show <- function(label, expr) {
  stopped <- FALSE
  lines <- tryCatch(
    utils::capture.output(expr),
    error = function(e) {
      stopped <<- TRUE
      paste("ERROR:", conditionMessage(e))
    }
  )
  # The note can wrap on a narrow console, so it is counted in the joined text.
  joined <- paste(lines, collapse = " ")
  faults[[label]] <<- data.frame(
    output = label,
    lines = length(lines),
    over_width = sum(nchar(lines, type = "width") > width),
    truncated = sum(grepl("…|\\.\\.\\.$", lines)),
    hidden = sum(grepl("more variable", lines)),
    type_rows = sum(grepl("<(chr|dbl|int|lgl|ord|fct)>", lines)),
    error = as.integer(stopped),
    dropped = lengths(regmatches(joined, gregexpr("Not shown for[[:space:]]+width:", joined)))
  )
  cat(sprintf("\n\n######## %s ########\n", label), file = gallery, append = TRUE)
  cat(lines, sep = "\n", file = gallery, append = TRUE)
}

# Fixtures -----------------------------------------------------------------------
cont <- nomo_demo_continuous
ab <- list(A = paste0("a", 1:5), B = paste0("b", 1:5))
scales3 <- list(
  Agency = paste0("ag", 1:4),
  Persistence = paste0("pe", 1:4),
  SocialDesirability = paste0("sd", 1:3)
)
h <- nomo_hypotheses(
  "Agency -> Persistence" = positive(min = .20),
  "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15)),
  "Agency -> Performance" = positive()
)

scr <- nomo_screen(cont)
fac <- nomo_factors(cont, seed = 2026)
efa <- nomo_efa(cont, factors = fac)
cfa <- nomo_cfa(nomo_model(ab), data = cont)
rel <- nomo_reliability(cfa)
val <- nomo_validity(cfa)
sc <- nomo_scores(cfa, method = "sum")
mis <- nomo_missing(cfa, data = cont)
no_b5 <- nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + 0*b5", data = cont)
cmp <- nomo_compare(full = cfa, no_b5 = no_b5, rationale = "Is b5 needed?")
inv <- nomo_invariance("Agency =~ ag1 + ag2 + ag3 + ag4", data = nomo_demo_network,
                       group = "group", levels = c("configural", "metric", "scalar"))
part <- nomo_partial(level = "scalar", syntax = "ag3 ~ 1",
                     rationale = "Anticipated mode difference.")
net <- nomo_network(nomo_model(scales3), data = nomo_demo_network, hypotheses = h)
sp <- nomo_split(nomo_demo_network, validation_prop = 0.4, seed = 2026)
# The same network with a validation sample (#145), so the replication table
# and plot(net_val, type = "replication") reach the gallery.
net_val <- nomo_network(nomo_model(scales3), data = sp$calibration, hypotheses = h,
                        validation_data = sp$validation)
# Ordered indicators (#145): the five-category version of `cont`, stored as
# ordered factors, for the WLSMV wording and the ordinal coefficients.
ord <- nomo_demo_ordinal
cfa_ord <- nomo_cfa(nomo_model(ab), data = ord)
rel_ord <- nomo_reliability(cfa_ord)
val_ord <- nomo_validity(cfa_ord)

set.seed(2026)
n <- 500
g <- rnorm(n)
s <- matrix(rnorm(n * 3), n, 3)
hd <- as.data.frame(sapply(1:9, function(i) .6 * g + .45 * s[, ceiling(i / 3)] + rnorm(n, sd = .65)))
names(hd) <- paste0("x", 1:9)
hf <- list(A = c("x1", "x2", "x3"), B = c("x4", "x5", "x6"), C = c("x7", "x8", "x9"))
hier <- nomo_hierarchical(nomo_cfa(nomo_model(hf, structure = "bifactor"), data = hd))

run_paused <- nomo_run(nomo_demo_network, scales = scales3,
                       settings = list(factors = list(seed = 2026)))
run_done <- nomo_run(
  nomo_demo_network, scales = scales3,
  decisions = list(
    factor_count = c(Agency = 1, Persistence = 1, SocialDesirability = 1),
    cfa_model = nomo_model(scales3), measurement_model = "proceed"
  ),
  settings = list(
    factors = list(seed = 2026), network = list(hypotheses = h),
    screen = list(effort = TRUE), scores = list(method = "sum"), missing = list()
  )
)

# Gap-review features (#129): ESEM, marker-based method variance, test-retest
# reliability, power, latent means, longitudinal invariance, and single
# indicators.
esem <- nomo_esem(nomo_model(ab), data = cont)

# Two constructs and a three-item marker that share a method factor loading
# .30 on every item. The data are drawn with base R, as in the
# "method-variance" chunk of vignettes/measurement-model-evidence.Rmd, and not
# with lavaan::simulateData(): lavaan 0.7-3 changed that function's default
# generator, so a seed gave other data than under earlier versions. Drawn this
# way the gallery is the same under every lavaan version.
survey <- local({
  set.seed(2010)
  n <- 600

  A <- rnorm(n)
  B <- .40 * A + sqrt(1 - .40^2) * rnorm(n)
  M <- rnorm(n)
  method <- rnorm(n)

  # An item is its factor, the method factor, and an error that leaves it with
  # unit variance.
  item <- function(factor, loading) {
    loading * factor + .30 * method + rnorm(n, sd = sqrt(1 - loading^2 - .30^2))
  }

  data.frame(
    a1 = item(A, .70), a2 = item(A, .70), a3 = item(A, .60), a4 = item(A, .60),
    b1 = item(B, .70), b2 = item(B, .60), b3 = item(B, .60), b4 = item(B, .50),
    m1 = item(M, .70), m2 = item(M, .70), m3 = item(M, .60)
  )
})
mv <- nomo_method_variance("A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4",
                           data = survey, marker = c("m1", "m2", "m3"))

set.seed(2026)
true_agency <- stats::rnorm(150)
true_persistence <- stats::rnorm(150)
panel <- data.frame(
  agency_t1 = 3 + true_agency + stats::rnorm(150, sd = .45),
  agency_t2 = 3.3 + true_agency + stats::rnorm(150, sd = .45),
  persistence_t1 = 3 + true_persistence + stats::rnorm(150, sd = .6),
  persistence_t2 = 3 + true_persistence + stats::rnorm(150, sd = .6)
)
rt_one <- nomo_retest(panel, scores = c("agency_t1", "agency_t2"))
rt_two <- nomo_retest(panel, scores = list(
  Agency = c("agency_t1", "agency_t2"),
  Persistence = c("persistence_t1", "persistence_t2")
), interval = "two weeks")

pw_rmsea <- nomo_power_rmsea(nomo_model(ab), n = c(100, 200, 400))
pw_never <- nomo_power_rmsea(df = 1, rmsea_null = .05, rmsea_alt = .05001)
pw_population <- "
  A =~ 0.7*a1 + 0.7*a2 + 0.6*a3 + 0.5*a4
  B =~ 0.7*b1 + 0.6*b2 + 0.6*b3 + 0.5*b4
  A ~~ 0.3*B
"
pw_sim <- nomo_power_simulate(pw_population, n = c(100, 200), reps = 50,
                              focus = "A~~B", seed = 2026)

inv_partial <- nomo_invariance("Agency =~ ag1 + ag2 + ag3 + ag4", data = nomo_demo_network,
                               group = "group", levels = c("configural", "metric", "scalar"),
                               partial = part)
long <- nomo_invariance_longitudinal("Wellbeing =~ w1 + w2 + w3 + w4",
                                     data = nomo_demo_longitudinal,
                                     occasions = c("t1", "t2", "t3"))
long_partial <- nomo_invariance_longitudinal(
  "Wellbeing =~ w1 + w2 + w3 + w4", data = nomo_demo_longitudinal,
  occasions = c("t1", "t2", "t3"), levels = c("configural", "metric", "scalar"),
  partial = nomo_partial(level = "scalar", syntax = "w3 ~ 1",
                         rationale = "The score diagnostics point to the w3 intercept.")
)

si_data <- nomo_demo_network
si_data$persistence <- rowMeans(si_data[c("pe1", "pe2", "pe3", "pe4")])
si_rel <- nomo_reliability(nomo_cfa("Persistence =~ pe1 + pe2 + pe3 + pe4", si_data))
si <- nomo_single_indicator(si_rel)
si_published <- nomo_single_indicator(.85, se = .02, coefficient = "alpha",
                                      source = "Test manual, Table 4")
net_si <- nomo_network(
  "Agency =~ ag1 + ag2 + ag3 + ag4", si_data,
  nomo_hypotheses("Agency -> persistence" = positive(min = .20)),
  single_indicators = list(persistence = si)
)

# Console output -----------------------------------------------------------------
show("screen print", print(scr))
show("screen summary", print(summary(scr)))
# The items are on a rating metric with mean 4 and observed values from 0.31
# to 7.64, so the declared response scale is 0 to 8. A range the responses
# fall outside of gives every item an out-of-range concern.
show("screen effort print", print(nomo_screen(cont, effort = TRUE, scales = ab,
                                              scale_range = c(0, 8))))
show("factors print", print(fac))
show("factors summary", print(summary(fac)))
show("efa print", print(efa))
show("efa summary", print(summary(efa)))
show("cfa print", print(cfa))
show("cfa summary", print(summary(cfa)))
show("reliability print", print(rel))
show("reliability summary", print(summary(rel)))
show("validity print", print(val))
show("validity summary", print(summary(val)))
show("scores print", print(sc))
show("scores summary", print(summary(sc)))
show("missing print", print(mis))
show("compare print", print(cmp))
show("compare summary", print(summary(cmp)))
show("invariance print", print(inv))
show("invariance summary", print(summary(inv)))
show("partial print", print(part))
show("hypotheses print", print(h))
show("hypotheses summary", print(summary(h)))
show("network print", print(net))
show("network summary", print(summary(net)))
show("model print", print(nomo_model(scales3)))
show("split print", print(sp))
show("hierarchical print", print(hier))
show("hierarchical summary", print(summary(hier)))
show("apa loadings", print(nomo_apa_table(cfa, "loadings", number = 1)))
show("run paused print", print(run_paused))
show("run paused summary", print(summary(run_paused)))
show("run complete print", print(run_done))
show("run complete summary", print(summary(run_done)))
show("run research print", print(nomo_run(resume = run_done, mode = "research")))
show("esem print", print(esem))
show("esem summary", print(summary(esem)))
show("method variance print", print(mv))
show("method variance summary", print(summary(mv)))
show("retest one print", print(rt_one))
show("retest one summary", print(summary(rt_one)))
show("retest two print", print(rt_two))
show("retest two summary", print(summary(rt_two)))
show("power rmsea print", print(pw_rmsea))
show("power rmsea unreachable print", print(pw_never))
show("power simulate print", print(pw_sim))
show("invariance partial print", print(inv_partial))
show("invariance partial summary", print(summary(inv_partial)))
show("longitudinal print", print(long))
show("longitudinal summary", print(summary(long)))
show("longitudinal partial print", print(long_partial))
show("longitudinal partial summary", print(summary(long_partial)))
show("single indicator print", print(si))
show("single indicator published print", print(si_published))
show("network single indicator print", print(net_si))
show("network single indicator summary", print(summary(net_si)))
show("network validation print", print(net_val))
show("network validation summary", print(summary(net_val)))
show("screen ordered print", print(nomo_screen(ord)))
show("cfa ordered print", print(cfa_ord))
show("cfa ordered summary", print(summary(cfa_ord)))
show("reliability ordered print", print(rel_ord))
show("reliability ordered summary", print(summary(rel_ord)))
show("validity ordered print", print(val_ord))
show("validity ordered summary", print(summary(val_ord)))

# Plots --------------------------------------------------------------------------
# Each class is drawn from a fixture that supports every plot type it has, so
# a type that stops is a fault: the network is the one with a validation
# sample, because type = "replication" needs one. A class that has no plot()
# method is passed over, so a method added later is drawn without an edit here.
plot_size <- c(width = 7, height = 5)
objects <- list(
  nomo_screen = scr, nomo_factors = fac, nomo_efa = efa, nomo_cfa = cfa,
  nomo_reliability = rel, nomo_validity = val, nomo_compare = cmp,
  nomo_invariance = inv, nomo_invariance_longitudinal = long, nomo_network = net_val,
  nomo_hierarchical = hier, nomo_scores = sc, nomo_missing = mis,
  nomo_power_rmsea = pw_rmsea, nomo_power_simulate = pw_sim, nomo_esem = esem,
  nomo_method_variance = mv, nomo_retest = rt_two
)

# The plot() method of an object's own classes. A subclass, such as
# nomo_invariance_longitudinal, uses its parent's method.
plot_method <- function(x) {
  for (k in grep("^nomo_", class(x), value = TRUE)) {
    method <- utils::getS3method("plot", k, optional = TRUE)
    if (!is.null(method)) return(method)
  }
  NULL
}

# A plot's title, subtitle, and caption, each as its lines.
label_lines <- function(p) {
  lapply(
    stats::setNames(nm = c("title", "subtitle", "caption")),
    function(k) {
      v <- p$labels[[k]]
      if (is.null(v) || !length(v)) return(character())
      unlist(strsplit(paste(v, collapse = "\n"), "\n", fixed = TRUE))
    }
  )
}

# How far each label runs past the edge of the plot as drawn, in inches. A
# label is measured as ggplot2 lays it out: its width is that of its widest
# line, and it sits in the columns it spans by its own justification, so a
# caption set flush left and a title aligned to the whole plot are each judged
# where they are drawn. Needs an open graphics device of the plot's size.
label_overflow <- function(p) {
  gt <- ggplot2::ggplotGrob(p)
  inches <- function(u) grid::convertWidth(u, "in", valueOnly = TRUE)
  # Flexible ("null") columns convert to 0, which leaves the fixed widths on
  # either side of a label's span.
  columns <- inches(gt$widths)
  vapply(c(title = "title", subtitle = "subtitle", caption = "caption"), function(k) {
    i <- match(k, gt$layout$name)
    if (is.na(i) || inherits(gt$grobs[[i]], "zeroGrob")) return(0)
    grob <- gt$grobs[[i]]
    label_width <- inches(grid::grobWidth(grob))
    left <- sum(columns[seq_along(columns) < gt$layout$l[[i]]])
    right <- sum(columns[seq_along(columns) > gt$layout$r[[i]]])
    span <- plot_size[["width"]] - left - right
    hjust <- tryCatch(as.numeric(grob$children[[1L]]$hjust)[[1L]], error = function(e) NA_real_)
    if (is.na(hjust)) hjust <- if (identical(k, "caption")) 1 else 0
    start <- left + hjust * (span - label_width)
    max(0, -start, start + label_width - plot_size[["width"]])
  }, numeric(1))
}

plot_text <- list()
# ggplotGrob() measures text on the open device. Without this one R would open
# its default device, which under Rscript writes Rplots.pdf to the directory.
measure_file <- tempfile(fileext = ".png")
grDevices::png(measure_file, width = plot_size[["width"]], height = plot_size[["height"]],
               units = "in", res = 96)
measure_device <- grDevices::dev.cur()
for (cls in names(objects)) {
  method <- plot_method(objects[[cls]])
  if (is.null(method)) next
  types <- eval(formals(method)$type)
  if (!is.character(types)) types <- "default"
  for (type in types) {
    label <- paste(cls, type, sep = "__")
    row <- data.frame(plot = label, title = 0L, subtitle = 0L, caption = 0L, lines = 0L,
                      overflow_in = 0, long = FALSE, error = "")
    p <- tryCatch(
      if (identical(type, "default")) plot(objects[[cls]]) else plot(objects[[cls]], type = type),
      error = function(e) e
    )
    if (inherits(p, "error")) {
      row$error <- gsub("[[:space:]]+", " ", conditionMessage(p))
      plot_text[[label]] <- row
      next
    }
    ggplot2::ggsave(file.path(out, "plots", paste0(label, ".png")), p,
                    width = plot_size[["width"]], height = plot_size[["height"]], dpi = 96)
    text <- label_lines(p)
    longest <- vapply(text, function(v) if (length(v)) max(nchar(v)) else 0L, integer(1))
    row[names(longest)] <- as.list(longest)
    row$lines <- max(lengths(text))
    grDevices::dev.set(measure_device)
    overflow <- tryCatch(label_overflow(p), error = function(e) NULL)
    if (is.null(overflow)) {
      # Not measurable: about 95 characters fit across 7 inches at ggplot2's
      # default sizes, counted for each line of a label.
      row$overflow_in <- NA_real_
      row$long <- any(longest > 95L)
    } else {
      row$overflow_in <- round(max(overflow), 2)
      row$long <- max(overflow) > 0.02
    }
    plot_text[[label]] <- row
  }
}
invisible(grDevices::dev.off(measure_device))
unlink(measure_file)

fault_table <- do.call(rbind, faults)
plot_table <- do.call(rbind, plot_text)
utils::write.csv(fault_table, file.path(out, "faults.csv"), row.names = FALSE)
utils::write.csv(plot_table, file.path(out, "plot-text.csv"), row.names = FALSE)

# Dropped columns are a fault where every column should fit (see the top).
fault_columns <- c("over_width", "truncated", "hidden", "type_rows", "error",
                   if (width >= 80L) "dropped")
faulted <- rowSums(fault_table[, fault_columns, drop = FALSE]) > 0
plot_stopped <- nzchar(plot_table$error)

cat("Console width:", width, "\n")
cat("Console outputs:", nrow(fault_table), "| with faults:", sum(faulted), "\n")
print(fault_table[faulted, ], row.names = FALSE)
cat("Tables that dropped columns for width:", sum(fault_table$dropped), "in",
    sum(fault_table$dropped > 0), "outputs",
    if (width >= 80L) "(a fault at this width)" else "(by design below 80 columns)", "\n")
cat("\nPlots:", sum(!plot_stopped), "| with long titles:", sum(plot_table$long),
    "| plot types that stopped:", sum(plot_stopped), "\n")
print(plot_table[plot_table$long | plot_stopped, ], row.names = FALSE)
cat("\nWritten to", out, "\n")
