# Reproducible nomologR report -------------------------------------------------

nomo_report_empty_table <- function() {
  tibble::tibble()
}


nomo_report_matrix_table <- function(x, row_name = "row") {
  if (is.null(x) || !is.matrix(x) || !length(x)) {
    return(nomo_report_empty_table())
  }

  out <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  rn <- rownames(x)

  if (!is.null(rn)) {
    out <- data.frame(
      stats::setNames(list(rn), row_name),
      out,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  tibble::as_tibble(out, .name_repair = "minimal")
}


nomo_report_flatten_table <- function(x) {
  if (is.null(x)) return(nomo_report_empty_table())

  if (is.matrix(x)) {
    x <- nomo_report_matrix_table(x)
  }

  if (!inherits(x, "data.frame")) {
    return(nomo_report_empty_table())
  }

  out <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)

  for (nm in names(out)) {
    if (is.list(out[[nm]])) {
      out[[nm]] <- vapply(
        out[[nm]],
        function(z) {
          if (is.null(z) || !length(z)) return("")
          paste(as.character(unlist(z, recursive = TRUE, use.names = FALSE)), collapse = "; ")
        },
        character(1)
      )
    }
  }

  tibble::as_tibble(out, .name_repair = "minimal")
}


nomo_report_validate_run <- function(x) {
  if (!inherits(x, "nomo_run")) {
    stop("`x` must be an object created by `nomo_run()`.", call. = FALSE)
  }

  required <- c(
    "status",
    "sample_design",
    "sample_n",
    "scales",
    "stage_status",
    "results",
    "decision_log",
    "source_data"
  )

  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      sprintf(
        "The `nomo_run` object is missing required field(s): %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


nomo_report_validate_scalar_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be `TRUE` or `FALSE`.", name), call. = FALSE)
  }
  invisible(TRUE)
}


nomo_report_validate_file <- function(file, overwrite) {
  if (!is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !nzchar(trimws(file))) {
    stop("`file` must be one non-empty character path.", call. = FALSE)
  }

  if (!grepl("\\.html?$", file, ignore.case = TRUE)) {
    stop(
      "nomologR currently renders HTML reports; `file` must end in `.html` or `.htm`.",
      call. = FALSE
    )
  }

  if (file.exists(file) && !isTRUE(overwrite)) {
    stop(
      sprintf(
        "Report file already exists: %s. Use `overwrite = TRUE` to replace it.",
        file
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


nomo_report_template_path <- function(
    installed = system.file(
      "rmarkdown",
      "nomo-report.Rmd",
      package = "nomologR"
    ),
    development = file.path(
      "inst",
      "rmarkdown",
      "nomo-report.Rmd"
    )) {

  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }

  if (file.exists(development)) {
    return(development)
  }

  stop(
    "Could not locate the packaged `nomo_report()` R Markdown template.",
    call. = FALSE
  )
}


nomo_report_component_list <- function(x, stage) {
  if (!stage %in% names(x$results)) return(list())

  obj <- x$results[[stage]]
  if (is.null(obj)) return(list())

  if (stage %in% c("screen", "factors", "efa")) {
    if (!is.list(obj) || !length(obj)) return(list())
    if (is.null(names(obj))) {
      names(obj) <- paste0("scope_", seq_along(obj))
    }
    return(obj)
  }

  stats::setNames(list(obj), "workflow")
}


nomo_report_component_summary_text <- function(obj) {
  if (is.null(obj)) return("No component result is available.")

  tryCatch(
    paste(
      utils::capture.output(print(summary(obj))),
      collapse = "\n"
    ),
    error = function(e) {
      tryCatch(
        paste(utils::capture.output(print(obj)), collapse = "\n"),
        error = function(e2) {
          paste0("Summary unavailable: ", conditionMessage(e2))
        }
      )
    }
  )
}


nomo_report_data_characteristics <- function(x) {
  roles <- tryCatch(
    nomo_run_data_roles(x$source_data),
    error = function(e) NULL
  )

  if (is.null(roles)) {
    return(nomo_report_empty_table())
  }

  all_items <- unique(unlist(x$scales, use.names = FALSE))

  describe <- function(dat, role) {
    item_names <- intersect(all_items, names(dat))
    missing_cells <- if (length(item_names)) {
      sum(is.na(dat[item_names]))
    } else {
      0L
    }
    total_cells <- nrow(dat) * length(item_names)

    tibble::tibble(
      role = role,
      n_cases = nrow(dat),
      n_columns = ncol(dat),
      candidate_items = length(item_names),
      candidate_item_missing_cells = missing_cells,
      candidate_item_missing_pct = if (total_cells > 0L) {
        missing_cells / total_cells
      } else {
        NA_real_
      }
    )
  }

  if (identical(roles$design, "calibration_validation")) {
    dplyr::bind_rows(
      describe(roles$exploratory, "calibration / exploratory"),
      describe(roles$confirmatory, "validation / confirmatory")
    )
  } else {
    describe(roles$exploratory, "same sample")
  }
}


nomo_report_call_history <- function(x) {
  calls <- x$call_history
  if (is.null(calls) || !length(calls)) {
    calls <- if (!is.null(x$call)) list(x$call) else list()
  }

  if (!length(calls)) return(nomo_report_empty_table())

  tibble::tibble(
    step = seq_along(calls),
    call = vapply(
      calls,
      function(z) paste(deparse(z, width.cutoff = 120L), collapse = " "),
      character(1)
    )
  )
}


nomo_report_run_table <- function(x, type) {
  tryCatch(
    nomo_report_flatten_table(nomo_table(x, type)),
    error = function(e) nomo_report_empty_table()
  )
}


nomo_report_component_table <- function(obj,
                                        stage,
                                        type = "primary") {
  if (is.null(obj)) return(nomo_report_empty_table())

  result <- tryCatch(
    {
      if (identical(stage, "screen")) {
        switch(
          type,
          primary = obj$item_summary,
          relationships = obj$relationship_summary,
          cases = obj$case_summary,
          decision_log = obj$decision_log,
          nomo_report_empty_table()
        )
      } else if (identical(stage, "factors")) {
        s <- summary(obj)
        switch(
          type,
          primary = s$evidence,
          criteria = s$criterion_status,
          adequacy = s$adequacy,
          concordance = s$concordance,
          decision_log = s$decision_log,
          nomo_report_empty_table()
        )
      } else if (identical(stage, "efa")) {
        switch(
          type,
          primary = obj$item_summary,
          pattern = nomo_report_matrix_table(obj$pattern_matrix, "item"),
          factor_correlations = nomo_report_matrix_table(
            obj$factor_correlations,
            "factor"
          ),
          residuals = obj$residual_pairs,
          decision_log = obj$decision_log,
          nomo_report_empty_table()
        )
      } else if (identical(stage, "cfa")) {
        switch(
          type,
          primary = obj$fit_evidence,
          loadings = obj$standardized_loadings,
          factor_correlations = obj$factor_correlations,
          heywood = obj$heywood,
          residuals = obj$residual_pairs,
          modification_indices = obj$top_modification_indices,
          decision_log = obj$decision_log,
          nomo_report_empty_table()
        )
      } else if (identical(stage, "reliability")) {
        s <- summary(obj)
        switch(
          type,
          primary = s$table,
          alpha_status = s$alpha_status,
          ci_status = s$ci_status,
          decision_log = s$decision_log,
          nomo_report_empty_table()
        )
      } else if (identical(stage, "validity")) {
        s <- summary(obj)
        switch(
          type,
          primary = s$convergent,
          discriminant = s$discriminant,
          htmt_status = s$htmt_status,
          decision_log = s$decision_log,
          nomo_report_empty_table()
        )
      } else if (identical(stage, "invariance")) {
        switch(
          type,
          primary = nomo_table(obj, "fit"),
          categories = nomo_table(obj, "categories"),
          partial = nomo_table(obj, "partial"),
          local_strain = nomo_table(obj, "local_strain"),
          decision_log = nomo_table(obj, "decision_log"),
          nomo_report_empty_table()
        )
      } else if (identical(stage, "network")) {
        switch(
          type,
          primary = nomo_table(obj, "hypotheses"),
          fit = nomo_table(obj, "fit"),
          measurement = nomo_table(obj, "measurement"),
          relations = nomo_table(obj, "relations"),
          replication = nomo_table(obj, "replication"),
          decision_log = nomo_table(obj, "decision_log"),
          nomo_report_empty_table()
        )
      } else {
        nomo_report_empty_table()
      }
    },
    error = function(e) nomo_report_empty_table()
  )

  nomo_report_flatten_table(result)
}


nomo_report_evidence_trace <- function(x) {
  log <- tryCatch(
    nomo_run_component_logs(x),
    error = function(e) tibble::tibble()
  )

  if (!nrow(log)) {
    return(tibble::tibble(
      evidence_id = character(),
      pipeline_component = character(),
      pipeline_scope = character(),
      stage = character(),
      object = character(),
      metric = character(),
      value = character(),
      reference = character(),
      severity = character(),
      observation = character(),
      recommendation = character()
    ))
  }

  log <- nomo_report_flatten_table(log)
  log$evidence_id <- sprintf("E%04d", seq_len(nrow(log)))

  preferred <- c(
    "evidence_id",
    "pipeline_component",
    "pipeline_scope",
    "stage",
    "object",
    "metric",
    "value",
    "reference",
    "severity",
    "observation",
    "recommendation"
  )

  preferred <- intersect(preferred, names(log))
  rest <- setdiff(names(log), preferred)
  log[, c(preferred, rest), drop = FALSE]
}


nomo_report_flagged_trace <- function(x) {
  trace <- nomo_report_evidence_trace(x)
  if (!nrow(trace) || !"severity" %in% names(trace)) {
    return(nomo_report_empty_table())
  }

  trace[
    tolower(trace$severity) %in% c("review", "concern"),
    ,
    drop = FALSE
  ]
}


nomo_report_deviations <- function(x) {
  rows <- list()
  cursor <- 0L

  inv <- x$results$invariance
  if (!is.null(inv)) {
    partial <- tryCatch(
      nomo_table(inv, "partial"),
      error = function(e) tibble::tibble()
    )

    if (nrow(partial)) {
      partial <- nomo_report_flatten_table(partial)
      for (i in seq_len(nrow(partial))) {
        cursor <- cursor + 1L
        detail_cols <- intersect(
          c("level", "syntax", "constraint", "release"),
          names(partial)
        )
        rationale_col <- intersect(
          c("rationale", "reason"),
          names(partial)
        )

        rows[[cursor]] <- tibble::tibble(
          type = "researcher-specified partial invariance",
          stage = "invariance",
          scope = if ("level" %in% names(partial)) {
            as.character(partial$level[[i]])
          } else {
            ""
          },
          detail = paste(
            vapply(
              detail_cols,
              function(nm) paste0(nm, "=", as.character(partial[[nm]][[i]])),
              character(1)
            ),
            collapse = "; "
          ),
          rationale = if (length(rationale_col)) {
            as.character(partial[[rationale_col[[1L]]]][[i]])
          } else {
            ""
          }
        )
      }
    }
  }

  net <- x$results$network
  if (!is.null(net) && !is.null(net$hypothesis_evidence)) {
    h <- nomo_report_flatten_table(net$hypothesis_evidence)

    if (nrow(h) && "confirmatory_status" %in% names(h)) {
      idx <- grepl(
        "post",
        as.character(h$confirmatory_status),
        ignore.case = TRUE
      )

      for (i in which(idx)) {
        cursor <- cursor + 1L
        rows[[cursor]] <- tibble::tibble(
          type = "post-hoc nomological relation",
          stage = "network",
          scope = if ("relation" %in% names(h)) {
            as.character(h$relation[[i]])
          } else {
            ""
          },
          detail = if ("concordance" %in% names(h)) {
            paste0(
              "confirmatory_status=",
              h$confirmatory_status[[i]],
              "; concordance=",
              h$concordance[[i]]
            )
          } else {
            paste0("confirmatory_status=", h$confirmatory_status[[i]])
          },
          rationale = ""
        )
      }
    }
  }

  if (inherits(x$decision_log, "data.frame") && nrow(x$decision_log)) {
    dl <- x$decision_log
    decision_revise <- if ("decision" %in% names(dl)) {
      tolower(as.character(dl$decision)) == "revise"
    } else {
      rep(FALSE, nrow(dl))
    }
    revise <- which(
      decision_revise |
        grepl("post|partial|deviation", dl$id, ignore.case = TRUE)
    )

    for (i in revise) {
      cursor <- cursor + 1L
      rows[[cursor]] <- tibble::tibble(
        type = "workflow deviation/revision decision",
        stage = if ("stage" %in% names(dl)) as.character(dl$stage[[i]]) else "",
        scope = if ("scope" %in% names(dl)) as.character(dl$scope[[i]]) else "",
        detail = if ("decision" %in% names(dl)) {
          as.character(dl$decision[[i]])
        } else {
          ""
        },
        rationale = if ("rationale" %in% names(dl)) {
          as.character(dl$rationale[[i]])
        } else {
          ""
        }
      )
    }
  }

  if (!length(rows)) {
    return(tibble::tibble(
      type = character(),
      stage = character(),
      scope = character(),
      detail = character(),
      rationale = character()
    ))
  }

  dplyr::bind_rows(rows)
}


nomo_report_namespace_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}


nomo_report_pandoc_available <- function() {
  rmarkdown::pandoc_available()
}


nomo_report_package_versions <- function(
    packages = c(
      "nomologR",
      "psych",
      "lavaan",
      "semTools",
      "EFAtools",
      "rmarkdown",
      "knitr"
    ),
    namespace_available = nomo_report_namespace_available,
    version_fun = utils::packageVersion) {
  tibble::tibble(
    package = packages,
    version = vapply(
      packages,
      function(pkg) {
        if (!isTRUE(namespace_available(pkg))) return("not installed")
        as.character(version_fun(pkg))
      },
      character(1)
    )
  )
}


nomo_report_sanitize_citation_text <- function(x) {
  x <- as.character(x)

  if (!length(x)) return(character())

  x <- gsub("[\r\n\t]+", " ", x, perl = TRUE)
  x <- gsub(
    "<(https?://[^<>[:space:]]+)>",
    "\\1",
    x,
    perl = TRUE
  )
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}


nomo_report_citations <- function(
    packages = c("nomologR", "psych", "lavaan", "semTools", "EFAtools"),
    namespace_available = nomo_report_namespace_available,
    citation_fun = utils::citation,
    version_fun = utils::packageVersion) {
  rows <- lapply(packages, function(pkg) {
    if (!isTRUE(namespace_available(pkg))) {
      return(tibble::tibble(
        package = pkg,
        installed_version = "not installed",
        citation = "Package not installed in the rendering session."
      ))
    }

    citation_text <- tryCatch(
      paste(
        utils::capture.output(
          print(suppressWarnings(citation_fun(pkg)))
        ),
        collapse = " "
      ),
      error = function(e) {
        paste0("Citation unavailable: ", conditionMessage(e))
      }
    )
    citation_text <- nomo_report_sanitize_citation_text(citation_text)

    tibble::tibble(
      package = pkg,
      installed_version = as.character(version_fun(pkg)),
      citation = citation_text
    )
  })

  dplyr::bind_rows(rows)
}


nomo_report_session_text <- function() {
  paste(utils::capture.output(utils::sessionInfo()), collapse = "\n")
}


nomo_report_safe_plot <- function(obj, type = NULL) {
  if (is.null(obj)) {
    return(list(ok = FALSE, plot = NULL, message = "No component result is available."))
  }

  tryCatch(
    {
      p <- if (is.null(type)) {
        plot(obj)
      } else {
        plot(obj, type = type)
      }

      list(ok = TRUE, plot = p, message = "")
    },
    error = function(e) {
      list(
        ok = FALSE,
        plot = NULL,
        message = conditionMessage(e)
      )
    }
  )
}


nomo_report_prepare_template <- function(template, input, title) {
  copied <- file.copy(template, input, overwrite = TRUE)
  if (!isTRUE(copied)) {
    stop("Could not prepare the temporary report template.", call. = FALSE)
  }

  lines <- readLines(input, warn = FALSE, encoding = "UTF-8")
  marker <- 'title: "__NOMO_REPORT_TITLE__"'
  hits <- which(lines == marker)

  if (length(hits) != 1L) {
    stop(
      "The packaged report template does not contain exactly one title marker.",
      call. = FALSE
    )
  }

  lines[[hits]] <- paste0(
    "title: ",
    encodeString(title, quote = '"')
  )

  writeLines(lines, input, useBytes = TRUE)
  invisible(input)
}


#' Render a reproducible nomologR analysis report
#'
#' `nomo_report()` renders a self-contained HTML document from a `nomo_run`
#' object. The report archives researcher inputs, sample roles, item and factor
#' evidence, EFA/CFA results, reliability, convergent/discriminant evidence,
#' optional invariance and nomological-network results, researcher decisions,
#' deviations/post-hoc decisions, method citations, an evidence trace, and
#' session information.
#'
#' The report is a presentation and provenance layer. It does not refit models,
#' alter data, free parameters, remove items, or manufacture additional
#' statistical conclusions.
#'
#' Reports can be rendered from complete, paused, or blocked `nomo_run` objects.
#' Incomplete stages are labeled as such rather than silently omitted.
#'
#' @param x An object created by [nomo_run()].
#' @param file Output HTML path.
#' @param title Report title.
#' @param include_plots Logical; include a compact set of component plots when
#'   those plots are available.
#' @param include_session Logical; include full `sessionInfo()` output.
#' @param max_table_rows Positive integer used for ordinary display tables.
#'   The final evidence-trace appendix is not truncated.
#' @param overwrite Logical; replace an existing `file`.
#' @param quiet Logical passed to [rmarkdown::render()].
#'
#' @return The normalized report file path, invisibly.
#'
#' @examples
#' \dontrun{
#' run <- nomo_run(
#'   data = dat,
#'   scales = list(WellBeing = c("w1", "w2", "w3", "w4")),
#'   decisions = list(
#'     factor_count = 1L,
#'     cfa_model = "WellBeing =~ w1 + w2 + w3 + w4",
#'     measurement_model = "proceed"
#'   )
#' )
#'
#' nomo_report(
#'   run,
#'   file = "well-being-validation.html"
#' )
#' }
#'
#' @export
nomo_report <- function(x,
                        file = "nomologR-report.html",
                        title = "nomologR reproducible analysis report",
                        include_plots = TRUE,
                        include_session = TRUE,
                        max_table_rows = 50L,
                        overwrite = FALSE,
                        quiet = TRUE) {
  nomo_report_validate_run(x)
  nomo_report_validate_scalar_logical(include_plots, "include_plots")
  nomo_report_validate_scalar_logical(include_session, "include_session")
  nomo_report_validate_scalar_logical(overwrite, "overwrite")
  nomo_report_validate_scalar_logical(quiet, "quiet")
  nomo_report_validate_file(file, overwrite)

  if (!is.character(title) ||
      length(title) != 1L ||
      is.na(title) ||
      !nzchar(trimws(title))) {
    stop("`title` must be one non-empty character value.", call. = FALSE)
  }

  if (!is.numeric(max_table_rows) ||
      length(max_table_rows) != 1L ||
      is.na(max_table_rows) ||
      !is.finite(max_table_rows) ||
      max_table_rows < 1 ||
      abs(max_table_rows - round(max_table_rows)) > sqrt(.Machine$double.eps)) {
    stop("`max_table_rows` must be one positive integer.", call. = FALSE)
  }
  max_table_rows <- as.integer(round(max_table_rows))

  if (!nomo_report_namespace_available("rmarkdown")) {
    stop(
      "Rendering a report requires the suggested package `rmarkdown`.",
      call. = FALSE
    )
  }

  if (!nomo_report_namespace_available("knitr")) {
    stop(
      "Rendering a report requires the suggested package `knitr`.",
      call. = FALSE
    )
  }

  if (!nomo_report_pandoc_available()) {
    stop(
      "Pandoc is required to render the HTML report but was not found.",
      call. = FALSE
    )
  }

  template <- nomo_report_template_path()
  input <- tempfile(pattern = "nomologR-report-", fileext = ".Rmd")
  nomo_report_prepare_template(
    template = template,
    input = input,
    title = title
  )

  output_dir <- dirname(file)
  if (!dir.exists(output_dir)) {
    ok <- dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(ok) && !dir.exists(output_dir)) {
      stop(
        sprintf("Could not create report output directory: %s", output_dir),
        call. = FALSE
      )
    }
  }

  rendered <- rmarkdown::render(
    input = input,
    output_file = basename(file),
    output_dir = output_dir,
    params = list(
      run = x,
      report_title = title,
      include_plots = include_plots,
      include_session = include_session,
      max_table_rows = max_table_rows,
      generated_at = format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S %Z"
      )
    ),
    envir = new.env(parent = globalenv()),
    clean = TRUE,
    quiet = quiet
  )

  invisible(normalizePath(rendered, winslash = "/", mustWork = TRUE))
}
