#' Render a standalone HTML audit report
#' @param result A result from [audit_dp()].
#' @param file Destination HTML path.
#' @param include_evidence Include raw source excerpts and participant IDs.
#'   Default FALSE; the returned R object always retains supplied evidence.
#' @param title Report title.
#' @return The normalized output path, invisibly. Requires rmarkdown and Pandoc.
#' @examples
#' if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
#'   result <- audit_dp(example_deliberation(), config = audit_config(permutations = 9))
#'   file <- tempfile(fileext = ".html")
#'   render_audit(result, file)
#'   unlink(file)
#' }
#' @export
render_audit <- function(
  result,
  file,
  include_evidence = FALSE,
  title = "A deliberate look"
) {
  if (
    !is.logical(include_evidence) ||
      length(include_evidence) != 1L ||
      is.na(include_evidence)
  ) {
    cli::cli_abort("include_evidence must be TRUE or FALSE.")
  }
  if (!inherits(result, "deliberation_audit")) {
    stop("Expected deliberation_audit.")
  }
  if (
    !requireNamespace("rmarkdown", quietly = TRUE) ||
      !requireNamespace("knitr", quietly = TRUE)
  ) {
    stop("Install rmarkdown and knitr to render reports.")
  }
  if (!rmarkdown::pandoc_available()) {
    stop("Pandoc is required; install Pandoc or use an RStudio installation.")
  }
  template <- system.file("rmarkdown", "audit.Rmd", package = "deliberately")
  if (!nzchar(template)) {
    stop("Report template unavailable; install the package first.")
  }
  for (pkg in c("bslib", "ggplot2", "gt", "DT", "htmltools")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_abort("Install {.pkg {pkg}} to render reports.")
    }
  }
  file <- path.expand(file)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  staging <- tempfile("deliberately-report-")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  input <- file.path(staging, "audit.Rmd")
  file.copy(template, input)
  env <- new.env(parent = baseenv())
  env$result <- result
  env$include_evidence <- include_evidence
  env$report_title <- title
  env$report_summary <- report_summary
  env$report_plot <- report_plot
  env$report_metrics <- report_metrics
  env$report_findings <- report_findings
  rmarkdown::render(
    input,
    output_file = basename(file),
    output_dir = normalizePath(dirname(file)),
    envir = env,
    quiet = TRUE,
    output_options = list(
      self_contained = TRUE,
      mathjax = NULL,
      theme = bslib::bs_theme(version = 5, bootswatch = "flatly")
    )
  )
  invisible(normalizePath(file))
}
