#' Render a standalone HTML audit report
#' @param result A result from [audit_dp()].
#' @param file Destination HTML path.
#' @param include_evidence Include raw source excerpts and participant IDs.
#'   Default FALSE; the returned R object always retains supplied evidence.
#' @param title Report title.
#' @return The normalized output path, invisibly. Requires rmarkdown and Pandoc.
#' @export
render_audit <- function(
  result,
  file,
  include_evidence = FALSE,
  title = "A deliberate look"
) {
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
  rmarkdown::render(
    input,
    output_file = basename(file),
    output_dir = normalizePath(dirname(file)),
    envir = env,
    quiet = TRUE,
    output_options = list(self_contained = TRUE, mathjax = NULL)
  )
  invisible(normalizePath(file))
}
