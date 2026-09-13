library(deliberately)

args <- commandArgs(trailingOnly = TRUE)
source_path <- Sys.getenv(
  "DELIBERATELY_SOURCE",
  unset = if (length(args)) args[1] else "../distortions"
)
dir.create("artifacts/gallery", recursive = TRUE, showWarnings = FALSE)
education <- define_contrast(
  "education",
  function(p) p$education == "lower",
  function(p) p$education == "higher",
  rationale = "Synthetic educational disadvantage",
  reference_advantaged = TRUE
)
policy <- audit_policy(
  "demonstration-1",
  data.frame(
    rule_id = c("concentration", "participation", "participation"),
    scope = "session",
    metric = c("max_speaker_share", "max_speaker_share", "silent_fraction"),
    operator = c(">", "<=", "=="),
    threshold = c(.6, .4, 0),
    verdict = c("WARNING", "PASS", "PASS"),
    min_coverage = 1,
    aggregation = "all"
  ),
  rationale = "Illustrative participation rules, not validated deliberation standards."
)
for (scenario in c("balanced", "concentrated", "incomplete")) {
  result <- audit_dp(example_deliberation(scenario), education, policy)
  saveRDS(result, file.path("artifacts/gallery", paste0(scenario, ".rds")))
  render_audit(
    result,
    file.path("artifacts/gallery", paste0(scenario, ".html")),
    title = paste("Synthetic deliberation:", scenario)
  )
}
if (dir.exists(source_path)) {
  x <- read_distortions(source_path)
  education <- define_contrast(
    "education",
    function(p) p$bettered == 0,
    function(p) p$bettered == 1,
    rationale = "Source higher-education indicator within each poll",
    reference_advantaged = TRUE
  )
  paired <- audit_dp(x, education)
  available <- audit_dp(
    x,
    education,
    config = audit_config(membership = "available")
  )
  paired$sensitivity <- available$metrics
  saveRDS(paired, "artifacts/gallery/distortions.rds")
  saveRDS(available, "artifacts/gallery/distortions-available.rds")
  render_audit(
    paired,
    "artifacts/gallery/distortions.html",
    title = "Deliberative distortions: what the surveys establish"
  )
  render_audit(
    available,
    "artifacts/gallery/distortions-available.html",
    title = "Deliberative distortions: available-wave sensitivity"
  )
  utils::write.csv(
    paired$metrics,
    "artifacts/gallery/distortions-metrics.csv",
    row.names = FALSE
  )
}
