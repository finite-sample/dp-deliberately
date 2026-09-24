library(deliberately)

args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args)) args[1] else "../dp-data"
x <- read_distortions(path)
education <- define_contrast(
  "education",
  function(p) !is.na(p$bettered) & p$bettered == 0,
  function(p) !is.na(p$bettered) & p$bettered == 1,
  rationale = "Source bettered variable defines the higher-education reference within each poll.",
  reference_advantaged = TRUE
)
result <- audit_dp(
  x,
  education,
  config = audit_config(membership = "available")
)
dir.create("artifacts", showWarnings = FALSE)
saveRDS(result, "artifacts/distortions.rds")
utils::write.csv(
  result$metrics,
  "artifacts/distortions-metrics.csv",
  row.names = FALSE
)
render_audit(
  result,
  "artifacts/distortions.html",
  title = "Deliberative distortions: a survey audit"
)
print(result)
