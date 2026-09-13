library(deliberately)

x <- example_deliberation()
education <- define_contrast(
  "education",
  function(p) p$education == "lower",
  function(p) p$education == "higher",
  rationale = "Illustrative educational contrast in synthetic data.",
  reference_advantaged = TRUE
)
policy <- audit_policy(
  "illustration-1",
  data.frame(
    rule_id = "concentration",
    scope = "session",
    metric = "max_speaker_share",
    operator = ">",
    threshold = 0.4,
    verdict = "WARNING",
    min_coverage = 1,
    aggregation = "any"
  ),
  rationale = "Illustrative threshold for demonstrating software; not a validated quality standard."
)
result <- audit_dp(
  x,
  education,
  policy,
  audit_config(
    baseline_predictors = c("education", "age"),
    probability_sample = TRUE
  )
)
dir.create("artifacts", showWarnings = FALSE)
saveRDS(result, "artifacts/synthetic.rds")
utils::write.csv(
  result$metrics,
  "artifacts/synthetic-metrics.csv",
  row.names = FALSE
)
render_audit(
  result,
  "artifacts/synthetic.html",
  title = "A deliberate look at a bus route"
)
print(result)
