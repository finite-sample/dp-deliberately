library(deliberately)
args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args)) args[1] else "../dp-data"
reference_path <- if (length(args) >= 2L) args[2] else "../dp-distortions"
x <- read_distortions(path)
ct <- define_contrast(
  "education",
  function(p) p$bettered == 0,
  function(p) p$bettered == 1,
  rationale = "Source education indicator",
  reference_advantaged = TRUE
)
m <- audit_dp(x, ct, config = audit_config(membership = "available"))$metrics
checks <- list()
for (metric in c("H", "P", "D")) {
  file <- if (metric == "D") {
    "03_dom_educ_by_group_issue.csv"
  } else {
    "03_hom_pol_by_group_issue.csv"
  }
  ref <- read.csv(file.path(reference_path, "tabs", file))
  value <- c(H = "homoex", P = "polarex", D = "ext_grp")[[metric]]
  expected <- data.frame(
    event_id = as.character(ref$source_poll_id),
    group_id = as.character(ref$group_id),
    item_id = ref$issue_id,
    expected = ref[[value]]
  )
  got <- m[
    m$metric == metric & !is.na(m$group_id),
    c("event_id", "group_id", "item_id", "estimate")
  ]
  keys <- c("event_id", "group_id", "item_id")
  stopifnot(nrow(dplyr::anti_join(expected, got, by = keys)) == 0)
  joined <- dplyr::left_join(
    expected,
    got,
    by = keys,
    relationship = "one-to-one"
  )
  stopifnot(identical(is.na(joined$expected), is.na(joined$estimate)))
  difference <- max(abs(joined$expected - joined$estimate), na.rm = TRUE)
  stopifnot(difference < 1e-10)
  checks[[metric]] <- data.frame(
    metric = metric,
    pairs = nrow(joined),
    finite = sum(is.finite(joined$estimate)),
    max_absolute_difference = difference
  )
}
dir.create("artifacts", showWarnings = FALSE)
checks <- dplyr::bind_rows(checks)
write.csv(checks, "artifacts/distortions-verification.csv", row.names = FALSE)
print(checks)
