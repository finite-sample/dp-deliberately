#' Configure a reproducible audit
#' @param membership `"paired"` (default) or `"available"` wave membership.
#' @param weighted Use supplied participant weights for descriptive outcomes
#'   and sample benchmarks. Dialogue metrics retain observed speaker weights.
#' @param baseline_predictors Explicit pre-deliberation columns used in selection
#'   and assignment diagnostics. Baseline items use `item:episode_id:item_id`.
#' @param annotation_status Included review statuses. Defaults to accepted and
#'   adjudicated; rejected annotations cannot be included.
#' @param probability_sample Explicit declaration enabling sampling inference.
#' @param confidence Confidence level for supported intervals.
#' @param seed Reproducible seed, without changing the caller's random state.
#' @param permutations Monte Carlo draws for supported assignment tests.
#' @return A configuration list recorded in [audit_dp()] output.
#' @export
audit_config <- function(
  membership = c("paired", "available"),
  weighted = FALSE,
  baseline_predictors = character(),
  annotation_status = c("accepted", "adjudicated"),
  probability_sample = FALSE,
  confidence = 0.95,
  seed = 104729L,
  permutations = 999L
) {
  membership <- match.arg(membership)
  if (
    !all(annotation_status %in% c("accepted", "adjudicated", "unreviewed")) ||
      !length(annotation_status)
  ) {
    stop("Invalid annotation_status.")
  }
  if (
    length(confidence) != 1L ||
      !is.finite(confidence) ||
      confidence <= 0 ||
      confidence >= 1 ||
      length(permutations) != 1L ||
      !is.finite(permutations) ||
      permutations < 1 ||
      permutations != as.integer(permutations)
  ) {
    stop("Invalid inference configuration.")
  }
  list(
    membership = membership,
    weighted = weighted,
    baseline_predictors = baseline_predictors,
    annotation_status = annotation_status,
    probability_sample = probability_sample,
    confidence = confidence,
    seed = seed,
    permutations = as.integer(permutations)
  )
}

#' Run all supported audit modules
#' @param x A [deliberation_data()] bundle.
#' @param contrasts A contrast or list of definitions from [define_contrast()].
#' @param policy Optional versioned [audit_policy()]. NULL produces no verdicts.
#' @param config Configuration from [audit_config()].
#' @return A `deliberation_audit` with metrics, capabilities, validation,
#'   policy_results, evaluated contrasts, evidence, and reproducibility metadata.
#' @export
audit_dp <- function(
  x,
  contrasts = list(),
  policy = NULL,
  config = audit_config()
) {
  config <- do.call(audit_config, config)
  validation <- validate_data(x)
  if (any(validation$severity == "error")) {
    condition <- structure(
      list(
        message = paste(
          "Invalid audit data:",
          paste(
            unique(validation$message[validation$severity == "error"]),
            collapse = "; "
          )
        ),
        call = NULL,
        validation = validation
      ),
      class = c("deliberation_validation_error", "error", "condition")
    )
    stop(condition)
  }
  contrasts <- resolve_contrasts(x, contrasts)
  capabilities <- available_analyses(x, contrasts)
  rows <- list()
  for (event in table_data(x, "events")$event_id) {
    ex <- x
    ex$tables <- lapply(x$tables, function(z) {
      z[z$event_id == event, , drop = FALSE]
    })
    ready <- capabilities$analysis[
      capabilities$event_id == event & capabilities$readiness == "ready"
    ]
    if (any(c("sample", "attendance", "assignment") %in% ready)) {
      rows[[length(rows) + 1L]] <- design_metrics(ex, contrasts, config)
    }
    if ("participation" %in% ready) {
      rows[[length(rows) + 1L]] <- participation_metrics(ex, contrasts, config)
    }
    if ("exchange" %in% ready) {
      rows[[length(rows) + 1L]] <- exchange_metrics(ex, contrasts, config)
    }
    if (any(c("arguments", "materials") %in% ready)) {
      rows[[length(rows) + 1L]] <- argument_metrics(ex, contrasts, config)
    }
    if ("exposure" %in% ready) {
      rows[[length(rows) + 1L]] <- exposure_metrics(ex, contrasts, config)
    }
    if ("outcomes" %in% ready) {
      rows[[length(rows) + 1L]] <- outcome_metrics(ex, contrasts, config)
    }
    if ("group_effects" %in% ready) {
      rows[[length(rows) + 1L]] <- group_metrics(ex, contrasts, config)
    }
    if ("survey_inference" %in% ready) {
      rows[[length(rows) + 1L]] <- survey_metrics(ex, contrasts, config)
    }
  }
  metrics <- bind_rows(rows)
  if (!nrow(metrics)) {
    metrics <- metric("", "", NA_real_, "")[FALSE, ]
  }
  metrics$metric_id <- sprintf("m%06d", seq_len(nrow(metrics)))
  metrics$units[metrics$metric == "knowledge_gain_sd"] <- "baseline SD"
  metrics$units[metrics$metric == "cross_person_links"] <- "coded links"
  metrics$units[metrics$metric %in% c("moderator_stance_mean", "expert_stance_mean")] <- "codebook stance points"
  metrics$units[metrics$metric == "assignment_randomization_statistic"] <- "sum of squared scale fractions"
  for (i in seq_len(nrow(capabilities))) {
    m <- metrics[
      metrics$event_id == capabilities$event_id[i] &
        metrics$analysis == capabilities$analysis[i],
      ,
      drop = FALSE
    ]
    capabilities$status[i] <- if (capabilities$readiness[i] == "unavailable") {
      "unavailable"
    } else if (any(m$status == "computed")) {
      "computed"
    } else {
      "unassessable"
    }
    if (capabilities$status[i] == "unassessable") {
      capabilities$reason[i] <- if (nrow(m)) {
        paste(unique(m$reason[nzchar(m$reason)]), collapse = "; ")
      } else {
        "Tables present, but no eligible diagnostic units or declared predictors."
      }
    }
  }
  evidence <- list(
    annotations = table_data(x, "annotations"),
    links = table_data(x, "links")
  )
  for (nm in c("turns", "materials", "arguments")) {
    d <- table_data(x, nm)
    if (!is.null(d)) evidence[[nm]] <- d
  }
  result <- structure(
    list(
      metrics = metrics,
      capabilities = capabilities,
      validation = validation,
      policy_results = evaluate_policy(metrics, policy, x),
      policy = policy,
      contrasts = contrasts,
      evidence = evidence,
      metadata = list(
        schema_version = x$schema_version,
        package_version = as.character(utils::packageVersion("deliberately")),
        created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        config = config,
        input_hashes = vapply(x$tables, digest::digest, "", algo = "sha256"),
        provenance = x$provenance,
        session = utils::sessionInfo()
      )
    ),
    class = "deliberation_audit"
  )
  result
}

#' @export
print.deliberation_audit <- function(x, ...) {
  cat("Deliberation audit\n")
  cat(
    nrow(x$metrics),
    "diagnostics;",
    sum(x$metrics$status == "computed"),
    "computed\n"
  )
  cat(
    sum(x$capabilities$status == "unavailable"),
    "event/analysis combinations unavailable\n"
  )
  if (is.null(x$policy)) {
    cat("No policy supplied; no PASS/FAIL verdicts.\n")
  } else {
    print(x$policy_results)
  }
  invisible(x)
}
