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
#' @param cluster Optional specification from [cluster_inference()].
#' @param test_families Named list of metric-name vectors. Each family includes
#'   every matching metric row across events; Holm adjustment retains raw p-values.
#' @return A configuration list recorded in [audit_dp()] output.
#' @examples
#' audit_config(permutations = 99, membership = "paired")
#' @export
audit_config <- function(
  membership = c("paired", "available"),
  weighted = FALSE,
  baseline_predictors = character(),
  annotation_status = c("accepted", "adjudicated"),
  probability_sample = FALSE,
  confidence = 0.95,
  seed = 104729L,
  permutations = 999L,
  cluster = NULL,
  test_families = NULL
) {
  if (
    !is.logical(weighted) ||
      length(weighted) != 1L ||
      is.na(weighted) ||
      !is.logical(probability_sample) ||
      length(probability_sample) != 1L ||
      is.na(probability_sample)
  ) {
    cli::cli_abort("Flags must be single nonmissing logical values.")
  }
  if (
    !is.numeric(seed) ||
      length(seed) != 1L ||
      !is.finite(seed) ||
      seed < 0 ||
      seed > .Machine$integer.max ||
      seed != floor(seed)
  ) {
    cli::cli_abort("Seed must be a nonnegative R integer.")
  }
  if (
    !is.character(baseline_predictors) ||
      anyNA(baseline_predictors) ||
      any(!nzchar(baseline_predictors)) ||
      anyDuplicated(baseline_predictors)
  ) {
    cli::cli_abort("baseline_predictors must contain unique character names.")
  }
  if (!is.null(cluster)) {
    cluster <- do.call(cluster_inference, unclass(cluster))
  }
  if (!is.null(test_families)) {
    if (
      !is.list(test_families) ||
        is.null(names(test_families)) ||
        anyNA(names(test_families)) ||
        any(!nzchar(names(test_families))) ||
        anyDuplicated(names(test_families)) ||
        any(
          !vapply(
            test_families,
            function(v) {
              is.character(v) && length(v) > 0L && !anyNA(v) && all(nzchar(v))
            },
            logical(1)
          )
        ) ||
        anyDuplicated(unlist(test_families))
    ) {
      cli::cli_abort(
        "Test families must be uniquely named, nonoverlapping metric-name vectors."
      )
    }
  }
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
    permutations = as.integer(permutations),
    cluster = cluster,
    test_families = test_families
  )
}

#' Run all supported audit modules
#' @param x A [deliberation_data()] bundle.
#' @param contrasts A contrast or list of definitions from [define_contrast()].
#' @param policy Optional versioned [audit_policy()]. NULL produces no verdicts.
#' @param config Configuration from [audit_config()].
#' @return A `deliberation_audit` with metrics, capabilities, validation,
#'   policy_results, evaluated contrasts, evidence, and reproducibility metadata.
#' @examples
#' result <- audit_dp(example_deliberation(), config = audit_config(permutations = 9))
#' head(result$metrics)
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
      rows[[length(rows) + 1L]] <- attrition_metrics(ex, contrasts)
      rows[[length(rows) + 1L]] <- cluster_metrics(ex, contrasts, config)
    }
    if ("group_effects" %in% ready) {
      rows[[length(rows) + 1L]] <- group_metrics(ex, contrasts, config)
    }
    if ("survey_inference" %in% ready) {
      rows[[length(rows) + 1L]] <- survey_metrics(ex, contrasts, config)
    }
  }
  metrics <- dplyr::bind_rows(rows)
  if (!nrow(metrics)) {
    metrics <- metric("", "", NA_real_, "")[FALSE, ]
  }
  metrics <- adjust_families(metrics, config$test_families)
  metrics$metric_id <- sprintf("m%06d", seq_len(nrow(metrics)))
  metrics$units[metrics$metric == "knowledge_gain_sd"] <- "baseline SD"
  metrics$units[metrics$metric == "cross_person_links"] <- "coded links"
  metrics$units[
    metrics$metric %in% c("moderator_stance_mean", "expert_stance_mean")
  ] <- "codebook stance points"
  metrics$units[
    metrics$metric == "assignment_randomization_statistic"
  ] <- "sum of squared scale fractions"
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
      dictionary = x$tables[intersect(
        names(x$tables),
        c("events", "episodes", "items")
      )],
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
