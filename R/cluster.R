#' Declare independent clusters for model-based change inference
#'
#' Clustering describes dependence under a declared repeated-sampling model.
#' It does not establish probability sampling or identify an effect of deliberation.
#' @param clusters Character column names, resolved first in derived outcomes
#'   (for example `group_id`), then assignments, then people. Assignment fields
#'   join by event, episode and person; people fields join by event and person.
#'   Multiple columns identify one composite cluster, not multiway clustering.
#' @param target Population or repeated-process target of inference.
#' @param rationale Why these clusters can be treated as independent.
#' @return A specification for `audit_config(cluster = ...)`.
#' @examples
#' cluster_inference("group_id", "Comparable small groups", "Independent groups")
#' @export
cluster_inference <- function(clusters, target, rationale) {
  for (v in list(clusters, target, rationale)) {
    if (!is.character(v) || !length(v) || anyNA(v) || any(!nzchar(trimws(v)))) {
      cli::cli_abort(
        "Cluster columns, target and rationale must be nonempty text."
      )
    }
  }
  if (
    length(target) != 1L || length(rationale) != 1L || anyDuplicated(clusters)
  ) {
    cli::cli_abort(
      "Supply unique cluster columns and one target and rationale."
    )
  }
  structure(
    list(clusters = clusters, target = target, rationale = rationale),
    class = "cluster_inference"
  )
}

cluster_interval <- function(d, confidence) {
  if (length(unique(d$cluster)) < 2L) {
    cli::cli_abort("Need at least two independent clusters.")
  }
  formula <- if (all(d$focal == 0)) change ~ 1 else change ~ focal
  fit <- stats::lm(formula, data = d, weights = d$weight)
  term <- if (all(d$focal == 0)) "(Intercept)" else "focal"
  if (is.na(stats::coef(fit)[term])) {
    cli::cli_abort("Contrast is not estimable.")
  }
  if (
    sum(stats::residuals(fit)^2) <=
      .Machine$double.eps * max(1, sum(d$change^2))
  ) {
    cli::cli_abort(paste(
      "Residual variance is numerically zero; model-based uncertainty is",
      "unassessable."
    ))
  }
  test <- clubSandwich::coef_test(
    fit,
    vcov = "CR2",
    cluster = d$cluster,
    test = "Satterthwaite",
    coefs = term
  )
  if (!is.finite(test$df_Satt) || test$df_Satt < 1 || !is.finite(test$SE)) {
    cli::cli_abort("Insufficient effective degrees of freedom.")
  }
  critical <- stats::qt((1 + confidence) / 2, test$df_Satt)
  c(
    estimate = test$beta,
    se = test$SE,
    df = test$df_Satt,
    lower = test$beta - critical * test$SE,
    upper = test$beta + critical * test$SE,
    p_value = test$p_Satt
  )
}

cluster_metrics <- function(x, contrasts, config) {
  if (is.null(config$cluster)) {
    return(data.frame())
  }
  d <- paired_responses(x, "paired", config$weighted)
  people <- table_data(x, "people")
  cols <- config$cluster$clusters
  if (!nrow(d)) {
    return(data.frame())
  }
  assignments <- table_data(x, "assignments")
  extra <- intersect(setdiff(cols, names(d)), names(assignments))
  if (length(extra)) {
    keys <- c("event_id", "episode_id", "person_id")
    d <- dplyr::left_join(
      d,
      assignments[c(keys, extra)],
      by = keys,
      relationship = "many-to-one"
    )
  }
  extra <- intersect(setdiff(cols, names(d)), names(people))
  if (length(extra)) {
    d <- dplyr::left_join(
      d,
      people[c("event_id", "person_id", extra)],
      by = c("event_id", "person_id"),
      relationship = "many-to-one"
    )
  }
  rows <- list()
  for (z in split_rows(d, c("event_id", "episode_id", "item_id"))) {
    domains <- c(list(NULL), contrasts)
    for (ct in domains) {
      domain <- z
      if (!is.null(ct)) {
        side <- contrast_side(domain, ct)
        domain$focal <- as.numeric(side == "focal")
        domain <- domain[side %in% c("focal", "reference"), , drop = FALSE]
      } else {
        domain$focal <- 0
      }
      zz <- domain[
        domain$paired & is.finite(domain$pre) & is.finite(domain$post),
        ,
        drop = FALSE
      ]
      reason <- ""
      fit <- c(
        estimate = NA_real_,
        se = NA_real_,
        df = NA_real_,
        lower = NA_real_,
        upper = NA_real_,
        p_value = NA_real_
      )
      n_clusters <- NA_integer_
      if (!all(cols %in% names(zz)) || anyNA(zz[intersect(cols, names(zz))])) {
        reason <- "Missing declared cluster identifiers."
      } else if (!is.null(ct) && length(unique(zz$focal)) != 2L) {
        reason <- "Both focal and reference respondents are required."
      } else if (!requireNamespace("clubSandwich", quietly = TRUE)) {
        reason <- "Install clubSandwich for CR2 inference."
      } else {
        zz$cluster <- row_key(zz, cols)
        zz$change <- zz$post - zz$pre
        n_clusters <- length(unique(zz$cluster))
        attempt <- tryCatch(
          cluster_interval(zz, config$confidence),
          error = identity
        )
        if (inherits(attempt, "error")) {
          reason <- conditionMessage(attempt)
        } else {
          fit <- attempt
        }
      }
      row <- metric(
        "cluster_inference",
        if (is.null(ct)) "cluster_mean_change" else "cluster_change_difference",
        fit["estimate"],
        z$event_id[1],
        z$episode_id[1],
        item_id = z$item_id[1],
        contrast_id = if (is.null(ct)) NA_character_ else ct$id,
        n = nrow(zz),
        denominator = nrow(domain),
        coverage = if (nrow(domain)) nrow(zz) / nrow(domain) else NA_real_,
        se = fit["se"],
        lower = fit["lower"],
        upper = fit["upper"],
        p_value = fit["p_value"],
        reason = reason,
        method = "Model-based paired change; CR2/Satterthwaite; declared independent clusters"
      )
      row$n_clusters <- n_clusters
      row$df <- unname(fit["df"])
      rows[[length(rows) + 1L]] <- row
    }
  }
  dplyr::bind_rows(rows)
}

adjust_families <- function(metrics, families) {
  metrics$test_family <- rep(NA_character_, nrow(metrics))
  metrics$p_adjusted <- rep(NA_real_, nrow(metrics))
  if (is.null(families)) {
    return(metrics)
  }
  unknown <- setdiff(unlist(families), metrics$metric)
  if (length(unknown)) {
    cli::cli_abort("Unknown family metrics: {unknown}.")
  }
  for (family in names(families)) {
    keep <- metrics$metric %in% families[[family]]
    metrics$test_family[keep] <- family
    metrics$p_adjusted[keep] <- stats::p.adjust(
      metrics$p_value[keep],
      method = "holm"
    )
  }
  metrics
}

attrition_metrics <- function(x, contrasts) {
  d <- paired_responses(x, "available")
  rows <- list()
  units <- c(
    split_rows(d, c("event_id", "episode_id", "item_id")),
    split_rows(
      d[!is.na(d$group_id), , drop = FALSE],
      c("event_id", "episode_id", "item_id", "group_id")
    )
  )
  n_event <- length(split_rows(d, c("event_id", "episode_id", "item_id")))
  for (i in seq_along(units)) {
    z <- units[[i]]
    domains <- list(all = rep(TRUE, nrow(z)))
    for (ct in contrasts) {
      side <- contrast_side(z, ct)
      domains[[paste0(ct$id, ":focal")]] <- side %in% "focal"
      domains[[paste0(ct$id, ":reference")]] <- side %in% "reference"
    }
    for (nm in names(domains)) {
      zz <- z[domains[[nm]], , drop = FALSE]
      baseline <- is.finite(zz$pre)
      n <- sum(baseline)
      rows[[length(rows) + 1L]] <- metric(
        "attrition",
        "post_attrition",
        if (n) mean(!is.finite(zz$post[baseline])) else NA_real_,
        z$event_id[1],
        z$episode_id[1],
        group_id = if (i > n_event) z$group_id[1] else NA_character_,
        item_id = z$item_id[1],
        detail = nm,
        n = n,
        denominator = nrow(zz),
        coverage = if (nrow(zz)) n / nrow(zz) else NA_real_,
        reason = if (!n) "No observed baseline respondents." else "",
        method = "Post missingness among observed baseline respondents; no MAR assumption"
      )
    }
  }
  dplyr::bind_rows(rows)
}
