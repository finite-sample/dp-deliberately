baseline_people <- function(x) {
  p <- table_data(x, "people")
  if (is.null(p)) {
    return(data.frame())
  }
  p <- p[p$role == "participant", , drop = FALSE]
  d <- paired_responses(x, "available")
  if (nrow(d)) {
    for (item in unique(d$item_id)) {
      z <- d[d$item_id == item, , drop = FALSE]
      for (episode in unique(z$episode_id)) {
        zz <- z[z$episode_id == episode, , drop = FALSE]
        idx <- match(
          row_key(p, c("event_id", "person_id")),
          row_key(zz, c("event_id", "person_id"))
        )
        p[[paste0("item:", episode, ":", item)]] <- zz$pre[idx]
      }
    }
  }
  p
}

auc_score <- function(y, score) {
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  if (!n1 || !n0 || any(!is.finite(score))) {
    return(NA_real_)
  }
  (sum(rank(score, ties.method = "average")[y == 1]) - n1 * (n1 + 1) / 2) /
    (n1 * n0)
}

attendance_auc <- function(d, predictors, seed) {
  if (!length(predictors) || !all(predictors %in% names(d))) {
    return(list(value = NA_real_, n = 0L))
  }
  z <- d[c("attended", predictors)]
  z <- z[stats::complete.cases(z), , drop = FALSE]
  y <- as.integer(z$attended)
  folds <- min(5L, sum(y == 0), sum(y == 1))
  if (folds < 2L) {
    return(list(value = NA_real_, n = nrow(z)))
  }
  fold <- integer(nrow(z))
  with_seed(seed, {
    for (v in 0:1) {
      idx <- which(y == v)
      fold[idx] <- sample(rep(seq_len(folds), length.out = length(idx)))
    }
  })
  score <- rep(NA_real_, nrow(z))
  names(z) <- c("attended", paste0("x", seq_along(predictors)))
  for (k in seq_len(folds)) {
    score[fold == k] <- tryCatch(
      {
        fit <- suppressWarnings(stats::glm(
          attended ~ .,
          data = z[fold != k, , drop = FALSE],
          family = stats::binomial()
        ))
        if (!isTRUE(fit$converged)) {
          stop("Nonconvergence")
        }
        as.numeric(stats::predict(
          fit,
          z[fold == k, , drop = FALSE],
          type = "response"
        ))
      },
      error = function(e) rep(NA_real_, sum(fold == k))
    )
  }
  list(value = auc_score(y, score), n = nrow(z))
}

design_metrics <- function(x, contrasts, config) {
  p <- baseline_people(x)
  b <- table_data(x, "benchmarks")
  r <- table_data(x, "recruitment")
  a <- table_data(x, "assignments")
  rows <- list()
  for (event in table_data(x, "events")$event_id) {
    z <- p[p$event_id == event, , drop = FALSE]
    sample <- z
    rr <- if (!is.null(r)) {
      r[r$event_id == event, , drop = FALSE]
    } else {
      data.frame()
    }
    if (nrow(rr)) {
      sample <- z[
        z$person_id %in% rr$person_id[!is.na(rr$attended) & rr$attended],
        ,
        drop = FALSE
      ]
    }
    if (!is.null(b)) {
      bb <- b[b$event_id == event, , drop = FALSE]
      for (i in seq_len(nrow(bb))) {
        variable <- bb$variable[i]
        vals <- if (variable %in% names(sample)) {
          sample[[variable]]
        } else {
          rep(NA_real_, nrow(sample))
        }
        numeric <- is.na(bb$level[i]) || !nzchar(bb$level[i])
        if (!numeric) {
          vals <- ifelse(
            is.na(vals),
            NA,
            as.numeric(as.character(vals) == bb$level[i])
          )
        }
        if (!is.numeric(vals)) {
          vals <- rep(NA_real_, length(vals))
        }
        weights <- rep(1, nrow(sample))
        if (config$weighted) {
          ss <- table_data(x, "sampling")
          idx <- match(
            row_key(sample, c("event_id", "person_id")),
            row_key(ss, c("event_id", "person_id"))
          )
          if (anyNA(idx)) {
            stop("Missing sample benchmark weights.")
          }
          weights <- ss$weight[idx]
        }
        estimate <- safe_mean(vals, weights)
        target_sd <- if (numeric) {
          bb$sd[i]
        } else {
          sqrt(bb$target[i] * (1 - bb$target[i]))
        }
        for (id in c("benchmark_difference", "benchmark_smd")) {
          value <- estimate - bb$target[i]
          if (id == "benchmark_smd") {
            value <- if (is.finite(target_sd) && target_sd > 0) {
              value / target_sd
            } else {
              NA_real_
            }
          }
          rows[[length(rows) + 1L]] <- metric(
            "sample",
            id,
            value,
            event,
            detail = paste(
              bb$benchmark_id[i],
              variable,
              bb$level[i],
              bb$population[i],
              sep = " | "
            ),
            n = sum(is.finite(vals)),
            denominator = nrow(sample),
            coverage = if (nrow(sample)) mean(is.finite(vals)) else NA_real_,
            units = if (id == "benchmark_smd") "target SD" else "variable units"
          )
        }
      }
    }
    if (nrow(rr)) {
      zz <- merge(z, rr, by = c("event_id", "person_id"))
      known <- !is.na(zz$attended)
      rows[[length(rows) + 1L]] <- metric(
        "attendance",
        "attendance_rate",
        safe_mean(as.numeric(zz$attended)),
        event,
        n = sum(known),
        denominator = nrow(zz),
        coverage = mean(known)
      )
      for (stage in unique(zz$stage)) {
        rows[[length(rows) + 1L]] <- metric(
          "attendance",
          "recruitment_stage_count",
          sum(zz$stage == stage),
          event,
          detail = stage,
          n = nrow(zz),
          denominator = nrow(zz),
          units = "people"
        )
      }
      auc <- attendance_auc(zz, config$baseline_predictors, config$seed)
      rows[[length(rows) + 1L]] <- metric(
        "attendance",
        "attendance_auc",
        auc$value,
        event,
        n = auc$n,
        denominator = nrow(zz),
        coverage = auc$n / nrow(zz),
        method = "stratified out-of-fold logistic prediction; complete baseline cases",
        reason = if (!length(config$baseline_predictors)) {
          "Declare baseline_predictors to fit attendance prediction."
        } else {
          ""
        }
      )
      for (variable in intersect(config$baseline_predictors, names(zz))) {
        vals <- zz[[variable]]
        levels <- if (is.numeric(vals)) {
          NA_character_
        } else {
          unique(as.character(vals[!is.na(vals)]))
        }
        for (level in levels) {
          v <- if (is.na(level)) {
            vals
          } else {
            ifelse(is.na(vals), NA_real_, as.numeric(vals == level))
          }
          yes <- known & zz$attended
          no <- known & !zz$attended
          rows[[length(rows) + 1L]] <- metric(
            "attendance",
            "attendance_baseline_difference",
            safe_mean(v[yes]) - safe_mean(v[no]),
            event,
            detail = paste(variable, level, sep = " | "),
            n = sum(is.finite(v) & known),
            denominator = nrow(zz),
            units = "variable units"
          )
        }
      }
    }
  }
  if (!is.null(a)) {
    for (aa in split_rows(a, c("event_id", "episode_id"))) {
      zz <- merge(aa, p, by = c("event_id", "person_id"))
      for (variable in intersect(config$baseline_predictors, names(zz))) {
        vals <- zz[[variable]]
        levels <- if (is.numeric(vals)) {
          NA_character_
        } else {
          unique(as.character(vals[!is.na(vals)]))
        }
        for (level in levels) {
          v <- if (is.na(level)) {
            vals
          } else {
            ifelse(is.na(vals), NA_real_, as.numeric(vals == level))
          }
          grand <- safe_mean(v)
          sd <- safe_sd(v)
          for (group in unique(stats::na.omit(zz$intended_group))) {
            g <- !is.na(zz$intended_group) & zz$intended_group == group
            rows[[length(rows) + 1L]] <- metric(
              "assignment",
              "assignment_smd",
              if (is.finite(sd) && sd > 0) {
                (safe_mean(v[g]) - grand) / sd
              } else {
                NA_real_
              },
              aa$event_id[1],
              aa$episode_id[1],
              group,
              detail = paste(variable, level, sep = " | "),
              n = sum(is.finite(v[g])),
              denominator = sum(g),
              units = "event baseline SD"
            )
          }
        }
      }
      rows[[length(rows) + 1L]] <- metric(
        "assignment",
        "assignment_deviation_rate",
        safe_mean(as.numeric(aa$intended_group != aa$actual_group)),
        aa$event_id[1],
        aa$episode_id[1],
        n = sum(stats::complete.cases(aa[c("intended_group", "actual_group")])),
        denominator = nrow(aa)
      )
    }
  }
  bind_rows(rows)
}

#' Detectable paired change under a declared planning model
#' @param n Number of paired observations.
#' @param sd_change Assumed standard deviation of individual changes.
#' @param design_effect Variance inflation relative to independent observations.
#' @param power Desired power; default 0.8.
#' @param alpha Two-sided test size; default 0.05.
#' @return Normal-approximation standard error, interval half width and minimum
#'   detectable change, in the same units as sd_change. This is a planning
#'   approximation, not achieved power inferred from an observed effect.
#' @export
detectable_change <- function(
  n,
  sd_change,
  design_effect = 1,
  power = 0.8,
  alpha = 0.05
) {
  if (
    any(!is.finite(c(n, sd_change, design_effect, power, alpha))) ||
      n <= 1 ||
      sd_change <= 0 ||
      design_effect <= 0 ||
      power <= 0 ||
      power >= 1 ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    stop("Invalid precision assumptions.")
  }
  se <- sd_change * sqrt(design_effect / n)
  data.frame(
    n = n,
    sd_change = sd_change,
    design_effect = design_effect,
    power = power,
    alpha = alpha,
    se = se,
    half_width = stats::qnorm(1 - alpha / 2) * se,
    detectable_change = (stats::qnorm(1 - alpha / 2) + stats::qnorm(power)) * se
  )
}
