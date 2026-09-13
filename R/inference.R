group_statistic <- function(pre, post, group) {
  base <- stats::lm(post ~ pre)
  augmented <- stats::lm(post ~ pre + factor(group))
  max(0, sum(stats::residuals(base)^2) - sum(stats::residuals(augmented)^2))
}

group_metrics <- function(x, contrasts, config) {
  d <- paired_responses(x, "paired")
  collector <- new.env(parent = emptyenv())
  collector$rows <- list()
  a <- table_data(x, "assignments")
  randomization <- table_data(x, "randomization")
  for (z in split_rows(d, c("event_id", "episode_id", "item_id"))) {
    eligible <- z$paired & !is.na(z$group_id)
    zz <- z[eligible, , drop = FALSE]
    add <- function(
      id,
      value,
      reason = "",
      group = NA_character_,
      analysis = "group_effects",
      p_value = NA_real_,
      method = "baseline-adjusted descriptive association"
    ) {
      collector$rows[[length(collector$rows) + 1L]] <- metric(
        analysis,
        id,
        value,
        z$event_id[1],
        z$episode_id[1],
        group,
        item_id = z$item_id[1],
        n = nrow(zz),
        denominator = nrow(z),
        coverage = nrow(zz) / nrow(z),
        reason = reason,
        p_value = p_value,
        method = method
      )
    }
    if (
      length(unique(zz$group_id)) < 2L ||
        nrow(zz) <= length(unique(zz$group_id)) + 1L
    ) {
      add(
        "group_partial_r2",
        NA_real_,
        "Need multiple groups and residual degrees of freedom."
      )
      next
    }
    residual <- stats::residuals(stats::lm(post ~ pre, data = zz))
    ss <- sum(residual^2)
    stat <- group_statistic(zz$pre, zz$post, zz$group_id)
    add("group_partial_r2", if (ss > 1e-12) stat / ss else NA_real_)
    for (group in unique(zz$group_id)) {
      add(
        "group_adjusted_deviation",
        mean(residual[zz$group_id == group]),
        group = group
      )
    }
    moderators <- table_data(x, "moderators")
    sessions <- table_data(x, "sessions")
    if (!is.null(moderators) && !is.null(sessions)) {
      m <- merge(moderators, sessions, by = c("event_id", "session_id"))
      m <- unique(m[
        m$event_id == z$event_id[1] & m$episode_id == z$episode_id[1],
        c("group_id", "person_id"),
        drop = FALSE
      ])
      if (nrow(m)) {
        counts <- table(m$person_id)
        crossed <- any(counts > 1) && any(table(m$group_id) > 1)
        add(
          "moderator_separate_effect",
          NA_real_,
          if (crossed) {
            paste(
              "Multiple moderators per group require a session-level outcome design;",
              "episode outcomes cannot separate exposure."
            )
          } else {
            "Moderator indicators are confounded with episode group indicators."
          }
        )
        for (id in names(counts[counts > 1])) {
          keep <- zz$group_id %in% m$group_id[m$person_id == id]
          add(
            "moderator_adjusted_association",
            safe_mean(residual[keep]),
            reason = paste(
              "Moderator",
              id,
              "association includes group composition and exposure differences."
            )
          )
        }
      }
    }
    rr <- if (is.null(randomization)) {
      data.frame()
    } else {
      randomization[
        randomization$event_id == z$event_id[1] &
          randomization$episode_id == z$episode_id[1],
        ,
        drop = FALSE
      ]
    }
    if (!nrow(rr)) {
      next
    }
    aa <- a[
      a$event_id == z$event_id[1] & a$episode_id == z$episode_id[1],
      ,
      drop = FALSE
    ]
    valid <- rr$mechanism %in%
      c("complete_fixed_sizes", "blocked_fixed_sizes") &&
      nrow(aa) == nrow(zz) &&
      setequal(aa$person_id, zz$person_id) &&
      all(
        !is.na(aa$intended_group) &
          !is.na(aa$actual_group) &
          aa$intended_group == aa$actual_group
      )
    if (!valid) {
      add(
        "assignment_randomization_statistic",
        NA_real_,
        paste(
          "Requires supported fixed-size randomization, full paired roster",
          "and no assignment deviations."
        ),
        analysis = "randomization"
      )
      next
    }
    block <- if (rr$mechanism == "complete_fixed_sizes") {
      rep("all", nrow(zz))
    } else {
      aa$block[match(zz$person_id, aa$person_id)]
    }
    if (anyNA(block)) {
      add(
        "assignment_randomization_statistic",
        NA_real_,
        "Missing assignment blocks.",
        analysis = "randomization"
      )
      next
    }
    conditions <- sort(unique(zz$group_id))
    declaration <- if (rr$mechanism == "complete_fixed_sizes") {
      randomizr::declare_ra(
        N = nrow(zz),
        m_each = as.integer(table(factor(zz$group_id, levels = conditions))),
        conditions = conditions
      )
    } else {
      randomizr::declare_ra(
        N = nrow(zz),
        blocks = block,
        block_m_each = unclass(table(
          block,
          factor(zz$group_id, levels = conditions)
        )),
        conditions = conditions
      )
    }
    draws <- with_seed(
      config$seed,
      replicate(config$permutations, {
        labels <- randomizr::conduct_ra(declaration)
        group_statistic(zz$pre, zz$post, labels)
      })
    )
    add(
      "assignment_randomization_statistic",
      stat,
      analysis = "randomization",
      p_value = (1 + sum(draws >= stat - 1e-12)) / (1 + length(draws)),
      method = paste(
        "Monte Carlo randomization; sharp null of no assignment effects;",
        rr$mechanism,
        ";",
        config$permutations,
        "draws; plus-one p-value"
      )
    )
  }
  dplyr::bind_rows(collector$rows)
}

survey_metrics <- function(x, contrasts, config) {
  s <- table_data(x, "sampling")
  d <- paired_responses(x, "paired")
  if (is.null(s) || !nrow(d)) {
    return(data.frame())
  }
  rows <- list()
  for (z in split_rows(d, c("event_id", "episode_id", "item_id"))) {
    ss <- s[s$event_id == z$event_id[1], , drop = FALSE]
    domains <- list(all = rep(TRUE, nrow(ss)))
    for (ct in contrasts) {
      side <- contrast_side(ss, ct)
      domains[[paste0(ct$id, ":focal")]] <- !is.na(side) & side == "focal"
      domains[[paste0(ct$id, ":reference")]] <- !is.na(side) &
        side == "reference"
    }
    idx <- match(ss$person_id, z$person_id)
    ss$change <- z$post[idx] - z$pre[idx]
    for (domain in names(domains)) {
      keep <- domains[[domain]] & is.finite(ss$change)
      reason <- ""
      result <- c(
        estimate = NA_real_,
        se = NA_real_,
        lower = NA_real_,
        upper = NA_real_
      )
      if (!isTRUE(config$probability_sample)) {
        reason <- "Set probability_sample = TRUE only for a documented probability sample."
      } else if (!requireNamespace("survey", quietly = TRUE)) {
        reason <- "Install the survey package for design-based inference."
      } else if (anyNA(ss[c("psu", "stratum")])) {
        reason <- paste(
          "Sampling requires explicit PSU and stratum IDs (one stratum for",
          "unstratified samples)."
        )
      } else if (!all(z$person_id %in% ss$person_id)) {
        reason <- "Response records missing from the sampling roster."
      } else if (sum(keep) < 2L) {
        reason <- "Insufficient paired observations in this domain."
      } else {
        attempt <- tryCatch(
          survey_interval(ss, keep, config$confidence),
          error = identity
        )
        if (inherits(attempt, "error")) {
          reason <- conditionMessage(attempt)
        } else {
          result <- attempt
        }
      }
      rows[[length(rows) + 1L]] <- metric(
        "survey_inference",
        "survey_mean_change",
        result["estimate"],
        z$event_id[1],
        z$episode_id[1],
        item_id = z$item_id[1],
        detail = domain,
        n = sum(keep),
        denominator = sum(domains[[domain]]),
        coverage = sum(keep) / sum(domains[[domain]]),
        se = result["se"],
        lower = result["lower"],
        upper = result["upper"],
        reason = reason,
        method = paste(
          "Taylor survey variance; design-df t interval; paired-response",
          "domain; supplied weights"
        )
      )
    }
  }
  dplyr::bind_rows(rows)
}


survey_interval <- function(data, keep, confidence) {
  old <- options(survey.lonely.psu = "fail")
  on.exit(options(old))
  design <- survey::svydesign(
    ids = ~psu,
    strata = ~stratum,
    weights = ~weight,
    fpc = if ("fpc" %in% names(data)) ~fpc else NULL,
    data = data,
    nest = TRUE
  )
  domain_design <- design[keep, ]
  est <- survey::svymean(~change, domain_design)
  df <- survey::degf(domain_design)
  if (df < 1) {
    stop("No design degrees of freedom.")
  }
  ci <- stats::confint(est, level = confidence, df = df)
  c(
    estimate = unname(stats::coef(est)[1]),
    se = sqrt(stats::vcov(est)[1, 1]),
    lower = ci[1, 1],
    upper = ci[1, 2]
  )
}
