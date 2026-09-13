paired_responses <- function(x, membership = "paired", weighted = FALSE) {
  r <- table_data(x, "responses")
  w <- table_data(x, "waves")
  it <- table_data(x, "items")
  if (any(vapply(list(r, w, it), is.null, logical(1)))) {
    return(data.frame())
  }
  r <- dplyr::left_join(
    r,
    w[c("event_id", "episode_id", "wave_id", "phase")],
    by = c("event_id", "episode_id", "wave_id"),
    relationship = "many-to-one"
  )
  r <- r[r$phase %in% c("pre", "post"), , drop = FALSE]
  key <- c("event_id", "episode_id", "person_id", "item_id")
  pre <- r[r$phase == "pre", c(key, "value"), drop = FALSE]
  post <- r[r$phase == "post", c(key, "value"), drop = FALSE]
  names(pre)[ncol(pre)] <- "pre_raw"
  names(post)[ncol(post)] <- "post_raw"
  d <- dplyr::full_join(pre, post, by = key, relationship = "one-to-one")
  d <- dplyr::left_join(
    d,
    it,
    by = c("event_id", "item_id"),
    relationship = "many-to-one"
  )
  p <- table_data(x, "people")
  if (!is.null(p)) {
    p <- p[p$role == "participant", c("event_id", "person_id"), drop = FALSE]
    d <- dplyr::inner_join(
      d,
      p,
      by = c("event_id", "person_id"),
      relationship = "many-to-one"
    )
  }
  if (!nrow(d)) {
    return(data.frame())
  }
  d$paired <- is.finite(d$pre_raw) & is.finite(d$post_raw)
  d$pre <- (d$pre_raw - d$lower) / (d$upper - d$lower)
  d$post <- (d$post_raw - d$lower) / (d$upper - d$lower)
  d$midpoint_scaled <- (d$midpoint - d$lower) / (d$upper - d$lower)
  reverse <- d$direction == -1
  d$pre[reverse] <- 1 - d$pre[reverse]
  d$post[reverse] <- 1 - d$post[reverse]
  d$midpoint_scaled[reverse] <- 1 - d$midpoint_scaled[reverse]
  if (membership == "paired") {
    d$pre[!d$paired] <- d$post[!d$paired] <- NA_real_
  }
  d$knowledge_scored <- FALSE
  if (!"scoring" %in% names(d)) {
    d$scoring <- NA_character_
  }
  if (!"correct_value" %in% names(d)) {
    d$correct_value <- NA_real_
  }
  knowledge <- d$kind == "knowledge"
  binary <- knowledge & d$scoring %in% "binary_key" & is.finite(d$correct_value)
  proportion <- knowledge & d$scoring %in% "proportion_correct"
  d$knowledge_scored <- binary | proportion
  d$pre[binary] <- ifelse(
    is.na(d$pre[binary]),
    NA_real_,
    as.numeric(d$pre_raw[binary] == d$correct_value[binary])
  )
  d$post[binary] <- ifelse(
    is.na(d$post[binary]),
    NA_real_,
    as.numeric(d$post_raw[binary] == d$correct_value[binary])
  )
  d$pre[knowledge & !d$knowledge_scored] <- d$post[
    knowledge & !d$knowledge_scored
  ] <- NA_real_
  d$weight <- 1
  if (weighted) {
    s <- table_data(x, "sampling")
    if (is.null(s)) {
      stop("Weighted results require the sampling table.")
    }
    idx <- match(
      row_key(d, c("event_id", "person_id")),
      row_key(s, c("event_id", "person_id"))
    )
    if (anyNA(idx)) {
      stop("Sampling weights missing for response records.")
    }
    d$weight <- s$weight[idx]
  }
  a <- table_data(x, "assignments")
  d$group_id <- NA_character_
  if (!is.null(a)) {
    idx <- match(
      row_key(d, c("event_id", "episode_id", "person_id")),
      row_key(a, c("event_id", "episode_id", "person_id"))
    )
    d$group_id <- as.character(a$actual_group[idx])
  }
  append_knowledge_index(d, it)
}

append_knowledge_index <- function(d, items) {
  rows <- list(d)
  knowledge <- d[d$kind == "knowledge", , drop = FALSE]
  if ("include_in_knowledge" %in% names(knowledge)) {
    knowledge <- knowledge[
      !is.na(knowledge$include_in_knowledge) & knowledge$include_in_knowledge,
      ,
      drop = FALSE
    ]
  }
  for (event in split_rows(knowledge, c("event_id", "episode_id"))) {
    expected <- items[
      items$event_id == event$event_id[1] & items$kind == "knowledge",
      ,
      drop = FALSE
    ]
    if ("include_in_knowledge" %in% names(expected)) {
      expected <- expected[
        !is.na(expected$include_in_knowledge) & expected$include_in_knowledge,
        ,
        drop = FALSE
      ]
    }
    n_items <- nrow(expected)
    if (n_items < 2L) {
      next
    }
    for (z in split_rows(event, c("person_id"))) {
      row <- z[1, , drop = FALSE]
      row$item_id <- "__knowledge_mean__"
      row$label <- "Mean knowledge score (complete declared item set)"
      row$scoring <- "proportion_correct"
      row$correct_value <- NA_real_
      row$lower <- 0
      row$upper <- 1
      row$direction <- 1
      row$midpoint <- row$midpoint_scaled <- NA_real_
      row$pre <- row$pre_raw <- if (
        nrow(z) == n_items && all(is.finite(z$pre))
      ) {
        mean(z$pre)
      } else {
        NA_real_
      }
      row$post <- row$post_raw <- if (
        nrow(z) == n_items && all(is.finite(z$post))
      ) {
        mean(z$post)
      } else {
        NA_real_
      }
      row$paired <- is.finite(row$pre) & is.finite(row$post)
      row$knowledge_scored <- all(z$knowledge_scored)
      rows[[length(rows) + 1L]] <- row
    }
  }
  dplyr::bind_rows(rows)
}

signed_movement <- function(pre, post, reference, eps = 1e-12) {
  if (!all(is.finite(c(pre, post, reference))) || abs(reference - pre) < eps) {
    return(NA_real_)
  }
  delta <- post - pre
  if (abs(delta) < eps) {
    return(0)
  }
  delta * sign(reference - pre)
}

outcome_metrics <- function(x, contrasts, config) {
  d <- paired_responses(x, config$membership, config$weighted)
  if (!nrow(d)) {
    return(data.frame())
  }
  collector <- new.env(parent = emptyenv())
  collector$rows <- list()
  sets <- split_rows(d, c("event_id", "episode_id", "item_id"))
  grouped <- d[!is.na(d$group_id), , drop = FALSE]
  sets <- c(
    lapply(sets, function(z) {
      z$group_id <- NA_character_
      z
    }),
    split_rows(grouped, c("event_id", "episode_id", "group_id", "item_id"))
  )
  for (z in sets) {
    add <- function(
      id,
      value,
      detail = "",
      contrast = NA_character_,
      n = sum(is.finite(z$pre) & is.finite(z$post)),
      denominator = nrow(z),
      reason = ""
    ) {
      collector$rows[[length(collector$rows) + 1L]] <- metric(
        "outcomes",
        id,
        value,
        z$event_id[1],
        z$episode_id[1],
        z$group_id[1],
        item_id = z$item_id[1],
        contrast_id = contrast,
        detail = detail,
        n = n,
        denominator = denominator,
        coverage = if (denominator > 0) n / denominator else NA_real_,
        reason = reason
      )
    }
    a <- safe_mean(z$pre, z$weight)
    b <- safe_mean(z$post, z$weight)
    add("mean_pre", a, n = sum(is.finite(z$pre)))
    add("mean_post", b, n = sum(is.finite(z$post)))
    if (z$kind[1] == "attitude") {
      add("opinion_change", b - a)
      add("H", safe_sd(z$pre, z$weight) - safe_sd(z$post, z$weight))
      add("P", -signed_movement(a, b, z$midpoint_scaled[1]))
      add(
        "P_absolute",
        abs(b - z$midpoint_scaled[1]) - abs(a - z$midpoint_scaled[1])
      )
    }
    if (z$kind[1] == "knowledge") {
      scored <- "scoring" %in%
        names(z) &&
        z$scoring[1] %in% c("proportion_correct", "binary_key")
      pre <- z$pre
      post <- z$post
      if (scored && z$scoring[1] == "binary_key") {
        scored <- "correct_value" %in% names(z) && is.finite(z$correct_value[1])
        if (scored) {
          pre <- ifelse(
            is.na(z$pre),
            NA_real_,
            as.numeric(z$pre_raw == z$correct_value)
          )
          post <- ifelse(
            is.na(z$post),
            NA_real_,
            as.numeric(z$post_raw == z$correct_value)
          )
        }
      }
      gain <- post - pre
      if (!scored) {
        gain[] <- NA_real_
      }
      why <- if (scored) {
        ""
      } else {
        "Knowledge requires explicit scoring and, for binary_key, correct_value."
      }
      add("knowledge_gain", safe_mean(gain, z$weight), reason = why)
      add(
        "fraction_learning",
        safe_mean(ifelse(is.na(gain), NA, gain > 0), z$weight),
        reason = why
      )
      base_sd <- safe_sd(pre, z$weight)
      add(
        "knowledge_gain_sd",
        if (scored && is.finite(base_sd) && base_sd > 0) {
          safe_mean(gain, z$weight) / base_sd
        } else {
          NA_real_
        },
        reason = why
      )
    }
    for (ct in contrasts) {
      side <- contrast_side(z, ct)
      focal <- !is.na(side) & side == "focal"
      reference <- !is.na(side) & side == "reference"
      eligible <- focal | reference
      za <- safe_mean(z$pre[eligible], z$weight[eligible])
      zb <- safe_mean(z$post[eligible], z$weight[eligible])
      ref <- safe_mean(z$pre[reference], z$weight[reference])
      both <- any(focal & is.finite(z$pre)) && any(reference & is.finite(z$pre))
      if (z$kind[1] == "attitude" && isTRUE(ct$reference_advantaged)) {
        value <- if (both) signed_movement(za, zb, ref) else NA_real_
        add(
          "D",
          value,
          "Focal + reference population",
          ct$id,
          sum(z$paired & eligible),
          sum(eligible)
        )
        add(
          "D_focal",
          if (both) {
            signed_movement(
              safe_mean(z$pre[focal], z$weight[focal]),
              safe_mean(z$post[focal], z$weight[focal]),
              ref
            )
          } else {
            NA_real_
          },
          "Focal movement toward reference baseline",
          ct$id,
          sum(z$paired & focal),
          sum(focal)
        )
      }
      if (z$kind[1] == "knowledge" && scored) {
        ga <- safe_mean(pre[focal], z$weight[focal]) -
          safe_mean(pre[reference], z$weight[reference])
        gb <- safe_mean(post[focal], z$weight[focal]) -
          safe_mean(post[reference], z$weight[reference])
        add(
          "knowledge_gap_change",
          gb - ga,
          "Focal minus reference gap: post minus pre",
          ct$id,
          sum(z$paired & eligible),
          sum(eligible)
        )
        add(
          "knowledge_gain_focal",
          safe_mean(gain[focal], z$weight[focal]),
          "",
          ct$id,
          sum(z$paired & focal),
          sum(focal)
        )
        add(
          "knowledge_gain_reference",
          safe_mean(gain[reference], z$weight[reference]),
          "",
          ct$id,
          sum(z$paired & reference),
          sum(reference)
        )
      }
    }
  }
  out <- dplyr::bind_rows(collector$rows)
  out$membership <- config$membership
  out$weighting <- if (config$weighted) "supplied" else "equal participant"
  out
}
