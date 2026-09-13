accepted_rows <- function(d, config) {
  if (is.null(d)) {
    return(data.frame())
  }
  d[d$review_status %in% config$annotation_status, , drop = FALSE]
}

resolved_codes <- function(x, config) {
  a <- accepted_rows(table_data(x, "annotations"), config)
  if (!nrow(a)) {
    return(a)
  }
  for (z in split_rows(a, c("event_id", "code"))) {
    if (length(unique(z$codebook_version)) > 1L) {
      stop("Select one codebook version per event and code before auditing.")
    }
  }
  rows <- lapply(
    split_rows(a, c("event_id", "target_type", "target_id", "code")),
    function(z) {
      if (any(z$review_status == "adjudicated")) {
        z <- z[z$review_status == "adjudicated", , drop = FALSE]
      }
      z$value <- as.character(z$value)
      if (length(unique(z$value)) != 1L) {
        return(NULL)
      }
      z[1, , drop = FALSE]
    }
  )
  dplyr::bind_rows(rows)
}

code_values <- function(a, ids, code) {
  z <- a[a$code == code, , drop = FALSE]
  suppressWarnings(as.numeric(z$value[match(ids, z$target_id)]))
}

exchange_metrics <- function(x, contrasts, config) {
  sessions <- table_data(x, "sessions")
  annotations <- resolved_codes(x, config)
  raw <- table_data(x, "annotations")
  links <- accepted_rows(table_data(x, "links"), config)
  collector <- new.env(parent = emptyenv())
  collector$rows <- list()
  for (i in seq_len(nrow(sessions))) {
    s <- sessions[i, ]
    t <- table_data(x, "turns")
    t <- t[
      t$event_id == s$event_id & t$session_id == s$session_id,
      ,
      drop = FALSE
    ]
    a <- annotations
    if (nrow(a)) {
      a <- a[
        a$event_id == s$event_id &
          a$target_type == "turn" &
          a$target_id %in% t$turn_id,
        ,
        drop = FALSE
      ]
    }
    if (!nrow(a)) {
      a <- data.frame(
        code = character(),
        value = character(),
        target_id = character()
      )
    }
    l <- links
    if (nrow(l)) {
      l <- l[
        l$event_id == s$event_id & l$from_turn %in% t$turn_id,
        ,
        drop = FALSE
      ]
    }
    add <- function(
      id,
      value,
      n,
      denom,
      detail = "",
      contrast = NA_character_,
      analysis = "exchange"
    ) {
      collector$rows[[length(collector$rows) + 1L]] <- metric(
        analysis,
        id,
        value,
        s$event_id,
        s$episode_id,
        s$group_id,
        s$session_id,
        contrast_id = contrast,
        detail = detail,
        n = n,
        denominator = denom,
        coverage = if (denom > 0) n / denom else NA_real_,
        method = "descriptive; conditional on supplied annotations"
      )
    }
    ids <- t$turn_id
    recommendation <- code_values(a, ids, "recommendation")
    reasons <- code_values(a, ids, "reason_level")
    eligible <- recommendation == 1 & !is.na(recommendation)
    known <- eligible & is.finite(reasons)
    add(
      "reason_giving",
      safe_mean(as.numeric(reasons[known] >= 1)),
      sum(known),
      sum(eligible),
      "Reason levels: 0 claim; 1 reason; 2 evidence/example; 3 engagement with another reason"
    )
    evidence <- code_values(a, ids, "evidence_present")
    fallback <- is.na(evidence) & reasons %in% 0:2
    evidence[fallback] <- as.numeric(reasons[fallback] == 2)
    add(
      "evidence_giving",
      safe_mean(evidence[eligible]),
      sum(eligible & is.finite(evidence)),
      sum(eligible)
    )
    for (code in c(
      "personal_attack",
      "dismissive",
      "conformity_pressure",
      "moderator_correction",
      "interruption_given",
      "interruption_received",
      "floor_attempt",
      "floor_success"
    )) {
      v <- code_values(a, ids, code)
      add(paste0(code, "_rate"), safe_mean(v), sum(is.finite(v)), nrow(t))
    }
    attempt <- code_values(a, ids, "floor_attempt")
    success <- code_values(a, ids, "floor_success")
    eligible_attempt <- !is.na(attempt) & attempt == 1
    add(
      "floor_success_probability",
      safe_mean(success[eligible_attempt]),
      sum(eligible_attempt & is.finite(success)),
      sum(eligible_attempt)
    )
    counter <- code_values(a, ids, "counterargument")
    candidates <- ids[!is.na(counter) & counter == 1]
    response_coded <- code_values(a, candidates, "response_observed")
    # Absence of a link is not evidence of an unanswered counterargument.
    responded <- if (nrow(l)) {
      unique(l$to_id[
        l$to_type == "turn" &
          l$relation == "substantive_response"
      ])
    } else {
      character()
    }
    response_coded[candidates %in% responded] <- 1
    add(
      "counterargument_response_rate",
      safe_mean(response_coded),
      sum(is.finite(response_coded)),
      length(candidates),
      analysis = "responsiveness"
    )
    direct <- if (nrow(l)) {
      unique(l$from_turn[
        l$to_type == "turn" &
          l$relation %in% c("substantive_response", "acknowledges", "rebuts")
      ])
    } else {
      character()
    }
    engaged <- code_values(a, ids[eligible], "engages_previous")
    engaged[ids[eligible] %in% direct] <- 1
    add(
      "recommendation_engagement",
      safe_mean(engaged),
      sum(is.finite(engaged)),
      sum(eligible),
      analysis = "responsiveness"
    )
    if (nrow(l)) {
      edges <- l[l$to_type == "turn", , drop = FALSE]
      from <- t$person_id[match(edges$from_turn, t$turn_id)]
      to <- t$person_id[match(edges$to_id, t$turn_id)]
      add(
        "cross_person_links",
        sum(!is.na(from) & !is.na(to) & from != to),
        nrow(edges),
        nrow(edges)
      )
    }
    if (!is.null(raw)) {
      aa <- raw[
        raw$event_id == s$event_id &
          raw$target_type == "turn" &
          raw$target_id %in% ids,
        ,
        drop = FALSE
      ]
      add(
        "reviewed_annotation_fraction",
        if (nrow(aa)) {
          mean(aa$review_status %in% c("accepted", "adjudicated"))
        } else {
          NA_real_
        },
        nrow(aa),
        nrow(aa)
      )
      repeats <- Filter(
        function(z) length(unique(z$coder)) > 1,
        split_rows(aa, c("target_id", "code", "codebook_version"))
      )
      agreement <- vapply(
        repeats,
        function(z) length(unique(z$value)) == 1L,
        logical(1)
      )
      add(
        "coder_exact_agreement",
        safe_mean(as.numeric(agreement)),
        length(repeats),
        length(repeats),
        "Exact agreement among multiply coded targets; not chance corrected"
      )
    }
    p <- table_data(x, "people")
    for (role in c("moderator", "expert")) {
      role_ids <- p$person_id[p$event_id == s$event_id & p$role == role]
      selected <- t$person_id %in% role_ids
      stance <- code_values(a, ids, "stance")
      add(
        paste0(role, "_stance_mean"),
        safe_mean(stance[selected]),
        sum(selected & is.finite(stance)),
        sum(selected),
        "Signed orientation defined by the supplied topic codebook"
      )
    }
    for (ct in contrasts) {
      side <- contrast_side(t, ct)
      v <- code_values(a, ids, "interruption_received")
      f <- !is.na(side) & side == "focal"
      r <- !is.na(side) & side == "reference"
      ref <- safe_mean(v[r])
      add(
        "interruption_received_ratio",
        if (is.finite(ref) && ref > 0) safe_mean(v[f]) / ref else NA_real_,
        sum(is.finite(v[f | r])),
        sum(f | r),
        "Per coded turn focal/reference ratio; coverage among eligible contrast turns",
        ct$id
      )
    }
  }
  dplyr::bind_rows(collector$rows)
}

argument_metrics <- function(x, contrasts, config) {
  sessions <- table_data(x, "sessions")
  arguments <- inventory_arguments(table_data(x, "arguments"))
  materials <- table_data(x, "materials")
  links <- accepted_rows(table_data(x, "links"), config)
  collector <- new.env(parent = emptyenv())
  collector$rows <- list()
  for (i in seq_len(nrow(sessions))) {
    s <- sessions[i, ]
    args <- arguments[
      arguments$event_id == s$event_id &
        !is.na(arguments$topic_id) &
        arguments$topic_id %in% s$topic_id,
      ,
      drop = FALSE
    ]
    t <- table_data(x, "turns")
    if (!is.null(t)) {
      t <- t[
        t$event_id == s$event_id & t$session_id == s$session_id,
        ,
        drop = FALSE
      ]
    }
    l <- links
    if (nrow(l)) {
      l <- l[
        l$event_id == s$event_id &
          l$from_turn %in% t$turn_id &
          l$to_type == "argument" &
          l$relation %in% c("expresses", "rebuts", "supports"),
        ,
        drop = FALSE
      ]
    }
    seen <- if (nrow(l)) l$to_id else character()
    complete <- "argument_coding_complete" %in%
      names(t) &&
      nrow(t) > 0 &&
      all(!is.na(t$argument_coding_complete) & t$argument_coding_complete)
    complete <- complete && session_recorded(x, s)
    for (perspective in unique(args$perspective)) {
      aa <- args[args$perspective == perspective, , drop = FALSE]
      add <- function(
        analysis,
        id,
        value,
        n,
        denom,
        coverage,
        detail = perspective
      ) {
        collector$rows[[length(collector$rows) + 1L]] <- metric(
          analysis,
          id,
          value,
          s$event_id,
          s$episode_id,
          s$group_id,
          s$session_id,
          detail = detail,
          n = n,
          denominator = denom,
          coverage = coverage,
          method = "descriptive; conditional on supplied inventory and coding"
        )
      }
      if (!is.null(t)) {
        add(
          "arguments",
          "argument_coverage_lower_bound",
          mean(aa$argument_id %in% seen),
          sum(aa$argument_id %in% seen),
          nrow(aa),
          as.numeric(complete)
        )
        add(
          "arguments",
          "argument_coverage",
          if (complete) mean(aa$argument_id %in% seen) else NA_real_,
          sum(aa$argument_id %in% seen),
          nrow(aa),
          as.numeric(complete)
        )
        reviewed <- !is.na(aa$reviewed) & aa$reviewed
        add(
          "arguments",
          "vetted_argument_coverage",
          if (complete && any(reviewed)) {
            mean(aa$argument_id[reviewed] %in% seen)
          } else {
            NA_real_
          },
          sum(aa$argument_id[reviewed] %in% seen),
          sum(reviewed),
          as.numeric(complete)
        )
      }
      if (!is.null(materials)) {
        m <- materials[materials$event_id == s$event_id, , drop = FALSE]
        idx <- match(aa$passage_id, m$passage_id)
        present <- !is.na(idx) &
          is.finite(m$available_at[idx]) &
          m$available_at[idx] <= s$start
        add(
          "materials",
          "briefing_argument_coverage",
          mean(present),
          sum(present),
          nrow(aa),
          1
        )
      }
    }
    if (!is.null(t) && nrow(l)) {
      p <- table_data(x, "people")
      for (role in c("expert", "moderator")) {
        ids <- p$person_id[p$event_id == s$event_id & p$role == role]
        for (perspective in unique(args$perspective)) {
          matched <- unique(l$from_turn[
            l$to_id %in% args$argument_id[args$perspective == perspective]
          ])
          tt <- t[t$person_id %in% ids, , drop = FALSE]
          collector$rows[[length(collector$rows) + 1L]] <- metric(
            "arguments",
            paste0(role, "_argument_turns"),
            sum(tt$turn_id %in% matched),
            s$event_id,
            s$episode_id,
            s$group_id,
            s$session_id,
            detail = perspective,
            n = nrow(tt),
            denominator = nrow(tt),
            units = "coded turns",
            coverage = as.numeric(complete)
          )
        }
      }
    }
  }
  dplyr::bind_rows(collector$rows)
}
