union_intervals <- function(start, end) {
  keep <- is.finite(start) & is.finite(end) & end > start
  start <- start[keep]
  end <- end[keep]
  if (!length(start)) {
    return(data.frame(start = numeric(), end = numeric()))
  }
  ord <- order(start, end)
  start <- start[ord]
  end <- end[ord]
  out <- list()
  a <- start[1]
  b <- end[1]
  for (i in seq_along(start)[-1]) {
    if (start[i] <= b) {
      b <- max(b, end[i])
    } else {
      out[[length(out) + 1L]] <- data.frame(start = a, end = b)
      a <- start[i]
      b <- end[i]
    }
  }
  bind_rows(c(out, list(data.frame(start = a, end = b))))
}

interval_length <- function(start, end) {
  d <- union_intervals(start, end)
  sum(d$end - d$start)
}

intersection_length <- function(a, b) {
  if (!nrow(a) || !nrow(b)) {
    return(0)
  }
  total <- 0
  for (i in seq_len(nrow(a))) {
    total <- total +
      sum(pmax(0, pmin(a$end[i], b$end) - pmax(a$start[i], b$start)))
  }
  total
}

participation_metrics <- function(x, contrasts, config) {
  sessions <- table_data(x, "sessions")
  collector <- new.env(parent = emptyenv())
  collector$rows <- list()
  for (i in seq_len(nrow(sessions))) {
    s <- sessions[i, ]
    select <- function(nm) {
      d <- table_data(x, nm)
      d[d$event_id == s$event_id & d$session_id == s$session_id, , drop = FALSE]
    }
    a <- select("attendance")
    a <- a[a$status == "present", , drop = FALSE]
    p <- table_data(x, "people")
    ids <- p$person_id[p$event_id == s$event_id & p$role == "participant"]
    a <- a[a$person_id %in% ids, , drop = FALSE]
    t <- select("turns")
    c <- select("coverage")
    recorded <- union_intervals(c$start, c$end)
    session_length <- s$end - s$start
    cov <- if (is.finite(session_length) && session_length > 0) {
      intersection_length(data.frame(start = s$start, end = s$end), recorded) /
        session_length
    } else {
      NA_real_
    }
    duration_ok <- all(c("start", "end") %in% names(t)) &&
      all(is.finite(t$start) & is.finite(t$end))
    identified <- all(!is.na(t$person_id))
    attendees <- unique(as.character(a$person_id))
    persons <- data.frame(
      event_id = rep(s$event_id, length(attendees)),
      person_id = attendees,
      turns = integer(length(attendees)),
      words = integer(length(attendees)),
      seconds = rep(NA_real_, length(attendees)),
      observed = rep(NA_real_, length(attendees))
    )
    for (j in seq_along(attendees)) {
      tt <- t[!is.na(t$person_id) & t$person_id == attendees[j], , drop = FALSE]
      persons$turns[j] <- nrow(tt)
      words <- strsplit(trimws(ifelse(is.na(tt$text), "", tt$text)), "\\s+")
      persons$words[j] <- sum(vapply(
        words,
        function(z) sum(nzchar(z)),
        integer(1)
      ))
      if (duration_ok) {
        persons$seconds[j] <- interval_length(tt$start, tt$end)
      }
      aa <- a[a$person_id == attendees[j], , drop = FALSE]
      if (all(is.finite(aa$enter) & is.finite(aa$exit))) {
        observed <- union_intervals(aa$enter, aa$exit)
        persons$observed[j] <- intersection_length(observed, recorded)
      }
    }
    add <- function(
      id,
      value,
      person = NA_character_,
      contrast = NA_character_,
      n = nrow(persons),
      denom = nrow(persons),
      units = "fraction",
      detail = "",
      reason = ""
    ) {
      collector$rows[[length(collector$rows) + 1L]] <- metric(
        "participation",
        id,
        value,
        s$event_id,
        s$episode_id,
        s$group_id,
        s$session_id,
        contrast_id = contrast,
        person_id = person,
        detail = detail,
        n = n,
        denominator = denom,
        coverage = cov,
        units = units,
        reason = reason
      )
    }
    add("recording_coverage", cov, units = "fraction of session")
    add(
      "unknown_speaker_turns",
      sum(is.na(t$person_id)),
      denom = nrow(t),
      units = "turns"
    )
    add("participant_turns", sum(persons$turns), units = "turns")
    talk_total <- if (duration_ok) sum(persons$seconds) else NA_real_
    shares <- if (identified && is.finite(talk_total) && talk_total > 0) {
      persons$seconds / talk_total
    } else {
      rep(NA_real_, nrow(persons))
    }
    for (j in seq_len(nrow(persons))) {
      add(
        "person_turns",
        persons$turns[j],
        persons$person_id[j],
        units = "turns"
      )
      add(
        "person_word_share",
        if (sum(persons$words) > 0 && identified && !anyNA(t$text)) {
          persons$words[j] / sum(persons$words)
        } else {
          NA_real_
        },
        persons$person_id[j]
      )
      add(
        "person_seconds",
        persons$seconds[j],
        persons$person_id[j],
        units = "speaker seconds"
      )
      add(
        "person_talk_share",
        shares[j],
        persons$person_id[j],
        denom = talk_total
      )
      add(
        "person_observed_seconds",
        persons$observed[j],
        persons$person_id[j],
        units = "seconds"
      )
    }
    add(
      "max_speaker_share",
      if (any(is.finite(shares))) max(shares) else NA_real_,
      denom = talk_total
    )
    gini <- if (all(is.finite(shares)) && length(shares)) {
      sum(abs(outer(shares, shares, "-"))) / (2 * length(shares))
    } else {
      NA_real_
    }
    add("talk_gini", gini, denom = talk_total)
    complete <- is.finite(cov) && cov >= 1 - 1e-10 && identified
    add(
      "silent_fraction",
      if (complete && length(attendees)) mean(persons$turns == 0) else NA_real_,
      reason = if (!complete) {
        "Silence requires complete recording and identified speakers."
      } else {
        ""
      }
    )
    if (duration_ok) {
      overlap <- 0
      for (j in seq_len(nrow(t))) {
        later <- seq_len(nrow(t)) > j &
          !is.na(t$person_id) &
          !is.na(t$person_id[j]) &
          t$person_id != t$person_id[j]
        overlap <- overlap +
          sum(pmax(
            0,
            pmin(t$end[j], t$end[later]) - pmax(t$start[j], t$start[later])
          ))
      }
      add(
        "overlap_pair_seconds",
        overlap,
        units = "pairwise speaker-overlap seconds",
        detail = "Not a count of interruptions; triple overlap contributes three pairs"
      )
    }
    for (role in c("moderator", "expert")) {
      role_ids <- p$person_id[p$event_id == s$event_id & p$role == role]
      tt <- t[t$person_id %in% role_ids, , drop = FALSE]
      seconds <- if (duration_ok) sum(tt$end - tt$start) else NA_real_
      all_seconds <- if (duration_ok) sum(t$end - t$start) else NA_real_
      add(
        paste0(role, "_talk_share"),
        if (identified && is.finite(all_seconds) && all_seconds > 0) {
          seconds / all_seconds
        } else {
          NA_real_
        },
        denom = all_seconds
      )
    }
    for (ct in contrasts) {
      side <- contrast_side(persons, ct)
      f <- !is.na(side) & side == "focal"
      r <- !is.na(side) & side == "reference"
      reference <- safe_mean(persons$seconds[r])
      ratio <- if (identified && is.finite(reference) && reference > 0) {
        safe_mean(persons$seconds[f]) / reference
      } else {
        NA_real_
      }
      add(
        "voice_ratio",
        ratio,
        contrast = ct$id,
        n = sum(f),
        denom = sum(r),
        detail = "Mean focal speaker seconds / mean reference speaker seconds"
      )
    }
  }
  bind_rows(collector$rows)
}
