exposure_metrics <- function(x, contrasts, config) {
  d <- paired_responses(x, "available")
  a <- table_data(x, "attendance")
  args <- inventory_arguments(table_data(x, "arguments"))
  links <- accepted_rows(table_data(x, "links"), config)
  sessions <- table_data(x, "sessions")
  turns <- table_data(x, "turns")
  rows <- list()
  for (i in seq_len(nrow(sessions))) {
    s <- sessions[i, ]
    if (is.na(s$topic_id)) next
    aa <- a[
      a$event_id == s$event_id &
        a$session_id == s$session_id &
        a$status == "present",
      ,
      drop = FALSE
    ]
    tt <- turns[
      turns$event_id == s$event_id & turns$session_id == s$session_id,
      ,
      drop = FALSE
    ]
    ll <- links
    if (nrow(ll)) {
      ll <- ll[
        ll$event_id == s$event_id &
          ll$from_turn %in% tt$turn_id &
          ll$to_type == "argument" &
          ll$relation %in% c("expresses", "supports", "rebuts"),
        ,
        drop = FALSE
      ]
    }
    for (z in split_rows(
      d[
        d$event_id == s$event_id &
          d$episode_id == s$episode_id &
          d$kind == "attitude" & !is.na(d$topic_id) &
          d$topic_id == s$topic_id,
        ,
        drop = FALSE
      ],
      "item_id"
    )) {
      z <- z[
        z$person_id %in%
          aa$person_id &
          is.finite(z$pre) &
          is.finite(z$midpoint_scaled) &
          abs(z$pre - z$midpoint_scaled) >= 1e-12,
        ,
        drop = FALSE
      ]
      exposed <- rep(NA_real_, nrow(z))
      for (j in seq_len(nrow(z))) {
        opposition <- if (z$pre[j] > z$midpoint_scaled[j]) {
          z$perspective_negative[j]
        } else {
          z$perspective_positive[j]
        }
        ar <- args[
          args$event_id == s$event_id &
            args$topic_id == s$topic_id &
            args$perspective == opposition &
            args$reviewed,
          ,
          drop = FALSE
        ]
        att <- aa[aa$person_id == z$person_id[j], , drop = FALSE]
        if (!nrow(ar) || !all(is.finite(att$enter) & is.finite(att$exit))) {
          next
        }
        candidate <- if (nrow(ll)) {
          tt[
            tt$turn_id %in% ll$from_turn[ll$to_id %in% ar$argument_id],
            ,
            drop = FALSE
          ]
        } else {
          tt[FALSE, ]
        }
        opportunity <- intersection_length(
          union_intervals(att$enter, att$exit),
          union_intervals(candidate$start, candidate$end)
        ) >
          0
        complete <- "argument_coding_complete" %in%
          names(tt) &&
          nrow(tt) > 0 &&
          all(!is.na(tt$argument_coding_complete) & tt$argument_coding_complete)
        complete <- complete &&
          session_recorded(x, s) &&
          all(is.finite(tt$start) & is.finite(tt$end))
        if (opportunity) {
          exposed[j] <- 1
        } else if (complete) {
          exposed[j] <- 0
        }
      }
      rows[[length(rows) + 1L]] <- metric(
        "exposure",
        "opposing_argument_opportunity",
        safe_mean(exposed),
        s$event_id,
        s$episode_id,
        s$group_id,
        s$session_id,
        item_id = if (nrow(z)) z$item_id[1] else NA_character_,
        n = sum(is.finite(exposed)),
        denominator = nrow(z),
        coverage = if (nrow(z)) mean(is.finite(exposed)) else NA_real_,
        method = "attendance overlap with vetted argument; opportunity, not demonstrated attention"
      )
    }
  }
  bind_rows(rows)
}
