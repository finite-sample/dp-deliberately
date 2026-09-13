validate_semantics <- function(x) {
  collector <- new.env(parent = emptyenv())
  collector$issues <- list()
  add <- function(nm, code, message, rows, severity = "error") {
    if (length(rows)) {
      collector$issues[[length(collector$issues) + 1L]] <- data.frame(
        severity = severity,
        table = nm,
        row = rows,
        code = code,
        message = message
      )
    }
  }
  turns <- table_data(x, "turns")
  if (!is.null(turns)) {
    if (xor("start" %in% names(turns), "end" %in% names(turns))) {
      add(
        "turns",
        "TIME_COLUMNS",
        "Supply both start and end columns, or neither.",
        NA_integer_
      )
    }
    if (is.numeric(turns$sequence)) {
      add(
        "turns",
        "TURN_SEQUENCE",
        "Sequence must be a positive integer.",
        which(
          is.na(turns$sequence) |
            turns$sequence < 1 |
            turns$sequence != floor(turns$sequence)
        )
      )
    }
    add(
      "turns",
      "MISSING_TEXT",
      "Missing text prevents complete word-share measurement.",
      which(is.na(turns$text)),
      "warning"
    )
  }
  a <- table_data(x, "annotations")
  if (!is.null(a)) {
    binary <- c(
      "recommendation",
      "evidence_present",
      "counterargument",
      "response_observed",
      "engages_previous",
      "personal_attack",
      "dismissive",
      "conformity_pressure",
      "moderator_correction",
      "interruption_given",
      "interruption_received",
      "floor_attempt",
      "floor_success"
    )
    values <- suppressWarnings(as.numeric(a$value))
    for (code in intersect(
      unique(a$code),
      c(binary, "reason_level", "stance")
    )) {
      allowed <- if (code == "reason_level") {
        0:3
      } else if (code == "stance") {
        -1:1
      } else {
        0:1
      }
      add(
        "annotations",
        "CODE_VALUE",
        paste("Invalid value for", code),
        which(a$code == code & !values %in% allowed)
      )
    }
    for (kind in c("turn", "passage", "argument")) {
      if (!all(c("start_char", "end_char") %in% names(a))) {
        next
      }
      source <- table_data(
        x,
        c(turn = "turns", passage = "materials", argument = "arguments")[[kind]]
      )
      if (is.null(source)) {
        next
      }
      rows <- which(a$target_type == kind)
      target <- data.frame(
        event_id = a$event_id[rows],
        target_id = a$target_id[rows]
      )
      names(target)[2] <- paste0(kind, "_id")
      idx <- match(
        row_key(target, names(target)),
        row_key(source, names(target))
      )
      start <- a$start_char[rows]
      end <- a$end_char[rows]
      if (!is.numeric(start) || !is.numeric(end)) {
        add(
          "annotations",
          "SPAN_TYPE",
          "Character offsets must be numeric.",
          rows
        )
        next
      }
      outside <- start < 1 |
        start != floor(start) |
        end != floor(end) |
        end < start |
        end > nchar(source$text[idx])
      bad <- xor(is.na(start), is.na(end)) | (!is.na(start) & outside)
      add(
        "annotations",
        "SOURCE_SPAN",
        "Use inclusive 1-based character offsets within the source text.",
        rows[which(bad)]
      )
    }
  }
  links <- table_data(x, "links")
  if (!is.null(links) && !is.null(turns)) {
    tt <- links[links$to_type == "turn", , drop = FALSE]
    from <- data.frame(event_id = tt$event_id, turn_id = tt$from_turn)
    to <- data.frame(event_id = tt$event_id, turn_id = tt$to_id)
    fi <- match(row_key(from, names(from)), row_key(turns, names(from)))
    ti <- match(row_key(to, names(to)), row_key(turns, names(to)))
    same_session <- turns$session_id[fi] == turns$session_id[ti]
    bad <- same_session & turns$sequence[fi] <= turns$sequence[ti]
    add(
      "links",
      "RESPONSE_ORDER",
      "A within-session response must refer to an earlier turn.",
      match(tt$link_id[which(bad)], links$link_id)
    )
  }
  args <- table_data(x, "arguments")
  if (!is.null(args)) {
    add(
      "arguments",
      "UNATTRIBUTED_REVIEW",
      "Reviewed arguments require a reviewer.",
      which(args$reviewed & (is.na(args$reviewer) | !nzchar(args$reviewer)))
    )
  }
  sampling <- table_data(x, "sampling")
  if (!is.null(sampling) && "fpc" %in% names(sampling)) {
    add(
      "sampling",
      "FPC",
      "Finite population correction must be positive and finite.",
      which(!is.finite(sampling$fpc) | sampling$fpc <= 0)
    )
  }
  items <- table_data(x, "items")
  if (!is.null(items)) {
    add(
      "items",
      "RESERVED_ID",
      "__knowledge_mean__ is reserved for a derived score.",
      which(items$item_id == "__knowledge_mean__")
    )
    if ("scoring" %in% names(items)) {
      proportion <- items$kind == "knowledge" &
        items$scoring %in% "proportion_correct"
      add(
        "items",
        "KNOWLEDGE_SCALE",
        "proportion_correct needs endpoints 0/1 and direction 1.",
        which(
          proportion &
            (items$lower != 0 | items$upper != 1 | items$direction != 1)
        )
      )
    }
  }
  dplyr::bind_rows(collector$issues)
}
