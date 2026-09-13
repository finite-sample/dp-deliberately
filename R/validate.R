#' Validate table structure, values, and relationships
#' @param x A [deliberation_data()] bundle.
#' @return A data frame of severity, table, row, code, and message. An empty
#'   result means no detected problems, not evidence of substantive validity.
#' @export
validate_data <- function(x) {
  if (!inherits(x, "deliberation_data")) {
    stop("Expected deliberation_data.")
  }
  collector <- new.env(parent = emptyenv())
  collector$issues <- list()
  add <- function(
    table,
    code,
    message,
    rows = NA_integer_,
    severity = "error"
  ) {
    if (!length(rows)) {
      return(invisible(NULL))
    }
    collector$issues[[length(collector$issues) + 1L]] <- data.frame(
      severity = severity,
      table = table,
      row = rows,
      code = code,
      message = message,
      stringsAsFactors = FALSE
    )
  }
  if (!identical(x$schema_version, "1.0")) {
    add("bundle", "SCHEMA_VERSION", "Supported schema version is 1.0.")
  }
  schema <- data_schema()
  valid <- character()
  for (nm in names(x$tables)) {
    d <- x$tables[[nm]]
    if (!nm %in% names(schema)) {
      add(nm, "UNKNOWN_TABLE", "Unrecognized table.")
      next
    }
    missing <- setdiff(schema[[nm]]$fields, names(d))
    if (length(missing)) {
      add(nm, "MISSING_COLUMNS", paste(missing, collapse = ", "))
      next
    }
    valid <- c(valid, nm)
    keys <- schema[[nm]]$key
    bad <- !stats::complete.cases(d[keys]) |
      apply(d[keys], 1, function(z) any(!nzchar(trimws(z))))
    add(
      nm,
      "MISSING_KEY",
      "Primary keys cannot be missing or empty.",
      which(bad)
    )
    add(
      nm,
      "DUPLICATE_KEY",
      "Conflicting or repeated primary key.",
      which(duplicated(row_key(d, keys)))
    )
  }
  fk <- function(child, parent, from, to = from, nullable = FALSE) {
    if (!child %in% valid) {
      return(invisible(NULL))
    }
    d <- x$tables[[child]]
    if (!all(from %in% names(d)) || !nrow(d)) {
      return(invisible(NULL))
    }
    present <- stats::complete.cases(d[from])
    if (!nullable) {
      add(
        child,
        "MISSING_REFERENCE",
        paste(from, collapse = ", "),
        which(!present)
      )
    }
    if (!parent %in% valid) {
      if (any(present)) {
        add(child, "MISSING_PARENT", paste("Requires", parent))
      }
      return(invisible(NULL))
    }
    names_local <- d[from]
    names(names_local) <- to
    bad <- present &
      !row_key(names_local, to) %in% row_key(x$tables[[parent]], to)
    add(
      child,
      "ORPHAN_REFERENCE",
      paste("Unmatched", parent, "reference."),
      which(bad)
    )
  }
  for (nm in setdiff(valid, "events")) {
    fk(nm, "events", "event_id")
  }
  for (nm in c(
    "recruitment",
    "attendance",
    "assignments",
    "moderators",
    "responses",
    "sampling"
  )) {
    fk(nm, "people", c("event_id", "person_id"))
  }
  fk("turns", "people", c("event_id", "person_id"), nullable = TRUE)
  for (nm in c(
    "sessions",
    "assignments",
    "waves",
    "responses",
    "randomization"
  )) {
    fk(nm, "episodes", c("event_id", "episode_id"))
  }
  for (nm in c("attendance", "moderators", "turns", "coverage")) {
    fk(nm, "sessions", c("event_id", "session_id"))
  }
  fk("responses", "items", c("event_id", "item_id"))
  fk("responses", "waves", c("event_id", "episode_id", "wave_id"))
  fk("arguments", "materials", c("event_id", "passage_id"), nullable = TRUE)
  for (col in c("from_argument", "to_argument")) {
    fk(
      "argument_edges",
      "arguments",
      c("event_id", col),
      c("event_id", "argument_id")
    )
  }
  choices <- function(nm, col, allowed, nullable = FALSE) {
    if (!nm %in% valid || !col %in% names(x$tables[[nm]])) {
      return(invisible(NULL))
    }
    z <- x$tables[[nm]][[col]]
    bad <- !z %in% allowed
    if (nullable) {
      bad <- bad & !is.na(z)
    }
    add(
      nm,
      "INVALID_VALUE",
      paste(col, "must be one of", paste(allowed, collapse = ", ")),
      which(bad)
    )
  }
  choices("people", "role", c("participant", "moderator", "expert", "observer"))
  choices("attendance", "status", c("present", "absent", "unknown"))
  choices("recruitment", "attended", c(TRUE, FALSE), TRUE)
  choices("items", "kind", c("attitude", "knowledge", "experience"))
  choices("items", "direction", c(-1, 1))
  choices("waves", "phase", c("pre", "post", "other"))
  choices("annotations", "target_type", c("turn", "passage", "argument"))
  choices("links", "to_type", c("turn", "argument"))
  choices("arguments", "reviewed", c(TRUE, FALSE))
  for (nm in c("annotations", "links")) {
    choices(nm, "source_type", c("human", "model"))
    choices(
      nm,
      "review_status",
      c("unreviewed", "accepted", "rejected", "adjudicated")
    )
    if (nm %in% valid) {
      d <- x$tables[[nm]]
      cols <- c("coder", "codebook_version")
      bad <- !stats::complete.cases(d[cols]) |
        apply(d[cols], 1, function(z) any(!nzchar(trimws(z))))
      add(
        nm,
        "MISSING_PROVENANCE",
        "Coder and codebook version are required.",
        which(bad)
      )
      model <- d$source_type == "model"
      for (col in c("model", "prompt_version")) {
        missing <- if (col %in% names(d)) {
          is.na(d[[col]]) | !nzchar(d[[col]])
        } else {
          rep(TRUE, nrow(d))
        }
        add(
          nm,
          "MODEL_PROVENANCE",
          paste("Model annotations require", col),
          which(model & missing)
        )
      }
    }
  }
  # Validate polymorphic targets using the same composite-key machinery.
  for (nm in intersect(c("annotations", "links"), valid)) {
    d <- x$tables[[nm]]
    type <- if (nm == "annotations") "target_type" else "to_type"
    id <- if (nm == "annotations") "target_id" else "to_id"
    for (kind in intersect(
      unique(d[[type]]),
      c("turn", "argument", "passage")
    )) {
      parent <- c(
        turn = "turns",
        argument = "arguments",
        passage = "materials"
      )[[kind]]
      rows <- which(d[[type]] == kind)
      target <- d[rows, c("event_id", id), drop = FALSE]
      names(target)[2] <- paste0(kind, "_id")
      bad <- if (parent %in% valid) {
        !row_key(target, names(target)) %in%
          row_key(x$tables[[parent]], names(target))
      } else {
        rep(TRUE, length(rows))
      }
      add(nm, "ORPHAN_TARGET", paste("Unmatched", kind), rows[bad])
    }
    if (nm == "links") {
      fk(nm, "turns", c("event_id", "from_turn"), c("event_id", "turn_id"))
    }
  }
  numeric_fields <- list(
    sessions = c("start", "end"),
    attendance = c("enter", "exit"),
    items = c("lower", "upper", "midpoint", "direction"),
    responses = "value",
    benchmarks = c("target", "sd"),
    sampling = "weight",
    turns = c("sequence", "start", "end"),
    coverage = c("start", "end"),
    materials = "available_at"
  )
  numeric_ok <- TRUE
  for (nm in intersect(names(numeric_fields), valid)) {
    d <- x$tables[[nm]]
    for (col in intersect(numeric_fields[[nm]], names(d))) {
      if (!is.numeric(d[[col]]) && !all(is.na(d[[col]]))) {
        add(nm, "TYPE", paste(col, "must be numeric."))
        numeric_ok <- FALSE
      } else {
        add(
          nm,
          "NONFINITE",
          paste(col, "cannot be infinite."),
          which(is.infinite(d[[col]]))
        )
      }
    }
  }
  if (numeric_ok) {
    for (nm in intersect(
      c("sessions", "coverage", "turns", "attendance"),
      valid
    )) {
      d <- x$tables[[nm]]
      cols <- if (nm == "attendance") c("enter", "exit") else c("start", "end")
      if (!all(cols %in% names(d))) {
        next
      }
      a <- d[[cols[1]]]
      b <- d[[cols[2]]]
      bad <- xor(is.na(a), is.na(b)) | (!is.na(a) & (a < 0 | b <= a))
      if (nm == "coverage") {
        bad <- bad | is.na(a) | is.na(b)
      }
      add(
        nm,
        "INTERVAL",
        "Intervals must have two finite endpoints and positive duration, or both missing.",
        which(bad)
      )
      if (nm != "sessions" && "sessions" %in% valid) {
        s <- x$tables$sessions
        idx <- match(
          row_key(d, c("event_id", "session_id")),
          row_key(s, c("event_id", "session_id"))
        )
        add(
          nm,
          "OUTSIDE_SESSION",
          "Interval extends beyond the session.",
          which(a < s$start[idx] | b > s$end[idx])
        )
      }
    }
    if ("items" %in% valid) {
      d <- x$tables$items
      add(
        "items",
        "SCALE",
        "Finite lower < upper is required.",
        which(is.na(d$lower) | is.na(d$upper) | d$lower >= d$upper)
      )
      add(
        "items",
        "MIDPOINT",
        "Midpoint must lie inside the declared scale.",
        which(d$midpoint < d$lower | d$midpoint > d$upper)
      )
      if ("responses" %in% valid) {
        r <- x$tables$responses
        idx <- match(
          row_key(r, c("event_id", "item_id")),
          row_key(d, c("event_id", "item_id"))
        )
        add(
          "responses",
          "OUT_OF_SCALE",
          "Response is outside its declared scale.",
          which(r$value < d$lower[idx] | r$value > d$upper[idx])
        )
      }
    }
    if ("sampling" %in% valid) {
      d <- x$tables$sampling
      add(
        "sampling",
        "WEIGHT",
        "Weights must be finite and strictly positive.",
        which(is.na(d$weight) | d$weight <= 0)
      )
    }
  }
  if ("waves" %in% valid) {
    d <- x$tables$waves
    d <- d[d$phase %in% c("pre", "post"), , drop = FALSE]
    add(
      "waves",
      "AMBIGUOUS_PHASE",
      "Each episode permits at most one pre and one post wave.",
      which(duplicated(row_key(d, c("event_id", "episode_id", "phase"))))
    )
  }
  if ("turns" %in% valid) {
    d <- x$tables$turns
    add(
      "turns",
      "TURN_SEQUENCE",
      "Sequence must be unique within a session.",
      which(duplicated(row_key(d, c("event_id", "session_id", "sequence"))))
    )
    add(
      "turns",
      "UNKNOWN_SPEAKER",
      "Unknown speakers limit participation estimates.",
      which(is.na(d$person_id)),
      "warning"
    )
  }
  out <- bind_rows(collector$issues)
  if (!nrow(out) || !any(out$severity == "error")) {
    out <- bind_rows(list(out, validate_semantics(x)))
  }
  if (!nrow(out)) {
    out <- data.frame(
      severity = character(),
      table = character(),
      row = integer(),
      code = character(),
      message = character()
    )
  }
  out
}
