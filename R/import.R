#' Import historical distortions survey data from dp-data
#' @param path Root of a dp-data checkout or release archive. Defaults to
#'   `DP_DATA_ROOT`, or `../dp-data`.
#' @param source_manifest Optional data frame with `file` and `sha256` columns
#'   pinning both benchmark files. Defaults to the bundled source manifest.
#'   Supply reviewed checksums explicitly when adopting a different snapshot.
#' @return A validated survey-only bundle, with source hashes and an import ledger.
#'   Source scales are already normalized to `[0,1]`; midpoint 0.5 follows the
#'   replication's explicit convention. Source wave column names are retained.
#'   Exact duplicates ignoring X are removed and logged. Missing source person
#'   identifiers receive marked row IDs; conflicting nonmissing IDs are errors.
#' @examples
#' if (dir.exists("../dp-data/evidence")) read_distortions("../dp-data")
#' @export
read_distortions <- function(
  path = Sys.getenv("DP_DATA_ROOT", unset = "../dp-data"),
  source_manifest = NULL
) {
  names <- c("polardata.tab", "attitude-indices.tab")
  files <- file.path(path, "evidence", "benchmarks", names)
  if (is.null(source_manifest)) {
    source_manifest <- utils::read.csv(system.file(
      "examples", "distortions-source.csv", package = "deliberately"
    ), stringsAsFactors = FALSE)
  }
  valid_manifest <- all(c("file", "sha256") %in% colnames(source_manifest)) &&
    nrow(source_manifest) == 2L && !anyDuplicated(source_manifest$file) &&
    setequal(source_manifest$file, names)
  if (!valid_manifest) {
    stop("Expected a unique checksum entry for each historical benchmark.")
  }
  if (!all(file.exists(files))) {
    stop("Missing dp-data historical benchmarks under evidence/benchmarks/.")
  }
  observed <- vapply(files, digest::digest, "", algo = "sha256", file = TRUE)
  expected <- source_manifest$sha256[match(names, source_manifest$file)]
  if (anyNA(expected) || !identical(unname(observed), unname(expected))) {
    stop("Historical source checksum mismatch; review changes before updating pins.")
  }
  raw <- utils::read.delim(
    files[1],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  dictionary <- utils::read.delim(
    files[2],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  needed <- c("pollid", "caseid", "pollgroup")
  if (
    !all(needed %in% names(raw)) ||
      !all(
        c("poll_id", "poll_name", "att_index", "t1var", "t2_t3var") %in%
          names(dictionary)
      )
  ) {
    stop("Unsupported distortions input columns.")
  }
  duplicate <- duplicated(raw[setdiff(names(raw), "X")])
  source_row <- which(!duplicate)
  d <- raw[!duplicate, , drop = FALSE]
  d$event_id <- as.character(d$pollid)
  unresolved <- is.na(d$caseid)
  d$person_id <- ifelse(
    unresolved,
    paste0("unresolved_row_", source_row),
    as.character(d$caseid)
  )
  if (anyDuplicated(row_key(d, c("event_id", "person_id")))) {
    stop("Conflicting source person IDs; resolve before import.")
  }
  if (!all(c(dictionary$t1var, dictionary$t2_t3var) %in% names(d))) {
    stop("Dictionary references missing response columns.")
  }
  events <- unique(data.frame(
    event_id = as.character(dictionary$poll_id),
    label = dictionary$poll_name
  ))
  episodes <- data.frame(
    event_id = events$event_id,
    episode_id = "deliberation",
    label = "Source deliberative episode"
  )
  attribute_cols <- intersect(
    c(
      "female",
      "educ4",
      "educ3",
      "bettered",
      "hhincome",
      "highinc",
      "minority",
      "ppage"
    ),
    names(d)
  )
  people <- data.frame(
    d[c("event_id", "person_id")],
    role = "participant",
    identity_verified = !unresolved,
    d[attribute_cols],
    check.names = FALSE
  )
  assignments <- data.frame(
    d[c("event_id", "person_id")],
    episode_id = "deliberation",
    intended_group = NA_character_,
    actual_group = as.character(d$pollgroup),
    block = NA_character_
  )
  items <- data.frame(
    event_id = as.character(dictionary$poll_id),
    item_id = dictionary$t1var,
    label = dictionary$att_index,
    kind = "attitude",
    lower = 0,
    upper = 1,
    direction = 1,
    midpoint = 0.5,
    source_pre = dictionary$t1var,
    source_post = dictionary$t2_t3var
  )
  waves <- dplyr::bind_rows(lapply(events$event_id, function(event) {
    data.frame(
      event_id = event,
      episode_id = "deliberation",
      wave_id = c("source_pre", "source_post"),
      phase = c("pre", "post"),
      timing = c(
        "Source pre-deliberation measurement",
        "Source designated post-deliberation measurement; original column preserved on items"
      )
    )
  }))
  responses <- list()
  for (i in seq_len(nrow(items))) {
    z <- d[d$event_id == items$event_id[i], , drop = FALSE]
    for (phase in c("pre", "post")) {
      col <- items[[paste0("source_", phase)]][i]
      responses[[length(responses) + 1L]] <- data.frame(
        z[c("event_id", "person_id")],
        episode_id = "deliberation",
        item_id = items$item_id[i],
        wave_id = paste0("source_", phase),
        value = z[[col]],
        source_column = col
      )
    }
  }
  if (all(c("t1know", "t2know") %in% names(d))) {
    knowledge <- data.frame(
      event_id = events$event_id,
      item_id = "knowledge_aggregate",
      label = "Source aggregate knowledge proportion",
      kind = "knowledge",
      lower = 0,
      upper = 1,
      direction = 1,
      midpoint = NA_real_,
      scoring = "proportion_correct"
    )
    items <- dplyr::bind_rows(list(items, knowledge))
    for (phase in c("pre", "post")) {
      col <- if (phase == "pre") "t1know" else "t2know"
      responses[[length(responses) + 1L]] <- data.frame(
        d[c("event_id", "person_id")],
        episode_id = "deliberation",
        item_id = "knowledge_aggregate",
        wave_id = paste0("source_", phase),
        value = d[[col]],
        source_column = col
      )
    }
  }
  responses <- dplyr::bind_rows(responses)
  responses$source_value <- responses$value
  roundoff <- is.finite(responses$value) &
    ((responses$value < 0 & responses$value >= -1e-10) |
       (responses$value > 1 & responses$value <= 1 + 1e-10))
  responses$value[roundoff] <- pmin(1, pmax(0, responses$value[roundoff]))
  ledger <- data.frame(
    action = c(
      "read",
      "exact_duplicates_removed",
      "unresolved_ids",
      "retained",
      "endpoint_roundoff"
    ),
    n = c(nrow(raw), sum(duplicate), sum(unresolved), nrow(d), sum(roundoff)),
    detail = c(
      "Source participant rows",
      "Equality on all columns except X",
      "Row IDs do not establish person identity",
      "No derived source group statistics imported",
      "Clamp excursions within 1e-10 of [0,1]; original values preserved in source_value"
    )
  )
  x <- deliberation_data(
    events = events,
    episodes = episodes,
    people = people,
    assignments = assignments,
    items = items,
    waves = waves,
    responses = responses,
    provenance = list(
      adapter = "distortions-dp-data-1",
      ledger = ledger,
      duplicate_source_rows = which(duplicate),
      unresolved_source_rows = source_row[unresolved],
      files = data.frame(
        file = basename(files),
        sha256 = vapply(files, digest::digest, "", algo = "sha256", file = TRUE)
      ),
      conventions = paste(
        "Normalized source scales; midpoint 0.5; source t2/t3 selection from dictionary;",
        "no assumed randomization."
      )
    )
  )
  errors <- validate_data(x)
  if (any(errors$severity == "error")) {
    stop(paste(
      unique(errors$message[errors$severity == "error"]),
      collapse = "; "
    ))
  }
  x
}
