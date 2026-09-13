#' Input table specification
#'
#' Required columns and primary keys for supplied tables. Optional columns and
#' analysis-specific requirements are described in the schema vignette.
#' @return A named list of table specifications.
#' @examples
#' data_schema()$turns
#' @export
data_schema <- function() {
  spec <- function(key, fields) {
    list(
      key = strsplit(key, " ")[[1]],
      fields = strsplit(fields, " ")[[1]]
    )
  }
  list(
    events = spec("event_id", "event_id label"),
    episodes = spec("event_id episode_id", "event_id episode_id label"),
    sessions = spec(
      "event_id session_id",
      paste(
        "event_id session_id episode_id group_id type topic_id start end"
      )
    ),
    people = spec("event_id person_id", "event_id person_id role"),
    recruitment = spec(
      "event_id person_id",
      "event_id person_id stage attended"
    ),
    attendance = spec(
      "event_id session_id person_id interval_id",
      paste(
        "event_id session_id person_id interval_id status enter exit"
      )
    ),
    assignments = spec(
      "event_id episode_id person_id",
      paste(
        "event_id episode_id person_id intended_group actual_group block"
      )
    ),
    moderators = spec(
      "event_id session_id person_id",
      "event_id session_id person_id"
    ),
    items = spec(
      "event_id item_id",
      paste(
        "event_id item_id label kind lower upper direction midpoint"
      )
    ),
    waves = spec(
      "event_id episode_id wave_id",
      paste(
        "event_id episode_id wave_id phase timing"
      )
    ),
    responses = spec(
      "event_id episode_id person_id item_id wave_id",
      paste(
        "event_id episode_id person_id item_id wave_id value"
      )
    ),
    benchmarks = spec(
      "event_id benchmark_id",
      paste(
        "event_id benchmark_id variable level target sd population"
      )
    ),
    sampling = spec(
      "event_id person_id",
      "event_id person_id weight psu stratum"
    ),
    randomization = spec(
      "event_id episode_id",
      "event_id episode_id mechanism"
    ),
    turns = spec(
      "event_id turn_id",
      paste(
        "event_id turn_id session_id person_id sequence text"
      )
    ),
    coverage = spec(
      "event_id session_id coverage_id",
      paste(
        "event_id session_id coverage_id start end"
      )
    ),
    materials = spec(
      "event_id passage_id",
      paste(
        "event_id passage_id source_id version text available_at"
      )
    ),
    arguments = spec(
      "event_id argument_id",
      paste(
        "event_id argument_id topic_id perspective text reviewed reviewer passage_id"
      )
    ),
    argument_edges = spec(
      "event_id edge_id",
      paste(
        "event_id edge_id from_argument to_argument relation"
      )
    ),
    annotations = spec(
      "event_id annotation_id",
      paste(
        "event_id annotation_id target_type target_id code value coder source_type",
        "codebook_version review_status"
      )
    ),
    links = spec(
      "event_id link_id",
      paste(
        "event_id link_id from_turn to_type to_id relation coder",
        "source_type codebook_version review_status"
      )
    )
  )
}

#' Construct a deliberation data bundle
#'
#' Each named argument is a data frame described by [data_schema()]. Person IDs
#' are scoped to an event. Extra columns (including attributes on people) are
#' preserved. No tables or values are imputed.
#' @param ... Named data frames, or one named list of data frames.
#' @param schema_version Input schema version; currently `"1.0"`.
#' @param provenance Additional source and transformation metadata.
#' @return A `deliberation_data` object. Use [validate_data()] before analysis.
#' @examples
#' deliberation_data(events = data.frame(event_id = "event1", label = "Example"))
#' @export
deliberation_data <- function(
  ...,
  schema_version = "1.0",
  provenance = list()
) {
  tables <- list(...)
  if (
    length(tables) == 1L && is.list(tables[[1]]) && !is.data.frame(tables[[1]])
  ) {
    tables <- tables[[1]]
  }
  if (
    is.null(names(tables)) ||
      any(!nzchar(names(tables))) ||
      anyDuplicated(names(tables))
  ) {
    stop("Supply uniquely named tables.")
  }
  if (!all(vapply(tables, is.data.frame, logical(1)))) {
    stop("Every table must be a data frame.")
  }
  structure(
    list(
      tables = tables,
      schema_version = schema_version,
      provenance = provenance
    ),
    class = "deliberation_data"
  )
}

#' Read named CSV input tables
#' @param path Directory containing CSV files named for [data_schema()] tables.
#' @return A `deliberation_data` object with file hashes.
#' @examples
#' path <- tempfile()
#' dir.create(path)
#' write.csv(data.frame(event_id = "001", label = "Example"),
#'   file.path(path, "events.csv"), row.names = FALSE)
#' x <- read_deliberation(path)
#' unlink(path, recursive = TRUE)
#' @export
read_deliberation <- function(path) {
  files <- file.path(path, paste0(names(data_schema()), ".csv"))
  files <- files[file.exists(files)]
  if (!length(files)) {
    stop("No recognized CSV tables found.")
  }
  tables <- lapply(files, function(file) {
    header <- names(readr::read_csv(
      file,
      n_max = 0,
      show_col_types = FALSE,
      name_repair = "minimal"
    ))
    ids <- header[grepl(
      "(_id$|^intended_group$|^actual_group$|^block$|^psu$|^stratum$)",
      header
    )]
    spec <- readr::cols(.default = readr::col_guess())
    spec$cols[ids] <- lapply(ids, function(id) readr::col_character())
    d <- readr::read_csv(
      file,
      col_types = spec,
      name_repair = "minimal",
      na = "NA",
      trim_ws = FALSE,
      show_col_types = FALSE,
      progress = FALSE
    )
    if (nrow(readr::problems(d))) {
      cli::cli_abort("Parsing failed for {.file {file}}.")
    }
    as.data.frame(d)
  })
  names(tables) <- sub("\\.csv$", "", basename(files))
  deliberation_data(
    tables,
    provenance = list(
      files = data.frame(
        file = basename(files),
        sha256 = vapply(files, digest::digest, "", algo = "sha256", file = TRUE)
      )
    )
  )
}

table_data <- function(x, name) x$tables[[name]]

row_key <- function(d, columns) {
  if (!nrow(d)) {
    return(character())
  }
  # Length prefixes avoid collisions when identifiers contain separators.
  values <- lapply(d[columns], function(v) {
    v <- as.character(v)
    ifelse(is.na(v), "N;", paste0(nchar(v), ":", v))
  })
  do.call(paste0, values)
}

split_rows <- function(d, keys) {
  if (is.null(d) || !nrow(d)) {
    return(list())
  }
  split(d, row_key(d, keys))
}

safe_mean <- function(x, w = rep(1, length(x))) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[keep], w[keep])
}

safe_sd <- function(x, w = rep(1, length(x))) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  if (length(x) < 2L) {
    return(NA_real_)
  }
  denom <- sum(w) - sum(w^2) / sum(w)
  sqrt(sum(w * (x - safe_mean(x, w))^2) / denom)
}

with_seed <- function(seed, expr) {
  withr::with_seed(seed, expr)
}
