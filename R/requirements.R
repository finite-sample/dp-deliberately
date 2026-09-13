#' Discover analysis requirements
#' @return A data frame with analysis, layer, required tables, required fields,
#'   unit, denominator, and assumptions. Semicolon-separated entries are AND
#'   requirements; additional checks are made on each event's usable data.
#' @export
analysis_requirements <- function() {
  collector <- new.env(parent = emptyenv())
  collector$rows <- list()
  add <- function(id, layer, tables, fields, unit, denominator, assumptions) {
    collector$rows[[length(collector$rows) + 1L]] <- data.frame(
      analysis = id,
      layer = layer,
      tables = tables,
      fields = fields,
      unit = unit,
      denominator = denominator,
      assumptions = assumptions
    )
  }
  add(
    "sample",
    "design",
    "people;benchmarks",
    "",
    "event/benchmark",
    "nonmissing eligible participants",
    "Comparable target population and measurement; marginal balance only"
  )
  add(
    "attendance",
    "design",
    "people;recruitment",
    "",
    "event",
    "recruits with known attendance",
    "Predictors measured before deliberation; AUC is descriptive"
  )
  add(
    "assignment",
    "design",
    "people;assignments",
    "",
    "episode/attribute",
    "assigned participants with baseline data",
    "Balance does not prove randomization"
  )
  add(
    "participation",
    "participation",
    "people;sessions;attendance;turns;coverage",
    "",
    "session/person/contrast",
    "observed participant attendees",
    "Silence requires complete recording and identified speakers; overlap is not interruption"
  )
  add(
    "exchange",
    "exchange",
    "sessions;turns;annotations",
    "",
    "session/code",
    "coded eligible turns",
    "Conditional on supplied coding; unannotated is unknown"
  )
  add(
    "responsiveness",
    "exchange",
    "sessions;turns;annotations;links",
    "",
    "session",
    "coded counterarguments/recommendations",
    "Links identify substantive responses"
  )
  add(
    "arguments",
    "exchange",
    "sessions;turns;arguments;links",
    "",
    "session/perspective",
    "declared inventory for the session topic",
    "Reviewed inventory supports vetted coverage, not objective truth"
  )
  add(
    "materials",
    "design",
    "sessions;materials;arguments",
    "",
    "session/perspective",
    "declared inventory for the session topic",
    "Material availability precedes the session; review is attributed"
  )
  add(
    "exposure",
    "exchange",
    "sessions;turns;arguments;links;attendance;items;waves;responses",
    "items.perspective_positive;items.perspective_negative;items.topic_id;turns.start;turns.end",
    "session/item",
    "attendees with a directional baseline view",
    "Temporal opportunity to encounter an opposing argument, not evidence of attention"
  )
  add(
    "outcomes",
    "outcomes",
    "people;episodes;items;waves;responses",
    "",
    "episode/item/group",
    "paired respondents by default",
    "Descriptive pre/post change; P requires a substantive midpoint"
  )
  add(
    "group_effects",
    "outcomes",
    "people;assignments;items;waves;responses",
    "",
    "episode/item",
    "paired participants with actual group",
    "Baseline-adjusted group associations; moderator confounding checked"
  )
  add(
    "randomization",
    "outcomes",
    "people;assignments;items;waves;responses;randomization",
    "",
    "episode/item",
    "complete assignment roster",
    "Sharp null of no assignment effects; fixed-size complete or blocked randomization"
  )
  add(
    "survey_inference",
    "outcomes",
    "people;items;waves;responses;sampling",
    "",
    "episode/item/domain",
    "paired response domain of supplied sample",
    "Declared probability sampling; response ignorability is not established"
  )
  do.call(rbind, collector$rows)
}

#' Determine which analyses the supplied tables unlock
#' @param x A [deliberation_data()] bundle.
#' @param contrasts Optional definitions from [define_contrast()].
#' @return One row per event and analysis with readiness and missing inputs.
#'   Readiness describes inputs, not whether every metric is estimable.
#' @export
available_analyses <- function(x, contrasts = list()) {
  req <- analysis_requirements()
  events <- table_data(x, "events")
  if (is.null(events)) {
    return(data.frame())
  }
  rows <- list()
  for (event in events$event_id) {
    for (i in seq_len(nrow(req))) {
      tables <- strsplit(req$tables[i], ";", fixed = TRUE)[[1]]
      missing <- tables[vapply(
        tables,
        function(nm) {
          d <- table_data(x, nm)
          is.null(d) || !"event_id" %in% names(d) || !any(d$event_id == event)
        },
        logical(1)
      )]
      fields <- strsplit(req$fields[i], ";", fixed = TRUE)[[1]]
      for (field in fields[nzchar(fields)]) {
        parts <- strsplit(field, ".", fixed = TRUE)[[1]]
        if (!parts[2] %in% names(table_data(x, parts[1]))) {
          missing <- c(missing, field)
        }
      }
      rows[[length(rows) + 1L]] <- data.frame(
        event_id = as.character(event),
        analysis = req$analysis[i],
        layer = req$layer[i],
        readiness = if (length(missing)) "unavailable" else "ready",
        reason = paste(unique(missing), collapse = "; "),
        contrasts = length(contrasts)
      )
    }
  }
  bind_rows(rows)
}

metric <- function(
  analysis,
  id,
  estimate,
  event_id,
  episode_id = NA_character_,
  group_id = NA_character_,
  session_id = NA_character_,
  item_id = NA_character_,
  contrast_id = NA_character_,
  person_id = NA_character_,
  detail = "",
  n = NA_integer_,
  denominator = NA_real_,
  coverage = NA_real_,
  units = "fraction",
  reason = "",
  method = "finite-event descriptive",
  se = NA_real_,
  lower = NA_real_,
  upper = NA_real_,
  p_value = NA_real_
) {
  data.frame(
    analysis = analysis,
    metric = id,
    event_id = as.character(event_id),
    episode_id = as.character(episode_id),
    group_id = as.character(group_id),
    session_id = as.character(session_id),
    item_id = as.character(item_id),
    contrast_id = as.character(contrast_id),
    person_id = as.character(person_id),
    detail = detail,
    estimate = estimate,
    n = n,
    denominator = denominator,
    coverage = coverage,
    units = units,
    status = ifelse(is.finite(estimate), "computed", "unassessable"),
    reason = ifelse(
      !is.finite(estimate) & !nzchar(reason),
      "Insufficient usable observations or undefined denominator.",
      reason
    ),
    method = method,
    se = se,
    lower = lower,
    upper = upper,
    p_value = p_value,
    stringsAsFactors = FALSE
  )
}
