#' @importFrom rlang .data
NULL

report_metrics <- function(result) {
  m <- tibble::as_tibble(result$metrics)
  m <- m[is.na(m$person_id), , drop = FALSE]
  for (kind in c("events", "items")) {
    dictionary <- result$dictionary[[kind]]
    if (is.null(dictionary)) {
      next
    }
    keys <- if (kind == "events") "event_id" else c("event_id", "item_id")
    label <- if (kind == "events") "event" else "item"
    dictionary <- dictionary[c(keys, "label")]
    names(dictionary)[names(dictionary) == "label"] <- label
    m <- dplyr::left_join(
      m,
      dictionary,
      by = keys,
      relationship = "many-to-one"
    )
  }
  if (!"event" %in% names(m)) {
    m$event <- m$event_id
  }
  if (!"item" %in% names(m)) {
    m$item <- m$item_id
  }
  m
}

report_summary <- function(result) {
  report_metrics(result) |>
    dplyr::filter(
      is.na(.data$group_id),
      is.na(.data$session_id),
      .data$metric %in%
        c("opinion_change", "knowledge_gain", "H", "P", "D", "post_attrition"),
      .data$detail %in% c("", "all", "Focal + reference population"),
      .data$status == "computed"
    ) |>
    dplyr::select(dplyr::any_of(c(
      "event",
      "item",
      "metric",
      "contrast_id",
      "estimate",
      "units",
      "n",
      "denominator",
      "coverage"
    )))
}

report_findings <- function(result) {
  m <- report_metrics(result)
  shifts <- m[
    m$metric == "opinion_change" & is.na(m$group_id) & m$status == "computed",
  ]
  knowledge <- m[
    m$metric == "knowledge_gain" & is.na(m$group_id) & m$status == "computed",
  ]
  text <- character()
  if (nrow(shifts)) {
    text <- c(
      text,
      sprintf(
        paste(
          "%s event\u2013item opinion changes are estimable: %s positive, %s",
          "negative and %s zero. Signs follow each item's declared",
          "orientation; none is a quality verdict."
        ),
        nrow(shifts),
        sum(shifts$estimate > 0),
        sum(shifts$estimate < 0),
        sum(shifts$estimate == 0)
      )
    )
  }
  if (nrow(knowledge)) {
    text <- c(
      text,
      sprintf(
        paste(
          "Knowledge changes are estimable for %s event\u2013item combinations,",
          "ranging from %.3f to %.3f of the score range. These describe",
          "respondents with usable scores."
        ),
        nrow(knowledge),
        min(knowledge$estimate),
        max(knowledge$estimate)
      )
    )
  }
  concentration <- m[m$metric == "max_speaker_share" & m$status == "computed", ]
  if (nrow(concentration)) {
    text <- c(
      text,
      sprintf(
        paste(
          "Across %s observed sessions, the largest participant's airtime",
          "share ranges from %.0f%% to %.0f%%. Recording coverage and",
          "attendance define what these shares describe."
        ),
        nrow(concentration),
        100 * min(concentration$estimate),
        100 * max(concentration$estimate)
      )
    )
  }
  missing <- result$capabilities[result$capabilities$status == "unavailable", ]
  if (nrow(missing)) {
    text <- c(
      text,
      sprintf(
        paste(
          "%s event\u2013analysis combinations lack required inputs. The evidence",
          "table lists what would unlock each analysis."
        ),
        nrow(missing)
      )
    )
  }
  if (!length(text)) {
    text <- "No headline diagnostic is assessable from the supplied measurements."
  }
  text
}

report_plot <- function(result, metrics, title) {
  d <- report_metrics(result)
  d <- d[d$metric %in% metrics & d$status == "computed", , drop = FALSE]
  if (!nrow(d)) {
    return(NULL)
  }
  if (all(metrics %in% c("H", "P", "D"))) {
    d <- d[!is.na(d$group_id), , drop = FALSE]
    if (!nrow(d)) {
      return(NULL)
    }
    return(
      ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate, y = .data$event)) +
        ggplot2::geom_vline(xintercept = 0, colour = "grey70") +
        ggplot2::geom_boxplot(outlier.shape = NA, colour = "#166b64") +
        ggplot2::facet_wrap(~metric, scales = "free_x") +
        ggplot2::labs(
          title = title,
          x = "Fraction of declared scale range",
          y = NULL,
          caption = paste(
            "Distributions of group\u2013item estimates within each event. Repeated",
            "items and groups are not independent observations."
          )
        ) +
        ggplot2::theme_minimal(base_size = 11)
    )
  }
  if (
    all(metrics %in% c("opinion_change", "knowledge_gain", "post_attrition"))
  ) {
    d <- d[is.na(d$group_id) & d$detail %in% c("", "all"), , drop = FALSE]
    d$unit <- d$event
  } else {
    d$unit <- paste(d$event, d$session_id)
  }
  if (!nrow(d)) {
    return(NULL)
  }
  ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate, y = .data$unit)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70") +
    ggplot2::geom_point(colour = "#166b64", alpha = .7, size = 2) +
    ggplot2::facet_wrap(~metric, scales = "free_x") +
    ggplot2::labs(
      title = title,
      x = "Estimate (metric units shown in the tables)",
      y = NULL,
      caption = paste(
        "Each point is an eligible diagnostic. Overlapping points can",
        "represent different items or perspectives; the tables retain",
        "every value."
      )
    ) +
    ggplot2::theme_minimal(base_size = 11)
}
