#' Define explicit rules for audit verdicts
#' @param version Nonempty version identifying the researcher's policy.
#' @param rules Data frame with rule_id, scope (`event`, `group`, `session`),
#'   metric, operator (`<`, `<=`, `>`, `>=`, `==`), threshold, verdict
#'   (`PASS`, `WARNING`, `FAIL`), min_coverage (0 to 1), and aggregation (`all`
#'   or `any`). Rows sharing a rule_id are AND conditions. Optional item_id,
#'   contrast_id, detail, and person_id columns restrict matched diagnostics;
#'   NA means unrestricted. Unrestricted rules exclude person-level metrics.
#' @param rationale Explanation of the policy's normative basis.
#' @return A policy object. Rules do not create a composite score. A rule whose
#'   conditions fail returns NOT_TRIGGERED; missing or incomplete evidence
#'   returns NOT_ASSESSED. These are separate from explicit PASS verdicts.
#' @examples
#' audit_policy("example-1", data.frame(
#'   rule_id = "concentration", scope = "session", metric = "max_speaker_share",
#'   operator = ">", threshold = .6, verdict = "WARNING", min_coverage = 1,
#'   aggregation = "any"), rationale = "Illustrative rule, not a validated threshold")
#' @export
audit_policy <- function(version, rules, rationale) {
  required <- c(
    "rule_id",
    "scope",
    "metric",
    "operator",
    "threshold",
    "verdict",
    "min_coverage",
    "aggregation"
  )
  if (
    !is.data.frame(rules) || !all(required %in% names(rules)) || !nrow(rules)
  ) {
    stop("Policy rules are incomplete.")
  }
  if (
    length(version) != 1L ||
      is.na(version) ||
      !nzchar(version) ||
      length(rationale) != 1L ||
      is.na(rationale) ||
      !nzchar(rationale)
  ) {
    stop("Policy needs version and rationale.")
  }
  if (
    anyNA(rules[required]) ||
      !all(rules$scope %in% c("event", "group", "session")) ||
      !all(rules$operator %in% c("<", "<=", ">", ">=", "==")) ||
      !all(rules$verdict %in% c("PASS", "WARNING", "FAIL")) ||
      !all(rules$aggregation %in% c("all", "any")) ||
      any(!is.finite(rules$threshold)) ||
      any(!is.finite(rules$min_coverage)) ||
      any(rules$min_coverage < 0 | rules$min_coverage > 1)
  ) {
    stop("Invalid policy rule values.")
  }
  for (r in split_rows(rules, "rule_id")) {
    if (length(unique(r$scope)) != 1L || length(unique(r$verdict)) != 1L) {
      stop("Conditions in a rule must share scope and verdict.")
    }
  }
  structure(
    list(version = version, rules = rules, rationale = rationale),
    class = "deliberation_policy"
  )
}

evaluate_policy <- function(metrics, policy, x = NULL) {
  empty <- data.frame(
    event_id = character(),
    episode_id = character(),
    group_id = character(),
    session_id = character(),
    rule_id = character(),
    verdict = character(),
    reason = character(),
    metric_ids = character(),
    policy_version = character()
  )
  if (is.null(policy)) {
    return(empty)
  }
  if (!inherits(policy, "deliberation_policy")) {
    stop("Use audit_policy() to define a policy.")
  }
  if (!is.null(x)) {
    units <- list()
    for (event in x$tables$events$event_id) {
      units[[length(units) + 1L]] <- metric("", "", NA_real_, event)
    }
    for (s in split_rows(x$tables$sessions, c("event_id", "session_id"))) {
      units[[length(units) + 1L]] <- metric(
        "",
        "",
        NA_real_,
        s$event_id[1],
        s$episode_id[1],
        s$group_id[1],
        s$session_id[1]
      )
    }
    for (a in split_rows(
      x$tables$assignments,
      c("event_id", "episode_id", "actual_group")
    )) {
      units[[length(units) + 1L]] <- metric(
        "",
        "",
        NA_real_,
        a$event_id[1],
        a$episode_id[1],
        a$actual_group[1]
      )
    }
    metrics <- dplyr::bind_rows(c(list(metrics), units))
  }
  rows <- list()
  for (rule in split_rows(policy$rules, "rule_id")) {
    scope <- rule$scope[1]
    keys <- switch(
      scope,
      event = "event_id",
      group = c("event_id", "episode_id", "group_id"),
      session = c("event_id", "episode_id", "group_id", "session_id")
    )
    eligible <- metrics
    if (scope == "group") {
      eligible <- eligible[!is.na(eligible$group_id), , drop = FALSE]
    }
    if (scope == "session") {
      eligible <- eligible[!is.na(eligible$session_id), , drop = FALSE]
    }
    for (z in split_rows(eligible, keys)) {
      conditions <- logical(nrow(rule))
      assessable <- logical(nrow(rule))
      ids <- character()
      for (i in seq_len(nrow(rule))) {
        m <- z[z$metric == rule$metric[i], , drop = FALSE]
        if (scope == "event") {
          m <- m[is.na(m$group_id) & is.na(m$session_id), , drop = FALSE]
        }
        if (scope == "group") {
          m <- m[is.na(m$person_id), , drop = FALSE]
        }
        for (col in intersect(
          c("item_id", "contrast_id", "detail", "person_id"),
          names(rule)
        )) {
          if (!is.na(rule[[col]][i])) {
            m <- m[
              !is.na(m[[col]]) & m[[col]] == rule[[col]][i],
              ,
              drop = FALSE
            ]
          }
        }
        if (!"person_id" %in% names(rule) || is.na(rule$person_id[i])) {
          m <- m[is.na(m$person_id), , drop = FALSE]
        }
        usable <- m$status == "computed" &
          is.finite(m$coverage) &
          m$coverage >= rule$min_coverage[i]
        assessable[i] <- nrow(m) > 0 && all(usable)
        ids <- c(ids, m$metric_id)
        if (assessable[i]) {
          compare <- switch(
            rule$operator[i],
            `<` = `<`,
            `<=` = `<=`,
            `>` = `>`,
            `>=` = `>=`,
            `==` = `==`
          )
          passed <- compare(m$estimate, rule$threshold[i])
          conditions[i] <- if (rule$aggregation[i] == "all") {
            all(passed)
          } else {
            any(passed)
          }
        }
      }
      verdict <- if (!all(assessable)) {
        "NOT_ASSESSED"
      } else if (all(conditions)) {
        rule$verdict[1]
      } else {
        "NOT_TRIGGERED"
      }
      row <- empty[NA_integer_, , drop = FALSE]
      row$event_id <- as.character(z$event_id[1])
      for (key in setdiff(keys, "event_id")) {
        row[[key]] <- as.character(z[[key]][1])
      }
      row$rule_id <- rule$rule_id[1]
      row$verdict <- verdict
      row$reason <- if (!all(assessable)) {
        "Missing, unassessable, or insufficiently covered required diagnostics."
      } else if (!all(conditions)) {
        "Rule conditions not met."
      } else {
        "All declared rule conditions met."
      }
      row$metric_ids <- paste(unique(ids), collapse = ";")
      row$policy_version <- policy$version
      rows[[length(rows) + 1L]] <- row
    }
  }
  if (!length(rows)) {
    return(empty)
  }
  dplyr::bind_rows(rows)
}
