education_contrast <- function() {
  define_contrast(
    "education",
    function(p) p$education == "lower",
    function(p) p$education == "higher",
    rationale = "Synthetic educational disadvantage",
    reference_advantaged = TRUE
  )
}

quick_config <- function(...) audit_config(permutations = 19L, ...)

estimate_of <- function(m, id, group = "g1", item = NULL) {
  keep <- m$metric == id
  keep <- keep &
    if (is.null(group)) {
      is.na(m$group_id)
    } else {
      !is.na(m$group_id) & m$group_id == group
    }
  if (!is.null(item)) {
    keep <- keep & !is.na(m$item_id) & m$item_id == item
  }
  m$estimate[keep]
}

survey_fixture <- function(
  pre = c(0.2, 0.4, 0.6, 0.8),
  post = c(0.4, 0.5, 0.6, 0.7)
) {
  n <- length(pre)
  deliberation_data(
    events = data.frame(event_id = "e", label = "Test"),
    episodes = data.frame(event_id = "e", episode_id = "ep", label = "Episode"),
    people = data.frame(
      event_id = "e",
      person_id = paste0("p", seq_len(n)),
      role = "participant",
      advantage = rep(c(FALSE, TRUE), length.out = n)
    ),
    assignments = data.frame(
      event_id = "e",
      episode_id = "ep",
      person_id = paste0("p", seq_len(n)),
      intended_group = "g1",
      actual_group = "g1",
      block = "all"
    ),
    items = data.frame(
      event_id = "e",
      item_id = "a",
      label = "Attitude",
      kind = "attitude",
      lower = 0,
      upper = 1,
      direction = 1,
      midpoint = 0.5
    ),
    waves = data.frame(
      event_id = "e",
      episode_id = "ep",
      wave_id = c("t1", "t2"),
      phase = c("pre", "post"),
      timing = c("before", "after")
    ),
    responses = data.frame(
      event_id = "e",
      episode_id = "ep",
      person_id = rep(paste0("p", seq_len(n)), 2),
      item_id = "a",
      wave_id = rep(c("t1", "t2"), each = n),
      value = c(pre, post)
    )
  )
}
