#' A synthetic deliberative event with all audit inputs
#' @return A [deliberation_data()] bundle. All people, statements, measurements
#'   and annotations are invented for software testing, not empirical evidence.
#' @export
example_deliberation <- function() {
  ids <- sprintf("p%02d", 1:20)
  people <- data.frame(
    event_id = "demo",
    person_id = c(ids, "m1", "m2", "e1", "e2"),
    role = c(
      rep("participant", 20),
      "moderator",
      "moderator",
      "expert",
      "expert"
    ),
    education = c(rep(c("lower", "higher"), 10), rep(NA_character_, 4)),
    age = c(25 + seq_len(20), rep(NA_real_, 4))
  )
  assignments <- data.frame(
    event_id = "demo",
    episode_id = "main",
    person_id = ids[1:16],
    intended_group = rep(paste0("g", 1:4), each = 4),
    actual_group = rep(paste0("g", 1:4), each = 4),
    block = "all"
  )
  sessions <- data.frame(
    event_id = "demo",
    episode_id = "main",
    session_id = paste0("s", 1:4),
    group_id = paste0("g", 1:4),
    type = "small_group",
    topic_id = "transport",
    start = 0,
    end = 120
  )
  attendance <- data.frame(
    event_id = "demo",
    session_id = rep(sessions$session_id, each = 4),
    person_id = ids[1:16],
    interval_id = "whole",
    status = "present",
    enter = 0,
    exit = 120
  )
  turns <- list()
  for (i in 1:4) {
    speakers <- c(
      ids[(4 * i - 3):(4 * i)],
      if (i <= 2) "m1" else "m2",
      if (i %% 2) "e1" else "e2"
    )
    text <- c(
      "I support the bus route because it makes jobs easier to reach.",
      "The route costs money; the briefing estimates a substantial operating subsidy.",
      "That cost matters, but could a smaller route preserve access to work?",
      "I disagree with the smaller route because it would omit our neighborhood.",
      "Let us hear the reasons on both sides before moving on.",
      "The briefing gives both the accessibility benefit and the budget estimate."
    )
    turns[[i]] <- data.frame(
      event_id = "demo",
      turn_id = paste0("s", i, "t", 1:6),
      session_id = paste0("s", i),
      person_id = speakers,
      sequence = 1:6,
      text = text,
      start = c(5, 20, 40, 60, 80, 100),
      end = c(15, 35, 55, 75, 90, 115),
      argument_coding_complete = TRUE
    )
  }
  turns <- bind_rows(turns)
  items <- data.frame(
    event_id = "demo",
    item_id = c("support", "knowledge"),
    label = c("Support for bus route", "Correct factual answer"),
    kind = c("attitude", "knowledge"),
    lower = 0,
    upper = c(10, 1),
    direction = 1,
    midpoint = c(5, NA),
    scoring = c(NA, "binary_key"),
    correct_value = c(NA, 1),
    topic_id = "transport",
    perspective_positive = c("support", NA),
    perspective_negative = c("oppose", NA)
  )
  responses <- expand.grid(
    event_id = "demo",
    episode_id = "main",
    person_id = ids,
    item_id = items$item_id,
    wave_id = c("baseline", "after"),
    stringsAsFactors = FALSE
  )
  index <- match(responses$person_id, ids)
  responses$value <- ifelse(
    responses$item_id == "support",
    rep(c(2, 7, 4, 8), length.out = nrow(responses)),
    index %% 2
  )
  post <- responses$wave_id == "after"
  responses$value[post & responses$item_id == "support"] <- pmin(
    10,
    responses$value[post & responses$item_id == "support"] + 1
  )
  responses$value[post & responses$item_id == "knowledge"] <- 1
  responses$value[post & index > 16] <- NA_real_
  annotations <- list()
  for (code in c(
    "recommendation",
    "reason_level",
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
    "floor_success",
    "stance"
  )) {
    value <- switch(
      code,
      recommendation = as.integer(turns$sequence <= 4),
      reason_level = rep(c(1, 2, 3, 1, 0, 2), 4),
      evidence_present = rep(c(0, 1, 0, 0, 0, 1), 4),
      counterargument = as.integer(turns$sequence == 2),
      response_observed = rep(1, nrow(turns)),
      engages_previous = as.integer(turns$sequence %in% c(3, 4)),
      floor_attempt = rep(1, nrow(turns)),
      floor_success = rep(1, nrow(turns)),
      stance = rep(c(1, -1, 0, -1, 0, 0), 4),
      rep(0, nrow(turns))
    )
    annotations[[length(annotations) + 1L]] <- data.frame(
      event_id = "demo",
      annotation_id = paste(turns$turn_id, code, sep = "-"),
      target_type = "turn",
      target_id = turns$turn_id,
      code = code,
      value = as.character(value),
      coder = "synthetic-coder",
      source_type = "human",
      codebook_version = "demo-1",
      review_status = "accepted"
    )
  }
  links <- list()
  for (i in 1:4) {
    links[[i]] <- data.frame(
      event_id = "demo",
      link_id = paste0("s", i, "l", 1:6),
      from_turn = paste0("s", i, "t", c(1, 2, 3, 4, 3, 4)),
      to_type = c(rep("argument", 4), "turn", "turn"),
      to_id = c(
        "access",
        "cost",
        "compromise",
        "coverage",
        paste0("s", i, "t2"),
        paste0("s", i, "t3")
      ),
      relation = c(rep("expresses", 4), "substantive_response", "rebuts"),
      coder = "synthetic-coder",
      source_type = "human",
      codebook_version = "demo-1",
      review_status = "accepted"
    )
  }
  deliberation_data(
    events = data.frame(
      event_id = "demo",
      label = "Synthetic transport deliberation"
    ),
    episodes = data.frame(
      event_id = "demo",
      episode_id = "main",
      label = "Discussion"
    ),
    sessions = sessions,
    people = people,
    recruitment = data.frame(
      event_id = "demo",
      person_id = ids,
      stage = "recruited",
      attended = seq_len(20) <= 16
    ),
    attendance = attendance,
    assignments = assignments,
    moderators = data.frame(
      event_id = "demo",
      session_id = sessions$session_id,
      person_id = c("m1", "m1", "m2", "m2")
    ),
    items = items,
    waves = data.frame(
      event_id = "demo",
      episode_id = "main",
      wave_id = c("baseline", "after"),
      phase = c("pre", "post"),
      timing = c("before briefing", "after discussion")
    ),
    responses = responses,
    benchmarks = data.frame(
      event_id = "demo",
      benchmark_id = c("education", "baseline"),
      variable = c("education", "item:main:support"),
      level = c("lower", NA),
      target = c(0.5, 0.5),
      sd = c(NA, 0.25),
      population = "Synthetic target"
    ),
    sampling = data.frame(
      event_id = "demo",
      person_id = ids,
      weight = 1,
      psu = ids,
      stratum = "all"
    ),
    randomization = data.frame(
      event_id = "demo",
      episode_id = "main",
      mechanism = "complete_fixed_sizes"
    ),
    turns = turns,
    coverage = data.frame(
      event_id = "demo",
      session_id = sessions$session_id,
      coverage_id = "whole",
      start = 0,
      end = 120
    ),
    materials = data.frame(
      event_id = "demo",
      passage_id = c("b1", "b2"),
      source_id = "briefing",
      version = "demo-1",
      text = c(
        "Access and route coverage benefits.",
        "Costs and a smaller-route alternative."
      ),
      available_at = 0
    ),
    arguments = data.frame(
      event_id = "demo",
      argument_id = c("access", "cost", "compromise", "coverage"),
      topic_id = "transport",
      perspective = c("support", "oppose", "support", "oppose"),
      text = c(
        "Access to jobs",
        "Operating cost",
        "Smaller route",
        "Excluded neighborhoods"
      ),
      reviewed = TRUE,
      reviewer = "synthetic-reviewer",
      passage_id = c("b1", "b2", "b2", "b1")
    ),
    argument_edges = data.frame(
      event_id = "demo",
      edge_id = "edge1",
      from_argument = "compromise",
      to_argument = "cost",
      relation = "responds_to"
    ),
    annotations = bind_rows(annotations),
    links = bind_rows(links),
    provenance = list(
      synthetic = TRUE,
      notice = "Invented data for software demonstration; not empirical evidence."
    )
  )
}
