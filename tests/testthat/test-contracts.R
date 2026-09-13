test_that("an event with no measurements cannot pass a policy", {
  x <- deliberation_data(
    events = data.frame(event_id = "empty", label = "No measurements")
  )
  p <- audit_policy(
    "test",
    data.frame(
      rule_id = "learning",
      scope = "event",
      metric = "knowledge_gain",
      operator = ">",
      threshold = 0,
      verdict = "PASS",
      min_coverage = 0,
      aggregation = "all"
    ),
    rationale = "Test missing-data handling"
  )
  a <- audit_dp(x, policy = p)
  expect_equal(a$policy_results$verdict, "NOT_ASSESSED")
  expect_true(all(a$capabilities$status == "unavailable"))
})

test_that("CSV input preserves leading zeroes in identifiers", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  utils::write.csv(
    data.frame(event_id = "001", label = "Event"),
    file.path(path, "events.csv"),
    row.names = FALSE
  )
  x <- read_deliberation(path)
  expect_identical(x$tables$events$event_id, "001")
})

test_that("knowledge indices require the complete declared item set", {
  x <- example_deliberation()
  item <- x$tables$items[x$tables$items$kind == "knowledge", ]
  item$item_id <- "second_knowledge"
  x$tables$items <- rbind(x$tables$items, item)
  extra <- x$tables$responses[x$tables$responses$item_id == "knowledge", ]
  extra$item_id <- "second_knowledge"
  x$tables$responses <- rbind(x$tables$responses, extra)
  m <- outcome_metrics(x, list(), quick_config())
  expect_equal(
    estimate_of(m, "knowledge_gain", item = "__knowledge_mean__"),
    .5
  )
  x$tables$responses <- x$tables$responses[
    x$tables$responses$item_id != "second_knowledge",
  ]
  m <- outcome_metrics(x, list(), quick_config())
  expect_true(is.na(estimate_of(
    m,
    "knowledge_gain",
    item = "__knowledge_mean__"
  )))
})

test_that("response annotation order and source spans are checked", {
  x <- example_deliberation()
  row <- which(x$tables$links$to_type == "turn")[1]
  x$tables$links$to_id[row] <- x$tables$links$from_turn[row]
  expect_true("RESPONSE_ORDER" %in% validate_data(x)$code)
  x <- example_deliberation()
  x$tables$annotations$start_char <- 1
  x$tables$annotations$end_char <- 100000
  expect_true("SOURCE_SPAN" %in% validate_data(x)$code)
})

test_that("reviewed evidence does not silently mix codebook versions", {
  x <- example_deliberation()
  x$tables$annotations$codebook_version[1] <- "different"
  expect_error(resolved_codes(x, quick_config()), "one codebook version")
})

test_that("derived knowledge scoring also feeds sampling inference", {
  skip_if_not_installed("survey")
  x <- survey_fixture(c(1, 2, 1, 2), c(1, 1, 1, 2))
  x$tables$items$kind <- "knowledge"
  x$tables$items$lower <- 1
  x$tables$items$upper <- 2
  x$tables$items$midpoint <- NA_real_
  x$tables$items$scoring <- "binary_key"
  x$tables$items$correct_value <- 1
  x$tables$sampling <- data.frame(
    event_id = "e",
    person_id = paste0("p", 1:4),
    weight = 1,
    psu = paste0("p", 1:4),
    stratum = "all"
  )
  m <- survey_metrics(x, list(), audit_config(probability_sample = TRUE))
  expect_equal(m$estimate, .25)
})

test_that("empty sessions and non-comparison waves remain assessable as inputs", {
  x <- example_deliberation()
  x$tables$turns <- x$tables$turns[x$tables$turns$session_id != "s1", ]
  m <- participation_metrics(x, list(), quick_config())
  expect_equal(estimate_of(m, "silent_fraction"), 1)
  x$tables$attendance <- x$tables$attendance[x$tables$attendance$session_id != "s1", ]
  m <- participation_metrics(x, list(), quick_config())
  expect_true(is.na(estimate_of(m, "silent_fraction")))
  x <- survey_fixture()
  x$tables$waves$phase <- "other"
  expect_equal(nrow(paired_responses(x)), 0L)
  expect_equal(audit_dp(x)$capabilities$status[audit_dp(x)$capabilities$analysis == "outcomes"], "unassessable")
})

test_that("survey inference restores the caller's variance options", {
  skip_if_not_installed("survey")
  old <- options(survey.lonely.psu = "adjust")
  on.exit(options(old))
  x <- example_deliberation()
  survey_metrics(x, resolve_contrasts(x, list(education_contrast())), audit_config(probability_sample = TRUE))
  expect_identical(getOption("survey.lonely.psu"), "adjust")
})
