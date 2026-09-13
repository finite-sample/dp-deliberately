test_that("participation includes silent attendees and matches hand calculations", {
  x <- example_deliberation()
  m <- participation_metrics(x, list(), quick_config())
  expect_equal(estimate_of(m, "max_speaker_share"), 15 / 55)
  expect_equal(estimate_of(m, "talk_gini"), 15 / 220)
  expect_equal(estimate_of(m, "silent_fraction"), 0)
  x$tables$turns <- x$tables$turns[x$tables$turns$turn_id != "s1t1", ]
  m <- participation_metrics(x, list(), quick_config())
  expect_equal(estimate_of(m, "silent_fraction"), .25)
  expect_equal(estimate_of(m, "talk_gini"), .25)
})

test_that("partial recordings do not classify silence; word shares survive missing timing", {
  x <- example_deliberation()
  x$tables$coverage$end <- 100
  m <- participation_metrics(x, list(), quick_config())
  expect_true(all(is.na(m$estimate[m$metric == "silent_fraction"])))
  x$tables$turns$start <- x$tables$turns$end <- NULL
  m <- participation_metrics(x, list(), quick_config())
  expect_true(any(is.finite(m$estimate[m$metric == "person_word_share"])))
  expect_true(all(is.na(m$estimate[m$metric == "person_seconds"])))
})

test_that("overlap is counted separately and repeated attendee intervals are unioned", {
  expect_equal(interval_length(c(0, 5, 20), c(10, 15, 25)), 20)
  expect_equal(
    intersection_length(
      union_intervals(c(0, 5), c(10, 15)),
      union_intervals(10, 20)
    ),
    5
  )
  x <- example_deliberation()
  x$tables$turns$start[2] <- 10
  m <- participation_metrics(x, list(), quick_config())
  expect_equal(estimate_of(m, "overlap_pair_seconds"), 5)
  expect_false(any(m$metric == "interruptions"))
})

test_that("unknown speakers prevent misleading participant shares", {
  x <- example_deliberation()
  x$tables$turns$person_id[1] <- NA_character_
  m <- participation_metrics(x, list(), quick_config())
  expect_true(is.na(estimate_of(m, "max_speaker_share")))
  expect_equal(estimate_of(m, "unknown_speaker_turns"), 1)
})

test_that("annotations distinguish absent, missing, conflicting and adjudicated codes", {
  x <- example_deliberation()
  m <- exchange_metrics(x, list(), quick_config())
  expect_equal(estimate_of(m, "reason_giving"), 1)
  expect_equal(estimate_of(m, "evidence_giving"), .25)
  expect_equal(estimate_of(m, "counterargument_response_rate"), 1)
  target <- x$tables$annotations[
    x$tables$annotations$target_id == "s1t1" &
      x$tables$annotations$code == "reason_level",
  ]
  target$annotation_id <- "conflict"
  target$coder <- "other"
  target$value <- "0"
  x$tables$annotations <- rbind(x$tables$annotations, target)
  m <- exchange_metrics(x, list(), quick_config())
  row <- m[m$group_id == "g1" & m$metric == "reason_giving", ]
  expect_equal(row$n, 3)
  expect_equal(row$coverage, .75)
  target$annotation_id <- "adjudication"
  target$review_status <- "adjudicated"
  x$tables$annotations <- rbind(x$tables$annotations, target)
  m <- exchange_metrics(x, list(), quick_config())
  expect_equal(estimate_of(m, "reason_giving"), .75)
})

test_that("missing response links do not establish ignored counterarguments", {
  x <- example_deliberation()
  x$tables$links <- NULL
  x$tables$annotations <- x$tables$annotations[
    x$tables$annotations$code != "response_observed",
  ]
  m <- exchange_metrics(x, list(), quick_config())
  expect_true(all(is.na(m$estimate[
    m$metric == "counterargument_response_rate"
  ])))
})

test_that("argument coverage distinguishes lower bounds from completed coding", {
  x <- example_deliberation()
  m <- argument_metrics(x, list(), quick_config())
  expect_true(all(m$estimate[m$metric == "argument_coverage"] == 1))
  x$tables$turns$argument_coding_complete <- FALSE
  m <- argument_metrics(x, list(), quick_config())
  expect_true(all(is.na(m$estimate[m$metric == "argument_coverage"])))
  expect_true(all(m$estimate[m$metric == "argument_coverage_lower_bound"] == 1))
  x$tables$materials$available_at <- 200
  m <- argument_metrics(x, list(), quick_config())
  expect_true(all(m$estimate[m$metric == "briefing_argument_coverage"] == 0))
})

test_that("opposing exposure requires attendance overlap and reviewed arguments", {
  x <- example_deliberation()
  m <- exposure_metrics(x, list(), quick_config())
  expect_true(all(m$estimate == 1))
  x$tables$attendance$enter <- 90
  m <- exposure_metrics(x, list(), quick_config())
  expect_true(all(m$estimate == 0))
  x$tables$arguments$reviewed <- FALSE
  m <- exposure_metrics(x, list(), quick_config())
  expect_true(all(is.na(m$estimate)))
})
