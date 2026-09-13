policy_fixture <- function(verdict = "WARNING") {
  audit_policy(
    "test-1",
    data.frame(
      rule_id = "concentration",
      scope = "session",
      metric = "max_speaker_share",
      operator = ">=",
      threshold = .27,
      verdict = verdict,
      min_coverage = 1,
      aggregation = "any"
    ),
    rationale = "Illustrative threshold, not a validated quality standard"
  )
}

test_that("policy boundaries produce explicit verdicts only", {
  x <- example_deliberation()
  m <- participation_metrics(x, list(), quick_config())
  m$metric_id <- paste0("m", seq_len(nrow(m)))
  policy <- policy_fixture()
  p <- evaluate_policy(m, policy)
  expect_true(all(p$verdict == "WARNING"))
  policy$rules$threshold <- 15 / 55
  expect_true(all(evaluate_policy(m, policy)$verdict == "WARNING"))
  policy$rules$threshold <- .3
  expect_true(all(evaluate_policy(m, policy)$verdict == "NOT_TRIGGERED"))
})

test_that("missing evidence cannot yield a PASS and compound rules need every operand", {
  x <- example_deliberation()
  m <- participation_metrics(x, list(), quick_config())
  m$metric_id <- paste0("m", seq_len(nrow(m)))
  p <- policy_fixture("PASS")
  p$rules <- rbind(
    p$rules,
    transform(p$rules, metric = "counterargument_response_rate", threshold = .5)
  )
  expect_true(all(evaluate_policy(m, p)$verdict == "NOT_ASSESSED"))
  p <- policy_fixture("PASS")
  m$coverage <- .8
  expect_true(all(evaluate_policy(m, p)$verdict == "NOT_ASSESSED"))
})

test_that("configuration, hashes and evaluated memberships are reproducible", {
  x <- survey_fixture()
  set.seed(72)
  state <- .Random.seed
  a <- audit_dp(x)
  b <- audit_dp(x)
  expect_identical(.Random.seed, state)
  expect_identical(a$metrics, b$metrics)
  expect_identical(a$metadata$input_hashes, b$metadata$input_hashes)
  x$tables$responses$value[1] <- .3
  expect_false(identical(
    a$metadata$input_hashes,
    audit_dp(x)$metadata$input_hashes
  ))
  expect_error(audit_config(permutations = 0), "configuration")
})
