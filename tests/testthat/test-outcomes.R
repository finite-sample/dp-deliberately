test_that("H P and D match hand calculations including undefined direction", {
  x <- survey_fixture()
  ct <- define_contrast(
    "adv",
    function(p) !p$advantage,
    function(p) p$advantage,
    rationale = "Test reference",
    reference_advantaged = TRUE
  )
  m <- audit_dp(x, ct)$metrics
  expect_equal(
    estimate_of(m, "H"),
    stats::sd(c(.2, .4, .6, .8)) - stats::sd(c(.4, .5, .6, .7))
  )
  expect_true(is.na(estimate_of(m, "P")))
  expect_equal(estimate_of(m, "P_absolute"), .05)
  expect_equal(estimate_of(m, "D"), .05)
  expect_equal(estimate_of(m, "D_focal"), .1)
  expect_equal(signed_movement(.2, .8, .6), .6)
  expect_true(is.na(signed_movement(.5, .7, .5)))
  expect_equal(signed_movement(.2, .2, .5), 0)
})

test_that("crossing the midpoint distinguishes directional and absolute P", {
  x <- survey_fixture(rep(.4, 4), rep(.9, 4))
  m <- audit_dp(x)$metrics
  expect_equal(estimate_of(m, "P"), -.5)
  expect_equal(estimate_of(m, "P_absolute"), .3)
})

test_that("paired and available-wave membership have distinct denominators", {
  x <- survey_fixture(c(.2, .4, .6, .8), c(.4, .5, .6, NA))
  paired <- audit_dp(x)$metrics
  available <- audit_dp(
    x,
    config = audit_config(membership = "available")
  )$metrics
  expect_equal(estimate_of(paired, "mean_pre"), .4)
  expect_equal(estimate_of(available, "mean_pre"), .5)
  expect_equal(paired$n[paired$metric == "H" & !is.na(paired$group_id)], 3)
})

test_that("contrast predicates and membership tables agree, with no complement assumption", {
  x <- example_deliberation()
  ct <- education_contrast()
  p <- x$tables$people[x$tables$people$role == "participant", ]
  membership <- data.frame(
    p[c("event_id", "person_id")],
    focal = p$education == "lower",
    reference = p$education == "higher"
  )
  explicit <- define_contrast(
    "education",
    rationale = ct$rationale,
    membership = membership,
    reference_advantaged = TRUE
  )
  a <- outcome_metrics(x, resolve_contrasts(x, list(ct)), quick_config())
  b <- outcome_metrics(x, resolve_contrasts(x, list(explicit)), quick_config())
  expect_equal(a, b)
  membership$reference[1] <- TRUE
  bad <- define_contrast("bad", rationale = "Overlap", membership = membership)
  expect_error(resolve_contrasts(x, list(bad)), "disjoint")
  expect_error(resolve_contrasts(x, list(ct, ct)), "unique")
})

test_that("D needs an explicit advantaged reference and two eligible sides", {
  x <- survey_fixture()
  ct <- define_contrast(
    "plain",
    function(p) !p$advantage,
    function(p) p$advantage,
    rationale = "Plain comparison"
  )
  expect_false("D" %in% audit_dp(x, ct)$metrics$metric)
  ct$reference_advantaged <- TRUE
  x$tables$people$advantage <- FALSE
  expect_true(all(is.na(audit_dp(x, ct)$metrics$estimate[
    audit_dp(x, ct)$metrics$metric == "D"
  ])))
})

test_that("knowledge scoring, learning fractions and subgroup gaps are explicit", {
  x <- example_deliberation()
  m <- outcome_metrics(
    x,
    resolve_contrasts(x, list(education_contrast())),
    quick_config()
  )
  expect_equal(estimate_of(m, "knowledge_gain", item = "knowledge"), .5)
  expect_equal(estimate_of(m, "fraction_learning", item = "knowledge"), .5)
  expect_equal(estimate_of(m, "knowledge_gap_change", item = "knowledge"), -1)
  x$tables$items$scoring <- NA_character_
  m <- outcome_metrics(x, list(), quick_config())
  expect_true(all(is.na(m$estimate[m$metric == "knowledge_gain"])))
})

test_that("weighted summaries use supplied weights and reject missing weights", {
  x <- survey_fixture()
  x$tables$sampling <- data.frame(
    event_id = "e",
    person_id = paste0("p", 1:4),
    weight = c(1, 1, 1, 7),
    psu = paste0("p", 1:4),
    stratum = "all"
  )
  m <- audit_dp(x, config = audit_config(weighted = TRUE))$metrics
  expect_equal(estimate_of(m, "mean_pre"), .68)
  x$tables$sampling <- x$tables$sampling[-1, ]
  expect_error(
    audit_dp(x, config = audit_config(weighted = TRUE)),
    "weights missing"
  )
})
