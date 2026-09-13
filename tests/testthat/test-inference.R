test_that("survey inference agrees with an independent paired-mean calculation", {
  skip_if_not_installed("survey")
  x <- survey_fixture()
  x$tables$sampling <- data.frame(
    event_id = "e",
    person_id = paste0("p", 1:4),
    weight = 1,
    psu = paste0("p", 1:4),
    stratum = "all"
  )
  m <- survey_metrics(x, list(), audit_config(probability_sample = TRUE))
  changes <- c(.2, .1, 0, -.1)
  se <- stats::sd(changes) / 2
  expect_equal(m$estimate, mean(changes))
  expect_equal(m$se, se)
  expect_equal(m$lower, mean(changes) - stats::qt(.975, 3) * se)
  expect_true(is.na(survey_metrics(x, list(), audit_config())$estimate))
})

test_that("unsupported randomization and incomplete rosters are not permuted", {
  x <- example_deliberation()
  x$tables$randomization$mechanism <- "unknown"
  m <- group_metrics(x, list(), quick_config())
  expect_true(all(is.na(m$p_value)))
  x$tables$randomization$mechanism <- "complete_fixed_sizes"
  x$tables$responses$value[1] <- NA_real_
  m <- group_metrics(x, list(), quick_config())
  row <- m[m$analysis == "randomization" & m$item_id == "support", ]
  expect_true(is.na(row$p_value))
})

test_that("blocked assignment tests reproduce the null and preserve random state", {
  x <- example_deliberation()
  x$tables$randomization$mechanism <- "blocked_fixed_sizes"
  x$tables$assignments$block <- rep(c("a", "b"), 8)
  set.seed(42)
  before <- .Random.seed
  a <- group_metrics(x, list(), quick_config())
  b <- group_metrics(x, list(), quick_config())
  expect_identical(a, b)
  expect_identical(before, .Random.seed)
  expect_true(all(a$p_value[a$analysis == "randomization"] == 1))
})

test_that("normal paired intervals have expected coverage in a known-truth simulation", {
  coverage <- with_seed(
    77,
    replicate(500, {
      delta <- stats::rnorm(30, .2, .5)
      ci <- mean(delta) +
        c(-1, 1) * stats::qt(.975, 29) * stats::sd(delta) / sqrt(30)
      ci[1] <= .2 && ci[2] >= .2
    })
  )
  expect_gt(mean(coverage), .92)
  expect_lt(mean(coverage), .98)
  expected <- (.5 / sqrt(100)) * (stats::qnorm(.975) + stats::qnorm(.8))
  expect_equal(detectable_change(100, .5)$detectable_change, expected)
  expect_error(detectable_change(1, .5), "Invalid")
})

test_that("out-of-fold AUC handles ties and unknown selection", {
  expect_equal(auc_score(c(0, 1, 0, 1), rep(.5, 4)), .5)
  expect_equal(auc_score(c(0, 0, 1, 1), c(.1, .2, .8, .9)), 1)
  expect_true(is.na(auc_score(rep(1, 4), rep(.5, 4))))
  d <- data.frame(
    attended = c(FALSE, TRUE, FALSE, TRUE),
    age = c(20, 30, 40, 50)
  )
  expect_true(is.na(attendance_auc(d, character(), 1)$value))
})
