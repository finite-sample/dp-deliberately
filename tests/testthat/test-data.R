test_that("the synthetic example validates and exposes all layers", {
  x <- example_deliberation()
  expect_equal(nrow(validate_data(x)), 0L)
  ready <- available_analyses(x)
  expect_true(all(ready$readiness == "ready"))
  expect_setequal(
    unique(ready$layer),
    c("design", "participation", "exchange", "outcomes")
  )
})

test_that("keys, parent joins and invalid scales fail with structured findings", {
  x <- survey_fixture()
  x$tables$responses <- rbind(x$tables$responses, x$tables$responses[1, ])
  expect_true("DUPLICATE_KEY" %in% validate_data(x)$code)
  expect_error(audit_dp(x), class = "deliberation_validation_error")
  x <- survey_fixture()
  x$tables$responses$person_id[1] <- "missing"
  expect_true("ORPHAN_REFERENCE" %in% validate_data(x)$code)
  x$tables$responses$value[1] <- 1.1
  expect_true("OUT_OF_SCALE" %in% validate_data(x)$code)
  x$tables$responses$value <- as.character(x$tables$responses$value)
  expect_true("TYPE" %in% validate_data(x)$code)
})

test_that("missing columns and timing are reported without crashing validation", {
  x <- example_deliberation()
  x$tables$people$person_id <- NULL
  expect_true("MISSING_COLUMNS" %in% validate_data(x)$code)
  x <- example_deliberation()
  x$tables$turns$end[1] <- 2
  expect_true("INTERVAL" %in% validate_data(x)$code)
  x$tables$turns$end[1] <- 130
  expect_true("OUTSIDE_SESSION" %in% validate_data(x)$code)
  x$tables$waves <- rbind(
    x$tables$waves,
    transform(x$tables$waves[1, ], wave_id = "second-pre")
  )
  expect_true("AMBIGUOUS_PHASE" %in% validate_data(x)$code)
})

test_that("polymorphic evidence references and model provenance are validated", {
  x <- example_deliberation()
  x$tables$annotations$target_id[1] <- "nonexistent"
  expect_true("ORPHAN_TARGET" %in% validate_data(x)$code)
  x$tables$annotations$source_type[1] <- "model"
  expect_true("MODEL_PROVENANCE" %in% validate_data(x)$code)
})

test_that("survey-only input is useful and missing transcripts stay unavailable", {
  x <- survey_fixture()
  a <- audit_dp(x, config = quick_config())
  expect_true(any(a$metrics$metric == "H" & a$metrics$status == "computed"))
  expect_equal(
    a$capabilities$status[a$capabilities$analysis == "participation"],
    "unavailable"
  )
  expect_equal(nrow(a$policy_results), 0L)
  expect_false(any(a$metrics$analysis == "participation"))
})

test_that("CSV bundles preserve IDs and raw attributes", {
  x <- survey_fixture()
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  for (nm in names(x$tables)) {
    utils::write.csv(
      x$tables[[nm]],
      file.path(path, paste0(nm, ".csv")),
      row.names = FALSE
    )
  }
  y <- read_deliberation(path)
  expect_equal(validate_data(y), validate_data(x))
  expect_equal(audit_dp(y)$metrics$estimate, audit_dp(x)$metrics$estimate)
  expect_equal(nrow(y$provenance$files), length(x$tables))
})
