test_that("unscored knowledge has no usable scores", {
  x <- example_deliberation()
  x$tables$items$scoring <- NA_character_
  m <- outcome_metrics(x, list(), quick_config())
  m <- m[
    m$metric %in% c("knowledge_gain", "fraction_learning", "knowledge_gain_sd"),
  ]
  expect_true(all(m$n == 0))
  expect_true(all(m$coverage == 0))
})

test_that("interruption ratio coverage uses all eligible coded turns", {
  x <- example_deliberation()
  a <- x$tables$annotations
  a$value[a$code == "interruption_received"] <- "1"
  x$tables$annotations <- a
  ct <- define_contrast(
    "one",
    function(p) p$person_id == "p01",
    function(p) p$person_id %in% c("p02", "p03", "p04"),
    rationale = "Test"
  )
  ct <- resolve_contrasts(x, ct)
  m <- exchange_metrics(x, ct, quick_config())
  m <- m[m$metric == "interruption_received_ratio" & m$session_id == "s1", ]
  expect_equal(m$estimate, 1)
  expect_equal(m$n, 4)
  expect_equal(m$denominator, 4)
  expect_equal(m$coverage, 1)
})

test_that("logical schema fields do not coerce character flags", {
  x <- example_deliberation()
  x$tables$arguments$reviewed <- as.character(x$tables$arguments$reviewed)
  expect_true(any(validate_data(x)$code == "TYPE"))
})
