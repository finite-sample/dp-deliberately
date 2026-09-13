test_that("the adapter logs exact duplicates, unresolved identities and endpoint roundoff", {
  path <- tempfile()
  dir.create(file.path(path, "data"), recursive = TRUE)
  on.exit(unlink(path, recursive = TRUE))
  d <- data.frame(
    X = 1:4,
    pollid = 7,
    caseid = c(1, 2, NA, 1),
    pollgroup = 1,
    pre = c(.2, .4, .6, .2),
    post = c(.3, .4, .7, .3),
    t1know = c(0, 1 + 1e-12, 0, 0),
    t2know = c(1, 1, 1, 1)
  )
  dictionary <- data.frame(
    poll_id = 7,
    poll_name = "Synthetic source",
    att_index = "Item",
    t1var = "pre",
    t2_t3var = "post"
  )
  utils::write.csv(
    d,
    file.path(path, "data", "polardata.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    dictionary,
    file.path(path, "data", "poll_indices.csv"),
    row.names = FALSE
  )
  x <- read_distortions(path)
  expect_equal(nrow(x$tables$people), 3)
  expect_equal(
    x$provenance$ledger$n[
      x$provenance$ledger$action == "exact_duplicates_removed"
    ],
    1
  )
  expect_equal(
    x$provenance$ledger$n[x$provenance$ledger$action == "endpoint_roundoff"],
    1
  )
  expect_equal(sum(!x$tables$people$identity_verified), 1)
  expect_true(all(x$tables$responses$value <= 1))
  expect_true(any(x$tables$responses$source_value > 1))
  expect_true(is.null(x$tables$randomization))
  d$post[4] <- .9
  utils::write.csv(
    d,
    file.path(path, "data", "polardata.csv"),
    row.names = FALSE
  )
  expect_error(read_distortions(path), "Conflicting source person IDs")
})

test_that("HTML rendering works offline and evidence inclusion is explicit", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(
    rmarkdown::pandoc_available(),
    "Pandoc is required for report rendering"
  )
  x <- example_deliberation()
  x$tables$turns$text[1] <- "PRIVATE_EXCERPT_<script>alert('x')</script>"
  a <- audit_dp(x, config = quick_config())
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file))
  render_audit(a, file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "Synthetic demonstration")
  expect_match(html, "No verdict policy supplied")
  expect_false(grepl("PRIVATE_EXCERPT", html, fixed = TRUE))
  expect_false(grepl('<script[^>]+src="https?://', html))
  render_audit(a, file, include_evidence = TRUE)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "PRIVATE_EXCERPT_&lt;script&gt;")
  expect_false(grepl("PRIVATE_EXCERPT_<script>", html, fixed = TRUE))
})
