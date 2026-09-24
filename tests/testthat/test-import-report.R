test_that("the adapter logs exact duplicates, unresolved identities and endpoint roundoff", {
  path <- tempfile()
  dir.create(file.path(path, "evidence", "benchmarks"), recursive = TRUE)
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
  utils::write.table(
    d,
    file.path(path, "evidence", "benchmarks", "polardata.tab"),
    sep = "\t", row.names = FALSE
  )
  utils::write.table(
    dictionary,
    file.path(path, "evidence", "benchmarks", "attitude-indices.tab"),
    sep = "\t", row.names = FALSE
  )
  source_manifest <- data.frame(
    file = c("polardata.tab", "attitude-indices.tab"),
    sha256 = vapply(
      file.path(path, "evidence", "benchmarks", c("polardata.tab", "attitude-indices.tab")),
      digest::digest, "", algo = "sha256", file = TRUE
    )
  )
  x <- read_distortions(path, source_manifest)
  withr::local_envvar(c(DP_DATA_ROOT = path))
  expect_identical(read_distortions(source_manifest = source_manifest)$tables, x$tables)
  expect_error(read_distortions(path), "checksum mismatch")
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
  utils::write.table(
    d,
    file.path(path, "evidence", "benchmarks", "polardata.tab"),
    sep = "\t", row.names = FALSE
  )
  expect_error(read_distortions(path, source_manifest), "checksum mismatch")
  source_manifest$sha256[1] <- digest::digest(
    file.path(path, "evidence", "benchmarks", "polardata.tab"),
    algo = "sha256", file = TRUE
  )
  expect_error(read_distortions(path, source_manifest), "Conflicting source person IDs")
  expect_error(read_distortions(path, source_manifest[c(1, 1), ]), "unique checksum")
  unlink(file.path(path, "evidence", "benchmarks", "polardata.tab"))
  expect_error(read_distortions(path, source_manifest), "Missing dp-data")
})

test_that("HTML rendering works offline and evidence inclusion is explicit", {
  for (pkg in c("rmarkdown", "knitr", "bslib", "ggplot2", "gt", "DT", "htmltools")) {
    skip_if_not_installed(pkg)
  }
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
