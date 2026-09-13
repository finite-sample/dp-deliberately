test_that("CR2 results equal the reference software", {
  skip_if_not_installed("clubSandwich")
  d <- with_seed(
    71,
    data.frame(
      cluster = rep(letters[1:12], each = 5),
      change = rep(stats::rnorm(12), each = 5) + stats::rnorm(60),
      weight = 1,
      focal = rep(c(0, 1), 30)
    )
  )
  fit <- stats::lm(change ~ focal, data = d)
  ref <- clubSandwich::coef_test(
    fit,
    vcov = "CR2",
    cluster = d$cluster,
    test = "Satterthwaite",
    coefs = "focal"
  )
  got <- cluster_interval(d, .95)
  expect_equal(unname(got["estimate"]), ref$beta)
  expect_equal(unname(got["se"]), ref$SE)
  expect_equal(unname(got["df"]), ref$df_Satt)
  expect_equal(unname(got["p_value"]), ref$p_Satt)
  expect_equal(
    unname(got["lower"]),
    ref$beta - stats::qt(.975, ref$df_Satt) * ref$SE
  )
  d$focal <- 0
  expect_true(is.finite(cluster_interval(d, .95)["estimate"]))
  d$cluster <- "only"
  expect_error(cluster_interval(d, .95), "two independent")
})

test_that("cluster declarations and families have explicit domains", {
  expect_error(audit_config(weighted = 1), "logical")
  expect_error(audit_config(probability_sample = NA), "logical")
  expect_error(audit_config(seed = NA), "Seed")
  expect_error(cluster_inference("group_id", "", "because"), "nonempty")
  expect_error(
    audit_config(test_families = list(a = "x", b = "x")),
    "nonoverlapping"
  )
  m <- metric("test", "a", 0, "e", p_value = .02)
  m <- dplyr::bind_rows(m, transform(m, metric = "b", p_value = .04))
  expect_equal(
    adjust_families(m, list(family = c("a", "b")))$p_adjusted,
    c(.04, .04)
  )
  expect_error(adjust_families(m, list(family = "typo")), "Unknown")
})

test_that("the audit reports attrition and requires a clustering declaration", {
  x <- example_deliberation()
  spec <- cluster_inference(
    "group_id",
    "Comparable events",
    "Groups are independent"
  )
  m <- audit_dp(x, config = quick_config())$metrics
  expect_false(any(m$analysis == "cluster_inference"))
  m <- audit_dp(
    x,
    config = audit_config(permutations = 9, cluster = spec)
  )$metrics
  expect_true(any(m$analysis == "cluster_inference"))
  expect_true(all(m$n_clusters[m$analysis == "cluster_inference"] == 4))
  r <- x$tables$responses
  r$value[r$person_id == "p01" & r$wave_id == "after"] <- NA_real_
  x$tables$responses <- r
  m <- attrition_metrics(x, list())
  expect_true(all(m$estimate[m$group_id %in% "g1"] >= .25))
})


test_that("cluster contrasts exclude outsiders and use their own denominator", {
  x <- example_deliberation()
  ct <- define_contrast(
    "restricted",
    function(p) p$person_id == "p01",
    function(p) p$person_id == "p02",
    rationale = "Two-person domain"
  )
  ct <- resolve_contrasts(x, ct)
  spec <- cluster_inference(
    "group_id",
    "Comparable groups",
    "Independent groups"
  )
  m <- cluster_metrics(x, ct, audit_config(cluster = spec))
  domain <- m[m$contrast_id %in% "restricted", ]
  expect_true(all(domain$n == 2))
  expect_true(all(domain$denominator == 2))
  expect_true(all(domain$coverage == 1))
})
