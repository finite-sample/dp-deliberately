test_that("CR2 mean intervals recover known truth under independent clusters", {
  skip_on_cran()
  skip_if_not_installed("clubSandwich")
  truth <- .03
  sims <- with_seed(
    20260912,
    replicate(1000, {
      d <- data.frame(
        cluster = rep(seq_len(20), each = 6),
        focal = 0,
        weight = 1,
        change = truth +
          rep(stats::rnorm(20, sd = .02), each = 6) +
          stats::rnorm(120, sd = .03)
      )
      z <- cluster_interval(d, .95)
      c(
        estimate = unname(z["estimate"]),
        covered = as.numeric(z["lower"] <= truth && z["upper"] >= truth),
        rejected = as.numeric(z["lower"] > truth || z["upper"] < truth)
      )
    })
  )
  bounds <- stats::qbinom(c(.001, .999), 1000, .95) / 1000
  expect_gte(mean(sims["covered", ]), bounds[1])
  expect_lte(mean(sims["covered", ]), bounds[2])
  expect_lt(abs(mean(sims["estimate", ]) - truth), .001)
  expect_equal(mean(sims["rejected", ]), 1 - mean(sims["covered", ]))
})
