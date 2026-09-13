library(deliberately)
if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
  stop(paste(
    "Install fwildclusterboot from https://s3alfisc.r-universe.dev to",
    "run this optional example."
  ))
}
set.seed(20260912)
d <- data.frame(
  cluster = rep(seq_len(30), each = 8),
  focal = rep(c(0, 1), 120)
)
d$change <- .02 +
  .015 * d$focal +
  rep(rnorm(30, sd = .025), each = 8) +
  rnorm(nrow(d), sd = .04)
fit <- lm(change ~ focal, data = d)
cr2 <- clubSandwich::coef_test(
  fit,
  vcov = "CR2",
  cluster = d$cluster,
  test = "Satterthwaite",
  coefs = "focal"
)
set.seed(20260912)
dqrng::dqset.seed(20260912)
wild <- fwildclusterboot::boottest(
  fit,
  param = "focal",
  clustid = "cluster",
  B = 9999,
  impose_null = TRUE,
  conf_int = TRUE,
  type = "rademacher",
  engine = "R",
  nthreads = 1
)
stopifnot(
  is.finite(wild$p_val),
  length(wild$conf_int) == 2L,
  all(is.finite(wild$conf_int)),
  wild$p_val >= 0,
  wild$p_val <= 1,
  abs(unname(coef(fit)["focal"]) - wild$point_estimate) < 1e-10
)
dir.create("artifacts", showWarnings = FALSE)
saveRDS(
  list(
    data = d,
    cr2 = cr2,
    wild = wild,
    seed = 20260912,
    package_version = as.character(packageVersion("fwildclusterboot")),
    interpretation = paste(
      "Difference in paired mean changes under independent clusters; not",
      "a treatment effect"
    ),
    session = sessionInfo()
  ),
  "artifacts/wild-bootstrap.rds"
)
print(cr2)
print(wild)
