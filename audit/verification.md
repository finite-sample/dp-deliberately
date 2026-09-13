# Candidate verification

This is a development candidate for 0.1.0. No CRAN submission or release tag is
part of this change. Source data remain in the separate distortions repository.

## Local evidence

- `NOT_CRAN=true make ci`: 160 assertions passed; no failures, warnings or skips.
  Built source package: R CMD check, zero errors, warnings and notes.
- `make ci-docker` with `rocker/r2u:latest`: lint, tests and built-package check
  passed with zero errors, warnings and notes.
- `covr::package_coverage()`: 91.33% line coverage. Coverage is diagnostic and is
  not a correctness certificate.
- The 1,000-replication CR2 simulation passed prespecified Monte Carlo coverage
  bounds and a bias check under independent, normally distributed clusters.
- `Rscript inst/examples/verify-distortions.R`: all source keys and NA masks
  matched. H: 2,480 comparisons; P: 2,480; D: 2,437. Maximum finite absolute
  difference was 7.22e-16. Source commit:
  `ffd7aafb017c039726d95d61e90f13ca4521e317`.
- Optional wild cluster bootstrap example passed with `fwildclusterboot` 0.14.3,
  9,999 null-imposed Rademacher draws and the R engine. Estimate 0.0125;
  CR2 p = 0.0205; bootstrap p = 0.0241 and CI approximately [0.002, 0.0228].
  Both R and dqrng seeds are set. This is a simulation example, not a DP result.
- `pkgdown::build_site()` completed, generating five standalone reports.
- `actionlint`, package lint, `git diff --check` and spelling review passed.
  The word list contains reviewed domain terms and package names.

## Baseline audit dispositions

The independent baseline review is retained in `baseline-review.md`.

| Finding | Disposition and evidence |
|---|---|
| Unscored knowledge had positive usable counts | Fixed: usable paired counts require finite scored values; regression failed before the fix and passes after it |
| Interruption-ratio coverage used focal/reference counts | Fixed: coding completeness uses coded turns over eligible turns in both contrast groups; regression failed before the fix and passes after it |
| Invalid configuration flags, seeds and predictor specifications were accepted | Fixed at the public boundary with regression tests |
| Character values in logical schema fields reached semantic checks | Fixed with explicit TYPE issues; regression failed before the fix |
| Direct source-directory check lacked generated Author/Maintainer fields | Refuted as an invalid release harness; built-tarball checks pass |

A subsequent review of the new cluster path found that an outside-contrast label
could enter the reference category and dilute coverage. It now restricts to the
focal and reference membership domain before selecting complete pairs. A domain
regression verifies usable n, denominator and coverage.

## Scope of the statistical evidence

CR2 tests verify reference-software agreement and performance under a declared
simulation, not robustness to every assignment mechanism. Randomization tests
require the recorded fixed-size mechanism and full paired roster. Missingness,
coding validity, argument inventory quality and independence remain substantive
assumptions. See the design-review and methods articles for claim limits.

Independent candidate review, hosted checks and final artifact checks are recorded
separately once completed. This record alone does not assert release readiness.
