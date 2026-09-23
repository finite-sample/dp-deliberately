# Independent repair and release-delta review

## Identity and verdict

- Package: `deliberately` 0.1.0
- Shared repository: `/Users/soodoku/Documents/GitHub/deliberately`
- Repair candidate: `70cb2aaaff6f135b954a8c5c10e084f20c1e1d65`
- Parent: `7d6c4e2b2737795e2d534bf160552c175956f7e2`
- Fresh isolated review tree: `/private/tmp/deliberately-repair.CuYN0G`
- Isolation: `git archive 70cb2aa | tar -x -C /private/tmp/deliberately-repair.CuYN0G`
- Shared source was not edited.

The assignment-column repair is correct, including composite clusters spanning assignment and people tables. The release-only spelling, R-hub, renderer-dependency, and documentation changes also pass review. One material inferential finding remains unresolved: theoretically zero cluster-robust variance is emitted as a machine-roundoff standard error, an essentially point-mass confidence interval, and an extreme p-value. This should be made unassessable before release, consistently with the existing numerical-zero residual guard.

## Assignment cluster repair

The repair resolves cluster names in a documented order:

1. fields already in derived paired outcomes, including `group_id`;
2. requested assignment fields joined by `event_id`, `episode_id`, and `person_id`;
3. remaining people fields joined by `event_id` and `person_id`.

Both joins declare `many-to-one` relationships. Only requested fields are joined, so unrelated names cannot unexpectedly shadow one another. `setdiff(cols, names(d))` at each step implements the documented precedence.

The regression test adds an assignment-only `site_id`, makes both outcome changes nondegenerate, and verifies two resolved clusters and finite estimates. It then adds a people-only `region` and verifies that the composite `(region, site_id)` declaration preserves estimates and cluster counts. The fixture change is necessary: the original support outcome has exactly constant individual change and is correctly unassessable.

Evidence:

- `/private/tmp/deliberately-assignment-before.log` contains the two expected pre-fix failures: missing cluster count and nonfinite estimate.
- The isolated repaired `test-cluster.R` passes 25 assertions.
- `test-import-report.R` passes 14 assertions.
- The supplied repaired full gate log `/private/tmp/deliberately-repair-ci3.log` records 164 passing assertions and a built-package check at 0 errors, 0 warnings, 0 notes.

## Remaining material finding: numerically zero CR2 covariance

Trigger against `70cb2aa`:

```r
x <- example_deliberation()
x$tables$assignments$site_id <- rep(c("a", "b"), each = 8)
config <- audit_config(
  permutations = 9,
  cluster = cluster_inference("site_id", "sites", "independent")
)
deliberately:::cluster_metrics(x, list(), config)
```

For the original knowledge outcome, both sites have identical mean changes. The declared cluster-level repeated-sampling variation is therefore exactly zero. Individual residual RSS is positive, so the current pre-CR2 residual guard does not fire. Floating-point output from the CR2 backend is then reported as substantive inference:

| Field | Current value |
|---|---:|
| estimate | 0.5 |
| SE | 2.943923360032078e-17 |
| Satterthwaite df | 1 |
| lower | 0.49999999999999961 |
| upper | 0.50000000000000033 |
| p-value | 3.7483196386624532e-17 |
| clusters | 2 |

This is not merely a small-sample limitation. The estimated cluster-robust covariance is theoretically zero under the declared independent-cluster model; the reported SE is numerical roundoff. The near point-mass interval and extreme p-value falsely imply extraordinary identifying information from two clusters.

Disposition: material blocker. Add a scale-aware post-CR2 numerical-zero variance/SE guard and return an unassessable result with an explicit cluster-level variance reason. The tolerance should distinguish theoretical or numerical zero from legitimately small nonzero variance using the scale of the coefficient/design/outcome rather than a fixed absolute cutoff. Add a regression for identical cluster mean/score contributions and a nearby nonzero control above tolerance.

This treatment is consistent with the current decision to make numerically zero model residual variance unassessable. Within-cluster residual variation cannot rescue absence of between-cluster identifying variation for a cluster-robust repeated-sampling claim.

## Release-only delta

### Spelling test

`tests/spelling.R` follows the standard `spelling::spell_check_test()` pattern, includes vignettes, does not turn findings into an uncontrolled hard failure, and skips on CRAN. The independently built tarball ran both `spelling.R` and `testthat.R` during `R CMD check`; final status was 0/0/0.

### R-hub workflow

`.github/workflows/rhub.yaml` matches the generated R-hub v1 workflow structure. Every R-hub action is pinned to `cf6c8eff3145ef225104294e6b0c023de4bba9c6`. An official remote tag check returned the same SHA for `refs/tags/v1`. The workflow has read-only contents permission, supports manual platform selection, separates container and hosted platforms, and passes local actionlint.

### Optional renderer guards

The `render_audit()` example and render test now check all optional packages used by the report: `rmarkdown`, `knitr`, `bslib`, `ggplot2`, `gt`, `DT`, and `htmltools`, plus Pandoc availability. This aligns examples/tests with the function's own dependency checks and avoids partial-environment failures. README names the same renderer dependency set and gives the updated `finite-sample/dp-deliberately` installation location.

### Documentation and audit records

The cluster documentation now states source precedence and exact join keys. Generated Rd matches source. Verification and CRAN comments correctly update the suite count to 164 and distinguish local, hosted, optional-backend, and pending external checks. Embedded independent audit records reproduce the earlier reports; machine-specific temporary paths appear only as evidence in those audit records, not as package usage instructions.

## Independent commands and results

| Command | Result |
|---|---|
| `Rscript -e 'pkgload::load_all(...); lintr::lint_package()'` | pass, 0 lints |
| `testthat::test_file("tests/testthat/test-cluster.R")` | pass, 25 assertions |
| `testthat::test_file("tests/testthat/test-import-report.R")` | pass, 14 assertions |
| `actionlint .github/workflows/*.yml` | pass, including `rhub.yaml` |
| `git ls-remote https://github.com/r-hub/actions.git refs/tags/v1` | official v1 resolves to pinned `cf6c8eff...` |
| `R CMD build .` | pass; vignettes built |
| `rcmdcheck` on `deliberately_0.1.0.tar.gz`, `--no-manual` | pass, 0 errors, 0 warnings, 0 notes; spelling and testthat both ran |
| direct assignment/composite reproduction | repaired behavior verified by affected tests |
| direct zero-cluster-variance reproduction | finding reproduced with exact values above |

The independently run package check printed repository-index warnings because sandbox network access was restricted, but dependency inspection completed and the final check result remained 0/0/0.

## Rejected concerns

- **Assignment join can multiply response rows:** rejected; validation establishes assignment keys and the join explicitly requires many-to-one.
- **People columns override same-named assignment fields:** rejected; only still-missing fields are joined, matching documented precedence.
- **Composite cluster columns cannot span tables:** rejected by the new regression.
- **R-hub actions float on a mutable tag:** rejected; every use is pinned to the official v1 commit SHA.
- **Spelling test is absent from built-package checks:** rejected by independent tarball check output.
- **Report examples can run with only rmarkdown and fail on later dependencies:** rejected by the complete dependency guard.
- **Two clusters alone always require rejection:** rejected. Cluster count alone is not the finding; the demonstrated issue is zero estimated cluster-level covariance being represented by floating-point noise.

## Final disposition

Assignment-cluster defect: fixed and independently verified.

Release-only delta: clean in reviewed scope.

Unresolved material finding: numerical-zero CR2 covariance must become unassessable and receive a discriminating regression before the release candidate is clear.
