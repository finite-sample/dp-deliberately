# Independent release-candidate review

## Identity and verdict

- Package: `deliberately` 0.1.0
- Shared repository: `/Users/soodoku/Documents/GitHub/deliberately`
- Candidate: `7d6c4e2b2737795e2d534bf160552c175956f7e2`
- Baseline: `5cb530da869391080e0c32a898873e542d9e2f6e`
- Isolated review tree: `/private/tmp/deliberately-candidate.1ZXVjD`
- Isolated R cache: `/private/tmp/deliberately-cache.sKWyiS`
- Isolation method: `git archive 7d6c4e2 | tar -x -C /private/tmp/deliberately-candidate.1ZXVjD`

The candidate passes its full release gate and the audited baseline defects are repaired. One concrete public-contract defect remains: documented assignment-table cluster columns are not made available to cluster inference. The implementation should join them or the public contract must be narrowed before release. No shared source file was edited.

## Verified finding

### Assignment-table cluster columns are documented but ignored

Location: `R/cluster.R`, `cluster_metrics()`, especially the resolution of `extra <- setdiff(cols, names(d))` and the subsequent join from `people` only. The public `cluster_inference()` documentation says cluster columns may be in “assignments/outcomes or people.”

Trigger:

```r
x <- example_deliberation()
x$tables$assignments$site_id <- rep(c("a", "b"), each = 8)
spec <- cluster_inference("site_id", "sites", "independent")
m <- audit_dp(
  x,
  config = audit_config(permutations = 9, cluster = spec)
)$metrics
m[m$analysis == "cluster_inference",
  c("metric", "estimate", "n_clusters", "reason")]
```

Observed for both items:

```text
metric                 estimate  n_clusters  reason
cluster_mean_change    NA        NA          Missing declared cluster identifiers.
```

Cause: `paired_responses()` carries the specially derived `group_id` from `assignments$actual_group`, but not arbitrary assignment columns. `cluster_metrics()` joins missing cluster columns only when all are present in `people`.

Expected contract: join declared assignment fields on `event_id`, `episode_id`, and `person_id`, with the existing many-to-one relationship guarantee, then form the composite cluster. For the trigger, `n_clusters` should resolve to 2; inference may still be unassessable if effective degrees of freedom are insufficient, which would be the correct statistical disposition. If assignment columns are intentionally unsupported, remove “assignments” from the public documentation, though this would exclude a natural location for randomized group/site identifiers.

Required discriminating tests:

- A cluster column present only in assignments resolves and reports the correct cluster count.
- A people-only cluster column continues to resolve.
- A composite declaration spanning supported sources either joins explicitly or fails at the public boundary with a documented incompatibility.
- Missing assignment cluster values return the existing missing-identifier reason.

## Statistical review

### Cluster inference

- The estimands match the documentation: an intercept-only weighted model estimates paired mean change; a focal indicator estimates focal-minus-reference paired change after excluding outsiders.
- CR2 covariance, Satterthwaite degrees of freedom, coefficient p-value, and t critical value are obtained from `clubSandwich` without substituting normal critical values.
- Fewer than two clusters, an unidentified focal contrast, numerically zero residual variance, nonfinite/less-than-one effective degrees of freedom, and missing cluster identifiers return unassessable results with reasons.
- `n`, denominator, coverage, `n_clusters`, and effective `df` are carried separately.
- Multiple declared columns form one composite cluster via collision-resistant length-prefixed row keys; the package does not claim multiway clustering.
- The 1,000-run known-truth simulation passed its prespecified binomial coverage bounds and estimate-bias tolerance.
- Interpretation is appropriately limited to a declared repeated-sampling model. It does not claim representative sampling or a causal effect.

The declaration's `target` and `rationale` are retained in the recorded configuration. Their truth and the independence of clusters remain user-supplied assumptions.

### Attrition

- `post_attrition` correctly uses observed baseline respondents as its risk set and counts unusable post scores among that set.
- Its denominator is the supplied response roster and its coverage is baseline-observed roster coverage, matching the methods vignette.
- Event, assigned-group, and declared focal/reference domains are emitted separately.
- Available-case change is correctly described as a composition sensitivity calculation rather than an attrition correction.
- The report explicitly states that paired analysis and robust standard errors do not establish ignorable attrition.

The package cannot recover people omitted from the response roster or identify missing-at-random behavior; these are correctly stated limitations.

### Multiplicity

- `test_families` requires named, nonempty, nonoverlapping metric-name vectors.
- Unknown metric names fail after the metric inventory exists.
- Holm adjustment pools all matching event, episode, item, and contrast rows, as documented.
- Raw p-values remain intact; adjusted values and family labels occupy separate fields.
- Unassessable rows remain `NA` and count in the declared family size through `stats::p.adjust`, a documented conservative choice.

Metric-name families are broad by design. Confirmatory validity still depends on declaring them before inspecting results and including every relevant outcome.

### Optional wild bootstrap

The installed candidate and locally available `fwildclusterboot` backend completed `inst/examples/wild-bootstrap.R`. The script:

- sets both base R and `dqrng` seeds;
- uses a null-imposed Rademacher bootstrap;
- fixes one thread;
- checks finite p-value and interval bounds, probability range, and point-estimate equality;
- saves data, CR2 output, bootstrap output, seed, package version, interpretation, and session information.

Observed output was finite: CR2 focal estimate approximately 0.0125 with Satterthwaite p approximately 0.0205; bootstrap p approximately 0.0241 and interval approximately [0.0020, 0.0228]. `fwildclusterboot` emitted its upstream notice about changed seeding behavior since version 0.13, but the example uses the current two-seed requirement and passed all assertions.

## Baseline repair verification

- Unscored knowledge metrics now report `n=0` and coverage 0 rather than paired-response counts. Reproduced event result: estimate `NA`, `n=0`, denominator 20, coverage 0; group results: `0/4/0`.
- A fully coded interruption-received ratio with one focal and three reference turns now reports estimate 1, `n=4`, denominator 4, coverage 1.
- Character `arguments$reviewed` now produces a structured `TYPE` validation row rather than crashing semantic validation.
- `audit_config()` rejects malformed logical flags, seeds, and predictor-name vectors at the public boundary.

## Import and validation review

- `read_deliberation()` now uses explicit readr column specifications, forces identifiers/group/PSU/stratum fields to character, preserves minimal names, and aborts on recorded parse problems.
- Leading-zero identifiers and CSV round trips pass the tests.
- Logical schema fields receive type checks before semantic operators run.
- Keys, foreign keys, allowed values, scales, intervals, response bounds, evidence targets, source spans, and provenance remain structured validation results.
- The assertr-backed row resolver preserves exact failing row indices in the regression tests and does not change the caller's random state.

External source semantics for the distortions adapter were not independently re-downloaded in this review. The pinned statistical workflow checks source results against commit `ffd7aafb017c039726d95d61e90f13ca4521e317` and the included verification script requires one-to-one keys, identical missingness, and maximum absolute error below `1e-10` for H, P, and D.

## Reporting review

- The result now retains event/item dictionaries needed for human labels while preserving raw IDs in the metrics.
- Summary text counts only computed diagnostics and includes explicit non-verdict/measurement qualifications.
- Headline summary, attrition, participation, argument, inference, capability, policy, contrast, reproducibility, and optional evidence sections use the producing metric fields rather than README text.
- Person-level rows are excluded from general report tables; raw evidence inclusion remains an explicit logical option and the R result intentionally retains supplied evidence.
- Raw and Holm-adjusted p-values are distinct, and absent families are labeled exploratory.
- Existing render tests passed. The parent separately reported browser checks at 390- and 1400-pixel widths with working filters and CSV export; those browser checks were not rerun in this isolated source review.

One nonblocking presentation issue remains: when no cluster declaration is supplied, the `cluster_inference` capability is `unassessable` with the generic reason “Tables present, but no eligible diagnostic units or declared predictors.” A declaration-specific reason would be clearer, but documentation and the absence of cluster metrics prevent a false inferential claim.

## Workflow review

- `actionlint .github/workflows/*.yml` passed all five workflow files.
- Reusable workflow references intentionally remain under `gojiplus/r-canon@v2`; the destination repository change does not require changing those references.
- All action references in the bespoke statistical workflow are immutable SHAs with current version comments.
- The statistical workflow checks out the package and the pinned distortions source into a separate subdirectory, installs local dependencies, verifies source values, installs the optional bootstrap backend from the stated repository, and runs the sensitivity example.
- The duplicate `actions/checkout` steps have distinct purposes and paths; the second does not replace the package checkout.
- No shell files are present in the candidate archive, so ShellCheck is inapplicable.
- `git diff --check 5cb530d 7d6c4e2` passed.

## Commands and counts

Primary gate:

```sh
NOT_CRAN=true \
R_USER_CACHE_DIR=/private/tmp/deliberately-cache.sKWyiS \
make ci
```

Results:

- `lintr::lint_package()`: 0 lints.
- `testthat::test_local(stop_on_failure=TRUE)`: 160 passed, 0 failed, 0 warnings, 0 skipped.
- Context counts: audit-regressions 7; cluster 21; contracts 14; data 21; dialogue 28; import-report 14; inference 17; outcomes 24; policy 10; simulation 4.
- `R CMD build .`: built `deliberately_0.1.0.tar.gz`, including vignettes.
- `rcmdcheck` on that tarball with `--no-manual`: 0 errors, 0 warnings, 0 notes.

Additional checks:

- `actionlint .github/workflows/*.yml`: pass.
- `git diff --check 5cb530d 7d6c4e2`: pass.
- Installed built tarball into `/private/tmp/deliberately-candidate.1ZXVjD-lib`: pass.
- `R_LIBS=/private/tmp/deliberately-candidate.1ZXVjD-lib Rscript inst/examples/wild-bootstrap.R`: pass.
- Direct adversarial reproductions of all four baseline repairs: pass.

The check attempted CRAN/Bioconductor index lookups while network access was restricted and printed repository-index warnings during dependency inspection; the installed dependency set was complete and the final built-package check status remained 0/0/0.

## Rejected concerns and untestable assumptions

- **Normal critical values used with CR2:** rejected; Satterthwaite t critical values are used.
- **Outsiders contaminate focal/reference contrasts:** rejected by source and denominator tests.
- **Available cases presented as attrition correction:** rejected; reports explicitly call this composition sensitivity.
- **Unassessable tests disappear from Holm family size:** rejected; they remain NA and count, as documented.
- **Bootstrap silently becomes the primary method:** rejected; it is a separate sensitivity example and artifact.
- **Malformed logical CSV values reach semantic operators:** rejected by structured type checks.
- **Interruption coverage remains a focal/reference ratio:** rejected by reproduced 4/4 result.
- **Release workflows point at the old destination repository:** rejected; reusable workflow ownership intentionally remains `gojiplus/r-canon`.

Untestable from this package alone: actual cluster independence; validity of the repeated-process target; representative sampling; truthful weights, strata, PSUs, FPCs, attendance and exhaustive-coding declarations; missing-at-random attrition; causal effects of deliberation; construct validity of annotations, inventories, midpoint/direction choices, and advantaged-reference designations; and external generalization beyond supplied events.
