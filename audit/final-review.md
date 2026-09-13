# Final narrow CR2 repair review

## Identity and verdict

- Package: `deliberately` 0.1.0
- Shared repository: `/Users/soodoku/Documents/GitHub/deliberately`
- Final candidate: `2fdeb2906f869a91931c5f167be5bc19b0a29440`
- Base: `70cb2aaaff6f135b954a8c5c10e084f20c1e1d65`
- Fresh isolated review tree: `/private/tmp/deliberately-final.YlJoYo`
- Isolation method: `git archive 2fdeb2906f869a91931c5f167be5bc19b0a29440 | tar -x -C /private/tmp/deliberately-final.YlJoYo`
- Shared source was not edited.

Verdict: clear in the requested scope. The numerical-zero CR2 covariance defect is repaired, the test distinguishes it from small nonzero cluster variance, and no unresolved material finding remains from the independent statistical reviews.

## Repair verification

`cluster_interval()` now checks the CR2 standard error after validating its finiteness and effective degrees of freedom:

```r
test$SE <= .Machine$double.eps * max(abs(d$change))
```

When this holds, the result is unassessable with the reason `Cluster-robust variance is numerically zero; uncertainty is unassessable.` This is a scale-aware machine-precision guard, not a broad small-SE rule. In package outcomes, normalized change is finite and bounded, and the earlier residual-degeneracy check handles the all-zero-change case.

### Exact zero-variance case

Trigger: the original synthetic knowledge outcome with two assignment-defined sites whose site mean changes are identical.

Before the guard, the backend roundoff was reported as SE `2.943923360032078e-17`, df 1, an essentially point-mass interval around 0.5, and p-value `3.7483196386624532e-17`.

The fresh isolated candidate now returns:

| Field | Verified result |
|---|---|
| estimate | `NA` |
| SE | `NA` |
| df | `NA` |
| interval | `NA`, `NA` |
| p-value | `NA` |
| clusters | 2 |
| status | `unassessable` |
| reason | numerical-zero cluster-robust variance |

This is the correct disposition: positive individual residual variation cannot substitute for absent cluster-level repeated-sampling variation.

### Discriminating nonzero control

The control uses six clusters with cluster shifts of order `1e-4` and within-cluster residual variation. It remains estimable:

| Field | Verified result |
|---|---:|
| estimate | 0.10035000000000006 |
| SE | 0.00007637626158258710 |
| df | 4.999999999999997 |
| lower | 0.10015366856930205 |
| upper | 0.10054633143069808 |

The finite result is many orders of magnitude above the machine-scale threshold. The guard therefore rejects floating-point zero without suppressing substantively small, nonzero covariance.

## Regression evidence

The pre-fix log `/private/tmp/deliberately-variance-before.log` shows exactly three failures for the new case:

1. status was `computed` instead of `unassessable`;
2. p-value was nonmissing instead of `NA`;
3. the numerical-zero reason was absent.

Against `2fdeb2906f869a91931c5f167be5bc19b0a29440`, `tests/testthat/test-cluster.R` passes all 29 assertions. The new block contributes four assertions: unassessable status, missing p-value, explicit reason, and a finite SE for the nonzero control.

## Documentation and records

The methods vignette now states the exact tolerance and its interpretation: individual residual variation cannot replace between-cluster variation. `audit/verification.md` records the old numerical output, pre-fix regression failure, repaired 168-assertion suite, and full check outcome. CRAN comments update the suite count from 164 to 168. The historical repair review is retained unchanged as evidence of the finding that prompted this fix.

## Independent commands and results

| Command | Result |
|---|---|
| `git diff 70cb2aa 2fdeb2906f869a91931c5f167be5bc19b0a29440` | 6 changed files reviewed |
| `Rscript -e 'pkgload::load_all(...); testthat::test_file("tests/testthat/test-cluster.R")'` | pass, 29 assertions |
| `Rscript -e 'pkgload::load_all(...); lintr::lint_package()'` | pass, 0 lints |
| direct original two-site knowledge reproduction | unassessable, all inferential fields NA, 2 clusters, explicit reason |
| direct small-nonzero control | finite estimate, SE, df, interval, and p-value |
| `/private/tmp/deliberately-variance-before.log` inspection | confirms 3 intended failures before guard |

The parent-provided final harness evidence reports 168 native assertions, an isolated Docker pass, built-package check 0/0/0, and full as-CRAN check including manuals at 0/0/0. Those expensive unchanged gates were not repeated in this narrow review, as requested.

## Rejected concerns

- **The guard rejects every result with very few clusters:** rejected. It tests numerical covariance magnitude, not cluster count.
- **A fixed absolute tolerance suppresses scale-dependent valid results:** rejected. The threshold scales with the maximum absolute observed change.
- **Small but genuine cluster variance is suppressed:** rejected by the explicit `1e-4` control.
- **The old extreme p-value remains somewhere in the emitted metric:** rejected. The failed interval attempt is converted to the package's all-NA inferential result with a reason.
- **Documentation hides the package-specific numerical rule:** rejected. The methods vignette states the exact rule and rationale.

## Final disposition

The assignment-cluster contract defect and numerical-zero CR2 covariance defect are both fixed and independently verified. The release-only spelling, workflow, renderer, documentation, and audit-record changes were cleared in the preceding review. No unresolved material finding remains within the audited release scope.
