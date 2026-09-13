# Independent Windows df-boundary repair review

## Identity and verdict

- Package: `deliberately` 0.1.0
- Shared repository: `/Users/soodoku/Documents/GitHub/deliberately`
- Candidate: `8554d5a9094e3ce2b2919fb236dae8655fce7bcd`
- Base: `a332a0d3f2e8314d0b46b238fd8c41e8e9ffee93`
- Fresh isolated review tree: `/private/tmp/deliberately-windows.HRItKS`
- Isolation: `git archive 8554d5a9094e3ce2b2919fb236dae8655fce7bcd | tar -x -C /private/tmp/deliberately-windows.HRItKS`
- Shared source was not edited.

Verdict: clear in the requested scope. The df-boundary change is mathematically narrow, ordered correctly with the finite and variance guards, and preserves the prior numerical-zero covariance repair. Cluster tests and lint pass. Documentation, spelling vocabulary, and the accessible publication link agree with the implementation. No unresolved material finding remains; new hosted Windows runs are the decisive cross-platform confirmation.

## Windows failure evidence

Both `/private/tmp/deliberately-windows-job.log` and `/private/tmp/deliberately-rhub-windows.log` reproduce the same two failures from the prior candidate:

1. the nondegenerate two-cluster assignment/composite fixture became unassessable;
2. the theoretical zero-covariance fixture stopped at `Insufficient effective degrees of freedom` rather than reaching the numerical-zero variance guard.

The standard hosted Windows release reported 166 passing assertions plus these two failures. R-hub Windows R-devel reported 162 passing assertions, one expected CRAN simulation skip, and the same two failures. Their identical failure shape supports a platform floating-point boundary issue rather than a distinct workflow or dependency failure.

## Guard order and numerical contract

The candidate applies checks in this order after `clubSandwich::coef_test()`:

1. Reject nonfinite Satterthwaite df.
2. Reject df below `1 - sqrt(.Machine$double.eps)`.
3. Reject nonfinite SE.
4. Clamp surviving df to `max(1, df)`.
5. Reject numerical-zero CR2 SE using the unchanged scale-aware tolerance.
6. Compute the t critical value and interval with the clamped df.
7. Return the backend coefficient, SE, and p-value, plus the clamped df and interval.

This order is correct. A theoretical df of one that rounds downward by platform arithmetic survives and reaches the substantive covariance check. A nonfinite df or SE cannot pass. A materially sub-one effective df remains unassessable. The interval uses the contractual boundary value of one rather than a platform-specific value infinitesimally below it.

The tolerance is approximately `1.49e-8` below one. It is narrow relative to the scale on which degrees of freedom have substantive meaning, while being deliberately wider than ordinary accumulated floating-point error. It does not admit values meaningfully below one. The backend p-value is not recomputed, appropriately preserving `clubSandwich`'s result; its use of a df differing from one only by boundary roundoff cannot materially differ from the clamped interval calculation.

The existing covariance guard remains unchanged:

```r
test$SE <= .Machine$double.eps * max(abs(d$change))
```

On the isolated local platform, the original synthetic fixture still gives the intended dispositions: constant support change is unassessable for numerical-zero residual variance, and equal-site knowledge change is unassessable for numerical-zero cluster-robust variance. All estimate, SE, df, interval, and p-value fields remain `NA` for those degenerate cases.

## Tests and lint

Independent commands in the fresh archive:

| Command | Result |
|---|---|
| `testthat::test_file("tests/testthat/test-cluster.R")` after `pkgload::load_all()` | pass, 29 assertions |
| `lintr::lint_package()` after `pkgload::load_all()` | pass, 0 lints |
| direct two-site synthetic reproduction | correct residual-zero and cluster-covariance-zero reasons; inferential fields NA |
| inspection of both hosted Windows failure logs | same two pre-repair failures confirmed |

The parent-provided `/private/tmp/deliberately-windows-repair-ci2.log` records the complete local suite at 168 passed, 0 failed, 0 warnings, 0 skips and the built-package check at 0 errors, 0 warnings, 0 notes. The narrow review did not repeat unchanged expensive package and simulation gates.

## Documentation and release text

- `vignettes/methods.Rmd` states that df within the square root of machine epsilon below one is rounded to one and explains that this prevents platform roundoff from changing assessability at the two-cluster boundary.
- `audit/verification.md` accurately records the two hosted failures, the tolerance and clamp, the unchanged local 168/0/0/0 suite, and the need for hosted Windows confirmation.
- `assessability` is added to `inst/WORDLIST`, matching the reviewed prose term.
- README now links Fishkin's design criteria to the Stanford Deliberative Democracy Lab publication record. Independent page retrieval succeeded and the page identifies the correct article, author, journal, date, and DOI `10.1002/hast.1316`.

## Rejected concerns

- **The repair admits materially sub-one df:** rejected; only values at least approximately 0.999999985 survive.
- **The zero-covariance result becomes computed after df clamping:** rejected by direct reproduction and guard order.
- **Nonfinite SE can reach the numerical comparison:** rejected; finiteness is checked first.
- **Intervals remain platform-dependent at the boundary:** rejected; surviving df is explicitly clamped before `qt()`.
- **The p-value is silently replaced by a hand calculation:** rejected; the backend Satterthwaite p-value remains intact.
- **The publication link points to a different work:** rejected; the accessible Stanford record identifies Fishkin's article and the same DOI.

## Residual verification

The local platform returns the theoretical df boundary without the Windows downward rounding, so local execution cannot directly reproduce the repaired Windows arithmetic. Source inspection establishes the intended behavior for any finite df in `[1 - sqrt(eps), 1)`, and the existing two Windows systems independently reproduced the exact old branch error. New hosted Windows release and R-hub runs should be required before publication is considered cross-platform green.

No unresolved material code or statistical finding remains in this delta.
