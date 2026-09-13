# Independent review: r-canon workflow-reference validation

## Identity and isolation

- Shared repository inspected: `/Users/soodoku/Documents/GitHub/r-canon`
- Fix commit: `604ded50f02e7dc09387fe00fa5095732ec78b89`
- Parent/base: `10af4b97ca2a6a27cc4c742ce315adb781f64621`
- Isolated read-only review tree: `/private/tmp/r-canon-review.xrjdPH`
- Isolation method: `git archive 604ded5 | tar -x -C /private/tmp/r-canon-review.xrjdPH`
- Shared source was not edited.

## Verdict

No blocking or material correctness defect was found in the scoped change. The implementation parses the workflow document, inspects job-level `uses` declarations, requires exact equality with the canonical reusable-workflow path and `@v2`, distinguishes a wrong canonical pin from a noncanonical reference, and returns an explicit status for malformed YAML. The four added assertions discriminate the former text-search implementation from the fixed behavior.

The CI dependency addition is sufficient for the new test and runtime path: the tools job installs both `yaml` and `testthat` before running the test suite and before invoking `drift.R`. The reusable lint consumer was consistently advanced from `v1` to `v2`.

## Source review

### Exact workflow-reference validation

`references(path, kind)` now:

1. Resolves the expected workflow filename from `wanted`.
2. Returns `missing` when that file is absent.
3. Requires the `yaml` namespace with an actionable installation message.
4. Parses with `yaml::read_yaml(..., eval.expr = FALSE)`.
5. Reads only `workflow$jobs` and only scalar character `job$uses` values.
6. Returns `ok` only for exact equality with `gojiplus/r-canon/.github/workflows/<expected>@v2`.
7. Returns `unpinned` for another `@...` revision of that exact canonical workflow path.
8. Returns `not-canon` otherwise.

This rejects the two demonstrated false positives: a canonical-looking string in a comment and `@v20` satisfying an `@v2` substring search. Step-level `uses` declarations cannot satisfy the check because only job-level declarations are inspected, which matches reusable-workflow syntax.

The same function serves check, pkgdown/Sphinx, coverage, link-check, and lint workflow kinds. The new unit fixture directly exercises `check`; the remaining kinds differ only through the existing `wanted` and `reusable` lookup tables.

### YAML error handling

Parser errors are caught and converted to `invalid-yaml`, so one malformed consumer workflow remains a drift result instead of aborting the entire fleet scan. Empty YAML, non-list documents, absent `jobs`, and jobs without scalar character `uses` fall through to `not-canon` without being mistaken for valid references.

Dependency absence intentionally remains a fatal setup error with `Install yaml to inspect workflows.` This is reasonable because missing tooling affects the entire scan and is now documented in README and installed in CI.

### Tests

The added test has four assertions:

| Case | Expected | Verified |
|---|---|---|
| Reference appears only in a comment | not `ok` | yes |
| Actual job reference ends in `@v20` | not `ok` | yes |
| Actual job reference exactly ends in `@v2` | `ok` | yes |
| Malformed YAML | `invalid-yaml` | yes |

The test evaluates all top-level definitions in `tools/drift.R` except its final noninteractive entry point, avoiding an accidental fleet scan while exercising the production function.

### CI and documentation

- `.github/workflows/ci.yml` installs `yaml` and `testthat`, then runs `testthat::test_dir("tests/testthat")` in the tools job.
- The dependency installation precedes all later `drift.R` use in that job.
- The existing lint job continues to run the canonical `.lintr` against `tools/`.
- `.github/workflows/reusable-lint.yml` now checks out `r-canon` at `v2`, consistent with the changed standard.
- README and CHANGELOG disclose the new `yaml` runtime dependency and exact-reference behavior.

## Commands and results

| Command | Result |
|---|---|
| `git rev-parse 604ded5 604ded5^` in the shared repository | confirmed fix and base identities above |
| `git diff 604ded5^ 604ded5` | 6 files, 51 insertions, 6 deletions reviewed |
| `Rscript -e 'testthat::test_dir("tests/testthat", reporter="summary")'` | pass; 1 test block, 4 assertions |
| `Rscript -e 'lints <- lintr::lint_dir("tools"); print(lints); quit(status=as.integer(length(lints)>0))'` | pass; 0 lints |
| `shellcheck tools/*.sh` | pass; ShellCheck 0.11.0; one shell script (`tools/adopt.sh`) checked |
| `actionlint .github/workflows/*.yml` | pass; actionlint 1.7.12; 9 workflow files checked |

An initial attempt to run bare `actionlint` returned its expected “no project found” result because a `git archive` contains no `.git` directory. Re-running it with the same explicit workflow glob used by CI passed. An initial guessed command `Rscript tools/lint-tools.R` failed because that file does not exist; the actual CI lintr invocation was then run and passed. Neither initial command indicates a source defect.

## Findings and dispositions

No verified defect survives review.

- **Concern: substring false positives remain.** Rejected. Parsed scalar job references use exact equality for success.
- **Concern: malformed YAML aborts the fleet scan.** Rejected. Parser errors return `invalid-yaml`.
- **Concern: comments or step actions can satisfy the reusable-job check.** Rejected. YAML comments disappear during parsing and only `jobs.*.uses` is inspected.
- **Concern: the test does not exercise every workflow kind.** Accepted as a coverage observation, not a defect. All kinds share the same function and differ through fixed mappings; the four assertions cover the changed logic. A small table-driven test across kinds could protect mapping regressions, but is not required for this fix.
- **Concern: `yaml` is used without CI installation or documentation.** Rejected. CI installs it before tests and drift execution; README and CHANGELOG disclose it.
- **Concern: reusable lint still compares consumers to the old standard.** Rejected. Its checkout reference changes from `v1` to `v2`.

## Residual limits

- The test checks syntactic reference identity, not whether tag `v2` currently resolves remotely to the intended commit. That is outside the local drift check's contract.
- The unit test covers parser outcomes without running a full multi-repository drift scan. Existing CI subsequently exercises `adopt.sh` and `drift.R` against a real temporary Git fixture.
- CRAN package resolution is not locked to an exact `yaml` or `testthat` version. This follows the repository's existing CI dependency style and did not produce a verified incompatibility.
