## Candidate status

Initial development candidate; not submitted to CRAN and no release tag created.

## Local checks

macOS full `devtools::check(remote=TRUE, manual=TRUE)`: 0 errors, 0 warnings,
1 NOTE: “New submission,” expected because this is a new package.

Ordinary built-package checks on macOS and Linux (rocker/r2u) passed 0/0/0.
The final Windows df-boundary repair was independently reviewed and passes the
macOS full suite: 168 assertions, no failures, warnings or skips. The CRAN tier
skips the 1,000-replication simulation; the canonical check and coverage workflows
explicitly run it with NOT_CRAN=true. Hosted Windows confirmation is required
before treating the candidate as ready for submission.

## Data and examples

All shipped participant data are synthetic. Website-only real-data reports use
public source files pinned by commit and SHA-256 and are excluded from the source
tarball. Package examples and vignettes need no network or API credentials.
An optional bootstrap script uses an external backend; it is not a dependency of
the installed package or its examples/tests.

## Reverse dependencies and links

`devtools::revdep()` returned none. `urlchecker::url_check()` passed all 10 URLs.
Spelling and rendered documentation were checked locally. Independent review
reports and dispositions are in audit/.

## Remaining submission gates

Final hosted checks are tracked on GitHub Actions. Automatic approval review
blocked the separate win-builder upload; explicit approval has been requested.
No win-builder result or CRAN submission is claimed.
