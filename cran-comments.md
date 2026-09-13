## Candidate status

Initial development candidate; not submitted to CRAN.

## Local checks

macOS and Linux (rocker/r2u): 0 errors, 0 warnings, 0 notes from the built source
package. Local full tests: 160 passed, no failures, warnings or skips, including
the NOT_CRAN simulation tier. The CRAN tier skips that 1,000-replication test.

## Data and examples

All shipped participant data are synthetic. Website-only real-data reports use
public source files pinned by commit and SHA-256 and are excluded from the source
tarball. Package examples and vignettes need no network or API credentials.
An optional bootstrap script uses an external backend; it is not a dependency of
the installed package or its examples/tests.

## Remaining release gates

Independent candidate review, hosted platform checks, final URL checks and full
manual/as-CRAN checking are recorded in audit/ before any submission decision.
