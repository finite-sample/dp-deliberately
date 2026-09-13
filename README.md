# deliberately

**Who spoke, who was heard, and what changed?**

`deliberately` is an R package for auditing Deliberative Polls and related
deliberative processes. It connects design principles to observable diagnostics
and makes the requirements for each analysis explicit. Survey-only data still
produce a useful audit; missing transcripts remain missing evidence.

The package covers sample and assignment checks, participation, argument and
discourse coding, knowledge, opinion change, and signed D/P/H measures. It returns
ordinary data frames and a standalone HTML report. It does not assign a composite
quality score or infer disadvantaged groups from personal characteristics.

[Report gallery](https://finite-sample.github.io/deliberately/articles/report-gallery.html) ·
[Design review](https://finite-sample.github.io/deliberately/articles/design-review.html)

## Install and run

Install the development version, including the report dependencies:

```r
remotes::install_github("finite-sample/deliberately", dependencies = TRUE)
```

Or from a local checkout:

```sh
make deps
R CMD INSTALL .
```

```r
library(deliberately)

x <- example_deliberation()
validate_data(x)
available_analyses(x)

education <- define_contrast(
  "education",
  focal = function(p) p$education == "lower",
  reference = function(p) p$education == "higher",
  rationale = "Educational disadvantage as defined for this event.",
  reference_advantaged = TRUE
)

result <- audit_dp(x, contrasts = education)
result$metrics
render_audit(result, "audit.html")
```

Rendering uses `rmarkdown`, `knitr`, `bslib`, `ggplot2`, `gt`, `DT`, `htmltools`,
and Pandoc; RStudio includes Pandoc.
Sampling inference additionally uses `survey`. Typed inputs and validation use `readr`, `dplyr` and `assertr`.
No API key or model service is required.

## Given X, produce Y

| Inputs | Diagnostics |
|---|---|
| Participant roster + benchmarks | Marginal demographic and baseline-attitude differences |
| Recruitment outcomes + declared baseline predictors | Attendance selection differences and out-of-fold AUC |
| Intended/actual assignments + baseline attributes | Assignment balance and deviations |
| Attendance + turns + recording coverage | Turns, word shares, airtime concentration, silence, subgroup voice |
| Annotations with codebook and coder provenance | Reason giving, respect, floor attempts and coded interruptions |
| Argument inventory + transcript links | Perspective coverage and argument exposure opportunities |
| Versioned briefing passages + argument inventory | Availability of arguments before discussion |
| Pre/post survey responses + explicit scales | Opinion change, learning, H and P |
| Explicit advantaged reference + survey responses | D and focal-group movement |
| Assignments + survey responses | Baseline-adjusted group heterogeneity |
| Supported randomization specification + full roster | Assignment randomization tests |
| Declared probability sample + sampling design | Design-based change estimates and domain intervals |

`analysis_requirements()` exposes these requirements programmatically.
`data_schema()` lists the required columns and keys. See the **Input schema**
vignette for optional fields, timing, coding and join contracts.

## Your definitions, your policy

Contrasts accept R predicates or explicit membership tables with `event_id`,
`person_id`, `focal`, and `reference`. Membership is frozen in the result. Focal
and reference groups must be disjoint; they do not have to exhaust the population.
Only an explicitly advantaged reference enables D. Intersectional definitions
work exactly like other predicates.

An audit without a policy returns diagnostics and availability statuses, without
PASS/FAIL labels. A versioned policy can define individual or compound rules:

```r
policy <- audit_policy(
  version = "illustration-1",
  rules = data.frame(
    rule_id = "concentration", scope = "session",
    metric = "max_speaker_share", operator = ">", threshold = 0.4,
    verdict = "WARNING", min_coverage = 1, aggregation = "any"
  ),
  rationale = "Illustrative threshold, not a validated quality standard."
)
result <- audit_dp(x, education, policy)
result$policy_results
```

Rows sharing a rule ID form an AND rule. Each condition can test all or any
matching estimates. Missing estimates, missing diagnostics or inadequate coverage
produce `NOT_ASSESSED`. A condition not met produces `NOT_TRIGGERED`, never an
implicit PASS. D/P/H have no built-in failure thresholds.

## Uncertainty follows the question

The default audit describes the observed event. For a declared model with
independent groups, request CR2/Satterthwaite inference explicitly:

```r
config <- audit_config(cluster = cluster_inference(
  "group_id", target = "Comparable independently formed groups",
  rationale = "The study design supports independence across these groups."
))
result <- audit_dp(x, education, config = config)
```

This adds intervals for paired mean changes and direct subgroup differences.
The specification does not establish independence or identify an effect of
deliberation. `survey` handles declared probability samples separately; assignment
tests preserve declared randomization constraints using `randomizr`.

Use `test_families = list(changes = "cluster_mean_change")` to apply Holm adjustment
to all matching rows across events. Families are explicit and nonoverlapping;
raw p-values remain available. The optional wild-bootstrap example is separate
from the CRAN package's dependencies.

## Human and model coding

The same annotation and link tables accept either source. They require coder,
codebook version and review status; model records also require model and prompt
version. Accepted and adjudicated records are included by default. Conflicting
codes are unresolved until adjudicated. Model confidence is not treated as
validation or a probability that the label is correct.

An absent annotation is unknown. An absent argument link provides only a coverage
lower bound unless coding is explicitly complete. An absent response link is not
evidence that a counterargument was ignored. Overlapping speech is measured
separately from coded interruptions. Exposure is an opportunity to encounter an
argument during attendance, not proof of listening or persuasion.

## The distortions adapter

```r
x <- read_distortions("../distortions")
x$provenance$ledger
result <- audit_dp(x, config = audit_config(membership = "available"))
```

The adapter uses the participant CSV and item dictionary, preserving the source's
designated post wave. It removes logged exact duplicates, flags unresolved person
IDs and retains raw values when correcting tiny endpoint roundoff. It imports no
transcripts or inferred assignment mechanism. The R result includes input hashes
and transformations. Participant data are not bundled in this repository.

## Interpretation and reproducibility

Default results describe the finite observed event, with equal participant
weights and paired survey observations. Population intervals require explicit
sampling assumptions; assignment tests require an actual supported mechanism.
No causal effect of deliberation follows from pre/post change alone. D measures
attitude movement, distinct from airtime concentration. See the **Methods**
vignette for formulas, denominators and limitations.

HTML reports omit person-level records and source excerpts by default. The R
result retains the supplied evidence, including identifiers and transcript text;
treat saved R objects accordingly. `include_evidence = TRUE` adds that evidence to
the report explicitly. Nothing is transmitted to an external model service.

## Development

```sh
make deps
make document
make ci
make ci-docker
make examples
Rscript inst/examples/gallery.R ../distortions
make docs
```

The package follows [r-canon](https://github.com/gojiplus/r-canon).
Local Docker CI uses the standard `rocker/r2u` image, including native ARM support. `make ci` runs lint,
testthat, package build and `R CMD check`. Tests exercise formulas, incomplete
inputs, policies, inference, importer behavior and report rendering. pkgdown uses
this README for its home page and generated package help for its reference.

The software implements observational diagnostics informed by
[Fishkin's design criteria](https://deliberation.stanford.edu/publications/journal-articles/deliberative-public-consultation-deliberative-polling-criteria-and) and the signed
movement measures in
[Luskin et al., *Deliberative Distortions?*](https://doi.org/10.1017/S0007123421000168).
Its annotation categories are explicitly documented operational conventions,
not a claim to reproduce a validated DQI coding instrument.

MIT license.
