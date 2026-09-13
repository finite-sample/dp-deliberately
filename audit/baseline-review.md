# Baseline statistical and public-contract audit

## Scope and verdict

Repository: `/Users/soodoku/Documents/GitHub/deliberately`

Baseline audited: `5cb530da869391080e0c32a898873e542d9e2f6e`

Mode: read-only package audit. The baseline was exported with `git archive`, installed in an isolated temporary library, and loaded from an isolated source copy for tests. No tracked source file was changed.

The focused audit verified two numerical metadata defects and two related public-boundary validation defects. The substantive point estimates examined in outcomes, participation, exchange, argument coverage, exposure, and policy evaluation otherwise matched their documented finite-event estimands and hand calculations. The numerical defects affect usable-observation counts, coverage, and therefore policy assessability; neither changes the corresponding point estimate in the reproduced inputs.

## Claim-to-estimand and four-question audit

### Participation and voice

1. **What is computed and why?** Session-level participant turn counts, word shares, unioned speaking durations, talk shares, Gini concentration, silence, overlap, and focal/reference voice ratios characterize observed participation. Present participant attendees form the roster; nonparticipants do not enter participant airtime denominators.
2. **Why is the design reasonable?** These are finite-session descriptions when attendance, recording coverage, speaker identity, and timestamps are accurate. Silence appropriately requires complete recording and identified speakers. Voice ratios remain qualified by unequal attendance duration.
3. **Was it implemented correctly?** Hand checks confirmed unioned intervals, inclusion of zero-turn attendees, talk-share and Gini arithmetic, pairwise overlap, suppression of silence under incomplete coverage, and suppression of participant shares with unknown speakers.
4. **Was it interpreted reasonably?** Yes in the package documentation: talk equality is not imposed as a normative target; pair-seconds are not interruption counts; word shares are not elapsed speech; and voice ratios do not exposure-adjust for attendance time.

### Deliberative exchange and responsiveness

1. **What is computed and why?** Rates summarize explicitly coded recommendations, reasons, evidence, conduct, floor attempts, responses, engagement, links, agreement, stance, and interruption incidence.
2. **Why is the design reasonable?** The results are conditional on the supplied codebook, labels, review state, and link inventory. Missing codes and missing links must remain unknown unless an explicit code supplies a negative observation.
3. **Was it implemented correctly?** Reason/evidence fallback rules and counterargument response handling matched the declared conventions. One metadata implementation is wrong: interruption-ratio coverage is calculated as a focal/reference sample-size ratio rather than coding completeness.
4. **Was it interpreted reasonably?** Documentation correctly says exact coder agreement is not chance-corrected reliability and annotation metrics do not validate a latent deliberative-quality construct.

### Argument and material coverage

1. **What is computed and why?** Per-session, per-perspective metrics report the fraction of a declared topic inventory linked to observed turns, a lower bound under incomplete coding, a reviewed-inventory restriction, source-passage availability, and expert/moderator argument-linked turns.
2. **Why is the design reasonable?** Exact transcript coverage requires complete recording and an explicit exhaustive-coding declaration. The inventory remains researcher supplied.
3. **Was it implemented correctly?** Reproductions confirmed exact coverage becomes unavailable when coding completeness is false while the observed lower bound remains computed; reviewed and pre-session material restrictions operate as documented.
4. **Was it interpreted reasonably?** Yes. Coverage does not establish that the inventory is complete, balanced, strong, or objectively correct.

### Opposing-argument opportunity

1. **What is computed and why?** A participant has an opportunity when a directional baseline attitude maps to an opposing reviewed argument whose linked turn overlaps that participant's attendance.
2. **Why is the design reasonable?** It is a temporal opportunity measure conditional on the perspective map, reviewed inventory, exhaustive link coding, timing, and attendance.
3. **Was it implemented correctly?** Positive, negative, incomplete-coding, and attendance-nonoverlap cases behaved as documented.
4. **Was it interpreted reasonably?** Yes. The result does not demonstrate attention, comprehension, agreement, persuasion, or argument quality.

### Opinion, polarization, distortion, and knowledge outcomes

1. **What is computed and why?** Normalized pre/post means and changes describe attitudes and explicitly scored knowledge. `H` is the reduction in individual score SD; `P` is signed movement relative to the substantive midpoint; `P_absolute` is change in absolute midpoint distance; `D` and `D_focal` use an explicitly advantaged reference group's baseline; knowledge measures use paired scores and declared scoring rules.
2. **Why is the design reasonable?** The calculations are descriptive finite-event contrasts. Paired membership holds composition fixed; available-wave membership is explicitly a sensitivity estimand that can mix composition change with change. Direction, midpoint, scoring, and advantaged-reference choices must be substantively justified.
3. **Was it implemented correctly?** Scale normalization, reverse direction, midpoint crossing, signed movement, weighted means/SD, paired versus available membership, binary/proportion scoring, complete-item knowledge indices, subgroup gaps, and zero-variance behavior matched hand calculations and documented formulas. Unscored knowledge metrics have incorrect `n` and coverage metadata.
4. **Was it interpreted reasonably?** Documentation correctly avoids causal claims and notes that the sign of a gap change depends on the initial gap. Standardized gain is undefined at zero baseline variance and may be unstable near zero variance.

### Policy verdicts

1. **What is computed and why?** Researcher-authored rules compare matched diagnostics with declared thresholds, minimum coverage, scope, and within-condition aggregation. Conditions sharing a rule ID are ANDed.
2. **Why is the design reasonable?** It preserves `NOT_ASSESSED`, `NOT_TRIGGERED`, and explicit verdicts as distinct states. Its validity depends entirely on the researcher's normative policy and threshold justification.
3. **Was it implemented correctly?** Boundary comparisons, missing evidence, compound rules, unit scoping, and minimum-coverage behavior matched the contract. Incorrect upstream coverage metadata can nonetheless change assessability.
4. **Was it interpreted reasonably?** Yes. The package does not present its policy rules as a validated composite quality score.

## Verified findings

### 1. Unscored knowledge metrics report nonexistent usable observations

Location: `R/outcomes.R`, the knowledge branch of `outcome_metrics()` and the default `n`/`denominator` arguments of its local `add()` function.

Trigger:

```r
x <- example_deliberation()
x$tables$items$scoring <- NA_character_
m <- deliberately:::outcome_metrics(x, list(), audit_config())
```

The code correctly forces knowledge gains to `NA` when scoring is undeclared, but `knowledge_gain`, `fraction_learning`, and `knowledge_gain_sd` inherit `n = sum(z$paired)` and `denominator = nrow(z)`. The methods vignette defines `n` as the usable-observation count.

Verified old and corrected metadata:

| Unit | Estimate | Old n | Denominator | Old coverage | Correct n | Correct coverage |
|---|---:|---:|---:|---:|---:|---:|
| Event, each affected metric | `NA` | 16 | 20 | 0.8 | 0 | 0 |
| Each four-person group, each affected metric | `NA` | 4 | 4 | 1.0 | 0 | 0 |

Impact: reports claim high or complete measurement coverage when no score can be computed. Status remains `unassessable`, so current policy evaluation does not trigger a verdict from these particular rows, but the metadata and evidence description are numerically false.

Required gate: an unscored knowledge item with paired raw responses must produce `estimate=NA`, `n=0`, and coverage zero for all score-dependent knowledge metrics.

### 2. Interruption-ratio coverage is a focal/reference size ratio

Location: `R/exchange.R`, `interruption_received_ratio` in `exchange_metrics()`.

Trigger: use `example_deliberation()`, set every supplied `interruption_received` annotation to `1`, and define an explicit membership with one focal and three reference participants in session `s1`.

Verified old result:

| Estimate | Old n | Old denominator | Old coverage |
|---:|---:|---:|---:|
| 1 | 1 | 3 | 0.3333333 |

The generic exchange `add()` defines coverage as `n / denom`, but this call passes the number of coded focal turns as `n` and the number of coded reference turns as `denom`. Both groups are completely coded. Correct coding metadata is four usable eligible turns over four eligible turns, hence coverage `1`. With the reverse imbalance, the current coverage can exceed one.

Impact: a fully coded ratio can be rejected by a policy's minimum-coverage gate; an imbalanced focal-heavy comparison can claim impossible coverage above 100 percent.

Required gate: unequal focal/reference sizes with complete coding must produce coverage one; independently missing annotations in either group must reduce coverage using total eligible turns as the denominator.

### 3. `audit_config()` accepts malformed public arguments

Location: `R/audit.R`, `audit_config()`.

Verified accepted inputs include `weighted=NA`, `weighted=1`, `probability_sample=NA`, `seed=NA`, a length-two seed, and numeric `baseline_predictors`. `audit_dp(example_deliberation(), config=audit_config(weighted=NA))` later fails with the low-level error `missing value where TRUE/FALSE needed`.

Correct public contract:

- `weighted` and `probability_sample`: scalar, nonmissing logical values.
- `seed`: scalar finite value accepted by the declared reproducibility contract, preferably an integer-domain check.
- `baseline_predictors`: character vector without missing or empty names.

Required gates: below/domain/above-style malformed inputs should fail at `audit_config()` with an argument-specific message; valid boundary values should survive a full audit.

### 4. Logical schema fields can pass as character and crash validation

Locations: `R/validate.R`, `choices()` and `validate_semantics()`.

Trigger:

```r
x <- example_deliberation()
x$tables$arguments$reviewed <- as.character(x$tables$arguments$reviewed)
validate_data(x)
```

Old result: validation crashes at `args$reviewed & ...` with `operations are possible only for numeric, logical or complex types`.

Cause: `%in% c(TRUE, FALSE)` coerces character strings such as `"TRUE"` and `"FALSE"`, and no logical type check runs before semantic operations. `recruitment$attended` exposes the same boundary.

Correct result: a structured `TYPE` or `INVALID_VALUE` validation row naming the logical field; `audit_dp()` should then raise its structured `deliberation_validation_error` rather than leaking an internal operator error.

## Public export coverage

| Export | Contract reviewed | Positive/negative evidence | Result |
|---|---|---|---|
| `analysis_requirements` | analysis names, layers, units, denominators, assumptions | Compared with producing modules and methods vignette | Clean in focused domains; descriptions correctly state conditional estimands |
| `audit_config` | membership, booleans, predictors, annotation states, confidence, seed, permutations | Existing boundaries plus malformed booleans/seeds/predictors | Defect 3 |
| `audit_dp` | validation stop, module routing, event slicing, capability status, metadata, policy integration | Survey-only and full synthetic fixtures; repeatability | Clean apart from inherited metadata defects |
| `audit_policy` | required columns, scope, operators, thresholds, verdicts, coverage, aggregation | At/either-side threshold checks; missing and compound evidence | Clean for declared rule semantics |
| `available_analyses` | readiness by event and required tables/fields | Full synthetic and survey-only fixtures | Clean in focused domains |
| `data_schema` | required fields and primary keys | Cross-checked against validation and input vignette | Logical type enforcement gap feeds defect 4 |
| `define_contrast` | predicate/table alternatives, ID/rationale, disjoint logical membership | Predicate/table equivalence, overlap, duplicates, missing sides | Clean |
| `deliberation_data` | named data frames, schema version, preservation | Invalid names/types and standard fixtures | Clean as a lightweight constructor; substantive checks correctly delegated |
| `detectable_change` | power/design helper | Source and existing tests inspected | Outside the assigned outcome/dialogue slice; no independent statistical re-derivation |
| `example_deliberation` | complete positive-control bundle | Full validation and all-layer readiness | Clean |
| `read_deliberation` | recognized CSVs, identifier preservation, hashes | Leading-zero and round-trip fixtures | Clean in tested paths |
| `read_distortions` | source mapping, duplicate ledger, unresolved IDs, response columns, endpoint tolerance | Source and adapter tests inspected | No focused-domain defect verified; external source population semantics untestable here |
| `render_audit` | audit-object boundary and evidence inclusion | Existing render tests inspected | Statistical output unaffected; visual artifact QA outside this slice |
| `validate_data` | structure, values, joins, timing, scales, evidence references | Keys, joins, scales, time, provenance, spans, malformed logical field | Defect 4 |

Relevant internal callables reviewed separately: `paired_responses`, `append_knowledge_index`, `signed_movement`, `outcome_metrics`, `union_intervals`, `interval_length`, `intersection_length`, `participation_metrics`, `accepted_rows`, `resolved_codes`, `code_values`, `exchange_metrics`, `argument_metrics`, `inventory_arguments`, `session_recorded`, `exposure_metrics`, and `evaluate_policy`.

## Test and reproduction coverage

- An isolated baseline source tree was loaded with `pkgload::load_all()`.
- The complete `testthat` suite passed: contracts, data, dialogue, import/report, inference, outcomes, and policy.
- Each reported defect was then reproduced independently against an installed baseline package or isolated loaded baseline.
- The earlier direct-source `R CMD check` Author/Maintainer result is excluded. Correct package checking must build a tarball first because `R CMD build` generates those fields from `Authors@R`; it is not a package defect.

## Rejected candidates

- **Silent attendees omitted from participation:** rejected. Present participant attendees form the roster and zero-turn attendees enter silence and Gini calculations.
- **Partial recordings treated as complete silence evidence:** rejected. `silent_fraction` becomes unassessable unless recording coverage is complete and speakers are identified.
- **Overlapping attendance or turn intervals double counted for participants:** rejected. Attendance and per-person turn intervals are unioned before duration calculations.
- **Pair overlap mislabeled as interruption count:** rejected. It is explicitly named and documented as pairwise speaker-overlap seconds.
- **Missing links treated as unanswered counterarguments:** rejected. Missing links remain unknown absent an explicit `response_observed` code.
- **Incomplete argument coding presented as exact coverage:** rejected. Exact coverage becomes `NA`; a separately labeled observed lower bound remains.
- **Available-wave means accidentally change membership:** rejected as a defect. Composition change is the declared sensitivity estimand and is documented.
- **Midpoint crossing sign error:** rejected. `P` retains baseline direction while `P_absolute` separately measures distance; hand calculations match both definitions.
- **Reverse-coded midpoint error:** rejected. Responses and substantive midpoint are both reversed after normalization.
- **Knowledge binary key scored twice:** rejected. The second scoring pass uses preserved raw values and returns the same declared 0/1 score.
- **Policy missing evidence yields PASS:** rejected. Missing, uncomputed, or insufficiently covered conditions produce `NOT_ASSESSED`.
- **Direct-source `R CMD check` missing Author/Maintainer:** rejected. This invocation skipped the required build-tarball step.

## Untestable assumptions and missing evidence

- Whether supplied attendance, timestamps, speaker identities, transcript coverage, and exhaustive-coding declarations are truthful cannot be established from the package fixtures.
- Whether contrast membership and the `reference_advantaged` designation are substantively valid is user supplied.
- Whether midpoint, direction, knowledge keys, perspective maps, and codebook categories validly measure the intended constructs requires external instruments and validation data.
- Whether a declared argument inventory is complete, balanced across perspectives, strong, and correctly reviewed requires source evidence and independent review.
- Whether coder labels are reliable or valid requires repeated coding data and a measurement design; exact agreement alone is insufficient.
- Whether probability-sample weights, PSU/stratum identifiers, FPC values, and randomization-mechanism declarations reflect the actual designs requires sampling and assignment records.
- Causal interpretation of pre/post movement is unsupported without a design identifying the relevant counterfactual.
- External validity beyond observed events, sessions, participants, and inventories is not identified by these finite-event summaries.
- The `distortions` adapter's source-population interpretation and dictionary provenance were not independently checked against the external replication repository in this bounded audit.

## Repair and gate order

1. Correct usable `n` and coverage for unscored knowledge metrics and add discriminating tests.
2. Define ratio coverage over all eligible focal/reference turns, preserve component sample sizes separately if needed, and add unequal-group policy-gate tests.
3. Tighten `audit_config()` argument validation and add boundary tests.
4. Enforce logical schema types before semantic validation and add structured-error tests for every logical field.
5. Run the full local test suite, then `R CMD build` followed by `R CMD check` on the generated tarball.
