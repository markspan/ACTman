# ACTman (development)

## Phase 2: bug fixes and non-interactive operation

Baseline: `compsy/ACTman@66d8f69`.

### Fixed
- `ACTman()`: an empty/no-match `workdir` (no `.csv` files after excluding
  sleeplog/markers) previously produced a cryptic
  `arguments imply differing number of rows` or `1:0`-indexing error partway
  through the run. Now fails fast with a clear message.
- `ACTman()`: main file loop used `for (i in 1:length(ACTdata.files))`, which
  breaks (`1:0` == `c(1, 0)`) if the file list is ever empty. Changed to
  `seq_along()`.
- `sleepdata_overview()`: `for (a in 1:loop_steps)` had the same `1:0` risk
  when `loop_steps` is 0; changed to `seq_len()`.
- `sleepdata_overview()`: the `is.null(sleepend)` check could never be TRUE,
  since `tail()` of an empty data frame returns a 0-row frame, not `NULL`.
  Changed to `nrow(sleepend) == 0`, so the intended sleeplog-Gotup fallback
  actually runs on genuinely empty results.
- `sleepdata_overview()`: the fallback assignment
  `rownr.sleep.end <- rownr.Gotup` for an unresolved sleep-end index was
  written *after* a `next()` call in the same block, making it unreachable
  dead code (the day was always skipped instead of using the fallback).
  Removed the `next()` so the fallback takes effect as originally intended.

### Changed (interactive -> parameterized)
- Removed both `readline()` prompts, which caused the pipeline to hang
  indefinitely in any non-interactive context (scripts, CI, Rscript).
  - `ACTman(..., on_high_missings = c("continue", "abort"))`: replaces the
    "more than 0.01% missing, continue? y/n" prompt. Default `"continue"`
    preserves the previous most-common manual answer.
  - `ACTman(..., on_missing_markers = c("median", "manual", "abort"))` /
    `sleeplog_from_markers(..., on_missing_markers = ...)`: replaces the
    "m/n/f/q" prompt for missing Bedtime/Gotup markers. Default `"median"`
    preserves the previous most-common manual answer (median imputation).
    `"manual"` still opens `fix()` for hand-editing, but now errors clearly
    if called outside an interactive session instead of hanging.

### Added
- Synthetic Actiwatch2 and MW8 fixtures (`tests/fixtures/`) and
  characterization tests (`tests/testthat/test-characterization.R`) that
  freeze the pre-Phase-2 numeric output of the circadian-analysis pipeline,
  to catch unintended behavior changes in later refactoring phases.
- Unit tests for `increase_by_days()` (including a DST spring-forward case),
  `nparcalc()` (IS/IV/RA/L5/M10 sanity and consistency checks, NA handling,
  2-column vs 3-column CRV.data equivalence), an end-to-end
  `sleepdata_overview()` integration test on a synthetic 2-night fixture, and
  parameter-validation tests for the new non-interactive arguments.

None of the fixes above changed the circadian-analysis (`IS`/`IV`/`RA`/
`L5`/`M10`) output on the existing characterization fixtures; those bugs
only manifest on inputs the fixtures didn't previously exercise (empty
directories, unresolved sleep-end indices).
