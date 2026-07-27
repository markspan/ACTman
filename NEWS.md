# ACTman (development)

## Phase 5: readability pass (dead code, docstrings, two bugs found by new tests)

Verified against the full test suite (49/49 assertions green, up from 46;
new tests added for previously-uncovered code).

### Fixed (found while auditing for awkward patterns)
- `plot_actogram()`: `as.character()` on a `POSIXct` silently drops the
  time-of-day when it is exactly midnight (e.g. `"2020-01-01 00:00:00"`
  prints as `"2020-01-01"`), which broke the `substr(Date, 12, 19) ==
  "00:00:00"` day-boundary detection whenever the first recorded minute was
  midnight. Fixed by using `format(..., "%Y-%m-%d %H:%M:%S")` instead.
  Caught by a new smoke test (`test-actogram.R`) -- `plot_actogram()` had no
  test coverage at all before this phase.
- `plot_actogram()`: the EWS-without-rolling-window guard called bare
  `stop()` after several `message()` calls, so the actual raised condition
  had an empty message (any `tryCatch` around it saw `""`). Consolidated
  into a single informative `stop("Cannot create EWS plot without rolling
  window results...")`.
- Removed two `<<-` global-environment-assignment debug leaks in
  `plot_actogram()` (`LOLkat <<- LOLkat` and
  `rollingwindow.results <<- rollingwindow.results`) -- both were dead
  debugging artifacts with no effect on the function's actual output, but
  polluted the global environment as a side effect on every call.

### Readability (no behavior change)
- Removed dead/commented-out code across `sleeplog.R` (~40 lines: an
  unimplemented "nearest neighbour" imputation alternative, debug prints,
  stale alternate implementations), `sleep_summary.R` (~60 lines: debug
  prints, superseded calculation attempts, a `test <- 1` unused variable,
  a wishlist of unimplemented metrics rewritten as a single comment),
  `actogram.R` (~20 lines: an abandoned "Work in Progress" experimental
  block, obsolete alternative `ylimit` calculation), and `actman.R` (a few
  stray dead assignment comments).
- Translated the one remaining Dutch comment (`sleep_summary.R`) to English.
- Renamed `mb_TEMP` -> `person_day_markers` (`sleeplog.R`) and `LOLkat` ->
  `ews_timeseries` (`actogram.R`) -- both meaningless placeholder names for
  otherwise-clear data.
- Extracted the local `roundup_power_10()` helper (previously defined
  inline inside `plot_actogram()`) to a documented top-level function in
  `utils.R`.
- `sleeplog.R`: changed remaining `1:nrow(x)` loops to `seq_len(nrow(x))`,
  consistent with the Phase 2 fix for the same pattern elsewhere.
- Confirmed (via an automated check) that every top-level function in the
  package now has a roxygen docstring.
- Added test coverage for `plot_actogram()` (previously none).

### Known awkward patterns identified but intentionally not changed here
(documented rather than fixed, to avoid introducing subtle bugs in
under-tested code within this pass)
- `sleep_summary.R`'s per-night loop uses extremely terse variable names
  (`aaa`, `aaa.Bedtime`, `sleep.start.`, `tempp`, `sleepend`, `rownr.*`)
  across ~150 references. This is the most fragile, least-formally-verified
  part of the codebase (see its own "known limitations" notes); renaming it
  safely would need substantially more test coverage of its edge-case
  branches than currently exists.
- `actogram.R` builds per-day variables via `assign(paste0("day", i), ...)`
  / `eval(parse(text = ...))` instead of a list (`days[[i]]`) -- idiomatic
  R would avoid `assign`/`eval(parse())` entirely, but converting this
  touches most of the function including both the 24h and 48h plotting
  branches.
- The same `as.character(POSIXct)` midnight-truncation risk found and fixed
  in `plot_actogram()` is also present in `actman.R`'s overview reporting
  (`start`/`end`/`end2` columns) -- lower severity there since it only
  affects a display value, not control flow, so left as-is.
- Actiwatch2's column selection uses positional magic numbers
  (`ACTdata.1[, c(4, 5, 6)]`) rather than named constants.
- The Actiwatch2 and MW8 device-reading branches in `actman.R` share
  significant structure without being unified into a common helper.

## Phase 4: path-based I/O and DST-safe date arithmetic

No behavior change to computed results; verified against the full
characterization/unit test suite (46/46 assertions green).

- Removed every remaining `setwd()`/`getwd()` call from the package
  (`actman.R`, `sleep_summary.R`, `sleeplog.R`, `actogram.R`). Added
  `actman_paths()` (`paths.R`): a small structure computed once per call,
  normalizing `workdir` to an absolute path and pre-computing the
  `managed_dir`/`results_dir`/`actogram_dir` subdirectory paths used
  throughout. Every read/write now goes through an explicit path via
  `file.path()`; no function mutates the R session's working directory.
- This fixes a real, previously-demonstrated bug: a *relative* `workdir`
  used to break partway through a run, because `ACTman()` `setwd()`'d into
  it and then built further paths from the same (now stale-relative)
  variable. This was worked around in the test harness back in Phase 0/1
  by always passing an absolute path; it is now fixed at the source, and a
  new regression test (`test-relative-workdir.R`) exercises the previously-
  broken case directly.
- Rewrote `increase_by_days()` using `lubridate::days()` (calendar-based
  Period arithmetic) instead of manual `gmtoff`-comparison DST correction.
  Same public behavior (verified by the existing DST unit test), 4 lines
  instead of ~15. Added `lubridate` to `Imports`.

## Phase 3: modular file split + full manual

No behavior change (verified against all characterization/unit tests at
every step). Purely structural + documentation:

- Split `nparcalc.R` into `circadian_metrics.R` (IS/IV/RA/L5/M10),
  `ews_metrics.R` (Mean/Variance/SD/CoV/Skewness/Kurtosis/Autocorrelation/
  Time-to-Recovery), and a thin `nparcalc.R` wrapper that handles
  device/window detection and delegates to the two. Both new functions are
  pure (no I/O, no printing, no `setwd()`).
- Extracted `score_epochs()` (`sleep_scoring.R`) from `sleepdata_overview()`:
  the per-epoch wake/sleep/mobile classification and sleep-chance/
  wakeup-chance rolling indicators, now a standalone pure function.
- Extracted `run_rolling_window()` (`rolling_window.R`) from `ACTman()`'s
  previously-inline moving-window closure.
- Renamed files for consistency: `actman.r` -> `actman.R`,
  `plot_actogram.R` -> `actogram.R`, `sleeplog_from_markers.R` ->
  `sleeplog.R`, `sleepdata_overview.R` -> `sleep_summary.R`.
- Added `utils.R` for shared constants (`MINUTES_PER_DAY`,
  `L5_WINDOW_MINUTES`, `M10_WINDOW_MINUTES`, `MAX_AUTOCORR_LAG`).
- Added test coverage for `run_rolling_window()` (previously the
  moving-window path had zero test coverage).
- Rewrote `README.md` as a full manual: installation, data format specs for
  both supported devices, worked examples for every major workflow, a
  package architecture map, a function-by-function reference, and the
  scientific references the calculations are based on (Van Someren et al.
  1999; Witting et al. 1990; Cole et al. 1992; Scheffer et al. 2009; Van de
  Leemput et al. 2014).

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
