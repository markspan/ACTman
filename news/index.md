# Changelog

## ACTman 2.0.0

### Fix R CMD check failures on CI (newer R version)

The `R-CMD-check` workflow failed on macOS with R 4.6.1 (passed locally
on this environment’s R 4.3.3), surfacing two real, fixable issues:

- `checking for code/documentation mismatches ... WARNING`:
  `data(ACTdata.1)` couldn’t find the dataset. The underlying file was
  `data/yoramdata.RData`, but R’s
  [`data()`](https://rdrr.io/r/utils/data.html) lookup expects the
  file’s base name to match the object name it contains (`ACTdata.1`)
  unless a `data/datalist` mapping file is provided; this apparently
  wasn’t enforced as strictly on R 4.3.3 but is on 4.6.1. Renamed the
  file to `data/ACTdata.1.RData` – the object itself is unchanged, only
  the file name.
- `checking for hidden files and directories ... NOTE`: `.lintr` was
  being bundled into the built tarball (it wasn’t in `.Rbuildignore`).
  Added it; this doesn’t affect the separate `lint` CI workflow, which
  runs `lintr::lint_package()` directly against the source checkout, not
  the built tarball.

### Update EWS reference

Replaced the Scheffer et al. (2009) and Van de Leemput et al. (2014)
citations for the early-warning-signal statistics
([`ews_metrics()`](https://markspan.github.io/ACTman/reference/ews_metrics.md),
README, vignette) with Helmich, Schreuder, Bringmann, Riese, Snippe &
Smit (2024), “Slow down and be critical before using early warning
signals in psychopathology” (*Nature Reviews Psychology*, 3, 767-780).
Since this paper takes a critical/cautionary stance on using EWS-based
critical slowing down to predict mental health symptom changes – unlike
the two references it replaces – the surrounding descriptive text was
also updated to reflect that framing accurately, rather than just
swapping the citation in place.

### Fix pkgdown CI build failure

The `pkgdown` GitHub Actions workflow failed on its first real run with
two issues visible only once real internet access reached the point past
this sandbox’s own network restrictions:

- `DESCRIPTION` was missing a `URL` field matching the configured
  pkgdown site (`https://markspan.github.io/ACTman/`) – added.
- Three internal helper functions (`actman_paths`, `ensure_dir`,
  `roundup_power_10`) had `man/*.Rd` pages but weren’t listed in
  `_pkgdown.yml`’s reference index, which pkgdown treats as an error
  rather than silently including them. Tagged all three
  `@keywords internal` (matching how the shared constants were already
  tagged), which is also the more accurate/intended state for functions
  not part of the documented public API.

### Housekeeping: remove obsolete branches, scripts, and Docker setup

- Deleted the per-phase feature branches (`phase-1-2-...` through
  `phase-6-...`); all their work is already in `development` (and was
  merged there phase-by-phase as each landed). Only `development` and
  `master` remain going forward.
- Removed `todo.md`: folded its not-yet-addressed items into README’s
  “Known limitations” section (the completed DST item was already noted
  in NEWS.md), so nothing tracked there is lost, but there’s now one
  place (README) for known issues instead of two.
- Removed `Dockerfile`, `test.Dockerfile`, and `docker_configs/`: an old
  OpenCPU-based Docker setup that already referenced
  `inst/bash/install-package-dependencies.sh`, itself removed as stale
  in Item 4 – these Dockerfiles were already broken before this cleanup.
- Removed `scripts/actman_runner.R` and `scripts/runners.R`: personal
  dev scripts hardcoding local machine paths (`/Users/ando/...`,
  `C:/mydata`), referencing a `myACTdevice = "Actical"` device that was
  never implemented, the `movingwindow.size = TRUERUE` typo bug noted
  very early in this modernization effort, and the pre-`actman_result`
  return API – fully superseded by
  [`vignette("actman-intro")`](https://markspan.github.io/ACTman/articles/actman-intro.md).
- Cleaned up dead `.Rbuildignore` entries left over from the above
  (`circle.yml`, `.circleci`, `scripts`, `Dockerfile` x2,
  `docker_configs`).

### Item 6: scientific validation harness

Verified against the full test suite (78/78 assertions green, up from
71).

- Installed the real `nparACT` package (from its CRAN mirror at
  `github.com/cran/nparACT`) and ran a genuine cross-validation of
  ACTman’s circadian metrics against it on the real bundled example
  recording – an independent, peer-reviewed implementation of the same
  published method (Van Someren et al., 1999), not just internal
  consistency checking. Results: `RA`/`L5`/`M10` agree closely,
  `L5_starttime` matches exactly, `IS`/`IV` show moderate divergence,
  and `M10_starttime` shows a more notable ~1.5 hour divergence.
  Investigated the latter against nparACT’s own source: both packages
  use a structurally identical sliding-window approach, so this is most
  likely a day-boundary/alignment difference rather than a fundamentally
  different method – not yet fully root-caused, tracked in `todo.md`.
- Added `test-nparact-validation.R`: a permanent test asserting the
  above agreement stays within tolerances set close to (not looser than)
  the currently-observed agreement, so it catches a future *regression*
  in cross-implementation agreement rather than just asserting a
  comfortable pass. Skips cleanly when `nparACT` isn’t installed.
- **Fixed a real dependency-hygiene issue found along the way**:
  `nparACT` was a hard `Imports` dependency despite being used only for
  the optional `nparACT_compare = TRUE` feature, forcing every ACTman
  install to also pull in `ggplot2`/`stringr`/`zoo`. Moved to
  `Suggests`, with a clear
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) check and
  error message if `nparACT_compare = TRUE` is requested without it
  installed (previously would have failed with a cryptic “could not find
  function” error instead).
- Documented the full comparison in README’s “Known limitations” and
  `todo.md`.

### Item 5: vignette, pkgdown site, and real data

Verified against the full test suite (71/71 assertions green, up from
67).

- **Bundled a real, anonymized example recording**: a ~33-day
  MotionWatch 8 recording (2017-07-05 to 2017-08-07, 94,721 observations
  at 30-second epochs) is now properly documented as `data(ACTdata.1)`
  (its docstring previously cited a broken placeholder URL). A ~7-day
  subset, in genuine raw MW8 export format, is bundled at
  `inst/extdata/example-mw8-participant.csv` for demonstrating the full
  file-reading pipeline.
- **Found and fixed a real integration bug via this real data**:
  [`plot_actogram()`](https://markspan.github.io/ACTman/reference/plot_actogram.md)’s
  Phase 5 midnight-truncation fix used
  [`format()`](https://rdrr.io/r/base/format.html) on `Date`, which
  broke when called through the actual
  [`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md)
  pipeline – there, `Date` arrives already as a character string
  (reformatted upstream before the managed dataset is written), not the
  `POSIXct` that
  [`plot_actogram()`](https://markspan.github.io/ACTman/reference/plot_actogram.md)’s
  own standalone unit tests use. Fixed to handle both cases; added a
  permanent regression test (`test-actman-params.R`) that calls
  `ACTman(..., plotactogram = ...)` end-to-end on real data – this
  integration path had no test coverage before, which is exactly why the
  bug wasn’t caught earlier.
- **Corrected a citation**: fetched and read the actual paper describing
  ACTman (Kunkels et al., 2020, *Journal of Science and Medicine in
  Sport*). The sleep-scoring epoch-weighting scheme is sourced from
  CamNtech’s own MotionWare “Information Bulletin No. 3,” not the
  Cole-Kripke algorithm as speculatively cited in Phase 3/5 – fixed in
  [`score_epochs()`](https://markspan.github.io/ACTman/reference/score_epochs.md)’s
  docstring and the README, with Cole-Kripke retained as related
  (independently-developed) context rather than the primary source.
  Added a “Citation” section to the README with the real paper.
- Added a full vignette (`vignettes/actman-intro.Rmd`) walking through
  reading real data, interpreting the circadian rhythm variables,
  actogram generation, and moving-window analysis – verified by actually
  rendering it (not just checking it parses). Real output values are
  properly noisy/realistic (IS = 0.5, IV = 0.81) compared to the
  near-perfect synthetic sine-wave fixtures used elsewhere in the test
  suite.
- Set up `pkgdown` (config in `_pkgdown.yml`, deployment via
  `.github/workflows/pkgdown.yaml` to GitHub Pages). Confirmed Rd
  parsing, reference index generation, and vignette building all
  succeed; the one remaining failure locally (fetching a JS asset from
  `cdnjs.cloudflare.com`) is a sandbox network restriction in the
  environment this work was done in, not a package or config problem – a
  real GitHub Actions runner has full internet access.
- Removed the stale, un-runnable `@examples` block on
  [`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md)
  (an old Windows path, pre-`actman_result`-API example that also broke
  pkgdown’s Rd parser) and replaced it with a working example using the
  bundled real data.
- Updated `todo.md` (previously stale/un-tracked) to reflect current
  state: checked off the now-completed DST item, and added a few items
  identified during this modernization effort (EWS-overlay test
  coverage, the
  [`assign()`](https://rdrr.io/r/base/assign.html)/`eval(parse())`
  pattern in `actogram.R`, etc.).

### Item 4: style enforcement (lintr + styler)

Verified against the full test suite (67/67 assertions green
throughout).

- Added `.lintr` config and `.github/workflows/lint.yaml` (CI gated on
  zero lints). Three linters are intentionally disabled with documented
  rationale rather than configured to silently accept pre-existing
  issues: `object_name_linter` (the mixed
  dotted.case/snake_case/PascalCase naming already documented as a Phase
  5 known issue), `cyclocomp_linter`
  ([`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md)
  is a long orchestrator by design), and
  `line_length_linter`/`indentation_linter` (real pre-existing debt in
  long-commented legacy lines, plus a genuine disagreement between
  lintr’s default hanging-indent alignment and styler’s non-strict
  output).
- Ran `styler` (installed from GitHub source; not packaged for this
  sandbox’s apt) across `R/` and `tests/` in non-strict tidyverse style
  – purely mechanical whitespace/spacing changes, verified against the
  full test suite before and after.
- Fixed everything else lintr flagged (375 lints -\> 0): 8 bare `T`/`F`
  -\> `TRUE`/`FALSE`; 12 more `1:n`-style loops -\>
  [`seq_along()`](https://rdrr.io/r/base/seq.html)/[`seq_len()`](https://rdrr.io/r/base/seq.html)
  (2 standalone loops fixed directly; the rest live in `actogram.R`’s
  EWS-overlay plotting code, which has no test coverage for the
  real-rolling-window-data path, so left in a documented `# nolint`
  block rather than mechanically “fixed” – tracked as follow-up work
  alongside adding that coverage); ~13 more genuine leftover dead-code
  comments that the Phase 5 cleanup pass had missed.
- Removed `inst/r/build_help.r` (a stale personal dev script hardcoding
  one original author’s local machine path) and `inst/docs/` (a stale
  pre-generated pkgdown site referencing the old 5-function API,
  superseded by Item 5’s real pkgdown setup) and the CircleCI-era
  `inst/bash/*.sh` helper scripts, all fully superseded by Items 1/2/4.

### Item 3: config object + unified return type

Breaking change to
[`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md)’s
return value (hence part of the 2.0.0 bump); verified against the full
test suite (67/67 assertions green, up from 56).

#### Changed

- Added
  [`actman_config()`](https://markspan.github.io/ACTman/reference/actman_config.md)
  (`config.R`): consolidates the parameter validation that used to be
  scattered across the top of
  [`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md) –
  including a `myACTdevice` check that was redundantly re-run on every
  file in the main loop – into a single validated config object built
  once, up front.
  [`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md)’s
  own argument list is unchanged for backward compatibility (existing
  calls with named arguments keep working); it now simply builds an
  [`actman_config()`](https://markspan.github.io/ACTman/reference/actman_config.md)
  as its first step.
- **[`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md)
  now always returns a single `actman_result` object** (an S3 list with
  `$overview`, `$circadian`, `$sleep`, `$rolling_window`) instead of a
  different bare data frame depending on which flags were set
  (previously: the sleep summary if `iwantsleepanalysis`, else the
  rolling window results if `movingwindow`, else the overview). Fields
  for analyses that weren’t requested are `NULL`. `$overview`’s content
  is unchanged (same columns/values as the old default return); existing
  code that did `overview <- ACTman(...)` needs to become
  `result <- ACTman(...); overview <- result$overview`.
- Added
  [`print.actman_result()`](https://markspan.github.io/ACTman/reference/print.actman_result.md):
  a short console summary (which analyses ran, dimensions of each
  populated field) instead of dumping raw list contents.
- Note: when
  [`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md)
  processes multiple files, `$sleep` and `$rolling_window` still reflect
  only the *last* file processed – this matches prior behavior (neither
  was ever accumulated across files; each file’s own results are still
  written to disk in full via the existing per-file CSV writes) and is
  documented here as a known limitation rather than silently changed.

### Item 2: GitHub Actions CI

- Added `.github/workflows/R-CMD-check.yaml`: runs
  `R CMD check --as-cran` and the full test suite on every push/PR to
  `development`, across Linux, macOS, and Windows, on both the current
  and previous R release.
- Added `.github/workflows/test-coverage.yaml`: tracks test coverage via
  `covr` and uploads to Codecov on every push/PR.
- Removed the stale `.circleci/config.yml`, which pointed at
  `compsy/ACTman` (a different repository) and was not actually running
  for this fork. Updated the README badges accordingly.

### Item 1: build hygiene

`R CMD check` went from 1 ERROR + 3 WARNINGs to a single WARNING (a
sandbox/CI-environment locale limitation, not a package issue).

- Regenerated `NAMESPACE` and `man/*.Rd` via
  [`roxygen2::roxygenise()`](https://roxygen2.r-lib.org/reference/roxygenize.html),
  which had never actually been run despite five phases of adding new
  functions – none of them were exported or had generated help pages
  until now.
- Exported the 9 functions the README already documented as public API
  (`increase_by_days`, `circadian_metrics`, `ews_metrics`, `nparcalc`,
  `run_rolling_window`, `score_epochs`, `sleepdata_overview`,
  `sleeplog_from_markers`, `plot_actogram`) – previously only `ACTman`
  itself was exported, so calling any of these required `:::`.
- Found and fixed an unescaped `%` in a roxygen comment (`0.01%`) that
  Rd format silently treats as a comment marker, corrupting everything
  after it in the generated `ACTman.Rd`.
- Removed `gridExtra` from `Imports`: confirmed via git history it was
  never actually used, even in the original upstream code.
- Added missing
  `@importFrom`/[`globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
  declarations (`png`, `abline`, `points`, `mtext`, `median`, `head`,
  `fix`, `read.delim`, plus NSE/dynamically-created variable names) and
  missing `@param` docs (`i_want_EWS`, `rollingwindow.results`).
- Fixed the two remaining bare
  [`stop()`](https://rdrr.io/r/base/stop.html) calls (same
  empty-condition-message issue already fixed once in Phase 5’s
  [`plot_actogram()`](https://markspan.github.io/ACTman/reference/plot_actogram.md)
  fix).
- Bumped `DESCRIPTION`: `Version: 2.0.0`, current date,
  `RoxygenNote: 7.3.1`, added `Encoding: UTF-8` (required by roxygen2)
  and `withr` to `Suggests`.

### Phase 6: fix silent per-night sleep-metric corruption (rowname/position bug)

Verified against the full test suite (56/56 assertions green, up from
49).

#### Fixed

- [`sleepdata_overview()`](https://markspan.github.io/ACTman/reference/sleepdata_overview.md):
  `rownr.sleep.end` was computed as `as.numeric(rownames(sleepend))`,
  while every other position in this function (`rownr.Bedtime`,
  `rownr.Gotup`, `rownr.sleep.start`, …) is computed via
  `which(aaa$Time == ...)`. Row names are inherited from the full
  multi-night `data` object and only coincide with the in-window
  position within `aaa` for the *first* night (since night 1’s window
  starts at row 1 of `data`); for every subsequent night, `aaa` starts
  partway through `data`, so the row name is an unrelated, much larger
  absolute row number. This silently corrupted `assumed_sleep`,
  `actual_sleep_perc`, and `actual_wake_perc` for every night after the
  first (confirmed directly: a synthetic 2-night fixture with identical
  nightly activity patterns produced `assumed_sleep = 8.00` for night 1
  but `29.03` for night 2). `sleep.efficiency`, `timeinbed`,
  `sleep.start`, `sleep.end`, `sleep.latency`, `actual_sleep_duration`,
  and `wakepochs_duration` were unaffected – they’re derived from a
  separate `aaa.assumedsleeptime` recomputation that an existing
  `nrow(...) > 1440` safety-net branch already corrected, which is
  almost certainly why this went unnoticed: the headline metric (sleep
  efficiency) was fine, and the corrupted columns are secondary. Fixed
  by deriving `rownr.sleep.end` via
  [`which()`](https://rdrr.io/r/base/which.html), consistent with every
  other position in the function.
- Same fix’s root-cause analysis also surfaced a related, narrower bug:
  the fallback when no epoch qualifies as “quiet” before Gotup
  (`post_bedtime_window[rownr.Gotup, ]`) indexed a position from `aaa`’s
  index space into `post_bedtime_window`, a different (shifted) subset –
  wrong whenever that position happened to still be in range. Fixed by
  re-expressing the index relative to `post_bedtime_window`’s own start
  (`rownr.Gotup - rownr.sleep.start + 1`).
- Removed a “first approach” sleep-offset calculation
  (`sleep.end.`/`sleep.end.new`/`sleep.end.row`) that was computed and
  then *unconditionally overwritten* by a second approach – pure dead
  code that also carried its own latent bug risk (a `2:nrow(x)`
  assignment that could error on a 0- or 1-row intermediate result).

#### Readability

- Renamed the variables directly involved in the above
  (`aaa.sleeptime`/`tempp`/`sleepend` -\> `post_bedtime_window`/
  `quiet_epochs_before_gotup`/`last_quiet_epoch`) and added comments
  explaining the index-space distinction that caused both bugs.

#### Added

- `test-sleepdata-overview.R`: a regression test asserting
  `assumed_sleep`/`actual_sleep_perc`/`actual_wake_perc` are identical
  across two nights with identical activity patterns, and fall within
  plausible ranges (this is what would have caught the bug above).

### Phase 5: readability pass (dead code, docstrings, two bugs found by new tests)

Verified against the full test suite (49/49 assertions green, up from
46; new tests added for previously-uncovered code).

#### Fixed (found while auditing for awkward patterns)

- [`plot_actogram()`](https://markspan.github.io/ACTman/reference/plot_actogram.md):
  [`as.character()`](https://rdrr.io/r/base/character.html) on a
  `POSIXct` silently drops the time-of-day when it is exactly midnight
  (e.g. `"2020-01-01 00:00:00"` prints as `"2020-01-01"`), which broke
  the `substr(Date, 12, 19) == "00:00:00"` day-boundary detection
  whenever the first recorded minute was midnight. Fixed by using
  `format(..., "%Y-%m-%d %H:%M:%S")` instead. Caught by a new smoke test
  (`test-actogram.R`) –
  [`plot_actogram()`](https://markspan.github.io/ACTman/reference/plot_actogram.md)
  had no test coverage at all before this phase.
- [`plot_actogram()`](https://markspan.github.io/ACTman/reference/plot_actogram.md):
  the EWS-without-rolling-window guard called bare
  [`stop()`](https://rdrr.io/r/base/stop.html) after several
  [`message()`](https://rdrr.io/r/base/message.html) calls, so the
  actual raised condition had an empty message (any `tryCatch` around it
  saw `""`). Consolidated into a single informative
  `stop("Cannot create EWS plot without rolling window results...")`.
- Removed two `<<-` global-environment-assignment debug leaks in
  [`plot_actogram()`](https://markspan.github.io/ACTman/reference/plot_actogram.md)
  (`LOLkat <<- LOLkat` and
  `rollingwindow.results <<- rollingwindow.results`) – both were dead
  debugging artifacts with no effect on the function’s actual output,
  but polluted the global environment as a side effect on every call.

#### Readability (no behavior change)

- Removed dead/commented-out code across `sleeplog.R` (~40 lines: an
  unimplemented “nearest neighbour” imputation alternative, debug
  prints, stale alternate implementations), `sleep_summary.R` (~60
  lines: debug prints, superseded calculation attempts, a `test <- 1`
  unused variable, a wishlist of unimplemented metrics rewritten as a
  single comment), `actogram.R` (~20 lines: an abandoned “Work in
  Progress” experimental block, obsolete alternative `ylimit`
  calculation), and `actman.R` (a few stray dead assignment comments).
- Translated the one remaining Dutch comment (`sleep_summary.R`) to
  English.
- Renamed `mb_TEMP` -\> `person_day_markers` (`sleeplog.R`) and `LOLkat`
  -\> `ews_timeseries` (`actogram.R`) – both meaningless placeholder
  names for otherwise-clear data.
- Extracted the local
  [`roundup_power_10()`](https://markspan.github.io/ACTman/reference/roundup_power_10.md)
  helper (previously defined inline inside
  [`plot_actogram()`](https://markspan.github.io/ACTman/reference/plot_actogram.md))
  to a documented top-level function in `utils.R`.
- `sleeplog.R`: changed remaining `1:nrow(x)` loops to
  `seq_len(nrow(x))`, consistent with the Phase 2 fix for the same
  pattern elsewhere.
- Confirmed (via an automated check) that every top-level function in
  the package now has a roxygen docstring.
- Added test coverage for
  [`plot_actogram()`](https://markspan.github.io/ACTman/reference/plot_actogram.md)
  (previously none).

#### Known awkward patterns identified but intentionally not changed here

(documented rather than fixed, to avoid introducing subtle bugs in
under-tested code within this pass) - `sleep_summary.R`’s per-night loop
uses extremely terse variable names (`aaa`, `aaa.Bedtime`,
`sleep.start.`, `tempp`, `sleepend`, `rownr.*`) across ~150 references.
This is the most fragile, least-formally-verified part of the codebase
(see its own “known limitations” notes); renaming it safely would need
substantially more test coverage of its edge-case branches than
currently exists. - `actogram.R` builds per-day variables via
`assign(paste0("day", i), ...)` / `eval(parse(text = ...))` instead of a
list (`days[[i]]`) – idiomatic R would avoid `assign`/`eval(parse())`
entirely, but converting this touches most of the function including
both the 24h and 48h plotting branches. - The same
`as.character(POSIXct)` midnight-truncation risk found and fixed in
[`plot_actogram()`](https://markspan.github.io/ACTman/reference/plot_actogram.md)
is also present in `actman.R`’s overview reporting (`start`/`end`/`end2`
columns) – lower severity there since it only affects a display value,
not control flow, so left as-is. - Actiwatch2’s column selection uses
positional magic numbers (`ACTdata.1[, c(4, 5, 6)]`) rather than named
constants. - The Actiwatch2 and MW8 device-reading branches in
`actman.R` share significant structure without being unified into a
common helper.

### Phase 4: path-based I/O and DST-safe date arithmetic

No behavior change to computed results; verified against the full
characterization/unit test suite (46/46 assertions green).

- Removed every remaining
  [`setwd()`](https://rdrr.io/r/base/getwd.html)/[`getwd()`](https://rdrr.io/r/base/getwd.html)
  call from the package (`actman.R`, `sleep_summary.R`, `sleeplog.R`,
  `actogram.R`). Added
  [`actman_paths()`](https://markspan.github.io/ACTman/reference/actman_paths.md)
  (`paths.R`): a small structure computed once per call, normalizing
  `workdir` to an absolute path and pre-computing the
  `managed_dir`/`results_dir`/`actogram_dir` subdirectory paths used
  throughout. Every read/write now goes through an explicit path via
  [`file.path()`](https://rdrr.io/r/base/file.path.html); no function
  mutates the R session’s working directory.
- This fixes a real, previously-demonstrated bug: a *relative* `workdir`
  used to break partway through a run, because
  [`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md)
  [`setwd()`](https://rdrr.io/r/base/getwd.html)’d into it and then
  built further paths from the same (now stale-relative) variable. This
  was worked around in the test harness back in Phase 0/1 by always
  passing an absolute path; it is now fixed at the source, and a new
  regression test (`test-relative-workdir.R`) exercises the previously-
  broken case directly.
- Rewrote
  [`increase_by_days()`](https://markspan.github.io/ACTman/reference/increase_by_days.md)
  using
  [`lubridate::days()`](https://lubridate.tidyverse.org/reference/period.html)
  (calendar-based Period arithmetic) instead of manual
  `gmtoff`-comparison DST correction. Same public behavior (verified by
  the existing DST unit test), 4 lines instead of ~15. Added `lubridate`
  to `Imports`.

### Phase 3: modular file split + full manual

No behavior change (verified against all characterization/unit tests at
every step). Purely structural + documentation:

- Split `nparcalc.R` into `circadian_metrics.R` (IS/IV/RA/L5/M10),
  `ews_metrics.R`
  (Mean/Variance/SD/CoV/Skewness/Kurtosis/Autocorrelation/
  Time-to-Recovery), and a thin `nparcalc.R` wrapper that handles
  device/window detection and delegates to the two. Both new functions
  are pure (no I/O, no printing, no
  [`setwd()`](https://rdrr.io/r/base/getwd.html)).
- Extracted
  [`score_epochs()`](https://markspan.github.io/ACTman/reference/score_epochs.md)
  (`sleep_scoring.R`) from
  [`sleepdata_overview()`](https://markspan.github.io/ACTman/reference/sleepdata_overview.md):
  the per-epoch wake/sleep/mobile classification and sleep-chance/
  wakeup-chance rolling indicators, now a standalone pure function.
- Extracted
  [`run_rolling_window()`](https://markspan.github.io/ACTman/reference/run_rolling_window.md)
  (`rolling_window.R`) from
  [`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md)’s
  previously-inline moving-window closure.
- Renamed files for consistency: `actman.r` -\> `actman.R`,
  `plot_actogram.R` -\> `actogram.R`, `sleeplog_from_markers.R` -\>
  `sleeplog.R`, `sleepdata_overview.R` -\> `sleep_summary.R`.
- Added `utils.R` for shared constants (`MINUTES_PER_DAY`,
  `L5_WINDOW_MINUTES`, `M10_WINDOW_MINUTES`, `MAX_AUTOCORR_LAG`).
- Added test coverage for
  [`run_rolling_window()`](https://markspan.github.io/ACTman/reference/run_rolling_window.md)
  (previously the moving-window path had zero test coverage).
- Rewrote `README.md` as a full manual: installation, data format specs
  for both supported devices, worked examples for every major workflow,
  a package architecture map, a function-by-function reference, and the
  scientific references the calculations are based on (Van Someren et
  al. 1999; Witting et al. 1990; Cole et al. 1992; Scheffer et al. 2009;
  Van de Leemput et al. 2014).

### Phase 2: bug fixes and non-interactive operation

Baseline: `compsy/ACTman@66d8f69`.

#### Fixed

- [`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md):
  an empty/no-match `workdir` (no `.csv` files after excluding
  sleeplog/markers) previously produced a cryptic
  `arguments imply differing number of rows` or `1:0`-indexing error
  partway through the run. Now fails fast with a clear message.
- [`ACTman()`](https://markspan.github.io/ACTman/reference/ACTman.md):
  main file loop used `for (i in 1:length(ACTdata.files))`, which breaks
  (`1:0` == `c(1, 0)`) if the file list is ever empty. Changed to
  [`seq_along()`](https://rdrr.io/r/base/seq.html).
- [`sleepdata_overview()`](https://markspan.github.io/ACTman/reference/sleepdata_overview.md):
  `for (a in 1:loop_steps)` had the same `1:0` risk when `loop_steps` is
  0; changed to [`seq_len()`](https://rdrr.io/r/base/seq.html).
- [`sleepdata_overview()`](https://markspan.github.io/ACTman/reference/sleepdata_overview.md):
  the `is.null(sleepend)` check could never be TRUE, since
  [`tail()`](https://rdrr.io/r/utils/head.html) of an empty data frame
  returns a 0-row frame, not `NULL`. Changed to `nrow(sleepend) == 0`,
  so the intended sleeplog-Gotup fallback actually runs on genuinely
  empty results.
- [`sleepdata_overview()`](https://markspan.github.io/ACTman/reference/sleepdata_overview.md):
  the fallback assignment `rownr.sleep.end <- rownr.Gotup` for an
  unresolved sleep-end index was written *after* a `next()` call in the
  same block, making it unreachable dead code (the day was always
  skipped instead of using the fallback). Removed the `next()` so the
  fallback takes effect as originally intended.

#### Changed (interactive -\> parameterized)

- Removed both [`readline()`](https://rdrr.io/r/base/readline.html)
  prompts, which caused the pipeline to hang indefinitely in any
  non-interactive context (scripts, CI, Rscript).
  - `ACTman(..., on_high_missings = c("continue", "abort"))`: replaces
    the “more than 0.01% missing, continue? y/n” prompt. Default
    `"continue"` preserves the previous most-common manual answer.
  - `ACTman(..., on_missing_markers = c("median", "manual", "abort"))` /
    `sleeplog_from_markers(..., on_missing_markers = ...)`: replaces the
    “m/n/f/q” prompt for missing Bedtime/Gotup markers. Default
    `"median"` preserves the previous most-common manual answer (median
    imputation). `"manual"` still opens
    [`fix()`](https://rdrr.io/r/utils/fix.html) for hand-editing, but
    now errors clearly if called outside an interactive session instead
    of hanging.

#### Added

- Synthetic Actiwatch2 and MW8 fixtures (`tests/fixtures/`) and
  characterization tests (`tests/testthat/test-characterization.R`) that
  freeze the pre-Phase-2 numeric output of the circadian-analysis
  pipeline, to catch unintended behavior changes in later refactoring
  phases.
- Unit tests for
  [`increase_by_days()`](https://markspan.github.io/ACTman/reference/increase_by_days.md)
  (including a DST spring-forward case),
  [`nparcalc()`](https://markspan.github.io/ACTman/reference/nparcalc.md)
  (IS/IV/RA/L5/M10 sanity and consistency checks, NA handling, 2-column
  vs 3-column CRV.data equivalence), an end-to-end
  [`sleepdata_overview()`](https://markspan.github.io/ACTman/reference/sleepdata_overview.md)
  integration test on a synthetic 2-night fixture, and
  parameter-validation tests for the new non-interactive arguments.

None of the fixes above changed the circadian-analysis (`IS`/`IV`/`RA`/
`L5`/`M10`) output on the existing characterization fixtures; those bugs
only manifest on inputs the fixtures didn’t previously exercise (empty
directories, unresolved sleep-end indices).
