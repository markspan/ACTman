## Characterization tests: lock in the CURRENT pipeline's numeric output on
## synthetic fixtures. These exist to catch *unintended* behavior changes
## during refactoring -- they are not correctness tests, and some of the
## frozen values reflect known bugs that Phase 2 will intentionally fix
## (at which point the corresponding baseline file must be regenerated and
## the change documented in NEWS.md).

## NOTE: workdir must be an absolute path. ACTman() setwd()'s into workdir
## internally and then builds further paths from the *same* variable, so a
## relative path here resolves against a moving target and breaks partway
## through the run. This is the exact setwd() fragility flagged in the
## modernization plan (Phase 4: path-based I/O); it is left as-is here since
## Phase 2/4 is where it actually gets fixed, not the test harness.
fixtures_root <- normalizePath(testthat::test_path("..", "fixtures"))
baseline_root <- normalizePath(testthat::test_path("..", "characterization", "output"))

run_overview <- function(workdir, device) {
  ## ACTman() now returns a unified actman_result object (see NEWS.md,
  ## "Item 3: config object + unified return type"); $overview holds the
  ## same content the function used to return directly when
  ## circadian_analysis = TRUE, iwantsleepanalysis = FALSE, movingwindow =
  ## FALSE, so the existing frozen baselines are still valid unchanged.
  result <- ACTman(workdir = workdir, myACTdevice = device,
                   iwantsleepanalysis = FALSE, plotactogram = FALSE,
                   selectperiod = FALSE, movingwindow = FALSE,
                   circadian_analysis = TRUE, nparACT_compare = FALSE,
                   na_omit = FALSE, na_impute = FALSE, missings_report = FALSE,
                   lengthcheck = TRUE, i_want_EWS = FALSE)
  result$overview
}

test_that("Actiwatch2 pipeline output matches frozen baseline", {
  testthat::skip_if_not(dir.exists(file.path(fixtures_root, "actiwatch2_sample")))
  baseline_file <- file.path(baseline_root, "actiwatch2_overview.rds")
  testthat::skip_if_not(file.exists(baseline_file), "run generate_fixtures.R + run_baseline.R first")

  baseline <- readRDS(baseline_file)
  current <- suppressWarnings(suppressMessages(
    run_overview(file.path(fixtures_root, "actiwatch2_sample"), "Actiwatch2")
  ))
  testthat::expect_equal(current, baseline)
})

test_that("MW8 pipeline output matches frozen baseline", {
  testthat::skip_if_not(dir.exists(file.path(fixtures_root, "mw8_sample")))
  baseline_file <- file.path(baseline_root, "mw8_overview.rds")
  testthat::skip_if_not(file.exists(baseline_file), "run generate_fixtures.R + run_baseline.R first")

  baseline <- readRDS(baseline_file)
  current <- suppressWarnings(suppressMessages(
    run_overview(file.path(fixtures_root, "mw8_sample"), "MW8")
  ))
  testthat::expect_equal(current, baseline)
})
