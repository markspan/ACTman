test_that("ACTman() end-to-end with plotactogram set works on real device-format data", {
  ## Regression test: plot_actogram()'s own unit tests (test-actogram.R) call
  ## it directly with a raw POSIXct Date column, which is not how ACTman()
  ## actually calls it -- by the time ACTman() calls plot_actogram(), Date
  ## has already been reformatted to a character string (for writing the
  ## managed dataset). A fix to plot_actogram()'s date handling that only
  ## worked for the direct-call (POSIXct) case broke the real ACTman()
  ## integration path silently, since no test exercised ACTman(...,
  ## plotactogram = ...) end-to-end with real data. Uses the bundled real
  ## (anonymized) example MW8 export for good measure.
  fixture_dir <- withr::local_tempdir()
  file.copy(
    system.file("extdata", "example-mw8-participant.csv", package = "ACTman"),
    file.path(fixture_dir, "participant01.csv")
  )

  result <- expect_no_error(
    ACTman(workdir = fixture_dir, myACTdevice = "MW8", circadian_analysis = TRUE,
          iwantsleepanalysis = FALSE, plotactogram = "24h", lengthcheck = FALSE)
  )

  expect_s3_class(result, "actman_result")
  expect_s3_class(result$circadian, "data.frame")
  expect_true(file.exists(file.path(fixture_dir, "Actograms")))
})

test_that("ACTman rejects an unknown myACTdevice value", {
  fixture_dir <- withr::local_tempdir()
  file.create(file.path(fixture_dir, "P01.csv"))
  expect_error(
    ACTman(workdir = fixture_dir, myACTdevice = "NotARealDevice",
      iwantsleepanalysis = FALSE, circadian_analysis = FALSE, plotactogram = FALSE),
    "Unknown value for myACTdevice"
  )
})

test_that("ACTman errors clearly (rather than a cryptic 1:0 indexing error) on an empty workdir", {
  fixture_dir <- withr::local_tempdir()
  expect_error(
    ACTman(workdir = fixture_dir, myACTdevice = "Actiwatch2",
      iwantsleepanalysis = FALSE, circadian_analysis = FALSE, plotactogram = FALSE),
    "No actigraphy .csv files found"
  )
})

test_that("ACTman rejects an invalid on_high_missings value via match.arg", {
  fixture_dir <- withr::local_tempdir()
  file.create(file.path(fixture_dir, "P01.csv"))
  expect_error(
    ACTman(workdir = fixture_dir, myACTdevice = "Actiwatch2", on_high_missings = "nonsense")
  )
})

test_that("ACTman rejects an invalid on_missing_markers value via match.arg", {
  fixture_dir <- withr::local_tempdir()
  file.create(file.path(fixture_dir, "P01.csv"))
  expect_error(
    ACTman(workdir = fixture_dir, myACTdevice = "Actiwatch2", on_missing_markers = "nonsense")
  )
})

test_that("sleeplog_from_markers rejects an invalid on_missing_markers value", {
  expect_error(
    sleeplog_from_markers(workdir = tempdir(), i = 1, ACTdata.files = "P01.csv",
      on_missing_markers = "nonsense")
  )
})
