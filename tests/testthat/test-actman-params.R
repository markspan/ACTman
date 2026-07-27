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
