## Integration-level test for sleepdata_overview(): a small hand-crafted
## activity series with clear day/night activity contrast and a matching
## sleeplog. This exercises the real control flow (score-based wake/sleep
## classification, sleep.chance/wakeup.chance, and the two bug fixes from
## Phase 2: the nrow(sleepend) == 0 guard and the rownr.sleep.end fallback),
## rather than just checking numeric output against a frozen baseline.

make_sleep_fixture <- function(dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  ## 5 days of 1-minute activity: awake (high counts) 07:00-23:00,
  ## asleep (low counts) 23:00-07:00. Starts at 15:00 on day 1 so the
  ## first 12:00:00 timestamp (used to anchor "night 1") falls the
  ## following day, matching how the function expects data to begin
  ## mid-recording rather than exactly at midnight.
  start <- as.POSIXct("2020-01-01 15:00:00", tz = "UTC")
  times <- seq(start, by = "min", length.out = 5 * 1440)
  hm <- as.numeric(format(times, "%H")) + as.numeric(format(times, "%M")) / 60
  awake <- hm >= 7 & hm < 23
  activity <- ifelse(awake, 80, 2)

  actdata <- data.frame(Date = times, Activity = activity)

  sleeplog <- data.frame(
    Date = c("2020-01-01", "2020-01-02"),
    Bedtime = c("23:00", "23:00"),
    Gotup = c("07:00", "07:00")
  )
  write.table(sleeplog, file.path(dir, "P01-sleeplog.csv"),
              sep = "\t", row.names = FALSE, quote = FALSE)

  list(actdata = actdata, dir = dir)
}

test_that("sleepdata_overview runs end-to-end on a clean 2-night fixture without error", {
  fixture_dir <- withr::local_tempdir()
  fx <- make_sleep_fixture(fixture_dir)
  withr::local_dir(fx$dir)

  result <- expect_no_error(
    sleepdata_overview(workdir = fx$dir, actdata = fx$actdata, i = 1,
                       lengthcheck = FALSE, ACTdata.files = "P01.csv")
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("sleep.efficiency", "sleep.latency", "timeinbed") %in% names(result)))
})

test_that("sleepdata_overview produces a plausible sleep efficiency for a clean signal", {
  fixture_dir <- withr::local_tempdir()
  fx <- make_sleep_fixture(fixture_dir)
  withr::local_dir(fx$dir)

  result <- sleepdata_overview(workdir = fx$dir, actdata = fx$actdata, i = 1,
                               lengthcheck = FALSE, ACTdata.files = "P01.csv")

  eff <- suppressWarnings(as.numeric(result$sleep.efficiency))
  ## With near-zero activity throughout every scored "asleep" window, the first
  ## night's sleep efficiency should be high (the classifier should not be
  ## reporting mostly-wake for an obviously quiet night).
  expect_true(!is.na(eff[1]) && eff[1] > 70, info = paste("efficiency was", eff[1]))
})

test_that("sleepdata_overview errors clearly when neither sleeplog nor markers file is present", {
  fixture_dir <- withr::local_tempdir()
  fx <- make_sleep_fixture(fixture_dir)
  file.remove(file.path(fx$dir, "P01-sleeplog.csv"))
  withr::local_dir(fx$dir)

  expect_error(
    sleepdata_overview(workdir = fx$dir, actdata = fx$actdata, i = 1,
                       lengthcheck = FALSE, ACTdata.files = "P01.csv")
  )
})

test_that("sleepdata_overview computes consistent assumed_sleep/actual_sleep_perc across nights for identical nightly patterns", {
  ## Regression test for a bug where rownr.sleep.end was derived from
  ## as.numeric(rownames(...)) instead of a which()-based position within
  ## `aaa`. Row names are inherited from the full multi-night dataset and
  ## only happen to equal the in-window position for the first night, so
  ## every subsequent night's assumed_sleep/actual_sleep_perc/
  ## actual_wake_perc were silently corrupted (e.g. "29 hours" of assumed
  ## sleep in a single night) while sleep.efficiency/timeinbed stayed
  ## correct via an unrelated safety-net branch -- which is exactly why it
  ## went unnoticed. With two nights of *identical* activity patterns,
  ## every metric should come out identical too.
  fixture_dir <- withr::local_tempdir()
  fx <- make_sleep_fixture(fixture_dir)
  withr::local_dir(fx$dir)

  result <- sleepdata_overview(workdir = fx$dir, actdata = fx$actdata, i = 1,
                               lengthcheck = FALSE, ACTdata.files = "P01.csv")

  expect_equal(nrow(result), 2)
  assumed_sleep <- suppressWarnings(as.numeric(result$assumed_sleep))
  actual_sleep_perc <- suppressWarnings(as.numeric(result$actual_sleep_perc))
  actual_wake_perc <- suppressWarnings(as.numeric(result$actual_wake_perc))

  expect_equal(assumed_sleep[1], assumed_sleep[2], tolerance = 1e-6)
  expect_equal(actual_sleep_perc[1], actual_sleep_perc[2], tolerance = 1e-6)
  expect_equal(actual_wake_perc[1], actual_wake_perc[2], tolerance = 1e-6)

  ## And they should be plausible values, not (e.g.) 29 hours of sleep or a
  ## negative/over-100 percentage.
  expect_true(all(assumed_sleep > 0 & assumed_sleep < 24))
  expect_true(all(actual_sleep_perc >= 0 & actual_sleep_perc <= 100))
  expect_true(all(actual_wake_perc >= 0 & actual_wake_perc <= 100))
})

test_that("sleepdata_overview rejects an invalid on_missing_markers value", {
  fixture_dir <- withr::local_tempdir()
  fx <- make_sleep_fixture(fixture_dir)
  withr::local_dir(fx$dir)

  expect_error(
    sleepdata_overview(workdir = fx$dir, actdata = fx$actdata, i = 1,
                       lengthcheck = FALSE, ACTdata.files = "P01.csv",
                       on_missing_markers = "not_a_real_option")
  )
})
