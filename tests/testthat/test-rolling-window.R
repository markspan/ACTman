test_that("run_rolling_window produces one row per window with expected columns", {
  ## Reuse the same deterministic pattern as the nparcalc tests, extended to
  ## 5 days so a couple of 2-day windows fit. Uses separate Date/Time columns
  ## (3-column CRV.data), matching what the real pipeline's "MANAGED.txt"
  ## data actually looks like when it reaches this function -- the
  ## Actiwatch2 + moving-window branch inside nparcalc() locates the window
  ## end via the raw `Time` column, not a combined Date-Time string.
  set.seed(7)
  times <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), by = "min", length.out = 5 * 1440)
  minute_of_day <- as.numeric(format(times, "%H")) * 60 + as.numeric(format(times, "%M"))
  pattern <- 50 + 40 * sin((minute_of_day - 6 * 60) / 1440 * 2 * pi)
  crv <- data.frame(Date = format(times, "%Y-%m-%d"), Time = format(times, "%H:%M:%S"),
    Activity = round(pattern), stringsAsFactors = FALSE)
  actdata <- data.frame(Date = paste(crv$Date, crv$Time), Activity = crv$Activity,
    stringsAsFactors = FALSE)

  result <- expect_no_error(
    run_rolling_window(x = crv, window = 2 * 1440, jump = 1440,
      myACTdevice = "Actiwatch2", ACTdata.1.sub = actdata, verbose = FALSE)
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2)
  expect_true(all(c("IS", "IV", "RA", "L5", "M10", "Mean", "SD") %in% names(result)))
  expect_false(any(is.na(as.numeric(result$IS))))
})
