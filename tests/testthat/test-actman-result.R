test_that("ACTman returns a unified actman_result with the right fields populated per flags", {
  fixture_dir <- withr::local_tempdir()
  times <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), by = "min", length.out = 16 * 1440)
  activity <- round(50 + 40 * sin((as.numeric(format(times, "%H")) * 60 +
    as.numeric(format(times, "%M")) - 6 * 60) / 1440 * 2 * pi))
  data_rows <- data.frame(V1 = seq_along(times), V2 = "Epoch", V3 = "",
                         V4 = format(times, "%d/%m/%Y"), V5 = format(times, "%H:%M:%S"), V6 = activity)
  write.table(data_rows, file.path(fixture_dir, "P01.csv"), sep = ",",
              row.names = FALSE, col.names = FALSE, quote = FALSE, na = "NA")

  result <- ACTman(workdir = fixture_dir, myACTdevice = "Actiwatch2",
                   iwantsleepanalysis = FALSE, plotactogram = FALSE,
                   circadian_analysis = TRUE, movingwindow = FALSE)

  expect_s3_class(result, "actman_result")
  expect_true(is.list(result))
  expect_setequal(names(result), c("overview", "circadian", "sleep", "rolling_window"))

  expect_s3_class(result$overview, "data.frame")
  expect_s3_class(result$circadian, "data.frame")
  expect_true(all(c("IS", "IV", "RA", "L5", "M10") %in% names(result$circadian)))
  expect_null(result$sleep)
  expect_null(result$rolling_window)

  ## print.actman_result should summarize, not error, and return x invisibly.
  expect_output(print(result), "actman_result")
  expect_invisible(print(result))
})
