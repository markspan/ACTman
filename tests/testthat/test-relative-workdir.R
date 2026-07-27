test_that("ACTman works with a relative workdir (no more setwd()-based fragility)", {
  ## This is the exact failure mode noted in test-characterization.R's
  ## comments: previously, ACTman() setwd()'d into workdir and then built
  ## further paths from the same (possibly relative) variable, so a
  ## relative workdir resolved against a moving target and broke partway
  ## through the run. actman_paths() normalizes to an absolute path up
  ## front, so this should now just work.
  parent_dir <- withr::local_tempdir()
  withr::local_dir(parent_dir)
  dir.create("relative_data_dir")

  times <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), by = "min", length.out = 16 * 1440)
  activity <- round(50 + 40 * sin((as.numeric(format(times, "%H")) * 60 +
    as.numeric(format(times, "%M")) - 6 * 60) / 1440 * 2 * pi))
  data_rows <- data.frame(V1 = seq_along(times), V2 = "Epoch", V3 = "",
                         V4 = format(times, "%d/%m/%Y"), V5 = format(times, "%H:%M:%S"), V6 = activity)
  write.table(data_rows, file.path("relative_data_dir", "P01.csv"), sep = ",",
              row.names = FALSE, col.names = FALSE, quote = FALSE, na = "NA")

  ## "relative_data_dir" (no leading / or drive letter) is a relative path
  ## from the current working directory set by withr::local_dir() above.
  result <- expect_no_error(
    ACTman(workdir = "relative_data_dir", myACTdevice = "Actiwatch2",
           iwantsleepanalysis = FALSE, plotactogram = FALSE, circadian_analysis = TRUE)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(file.exists(file.path("relative_data_dir", "Results", "ACTdata_overview.csv")))
})
