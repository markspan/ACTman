test_that("plot_actogram runs end-to-end and produces a PDF (24h)", {
  fixture_dir <- withr::local_tempdir()

  ## Build a 16-day, 1-minute-epoch activity series with a clean day/night
  ## contrast, matching the shape ACTdata.1.sub has by the time it reaches
  ## plot_actogram() inside ACTman().
  times <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), by = "min", length.out = 16 * 1440)
  hm <- as.numeric(format(times, "%H")) + as.numeric(format(times, "%M")) / 60
  activity <- ifelse(hm >= 7 & hm < 23, 80, 2)
  actdata <- data.frame(Date = times, Activity = activity)

  expect_no_error(
    plot_actogram(workdir = fixture_dir, ACTdata.1.sub = actdata, i = 1,
                 plotactogram = "24h", rollingwindow.results = NA, i_want_EWS = FALSE)
  )

  expect_true(file.exists(file.path(fixture_dir, "Actograms",
                                    "Actigraphy Data - 24h Plot 1 .pdf")))
})

test_that("plot_actogram rejects EWS request without rolling window results", {
  fixture_dir <- withr::local_tempdir()
  times <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), by = "min", length.out = 16 * 1440)
  actdata <- data.frame(Date = times, Activity = rep(10, length(times)))

  expect_error(
    plot_actogram(workdir = fixture_dir, ACTdata.1.sub = actdata, i = 1,
                 plotactogram = "24h", rollingwindow.results = NA, i_want_EWS = TRUE),
    "Cannot create EWS plot"
  )
})
