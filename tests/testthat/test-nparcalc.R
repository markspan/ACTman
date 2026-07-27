## Helper: builds a perfectly repeating 3-day, 1-minute-epoch activity series
## (same pattern every day) so IS/IV/L5/M10 have known, checkable properties,
## without having to hand-derive the exact numeric values of the full formula.
make_synthetic_days <- function(n_days = 3, seed = 1) {
  set.seed(seed)
  times <- seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
               by = "min", length.out = n_days * 1440)
  minute_of_day <- as.numeric(format(times, "%H")) * 60 + as.numeric(format(times, "%M"))
  ## Deterministic daily pattern: low at night, high in the day, identical
  ## every day (no noise), so interdaily stability should be (near) maximal.
  pattern <- 50 + 40 * sin((minute_of_day - 6 * 60) / 1440 * 2 * pi)
  data.frame(Date = format(times, "%Y-%m-%d %H:%M:%S"), Activity = round(pattern),
             stringsAsFactors = FALSE)
}

make_actdata_1_sub <- function(crv) {
  ## nparcalc() locates CRV.data.end via ACTdata.1.sub$Date for Actiwatch2;
  ## reuse the same series so start/end detection matches.
  data.frame(Date = crv$Date, Activity = crv$Activity, stringsAsFactors = FALSE)
}

test_that("nparcalc returns IS close to 1 for a perfectly repeating daily pattern", {
  crv <- make_synthetic_days(n_days = 3)
  actdata <- make_actdata_1_sub(crv)

  result <- nparcalc(myACTdevice = "Actiwatch2", movingwindow = FALSE,
                     CRV.data = crv, ACTdata.1.sub = actdata)

  expect_true(is.list(result))
  expect_true(result$IS > 0.9, info = paste("IS was", result$IS))
})

test_that("nparcalc RA is consistent with its own L5/M10 outputs", {
  crv <- make_synthetic_days(n_days = 3)
  actdata <- make_actdata_1_sub(crv)

  result <- nparcalc(myACTdevice = "Actiwatch2", movingwindow = FALSE,
                     CRV.data = crv, ACTdata.1.sub = actdata)

  expected_ra <- (result$M10 - result$L5) / (result$L5 + result$M10)
  expect_equal(result$RA, expected_ra, tolerance = 1e-6)
  expect_true(result$M10 > result$L5, info = "M10 (most active) should exceed L5 (least active)")
})

test_that("nparcalc Mean/SD/CoV match direct computation on the windowed data", {
  crv <- make_synthetic_days(n_days = 3)
  actdata <- make_actdata_1_sub(crv)

  result <- nparcalc(myACTdevice = "Actiwatch2", movingwindow = FALSE,
                     CRV.data = crv, ACTdata.1.sub = actdata)

  ## Recompute directly on the same windowed slice nparcalc used internally
  ## (start of first full day to the last full 00:00:00 stamp) to confirm
  ## the summary stats aren't silently using the wrong subset.
  windowed_activity <- result$CRV_data[, "Activity"]
  expect_equal(result$Mean, round(mean(windowed_activity, na.rm = TRUE), 2))
  expect_equal(result$SD, round(sd(windowed_activity, na.rm = TRUE), 2))
})

test_that("nparcalc IV is low for a smoothly varying (non-jagged) pattern", {
  crv <- make_synthetic_days(n_days = 3)
  actdata <- make_actdata_1_sub(crv)

  result <- nparcalc(myACTdevice = "Actiwatch2", movingwindow = FALSE,
                     CRV.data = crv, ACTdata.1.sub = actdata)

  expect_true(result$IV >= 0)
  expect_true(result$IV < 0.5, info = paste("IV was", result$IV))
})

test_that("nparcalc handles NA activity values without erroring", {
  crv <- make_synthetic_days(n_days = 3)
  na_idx <- sample(seq_len(nrow(crv)), size = 20)
  crv$Activity[na_idx] <- NA
  actdata <- make_actdata_1_sub(crv)

  result <- expect_no_error(
    nparcalc(myACTdevice = "Actiwatch2", movingwindow = FALSE,
             CRV.data = crv, ACTdata.1.sub = actdata)
  )
  expect_false(is.na(result$IS))
  expect_false(is.na(result$Mean))
})

test_that("nparcalc columns: 3-column CRV.data (Date, Time, Activity) is combined correctly", {
  crv2 <- make_synthetic_days(n_days = 3)
  crv3 <- data.frame(Date = substr(crv2$Date, 1, 10), Time = substr(crv2$Date, 12, 19),
                     Activity = crv2$Activity, stringsAsFactors = FALSE)
  actdata <- make_actdata_1_sub(crv2)

  result3col <- nparcalc(myACTdevice = "Actiwatch2", movingwindow = FALSE,
                         CRV.data = crv3, ACTdata.1.sub = actdata)
  result2col <- nparcalc(myACTdevice = "Actiwatch2", movingwindow = FALSE,
                         CRV.data = crv2, ACTdata.1.sub = actdata)

  expect_equal(result3col$IS, result2col$IS)
  expect_equal(result3col$L5, result2col$L5)
  expect_equal(result3col$M10, result2col$M10)
})
