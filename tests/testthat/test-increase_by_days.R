test_that("increase_by_days adds whole days correctly outside DST transitions", {
  result <- increase_by_days("2016-01-01 08:00:00", 5)
  expect_equal(as.Date(result), as.Date("2016-01-06"))
  expect_equal(format(result, "%H:%M:%S"), "08:00:00")
})

test_that("increase_by_days accepts a POSIXct input as well as a character string", {
  start <- as.POSIXct("2020-03-01 12:00:00", tz = "UTC")
  result <- increase_by_days(start, 3)
  expect_equal(as.Date(result), as.Date("2020-03-04"))
})

test_that("increase_by_days supports negative day counts", {
  result <- increase_by_days("2016-01-10 00:00:00", -3)
  expect_equal(as.Date(result), as.Date("2016-01-07"))
})

test_that("increase_by_days returns the same clock time across a DST spring-forward transition", {
  ## 2016-03-27 is the EU spring-forward date; local time skips 02:00-03:00.
  ## The function should still report the same wall-clock time on the target day.
  withr::local_timezone("Europe/Amsterdam")
  result <- increase_by_days("2016-03-25 09:00:00", 3)
  expect_equal(format(result, "%H:%M:%S"), "09:00:00")
  expect_equal(as.Date(result), as.Date("2016-03-28"))
})

test_that("increase_by_days with 0 days returns (approximately) the same time", {
  result <- increase_by_days("2016-06-15 10:30:00", 0)
  expect_equal(as.Date(result), as.Date("2016-06-15"))
  expect_equal(format(result, "%H:%M:%S"), "10:30:00")
})
