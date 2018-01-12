context("utilities")

test_that("time_to_fingerprint and fingerprint_to_time", {
  date <- as.POSIXct("2018-01-01 12:00:00 CET")
  fp <- time_to_fingerprint(date)
  expect_identical(time_to_fingerprint(date), "5A4A14B0")
  expect_identical(as.numeric(fingerprint_to_time(date)), as.numeric(date))
  expect_identical(time_to_fingerprint(fingerprint_to_time(fp)), fp)
  rm(date, fp)
})
