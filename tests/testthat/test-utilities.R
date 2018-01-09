context("utilities")

test_that("dont_ask returns current GUI state", {
  expect_identical(timeToFingerprint(as.POSIXct("2018-01-09 21:51:24 CET")), "5A552B4C")
})
