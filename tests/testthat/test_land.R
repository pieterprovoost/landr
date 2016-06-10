library(landr)
context("landr")

test_that("points on land have a positive distance", {
  distance <- land(lon = 3.1541, lat = 51.1028)
  expect_true(distance > 0)
})

test_that("points on sea have a zero distance", {
  distance <- land(lon = 2.6803, lat = 51.1836)
  expect_true(distance == 0)
})
