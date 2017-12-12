library(robis)
context("plot")

test_that("leafletmap works", {
  xy <- data.frame(decimalLongitude=0:1, decimalLatitude=0:1, popup=2:3)
  p <- leafletmap(xy)
  expect_is(p,"leaflet")
  p <- leafletmap(xy, popup = "popup")
  expect_is(p,"leaflet")
})
