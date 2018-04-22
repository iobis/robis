library(robis)
context("dataset")

test_ids <- seq(2500, 2520)

test_that("dataset returns requested dataset records", {
  datasets <- dataset(test_ids, verbose = TRUE)
  expect_gt(nrow(datasets), 1)
  expect_lt(nrow(datasets), 30)
  expect_true(all(datasets$id %in% test_ids))
})

test_that("datasets query parameter can be used", {
  d2 <- dataset(q = "MICROBIS")
  d1 <- dataset(id = d2$id[1])

  expect_equal(d1, d2)
  d3 <- dataset(startdate = "2015-01-01", scientificname = "Cetacea")
  expect_gt(nrow(d3), 10)
})

test_that("datasets id + query fails", {
  expect_error(dataset(id = 52, q = "MICROBIS"))
})
