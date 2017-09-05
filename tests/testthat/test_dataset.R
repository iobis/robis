library(robis)
context("dataset")

test_ids <- seq(2500, 2520)

test_that("dataset returns requested dataset records", {
  datasets <- dataset(test_ids)
  expect_gt(nrow(datasets), 1)
  expect_lt(nrow(datasets), 30)
  expect_true(all(datasets$id %in% test_ids))
})
