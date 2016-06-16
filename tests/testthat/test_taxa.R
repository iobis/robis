library(robis)
context("taxa")

test_genus <- "Abra"

test_that("taxa returns multiple records for a genus", {
  records <- taxa(scientificname = test_genus, verbose = TRUE)
  expect_gt(nrow(records), 1)
  expect_lt(nrow(records), 1000)
  expect_true(is.data.frame(records))
  expect_true(all(records$genus == test_genus))
})
