library(robis)
context("taxon")

test_genus <- "Abra"

test_that("taxon returns multiple records for a genus", {
  records <- taxon(scientificname = test_genus, verbose = TRUE)
  expect_more_than(nrow(records), 1)
  expect_less_than(nrow(records), 1000)
  expect_true(is.data.frame(records))
  expect_true(all(records$genus == test_genus))
})
