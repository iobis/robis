library(robis)
context("checklist")

test_genus <- "Abra"

test_that("checklist returns multiple records for a genus", {
  records <- checklist(scientificname = test_genus, verbose = TRUE)
  expect_gt(nrow(records), 1)
  expect_lt(nrow(records), 1000)
  expect_true(is.data.frame(records))
  expect_true(all(records$genus == test_genus))
})
