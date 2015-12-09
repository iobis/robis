library(obisclient)
context("occurrence")

test_that("occurrence returns some records for Abra sibogai", {
  records <- occurrence(scientificname = "Abra sibogai")
  expect_more_than(nrow(records), 0)
  expect_true(is.data.frame(records))
})
