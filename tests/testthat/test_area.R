library(robis)
context("area")

test_that("area api returns records", {
  d <- area()
  expect_gt(nrow(d), 100)
  expect_gte(ncol(d), 2)
  expect_true("id" %in% colnames(d))
})
