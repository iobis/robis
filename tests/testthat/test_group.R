library(robis)
context("group")

test_that("group api returns records", {
  d <- group()
  expect_gt(nrow(d), 50)
  expect_gte(ncol(d), 2)
  expect_true("id" %in% colnames(d))
})
