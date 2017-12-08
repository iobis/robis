library(robis)
context("node")

test_that("node api returns records", {
  d <- node()
  expect_gt(nrow(d), 15)
  expect_gte(ncol(d), 2)
  expect_true("id" %in% colnames(d))
})
