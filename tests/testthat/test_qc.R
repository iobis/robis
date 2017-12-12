library(robis)
context("qc")

test_that("qcflags works", {
  expect_equal(qcflags(6, 1), 0)
  expect_equal(qcflags(6, 2), 1)
  expect_equal(qcflags(6, 3), 1)
})
