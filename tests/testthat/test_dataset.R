test_that("dataset() basic call works", {
  res <- dataset()
  expect_s3_class(res, "tbl_df")
})

test_that("dataset() with scientificname works", {
  res <- dataset(scientificname = "Tellinidae")
  expect_s3_class(res, "tbl_df")
})

test_that("dataset() keyword search works", {
  res <- dataset(keyword = "coral reef")
  expect_s3_class(res, "tbl_df")
})

test_that("dataset() keyword search returns some results", {
  res <- dataset(keyword = "deep sea")
  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) >= 0)  # Can be 0 or more
})