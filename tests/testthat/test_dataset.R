library(testthat)

test_that("Scientific name search returns results", {
  res <- dataset(scientificname = "Tellinidae")
  expect_s3_class(res, "tbl_df")
  expect_gt(nrow(res), 0)
})

test_that("Area ID filter returns results", {
  res <- dataset(areaid = 10181)
  expect_s3_class(res, "tbl_df")
  expect_gt(nrow(res), 0)
})

test_that("Keyword-only search returns results", {
  res <- dataset(keyword = "coral")
  expect_s3_class(res, "tbl_df")
  expect_gt(nrow(res), 0)
})

test_that("Keyword with logical syntax works", {
  res <- dataset(keyword = '"Gulf of Mexico" OR sponge -juvenile')
  expect_s3_class(res, "tbl_df")
})

test_that("Keyword and structured filters trigger error", {
  expect_error(
    dataset(keyword = "coral", scientificname = "Tellinidae"),
    regexp = "no other filter parameters may be specified"
  )
})

test_that("Invalid keyword returns empty result safely", {
  res <- dataset(keyword = "NOT_A_REAL_TERM_123456")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
})

test_that("validate_keyword detects unbalanced quotes", {
  expect_warning(validate_keyword('"coral reef'), "Unbalanced")
})

test_that("validate_keyword warns on unsupported wildcards", {
  expect_warning(validate_keyword("*star*"), "Double-sided wildcards")
})

test_that("Grouped OR query returns results", {
  res <- dataset(keyword = "(coral | sponge)")
  expect_s3_class(res, "tbl_df")
})
