library(robis)
context("taxon")

test_that("taxon returns all requested taxa", {
  skip_on_cran()
  taxa <- taxon(c(141433, 141434))
  expect_equal(nrow(taxa), 2)
  expect_true(141433 %in% taxa$taxonID)
  expect_true(141434 %in% taxa$taxonID)
})
