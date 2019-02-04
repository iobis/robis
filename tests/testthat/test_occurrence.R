library(robis)
context("occurrence")

small_species <- "Abra sibogai"
small_taxonid <- 345684
small_datasetid <- "7d73a408-cb0b-48d3-8b08-ae1f25b0b729"
medium_species <- "Abra alba"
medium_taxonid <- 141433
small_record_limit <- 100000

test_that("occurrence returns small number of records for a scientific name", {
  records <- occurrence(scientificname = small_species, verbose = TRUE)
  expect_gt(nrow(records), 0)
  expect_lt(nrow(records), small_record_limit)
  expect_true(all(records$species == small_species))
  expect_true(all(records$aphiaID == small_taxonid))
})

test_that("occurrence returns small number of records for an aphia id", {
  records <- occurrence(taxonid = small_taxonid)
  expect_gt(nrow(records), 0)
  expect_lt(nrow(records), small_record_limit)
  expect_true(all(records$species == small_species))
  expect_true(all(records$aphiaID == small_taxonid))
})

test_that("startdate restricts results by date", {
  records <- occurrence(taxonid = medium_taxonid, startdate = "2010-01-01")
  expect_gt(nrow(records), 0)
  expect_true(all(records$date_year >= 2010))
})

test_that("startdate restricts results by date", {
  records <- occurrence(taxonid = medium_taxonid, enddate = "2009-12-31")
  expect_gt(nrow(records), 0)
  expect_true(all(records$date_year <= 2009))
})

test_that("datasetid restricts results by dataset", {
  records <- occurrence(datasetid = small_datasetid)
  expect_gt(nrow(records), 0)
  expect_lt(nrow(records), small_record_limit)
})
