library(robis)
context("occurrence")

options(robis_log_usage = FALSE)

small_species <- "Abra sibogai"
small_taxonid <- 345684
small_datasetid <- "7d73a408-cb0b-48d3-8b08-ae1f25b0b729"
medium_species <- "Abra nitida"
medium_taxonid <- 141435
small_record_limit <- 100000
absence_species <- "Hippocampus erectus"

test_that("occurrence returns small number of records for a scientific name", {
  skip_on_cran()
  records <- occurrence(scientificname = small_species, verbose = TRUE)
  expect_gt(nrow(records), 0)
  expect_lt(nrow(records), small_record_limit)
  expect_true(all(records$species == small_species))
  expect_true(all(records$aphiaID == small_taxonid))
})

test_that("occurrence returns small number of records for an aphia id", {
  skip_on_cran()
  records <- occurrence(taxonid = small_taxonid)
  expect_gt(nrow(records), 0)
  expect_lt(nrow(records), small_record_limit)
  expect_true(all(records$species == small_species))
  expect_true(all(records$aphiaID == small_taxonid))
})

test_that("occurrence with absence = TRUE returns only absence records", {
  skip_on_cran()
  records <- occurrence(scientificname = absence_species, absence = TRUE)
  expect_gt(nrow(records), 0)
  expect_true(all(records$absence == TRUE))
})

test_that("startdate restricts results by date", {
  skip_on_cran()
  records <- occurrence(taxonid = medium_taxonid, startdate = "2010-01-01")
  expect_gt(nrow(records), 0)
  expect_true(all(records$date_year >= 2010))
})

test_that("enddate restricts results by date", {
  skip_on_cran()
  records <- occurrence(taxonid = medium_taxonid, enddate = "2002-12-31")
  expect_gt(nrow(records), 0)
  expect_true(all(records$date_year <= 2002))
})

test_that("datasetid restricts results by dataset", {
  skip_on_cran()
  records <- occurrence(datasetid = small_datasetid)
  expect_gt(nrow(records), 0)
  expect_lt(nrow(records), small_record_limit)
})

test_that("fields restricts the fields returned", {
  skip_on_cran()
  records <- occurrence(datasetid = small_datasetid, fields = c("id", "scientificName"))
  expect_gt(nrow(records), 0)
  expect_true(length(names(records)) == 2)
  records <- occurrence(datasetid = small_datasetid, fields = c("scientificName"))
  expect_gt(nrow(records), 0)
  expect_true(length(names(records)) == 2)
})
