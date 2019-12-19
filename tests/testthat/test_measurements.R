library(robis)
context("measurements")

species_nomof <- "Abra sibogai"
species_mof <- "Abra segmentum"

test_that("occurrence with mof = TRUE returns mof column", {
  records <- occurrence(scientificname = species_nomof, mof = TRUE)
  expect_true("mof" %in% names(records))
})

test_that("mof table has expected columns", {
  records <- occurrence(scientificname = species_mof, mof = TRUE)
  expect_true("mof" %in% names(records))
  m <- measurements(records)
  expect_true(is.data.frame(m))
  expect_true(all(c("measurementType", "measurementValue") %in% names(m)))
})

test_that("measurements on dataframe with mof column returns dataframe", {
  records <- occurrence(scientificname = species_nomof, mof = TRUE)
  m <- measurements(records)
  expect_true(is.data.frame(m))
})

test_that("measurements on dataframe without mof column returns NULL", {
  records <- occurrence(scientificname = species_nomof, mof = FALSE)
  suppressWarnings({
    m <- measurements(records)
  })
  expect_true(is.null(m))
})

