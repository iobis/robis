library(robis)
context("measurements")

species_nomof <- "Abra sibogai"
species_mof <- "Abra segmentum"

test_that("occurrence with mof = TRUE returns mof column", {
  skip_on_cran()
  records <- occurrence(scientificname = species_nomof, mof = TRUE)
  expect_true("mof" %in% names(records))
})

test_that("mof table has expected columns", {
  skip_on_cran()
  records <- occurrence(scientificname = species_mof, mof = TRUE)
  expect_true("mof" %in% names(records))
  m <- unnest_extension(records, extension = "MeasurementOrFact")
  expect_true(is.data.frame(m))
  expect_true(all(c("measurementType", "measurementValue") %in% names(m)))
})

test_that("measurements on dataframe with mof column returns dataframe", {
  skip_on_cran()
  records <- occurrence(scientificname = species_nomof, mof = TRUE)
  m <- unnest_extension(records, extension = "MeasurementOrFact")
  expect_true(is.data.frame(m))
})

test_that("measurements on dataframe without mof column returns NULL", {
  skip_on_cran()
  records <- occurrence(scientificname = species_nomof, mof = FALSE)
  suppressWarnings({
    m <- unnest_extension(records, extension = "MeasurementOrFact")
  })
  expect_true(is.null(m))
})

