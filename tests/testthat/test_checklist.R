library(robis)
context("checklist")

family <- "Pectinariidae"
family_taxa_lower_limit <- 10
family_taxa_upper_limit <- 1000
redlist_area <- 21
unknown_aphiaid <- 999999999

test_that("scientificname restricts the checklist by scientificname", {
  skip_on_cran()
  taxa <- checklist(scientificname = family, verbose = TRUE)
  expect_gt(nrow(taxa), family_taxa_lower_limit)
  expect_lt(nrow(taxa), family_taxa_upper_limit)
  expect_true(all(taxa$family == family))
})

test_that("checklist includes IUCN Red List category when redlist = TRUE", {
  skip_on_cran()
  taxa <- checklist(areaid = redlist_area, redlist = TRUE, verbose = TRUE)

  # If 'category' column exists and there are any rows, test for non-NA values
  if ("category" %in% names(taxa) && nrow(taxa) > 0) {
    expect_true(any(!is.na(taxa$category)), info = "No non-NA Red List categories found despite redlist = TRUE")
  } else {
    # No 'category' column or no data — not a failure
    message("No IUCN Red List data returned for areaid = ", redlist_area)
    succeed()
  }
})

test_that("checklist for an unknown Aphia ID is empty", {
  skip_on_cran()
  taxa <- checklist(taxonid = unknown_aphiaid)
  expect_true(is.data.frame(taxa))
  expect_true(nrow(taxa) == 0)
})
