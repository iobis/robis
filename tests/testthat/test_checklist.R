library(robis)
context("checklist")

family <- "Pectinariidae"
family_taxa_lower_limit <- 10
family_taxa_upper_limit <- 1000

test_that("scientificname restricts the checklist by scientificname", {
  taxa <- checklist(scientificname = family, verbose = TRUE)
  expect_gt(nrow(taxa), family_taxa_lower_limit)
  expect_lt(nrow(taxa), family_taxa_upper_limit)
  expect_true(all(taxa$family == family))
})
