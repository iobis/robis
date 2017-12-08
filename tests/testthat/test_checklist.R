library(robis)
context("checklist")

test_genus <- "Abra"

test_that("checklist returns multiple records for a genus", {
  records <- checklist(scientificname = test_genus, verbose = TRUE)
  expect_gt(nrow(records), 1)
  expect_lt(nrow(records), 1000)
  expect_true(is.data.frame(records))
  expect_true(all(records$genus == test_genus))
})

test_that("checklist returns multiple records for a group", {
  g <- group()
  groupid <- g[g$name == "Seagrasses",]$id
  records <- checklist(groupid=groupid, verbose = TRUE)
  expect_gt(nrow(records), 1)
  expect_lt(nrow(records), 200)
  expect_true(is.data.frame(records))
  expect_true("Zostera" %in% unique(records$genus))
})
