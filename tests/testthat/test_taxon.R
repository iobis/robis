library(robis)
context("taxon")

test_species <- list(aphiaid = 145551,
                     scientificname = "Himanthalia elongata")

check_result <- function(t, sp) {
  expect_equal(t$worms_id, test_species$aphiaid)
  expect_equal(t$tname, test_species$scientificname)
}

test_that("taxon api works", {
  t <- taxon(aphiaid = test_species$aphiaid)
  check_result(t, test_species)
  t <- taxon(scientificname = test_species$scientificname)
  check_result(t, test_species)
  t <- taxon(obisid = t$id, verbose = TRUE)
  check_result(t, test_species)
})

test_that("taxon failure scenarios", {
  expect_error({taxon()}, "Missing parameter")
  expect_error({taxon(scientificname = test_species$scientificname,
                      aphiaid = test_species$aphiaid)}, "many")
  expect_error({taxon(obisid=c(0,1,2))}, "one by one")
  expect_error({taxon(obisid="sldkjdslkf")})
  expect_warning({taxon(obisid=-1)}, "not found")
})

test_that("taxon get common names found", {
  t <- taxon(aphiaid = test_species$aphiaid)
  common <- robis::taxon_common(t$id)
  expect_gte(nrow(common), 3)
  expect_true(all(c("name", "language") %in% colnames(common)))
  expect_true("riemwier" %in% common$name)
  expect_true("Dutch" %in% common$language)
})

test_that("taxon get common names empty", {
  common <- robis::taxon_common(-1)
  expect_equal(length(common), 0)
})

test_that("taxon get common names failure scenarios", {
  expect_error(robis::taxon_common("azerty"),"number")
  expect_error(robis::taxon_common(c(1,2,3)),"number")
})
