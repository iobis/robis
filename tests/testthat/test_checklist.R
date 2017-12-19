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

test_that("checklist returns multiple records for a year", {
  r1 <- checklist(year = 1802, verbose = TRUE)
  expect_warning({ r2 <- checklist(year = c(1803, "blabla"), verbose = TRUE) })
  records <- checklist(year = c(1802,1803), verbose = TRUE)
  expect_gt(nrow(records), 1)
  expect_lt(nrow(records), 1000)
  expect_true(is.data.frame(records))
  expect_equal(length(unique(c(r1$id, r2$id))), nrow(records))
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

test_that("checklist qc flags work", {
  r1 <- checklist(year = 1802, qc = 15:30)
  r2 <- checklist(year = 1802)
  expect_gt(NROW(r1), 0)
  expect_lt(NROW(r1), NROW(r2))
})

test_that("checklist allow multiple scientific names https://github.com/iobis/robis/issues/32", {
  taxa <- checklist(scientificname = c("Pterois volitans", "Voluta musica"))
  expect_equal(NROW(taxa), 2)
})

test_that("checklist WKT accross dateline slow https://github.com/iobis/robis/issues/24", {
  skip_on_cran()
  # only run test on Travis (sloooow test)
  if (identical(Sys.getenv("TRAVIS"), "true")) {
    al <- RCurl::getURLContent("https://gist.githubusercontent.com/jebyrnes/329f09b0ce2abc8710bade92f126d5e8/raw/345e93b90b9ab9d13fc8806c3d333d0d646806bc/aleutians.wkt")
    al_check <- checklist(geometry = al, verbose = TRUE)
    expect_gt(nrow(al_check), 500)
  } else {
    skip("Only run this slow test on TRAVIS but check once we are on OBIS 2.0")
  }
})
