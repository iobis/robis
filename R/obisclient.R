#' obisclient: R client for the OBIS API
#'
#' Work in progress
#'
#' @docType package
#' @name obisclient
NULL




.url <- function(local) {
  # options(obisclient_url) <- "http://127.0.0.1:8090/"
  getOption("obisclient_url", "http://api.iobis.org/")
}

#' Find occurrences.
#'
#' @param scientificname
#' @param year
#' @param obisid
#' @param aphiaid
#' @param startdate
#' @param enddate
#' @param geometry A wkt geometry string.
#' @return The occurrences.
#' @examples
#' occurrence(scientificname = "Abra sibogai")
#' @export
occurrence <- function(
  scientificname=NULL,
  year=NULL,
  obisid=NULL,
  aphiaid=NULL,
  startdate=NULL,
  enddate=NULL,
  geometry=NULL,
  verbose=FALSE) {

  baseurl <- paste0(.url(local), "occurrence?")

  if (!is.null(scientificname)) {
    baseurl <- paste0(baseurl, "&scientificname=", scientificname)
  }

  if (!is.null(year)) {
    baseurl <- paste0(baseurl, "&year=", year)
  }

  if (!is.null(obisid)) {
    baseurl <- paste0(baseurl, "&obisid=", obisid)
  }

  if (!is.null(aphiaid)) {
    baseurl <- paste0(baseurl, "&aphiaid=", aphiaid)
  }

  if (!is.null(startdate)) {
    baseurl <- paste0(baseurl, "&startdate=", startdate)
  }

  if (!is.null(geometry)) {
    baseurl <- paste0(baseurl, "&geometry=", geometry)
  }

  offset <- 0
  i <- 1
  lastpage <- FALSE
  total <- 0
  datalist <- list()

  while (!lastpage) {
    url <- paste0(baseurl, "&offset=", format(offset, scientific=FALSE))

    result <- httr::GET(URLencode(url), httr::user_agent("obisclient - https://github.com/iobis/obisclient"))
    httr::stop_for_status(result)
    res <- httr::content(result, simplifyVector=TRUE)

    limit <- res$limit
    offset <- offset + limit
    lastpage <- res$lastpage
    datalist[[i]] <- res$results
    total <- total + nrow(res$results)
    if (verbose) {
      cat(url, "\n")
    }
    cat("\rRetrieved ", total, " records of ", res$count, " (", floor(total/res$count*100),"%)", sep="")
    i <- i + 1
  }
  cat("\n")
  data <- rbind.fill(datalist)
  return(data)
}
