#' Find occurrences.
#'
#' @usage occurrence(scientificname = NULL, taxonid = NULL, datasetid = NULL,
#'   nodeid = NULL, areaid = NULL, startdate = NULL, enddate = NULL,
#'   startdepth = NULL, enddepth = NULL, geometry = NULL, verbose = FALSE)
#' @param scientificname the scientific name.
#' @param taxonid the taxon identifier (WoRMS AphiaID).
#' @param datasetid the dataset identifier.
#' @param nodeid the OBIS node identifier.
#' @param areaid the OBIS area identifier.
#' @param startdate the earliest date on which occurrence took place.
#' @param enddate the latest date on which the occurrence took place.
#' @param startdepth the minimum depth below the sea surface.
#' @param enddepth the maximum depth below the sea surface.
#' @param geometry a WKT geometry string.
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return The occurrence records.
#' @examples
#' records <- occurrence(scientificname = "Abra sibogai")
#' records <- occurrence(taxonid = 141438, startdate = as.Date("2007-10-10"))
#' records <- occurrence(taxon = 141438, geometry = "POLYGON ((0 0, 0 45, 45 45, 45 0, 0 0))")
#' @export
occurrence <- function(
  scientificname = NULL,
  taxonid = NULL,
  datasetid = NULL,
  nodeid = NULL,
  areaid = NULL,
  startdate = NULL,
  enddate = NULL,
  startdepth = NULL,
  enddepth = NULL,
  geometry = NULL,
  verbose = FALSE
) {

  after <- "-1"
  result_list <- list()
  last_page <- FALSE
  i <- 1
  fetched <- 0

  while (!last_page) {

    query <- list(
      scientificname = handle_vector(scientificname),
      taxonid = handle_vector(taxonid),
      datasetid = handle_vector(datasetid),
      nodeid = handle_vector(nodeid),
      areaid = handle_vector(areaid),
      startdate = handle_date(startdate),
      enddate = handle_date(enddate),
      startdepth = startdepth,
      enddepth = enddepth,
      geometry = geometry,
      after = after,
      size = page_size()
    )

    result <- http_request("GET", "occurrence", query)

    if (verbose) {
      log_request(result)
    }

    stop_for_status(result)

    text <- content(result, "text", encoding = "UTF-8")
    res <- fromJSON(text, simplifyVector = TRUE)
    total <- res$total
    after <- res$results$id[nrow(res$results)]

    if (!is.null(res$results) && is.data.frame(res$results) && nrow(res$results) > 0) {
      result_list[[i]] <- res$results
      fetched <- fetched + nrow(res$results)
      log_progress(fetched, total)
      i <- i + 1
    } else {
      last_page <- TRUE
    }

  }

  data <- bind_rows(result_list)
  return(data)
}
