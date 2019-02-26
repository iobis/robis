#' Create a list of datasets.
#'
#' @usage dataset(scientificname = NULL, taxonid = NULL, datasetid = NULL,
#'   nodeid = NULL, areaid = NULL, startdate = NULL, enddate = NULL,
#'   startdepth = NULL, enddepth = NULL, geometry = NULL, redlist = NULL,
#'   verbose = FALSE)
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
#' @param redlist include only IUCN Red List species.
#' @param verbose logical. Optional parameter to enable verbose logging (default = \code{FALSE}).
#' @return The datasets.
#' @examples
#' datasets <- dataset(scientificname = "Tellinidae")
#' datasets <- dataset(geometry = "POLYGON ((2.3 51.8, 2.3 51.6, 2.6 51.6, 2.6 51.8, 2.3 51.8))")
#' datasets <- dataset(areaid = 10181)
#' @export
dataset <- function(
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
  redlist = NULL,
  verbose = FALSE
) {

  skip <- 0
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
      redlist = handle_logical(redlist),
      skip = skip,
      size = page_size()
    )

    result <- http_request("GET", "dataset", query)

    if (verbose) {
      log_request(result)
    }

    stop_for_status(result)

    text <- content(result, "text", encoding = "UTF-8")
    res <- fromJSON(text, simplifyVector = TRUE)
    total <- res$total
    skip <- skip + page_size()

    if (!is.null(res$results) && is.data.frame(res$results) && nrow(res$results) > 0) {
      res$results$node_id <- apply(res$results$node, 1, function(x) { return(x["id"]) })
      res$results$node_name <- apply(res$results$node, 1, function(x) { return(x["name"]) })
      res$results <- res$results[,!(names(res$results) %in% c("node", "feed", "institutes", "contacts"))]
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
